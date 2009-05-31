//////////////////////////////////////////////////////////////////////////
//
// This file is © 2007 - 2009 JJ Fahner <jan-jaap@jan-jaap.net>
// This file is part of the cscript interpreter.
// CScript can be found at http://svn.jan-jaap.net/
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//
//////////////////////////////////////////////////////////////////////////
#include "eval.h"
#include "ast.h"
#include "scope.h"
#include "function.h"
#include "srcfile.h"
#include "cslexer.h"
#include "tokens.h"
#include "map_iter.h"
#include "native.h"
#include "timer.h"
#include "enumerator.h"
#include "lexstream.h"
#include "list.h"
#include "dict.h"
#include "datatype.h"
#include "scriptobj.h"

#include "csparser.h"
#include "csparser.gen.h"

#include <list>
#include <iostream>
#include <sstream>
#include <fstream>

//
// Parser functions
//
void *CScriptParseAlloc(void *(*mallocProc)(size_t));
void CScriptParseFree(void *p, void (*freeProc)(void*));
void CScriptParse(void*, int,Token, Evaluator*);
void CScriptParseTrace(FILE*, char*);

/*static*/ const GCString& Scope::parentName = *new GCString("__parent", true);

//////////////////////////////////////////////////////////////////////////
//
// Autoscoping implementation
//

class Evaluator::AutoScope
{
public:

  AutoScope(Evaluator* eval, Scope* scope = 0) : 
  m_eval (eval), 
  m_prv  (0),
  m_cur  (0)
  {
    if(scope)
    {
      Set(scope);
    }
  }

  ~AutoScope()
  {
    Reset();
  }

  void Reset()
  {
    if(m_prv)
    {
      m_eval->m_scope = m_prv;
      m_prv = 0;
      m_cur = 0;
    }
  }

  void Set(Scope* scope)
  {
    if(m_prv)
    {
      Reset();
    }
    m_prv = m_eval->m_scope;
    m_cur = scope;
    m_eval->m_scope = m_cur;
  }
  
private:

  //
  // MemberMap
  //
  Evaluator*  m_eval;
  Scope*      m_prv;
  Scope*      m_cur;

};

//////////////////////////////////////////////////////////////////////////

template <typename T>
struct VecRestore
{
  T& m_vec;
  size_t m_size;
  VecRestore(T& vec) : m_vec (vec)
  {
    m_size = m_vec.size();
  }
  ~VecRestore()
  {
    m_vec.resize(m_size);
  }
};

//////////////////////////////////////////////////////////////////////////
//
// Evaluator implementation
//

/*static*/ Scope*
Evaluator::GetGlobalScope()
{
  static Scope scope;
  return &scope;
}

Evaluator::Evaluator() :
m_scope   (0),
m_allocs  (0),
m_debugParser(0)
{
  // Create global scope
  m_global = new NamespaceScope(GetGlobalScope(), "");

  // Native calls
  static bool nativeCallsRegistered = false;
  if(!nativeCallsRegistered)
  {
    // Register native calls
    nativeCallsRegistered = true;
    NativeCallRegistrar::RegisterCalls();

    // Cleanup after native call registration
    Collect();
  }
}

void 
Evaluator::Reset()
{
  // Create a new global scope
  m_global = new NamespaceScope(GetGlobalScope(), "");

  // Collect all remaining objects
  Collect();
}

inline bool 
open_file(String const& path, std::ifstream& file)
{
  // Reset error flags
  file.clear();

  // Open the file
  file.open(path.c_str());
  
  // Check whether open succeeded
  return file.is_open();
}

bool 
Evaluator::OpenFile(String const& filename, std::ifstream& file)
{
  // Open file using unaltered path
  if(open_file(filename, file))
  {
    m_fileNames.push_back(filename);
    return true;
  }

  // If using an absolute path, give up
  if(Path::IsAbsolute(filename))
  {
    return false;
  }

  // Try to open the file relative to the location of 
  // the currently executing script
  if(m_fileNames.size())
  {
    String path = Path::DirectoryPart(m_fileNames.back());
    path = Path::Combine(path, filename);
    if(open_file(path, file))
    {
      m_fileNames.push_back(path);
      return true;
    }
  }

  // Last try: see if the script exists in the working directory
  String path = Path::Combine(Path::WorkingDirectory(), filename);
  if(open_file(path, file))
  {
    m_fileNames.push_back(path);
    return true;
  }

  // TODO search in env/path?

  // Failed to open file
  return false;
}

Value
Evaluator::ParseFile(String const& filename, bool executeImmediate)
{
  // Construct string stream
  std::ifstream is;
  if(!OpenFile(filename, is))
  {
    throw std::runtime_error("Failed to open file '" + filename + "'");
  }

  // Construct lexer stream
  LexStream ls(is);

  // Create parser
  CSParser parser;

  // Parse source code
  Object* root = parser.Parse(ls);

  // Execute or return code tree
  if(executeImmediate)
  {
    return Eval(root);
  }
  else
  {
    return root;
  }
}

Value
Evaluator::ParseText(String const& text, bool executeImmediate)
{
  // Construct string stream
  std::istringstream is(text);
  
  // Construct lexer stream
  LexStream ls(is);

  // Create parser
  CSParser parser;

  // Parse source code
  Object* root = parser.Parse(ls, executeImmediate);

  // Execute or return code tree
  if(executeImmediate)
  {
    return Eval(root);
  }
  else
  {
    return root;
  }
}

void 
Evaluator::ReportError(String text, Object* source)
{
//   if(source && source->ContainsKey(10))
//   {
//     std::cout << source->GetRValue(10).GetString() << "("
//               << source->GetRValue(11).GetInt()    << ") : ";
//   }
  std::cout << text << "\n";
}

/*static*/ Object* 
Evaluator::ParseNativeCall(String const& declaration)
{
  Evaluator eval;

  // Parse the call
  try
  {
    // Reset reporter
    eval.m_reporter.Reset();

    // Reset native call pointer
    eval.m_resultNode = 0;

    // Parse code
    Value result = eval.ParseText((String("__native ") + declaration).c_str(), false);

    // Check error count
    if(eval.m_reporter.GetErrorCount())
    {
      return 0;
    }

    // Done
    return result.GetObject();
  }
  catch(std::runtime_error const& e)
  {
    std::cout << e.what() << "\n";
    throw;
  }
  catch(...)
  {
    return 0;
  }
}

GC::CollectInfo
Evaluator::Collect()
{
  GC::ObjectVec valid;

  // Insert root object in scope
  valid.push_back(m_scope ? m_scope : m_global);

  // Append temporaries
  for(size_t i = 0; i < m_temporaries.size(); ++i)
  {
    if(GC::Object* o = m_temporaries[i].GetGCObject())
    {
      valid.push_back(o);
    }
  }

  // Collect invalid objects
  return GC::Collect(valid);
}

Value 
Evaluator::Eval(String text, bool isFileName)
{
  try
  {
    // Reset error reporter
    m_reporter.Reset();

    // Parse code - this evaluates 
    // every statement in the code
    if(isFileName)
    {
      return ParseFile(text);
    }
    else
    {
      return ParseText(text.c_str());
    }
  }
  // Return statement
  catch(ReturnException const& e)
  {
    return e.m_value;
  }
  // Reset statement
  catch(ResetException const&)
  {
    Reset();
  }
  // Uncaught user_exception in script
  catch(CatchableException const& e)
  {
    ReportError("Error: Uncaught exception '" + e.m_value.GetString() + "'", e.m_node);
  }
  // Invalid break statement
  catch(BreakException const& e)
  {
    ReportError("Error: Invalid break statement", e.m_node);
  }
  // Invalid continue statement
  catch(ContinueException const& e)
  {
    ReportError("Error: Invalid continue statement", e.m_node);
  }
  // Unexpected exception in script
  catch(ScriptException const& e)
  {
    ReportError(String("Runtime error: ") + e.what(), e.m_node);
  }
  // System exception
  catch(std::exception const& e)
  {
    ReportError(String("Runtime error: ") + e.what());
  }
  // Unknown exception type
  catch(...)
  {
    ReportError("Runtime error: Unexpected exception");
  }
  return Value();
}

Value
Evaluator::Eval(Object* astRoot)
{
  VecRestore<TempVec> vr(m_temporaries);

  // Place global scope on the scope stack
  AutoScope as(this, m_global);

  // Keep ast around during evaluation
  MakeTemp(astRoot);

  // Perform evaluation of ast tree
  return EvalStatement(astRoot);
}

void 
Evaluator::EvalStatementSeq(Object* node)
{
  List* list = AstList_A1(node);
  List::Iterator it = list->Begin();
  List::Iterator ie = list->End();
  for(; it != ie; ++it)
  {
    EvalStatement(it->GetObject());
  }  
}

Value
Evaluator::EvalStatement(Object* node)
{
  VecRestore<TempVec> vr(m_temporaries);

  Value result;
  switch(Ast_Type(node))
  {
  case empty_statement:       break;

  case translation_unit:      EvalStatement(Ast_A1(node));    break;
  case statement_sequence:    EvalStatementSeq(node);         break;
  case namespace_declaration: EvalNamespace(node);            break;
  case variable_declaration:  EvalVariableDeclaration(node);  break;
  case function_declaration:  EvalFunctionDeclaration(node);  break;
  case native_declaration:    EvalNativeDeclaration(node);    break;
  case extern_declaration:    EvalExternDeclaration(node);    break;
  case try_statement:         EvalTryStatement(node);         break;
  case include_statement:     EvalIncludeStatement(node);     break;
  case for_statement:         EvalForStatement(node);         break;
  case foreach_statement:     EvalForeachStatement(node);     break;
  case if_statement:          EvalIfStatement(node);          break;
  case while_statement:       EvalWhileStatement(node);       break;
  case return_statement:      EvalReturnStatement(node);      break;
  case switch_statement:      EvalSwitchStatement(node);      break;
  case unset_statement:       EvalUnsetStatement(node);       break;

  case break_statement:       throw BreakException(node);
  case continue_statement:    throw ContinueException(node);

  case expression_statement:  
    result = EvalExpression(Ast_A1(node));   
    break;
  
  case declarator_sequence:
    EvalStatement(Ast_A1(node));
    EvalStatement(Ast_A2(node));
    break;

  case declaration_sequence:
    EvalStatement(Ast_A1(node));
    if(!Ast_A2(node).Empty())
    {
      EvalStatement(Ast_A2(node));
    }
    break;

  case compound_statement:
    if(!Ast_A1(node).Empty())
    {
      AutoScope as(this, new Scope(m_scope));
      EvalStatement(Ast_A1(node));
    }
    break;

  default: 
    throw ScriptException(node, "Invalid node type");
  }

  // Done
  return result;
}

void
Evaluator::EvalLValue(Object* node, Object*& obj, Value& name)
{
  obj = 0;
  switch(Ast_Type(node))
  {
  case unqualified_id:
    obj = m_scope;
    name = Ast_A1(node);
    return;

  case qualified_id_g:
  case qualified_id_l:
    obj = EvalExpression(Ast_A1(Ast_A1(node)));
    name = Ast_A2(obj);
    return;

  case member_expression:
    obj = ValObject(EvalExpression(Ast_A1(node)));
    name = Ast_A1(Ast_A2(node));
    return;

  case index_expression:
    if(Ast_A2(node).Empty())
    {
      int i = 1;
    }
    obj = EvalExpression(Ast_A1(node));
    if(Ast_A2(node).Empty())
    {
      if(List* list = dynamic_cast<List*>(obj))
      {
        list->Append(Value());
        name = list->Length() - 1;
      }
      else
      {
        throw ScriptException(node, "Invalid type for array append");
      }
    }
    else
    {
      name = EvalExpression(Ast_A2(node));
    }
    return;

  case literal_value:
    obj = ValObject(Ast_A1(node));
    name = Ast_A2(node);
    break;
  }
  
  throw ScriptException(node, "Expression must yield an lvalue");
}

Value
Evaluator::EvalExpression(Object* node)
{
  switch(Ast_Type(node))
  {
  case assignment_expression: return EvalAssignment(node);
  case binary_expression:     return EvalBinary(node);
  case ternary_expression:    return EvalTernary(node);
  case prefix_expression:     return EvalPrefix(node);
  case postfix_expression:    return EvalPostfix(node);
  case typeof_expression:     return EvalTypeOf(node);
  case index_expression:      return EvalIndex(node);
  case function_call:         return EvalFunctionCall(node);
  case literal_value:         return Ast_A1(node);
  case null_literal:          return Value();
  case unqualified_id:        return EvalUnqualifiedId(node);
  case qualified_id_g:        return EvalQualifiedId(node);
  case qualified_id_l:        return EvalQualifiedId(node);
  case list_literal:          return EvalListLiteral(node);
  case list_append:           return EvalListAppend(node);
  case map_literal:           return EvalMapLiteral(node);
  case json_literal:          return EvalJsonLiteral(node);
  case new_expression:        return EvalNewExpression(node);
  case this_expression:       return EvalThisExpression(node);
  case member_expression:     return EvalMemberExpression(node);  
  case closure_expression:    return EvalClosure(node);
  case shell_command:         return EvalShellCommand(node);
  case type_conversion:       return EvalTypeConversion(node);
  case operator_declaration:  return EvalOperatorDeclaration(node);
  case function_member_expression:  return EvalFunctionMember(node);
  case function_index_expression:   return EvalFunctionIndex(node);
  case throw_expression:      
    throw UserException(node, EvalExpression(Ast_A1(node)));
  }
  throw ScriptException(node, "Invalid expression type");
}

Value
Evaluator::EvalAssignment(Object* node)
{
  // Opcode for the assignment
  opcodes opcode = (opcodes)Ast_A1(node).GetInt();

  // Determine left-hand side
  Object* obj;
  Value key;
  EvalLValue(Ast_A2(node), obj, key);

  // Evaluate right-hand side
  Value rhs = EvalExpression(Ast_A3(node));

  // Direct assignment
  if(opcode == op_assign)
  {
    return obj->Set(key, rhs);
  }

  // Retrieve current value
  Value const& lhs = obj->Get(key);

  // Convert right-hand side to left-hand type
  ConvertInPlace(node, rhs, lhs.Type());

  // Perform assignment
  switch(opcode)
  {
  case op_assadd: return obj->Set(key, ValAdd(lhs, rhs));
  case op_asssub: return obj->Set(key, ValSub(lhs, rhs));
  case op_assmul: return obj->Set(key, ValMul(lhs, rhs));
  case op_assdiv: return obj->Set(key, ValDiv(lhs, rhs));
  case op_assmod: return obj->Set(key, ValMod(lhs, rhs));
  default: throw ScriptException(node, "Invalid assignment operator");
  }
}

Value
Evaluator::EvalBinary(Object* node)
{
  // Retrieve operator
  opcodes opcode = (opcodes)Ast_A1(node).GetInt();

  // Evaluate left-hand side
  Value const& lhs = EvalExpression(Ast_A2(node));

  // Handle object
//   if(lhs.Type() == Value::tObject)
//   {
//     String opfun = "operator" + OpcodeToString(opcode);
//     if(lhs->ContainsKey(opfun))
//     {
//       Object* funObj = lhs->GetRValue(opfun).GetObject();
//       ScriptFunction* fun = dynamic_cast<ScriptFunction*>(funObj);
// 
//       return EvalFunctionCall(node, fun, lhs.GetObject(), Ast_A3(node));
//     }
//   }

  // Short-circuited operators
  if(opcode == op_logor)
  {
    // TODO type conversion
    return ValBool(lhs) || ValBool(EvalExpression(Ast_A3(node)));
  }
  if(opcode == op_logand)
  {
    // TODO type conversion
    return ValBool(lhs) && ValBool(EvalExpression(Ast_A3(node)));
  }
  
  // Evaluate right-hand side
  Value rhs = EvalExpression(Ast_A3(node));

  // Comparison without implicit type conversion
  if(opcode == op_seq)
  {
    return lhs.Type() == rhs.Type() && ValCmp(lhs, rhs) == 0;
  }
  if(opcode == op_sne)
  {
    return lhs.Type() == rhs.Type() && ValCmp(lhs, rhs) != 0;
  }

  // Convert to type of lhs
  ConvertInPlace(node, rhs, lhs.Type());

  // Perform operation
  Value result;
  switch(opcode)
  {
  case op_add:    result = ValAdd(lhs, rhs); break;
  case op_sub:    result = ValSub(lhs, rhs); break;
  case op_mul:    result = ValMul(lhs, rhs); break;
  case op_div:    result = ValDiv(lhs, rhs); break;
  case op_mod:    result = ValMod(lhs, rhs); break;
  case op_eq:     result = ValCmp(lhs, rhs) == 0; break;
  case op_ne:     result = ValCmp(lhs, rhs) != 0; break;
  case op_lt:     result = ValCmp(lhs, rhs) <  0; break;
  case op_le:     result = ValCmp(lhs, rhs) <= 0; break;
  case op_gt:     result = ValCmp(lhs, rhs) >  0; break;
  case op_ge:     result = ValCmp(lhs, rhs) >= 0; break;
  default: throw ScriptException(node, "Invalid binary operator");
  }  

  // Return new temporary
  return result;
}

Value
Evaluator::EvalTernary(Object* node)
{
  if(ValBool(EvalExpression(Ast_A1(node))))
  {
    return EvalExpression(Ast_A2(node));
  }
  return EvalExpression(Ast_A3(node));
}

Value
Evaluator::EvalPrefix(Object* node)
{
  // Unary operators
  switch(Ast_A1(node).GetInt())
  {
  case op_negate: 
    return ValNeg(EvalExpression(Ast_A2(node)));

  case op_not:    
    return ValNot(EvalExpression(Ast_A2(node)));
  }

  // Resolve lvalue
  Object* obj;
  Value key;
  EvalLValue(Ast_A2(node), obj, key);

  // Handle operator
  Value result;
  switch(Ast_A1(node).GetInt())
  {
  case op_preinc:
    result = ValAdd(obj->Get(key), 1);
    obj->Set(key, result);
    return result;

  case op_predec: 
    result = ValAdd(obj->Get(key), 1);
    obj->Set(key, result);
    return result;
  }

  // Failed
  throw ScriptException(node, "Invalid prefix operator");
}

Value
Evaluator::EvalPostfix(Object* node)
{
  // Evaluate lhs
  Object* obj;
  Value key;
  EvalLValue(Ast_A2(node), obj, key);

  // Create temporary with result value
  Value v = obj->Get(key);

  // Perform postfix operation
  switch(Ast_A1(node).GetInt())
  {
  case op_postinc:
    obj->Set(key, ValAdd(v, 1));
    return v;

  case op_postdec:
    obj->Set(key, ValSub(v, 1));
    return v;
  }
  
  throw ScriptException(node, "Invalid postfix operator");
}

Value
Evaluator::EvalTypeOf(Object* node)
{
  return EvalExpression(Ast_A1(node)).GetDataType();
}

Value
Evaluator::EvalIndex(Object* node)
{
  // Retrieve the container
  Object* obj;
  Value key;
  EvalLValue(node, obj, key);

  // Retrieve the list
  return obj->Get(key);
}

Value
Evaluator::EvalListAppend(Object* node)
{
  Object* obj = EvalExpression(Ast_A2(node));
  List* list = dynamic_cast<List*>(obj);
  if(list == 0)
  {
    throw ScriptException(node, "Invalid type for list append operator");
  }
  
  return list->Append(EvalExpression(Ast_A4(node)));
}

Value
Evaluator::EvalUnqualifiedId(Object* node)
{
  return m_scope->Get(Ast_A1(node));
}

NamespaceScope* 
FindNamespace(Scope* from)
{
  while(from)
  {
    if(NamespaceScope* ns = dynamic_cast<NamespaceScope*>(from))
    {
      return ns;
    }
    from = from->GetParent();
  }
  return 0;
}

Value
Evaluator::EvalQualifiedId(Object* node)
{
  // Find starting scope
  NamespaceScope* scope;
  if(Ast_Type(node) == qualified_id_g)
  {
    // Start lookups in global scope
    scope = m_global;
  }
  else
  {
    // Start lookups in current namespace
    scope = FindNamespace(m_scope);
  }

  // Perform lookup
  Object* cur = node;
  for(;;)
  {
    // Retrieve name
    String name = Ast_A1(cur);

    // Retrieve node
    Value const& rval = scope->Get(name);
    if(rval.Empty())
    {
      throw ScriptException(node, "Namespace '" + scope->GetName() + 
                      "' does not contain a member '" + name + "'");
    }

    // If nested, descend, else retrieve value
    if(!Ast_A2(cur).Empty())
    {
      // Retrieve namespace scope
      scope = dynamic_cast<NamespaceScope*>(rval.GetObject());
      if(scope == 0)
      {
        throw ScriptException(node, "Expected namespace, found function or variable");
      }

      // Descend into id
      cur = Ast_A2(cur);
    }
    else
    {
      // Found it
      return rval;
    }
  }
}

void 
Evaluator::EvalUnsetStatement(Object* node)
{
  if(Ast_Type(Ast_A1(node)) == unqualified_id)
  {
    String name = Ast_A1(Ast_A1(node));
    m_scope->Unset(name);
  }
}

Value
Evaluator::EvalOperatorDeclaration(Object* node)
{
  String oper;
  if(Ast_A1(node).Type() == Value::tInt)
  {
    oper = "operator" + Ast_A2(node).GetString();
  }
  else
  {
    oper = "operator " + Ast_A2(Ast_A1(node)).GetString();
  }
  return oper;
}

void 
Evaluator::EvalNamespace(Object* node)
{
  // Find current namespace
  NamespaceScope* curNS = FindNamespace(m_scope);

  // Determine namespace name
  String const& name = Ast_A1(node);

  // Create namespace scope
  AutoScope as(this, new NamespaceScope(curNS, name));

  // Evaluate declarations in scope
  if(!Ast_A2(node).Empty())
  {
    EvalStatement(Ast_A2(node));
  }
}

void
Evaluator::EvalVariableDeclaration(Object* node)
{
  // Determine right-hand value
  Value value;
  if(Ast_A2(node).Empty())
  {
    value = Value();
  }
  else
  {
    // Assign value-of rhs
    value = EvalExpression(Ast_A2(node));
  }

  // Create variable
  // TODO deletion of newed variable
  m_scope->Add(Ast_A1(node), value);
}

void
Evaluator::EvalFunctionDeclaration(Object* node)
{
  String name = Ast_A1(node);
  Function* fun = new ScriptFunction(name, node);
  
  fun->Set("name", name);
  fun->Set("parent", m_scope);
  fun->Set("scope", FindNamespace(m_scope));

  m_scope->Add(name, fun);
}

void
Evaluator::EvalNativeDeclaration(Object* node)
{
  m_resultNode = node;
}

Value
Evaluator::EvalClosure(Object* node)
{
  Function* fun = new ScriptFunction(Ast_A1(node), node);

  fun->Set("name", fun->GetName());
  fun->Set("parent", m_scope);
  fun->Set("scope", FindNamespace(m_scope));
  
  return fun;
}

Value
Evaluator::EvalFunctionMember(Object* node)
{
  String name = Ast_A1(Ast_A1(node));

  Scope* s = m_scope;
  while(s)
  {
    if(ObjectScope* c = dynamic_cast<ObjectScope*>(s))
    {
      if(Function* f = dynamic_cast<Function*>(c->GetObject()))
      {
        return f->Get(name);
      }
    }
    s = s->GetParent();
  }

  throw ScriptException(node, "Invalid scope for function keyword");
}

Value
Evaluator::EvalFunctionIndex(Object* node)
{
  throw ScriptException(node, "Fail");
}

void 
Evaluator::EvalExternDeclaration(Object* node)
{
  m_scope->Add(Ast_A1(node), new ExternFunction(Ast_A1(node), node));
}

Value
Evaluator::EvalFunctionCall(Object* node)
{
  // Evaluate left-hand side
  Object* obj;
  Value key;
  EvalLValue(Ast_A1(node), obj, key);
  
  // Retrieve arguments from node
  Object* argsource = 0;
  if(!Ast_A2(node).Empty())
  {
    argsource = Ast_A2(node);
  }

  // Setup arguments
  Arguments args;
  args.SetObject(obj);
  args.SetNode(node);

  // Evaluate arguments
  EvalArguments(node, argsource, args);

  // Evaluate the function call
  return obj->Eval(key, this, args);
}

Value
Evaluator::EvalScriptCall(ScriptFunction* fun, Arguments& args)
{
  // Class scope
  AutoScope cs(this);

  // Determine base scope for function
  Scope* parentScope = dynamic_cast<Scope*>(fun->Get("scope").GetObject());

  // Determine parent scope for invocation
  if(args.GetObject())
  {
    // Create class scope
    cs.Set(new ObjectScope(parentScope, args.GetObject()));

    // Adjust parent scope
    parentScope = m_scope;
  }

  // Create function scope
  AutoScope asf(this, new ObjectScope(parentScope, fun));

  // Create argument scope
  AutoScope asa(this, new Scope(m_scope));

  // Insert arguments into argument scope
  List::Iterator pi, pe;
  pi = fun->GetParameters()->Begin();
  pe = fun->GetParameters()->End();
  for(size_t index = 0; pi != pe; ++pi, ++index)
  {
    if(index >= args.size())
    {
      // FIXME add default values here
      m_scope->Add(Ast_A1(pi->GetObject()), Value());
    }
    else
    {
      m_scope->Add(Ast_A1(pi->GetObject()), args[index]);
    }
  }

  // Create function execution scope
  AutoScope es(this, new Scope(m_scope));

  // Execute expression
  try
  {
    EvalStatement(Ast_A3(fun->GetNode()));
    return Value();
  }
  catch(ReturnException const& e)
  {
    return e.m_value;
  }
}

void 
Evaluator::EvalArguments(Object* node, Object* argptr, Arguments& args)
{
  // Make iterator for argument list
  AstIterator ai(argptr, arguments);
  AstIterator ae;

//   // No formal parameter list
  // FIXME
//   if(fun->GetParameters() == 0)
//   {
    // Evaluate arguments
    for(; ai != ae; ++ai)
    {
      args.push_back(EvalExpression(*ai));
    }

    // Done
    return;
//   }
/*
  // Enumerate parameters
  List::Iterator pi = fun->GetParameters()->Begin();
  List::Iterator pe = fun->GetParameters()->End();
  for(;;)
  {
    // End of lists
    if(pi == pe && ai == ae)
    {
      break;
    }

    // End of parameters
    if(pi == pe)
    {
      throw ScriptException(node, "Too many arguments in call to '" + fun->GetName() + "'");
    }

    // Extract parameter
    Object* par = pi->GetObject();

    // Variadic parameter
    if(Ast_A2(par).GetInt() == ptVariadic)
    {
      // Insert map
      List* va = new List();
      args.push_back(va);

      // Insert remaining arguments
      for(; ai != ae; ++ai)
      {
        // Evaluate argument value. ATTN: this dereference
        // is intentional; variadic arguments are by value.
        Value val = EvalExpression(*ai);

        // Append to list
        va->Append(val);
      }

      // Done
      break;
    }

    // End of arguments
    Value value;
    if(ai == ae)
    {
      // Must have default value
      if(Ast_A4(par).Empty())
      {
        throw ScriptException(node, "Not enough arguments in call to '" + fun->GetName() + "'");
      }

      // Evaluate default value
      value = EvalExpression(Ast_A4(par));
    }
    else
    {
      // Evaluate argument
      value = EvalExpression(*ai);
    }

    // Handle byref/byval
    if(Ast_A2(par).GetInt() == ptByVal)
    {
      // Dereference the value
//       value = *value;
    }

    // Apply type conversion to value
    if(!Ast_A3(par).Empty())
    { 
      ConvertInPlace(node, value, (Value::Types)Ast_A1(Ast_A3(par)).GetInt());
    }

    // Add to argument list
    args.push_back(value);

    // Next iteration
    ++pi;
    if(ai != ae)
    {
      ++ai;
    }
  }
  */
}

Value
Evaluator::EvalListLiteral(Object* node)
{
  // Create empty list
  List* list = new List();
  MakeTemp(list);
  
  // Recurse into map values
  if(!Ast_A1(node).Empty())
  {
    Object* child = Ast_A1(node);
    int index = 0;
    while(child)
    {
      // Add element to list
      list->Append(EvalExpression(Ast_A1(Ast_A1(child))));

      // Check for next element
      if(Ast_A2(child).Empty()) 
      {
        break;
      }

      // Set next element
      child = Ast_A2(child);
    }
  }

  // Done
  return list;
}

Value
Evaluator::EvalMapLiteral(Object* node)
{
  // Create empty map
  Dictionary* dict = new Dictionary();
  MakeTemp(dict);

  // Recurse into map values
  if(!Ast_A1(node).Empty())
  {
    Object* child = Ast_A1(node);
    int index = 0;
    while(child)
    {
      // Evaluate key and value
      Value const& key = EvalExpression(Ast_A1(Ast_A1(child)));
      Value const& val = EvalExpression(Ast_A2(Ast_A1(child)));

      // Check for duplicate key in object, ignore prototype
      if(!dict->Insert(key, val))
      {
        throw ScriptException(node, "Duplicate key in list literal");
      }

      // Check for next element
      if(Ast_A2(child).Empty()) 
      {
        break;
      }

      // Set next element
      child = Ast_A2(child);
    }
  }

  // Done
  return dict;
}

Value
Evaluator::EvalJsonLiteral(Object* node)
{
  // Create empty map
  Value v(new ScriptObject());
  Object* o = v.GetObject();

  // Recurse into map values
  if(!Ast_A1(node).Empty())
  {
    Object* child = Ast_A1(node);
    while(child)
    {
      // Retrieve key
      String key;
      if(Ast_A1(Ast_A1(child)).Type() == Value::tString)
      {
        key = Ast_A1(Ast_A1(child)).GetString();
      }
      else
      {
        key = EvalExpression(Ast_A1(Ast_A1(child))).GetString();
      }

//       // Check for duplicate key in object, ignore prototype
      // FIXME
//       if(o->ContainsKey(key, false))
//       {
//         throw ScriptException(node, "Duplicate key in JSON literal");
//       }

      // Evaluate element
      Value const& element = EvalExpression(Ast_A2(Ast_A1(child)));

      // Insert into member variables
      o->Set(key, element);

      // Check for next element
      if(Ast_A2(child).Empty()) 
      {
        break;
      }

      // Set next element
      child = Ast_A2(child);
    }
  }

  // Done
  return v;
}

Value
Evaluator::EvalShellCommand(Object* node)
{
  // Extract command
  String command = Ast_A1(node).GetString();

  // Execute the command
  return system(command.c_str());
}

void 
Evaluator::EvalReturnStatement(Object* node)
{
  // Determine return value
  Value value;
  if(Ast_A1(node).Empty())
  {
    value = Value();
  }
  else
  {
    value = EvalExpression(Ast_A1(node));
  }

  // Throw return exception
  throw ReturnException(node, value);
}

void 
Evaluator::EvalIncludeStatement(Object* node)
{
  // Evaluate include expression
  Value const& rval = EvalExpression(Ast_A1(node));

  // Parse include file
  ParseFile(ValString(rval));
}

void 
Evaluator::EvalForStatement(Object* node)
{
  // Outer scope
  AutoScope scope(this, new Scope(m_scope));
  
  // Evaluate init expression
  EvalStatement(Ast_A1(node));

  // Evaluate loop
  for(;;)
  {
    // Check condition
    if(!ValBool(EvalExpression(Ast_A2(node))))
    {
      break;
    }

    // For body
    try 
    {
      AutoScope scope(this, new Scope(m_scope));
      EvalStatement(Ast_A4(node));
    }
    catch(BreakException const&)
    {
      break;
    }
    catch(ContinueException const&)
    {
    }

    // Evaluate post expression
    EvalExpression(Ast_A3(node));
  }  
}

void 
Evaluator::EvalForeachStatement(Object* node)
{
  AutoScope scope(this, new Scope(m_scope));
  
  Object* obj;
  Value key;

  // Fetch iterator variable
  if(Ast_Type(Ast_A1(node)) == variable_declaration)
  {
    EvalStatement(Ast_A1(node));
    obj = m_scope;
    key = Ast_A1(Ast_A1(node)).GetString();
  }
  else
  {
    EvalLValue(Ast_A1(node), obj, key);
  }

  // Evaluate expression
  Value const& rhs = EvalExpression(Ast_A2(node));

  // Retrieve enumerator
  Enumerator* enumerator = rhs->GetEnumerator();
  if(enumerator == 0)
  {
    throw ScriptException(node, "Invalid type specified in foreach");
  }

  // Add to local scope
  MakeTemp(enumerator);

  // Enumerate members
  Value value;
  while(enumerator->GetNext(value))
  {
    // Assign value to iterator variable
    obj->Set(key, value);

    // Evaluate expression
    try
    {
      AutoScope scope(this, new Scope(m_scope));
      EvalStatement(Ast_A3(node));
    }
    catch(BreakException const&)
    {
      break;
    }
    catch(ContinueException const&)
    {
    }
  }
}

void
Evaluator::EvalIfStatement(Object* node)
{
  if(ValBool(EvalExpression(Ast_A1(node))))
  {
    EvalStatement(Ast_A2(node));
  }
  else if(!Ast_A3(node).Empty())
  {
    EvalStatement(Ast_A3(node));
  }
}

void        
Evaluator::EvalWhileStatement(Object* node)
{
  for(;;)
  {
    if(!ValBool(EvalExpression(Ast_A1(node))))
    {
      break;
    }

    try
    {
      AutoScope scope(this, new Scope(m_scope));
      EvalStatement(Ast_A2(node));
    }
    catch(BreakException const&)
    {
      break;
    }
    catch(ContinueException const&)
    {
    }
  }
}

void
Evaluator::EvalSwitchStatement(Object* node)
{
  // Switch value
  Value value = EvalExpression(Ast_A1(node));

  // Create iterators
  List::Iterator it = AstList_A2(node)->Begin();
  List::Iterator ie = AstList_A2(node)->End();

  // Find case that matches switch value
  Object* statement = 0;
  for(; it != ie; ++it)
  {
    if(Ast_Type(*it) == default_case)
    {
      if(statement)
      {
        throw ScriptException(node, "More than one default case in switch statement");
      }
      statement = Ast_A1(*it);
    }
    else if(ValCmp(EvalExpression(Ast_A1(*it)), value) == 0)
    {
      statement = Ast_A2(*it);
      break;
    }
  }

  // Execute statement
  if(statement)
  {
    try
    {
      AutoScope as(this, new Scope(m_scope));
      EvalStatement(statement);
    }
    catch(BreakException const&)
    {
    }
  }
}

Value
Evaluator::EvalNewExpression(Object* node)
{
  // Find object
  Value const& rval = m_scope->Get(Ast_A1(node).GetString());
  if(rval.Type() != Value::tObject)
  {
    throw ScriptException(node, "Cannot instantiate a non-object variable");
  }

  // Construct an instance from the source object
  Object* inst = new ScriptObject();
  
  // Assign prototype object
  inst->Set("prototype", rval.GetObject());

  // Prepare arguments
  // TODO evaluate constructor arguments
  Arguments args;
  args.SetNode(node);
  args.SetObject(inst);

  // Execute constructor
  Value result;
  inst->TryEval("constructor", this, args, result);

//   if(inst->Find("constructor", funObj))
//   {
//     // Check whether it's a function
//     Function* fun = dynamic_cast<Function*>(funObj->GetObject());
//     if(fun == 0)
//     {
//       throw ScriptException(node, "Class has invalid constructor");
//     }
//     
//     // Evaluate constructor
//     EvalFunctionCall(node, fun, inst, Ast_A2(node));
//   }
// 
  // Return temporary
  return inst;
}

Value
Evaluator::EvalMemberExpression(Object* node)
{
  // Retrieve left-hand side
  Value const& lhs = EvalExpression(Ast_A1(node));
  Object* object;
  if(lhs.Type() == Value::tObject)
  {
    object = ValueToType<Object>(lhs);
  }
  else if(ScalarType* s = dynamic_cast<ScalarType*>(lhs.GetDataType()))
  {
    object = s->Box(lhs);
  }
  else
  {
    throw ScriptException(node, "Left-hand side does not yield an object");
  }

  // Determine name
  String const& name = Ast_A1(Ast_A2(node));

  // Return the value
  Value rval;
  if(!object->TryGet(name, rval))
  {
    throw ScriptException(node, "Object does not support this property");
  }

  // Done
  return rval;
}

Value
Evaluator::EvalThisExpression(Object* node)
{
  Scope* scope = m_scope;
  while(scope)
  {
    ObjectScope* cs = dynamic_cast<ObjectScope*>(scope);
    if(cs && dynamic_cast<Function*>(cs->GetObject()) == 0)
    {
      return cs->GetObject();
    }
    scope = scope->GetParent();
  }
  throw ScriptException(node, "Invalid context for 'this'");
}

void 
Evaluator::EvalTryStatement(Object* node)
{
  try
  {
    try 
    {
      // Execute guarded block
      EvalStatement(Ast_A1(node));
    }
    catch(CatchableException const& e)
    {
      // Handle only when handler is present
      if(!Ast_A2(node).Empty())
      {
        // Insert exception into scope
        // TODO this maketemp might crash horribly during exception cleanup!!!
        AutoScope scope(this, new Scope(m_scope));
        m_scope->Add(Ast_A1(Ast_A2(node)), e.m_value);

        // Evaluate catch block
        EvalStatement(Ast_A2(Ast_A2(node)));
      }
      else
      {
        // Propagate exception
        throw;
      }
    }
  }
  catch(...)
  {
    // Exception thrown in catch block
    if(!Ast_A3(node).Empty())
    {
      // Evaluate finally
      EvalStatement(Ast_A1(Ast_A3(node)));
    }

    // Re-throw exception from catch block
    throw;
  }

  // Evaluate finally
  if(!Ast_A3(node).Empty())
  {
    EvalStatement(Ast_A1(Ast_A3(node)));
  }
}

Value
Evaluator::EvalTypeConversion(Object* node)
{
  // Evaluate the expression
  Value const& rhs = EvalExpression(Ast_A2(node));
  
  // Determine type
  Value::Types newType = (Value::Types)Ast_A1(Ast_A1(node)).GetInt();

  // Perform conversion
  return Convert(node, rhs, newType);
}

Value 
Evaluator::Convert(Object* node, Value const& value, Value::Types newType)
{
  Value tempValue(value);
  ConvertInPlace(node, tempValue, newType);
  return tempValue;
}

void 
Evaluator::ConvertInPlace(Object* node, Value& value, Value::Types newType)
{
  // Ignore when equal
  if(value.Type() == newType)
  {
    return;
  }

  // Objects may implement certain type conversions themselves
//   if(value.Type() == Value::tObject)
//   {
//     String opfun = "operator " + Value::TypeToString(newType);
//     if(value->ContainsKey(opfun))
//     {
//       Object* funObj = value->Get(opfun);
//       ScriptFunction* fun = dynamic_cast<ScriptFunction*>(funObj);
// 
//       Arguments args;
//       args.SetObject(value);
// 
//       value = EvalScriptCall(fun, args);
//       
//       if(value.Type() != newType)
//       {
//         throw ScriptException(node, "Conversion operator yields wrong type");
//       }
// 
//       return;
//     }
//   }

  // Convert to basic types
  switch(newType)
  {
  case Value::tNull:    return;
  case Value::tBool:    value = ValBool(value);   return;
  case Value::tInt:     value = ValInt(value);    return;
  case Value::tString:  value = ValString(value); return;
  case Value::tObject:  if(value.Type() == Value::tNull) return;
  }

  // Failed conversion
  throw ScriptException(node, "Cannot convert between types");
}
