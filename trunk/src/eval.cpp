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
#include "cslexer.h"
#include "tokens.h"
#include "native.h"
#include "timer.h"
#include "enumerator.h"
#include "lexstream.h"
#include "list.h"
#include "dict.h"
#include "datatype.h"
#include "scriptobj.h"
#include "stack.h"

#include "csparser.h"
#include "csparser.gen.h"

#include <native/path.h>
#include <native/console.h>
#include <native/standard.h>

#include <list>
#include <iostream>
#include <sstream>
#include <fstream>

Stack g_stack(1024*1024);

//////////////////////////////////////////////////////////////////////////
//
// Autoscoping implementation
//

class Evaluator::AutoScope
{
public:

  AutoScope(Scope* scope = 0) : 
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
    Scope* cur = m_cur;
    m_cur = 0;

    if(m_prv)
    {
      CurEval.m_scope = m_prv;
      m_prv = 0;
    }

    if(cur)
    {
      if(cur->HasRefs())
      {
        return;
      }
      else
      {
        cur->Clear();
        CurEval.m_scopeCache.push_back(cur);
      }
    }
  }

  void Set(Scope* scope)
  {
    if(m_prv)
    {
      Reset();
    }
    m_prv = CurEval.m_scope;
    m_cur = scope;
    CurEval.m_scope = m_cur;
  }
  
private:

  //
  // MemberMap
  //
  Scope* m_prv;
  Scope* m_cur;

};

//////////////////////////////////////////////////////////////////////////

Evaluator::Evaluator() :
m_scope   (0),
m_allocs  (0),
m_debugParser(0)
{
  // Initialize the evaluator
  Reset();
}

void 
Evaluator::Reset()
{
  // Create superglobal scope
  Scope* scope = NewScope();
  scope->Add("Path", new Path());
  scope->Add("Console", new Console());
  scope->Add("CScript", new CScriptMethods());

  // Create a new global scope
  m_global = new NamespaceScope(scope, "");

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

inline Scope* 
Evaluator::NewScope(Scope* parent, Object* object)
{
  // Create new when out of scopes
  if(m_scopeCache.size() == 0)
  {
    return new Scope(parent, object);
  }

  // Retrieve scope from cache
  Scope* scope = m_scopeCache.back();
  m_scopeCache.pop_back();
  
  // Set parent pointer
  if(parent)
  {
    scope->SetParent(parent);
  }
  
  // Set object pointer
  if(object) 
  {
    scope->SetObject(object);
  }
  
  // Done
  return scope;
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
  Object* root = parser.Parse(ls, false);

  // Close the file
  is.close();

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
  Object* root = parser.Parse(ls, false);

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

GC::CollectInfo
Evaluator::Collect()
{
  // Push root scope onto stack
  StackFrame s(g_stack);
  g_stack.Push(m_scope ? m_scope : m_global);

  // Clear scope cache
  m_scopeCache.clear();

  // Collect invalid objects
  return GC::Collect(g_stack);
}

Value 
Evaluator::Eval(String text, bool isFileName)
{
  // Reset error reporter
  m_reporter.Reset();

  // Parse code - this evaluates 
  // every statement in the code
  Value result;
  if(isFileName)
  {
    result = ParseFile(text);
  }
  else
  {
    result = ParseText(text.c_str());
  }

  // Done
  return result;
}

Value
Evaluator::Eval(Object* astRoot)
{
  // Set TLS instance
  Context ctx(this);

  // Guard stack pointer
  StackFrame sf(g_stack);

  // Store current scope on stack
  g_stack.Push(m_scope);

  // Retrieve stack from node
  Scope* scope = 0;
  if(Ast_Type(astRoot) == expression_statement &&
     !Ast_A2(astRoot).Empty())
  {
    scope = (Scope*)Ast_A2(astRoot).GetObject();
  }

  // Place scope on the scope stack
  AutoScope at(NewScope(scope ? scope : m_global));

  // Keep ast around during evaluation
  g_stack.Push(astRoot);

  try
  {
    // Perform evaluation of ast tree
    EvalStatement(astRoot);
    return Value();
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
    ReportError("Error: Uncaught exception '" + ValString(e.m_value) + "'", e.m_node);
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

void 
Evaluator::EvalStatement(Object* node)
{
  StackFrame sf(g_stack);

  Value result;
  switch(Ast_Type(node))
  {
  case empty_statement:       break;

  case translation_unit:      EvalStatement(Ast_A1(node));    break;
//  case namespace_declaration: EvalNamespace(node);            break;
  case variable_declaration:  EvalVariableDeclaration(node);  break;
  case function_declaration:  EvalFunctionDeclaration(node);  break;
  case extern_declaration:    EvalExternDeclaration(node);    break;
  case try_statement:         EvalTryStatement(node);         break;
  case include_statement:     EvalIncludeStatement(node);     break;
  case for_statement:         EvalForStatement(node);         break;
  case foreach_statement:     EvalForeachStatement(node);     break;
  case while_statement:       EvalWhileStatement(node);       break;
  case return_statement:      EvalReturnStatement(node);      break;
  case switch_statement:      EvalSwitchStatement(node);      break;
  case unset_statement:       EvalUnsetStatement(node);       break;

  case break_statement:       
    throw BreakException(node);

  case compound_statement:
    if(!Ast_A1(node).Empty())
    {
      AutoScope as(NewScope(m_scope));
      EvalStatement(Ast_A1(node));
    }
    break;

  case continue_statement:    
    throw ContinueException(node);

  case declaration_sequence:
    EvalStatement(Ast_A1(node));
    if(!Ast_A2(node).Empty())
    {
      EvalStatement(Ast_A2(node));
    }
    break;

  case declarator_sequence:
    EvalStatement(Ast_A1(node));
    EvalStatement(Ast_A2(node));
    break;

  case expression_statement:
    EvalExpression(Ast_A1(node));   
    result = g_stack.Pop();
    break;
  
  case if_statement:
    EvalExpression(Ast_A1(node));
    if(ValBool(g_stack.Pop()))
    {
      EvalStatement(Ast_A2(node));
    }
    else if(!Ast_A3(node).Empty())
    {
      EvalStatement(Ast_A3(node));
    }
    break;

  case statement_sequence:
    for(List::Iterator it = AstList_A1(node)->Begin(),
                       ie = AstList_A1(node)->End()
                       ; it != ie; ++it)
    {
      EvalStatement(it->GetObject());
    }  
    break;

  case throw_expression:
    EvalExpression(Ast_A1(node));
    throw UserException(node, g_stack.Pop());

  default: 
    throw ScriptException(node, "Invalid node type");
  }
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
    EvalExpression(Ast_A1(Ast_A1(node)));
    obj = g_stack.Pop();
    name = Ast_A2(obj);
    return;

  case member_expression:
    EvalExpression(Ast_A1(node));
    obj = ValObject(g_stack.Pop());
    name = Ast_A1(Ast_A2(node));
    return;

  case index_expression:
    EvalExpression(Ast_A1(node));
    obj = ValObject(g_stack.Pop());
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
      EvalExpression(Ast_A2(node));
      name = g_stack.Pop();
    }
    return;

  case literal_value:
    obj = ValObject(Ast_A1(node));
    name = Ast_A2(node);
    break;
  }
  
  throw ScriptException(node, "Expression must yield an lvalue");
}

void
Evaluator::EvalExpression(Object* node)
{
  switch(Ast_Type(node))
  {
  case assignment_expression: return EvalAssignment(node);
  case binary_expression:     return EvalBinary(node);
  case prefix_expression:     return EvalPrefix(node);
  case postfix_expression:    return EvalPostfix(node);
  case index_expression:      return EvalIndex(node);
//  case qualified_id_g:        return EvalQualifiedId(node);
//  case qualified_id_l:        return EvalQualifiedId(node);
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
  
  case function_call:
    EvalFunctionCall(node);
    break;

  case lambda_expression:
    m_scope->AddRef();
    g_stack.Push(new AstNode(expression_statement, Ast_A1(node), m_scope));
    break;

  case literal_value:         
    g_stack.Push(Ast_A1(node));
    break;

  case logical_and_expression:    
    EvalExpression(Ast_A1(node));
    if(!ValBool(g_stack.Top()))
    {
      break;
    }
    g_stack.Pop();
    EvalExpression(Ast_A2(node));
    break;

  case logical_or_expression:
    EvalExpression(Ast_A1(node));
    if(ValBool(g_stack.Top()))
    {
      break;
    }
    g_stack.Pop();
    EvalExpression(Ast_A2(node));
    break;

  case null_literal:          
    g_stack.Push(Value());
    break;

  case ternary_expression:
    EvalExpression(Ast_A1(node));
    if(ValBool(g_stack.Pop()))
    {
      EvalExpression(Ast_A2(node));
    }
    else
    {
      EvalExpression(Ast_A3(node));
    }
    break;

  case typeof_expression:
    EvalExpression(Ast_A1(node));
    g_stack.Push(g_stack.Pop().GetDataType());
    break;

  case unqualified_id:
    g_stack.Push(m_scope->Get(Ast_A1(node)));
    break;

  default:
    throw ScriptException(node, "Invalid expression type");
  }
}

void
Evaluator::EvalAssignment(Object* node)
{
  // Evaluate right-hand side
  EvalExpression(Ast_A2(node));
  Value rhs = g_stack.Pop();

  // Determine left-hand side
  Object* obj;
  Value key;
  EvalLValue(Ast_A1(node), obj, key);

  // Direct assignment
  g_stack.Push(obj->Set(key, rhs));
}

void
Evaluator::EvalBinary(Object* node)
{
  // Retrieve operator
  opcodes opcode = (opcodes)Ast_A1(node).GetInt();

  // Evaluate expressions
  EvalExpression(Ast_A2(node));
  Value lhs = g_stack.Pop();
  EvalExpression(Ast_A3(node));
  Value rhs = g_stack.Pop();

  // Comparison without implicit type conversion
  if(opcode == op_seq)
  {
    bool res = lhs.Type() == rhs.Type() && ValCmp(lhs, rhs) == 0;
    g_stack.Push(res);
    return;
  }
  if(opcode == op_sne)
  {
    bool res = lhs.Type() == rhs.Type() && ValCmp(lhs, rhs) != 0;
    g_stack.Push(res);
    return;
  }

  // Convert to type of lhs
  ConvertInPlace(node, rhs, lhs.Type());

  // Perform operation
  Value result;
  switch(opcode)
  {
  case op_add: g_stack.Push(ValAdd(lhs, rhs));      break;
  case op_sub: g_stack.Push(ValSub(lhs, rhs));      break;
  case op_mul: g_stack.Push(ValMul(lhs, rhs));      break;
  case op_div: g_stack.Push(ValDiv(lhs, rhs));      break;
  case op_mod: g_stack.Push(ValMod(lhs, rhs));      break;
  case op_eq:  g_stack.Push(ValCmp(lhs, rhs) == 0); break;
  case op_ne:  g_stack.Push(ValCmp(lhs, rhs) != 0); break;
  case op_lt:  g_stack.Push(ValCmp(lhs, rhs) <  0); break;
  case op_le:  g_stack.Push(ValCmp(lhs, rhs) <= 0); break;
  case op_gt:  g_stack.Push(ValCmp(lhs, rhs) >  0); break;
  case op_ge:  g_stack.Push(ValCmp(lhs, rhs) >= 0); break;
  default: throw ScriptException(node, "Invalid binary operator");
  }  
}

void
Evaluator::EvalPrefix(Object* node)
{
  // Unary operators
  switch(Ast_A1(node).GetInt())
  {
  case op_negate: 
    EvalExpression(Ast_A2(node));
    g_stack.Push(ValNeg(g_stack.Pop()));
    return;

  case op_not:    
    EvalExpression(Ast_A2(node));
    g_stack.Push(ValNot(g_stack.Pop()));
    return;
  }

  // Resolve lvalue
  Object* obj;
  Value key;
  EvalLValue(Ast_A2(node), obj, key);

  // Handle operator
  Value result;
  switch(Ast_A1(node).GetInt())
  {
  case op_add:
    result = ValAdd(obj->Get(key), 1);
    obj->Set(key, result);
    g_stack.Push(result);
    return;

  case op_sub: 
    result = ValAdd(obj->Get(key), 1);
    obj->Set(key, result);
    g_stack.Push(result);
    return;
  }

  // Failed
  throw ScriptException(node, "Invalid prefix operator");
}

void
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
  case op_add:
    obj->Set(key, ValAdd(v, 1));
    g_stack.Push(v);
    return;

  case op_sub:
    obj->Set(key, ValSub(v, 1));
    g_stack.Push(v);
    return;
  }
  
  throw ScriptException(node, "Invalid postfix operator");
}

void
Evaluator::EvalIndex(Object* node)
{
  // Retrieve the container
  Object* obj;
  Value key;
  EvalLValue(node, obj, key);

  // Retrieve the list
  g_stack.Push(obj->Get(key));
}

void
Evaluator::EvalListAppend(Object* node)
{
  EvalExpression(Ast_A2(node));
  Object* obj = g_stack.Pop();

  List* list = dynamic_cast<List*>(obj);
  if(list == 0)
  {
    throw ScriptException(node, "Invalid type for list append operator");
  }
  
  EvalExpression(Ast_A4(node));
  list->Append(g_stack.Top());
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

// Value
// Evaluator::EvalQualifiedId(Object* node)
// {
//   // Find starting scope
//   NamespaceScope* scope;
//   if(Ast_Type(node) == qualified_id_g)
//   {
//     // Start lookups in global scope
//     scope = m_global;
//   }
//   else
//   {
//     // Start lookups in current namespace
//     scope = FindNamespace(m_scope);
//   }
// 
//   // Perform lookup
//   Object* cur = node;
//   for(;;)
//   {
//     // Retrieve name
//     String name = Ast_A1(cur);
// 
//     // Retrieve node
//     Value const& rval = scope->Get(name);
//     if(rval.Empty())
//     {
//       throw ScriptException(node, "Namespace '" + scope->GetName() + 
//                       "' does not contain a member '" + name + "'");
//     }
// 
//     // If nested, descend, else retrieve value
//     if(!Ast_A2(cur).Empty())
//     {
//       // Retrieve namespace scope
//       scope = dynamic_cast<NamespaceScope*>(rval.GetObject());
//       if(scope == 0)
//       {
//         throw ScriptException(node, "Expected namespace, found function or variable");
//       }
// 
//       // Descend into id
//       cur = Ast_A2(cur);
//     }
//     else
//     {
//       // Found it
//       return rval;
//     }
//   }
// }

void 
Evaluator::EvalUnsetStatement(Object* node)
{
  Object* obj;
  Value key;
  EvalLValue(node, obj, key);

  obj->Unset(key);
}

void
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
  g_stack.Push(oper);
}

// void 
// Evaluator::EvalNamespace(Object* node)
// {
//   // Find current namespace
//   NamespaceScope* curNS = FindNamespace(m_scope);
// 
//   // Determine namespace name
//   String const& name = Ast_A1(node);
// 
//   // Create namespace scope
//   AutoScope as(new NamespaceScope(curNS, name));
// 
//   // Evaluate declarations in scope
//   if(!Ast_A2(node).Empty())
//   {
//     EvalStatement(Ast_A2(node));
//   }
// }

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
    EvalExpression(Ast_A2(node));
    value = g_stack.Pop();
  }

  // Create variable
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
Evaluator::EvalClosure(Object* node)
{
  Function* fun = new ScriptFunction(Ast_A1(node), node);

  fun->Set("name", fun->GetName());
  fun->Set("parent", m_scope);
  fun->Set("scope", FindNamespace(m_scope));
  
  g_stack.Push(fun);
}

void
Evaluator::EvalFunctionMember(Object* node)
{
  String name = Ast_A1(Ast_A1(node));

  Scope* s = m_scope;
  while(s)
  {
    if(Object* object = s->GetObject())
    {
      if(Function* function = dynamic_cast<Function*>(object))
      {
        g_stack.Push(function->Get(name));
      }
    }
    s = s->GetParent();
  }

  throw ScriptException(node, "Invalid scope for function keyword");
}

void
Evaluator::EvalFunctionIndex(Object* node)
{
  throw ScriptException(node, "Fail");
}

void 
Evaluator::EvalExternDeclaration(Object* node)
{
  m_scope->Add(Ast_A1(node), new ExternFunction(Ast_A1(node), node));
}

void
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

  // Stack frame for arguments
  StackFrame s(g_stack);

  // Evaluate arguments
  EvalArguments(node, argsource, args);

  // Evaluate function call
  s.Return(obj->Eval(key, args));
}

Value
Evaluator::EvalScriptCall(ScriptFunction* fun, Arguments& args)
{
  // Class scope
  AutoScope cs;

  // Determine base scope for function
  Scope* parentScope = dynamic_cast<Scope*>(fun->Get("scope").GetObject());

  // Determine parent scope for invocation
  if(args.GetObject())
  {
    // Create class scope
    cs.Set(NewScope(parentScope, args.GetObject()));

    // Adjust parent scope
    parentScope = m_scope;
  }

  // Create function scope
  AutoScope asf(NewScope(parentScope, fun));

  // Create argument scope
  AutoScope asa(NewScope(m_scope));

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
  AutoScope es(NewScope(m_scope));

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
      EvalExpression(*ai);
      args.push_back(g_stack.Pop());
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

void
Evaluator::EvalListLiteral(Object* node)
{
  // Create empty list
  List* list = new List();
  g_stack.Push(list);
  
  // Recurse into map values
  if(!Ast_A1(node).Empty())
  {
    Object* child = Ast_A1(node);
    int index = 0;
    while(child)
    {
      // Add element to list
      EvalExpression(Ast_A1(Ast_A1(child)));
      list->Append(g_stack.Pop());

      // Check for next element
      if(Ast_A2(child).Empty()) 
      {
        break;
      }

      // Set next element
      child = Ast_A2(child);
    }
  }
}

void
Evaluator::EvalMapLiteral(Object* node)
{
  // Create empty map
  Dictionary* dict = new Dictionary();
  g_stack.Push(dict);

  // Recurse into map values
  if(!Ast_A1(node).Empty())
  {
    Object* child = Ast_A1(node);
    int index = 0;
    while(child)
    {
      // Evaluate key and value
      EvalExpression(Ast_A1(Ast_A1(child)));
      Value key = g_stack.Pop();
      EvalExpression(Ast_A2(Ast_A1(child)));
      Value val = g_stack.Pop();

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
}

void
Evaluator::EvalJsonLiteral(Object* node)
{
  // Create empty map
  Value v(new ScriptObject());
  Object* o = v.GetObject();

  // Place on stack
  g_stack.Push(o);

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
        EvalExpression(Ast_A1(Ast_A1(child)));
        key = g_stack.Pop();
      }

//       // Check for duplicate key in object, ignore prototype
      // FIXME
//       if(o->ContainsKey(key, false))
//       {
//         throw ScriptException(node, "Duplicate key in JSON literal");
//       }

      // Evaluate element
      EvalExpression(Ast_A2(Ast_A1(child)));
      Value element = g_stack.Pop();

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
}

void
Evaluator::EvalShellCommand(Object* node)
{
  // Extract command
  String command = Ast_A1(node).GetString();

  // Execute the command
  g_stack.Push(system(command.c_str()));
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
    EvalExpression(Ast_A1(node));
    value = g_stack.Pop();
  }

  // Throw return exception
  throw ReturnException(node, value);
}

void 
Evaluator::EvalIncludeStatement(Object* node)
{
  // Evaluate include expression
  EvalExpression(Ast_A1(node));

  // Parse include file
  ParseFile(ValString(g_stack.Pop()));
}

void 
Evaluator::EvalForStatement(Object* node)
{
  // Outer scope
  AutoScope scope(NewScope(m_scope));
  
  // Evaluate init expression
  EvalStatement(Ast_A1(node));

  // Evaluate loop
  for(;;)
  {
    // Check condition
    EvalExpression(Ast_A2(node));
    if(!ValBool(g_stack.Pop()))
    {
      break;
    }

    // For body
    try 
    {
      AutoScope scope(NewScope(m_scope));
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

    // Discard result
    g_stack.Pop();
  }  
}

void 
Evaluator::EvalForeachStatement(Object* node)
{
  StackFrame s(g_stack);
  AutoScope scope(NewScope(m_scope));
  
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
  EvalExpression(Ast_A2(node));
  Value rhs = g_stack.Top();

  // Retrieve enumerator
  Enumerator* enumerator = rhs->GetEnumerator();
  if(enumerator == 0)
  {
    throw ScriptException(node, "Invalid type specified in foreach");
  }

  // Store enumerator on stack
  g_stack.Push(enumerator);

  // Enumerate members
  Value value;
  while(enumerator->GetNext(value))
  {
    // Assign value to iterator variable
    obj->Set(key, value);

    // Evaluate expression
    try
    {
      AutoScope scope(NewScope(m_scope));
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
Evaluator::EvalWhileStatement(Object* node)
{
  for(;;)
  {
    EvalExpression(Ast_A1(node));
    if(!ValBool(g_stack.Pop()))
    {
      break;
    }

    try
    {
      AutoScope scope(NewScope(m_scope));
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
  EvalExpression(Ast_A1(node));
  Value value = g_stack.Pop();

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
    else 
    {
      EvalExpression(Ast_A1(*it));
      if(ValCmp(g_stack.Pop(), value) == 0)
      {
        statement = Ast_A2(*it);
        break;
      }
    }
  }

  // Execute statement
  if(statement)
  {
    try
    {
      AutoScope as(NewScope(m_scope));
      EvalStatement(statement);
    }
    catch(BreakException const&)
    {
    }
  }
}

void
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
  g_stack.Push(inst);
  
  // Assign prototype object
  inst->Set("prototype", rval.GetObject());

  // Prepare arguments
  // TODO evaluate constructor arguments
  Arguments args;
  args.SetNode(node);
  args.SetObject(inst);

  // Execute constructor
  Value result;
  inst->TryEval("constructor", args, result);
}

void
Evaluator::EvalMemberExpression(Object* node)
{
  // Evaluate lhs
  EvalExpression(Ast_A1(node));
  Value lhs = g_stack.Pop();

  // Retrieve objsect
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
  g_stack.Push(rval);
}

void
Evaluator::EvalThisExpression(Object* node)
{
  Scope* scope = m_scope;
  while(scope)
  {
    Object* object = scope->GetObject();
    if(dynamic_cast<Function*>(object) == 0)
    {
      g_stack.Push(object);
      return;
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
        AutoScope scope(NewScope(m_scope));
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

void
Evaluator::EvalTypeConversion(Object* node)
{
  // Evaluate the expression
  EvalExpression(Ast_A2(node));
  Value rhs = g_stack.Pop();
  
  // Determine type
  Value::Types newType = (Value::Types)Ast_A1(Ast_A1(node)).GetInt();

  // Perform conversion
  g_stack.Push(Convert(node, rhs, newType));
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
