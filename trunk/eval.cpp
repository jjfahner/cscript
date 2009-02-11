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
#include "astlist.h"
#include "native.h"
#include "scope.h"
#include "function.h"
#include "file.h"
#include "lexer.h"
#include "tokens.h"
#include <typeinfo>

//
// Parser functions
//
void *CScriptParseAlloc(void *(*mallocProc)(size_t));
void CScriptParseFree(void *p, void (*freeProc)(void*));
void CScriptParse(void*, int,Token, Evaluator*);

//
// Ratio between alloc and collect
//
static const size_t g_collect_threshold = 1000;

#define ATYPE(arg)  ((*arg)["type"].GetInt())
#define A1(arg)     ((*arg)["a1"])
#define A2(arg)     ((*arg)["a2"])
#define A3(arg)     ((*arg)["a3"])
#define A4(arg)     ((*arg)["a4"])

//////////////////////////////////////////////////////////////////////////
//
// Comparator implementation
//

bool 
ValueComparatorLess::operator () (Value const& lhs, Value const& rhs) const
{
  return Evaluator::Compare(lhs, rhs) < 0;
}


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
    for(size_t i = m_size; i < m_vec.size(); ++i)
    {
      delete m_vec[i];
      m_vec[i] = 0;
    }
    m_vec.resize(m_size);
  }
};

//////////////////////////////////////////////////////////////////////////
//
// Evaluator implementation
//

/*static*/ Scope&
Evaluator::GetGlobalScope()
{
  static Scope scope;
  return scope;
}

Evaluator::Evaluator() :
m_scope   (0),
m_file    (0),
m_allocs  (0)
{
  // Create global scope
  m_global = new Scope(&GetGlobalScope());

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
  // Collect all remaining objects
  Collect();

  // Create new global scope
  delete m_global;
  m_global = new Scope(&GetGlobalScope());
}

void
Evaluator::ParseFile(String const& filename)
{
  // Create file
  File file;
  file.Open(filename);

  // Check type
  if(file.GetType() != File::source)
  {
    throw std::runtime_error("Invalid file");
  }

  // Push file on stack
  File* prevfile = m_file;
  m_file = &file;

  // Try block for file pointer stacking
  try
  {
    ParseText((char const*)file.GetData());
    m_file = prevfile;
  }
  catch(...)
  {
    m_file = prevfile;
    throw;
  }
}

void 
Evaluator::ParseText(char const* text)
{
  // Create scope
  AutoScope as(this);
  if(m_scope == 0)
  {
    as.Set(&GetGlobalScope());
  }

  // Create lexer for file
  Lexer lexer(*this);
  lexer.SetText((char*)text);

  // Push lexer on stack
  Lexer* prevlexer = m_lexer;
  m_lexer = &lexer;

  // Allocate parser
  void *pParser = CScriptParseAlloc(malloc);

  // Try block for parser memory management
  try 
  {
    Token token;
    token.Init();

    // Run parser loop
    while(lexer.Lex(token))
    {
      CScriptParse(pParser, token.m_type, token, this);

#     ifdef _DEBUG
      token.Init();
#     endif
    }

    // Empty token to finalize parse
    CScriptParse(pParser, 0, Token(), this);

    // Destroy parser
    CScriptParseFree(pParser, free);

    // Remove lexer
    m_lexer = prevlexer;
  }
  catch(...)
  {
    // Destroy parser
    CScriptParseFree(pParser, free);

    // Remove lexer
    m_lexer = prevlexer;

    // Rethrow exception
    throw;
  }
}

void 
Evaluator::OnParseFailure()
{
  FilePos pos;
  pos.m_file = m_file ? m_file->GetPath() : "";
  pos.m_line = m_lexer->GetLine();
  m_reporter.ReportError(E0012, &pos);
  throw std::runtime_error("Aborted");
}

void 
Evaluator::OnSyntaxError()
{
  FilePos pos;
  pos.m_file = m_file ? m_file->GetPath() : "";
  pos.m_line = m_lexer->GetLine();
  m_reporter.ReportError(E0013, &pos);
  throw std::runtime_error("Aborted");
}

Object* 
Evaluator::AllocNode(AstTypes type, Value const& a1, Value const& a2, Value const& a3, Value const& a4)
{
  // Set file position
//   FilePos pos;
//   pos.m_file = m_file ? m_file->GetPath() : "";
//   pos.m_line = m_lexer->GetLine();

  Object* obj = new Object;
  (*obj)["type"] = type;
  if(!a1.Empty())(*obj)["a1"] = a1;
  if(!a2.Empty())(*obj)["a2"] = a2;
  if(!a3.Empty())(*obj)["a3"] = a3;
  if(!a4.Empty())(*obj)["a4"] = a4;
  return obj;

  // Create node
//   Ast* node = new Ast(type, a1, a2, a3, a4);  
//   node->m_pos = pos;
//   
//   // Return the new node
//   return node;
}

Object* 
Evaluator::ParseNativeCall(String const& declaration)
{
  // Reset reporter
  m_reporter.Reset();

  // Reset native call pointer
  m_native = 0;

  // Parse the call
  try
  {
    // Parse code
    ParseText(declaration.c_str());

    // Check error count
    if(m_reporter.GetErrorCount())
    {
      cserr << "Aborted.\n";
      return 0;
    }

    // Done
    return m_native;
  }
  catch(...)
  {
    return 0;
  }
}

void 
Evaluator::Collect()
{
  Objects valid;

  // Insert root object in scope
  valid.insert(m_scope ? m_scope : m_global);

  // Append temporaries
  for(size_t i = 0; i < m_temporaries.size(); ++i)
  {
    if(m_temporaries[i]->Type() == Value::tObject)
    {
      valid.insert(m_temporaries[i]->GetObject());
    }
  }

  // Collect invalid objects
  Object::Collect(valid);
}

/*static*/ int 
Evaluator::Compare(Value const& lhs, Value const& rhs)
{
  // Comparing different types
  if(lhs.Type() != rhs.Type())
  {
    // TODO this must be improved
    return int((char*)&lhs - (char*)&rhs);
  }

  // Type-based compare
  switch(lhs.Type())
  {
  case Value::tNull:   
    return 0;

  case Value::tBool:   
    return int(lhs.GetBool()) - int(rhs.GetBool());

  case Value::tInt:    
    return int(lhs.GetInt() - rhs.GetInt());

  case Value::tString: 
    return strcmp(lhs.GetString().c_str(), 
                  rhs.GetString().c_str());
  
  case Value::tObject: 
    return int(lhs.GetObject() - rhs.GetObject());
  }

  // Invalid type
  throw std::runtime_error("Unsupported value type for comparison");
}

/*static*/ Value::Bool   
Evaluator::ValBool(Value const& val)
{
  switch(val.Type())
  {
  case Value::tNull:    return false;
  case Value::tBool:    return val.GetBool();
  case Value::tInt:     return val.GetInt() != 0;
  case Value::tString:  return val.GetString().length() != 0;
  case Value::tObject:  break;
  }
  throw std::runtime_error("Cannot convert between types");
}

/*static*/ Value::Int    
Evaluator::ValInt(Value const& val)
{
  switch(val.Type())
  {
  case Value::tNull:    return 0;
  case Value::tBool:    return val.GetBool() ? 1 : 0;
  case Value::tInt:     return val.GetInt();
  case Value::tString:  return atoi(val.GetString().c_str());
  case Value::tObject:  break; // TODO
  }
  throw std::runtime_error("Cannot convert between types");
}

inline 
Value::String ToString(Value::Int val)
{
  char buf[25];
  sprintf(buf, "%d", val);
  return buf;
}

/*static*/ Value::String 
Evaluator::ValString(Value const& val)
{
  switch(val.Type())
  {
  case Value::tNull:    return Value::String();
  case Value::tBool:    return val.GetBool() ? "true" : "false";
  case Value::tInt:     return ToString(val.GetInt());
  case Value::tString:  return val.GetString();
  case Value::tObject:  return typeid(val.GetObject()).name();
  }
  throw std::runtime_error("Cannot convert between types");
}

/*static*/ Value 
Evaluator::ValAdd(Value const& lhs, Value const& rhs)
{
  switch(lhs.Type())
  {
  case Value::tInt:     return lhs.GetInt() + ValInt(rhs);
  case Value::tString:  return lhs.GetString() + ValString(rhs);
  case Value::tObject:  break; // TODO
  }
  throw std::runtime_error("Invalid type(s) for addition operator");
}

/*static*/ Value 
Evaluator::ValSub(Value const& lhs, Value const& rhs)
{
  switch(lhs.Type())
  {
  case Value::tInt:     return lhs.GetInt() - ValInt(rhs);
  case Value::tObject:  break; // TODO
  }
  throw std::runtime_error("Invalid type(s) for subtraction operator");
}

/*static*/ Value 
Evaluator::ValMul(Value const& lhs, Value const& rhs)
{
  switch(lhs.Type())
  {
  case Value::tInt:     return lhs.GetInt() * ValInt(rhs);
  case Value::tObject:  break; // TODO
  }
  throw std::runtime_error("Invalid type(s) for multiplication operator");
}

/*static*/ Value 
Evaluator::ValDiv(Value const& lhs, Value const& rhs)
{
  switch(lhs.Type())
  {
  case Value::tInt:     return lhs.GetInt() / ValInt(rhs);
  case Value::tObject:  break; // TODO
  }
  throw std::runtime_error("Invalid type(s) for division operator");
}

/*static*/ Value 
Evaluator::ValMod(Value const& lhs, Value const& rhs)
{
  switch(lhs.Type())
  {
  case Value::tInt:     return lhs.GetInt() % ValInt(rhs);
  case Value::tObject:  break; // TODO
  }
  throw std::runtime_error("Invalid type(s) for modulo operator");
}

/*static*/ Value 
Evaluator::ValNeg(Value const& lhs)
{
  switch(lhs.Type())
  {
  case Value::tInt:     return -lhs.GetInt();
  case Value::tObject:  break; // TODO
  }
  throw std::runtime_error("Invalid type(s) for negation operator");
}

/*static*/ Value 
Evaluator::ValNot(Value const& lhs)
{
  switch(lhs.Type())
  {
  case Value::tBool:    return lhs.GetBool() == false;
  case Value::tInt:     return lhs.GetInt()  == 0;
  case Value::tObject:  break; // TODO
  }
  throw std::runtime_error("Invalid type(s) for negation operator");
}

inline void 
PrintLineInfo(script_exception const& e)
{
  // TODO ast
//   if(e.m_node && !e.m_node->m_pos.m_file.empty())
//   {
//     csout
//       << "\n"
//       << e.m_node->m_pos.m_file 
//       << "("
//       << e.m_node->m_pos.m_line
//       << ") : ";
//   }
}

Value 
Evaluator::Eval(String text, bool isFileName)
{
  // Reset reporter
  m_reporter.Reset();

  // Create top-level scope
  AutoScope gs(this, m_global);

  try
  {
    // Parse code - this evaluates 
    // every statement in the code
    if(isFileName)
    {
      ParseFile(text);
    }
    else
    {
      ParseText(text.c_str());
    }

    // Collect remaining objects
    Collect();

    // Check error count
    if(m_reporter.GetErrorCount())
    {
      csout << "Aborted.\n";
      return Value();
    }
  }
  // Return statement
  catch(return_exception const& e)
  {
    return e.m_value;
  }
  // Reset statement
  catch(reset_exception const&)
  {
    Reset();
  }
  // Uncaught user_exception in script
  catch(user_exception const& e)
  {
    PrintLineInfo(e);
    cserr << "Error: Uncaught exception '" << e.m_value.GetString() << "'\n";
  }
  // Invalid break statement
  catch(break_exception const& e)
  {
    PrintLineInfo(e);
    cserr << "Error: Invalid break statement\n";
  }
  // Invalid continue statement
  catch(continue_exception const& e)
  {
    PrintLineInfo(e);
    cserr << "Error: Invalid continue statement\n";
  }
  // Unexpected exception in script
  catch(script_exception const& e)
  {
    PrintLineInfo(e);
    cserr << "Runtime error: " << e.what() << "\n";
  }
  // System exception
  catch(std::exception const& e)
  {
    cserr << "\nError: " << e.what() << "\n";
  }
  // Unknown exception type
  catch(...)
  {
    cserr << "\nError: Unexpected exception\n";
  }
  return Value();
}

void
Evaluator::Eval(Object* astRoot)
{
  // Keep ast around during evaluation
  MakeTemp(astRoot);

  // Perform evaluation of ast tree
  EvalStatement(astRoot);
}


void 
Evaluator::EvalStatement(Object* node)
{
  VecRestore<ValueVec> vr(m_temporaries);

  switch(ATYPE(node))
  {
  case empty_statement:       break;

  case translation_unit:      EvalStatement(A1(node));    break;
  case statement_sequence:    EvalStatementSeq(node);     break;
  case expression_statement:  EvalExpression(A1(node));   break;
  case variable_declaration:  EvalVarDecl(node);          break;
  case function_declaration:  EvalFunDecl(node);          break;
  case extern_declaration:    EvalExternDecl(node);       break;
  case try_statement:         EvalTryStatement(node);     break;
  case include_statement:     EvalIncludeStatement(node); break;
  case for_statement:         EvalForStatement(node);     break;
  case foreach_statement:     EvalForeachStatement(node); break;
  case if_statement:          EvalIfStatement(node);      break;
  case while_statement:       EvalWhileStatement(node);   break;  
  case return_statement:      EvalReturnStatement(node);  break;
  case switch_statement:      EvalSwitchStatement(node);  break;

  case break_statement:       throw break_exception(node);  
  case continue_statement:    throw continue_exception(node);
  case throw_statement:       throw user_exception(node, 
                                  EvalExpression(A1(node)));

  case declaration_sequence:
    EvalStatement(A1(node));
    EvalStatement(A2(node));
    break;

  case compound_statement:
    if(A1(node))
    {
      AutoScope as(this, new Scope(m_scope));
      EvalStatement(A1(node));
    }
    break;

  default: throw script_exception(node, "Invalid node type");
  }
}

RValue&
Evaluator::EvalExpression(Object* node)
{
  switch(ATYPE(node))
  {
  case assignment_expression: return EvalAssignment(node);
  case binary_expression:     return EvalBinary(node);
  case ternary_expression:    return EvalTernary(node);
  case prefix_expression:     return EvalPrefix(node);
  case postfix_expression:    return EvalPostfix(node);
  case index_expression:      return EvalIndex(node);
  case function_call:         return EvalFunctionCall(node);
  case literal_value:         return MakeTemp(A1(node));
  case lvalue:                return EvalLValue(node);
  case list_literal:          return EvalListLiteral(node);
  case json_literal:          return EvalJsonLiteral(node);
  case new_expression:        return EvalNewExpression(node);
  case this_expression:       return EvalThisExpression(node);
  case member_expression:     return EvalMemberExpression(node);  
  case conversion_expression: return EvalConversion(node);
  case closure_declaration:   return EvalClosure(node);
  case xml_expression:        return EvalXmlExpression(node);
  case function_member_expression:  return EvalFunctionMember(node);
  case function_index_expression:   return EvalFunctionIndex(node);
  }
  throw script_exception(node, "Invalid expression type");
}

RValue&
Evaluator::EvalAssignment(Object* node)
{
  // Evaluate left-hand side
  LValue& lhs = EvalExpression(A2(node)).LVal();

  // Evaluate right-hand side
  RValue& rhs = EvalExpression(A3(node));

  // Perform assignment
  switch(A1(node).GetInt())
  {
  case op_assign: return lhs = rhs;
  case op_assadd: return lhs = ValAdd(lhs, rhs);
  case op_asssub: return lhs = ValSub(lhs, rhs);
  case op_assmul: return lhs = ValMul(lhs, rhs);
  case op_assdiv: return lhs = ValDiv(lhs, rhs); 
  case op_assmod: return lhs = ValMod(lhs, rhs);
  }
  throw script_exception(node, "Invalid assignment operator");
}

RValue&
Evaluator::EvalBinary(Object* node)
{
  Value result;
  switch(A1(node).GetInt())
  {
  case op_add:    result = ValAdd (EvalExpression(A2(node)), EvalExpression(A3(node))); break;
  case op_sub:    result = ValSub (EvalExpression(A2(node)), EvalExpression(A3(node))); break;
  case op_mul:    result = ValMul (EvalExpression(A2(node)), EvalExpression(A3(node))); break;
  case op_div:    result = ValDiv (EvalExpression(A2(node)), EvalExpression(A3(node))); break;
  case op_mod:    result = ValMod (EvalExpression(A2(node)), EvalExpression(A3(node))); break;
  case op_eq:     result = Compare(EvalExpression(A2(node)), EvalExpression(A3(node))) == 0; break;
  case op_ne:     result = Compare(EvalExpression(A2(node)), EvalExpression(A3(node))) != 0; break;
  case op_lt:     result = Compare(EvalExpression(A2(node)), EvalExpression(A3(node))) <  0; break;
  case op_le:     result = Compare(EvalExpression(A2(node)), EvalExpression(A3(node))) <= 0; break;
  case op_gt:     result = Compare(EvalExpression(A2(node)), EvalExpression(A3(node))) >  0; break;
  case op_ge:     result = Compare(EvalExpression(A2(node)), EvalExpression(A3(node))) >= 0; break;

  case op_logor:  result = ValBool(EvalExpression(A2(node))) || 
                           ValBool(EvalExpression(A3(node))) ; break;
  case op_logand: result = ValBool(EvalExpression(A2(node))) && 
                           ValBool(EvalExpression(A3(node))) ; break;

  case op_seq:    result = Compare(EvalExpression(A2(node)), EvalExpression(A3(node))) == 0; break;
  case op_sne:    result = Compare(EvalExpression(A2(node)), EvalExpression(A3(node))) != 0; break;

  default: throw script_exception(node, "Invalid binary operator");
  }  
  
  return MakeTemp(result);
}

RValue&
Evaluator::EvalTernary(Object* node)
{
  if(EvalExpression(A1(node)).GetBool())
  {
    return EvalExpression(A2(node));
  }
  return EvalExpression(A3(node));
}

void 
Evaluator::EvalStatementSeq(Object* node)
{
  AstList::const_iterator it, ie;
  AstList list(A1(node));
  it = list.begin();
  ie = list.end();
  for(; it != ie; ++it)
  {
    EvalStatement(*it);
  }
}

RValue&
Evaluator::EvalPrefix(Object* node)
{
  RValue& lhs = EvalExpression(A2(node));
  switch(A1(node).GetInt())
  {
  case op_preinc:
    lhs.LVal() = ValAdd(lhs, 1); 
    return lhs;
  case op_predec: 
    lhs.LVal() = ValSub(lhs, 1); 
    return lhs;
  case op_negate: 
    return MakeTemp(ValNeg(lhs));
  case op_not:    
    return MakeTemp(ValNot(lhs));
  }
  throw script_exception(node, "Invalid prefix operator");
}

RValue&
Evaluator::EvalPostfix(Object* node)
{
  // Evaluate lhs
  LValue& lhs = EvalExpression(A2(node)).LVal();

  // Create temporary with result value
  RValue& result = MakeTemp(lhs);

  // Perform postfix operation
  switch(A1(node).GetInt())
  {
  case op_postinc: lhs.SetValue(ValAdd(lhs, 1)); break;
  case op_postdec: lhs.SetValue(ValSub(lhs, 1)); break;
  default: script_exception(node, "Invalid postfix operator");
  }
  
  // Done
  return result;
}

RValue&
Evaluator::EvalIndex(Object* node)
{
  // Check type of left-hand side
  RValue& lhs = EvalExpression(A1(node));
  switch(lhs.Type())
  {
  case Value::tNull:
    lhs.LVal() = new Object();
    break;
  case Value::tObject:
    // Fine
    break;
  default:  
    throw script_exception(node, "Invalid type for index operator");
  }

  // Locate value
  RValue* val = 0;
  if(A2(node).Empty())
  {
    // Use size as key
    Value key = (Value::Int)lhs.GetObject()->Members().size();

    // Make sure this key doesn't exist.
    if(lhs.GetObject()->Find(key, val))
    {
      throw std::runtime_error("Key to add already exists");
    }
    
    // Add value
    val = lhs.GetObject()->Add(key, new RWVariable());
  }
  else
  {
    // Evaluate index expression
    RValue& rhs = EvalExpression(A2(node));

    // Retrieve value
    if(!lhs.GetObject()->Find(rhs, val))
    {
      val = lhs->Add(rhs, new RWVariable());
    }
  }
  
  // Done
  return StoreTemp(new BoundLValue(val->LVal(), lhs));
}

RValue&
Evaluator::EvalLValue(Object* node)
{
  RValue* ptr;
  Object* owner;
  if(m_scope->Lookup(A1(node), ptr, owner))
  {
    return owner ? StoreTemp(BoundValue::Create(*ptr, owner)) : *ptr;
  }
  throw script_exception(node, "Undeclared variable '" + A1(node).GetString() + "'");
}

void
Evaluator::EvalVarDecl(Object* node)
{
  // Determine right-hand value
  Value value;
  if(A2(node).Empty())
  {
    value = Value();
  }
  else
  {
    // Assign value-of rhs
    value = EvalExpression(A2(node));
  }

  // Create variable
  // TODO deletion of newed variable
  m_scope->Add(A1(node).GetString(), new RWVariable(value));
}

void
Evaluator::EvalFunDecl(Object* node)
{
  Function* fun = new ScriptFunction(A1(node), node);
  fun->Members()["name"]   = new ROVariable(fun->GetName());
  fun->Members()["parent"] = new ROVariable(m_scope);
  m_scope->Add(A1(node).GetString(), new ROVariable(fun));
}

RValue& 
Evaluator::EvalClosure(Object* node)
{
  Function* fun = new ScriptFunction(A1(node), node);
  fun->Members()["name"]   = new ROVariable(fun->GetName());
  fun->Members()["parent"] = new ROVariable(m_scope);
  return MakeTemp(fun);
}

RValue& 
Evaluator::EvalFunctionMember(Object* node)
{
  String name = A1(A1(node));

  Scope* s = m_scope;
  while(s)
  {
    if(ObjectScope* c = dynamic_cast<ObjectScope*>(s))
    {
      if(Function* f = dynamic_cast<Function*>(c->GetObject()))
      {
        RValue* rval;
        if(!f->Find(name, rval))
        {
          rval = new RWVariable();
          f->Members()[name] = rval;
        }
        return *rval;
      }
    }
    s = s->GetParent();
  }

  throw script_exception(node, "Invalid scope for function keyword");
}

RValue& 
Evaluator::EvalFunctionIndex(Object* node)
{
  throw script_exception(node, "Fail");
}

void 
Evaluator::EvalExternDecl(Object* node)
{
  m_scope->Add(A1(node).GetString(), 
    new ROVariable(new ExternFunction(A1(node), node)));
}

RValue&
Evaluator::EvalFunctionCall(Object* node)
{
  RValue* rval  = 0;
  Object* owner = 0;

  // Resolve function pointer
  if(ATYPE(A1(node)) == lvalue)
  {
    // Named objects are found through the current scope
    if(!m_scope->Lookup(String(A1(A1(node))), rval, owner))
    {
      throw script_exception(node, "Function not found");
    }
  }
  else
  {
    // Expressions are evaluated in the current scope
    rval = &EvalExpression(A1(node));

    // Add object context to arguments
    if(BoundValue* bval = dynamic_cast<BoundValue*>(rval))
    {
      owner = bval->GetBoundObject();
    }
  }

  // Cast result to function
  Function* fun = ValueToType<Function>(*rval);
  if(fun == 0)
  {
    throw script_exception(node, "Function call on non-function object");
  }

  // Continue in overload
  return EvalFunctionCall(node, fun, owner, A2(node));
}

RValue& 
Evaluator::EvalFunctionCall(Object* node, Function* fun, Object* owner, Object* arguments)
{
  Arguments args;

  // Add object context to arguments
  args.SetObject(owner);

  // Add parameters to arguments
  args.SetParameters(fun->GetParameters());

  // Evaluate arguments
  AstList arglist(A1(arguments));
  if(ATYPE(arguments) == positional_arguments)
  {
    EvalPositionalArguments(node, fun, &arglist, args);
  }
  else
  {
    EvalNamedArguments(node, fun, &arglist, args);
  }

  // Evaluate function
  try
  {
    return MakeTemp(fun->Execute(this, args));
  }
  catch(return_exception const& e)
  {
    return MakeTemp(e.m_value);
  }
}

RValue&
Evaluator::EvalScriptCall(ScriptFunction* fun, Arguments& args)
{
  // Class scope
  AutoScope cs(this);

  // Determine parent scope
  Scope* parentScope = m_global;
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
  AstList::const_iterator pi, pe;
  AstList parlist(fun->GetParameters());
  pi = parlist.begin();
  pe = parlist.end();
  for(size_t index = 0; pi != pe; ++pi, ++index)
  {
    m_scope->Add(A1(*pi), new RWVariable(args[index]));
  }

  // Create function execution scope
  AutoScope es(this, new Scope(m_scope));

  // Execute expression
  try
  {
    EvalStatement(A3(fun->GetNode()));
    return MakeTemp(Value());
  }
  catch(return_exception const& e)
  {
    return MakeTemp(e.m_value);
  }
}

void 
Evaluator::EvalPositionalArguments(Object* node, Function* fun, AstList const* arglist, Arguments& args)
{
  // No formal parameter list
  if(fun->GetParameters() == 0)
  {
    // Evaluate arguments
    AstList::const_iterator ai = arglist->begin();
    for(; ai != arglist->end(); ++ai)
    {
      args.push_back(EvalExpression(*ai));
    }

    // Done
    return;
  }

  // Retrieve formal parameters
  AstList parlist(fun->GetParameters());

  // Enumerate parameters
  AstList::const_iterator pi, pe, ai, ae;
  pi = parlist.begin();
  pe = parlist.end();
  ai = arglist->begin();
  ae = arglist->end();
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
      throw script_exception(node, "Too many arguments in call to '" + fun->GetName() + "'");
    }

    // Variadic parameter
    if(A2(*pi).GetInt() == ptVariadic)
    {
      // Insert map
      Object* va = new Object();
      args.push_back(va);

      // Insert remaining arguments
      for(; ai != ae; ++ai)
      {
        // Evaluate argument value. ATTN: this dereference
        // is intentional; variadic arguments are by value.
        Value val = EvalExpression(*ai);

        // Append to list
        va->Members()[va->Members().size() - 1] = new RWVariable(val);
      }

      // Done
      break;
    }

    // End of arguments
    Value value;
    if(ai == ae)
    {
      // Must have default value
      if(!A4(*pi))
      {
        throw script_exception(node, "Not enough arguments in call to '" + fun->GetName() + "'");
      }

      // Evaluate default value
      value = EvalExpression(A4(*pi));
    }
    else
    {
      // Evaluate argument
      value = EvalExpression(*ai);
    }

    // Handle byref/byval
    if(A2(*pi).GetInt() == ptByVal)
    {
      // Dereference the value
//       value = *value;
    }

    // Apply type conversion to value
    if(A3(*pi))
    { 
      // TODO ast
      PerformConversion(value, A3(*pi).GetObject());
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
}

void 
Evaluator::EvalNamedArguments(Object* node, Function* fun, AstList const* arglist, Arguments& args)
{
  // TODO: validate superfluous/duplicate arguments
  // TODO: implement type conversions

  AstList parlist(fun->GetParameters());

  // Enumerate parameters
  AstList::const_iterator pi, pe;
  pi = parlist.begin();
  pe = parlist.end();
  for(; pi != pe; ++pi)
  {
    // Extract parameter name
    String parname = A2(*pi);

    // Cannot supply named arguments for variadic parameter
    if(A2(*pi).GetInt() == ptVariadic)
    {
      // Variadic is always last in list - add empty list and stop
//      args.push_back(Value::stAssoc);
      break;
    }

    // Find named argument for parameter
    AstList::const_iterator ai, ae;
    ai = arglist->begin();
    ae = arglist->end();
    for(; ai != ae; ++ai)
    {
      if(A1(*ai).GetString() == parname)
      {
        break;
      }
    }

    // Evaluate argument
    Value value;
    if(ai != ae)
    {
      // Fill in positional argument
      value = EvalExpression(A2(*ai));
    }
    else if(A3(*pi))
    {
      // Evaluate default value
      value = EvalExpression(A3(*pi));
    }
    else
    {
      // Missing argument
      throw script_exception(node, "No value specified for parameter '" + parname + "'");
    }

    // Assign by value/by ref
    if(A4(*pi) && A4(*pi).GetInt() == ptByRef)
    {
      // TODO validate type is correct for byref?
      args.push_back(value);
    }
    else
    {
      // TODO execute conversion to type
      args.push_back(value);
    }
  }
}

RValue&
Evaluator::EvalListLiteral(Object* node)
{
  // Create empty map
  Value v(new Object());
  Object* o = v.GetObject();
  
  // Recurse into map values
  if(!A1(node).Empty())
  {
    Object* child = A1(node);
    int index = 0;
    while(child)
    {
      // Evaluate element
      RValue const& element = EvalExpression(A1(A1(child)));

      // Evaluate key
      Value key = index++;
      if(A2(A1(child)))
      {
        key = EvalExpression(A2(A1(child)));
      }

      // Insert into member variables
      o->Members()[key] = new RWVariable(element);

      // Check for next element
      if(!A2(child)) 
      {
        break;
      }

      // Set next element
      child = A2(child);
    }
  }

  // Done
  return MakeTemp(v);
}

RValue&
Evaluator::EvalJsonLiteral(Object* node)
{
  // Create empty map
  Value v(new Object());
  Object* o = v.GetObject();

  // Recurse into map values
  if(!A1(node).Empty())
  {
    Object* child = A1(node);
    while(child)
    {
      // Retrieve and check key
      String key = A1(A1(child));
      if(o->Members().count(key))
      {
        throw script_exception(node, "Duplicate key in JSON literal");
      }

      // Evaluate element
      RValue const& element = EvalExpression(A2(A1(child)));

      // Insert into member variables
      o->Members()[key] = new RWVariable(element);

      // Check for next element
      if(!A2(child)) 
      {
        break;
      }

      // Set next element
      child = A2(child);
    }
  }

  // Done
  return MakeTemp(v);
}

void 
Evaluator::EvalReturnStatement(Object* node)
{
  // Determine return value
  Value value;
  if(A1(node).Empty())
  {
    value = Value();
  }
  else
  {
    value = EvalExpression(A1(node));
  }

  // Throw return exception
  throw return_exception(node, value);
}

void 
Evaluator::EvalIncludeStatement(Object* node)
{
  ParseFile(A1(node));
}

void 
Evaluator::EvalForStatement(Object* node)
{
  // Outer scope
  AutoScope scope(this, new Scope(m_scope));
  
  // Evaluate init expression
  EvalStatement(A1(node));

  // Evaluate loop
  for(;;)
  {
    // Evaluate condition
    RValue const& cond = EvalExpression(A2(node));
    if(cond.Type() != Value::tBool)
    {
      throw script_exception(A2(node), "Expression does not yield a boolean value");
    }

    // Check condition
    if(!EvalExpression(A2(node)).GetBool())
    {
      break;
    }

    // For body
    try 
    {
      AutoScope scope(this, new Scope(m_scope));
      EvalStatement(A4(node));
    }
    catch(break_exception const&)
    {
      break;
    }
    catch(continue_exception const&)
    {
    }

    // Evaluate post expression
    EvalExpression(A3(node));
  }  
}

void 
Evaluator::EvalForeachStatement(Object* node)
{
  AutoScope scope(this, new Scope(m_scope));

  String varName;

  // Determine variable name
  if(ATYPE(A1(node)) == variable_declaration)
  {
    EvalStatement(A1(node));
    varName = A1(A1(node));
  }
  else
  {
    varName = A1(node);
  }

  // Fetch variable
  RValue* rval;
  Object* owner;
  if(!m_scope->Lookup(varName, rval, owner))
  {
    throw script_exception(node, "Failed to find iterator variable");
  }

  // Convert to lvalue
  LValue& var = rval->LVal();

  // Evaluate expression
  RValue& rhs = EvalExpression(A2(node));

  // Retrieve enumerator
  Enumerator* enumerator = rhs.GetEnumerator();
  if(enumerator == 0)
  {
    throw script_exception(node, "Invalid type specified in foreach");
  }

  // Add to local scope
  MakeTemp(enumerator);

  // Enumerate members
  Value itVal;
  while(enumerator->GetNext(itVal))
  {
    // Assign value to iterator variable
    var = itVal;

    // Evaluate expression
    try
    {
      AutoScope scope(this, new Scope(m_scope));
      EvalStatement(A3(node));
    }
    catch(break_exception const&)
    {
      break;
    }
    catch(continue_exception const&)
    {
    }
  }
}

void
Evaluator::EvalIfStatement(Object* node)
{
  RValue const& cond = EvalExpression(A1(node));
  if(cond.Type() != Value::tBool)
  {
    throw script_exception(A1(node), "Expression does not yield a boolean value");
  }

  if(cond.GetBool())
  {
    EvalStatement(A2(node));
  }
  else if(A3(node))
  {
    EvalStatement(A3(node));
  }
}

void        
Evaluator::EvalWhileStatement(Object* node)
{
  for(;;)
  {
    RValue const& cond = EvalExpression(A1(node));
    if(cond.Type() != Value::tBool)
    {
      throw script_exception(A1(node), "Expression does not yield a boolean value");
    }

    if(!cond.GetBool())
    {
      break;
    }

    try
    {
      AutoScope scope(this, new Scope(m_scope));
      EvalStatement(A2(node));
    }
    catch(break_exception const&)
    {
      break;
    }
    catch(continue_exception const&)
    {
    }
  }
}

void
Evaluator::EvalSwitchStatement(Object* node)
{
  // Switch value
  Value value = EvalExpression(A1(node));

  // Create iterators
  AstList cases(A2(node));
  AstList::const_iterator it = cases.begin();
  AstList::const_iterator ie = cases.end();

  // Find case that matches switch value
  Object* statement = 0;
  for(; it != ie; ++it)
  {
    if(ATYPE(*it) == default_case)
    {
      if(statement)
      {
        throw script_exception(node, "More than one default case in switch statement");
      }
      statement = A1(*it);
    }
    else if(Compare(EvalExpression(A1(*it)), value) == 0)
    {
      statement = A2(*it);
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
    catch(break_exception const&)
    {
    }
  }
}

RValue&
Evaluator::EvalNewExpression(Object* node)
{
  // Find object
  RValue* rval;
  if(!m_scope->Lookup(A1(node), rval))
  {
    throw script_exception(node, "Variable not found");
  }

  // Must be an object
  if(rval->Type() != Value::tObject)
  {
    throw script_exception(node, "Cannot instantiate a non-object variable");
  }

  // Create a clone
  Object* inst = rval->GetObject()->Clone();

  // Invoke constructor when instantiating a function
  if(Function* fun = dynamic_cast<Function*>(inst))
  {
    // Evaluate constructor
    EvalFunctionCall(node, fun, inst, A2(node));
  }

  // Return temporary
  return MakeTemp(inst);
}

RValue&
Evaluator::EvalMemberExpression(Object* node)
{
  // Retrieve left-hand side
  Object* object = ValueToType<Object>(EvalExpression(A1(node)));
  if(object == 0)
  {
    throw std::runtime_error("Left-hand side does not yield an object");
  }

  // Lookup right-hand side
  RValue* rval;
  if(!object->Find(String(A1(A2(node))), rval))
  {
    rval = new RWVariable();
    object->Add(A1(A2(node)), rval);
  }

  // Construct bound member
  if(LValue* lval = dynamic_cast<LValue*>(rval))
  {
    return StoreTemp(new BoundLValue(*lval, object));
  }
  else
  {
    return StoreTemp(new BoundRValue(*rval, object));
  }
}

RValue&
Evaluator::EvalThisExpression(Object* node)
{
  Scope* scope = m_scope;
  while(scope)
  {
    ObjectScope* cs = dynamic_cast<ObjectScope*>(scope);
    if(cs && dynamic_cast<Function*>(cs->GetObject()) == 0)
    {
      return MakeTemp(cs->GetObject());
    }
    scope = scope->GetParent();
  }
  throw script_exception(node, "Invalid context for 'this'");
}

void 
Evaluator::EvalTryStatement(Object* node)
{
  try
  {
    try 
    {
      // Execute guarded block
      EvalStatement(A1(node));
    }
    catch(user_exception const& e)
    {
      // Handle only when handler is present
      if(A2(node))
      {
        // Insert exception into scope
        // TODO this maketemp might crash horribly during exception cleanup!!!
        AutoScope scope(this, new Scope(m_scope));
        m_scope->Add(A1(A2(node)), new RWVariable(e.m_value));

        // Evaluate catch block
        EvalStatement(A2(A2(node)));
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
    if(A3(node))
    {
      // Evaluate finally
      EvalStatement(A1(A3(node)));
    }

    // Re-throw exception from catch block
    throw;
  }

  // Evaluate finally
  if(A3(node))
  {
    EvalStatement(A1(A3(node)));
  }
}

RValue&
Evaluator::EvalConversion(Object* node)
{
  // Evaluate expression
  // ATTN: the implicit conversion from RValue& to Value
  // is intended: the conversion must not be applied to 
  // the source for the conversion!!!
  Value value = EvalExpression(A2(node));

  // Perform the conversion
  // TODO ast
  //PerformConversion(value, A1(node).GetNode());
  
  // Return converted value
  return MakeTemp(value);
}

void 
Evaluator::PerformConversion(Value& value, TypeInfo const& newType)
{
  // Determine old type
  TypeInfo oldType(value);

  // Ignore when equal
  if(oldType == newType)
  {
    return;
  }

  // Convert to basic types
  switch(newType.GetType())
  {
  case Value::tNull:    break;
  case Value::tBool:    value = ValBool(value);   break;
  case Value::tInt:     value = ValInt(value);    break;
  case Value::tString:  value = ValString(value); break;
  default:              throw std::runtime_error("Invalid conversion");
  }
}

//////////////////////////////////////////////////////////////////////////

enum XmlNodeTypes
{
  xmlUnknown = 0,
  xmlElement = 1,
  xmlAttribute = 2, 	
  xmlText = 3,
  xmlCDATASection = 4,
  xmlEntityReference = 5, 	
  xmlEntity = 6,
  xmlProcessingInstruction = 7,
  xmlComment = 8,
  xmlDocument = 9,
  xmlDocumentType = 10,
  xmlDocumentFragment = 11,
  xmlNotation = 12
};

String 
XmlNodeName(XmlNodeTypes nodeType)
{
  switch(nodeType)
  {
  case xmlElement:                return "#element";
  case xmlAttribute:              return "#attribute";
  case xmlText:                   return "#text";
  case xmlCDATASection:           return "#cdata-section";
  case xmlEntityReference:        return "#entity-reference";
  case xmlEntity:                 return "#entity";
  case xmlProcessingInstruction:  return "#processing-instruction";
  case xmlComment:                return "#comment";
  case xmlDocument:               return "#document";
  case xmlDocumentType:           return "#document-type";
  case xmlDocumentFragment:       return "#document-fragment";
  case xmlNotation:               return "#notation";
  default:                        throw std::runtime_error("Invalid XML node type");
  }
}
Object* 
CreateXmlObject(XmlNodeTypes nodeType, Object* parentNode = 0)
{
  // Create new node
  Object* childNode = new Object();

  // Create members
  childNode->LVal("nodeType")     = (Value::Int)nodeType;
  childNode->LVal("nodeName")     = XmlNodeName(nodeType);
  childNode->LVal("nodeTypeName") = XmlNodeName(nodeType);

  // Append to parent node
  if(parentNode)
  {
    childNode->LVal("parentNode") = parentNode;
    Object* coll = nodeType == xmlAttribute ?
      parentNode->LVal("attributes").GetObject() :
    parentNode->LVal("childNodes").GetObject() ;
    coll->LVal(coll->Members().size()) = childNode;
  }

  // Create complex members
  switch(nodeType)
  {
  case xmlDocument:
  case xmlElement:
    childNode->LVal("childNodes") = new Object();
    // Intentional fallthrough
  case xmlProcessingInstruction:
    childNode->LVal("attributes") = new Object();
    break;
  }

  // Done
  return childNode;
}

void 
SetNodeName(Object* ast, Object* node)
{
  String qname;
  String lname;
  String ns;
  if(ATYPE(ast) == xml_qname)
  {
    lname = A2(ast);
    ns    = A1(ast);
    qname = ns + ":" + lname;
  }
  else
  {
    lname = A1(ast);
    qname = lname;
  }

  node->LVal("nodeName")      = lname;
  node->LVal("localName")     = lname;
  node->LVal("qualifiedName") = qname;
  node->LVal("namespace")     = ns;
}

void 
AddXmlAttributes(Object* ast, Object* node)
{
  // Retrieve ast element list
  AstList list(A1(ast));

  // Walk through list
  AstList::iterator it = list.begin();
  for(; it != list.end(); ++it)
  {
    Object* attr = CreateXmlObject(xmlAttribute, node);
    SetNodeName(A1(*it), attr);
    attr->LVal("value") = A2(*it);
  }
}

RValue& 
Evaluator::EvalXmlExpression(Object* node)
{
  // Retrieve ast element list
  AstList list(A1(A1(node)));

  // Create document element
  Object* doc = CreateXmlObject(xmlDocument);

  // Setup temporaries
  Object* cur = doc;
  Object* tmp;

  // Walk through list
  AstList::iterator it = list.begin();
  for(; it != list.end(); ++it)
  {
    switch(ATYPE(*it))
    {
    case xml_processing_instruction:
      tmp = CreateXmlObject(xmlProcessingInstruction, cur);
      tmp->LVal("nodeName") = A1(*it).GetString();
      if(A2(*it))
      {
        AddXmlAttributes(A2(*it), tmp);
      }
      break;

    case xml_open_tag:
      tmp = CreateXmlObject(xmlElement, cur);
      cur = tmp;
      SetNodeName(A1(*it), cur);
      if(A2(*it))
      {
        AddXmlAttributes(A2(*it), tmp);
      }
      break;

    case xml_close_tag:
      if(cur == 0)
      {
        throw script_exception(node, "Invalid xml structure");
      }
      cur = cur->RVal("parentNode").GetObject();
      break;

    case xml_closed_tag:
      tmp = CreateXmlObject(xmlElement, cur);
      SetNodeName(A1(*it), tmp);
      if(A2(*it))
      {
        AddXmlAttributes(A2(*it), tmp);
      }
      break;

    case xml_text:
      tmp = CreateXmlObject(xmlText, cur);
      tmp->LVal("data")   = A1(*it).GetString();
      tmp->LVal("length") = A1(*it).GetString().length();
      break;

    default:
      throw script_exception(node, "Unexpected xml node type");
    }
  }

  // Check whether we've gotten back to the document element
  if(cur != doc)
  {
    throw script_exception(node, "Invalid xml structure");
  }

  // TODO Set document element

  // Done
  return MakeTemp(doc);
}
