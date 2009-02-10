//////////////////////////////////////////////////////////////////////////
//
// This file is � 2007 - 2009 JJ Fahner <jan-jaap@jan-jaap.net>
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
  // Members
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
Evaluator::AllocNode(AstTypes type)
{
  Object* node = new Object;
  (*node)["type"] = type;
  (*node)["file"] = m_file ? m_file->GetPath() : "";
  (*node)["line"] = m_lexer->GetLine();
  return node;
}

Object* 
Evaluator::AllocNode(AstTypes type, Value const& a1)
{
  Object* node = new Object;  
  (*node)["type"] = type;
  (*node)["a1"]   = a1;
  (*node)["file"] = m_file ? m_file->GetPath() : "";
  (*node)["line"] = m_lexer->GetLine();
  return node;
}

Object* 
Evaluator::AllocNode(AstTypes type, Value const& a1, Value const& a2)
{
  Object* node = new Object;  
  (*node)["type"] = type;
  (*node)["a1"]   = a1;
  (*node)["a2"]   = a2;
  (*node)["file"] = m_file ? m_file->GetPath() : "";
  (*node)["line"] = m_lexer->GetLine();
  return node;
}

Object* 
Evaluator::AllocNode(AstTypes type, Value const& a1, Value const& a2, Value const& a3)
{
  Object* node = new Object;  
  (*node)["type"] = type;
  (*node)["a1"]   = a1;
  (*node)["a2"]   = a2;
  (*node)["a3"]   = a3;
  (*node)["file"] = m_file ? m_file->GetPath() : "";
  (*node)["line"] = m_lexer->GetLine();
  return node;
}

Object* 
Evaluator::AllocNode(AstTypes type, Value const& a1, Value const& a2, Value const& a3, Value const& a4)
{
  Object* node = new Object;  
  (*node)["type"] = type;
  (*node)["a1"]   = a1;
  (*node)["a2"]   = a2;
  (*node)["a3"]   = a3;
  (*node)["a4"]   = a4;
  (*node)["file"] = m_file ? m_file->GetPath() : "";
  (*node)["file"] = m_lexer->GetLine();
  return node;
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
  if(e.m_node && !(*e.m_node)["file"].Empty())
  {
    csout
      << "\n"
      << (*e.m_node)["file"].GetString()
      << "("
      << (*e.m_node)["line"].GetInt()
      << ") : ";
 }
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
Evaluator::EvalStatement(Object& node)
{
  VecRestore<ValueVec> vr(m_temporaries);
  switch(node["type"].GetInt())
  {
  case empty_statement:       break;

  case translation_unit:      EvalStatement(node["a1"]);   break;
  case statement_sequence:    EvalStatementSeq(node);     break;
  case expression_statement:  EvalExpStmt(node["a1"]);     break;
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

  case break_statement:       throw break_exception(&node);  
  case continue_statement:    throw continue_exception(&node);
  case throw_statement:       throw user_exception(&node, 
                                  EvalExpression(node["a1"]));

  case declaration_sequence:
    EvalStatement(node["a1"]);
    EvalStatement(node["a2"]);
    break;

  case compound_statement:
    if(node.Contains("a1"))
    {
      AutoScope as(this, new Scope(m_scope));
      EvalStatement(node["a1"]);
    }
    break;

  default: throw script_exception(&node, "Invalid node type");
  }
}

void 
Evaluator::EvalExpStmt(Object& node)
{
  EvalExpression(node);
}

RValue&
Evaluator::EvalExpression(Object& node)
{
  switch(node["type"].GetInt())
  {
  case assignment_expression: return EvalAssignment(node);
  case binary_expression:     return EvalBinary(node);
  case ternary_expression:    return EvalTernary(node);
  case prefix_expression:     return EvalPrefix(node);
  case postfix_expression:    return EvalPostfix(node);
  case index_expression:      return EvalIndex(node);
  case function_call:         return EvalFunctionCall(node);
  case literal_value:         return MakeTemp(node["a1"].GetValue());
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
  throw script_exception(&node, "Invalid expression type");
}

RValue&
Evaluator::EvalAssignment(Object& node)
{
  // Evaluate left-hand side
  LValue& lhs = EvalExpression(node["a2"]).LVal();
  if(&lhs == 0)
  {
    throw std::runtime_error("Expression does not yield an lvalue");
  }

  // Evaluate right-hand side
  RValue& rhs = EvalExpression(node["a3"]);

  // Perform assignment
  switch(node["a1"].GetInt())
  {
  case op_assign: return lhs = rhs;
  case op_assadd: return lhs = ValAdd(lhs, rhs);
  case op_asssub: return lhs = ValSub(lhs, rhs);
  case op_assmul: return lhs = ValMul(lhs, rhs);
  case op_assdiv: return lhs = ValDiv(lhs, rhs); 
  case op_assmod: return lhs = ValMod(lhs, rhs);
  }
  throw script_exception(&node, "Invalid assignment operator");
}

RValue&
Evaluator::EvalBinary(Object& node)
{
  Value result;
  switch(node["a1"].GetInt())
  {
  case op_add:    result = ValAdd (EvalExpression(node["a2"]), EvalExpression(node["a3"])); break;
  case op_sub:    result = ValSub (EvalExpression(node["a2"]), EvalExpression(node["a3"])); break;
  case op_mul:    result = ValMul (EvalExpression(node["a2"]), EvalExpression(node["a3"])); break;
  case op_div:    result = ValDiv (EvalExpression(node["a2"]), EvalExpression(node["a3"])); break;
  case op_mod:    result = ValMod (EvalExpression(node["a2"]), EvalExpression(node["a3"])); break;
  case op_eq:     result = Compare(EvalExpression(node["a2"]), EvalExpression(node["a3"])) == 0; break;
  case op_ne:     result = Compare(EvalExpression(node["a2"]), EvalExpression(node["a3"])) != 0; break;
  case op_lt:     result = Compare(EvalExpression(node["a2"]), EvalExpression(node["a3"])) <  0; break;
  case op_le:     result = Compare(EvalExpression(node["a2"]), EvalExpression(node["a3"])) <= 0; break;
  case op_gt:     result = Compare(EvalExpression(node["a2"]), EvalExpression(node["a3"])) >  0; break;
  case op_ge:     result = Compare(EvalExpression(node["a2"]), EvalExpression(node["a3"])) >= 0; break;

  case op_logor:  result = ValBool(EvalExpression(node["a2"])) || 
                           ValBool(EvalExpression(node["a3"])) ; break;
  case op_logand: result = ValBool(EvalExpression(node["a2"])) && 
                           ValBool(EvalExpression(node["a3"])) ; break;

  case op_seq:    result = Compare(EvalExpression(node["a2"]), EvalExpression(node["a3"])) == 0; break;
  case op_sne:    result = Compare(EvalExpression(node["a2"]), EvalExpression(node["a3"])) != 0; break;

  //   case op_seq:    return EvalExpression(node["a2"])->Compare(*EvalExpression(node["a3"]), true) == 0; break;
  //   case op_sne:    return EvalExpression(node["a2"])->Compare(*EvalExpression(node["a3"]), true) != 0; break;

  default: throw script_exception(&node, "Invalid binary operator");
  }  
  
  return MakeTemp(result);
}

RValue&
Evaluator::EvalTernary(Object& node)
{
  if(EvalExpression(node["a1"]).GetBool())
  {
    return EvalExpression(node["a2"]);
  }
  return EvalExpression(node["a3"]);
}

void 
Evaluator::EvalStatementSeq(Object& node)
{
  Object::ConstIterator it, ie;
  it = node["a1"].GetObject()->Begin();
  ie = node["a1"].GetObject()->End();
  for(; it != ie; ++it)
  {
    EvalStatement(*dynamic_cast<Object*>(it->second->GetObject()));
  }
}

RValue&
Evaluator::EvalPrefix(Object& node)
{
  RValue& lhs = EvalExpression(node["a2"]);
  switch(node["a1"].GetInt())
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
  throw script_exception(&node, "Invalid prefix operator");
}

RValue&
Evaluator::EvalPostfix(Object& node)
{
  // Evaluate lhs
  LValue& lhs = EvalExpression(node["a2"]).LVal();

  // Create temporary with result value
  RValue& result = MakeTemp(lhs);

  // Perform postfix operation
  switch(node["a1"].GetInt())
  {
  case op_postinc: lhs.SetValue(ValAdd(lhs, 1)); break;
  case op_postdec: lhs.SetValue(ValSub(lhs, 1)); break;
  default: script_exception(&node, "Invalid postfix operator");
  }
  
  // Done
  return result;
}

RValue&
Evaluator::EvalIndex(Object& node)
{
  // Check type of left-hand side
  RValue& lhs = EvalExpression(node["a1"]);
  switch(lhs.Type())
  {
  case Value::tNull:
    lhs.LVal() = new Object();
    break;
  case Value::tObject:
    // Fine
    break;
  default:  
    throw script_exception(&node, "Invalid type for index operator");
  }

  // Locate value
  RValue* val = 0;
  if(node["a2"].Empty())
  {
    // Use size as key
    Value key = (Value::Int)lhs.GetObject()->GetMembers().size();

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
    RValue& rhs = EvalExpression(node["a2"]);

    // Retrieve value
    if(!lhs.GetObject()->Find(rhs, val))
    {
      val = lhs.GetObject()->Add(rhs.GetValue(), new RWVariable());
    }
  }
  
  // Done
  return StoreTemp(new BoundLValue(val->LVal(), lhs));
}

RValue&
Evaluator::EvalLValue(Object& node)
{
  RValue* ptr;
  Object* owner;
  if(m_scope->Lookup(node["a1"], ptr, owner))
  {
    return owner ? StoreTemp(BoundValue::Create(*ptr, owner)) : *ptr;
  }
  throw script_exception(&node, "Undeclared variable '" + node["a1"].GetString() + "'");
}

void
Evaluator::EvalVarDecl(Object& node)
{
  // Determine right-hand value
  Value value;
  if(node["a2"].Empty())
  {
    value = Value();
  }
  else
  {
    // Assign value-of rhs
    value = EvalExpression(node["a2"]);
  }

  // Create variable
  // TODO deletion of newed variable
  m_scope->Add(node["a1"].GetString(), new RWVariable(value));
}

void
Evaluator::EvalFunDecl(Object& node)
{
  Function* fun = new ScriptFunction(node["a1"], &node);
  (*fun)["name"]   = fun->GetName();
  (*fun)["parent"] = m_scope;
  (*fun)["code"] = &node;
  m_scope->Add(node["a1"].GetString(), new ROVariable(fun));
}

RValue& 
Evaluator::EvalClosure(Object& node)
{
  Function* fun = new ScriptFunction(node["a1"], &node);
  fun->GetMembers()["name"]   = new ROVariable(fun->GetName());
  fun->GetMembers()["parent"] = new ROVariable(m_scope);
  return MakeTemp(fun);
}

RValue& 
Evaluator::EvalFunctionMember(Object& node)
{
  String name = node["a1"]["a1"];

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
          f->GetMembers()[name] = rval;
        }
        return *rval;
      }
    }
    s = s->GetParent();
  }

  throw script_exception(&node, "Invalid scope for function keyword");
}

RValue& 
Evaluator::EvalFunctionIndex(Object& node)
{
  throw script_exception(&node, "Fail");
}

void 
Evaluator::EvalExternDecl(Object& node)
{
  m_scope->Add(node["a1"], new ROVariable(
    new ExternFunction(node["a1"], &node)));
}

RValue&
Evaluator::EvalFunctionCall(Object& node)
{
  RValue* rval  = 0;
  Object* owner = 0;

  // Resolve function pointer
  if(node["a1"]["type"].GetInt() == lvalue)
  {
    // Named objects are found through the current scope
    if(!m_scope->Lookup(String(node["a1"]["a1"]), rval, owner))
    {
      throw script_exception(&node, "Function not found");
    }
  }
  else
  {
    // Expressions are evaluated in the current scope
    rval = &EvalExpression(node["a1"]);

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
    throw script_exception(&node, "Function call on non-function object");
  }

  // Continue in overload
  return EvalFunctionCall(node, fun, owner, node["a2"]);
}

RValue& 
Evaluator::EvalFunctionCall(Object& node, Function* fun, Object* owner, Object& arguments)
{
  Arguments args;

  // Add object context to arguments
  args.SetObject(owner);

  // Add parameters to arguments
  args.SetParameters(fun->GetParameters());

  // Evaluate arguments
  if(arguments["type"].GetInt() == positional_arguments)
  {
    EvalPositionalArguments(node, fun, arguments["a1"], args);
  }
  else
  {
    EvalNamedArguments(node, fun, arguments["a1"], args);
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
  if(Object const* pars = fun->GetParameters())
  {
    Object::ConstIterator pi, pe;
    pi = pars->Begin();
    pe = pars->End();
    for(size_t index = 0; pi != pe; ++pi, ++index)
    {
      m_scope->Add((*pi->second)["a1"], new RWVariable(args[index]));
    }
  }

  // Create function execution scope
  AutoScope es(this, new Scope(m_scope));

  // Execute expression
  try
  {
    EvalStatement((*fun->GetNode())["a3"]);
    return MakeTemp(Value());
  }
  catch(return_exception const& e)
  {
    return MakeTemp(e.m_value);
  }
}

void 
Evaluator::EvalPositionalArguments(Object& node, Function* fun, Object& arglist, Arguments& args)
{
  // Retrieve formal parameters
  Object const* parlist = fun->GetParameters();

  // No formal parameter list
  if(parlist == 0)
  {
    // Evaluate arguments
    Object::ConstIterator ai = arglist.Begin();
    for(; ai != arglist.End(); ++ai)
    {
      args.push_back(EvalExpression(*ai->second));
    }

    // Done
    return;
  }

  // Enumerate parameters
  Object::ConstIterator pi, pe, ai, ae;
  pi = parlist->Begin();
  pe = parlist->End();
  ai = arglist.Begin();
  ae = arglist.End();
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
      throw script_exception(&node, "Too many arguments in call to '" + fun->GetName() + "'");
    }

    // Variadic parameter
    if((*pi->second)["a2"].GetInt() == ptVariadic)
    {
      // Insert map
      Object* va = new Object();
      args.push_back(va);

      // Insert remaining arguments
      for(; ai != ae; ++ai)
      {
        // Evaluate argument value. ATTN: this dereference
        // is intentional; variadic arguments are by value.
        Value val = EvalExpression(*ai->second);

        // Append to list
        va->GetMembers()[va->GetMembers().size() - 1] = new RWVariable(val);
      }

      // Done
      break;
    }

    // End of arguments
    Value value;
    if(ai == ae)
    {
      // Must have default value
      if(!(*pi->second)["a4"])
      {
        throw script_exception(&node, "Not enough arguments in call to '" + fun->GetName() + "'");
      }

      // Evaluate default value
      value = EvalExpression((*pi->second)["a4"]);
    }
    else
    {
      // Evaluate argument
      value = EvalExpression(*ai->second);
    }

    // Handle byref/byval
    if((*pi->second)["a2"].GetInt() == ptByVal)
    {
      // Dereference the value
//       value = *value;
    }

    // Apply type conversion to value
    if((*pi->second)["a3"])
    {
      PerformConversion(value, *(*pi->second)["a3"].GetObject());
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
Evaluator::EvalNamedArguments(Object& node, Function* fun, Object& arglist, Arguments& args)
{
  // TODO: validate superfluous/duplicate arguments
  // TODO: implement type conversions

//   // TODO ast
//   AstList const* parlist = 0; // fun->GetParameters();
// 
//   // Enumerate parameters
//   AstList::const_iterator pi, pe;
//   pi = parlist->begin();
//   pe = parlist->end();
//   for(; pi != pe; ++pi)
//   {
//     // Extract parameter name
//     String parname = (*pi)["a1"].GetString();
// 
//     // Cannot supply named arguments for variadic parameter
//     if((*pi)["a2"].GetInt() == ptVariadic)
//     {
//       // Variadic is always last in list - add empty list and stop
// //      args.push_back(Value::stAssoc);
//       break;
//     }
// 
//     // Find named argument for parameter
//     AstList::const_iterator ai, ae;
//     ai = arglist->begin();
//     ae = arglist->end();
//     for(; ai != ae; ++ai)
//     {
//       if((*ai)["a1"].GetString() == parname)
//       {
//         break;
//       }
//     }
// 
//     // Evaluate argument
//     Value value;
//     if(ai != ae)
//     {
//       // Fill in positional argument
//       value = EvalExpression((*ai)["a2"]);
//     }
//     else if((*pi)["a3"])
//     {
//       // Evaluate default value
//       value = EvalExpression((*pi)["a3"]);
//     }
//     else
//     {
//       // Missing argument
//       throw script_exception(&node, "No value specified for parameter '" + parname + "'");
//     }
// 
//     // Assign by value/by ref
//     if((*pi)->m_a4 && (*pi)->m_a4.GetInt() == ptByRef)
//     {
//       // TODO validate type is correct for byref?
//       args.push_back(value);
//     }
//     else
//     {
//       // TODO execute conversion to type
//       args.push_back(value);
//     }
//   }
}

RValue&
Evaluator::EvalListLiteral(Object& node)
{
  // Create empty map
  Value v(new Object());
  Object* o = v.GetObject();
  
  // Recurse into map values
  if(!node["a1"].Empty())
  {
    Object* child = node["a1"];
    int index = 0;
    while(child)
    {
      // Evaluate element
      RValue const& element = EvalExpression((*child)["a1"]["a1"]);

      // Evaluate key
      Value key = index++;
      if((*child)["a1"]["a2"])
      {
        key = EvalExpression((*child)["a1"]["a2"]);
      }

      // Insert into member variables
      o->GetMembers()[key] = new RWVariable(element);

      // Check for next element
      if((*child)["a2"].Empty()) 
      {
        break;
      }

      // Set next element
      child = (*child)["a2"];
    }
  }

  // Done
  return MakeTemp(v);
}

RValue&
Evaluator::EvalJsonLiteral(Object& node)
{
  // Create empty map
  Value v(new Object());
  Object* o = v.GetObject();

  // Recurse into map values
  if(!node["a1"].Empty())
  {
    Object* child = node["a1"];
    while(child)
    {
      // Retrieve and check key
      String key = (*child)["a1"]["a1"];
      if(o->GetMembers().count(key))
      {
        throw script_exception(&node, "Duplicate key in JSON literal");
      }

      // Evaluate element
      RValue const& element = EvalExpression((*child)["a1"]["a2"]);

      // Insert into member variables
      o->GetMembers()[key] = new RWVariable(element);

      // Check for next element
      if((*child)["a2"].Empty()) 
      {
        break;
      }

      // Set next element
      child = (*child)["a2"];
    }
  }

  // Done
  return MakeTemp(v);
}

void 
Evaluator::EvalReturnStatement(Object& node)
{
  // Determine return value
  Value value;
  if(node["a1"].Empty())
  {
    value = Value();
  }
  else
  {
    value = EvalExpression(node["a1"]);
  }

  // Throw return exception
  throw return_exception(&node, value);
}

void 
Evaluator::EvalIncludeStatement(Object& node)
{
  ParseFile(node["a1"]);
}

void 
Evaluator::EvalForStatement(Object& node)
{
  // Outer scope
  AutoScope scope(this, new Scope(m_scope));
  
  // Evaluate init expression
  EvalStatement(node["a1"]);

  // Evaluate loop
  for(;;)
  {
    // Evaluate condition
    RValue const& cond = EvalExpression(node["a2"]);
    if(cond.Type() != Value::tBool)
    {
      throw script_exception(node["a2"], "Expression does not yield a boolean value");
    }

    // Check condition
    if(!EvalExpression(node["a2"]).GetBool())
    {
      break;
    }

    // For body
    try 
    {
      AutoScope scope(this, new Scope(m_scope));
      EvalStatement(node["a4"]);
    }
    catch(break_exception const&)
    {
      break;
    }
    catch(continue_exception const&)
    {
    }

    // Evaluate post expression
    EvalExpression(node["a3"]);
  }  
}

void 
Evaluator::EvalForeachStatement(Object& node)
{
  AutoScope scope(this, new Scope(m_scope));

  String varName;
  Object&   varNode = node["a1"];

  // Determine variable name
  if(varNode["type"].GetInt() == variable_declaration)
  {
    EvalStatement(varNode);
    varName = varNode["a1"].GetString();
  }
  else
  {
    varName = node["a1"].GetString();
  }

  // Fetch variable
  RValue* rval;
  Object* owner;
  if(!m_scope->Lookup(varName, rval, owner))
  {
    throw script_exception(&node, "Failed to find iterator variable");
  }

  // Convert to lvalue
  LValue& var = rval->LVal();

  // Evaluate expression
  RValue& rhs = EvalExpression(node["a2"]);

  // Retrieve enumerator
  Enumerator* enumerator = rhs.GetEnumerator();
  if(enumerator == 0)
  {
    throw script_exception(&node, "Invalid type specified in foreach");
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
      EvalStatement(node["a3"]);
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
Evaluator::EvalIfStatement(Object& node)
{
  RValue const& cond = EvalExpression(node["a1"]);
  if(cond.Type() != Value::tBool)
  {
    throw script_exception(node["a1"], "Expression does not yield a boolean value");
  }

  if(cond.GetBool())
  {
    EvalStatement(node["a2"]);
  }
  else if(node["a3"])
  {
    EvalStatement(node["a3"]);
  }
}

void        
Evaluator::EvalWhileStatement(Object& node)
{
  for(;;)
  {
    RValue const& cond = EvalExpression(node["a1"]);
    if(cond.Type() != Value::tBool)
    {
      throw script_exception(node["a1"], "Expression does not yield a boolean value");
    }

    if(!cond.GetBool())
    {
      break;
    }

    try
    {
      AutoScope scope(this, new Scope(m_scope));
      EvalStatement(node["a2"]);
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
Evaluator::EvalSwitchStatement(Object& node)
{
  // Switch value
  Value value = EvalExpression(node["a1"]);

  // Create iterators
  // TODO ast
  AstList* cases = 0; // node["a2"];
  AstList::const_iterator it = cases->begin();
  AstList::const_iterator ie = cases->end();

  // Find case that matches switch value
  Object* statement = 0;
  for(; it != ie; ++it)
  {
    if((*it)->m_type == default_case)
    {
      if(statement)
      {
        throw script_exception(&node, "More than one default case in switch statement");
      }
      statement = (**it)["a1"];
    }
    else if(Compare(EvalExpression((**it)["a1"]), value) == 0)
    {
      statement = (**it)["a2"];
      break;
    }
  }

  // Execute statement
  if(statement)
  {
    try
    {
      AutoScope as(this, new Scope(m_scope));
      EvalStatement(*statement);
    }
    catch(break_exception const&)
    {
    }
  }
}

RValue&
Evaluator::EvalNewExpression(Object& node)
{
  // Find object
  RValue* rval;
  if(!m_scope->Lookup(node["a1"], rval))
  {
    throw script_exception(&node, "Variable not found");
  }

  // Must be an object
  if(rval->Type() != Value::tObject)
  {
    throw script_exception(&node, "Cannot instantiate a non-object variable");
  }

  // Create a clone
  Object* inst = rval->GetObject()->Clone();

  // Invoke constructor when instantiating a function
  if(Function* fun = dynamic_cast<Function*>(inst))
  {
    // Evaluate constructor
    EvalFunctionCall(node, fun, inst, node["a2"]);
  }

  // Return temporary
  return MakeTemp(inst);
}

RValue&
Evaluator::EvalMemberExpression(Object& node)
{
  // Retrieve left-hand side
  Object* object = ValueToType<Object>(EvalExpression(node["a1"]));
  if(object == 0)
  {
    throw std::runtime_error("Left-hand side does not yield an object");
  }

  // Lookup right-hand side
  RValue* rval;
  if(!object->Find(String(node["a2"]["a1"]), rval))
  {
    rval = new RWVariable();
    object->Add(node["a2"]["a1"].GetString(), rval);
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
Evaluator::EvalThisExpression(Object& node)
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
  throw script_exception(&node, "Invalid context for 'this'");
}

void 
Evaluator::EvalTryStatement(Object& node)
{
  try
  {
    try 
    {
      // Execute guarded block
      EvalStatement(node["a1"]);
    }
    catch(user_exception const& e)
    {
      // Handle only when handler is present
      if(node["a2"])
      {
        // Insert exception into scope
        // TODO this maketemp might crash horribly during exception cleanup!!!
        AutoScope scope(this, new Scope(m_scope));
        m_scope->Add(node["a2"]["a1"].GetString(), new RWVariable(e.m_value));

        // Evaluate catch block
        EvalStatement(node["a2"]["a2"]);
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
    if(node["a3"])
    {
      // Evaluate finally
      EvalStatement(node["a3"]["a1"]);
    }

    // Re-throw exception from catch block
    throw;
  }

  // Evaluate finally
  if(node["a3"])
  {
    EvalStatement(node["a3"]["a1"]);
  }
}

RValue&
Evaluator::EvalConversion(Object& node)
{
  // Evaluate expression
  // ATTN: the implicit conversion from RValue& to Value
  // is intended: the conversion must not be applied to 
  // the source for the conversion!!!
  Value value = EvalExpression(node["a2"]);

  // Perform the conversion
  PerformConversion(value, *node["a1"].GetObject());
  
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
  Object& childNode = *new Object();

  // Create members
  childNode["nodeType"]     = (Value::Int)nodeType;
  childNode["nodeName"]     = XmlNodeName(nodeType);
  childNode["nodeTypeName"] = XmlNodeName(nodeType);

  // Append to parent node
  if(parentNode)
  {
    childNode["parentNode"] = parentNode;
    Object* coll = nodeType == xmlAttribute ?
      (*parentNode)["attributes"].GetObject() :
      (*parentNode)["childNodes"].GetObject() ;
    coll->LVal(coll->GetMembers().size()) = &childNode;
  }

  // Create complex members
  switch(nodeType)
  {
  case xmlDocument:
  case xmlElement:
    childNode["childNodes"] = new Object();
    // Intentional fallthrough
  case xmlProcessingInstruction:
    childNode["attributes"] = new Object();
    break;
  }

  // Done
  return &childNode;
}

void 
SetNodeName(Object& ast, Object& node)
{
  String qname;
  String lname;
  String ns;
  if(ast["type"].GetInt() == xml_qname)
  {
    lname = ast["a2"].GetString();
    ns    = ast["a1"].GetString();
    qname = ns + ":" + lname;
  }
  else
  {
    lname = ast["a1"].GetString();
    qname = lname;
  }

  node["nodeName"]      = lname;
  node["localName"]     = lname;
  node["qualifiedName"] = qname;
  node["namespace"]     = ns;
}

void 
AddXmlAttributes(Object& ast, Object* node)
{
  // Retrieve ast element list
  // TODO ast
//   AstList& list = *ast["a1"].GetList();
// 
//   // Walk through list
//   AstList::iterator it = list.begin();
//   for(; it != list.end(); ++it)
//   {
//     Object& ast = *it;
//     Object* attr = CreateXmlObject(xmlAttribute, node);
//     SetNodeName(ast["a1"], attr);
//     attr->LVal("value") = ast["a2"].GetString();
//   }
}

RValue& 
Evaluator::EvalXmlExpression(Object& node)
{
  // TODO ast
//   // Retrieve ast element list
//   AstList& list = *node["a1"]["a1"].GetList();
// 
//   // Create document element
//   Object* doc = CreateXmlObject(xmlDocument);
// 
//   // Setup temporaries
//   Object* cur = doc;
//   Object* tmp;
// 
//   // Walk through list
//   AstList::iterator it = list.begin();
//   for(; it != list.end(); ++it)
//   {
//     Object& node = *it;
//     switch(node["type"])
//     {
//     case xml_processing_instruction:
//       tmp = CreateXmlObject(xmlProcessingInstruction, cur);
//       tmp->LVal("nodeName") = node["a1"].GetString();
//       if(!node["a2"].Empty())
//       {
//         AddXmlAttributes(node["a2"], tmp);
//       }
//       break;
// 
//     case xml_open_tag:
//       tmp = CreateXmlObject(xmlElement, cur);
//       cur = tmp;
//       SetNodeName(node["a1"], cur);
//       if(!node["a2"].Empty())
//       {
//         AddXmlAttributes(node["a2"], tmp);
//       }
//       break;
// 
//     case xml_close_tag:
//       if(cur == 0)
//       {
//         throw script_exception(&node, "Invalid xml structure");
//       }
//       cur = cur->RVal("parentNode").GetObject();
//       break;
// 
//     case xml_closed_tag:
//       tmp = CreateXmlObject(xmlElement, cur);
//       SetNodeName(node["a1"], tmp);
//       if(!node["a2"].Empty())
//       {
//         AddXmlAttributes(node["a2"], tmp);
//       }
//       break;
// 
//     case xml_text:
//       tmp = CreateXmlObject(xmlText, cur);
//       tmp->LVal("data")   = node["a1"].GetString();
//       tmp->LVal("length") = node["a1"].GetString().length();
//       break;
// 
//     default:
//       throw script_exception(&node, "Unexpected xml node type");
//     }
//   }
// 
//   // Check whether we've gotten back to the document element
//   if(cur != doc)
//   {
//     throw script_exception(&node, "Invalid xml structure");
//   }
// 
//   // TODO Set document element
// 
//   // Done
//   return MakeTemp(doc);
  throw "NOTIMPL";
}