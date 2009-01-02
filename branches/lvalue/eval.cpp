//////////////////////////////////////////////////////////////////////////
//
// This file is © 2007 JJ Fahner <jan-jaap@jan-jaap.net>
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
#include "instance.h"

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

ValueComparatorLess::ValueComparatorLess(Evaluator* evaluator) :
m_evaluator (evaluator)
{
}

bool 
ValueComparatorLess::operator () (Value const& lhs, Value const& rhs) const
{
  return m_evaluator->Compare(lhs, rhs) < 0;
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
      if(dynamic_cast<GlobalScope*>(m_eval->m_scope) == 0)
      {
        delete m_eval->m_scope;
      }
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
    m_vec.resize(m_size);
  }
};

//////////////////////////////////////////////////////////////////////////
//
// Evaluator implementation
//

/*static*/ GlobalScope&
Evaluator::GetGlobalScope()
{
  static GlobalScope scope;
  return scope;
}

Evaluator::Evaluator() :
m_scope   (0),
m_file    (0),
m_allocs  (0)
{
  // Register native calls
  static bool nativeCallsRegistered = false;
  if(!nativeCallsRegistered)
  {
    nativeCallsRegistered = true;
    NativeCallRegistrar::RegisterCalls();
  }

  // Create global scope
  m_global = new GlobalScope(&GetGlobalScope());
}

void 
Evaluator::Reset()
{
  // Collect all remaining objects
  Collect();

  // Create new global scope
  delete m_global;
  m_global = new GlobalScope(&GetGlobalScope());
}

Class* 
Evaluator::FindType(String const& name)
{
  Class* cl = 0;
  m_scope->FindClass(name, cl);
  return cl;
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
    // Run parser loop
    Token token;
    while(lexer.Lex(token))
    {
      CScriptParse(pParser, token.m_type, token, this);
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
}

void 
Evaluator::OnSyntaxError()
{
  FilePos pos;
  pos.m_file = m_file ? m_file->GetPath() : "";
  pos.m_line = m_lexer->GetLine();
  m_reporter.ReportError(E0013, &pos);
}

Ast* 
Evaluator::AllocAst(AstTypes type, AstData const& a1, AstData const& a2, AstData const& a3, AstData const& a4)
{
  // Set file position
  FilePos pos;
  pos.m_file = m_file ? m_file->GetPath() : "";
  pos.m_line = m_lexer->GetLine();

  // Create node
  Ast* node = new Ast(type, a1, a2, a3, a4);  
  node->m_pos = pos;
  
  // Register new types
  if(type == class_declaration)
  {
    m_scope->AddClass(new Class(a1));
  }
  
  // Return the new node
  return node;
}

Ast* 
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

  // Build list of valid objects
  m_scope->AddObjects(valid);

  // Append temporaries
  std::copy(m_temporaries.begin(), 
            m_temporaries.end(), 
            std::inserter(valid, valid.end()));

  // Collect invalid objects
  Object::Collect(valid);
}

int 
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
    return int(&lhs.GetObject() - &rhs.GetObject());
  }

  // Invalid type
  throw std::runtime_error("Unsupported value type for comparison");
}

Value::Bool   
Evaluator::ValBool(Value const& val)
{
  switch(val.Type())
  {
  case Value::tNull:    return false;
  case Value::tBool:    return val.GetBool();
  case Value::tInt:     return val.GetInt() != 0;
  case Value::tString:  return val.GetString().length() != 0;
  case Value::tObject:  break; // TODO
  }
  throw std::runtime_error("Cannot convert between types");
}

Value::Int    
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

inline Value::String ToString(Value::Int val)
{
  char buf[25];
  sprintf(buf, "%d", val);
  return buf;
}

Value::String 
Evaluator::ValString(Value const& val)
{
  switch(val.Type())
  {
  case Value::tNull:    return Value::String();
  case Value::tBool:    return val.GetBool() ? "true" : "false";
  case Value::tInt:     return ToString(val.GetInt());
  case Value::tString:  return val.GetString();
  case Value::tObject:  break; // TODO
  }
  throw std::runtime_error("Cannot convert between types");
}

Value 
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

Value 
Evaluator::ValSub(Value const& lhs, Value const& rhs)
{
  switch(lhs.Type())
  {
  case Value::tInt:     return lhs.GetInt() - ValInt(rhs);
  case Value::tObject:  break; // TODO
  }
  throw std::runtime_error("Invalid type(s) for subtraction operator");
}

Value 
Evaluator::ValMul(Value const& lhs, Value const& rhs)
{
  switch(lhs.Type())
  {
  case Value::tInt:     return lhs.GetInt() * ValInt(rhs);
  case Value::tObject:  break; // TODO
  }
  throw std::runtime_error("Invalid type(s) for multiplication operator");
}

Value 
Evaluator::ValDiv(Value const& lhs, Value const& rhs)
{
  switch(lhs.Type())
  {
  case Value::tInt:     return lhs.GetInt() / ValInt(rhs);
  case Value::tObject:  break; // TODO
  }
  throw std::runtime_error("Invalid type(s) for division operator");
}

Value 
Evaluator::ValMod(Value const& lhs, Value const& rhs)
{
  switch(lhs.Type())
  {
  case Value::tInt:     return lhs.GetInt() % ValInt(rhs);
  case Value::tObject:  break; // TODO
  }
  throw std::runtime_error("Invalid type(s) for modulo operator");
}

Value 
Evaluator::ValNeg(Value const& lhs)
{
  switch(lhs.Type())
  {
  case Value::tInt:     return -lhs.GetInt();
  case Value::tObject:  break; // TODO
  }
  throw std::runtime_error("Invalid type(s) for negation operator");
}

Value 
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
  if(!e.m_node->m_pos.m_file.empty())
  {
    csout
      << e.m_node->m_pos.m_file 
      << "("
      << e.m_node->m_pos.m_line
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
    cserr << "Error: Uncaught exception '" << typeid(e).name() << "'\n";
  }
  // System exception
  catch(std::exception const& e)
  {
    cserr << "Error: " << e.what() << "\n";
  }
  // Unknown exception type
  catch(...)
  {
    cserr << "Error: Unexpected exception\n";
  }
  return Value();
}

void 
Evaluator::EvalStatement(Ast* node)
{
  VecRestore<ObjectVec> vr(m_temporaries);
  switch(node->m_type)
  {
  case empty_statement:       break;

  case translation_unit:      EvalStatement(node->m_a1);  break;
  case statement_sequence:    EvalStatementSeq(node);     break;
  case expression_statement:  EvalExpStmt(node->m_a1);    break;
  case variable_declaration:  EvalVarDecl(node);          break;
  case function_declaration:  EvalFunDecl(node);          break;
  case extern_declaration:    EvalExternDecl(node);       break;
  case class_declaration:     EvalClassDecl(node);        break;
  case try_statement:         EvalTryStatement(node);     break;
  case for_statement:         EvalForStatement(node);     break;
  case foreach_statement:     EvalForeachStatement(node); break;
  case if_statement:          EvalIfStatement(node);      break;
  case while_statement:       EvalWhileStatement(node);   break;  
  case return_statement:      EvalReturnStatement(node);  break;
  case switch_statement:      EvalSwitchStatement(node);  break;

  case break_statement:       throw break_exception(node);  
  case continue_statement:    throw continue_exception(node);
  case throw_statement:       throw user_exception(node, 
                                  EvalExpression(node->m_a1));

  case declaration_sequence:
    EvalStatement(node->m_a1);
    EvalStatement(node->m_a2);
    break;

  case compound_statement:
    if(node->m_a1)
    {
      AutoScope as(this, new Scope(m_scope));
      EvalStatement(node->m_a1);
    }
    break;

  default: throw std::out_of_range("Invalid node type");
  }
}

void 
Evaluator::EvalExpStmt(Ast* node)
{
  EvalExpression(node);
}

RValue&
Evaluator::EvalExpression(Ast* node)
{
  switch(node->m_type)
  {
  case assignment_expression: return EvalAssignment(node);
  case binary_expression:     return EvalBinary(node);
  case ternary_expression:    return EvalTernary(node);
  case prefix_expression:     return EvalPrefix(node);
  case postfix_expression:    return EvalPostfix(node);
  case index_expression:      return EvalIndex(node);
  case function_call:         return MakeTemp(EvalFunctionCall(node));
  case literal_value:         return MakeTemp(node->m_a1.GetValue());
  case lvalue:                return EvalLValue(node);
  case list_literal:          return EvalListLiteral(node);
  case new_expression:        return EvalNewExpression(node);
  case this_expression:       return EvalThisExpression();
  case member_expression:     return EvalMemberExpression(node);  
  case conversion_expression: return EvalConversion(node);
  }
  throw std::out_of_range("Invalid expression type");
}

RValue&
Evaluator::EvalAssignment(Ast* node)
{
  // Evaluate left-hand side
  LValue& lhs = dynamic_cast<LValue&>(EvalExpression(node->m_a2));
  if(&lhs == 0)
  {
    throw std::runtime_error("Expression does not yield an lvalue");
  }

  // Evaluate right-hand side
  RValue& rhs = EvalExpression(node->m_a3);

  // Perform assignment
  switch(node->m_a1.GetNumber())
  {
  case op_assign: return lhs = rhs;
  case op_assadd: return lhs = ValAdd(lhs, rhs);
  case op_asssub: return lhs = ValSub(lhs, rhs);
  case op_assmul: return lhs = ValMul(lhs, rhs);
  case op_assdiv: return lhs = ValDiv(lhs, rhs); 
  case op_assmod: return lhs = ValMod(lhs, rhs);
  }
  throw std::out_of_range("Invalid assignment operator");
}

RValue&
Evaluator::EvalBinary(Ast* node)
{
  Value result;
  switch(node->m_a1.GetNumber())
  {
  case op_add:    result = ValAdd (EvalExpression(node->m_a2), EvalExpression(node->m_a3)); break;
  case op_sub:    result = ValSub (EvalExpression(node->m_a2), EvalExpression(node->m_a3)); break;
  case op_mul:    result = ValMul (EvalExpression(node->m_a2), EvalExpression(node->m_a3)); break;
  case op_div:    result = ValDiv (EvalExpression(node->m_a2), EvalExpression(node->m_a3)); break;
  case op_mod:    result = ValMod (EvalExpression(node->m_a2), EvalExpression(node->m_a3)); break;
  case op_eq:     result = Compare(EvalExpression(node->m_a2), EvalExpression(node->m_a3)) == 0; break;
  case op_ne:     result = Compare(EvalExpression(node->m_a2), EvalExpression(node->m_a3)) != 0; break;
  case op_lt:     result = Compare(EvalExpression(node->m_a2), EvalExpression(node->m_a3)) <  0; break;
  case op_le:     result = Compare(EvalExpression(node->m_a2), EvalExpression(node->m_a3)) <= 0; break;
  case op_gt:     result = Compare(EvalExpression(node->m_a2), EvalExpression(node->m_a3)) >  0; break;
  case op_ge:     result = Compare(EvalExpression(node->m_a2), EvalExpression(node->m_a3)) >= 0; break;

  case op_logor:  result = ValBool(EvalExpression(node->m_a2)) || 
                           ValBool(EvalExpression(node->m_a3)) ; break;
  case op_logand: result = ValBool(EvalExpression(node->m_a2)) && 
                           ValBool(EvalExpression(node->m_a3)) ; break;

  case op_seq:    result = Compare(EvalExpression(node->m_a2), EvalExpression(node->m_a3)) == 0; break;
  case op_sne:    result = Compare(EvalExpression(node->m_a2), EvalExpression(node->m_a3)) != 0; break;

  //   case op_seq:    return EvalExpression(node->m_a2)->Compare(*EvalExpression(node->m_a3), true) == 0; break;
  //   case op_sne:    return EvalExpression(node->m_a2)->Compare(*EvalExpression(node->m_a3), true) != 0; break;

  default: throw std::out_of_range("Invalid binary operator");
  }  
  
  return MakeTemp(result);
}

RValue&
Evaluator::EvalTernary(Ast* node)
{
  if(EvalExpression(node->m_a1).GetValue().GetBool())
  {
    return EvalExpression(node->m_a2);
  }
  return EvalExpression(node->m_a3);
}

void 
Evaluator::EvalStatementSeq(Ast* node)
{
  AstList::const_iterator it, ie;
  it = node->m_a1.GetList()->begin();
  ie = node->m_a1.GetList()->end();
  for(; it != ie; ++it)
  {
    EvalStatement(*it);
  }
}

RValue&
Evaluator::EvalPrefix(Ast* node)
{
  RValue& lhs = EvalExpression(node->m_a2);
  switch(node->m_a1.GetNumber())
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
  throw std::out_of_range("Invalid prefix operator");
}

RValue&
Evaluator::EvalPostfix(Ast* node)
{
  // Evaluate lhs
  LValue& lhs = EvalExpression(node->m_a2).LVal();

  // Create temporary with result value
  RValue& result = MakeTemp(lhs);

  // Perform postfix operation
  switch(node->m_a1.GetNumber())
  {
  case op_postinc: lhs.SetValue(ValAdd(lhs, 1)); break;
  case op_postdec: lhs.SetValue(ValSub(lhs, 1)); break;
  default: throw std::out_of_range("Invalid postfix operator");
  }
  
  // Done
  return result;
}

RValue&
Evaluator::EvalIndex(Ast* node)
{
  RValue& lhs = EvalExpression(node->m_a1);
  RValue& rhs = EvalExpression(node->m_a2);

  if(lhs.GetValue().Type() != Value::tObject)
  {
    throw std::out_of_range("Invalid type for index operator");
  }

  // TODO check whether position is valid
  return *lhs.GetValue().GetObject().GetVariables()[rhs];
}

RValue&
Evaluator::EvalLValue(Ast* node)
{
  RValue* ptr;
  if(m_scope->FindVar(node->m_a1, ptr))
  {
    return *ptr;
  }
  throw std::runtime_error("Undeclared variable '" + 
                      node->m_a1.GetString() + "'");
}

void
Evaluator::EvalVarDecl(Ast* node)
{
  // Determine right-hand value
  Value value;
  if(node->m_a2.Empty())
  {
    value = Value();
  }
  else
  {
    // Assign value-of rhs
    value = EvalExpression(node->m_a2);
  }

  // Create variable
  // TODO deletion of newed variable
  m_scope->AddVar(node->m_a1, *new Variable(value));
}

void        
Evaluator::EvalFunDecl(Ast* node)
{
  m_scope->AddFun(new ScriptFunction(node->m_a1, node));
}

void 
Evaluator::EvalExternDecl(Ast* node)
{
  m_scope->AddFun(new ExternFunction(node->m_a1, node));
}

void 
Evaluator::EvalClassDecl(Ast* node)
{
  // Find the class declaration
  Class* cl = FindType(node->m_a1);
  if(cl == 0)
  {
    cl = new Class(node->m_a1);
  }

  // Enumerate members
  AstList* list = node->m_a2;
  AstList::iterator it, ie;
  it = list->begin();
  ie = list->end();
  for(; it != ie; ++it)
  {
    Ast* node = *it;
    switch(node->m_type)
    {
    case constructor:
      cl->SetConstructor(new Constructor(cl, node));
      break;

    case destructor:
      cl->SetDestructor(new Destructor(cl, node));
      break;

    case variable_declaration:
      cl->AddVariable(node->m_a1, node);
      break;

    case function_declaration:
      cl->AddMethod(node->m_a1, new MemberFunction(node->m_a1, cl, node));
      break;

    case conversion_operator:
      cl->AddConversion(new ConversionOperator(cl, node->m_a1.GetNode(), node));
      break;

    default:
      throw std::runtime_error("Invalid member type");
    }
  }
}

inline Instance* 
GetInstance(Value const& v)
{
  if(v.Type() != Value::tObject)
  {
    throw std::runtime_error("Expression does not yield an object");
  }
  Instance* inst = dynamic_cast<Instance*>(&v.GetObject());
  if(inst == 0)
  {
    throw std::runtime_error("Expression does not yield an object");
  }
  return inst;
}

Value
Evaluator::EvalFunctionCall(Ast* node)
{
  Function* fun = 0;
  Arguments args;

  // Resolve function pointer
  if(node->m_a3)
  {
    // Evaluate instance expression
    args.SetInstance(GetInstance(EvalExpression(node->m_a3)));

    // Resolve function on object
    MemberFunction* memfun;
    if(!args.GetInstance()->FindMethod(node->m_a1, memfun))
    {
      throw std::runtime_error("Object doesn't support this method");
    }
    fun = memfun;
  }
  else
  {
    // Resolve function in scope stack
    if(!m_scope->FindMethod(node->m_a1, fun))
    {
      throw std::runtime_error("Unknown method");
    }

    // In case of member function, prepend instance
    if(dynamic_cast<MemberFunction*>(fun))
    {
      args.SetInstance(GetInstance(EvalThisExpression()));
    }
  }

  // Add parameters to arguments
  args.SetParameters(fun->GetParameters());

  // Evaluate arguments
  if(node->m_a2->m_type == positional_arguments)
  {
    EvalPositionalArguments(fun, node->m_a2->m_a1, args);
  }
  else
  {
    EvalNamedArguments(fun, node->m_a2->m_a1, args);
  }

  // Evaluate function
  try
  {
    return fun->Execute(this, args);
  }
  catch(return_exception const& e)
  {
    return e.m_value;
  }
}

Value 
Evaluator::EvalScriptCall(ScriptFunction* fun, Arguments& args)
{
  // Class scope
  AutoScope cs(this);

  // Determine parent scope
  Scope* parentScope = m_global;
  if(dynamic_cast<MemberFunction*>(fun))
  {
    // Create class scope
    cs.Set(new ClassScope(parentScope, args.GetInstance()));

    // Adjust parent scope
    parentScope = m_scope;
  }

  // Create argument scope
  AutoScope as(this, new Scope(parentScope));

  // Insert arguments into argument scope
  AstList::const_iterator pi, pe;
  pi = fun->GetParameters()->begin();
  pe = fun->GetParameters()->end();
  for(size_t index = 0; pi != pe; ++pi, ++index)
  {
    m_scope->AddVar((*pi)->m_a1, *new Variable(args[index]));
  }

  // Create function execution scope
  AutoScope es(this, new Scope(m_scope));

  // Execute expression
  try
  {
    EvalStatement(fun->GetNode()->m_a3);
    return Value();
  }
  catch(return_exception const& e)
  {
    return e.m_value;
  }
}

void 
Evaluator::EvalPositionalArguments(Function* fun, AstList const* arglist, Arguments& args)
{
  // Retrieve formal parameters
  AstList const* parlist = fun->GetParameters();

  // No formal parameter list
  if(parlist == 0)
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

  // Enumerate parameters
  AstList::const_iterator pi, pe, ai, ae;
  pi = parlist->begin();
  pe = parlist->end();
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
      throw std::runtime_error("Too many arguments in call to '" + fun->GetName() + "'");
    }

    // Variadic parameter
    if((*pi)->m_a2.GetNumber() == ptVariadic)
    {
      // Insert map
      Object* va = Object::Create(this);
      args.push_back(va);

      // Insert remaining arguments
      for(; ai != ae; ++ai)
      {
        // Evaluate argument value. ATTN: this dereference
        // is intentional; variadic arguments are by value.
        Value val = EvalExpression(*ai);

        // Append to list
        va->GetVariables()[va->GetVariables().size() - 1] = new RWMemberVariable(val);
      }

      // Done
      break;
    }

    // End of arguments
    Value value;
    if(ai == ae)
    {
      // Must have default value
      if(!(*pi)->m_a4)
      {
        throw std::runtime_error("Not enough arguments in call to '" + fun->GetName() + "'");
      }

      // Evaluate default value
      value = EvalExpression((*pi)->m_a4);
    }
    else
    {
      // Evaluate argument
      value = EvalExpression(*ai);
    }

    // Handle byref/byval
    if((*pi)->m_a2.GetNumber() == ptByVal)
    {
      // Dereference the value
//       value = *value;
    }

    // Apply type conversion to value
    if((*pi)->m_a3)
    {
      PerformConversion(value, (*pi)->m_a3.GetNode());
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
Evaluator::EvalNamedArguments(Function* fun, AstList const* arglist, Arguments& args)
{
  // TODO: validate superfluous/duplicate arguments
  // TODO: implement type conversions

  AstList const* parlist = fun->GetParameters();

  // Enumerate parameters
  AstList::const_iterator pi, pe;
  pi = parlist->begin();
  pe = parlist->end();
  for(; pi != pe; ++pi)
  {
    // Extract parameter name
    String parname = (*pi)->m_a1.GetString();

    // Cannot supply named arguments for variadic parameter
    if((*pi)->m_a2.GetNumber() == ptVariadic)
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
      if((*ai)->m_a1.GetString() == parname)
      {
        break;
      }
    }

    // Evaluate argument
    Value value;
    if(ai != ae)
    {
      // Fill in positional argument
      value = EvalExpression((*ai)->m_a2);
    }
    else if((*pi)->m_a3)
    {
      // Evaluate default value
      value = EvalExpression((*pi)->m_a3);
    }
    else
    {
      // Missing argument
      throw std::runtime_error("No value specified for parameter '" + parname + "'");
    }

    // Assign by value/by ref
    if((*pi)->m_a4.GetNumber() == ptByRef)
    {
      // TODO validate type is correct for byref?
      args.push_back(value);
    }
    else
    {
      // TODO execute conversion to type
      //args.push_back(*value);
    }
  }
}

RValue&
Evaluator::EvalListLiteral(Ast* node)
{
  // Create empty map
  Value v(Object::Create(this));
  
  // Recurse into map values
  Ast* child = node->m_a1;
  int index = 0;
  while(child)
  {
    RValue& element = EvalExpression(child->m_a1->m_a1);
    v.GetObject().GetVariables()[index++] = new RWMemberVariable(element);
    if(child->m_a2.Empty()) 
    {
      break;
    }
    child = child->m_a2;
  }

  // Done
  return MakeTemp(v);
}

void 
Evaluator::EvalReturnStatement(Ast* node)
{
  // Determine return value
  Value value;
  if(node->m_a1.Empty())
  {
    value = Value();
  }
  else
  {
    value = EvalExpression(node->m_a1);
  }

  // Throw return exception
  throw return_exception(node, value);
}

void 
Evaluator::EvalForStatement(Ast* node)
{
  // Outer scope
  AutoScope scope(this, new Scope(m_scope));
  
  // Evaluate init expression
  EvalStatement(node->m_a1);

  // Evaluate loop
  for(;;)
  {
    // Evaluate condition
    if(!EvalExpression(node->m_a2).GetValue().GetBool())
    {
      break;
    }

    // For body
    try 
    {
      AutoScope scope(this, new Scope(m_scope));
      EvalStatement(node->m_a4);
    }
    catch(break_exception const&)
    {
      break;
    }
    catch(continue_exception const&)
    {
    }

    // Evaluate post expression
    EvalExpression(node->m_a3);
  }  
}

void 
Evaluator::EvalForeachStatement(Ast* node)
{
  AutoScope scope(this, new Scope(m_scope));

  String varName;
  Ast*   varNode = node->m_a1;

  // Determine variable name
  if(varNode->m_type == variable_declaration)
  {
    EvalStatement(varNode);
    varName = varNode->m_a1.GetString();
  }
  else
  {
    varName = node->m_a1.GetString();
  }

  // Fetch variable
  RValue* rval;
  if(!m_scope->FindVar(varName, rval))
  {
    throw std::runtime_error("Failed to find iterator variable");
  }

  // Convert to lvalue
  LValue& var = rval->LVal();

  // Evaluate expression
  RValue& rhs = EvalExpression(node->m_a2);

  // Fetch iterator
  Variables::iterator it = rhs.GetValue().GetObject().GetVariables().begin();
  Variables::iterator ie = rhs.GetValue().GetObject().GetVariables().end();

  // Enumerate members
  for(; it != ie; ++it)
  {
    // Assign value to iterator variable
    var = *it->second;

    // Evaluate expression
    try
    {
      AutoScope scope(this, new Scope(m_scope));
      EvalStatement(node->m_a3);
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
Evaluator::EvalIfStatement(Ast* node)
{
  if(EvalExpression(node->m_a1).GetValue().GetBool())
  {
    EvalStatement(node->m_a2);
  }
  else if(node->m_a3)
  {
    EvalStatement(node->m_a3);
  }
}

void        
Evaluator::EvalWhileStatement(Ast* node)
{
  while(EvalExpression(node->m_a1).GetValue().GetBool())
  {
    try
    {
      AutoScope scope(this, new Scope(m_scope));
      EvalStatement(node->m_a2);
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
Evaluator::EvalSwitchStatement(Ast* node)
{
  // Switch value
  Value value = EvalExpression(node->m_a1);

  // Create iterators
  AstList* cases = node->m_a2;
  AstList::const_iterator it = cases->begin();
  AstList::const_iterator ie = cases->end();

  // Find case that matches switch value
  Ast* statement = 0;
  for(; it != ie; ++it)
  {
    if((*it)->m_type == default_case)
    {
      if(statement)
      {
        throw std::runtime_error("More than one default case in switch statement");
      }
      statement = (*it)->m_a1;
    }
    else if(Compare(EvalExpression((*it)->m_a1), value) == 0)
    {
      statement = (*it)->m_a2;
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
Evaluator::EvalNewExpression(Ast* node)
{
  // Find class type
  Class* c = 0;
  if(!m_scope->FindClass(node->m_a1->m_a2, c))
  {
    throw std::runtime_error("Undefined class '" + 
              node->m_a1->m_a2.GetString() + "'");
  }

  // Instantiate class
  Instance* inst = Instance::Create(this, c);

  // Add to temporaries
  m_temporaries.push_back(inst);

  // Execute constructor
  if(Constructor* fun = inst->GetClass()->GetConstructor())
  {
    // Prep arguments
    Arguments args;
    args.SetInstance(inst);
    args.SetParameters(fun->GetParameters());

    // Evaluate arguments
    if(node->m_a2->m_type == positional_arguments)
    {
      EvalPositionalArguments(fun, node->m_a2->m_a1, args);
    }
    else
    {
      EvalNamedArguments(fun, node->m_a2->m_a1, args);
    }
    
    // Execute constructor
    try 
    {
      fun->Execute(this, args);
    }
    catch(return_exception const&)
    {

    }
    catch(...)
    {
      delete inst;
    }
  }

  // Update new count
  if(++m_allocs == g_collect_threshold)
  {
    Collect();
    m_allocs = 0;
  }

  // Done
  return MakeTemp(inst);
}

RValue&
Evaluator::EvalMemberExpression(Ast* node)
{
  // Evaluate left side
  Instance* instPtr = GetInstance(EvalExpression(node->m_a1));

  // Retrieve value
  RValue* ptr;
  if(!instPtr->FindVar(node->m_a2, ptr))
  {
    throw std::runtime_error("Class has no member '" + node->m_a2.GetString() + "'");
  }

  // Done
  return *ptr;
}

RValue&
Evaluator::EvalThisExpression()
{
  Scope* scope = m_scope;
  while(scope)
  {
    ClassScope* cs = dynamic_cast<ClassScope*>(scope);
    if(cs)
    {
      return MakeTemp(cs->GetInstance());
    }
    scope = scope->GetParent();
  }
  throw std::runtime_error("Invalid context for 'this'");
}

void 
Evaluator::EvalTryStatement(Ast* node)
{
  try
  {
    try 
    {
      // Execute guarded block
      EvalStatement(node->m_a1);
    }
    catch(user_exception const& e)
    {
      // Handle only when handler is present
      if(node->m_a2)
      {
        // Insert exception into scope
        // TODO this maketemp might crash horribly during exception cleanup!!!
        AutoScope scope(this, new Scope(m_scope));
        m_scope->AddVar(node->m_a2->m_a1, MakeTemp(e.m_value));

        // Evaluate catch block
        EvalStatement(node->m_a2->m_a2);
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
    if(node->m_a3)
    {
      // Evaluate finally
      EvalStatement(node->m_a3->m_a1);
    }

    // Re-throw exception from catch block
    throw;
  }

  // Evaluate finally
  if(node->m_a3)
  {
    EvalStatement(node->m_a3->m_a1);
  }
}

RValue&
Evaluator::EvalConversion(Ast* node)
{
  // Evaluate expression
  // ATTN: the implicit conversion from RValue& to Value
  // is intended: the conversion must not be applied to 
  // the source for the conversion!!!
  Value value = EvalExpression(node->m_a2);

  // Perform the conversion
  PerformConversion(value, node->m_a1.GetNode());
  
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

//   // Conversion from class type via conversion operator
//   if(oldType == Value::stInstance)
//   {
//     // Extract instance
//     Instance* inst = value->GetInstance();
//     
//     // Try to find a conversion operator
//     ConversionOperator* conv;
//     if(inst->GetClass()->FindConversion(newType, conv))
//     {
//       // Create arguments
//       Arguments args;
//       args.SetInstance(inst);
// 
//       // Execute conversion
//       value = conv->Execute(this, args);
//       return;
//     }
//   }
// 
//   // Conversion to class type via constructor
//   if(newType == Value::stInstance)
//   {
//     // TODO
//   }
// 
//   // No way to convert from/to instance
//   if(oldType == Value::stInstance || newType == Value::stInstance)
//   {
//     throw std::runtime_error("Cannot convert from '" + oldType.GetName() + 
//                                             "' to '" + newType.GetName() + "'");
//   }
// 
//   // Attempt to perform scalar conversion
//   value->SetType(newType);
}
