#include "eval.h"
#include "ast.h"
#include "parser.h"
#include "astlist.h"
#include "native.h"
#include "rtscope.h"

//////////////////////////////////////////////////////////////////////////
//
// Scope implementation
//

struct Evaluator::AutoScope
{
  Evaluator& m_eval;
  AutoScope(Evaluator& eval, Scope* scope) : m_eval (eval) 
  {
    m_eval.PushScope(scope);
  }
  ~AutoScope()
  {
    m_eval.PopScope();
  }
};

//////////////////////////////////////////////////////////////////////////
//
// Control flow exceptions
//

struct return_exception
{
  Ast* m_node;
  VariantRef m_value;
  return_exception(Ast* node) : m_node (node) {}
  return_exception(Ast* node, VariantRef const& value) : m_node (node), m_value (value) {}
};

struct break_exception
{
  Ast* m_node;
  break_exception(Ast* node) : m_node (node) {}
};

struct continue_exception
{
  Ast* m_node;
  continue_exception(Ast* node) : m_node (node) {}
};

//////////////////////////////////////////////////////////////////////////
//
// Evaluator implementation
//

/*static*/ void
Evaluator::Run()
{
  Evaluator eval;
  char buf[4097];
  for(;;)
  {
    std::cout << "> ";
    std::cin.getline(buf, 4096);

    eval.Eval(buf);

    std::cout << "\n";
  }
}

Evaluator::Evaluator() :
m_scope   (0)
{
  m_global = new GlobalScope;
  PushScope(m_global);
}

void 
Evaluator::Eval(String text)
{
  Parser parser(m_reporter);
  parser.ParseText(text.c_str());
  if(m_reporter.GetErrorCount())
  {
    std::cout << "Aborted.\n";
    return;
  }

  Ast* root = parser.GetRoot();
  parser.SetRoot(0);

  try
  {
    EvalStatement(root);
  }
  catch(std::exception const& e)
  {
    std::cout << e.what() << "\n";
  }
  catch(...)
  {
    std::cout << "Unexpected\n";
  }
}

void
Evaluator::PushScope(Scope* scope)
{
  m_scopes.push_front(m_scope);
  m_scope = scope;
}

void 
Evaluator::PopScope()
{
  Scope* old = m_scope;
  m_scope = m_scopes.front();
  m_scopes.pop_front();
  delete old;
}

void 
Evaluator::EvalStatement(Ast* node)
{
  switch(node->m_type)
  {
  case empty_statement:
    break;

  case translation_unit:
    EvalStatement(node->m_a1);
    break;

  case statement_sequence:
    EvalStatementSeq(node);
    break;

  case expression_statement:
    EvalExpression(node->m_a1);
    break;

  //////////////////////////////////////////////////////////////////////////
  //
  // Declarations
  //

  case declaration_sequence:
    EvalStatement(node->m_a1);
    EvalStatement(node->m_a2);
    break;

  case variable_declaration:
    EvalVarDecl(node);
    break;

  case function_declaration:
    EvalFunDecl(node);
    break;

  case parameter_list:
    EvalStatement(node->m_a1);
    EvalStatement(node->m_a2);
    break;

  case parameter:
    EvalParameter(node);
    break;

  case argument_list:
    EvalStatement(node->m_a1);
    EvalStatement(node->m_a2);
    break;

  case argument:
    dynamic_cast<CallScope*>(m_scope)->AddArg(EvalExpression(node->m_a1));
    break;

  case class_declaration:
    EvalClassDecl(node);
    break;

  case class_members:
    break;
  
  case access_specifier:
    break;

  //////////////////////////////////////////////////////////////////////////
  //
  // Control flow
  //

  case for_statement:
    EvalForStatement(node);
    break;

  case foreach_statement:
    EvalForeachStatement(node);
    break;

  case if_statement:
    EvalIfStatement(node);
    break;

  case while_statement:
    EvalWhileStatement(node);
    break;
  
  case return_statement:
    EvalReturnStatement(node);
    break;

  case break_statement:
    throw break_exception(node);
  
  case continue_statement:
    throw continue_exception(node);
  
  case switch_statement:
    EvalSwitchStatement(node);
    break;
  
  case compound_statement:
    if(node->m_a1)
    {
      AutoScope as(*this, new Scope(m_scope));
      EvalStatement(node->m_a1);
    }
    break;

  case pause_statement:
    break;
  
  default:
    throw std::out_of_range("Invalid node type");
  }
}

VariantRef
Evaluator::EvalExpression(Ast* node)
{
  switch(node->m_type)
  {

  case assignment_expression:
    switch(node->m_a1.GetNumber())
    {
    case op_assign: return *EvalExpression(node->m_a2)  = *EvalExpression(node->m_a3);
    case op_assadd: return *EvalExpression(node->m_a2) += *EvalExpression(node->m_a3);
    case op_asssub: return *EvalExpression(node->m_a2) -= *EvalExpression(node->m_a3);
    case op_assmul: return *EvalExpression(node->m_a2) *= *EvalExpression(node->m_a3);
    case op_assdiv: return *EvalExpression(node->m_a2) /= *EvalExpression(node->m_a3);
    case op_assmod: return *EvalExpression(node->m_a2) %= *EvalExpression(node->m_a3);
    default:        throw std::out_of_range("Invalid assignment operator");
    }

  case binary_expression:
    switch(node->m_a1.GetNumber())
    {
    case op_add:    return *EvalExpression(node->m_a2) +  *EvalExpression(node->m_a3);
    case op_sub:    return *EvalExpression(node->m_a2) -  *EvalExpression(node->m_a3);
    case op_mul:    return *EvalExpression(node->m_a2) *  *EvalExpression(node->m_a3);
    case op_div:    return *EvalExpression(node->m_a2) /  *EvalExpression(node->m_a3);
    case op_mod:    return *EvalExpression(node->m_a2) %  *EvalExpression(node->m_a3);

    case op_eq:     return *EvalExpression(node->m_a2) == *EvalExpression(node->m_a3);
    case op_ne:     return *EvalExpression(node->m_a2) != *EvalExpression(node->m_a3);
    case op_lt:     return *EvalExpression(node->m_a2) <  *EvalExpression(node->m_a3);
    case op_le:     return *EvalExpression(node->m_a2) <= *EvalExpression(node->m_a3);
    case op_gt:     return *EvalExpression(node->m_a2) >  *EvalExpression(node->m_a3);
    case op_ge:     return *EvalExpression(node->m_a2) >= *EvalExpression(node->m_a3);
    
    case op_logor:  return (bool)*EvalExpression(node->m_a2) || 
                           (bool)*EvalExpression(node->m_a3) ;
    case op_logand: return (bool)*EvalExpression(node->m_a2) && 
                           (bool)*EvalExpression(node->m_a3) ;

    case op_seq:    return EvalExpression(node->m_a2)->Compare(*EvalExpression(node->m_a3)) == 0;
    case op_sne:    return EvalExpression(node->m_a2)->Compare(*EvalExpression(node->m_a3)) != 0;

    default:        throw std::out_of_range("Invalid binary operator");
    }

  case ternary_expression:
    return *EvalExpression(node->m_a1) ? EvalExpression(node->m_a2) : EvalExpression(node->m_a3);

  case prefix_expression:
    switch(node->m_a1.GetNumber())
    {
    case op_preinc: 
      {
        VariantRef value = EvalExpression(node->m_a2);
        ++*value;
        return value;
      }
    case op_predec: return --*EvalExpression(node->m_a2);
    case op_negate: return  -*EvalExpression(node->m_a2);
    case op_not:    return  !*EvalExpression(node->m_a2);
    default:        throw std::out_of_range("Invalid prefix operator");
    }

  case postfix_expression:
    switch(node->m_a1.GetNumber())
    {
    case op_postinc: return (*EvalExpression(node->m_a2))++;
    case op_postdec: return (*EvalExpression(node->m_a2))--;
    default:        throw std::out_of_range("Invalid postfix operator");
    }

  case index_expression:
    return (*EvalExpression(node->m_a1))[*EvalExpression(node->m_a2)];

  case function_call:
    return EvalFunctionCall(node);
    break;

  case literal:
    return node->m_a1.GetValue();

  case lvalue:
    return EvalLValue(node);

  case list_literal:
    return EvalListLiteral(node);

  case new_expression:
    return EvalNewExpression(node);

  case this_expression:
    return EvalThisExpression(node);

  case member_expression:
    return EvalMemberExpression(node);

  case member_call:
    return EvalMemberCall(node);
  
  default:
    throw std::out_of_range("Invalid expression type");
  }
  throw std::runtime_error("Not implemented");
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

VariantRef  
Evaluator::EvalLValue(Ast* node)
{
  VariantRef ref;
  if(m_scope->FindVar(node->m_a1, ref))
  {
    return ref;
  }
  throw std::runtime_error("Undeclared variable '" + 
                      node->m_a1.GetString() + "'");
}

void
Evaluator::EvalVarDecl(Ast* node)
{
  // Determine right-hand value
  VariantRef value;
  if(node->m_a2.Empty())
  {
    value = Variant();
  }
  else
  {
    value = EvalExpression(node->m_a2);
  }

  // Create variable
  m_scope->AddVar(node->m_a1, value);
}

void        
Evaluator::EvalFunDecl(Ast* node)
{
  // Insert into scope
  m_scope->AddFun(node->m_a1, node);
}

void 
Evaluator::EvalClassDecl(Ast* node)
{
  // Check class name
  if(m_classes.count(node->m_a1))
  {
    throw std::runtime_error("Class already declared");
  }

  // Create new class
  Class* cl = new Class(node->m_a1);
  m_classes[node->m_a1] = cl;

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
    case variable_declaration:
      cl->AddVar(node->m_a1, node);
      break;

    case function_declaration:
      cl->AddFun(node->m_a1, node);
      break;

    default:
      throw std::runtime_error("Invalid member type");
    }
  }
}

VariantRef  
Evaluator::EvalFunctionCall(Ast* node)
{
  // Script functions
  Ast* fun = 0;
  if(m_scope->FindFun(node->m_a1, fun))
  {
     return EvalScriptCall(fun, node);
  }

  // Native call
  NativeCallInfo* nc = FindNative(node->m_a1);
  if(nc)
  {
    return EvalNativeCall(nc, node);
  }

  // Unknown call
  throw std::runtime_error("Undeclared function '" + node->m_a1.GetString() + "'");
}

VariantRef 
Evaluator::EvalNativeCall(NativeCallInfo* fun, Ast* call)
{
  // Create scope
  CallScope* scope = new CallScope(m_scope);
  AutoScope as(*this, scope);

  // Evaluate arguments
  if(call->m_a2)
  {
    EvalStatement(call->m_a2);
  }

  // Execute native call
  CallScope::Arguments const& args = scope->GetArgs();
  return fun->m_funPtr(args, args.size());
}

VariantRef 
Evaluator::EvalScriptCall(Ast* fun, Ast* call)
{
  // Evaluate arguments
  AutoScope as(*this, new CallScope(m_scope));
  if(call->m_a2)
  {
    EvalStatement(call->m_a2);
    EvalStatement(fun->m_a2);
  }

  // Evaluate function body
  try
  {
    AutoScope as(*this, new Scope(m_scope));
    EvalStatement(fun->m_a3);
    return Variant();
  }
  catch(return_exception const& e)
  {
    return e.m_value;
  }
  catch(break_exception const&)
  {
    throw std::runtime_error("Invalid break statement");
  }
  catch(continue_exception const&)
  {
    throw std::runtime_error("Invalid continue statement");
  }
}

VariantRef 
Evaluator::EvalListLiteral(Ast* node)
{
  // Create empty map
  VariantRef v(Variant::stAssoc);
  
  // Recurse into map values
  Ast* child = node->m_a1;
  while(child)
  {
    v->Append(EvalExpression(child->m_a1->m_a1));
    if(child->m_a2.Empty()) 
    {
      break;
    }
    child = child->m_a2;
  }

  // Done
  return v;
}

void 
Evaluator::EvalReturnStatement(Ast* node)
{
  // Determine return value
  VariantRef value;
  if(node->m_a1.Empty())
  {
    value = Variant::Null;
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
  AutoScope scope(*this, new Scope(m_scope));
  
  // Evaluate init expression
  EvalStatement(node->m_a1);

  // Evaluate loop
  for(;;)
  {
    // Evaluate condition
    if(!*EvalExpression(node->m_a2))
    {
      break;
    }

    // For body
    try 
    {
      AutoScope scope(*this, new Scope(m_scope));
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
  AutoScope scope(*this, new Scope(m_scope));

  String varName;
  Ast*   varNode = node->m_a1;

  // Determine variable name
  if(varNode->m_type == variable_declaration)
  {
    EvalStatement(node->m_a1);
    varName = node->m_a1->m_a1;
  }
  else
  {
    varName = node->m_a1;
  }

  // Fetch variable
  VariantRef var;
  if(!m_scope->FindVar(varName, var))
  {
    throw std::runtime_error("Failed to find iterator variable");
  }

  // Evaluate expression
  VariantRef rhs = EvalExpression(node->m_a2);

  // Fetch iterator
  Variant::AssocType::iterator it = rhs->GetMap().begin();
  Variant::AssocType::iterator ie = rhs->GetMap().end();

  // Enumerate members
  for(; it != ie; ++it)
  {
    // Assign value to iterator variable
    *var = *it->second;

    // Evaluate expression
    try
    {
      AutoScope scope(*this, new Scope(m_scope));
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
  if(*EvalExpression(node->m_a1))
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
  while(*EvalExpression(node->m_a1))
  {
    try
    {
      AutoScope scope(*this, new Scope(m_scope));
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
  VariantRef value = EvalExpression(node->m_a1);

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
    else if(*EvalExpression((*it)->m_a1) == *value)
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
      AutoScope as(*this, new Scope(m_scope));
      EvalStatement(statement);
    }
    catch(break_exception const&)
    {
    }
  }
}

VariantRef 
Evaluator::EvalNewExpression(Ast* node)
{
  // Find class type
  Classes::iterator it = m_classes.find(node->m_a1);
  if(it == m_classes.end())
  {
    throw std::runtime_error("Undefined class '" + 
                    node->m_a1.GetString() + "'");
  }

  // Instantiate
  return it->second->CreateInstance(*this);
}

VariantRef 
Evaluator::EvalMemberExpression(Ast* node)
{
  // Evaluate left side
  VariantRef instVar = EvalExpression(node->m_a1);
  if(instVar->GetType() != Variant::stResource)
  {
    throw std::runtime_error("Expression does not yield a class instance");
  }
  Instance* instPtr = dynamic_cast<Instance*>(instVar->GetResource());
  if(instPtr == 0)
  {
    throw std::runtime_error("Expression does not yield a class instance");
  }

  // Retrieve value
  VariantRef ref;
  if(!instPtr->FindVar(node->m_a2, ref))
  {
    throw std::runtime_error("Class has no member '" + node->m_a2.GetString() + "'");
  }
  return ref;
}

VariantRef 
Evaluator::EvalMemberCall(Ast* node)
{
  // Extract subnodes
  Ast* lhs = node->m_a1;
  Ast* rhs = node->m_a2;

  // Evaluate left side
  VariantRef instVar = EvalExpression(lhs);
  if(instVar->GetType() != Variant::stResource)
  {
    throw std::runtime_error("Expression does not yield a class instance");
  }
  Instance* instPtr = dynamic_cast<Instance*>(instVar->GetResource());
  if(instPtr == 0)
  {
    throw std::runtime_error("Expression does not yield a class instance");
  }

  // Determine function name
  String name = rhs->m_a1;

  // Retrieve function
  Ast* fun = 0;
  if(!instPtr->FindFun(name, fun))
  {
    throw std::runtime_error("Class has no method '" + name + "'");
  }

  // Evaluate arguments
  CallScope* callScope = new CallScope(m_scope);
  PushScope(callScope);
  if(rhs->m_a2)
  {
    EvalStatement(rhs->m_a2);
    EvalStatement(fun->m_a2);
  }
  m_scope = m_scopes.front();
  m_scopes.pop_front();

  // Push class scope
  AutoScope as(*this, new ClassScope(m_global, instPtr));
  // TODO insert this

  // Push call as scope
  callScope->SetParent(m_scope);
  AutoScope cs(*this, callScope);

  // Push regular scope
  AutoScope fs(*this, new Scope(m_scope));

  // Evaluate the function
  EvalStatement(fun->m_a3);
  return Variant();
}

VariantRef 
Evaluator::EvalThisExpression(Ast* node)
{
  Scope* scope = m_scope;
  while(scope)
  {
    ClassScope* cs = dynamic_cast<ClassScope*>(scope);
    if(cs)
    {
      return cs->GetInstance();
    }
    scope = scope->GetParent();
  }
  throw std::runtime_error("Invalid context for this");
}

void
Evaluator::EvalParameter(Ast* node)
{
  // Fetch call scope
  CallScope* scope = dynamic_cast<CallScope*>(m_scope);
  if(scope == 0)
  {
    throw std::runtime_error("Invalid scope for parameter evaluation");
  }

  // Check argument count
  if(scope->GetArgCount() == scope->GetVarCount())
  {
    throw std::runtime_error("Not enough arguments in call to function");
  }

  // Add variable to scope
  scope->AddVar(node->m_a1.GetString(), scope->GetArg(scope->GetVarCount()));
}
