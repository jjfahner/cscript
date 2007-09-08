#include "eval.h"
#include "ast.h"
#include "parser.h"
#include "astlist.h"

//////////////////////////////////////////////////////////////////////////
//
// Scope implementation
//

struct Evaluator::Scope
{
  //
  // Types
  //
  typedef std::map<String, VariantRef>  Variables;
  typedef std::map<String, Ast*>        Functions;
  typedef std::vector<VariantRef>       Arguments;

  //
  // Members
  //
  Scope*      m_parent;
  Ast*        m_node;
  Variables   m_vars;
  Functions   m_funs;
  Arguments   m_args;

};

struct Evaluator::AutoScope
{
  Evaluator& m_eval;
  AutoScope(Evaluator& eval, Ast* node) : m_eval (eval) 
  {
    m_eval.PushScope(node);
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

Evaluator::Evaluator() :
m_scope   (0)
{
  m_global = new Scope;
  m_global->m_node   = 0;
  m_global->m_parent = 0;
  m_scope = m_global;
}

void 
Evaluator::Eval(String file)
{
  Parser parser(m_reporter);
  parser.Parse(file);
  if(m_reporter.GetErrorCount())
  {
    std::cout << "Aborted.\n";
    return;
  }

  try
  {
    EvalStatement(parser.GetRoot());
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
Evaluator::PushScope(Ast* node)
{
  Scope* newScope = new Scope;
  newScope->m_parent  = m_scope;
  newScope->m_node    = node;
  m_scope = newScope;
}

void 
Evaluator::PopScope()
{
  Scope* oldScope = m_scope;
  m_scope = oldScope->m_parent;
  delete oldScope;
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
    // TODO validate index in args vector
    m_scope->m_vars.insert(std::make_pair(
      node->m_a1.GetString(), 
      m_scope->m_args[m_scope->m_vars.size()]));
    break;

  case argument_list:
    EvalStatement(node->m_a1);
    EvalStatement(node->m_a2);
    break;

  case argument:
    m_scope->m_args.push_back(EvalExpression(node->m_a1));    
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
    if(*EvalExpression(node->m_a1))
    {
      EvalStatement(node->m_a2);
    }
    else if(node->m_a3)
    {
      EvalStatement(node->m_a3);
    }
    break;

  case while_statement:
    EvalWhileStatement(node);
    break;
  
  case return_statement:
    if(node->m_a1.Empty())
    {
      throw return_exception(node);
    }
    else
    {
      throw return_exception(node, EvalExpression(node->m_a1));
    }
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
      AutoScope as(*this, node);
      EvalStatement(node->m_a1);
    }
    break;

  case member_call:
    break;
  
  case pause_statement:
    break;
  
    // Invalid
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
    case op_logor:  return *EvalExpression(node->m_a2) || *EvalExpression(node->m_a3);
    case op_logand: return *EvalExpression(node->m_a2) && *EvalExpression(node->m_a3);
    case op_eq:     return *EvalExpression(node->m_a2) == *EvalExpression(node->m_a3);
    case op_ne:     return *EvalExpression(node->m_a2) != *EvalExpression(node->m_a3);
    case op_lt:     return *EvalExpression(node->m_a2) <  *EvalExpression(node->m_a3);
    case op_le:     return *EvalExpression(node->m_a2) <= *EvalExpression(node->m_a3);
    case op_gt:     return *EvalExpression(node->m_a2) >  *EvalExpression(node->m_a3);
    case op_ge:     return *EvalExpression(node->m_a2) >= *EvalExpression(node->m_a3);
    default:        throw std::out_of_range("Invalid binary operator");
    }

  case ternary_expression:
    return *EvalExpression(node->m_a1) ? EvalExpression(node->m_a2) : EvalExpression(node->m_a3);

  case prefix_expression:
    switch(node->m_a1.GetNumber())
    {
    case op_preinc: return ++*EvalExpression(node->m_a2);
    case op_predec: return --*EvalExpression(node->m_a2);
    case op_negate: return  -*EvalExpression(node->m_a2);
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
  case this_expression:
  case member_expression:
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
  // Walk scopes
  Scope* scope = m_scope;
  for(;;)
  {
    // Find variable in scope
    Scope::Variables::iterator it;
    it = scope->m_vars.find(node->m_a1);
    if(it != scope->m_vars.end())
    {
      return it->second;
    }

    // Determine next level
    if(scope->m_node->m_type == function_declaration)
    {
      break;
    }
    if((scope = scope->m_parent) == 0)
    {
      break;
    }
  }

  // Not found
  throw std::runtime_error("Undeclared variable '" + node->m_a1.GetString() + "'");
}

void
Evaluator::EvalVarDecl(Ast* node)
{
  // Check for duplicate declaration
  if(m_scope->m_vars.count(node->m_a1))
  {
    throw std::runtime_error("Variable '" + node->m_a1.GetString() + "' already declared");
  }

  // Determine right-hand value
  VariantRef value;
  if(node->m_a2)
  {
    value = EvalExpression(node->m_a2);
  }

  // Create variable
  m_scope->m_vars[node->m_a1] = value;
}

void        
Evaluator::EvalFunDecl(Ast* node)
{
  // Check for duplicate declaration
  if(m_scope->m_funs.count(node->m_a1))
  {
    throw std::runtime_error("Function '" + node->m_a1.GetString() + "' already declared");
  }

  // Insert into map
  m_scope->m_funs[node->m_a1] = node;
}

void 
Evaluator::EvalClassDecl(Ast* node)
{

}

VariantRef  
Evaluator::EvalFunctionCall(Ast* node)
{
  Ast* fun = 0;

  // Walk scopes
  Scope* scope = m_scope;
  for(;;)
  {
    // Find function declaration
    Scope::Functions::iterator it;
    it = scope->m_funs.find(node->m_a1);
    if(it != scope->m_funs.end())
    {
      fun = it->second;
      break;
    }

    // Parent scope
    if((scope = scope->m_parent) == 0)
    {
      break;
    }
  }

  // Unknown function
  if(fun == 0)
  {
    // Hack
    if(node->m_a1.GetString() == "print")
    {
      AutoScope as(*this, node);
      EvalStatement(node->m_a2);
      std::cout << m_scope->m_args[0]->AsString();
      return m_scope->m_args[0];
    }
    else
    {
      throw std::runtime_error("Undeclared function '" + node->m_a1.GetString() + "'");
    }
  }

  // Evaluate arguments
  AutoScope as(*this, node);
  EvalStatement(node->m_a2);
  EvalStatement(fun->m_a2);

  // Evaluate function body
  try
  {
    AutoScope as(*this, node);
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
  VariantRef v(Variant::stAssoc);
  
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

  return v;
}

void 
Evaluator::EvalForStatement(Ast* node)
{
  // Outer scope
  AutoScope scope(*this, node);
  
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
      AutoScope scope(*this, node);
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
  //VariantRef ref = EvalExpression(node->m_a1);
}

void        
Evaluator::EvalWhileStatement(Ast* node)
{
  while(*EvalExpression(node->m_a1))
  {
    try
    {
      AutoScope scope(*this, node);
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

  // Find statement
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
      AutoScope as(*this, node);
      EvalStatement(statement);
    }
    catch(break_exception const&)
    {
    }
  }
}
