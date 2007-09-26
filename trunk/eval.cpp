#include "eval.h"
#include "ast.h"
#include "parser.h"
#include "astlist.h"
#include "native.h"
#include "scope.h"
#include "function.h"
#include "class.h"

//////////////////////////////////////////////////////////////////////////
//
// Scope implementation
//

struct Evaluator::AutoScope
{
  Evaluator& m_eval;
  bool m_delete;
  AutoScope(Evaluator& eval, Scope* scope, bool autoDelete = true) : 
  m_eval    (eval), 
  m_delete  (autoDelete)
  {
    m_eval.PushScope(scope);
  }
  ~AutoScope()
  {
    m_eval.PopScope(m_delete);
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
m_scope   (0)
{
  m_global = new GlobalScope(&GetGlobalScope());
}

void 
Evaluator::Reset()
{
  // Clear any errors
  m_reporter.Reset();

  // Create new global scope
  delete m_global;
  m_global = new GlobalScope(&GetGlobalScope());
}

VariantRef 
Evaluator::GetClassList() const
{
  VariantRef list(Variant::stAssoc);
  Scope::Classes const& classes = m_global->GetClasses();
  Scope::Classes::const_iterator it, ie;
  it = classes.begin();
  ie = classes.end();
  for(; it != ie; ++it)
  {
    list->Append(it->first);
  }
  return list;
}

VariantRef 
Evaluator::GetFunctionList() const
{
  VariantRef list(Variant::stAssoc);
  Scope::Functions const& funs = m_global->GetFunctions();
  Scope::Functions::const_iterator it, ie;
  it = funs.begin();
  ie = funs.end();
  for(; it != ie; ++it)
  {
    list->Append(it->first);
  }
  return list;
}

VariantRef 
Evaluator::Eval(String text)
{
  // Clear errors
  m_reporter.Reset();

  // Parse code
  Parser parser(m_reporter);
  parser.ParseText(text.c_str());

  // Check error count
  if(m_reporter.GetErrorCount())
  {
    std::cout << "Aborted.\n";
    return Variant::Null;
  }

  // Retrieve code root
  Ast* root = parser.GetRoot();
  parser.SetRoot(0);

  // Evaluate code
  try
  {
    // Push global scope. Don't delete when done
    AutoScope gs(*this, m_global, false);

    // Evaluate code
    EvalStatement(root);
    
    // Statement returned nothing
    return Variant::Null;
  }
  catch(return_exception const& e)
  {
    // Statement returned value
    return e.m_value;
  }
  catch(reset_exception const&)
  {
    // Reset request
    Reset();
  }
  catch(std::exception const& e)
  {
    // Statement threw exception
    std::cout << e.what() << "\n";
    return Variant::Null;
  }
  catch(...)
  {
    std::cout << "Unexpected\n";
    return Variant::Null;
  }
}

void
Evaluator::PushScope(Scope* scope)
{
  m_scopes.push_front(m_scope);
  m_scope = scope;
}

void 
Evaluator::PopScope(bool doDelete)
{
  // Remove scope from stack
  Scope* old = m_scope;
  m_scope = m_scopes.front();
  m_scopes.pop_front();

  // Delete the scope
  if(doDelete)
  {
    delete old;
  }
}

void 
Evaluator::EvalStatement(Ast* node)
{
  switch(node->m_type)
  {
  case empty_statement:       break;
  case translation_unit:      EvalStatement(node->m_a1);  break;
  case statement_sequence:    EvalStatementSeq(node);     break;
  case expression_statement:  EvalExpression(node->m_a1); break;

  case declaration_sequence:
    EvalStatement(node->m_a1);
    EvalStatement(node->m_a2);
    break;

  case variable_declaration:  EvalVarDecl(node);  break;
  case function_declaration:  EvalFunDecl(node);  break;
  case extern_declaration:    EvalExternDecl(node);  break;
  case class_declaration:     EvalClassDecl(node);   break;
  case class_members:         break;  
  case access_specifier:      break;

  case for_statement:         EvalForStatement(node);     break;
  case foreach_statement:     EvalForeachStatement(node); break;
  case if_statement:          EvalIfStatement(node);      break;
  case while_statement:       EvalWhileStatement(node);   break;  
  case return_statement:      EvalReturnStatement(node);  break;
  case switch_statement:      EvalSwitchStatement(node);  break;

  case break_statement:       throw break_exception(node);  
  case continue_statement:    throw continue_exception(node);

  case compound_statement:
    if(node->m_a1)
    {
      AutoScope as(*this, new Scope(m_scope));
      EvalStatement(node->m_a1);
    }
    break;

  default: throw std::out_of_range("Invalid node type");
  }
}

VariantRef
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
  case function_call:         return EvalFunctionCall(node);
  case literal_value:         return node->m_a1.GetValue();
  case lvalue:                return EvalLValue(node);
  case list_literal:          return EvalListLiteral(node);
  case new_expression:        return EvalNewExpression(node);
  case this_expression:       return EvalThisExpression(node);
  case member_expression:     return EvalMemberExpression(node);
  }
  throw std::out_of_range("Invalid expression type");
}

VariantRef 
Evaluator::EvalAssignment(Ast* node)
{
  switch(node->m_a1.GetNumber())
  {
  case op_assign: return *EvalExpression(node->m_a2)  = *EvalExpression(node->m_a3);
  case op_assadd: return *EvalExpression(node->m_a2) += *EvalExpression(node->m_a3);
  case op_asssub: return *EvalExpression(node->m_a2) -= *EvalExpression(node->m_a3);
  case op_assmul: return *EvalExpression(node->m_a2) *= *EvalExpression(node->m_a3);
  case op_assdiv: return *EvalExpression(node->m_a2) /= *EvalExpression(node->m_a3);
  case op_assmod: return *EvalExpression(node->m_a2) %= *EvalExpression(node->m_a3);
  }
  throw std::out_of_range("Invalid assignment operator");
}

VariantRef 
Evaluator::EvalBinary(Ast* node)
{
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
  case op_seq:    return EvalExpression(node->m_a2)->Compare(*EvalExpression(node->m_a3), true) == 0;
  case op_sne:    return EvalExpression(node->m_a2)->Compare(*EvalExpression(node->m_a3), true) != 0;
  }  
  throw std::out_of_range("Invalid binary operator");
}

VariantRef 
Evaluator::EvalTernary(Ast* node)
{
  if(EvalExpression(node->m_a1)->AsBool())
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

VariantRef 
Evaluator::EvalPrefix(Ast* node)
{
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
  }
  throw std::out_of_range("Invalid prefix operator");
}

VariantRef 
Evaluator::EvalPostfix(Ast* node)
{
  switch(node->m_a1.GetNumber())
  {
  case op_postinc: return (*EvalExpression(node->m_a2))++;
  case op_postdec: return (*EvalExpression(node->m_a2))--;
  }
  throw std::out_of_range("Invalid postfix operator");
}

VariantRef
Evaluator::EvalIndex(Ast* node)
{
  return (*EvalExpression(node->m_a1))[*EvalExpression(node->m_a2)];
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
    // Assign value-of rhs
    value = *EvalExpression(node->m_a2);
  }

  // Create variable
  m_scope->AddVar(node->m_a1, value);
}

void        
Evaluator::EvalFunDecl(Ast* node)
{
  m_scope->AddFun(new ScriptFunction(node->m_a1, node));
}

void 
Evaluator::EvalExternDecl(Ast* node)
{
  m_scope->AddFun(new ExternFunction(node->m_a1));
}

void 
Evaluator::EvalClassDecl(Ast* node)
{
  // TODO memory management

  // Create new class
  Class* cl = new Class(node->m_a1);
  
  // Insert into scope - this fails 
  // outside of global/class scopes
  m_scope->AddClass(cl);

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
      cl->AddFun(node->m_a1, new ScriptFunction(node->m_a1, node));
      break;

    default:
      throw std::runtime_error("Invalid member type");
    }
  }
}

VariantRef  
Evaluator::EvalFunctionCall(Ast* node)
{
  Function* fun = 0;
  Arguments args;

  // Resolve function pointer
  if(node->m_a3)
  {
    // Evaluate instance expression
    args.push_back(EvalExpression(node->m_a3));

    // Resolve function on object
    if(!args[0]->GetTypedRes<Instance>()->FindFun(node->m_a1, fun))
    {
      throw std::runtime_error("Object doesn't support this method");
    }
  }
  else
  {
    // Resolve function in scope stack
    if(!m_scope->FindFun(node->m_a1, fun))
    {
      throw std::runtime_error("Unknown method");
    }
  }

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
    return fun->Execute(*this, args);
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

void 
Evaluator::EvalPositionalArguments(Function* fun, AstList const* arglist, Arguments& args)
{
  AstList const* parlist = fun->GetParameters();

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
      // TODO varargs
      throw std::runtime_error("Too many arguments in call to '" + fun->GetName() + "'");
    }

    // End of arguments
    if(ai == ae)
    {
      if((*pi)->m_a3)
      {
        args.push_back(EvalExpression((*pi)->m_a3));
        ++pi;
        continue;
      }
      throw std::runtime_error("Not enough arguments in call to '" + fun->GetName() + "'");
    }

    // Evaluate argument
    args.push_back(EvalExpression(*ai));

    // Next iteration
    ++pi;
    ++ai;
  }
}

void 
Evaluator::EvalNamedArguments(Function* fun, AstList const* arglist, Arguments& args)
{
  // TODO: validate superfluous/double arguments

  AstList const* parlist = fun->GetParameters();

  // Enumerate parameters
  AstList::const_iterator pi, pe;
  pi = parlist->begin();
  pe = parlist->end();
  for(; pi != pe; ++pi)
  {
    // Extract parameter name
    String parname = (*pi)->m_a1.GetString();

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

    // Handle result
    if(ai != ae)
    {
      // Fill in positional argument
      args.push_back(EvalExpression((*ai)->m_a2));
    }
    else if((*pi)->m_a3)
    {
      // Evaluate default value
      args.push_back(EvalExpression((*pi)->m_a3));
    }
    else
    {
      // Missing argument
      throw std::runtime_error("No value specified for parameter '" + parname + "'");
    }
  }
}

VariantRef 
Evaluator::EvalScriptCall(ScriptFunction* fun, Arguments const& args)
{
  // Parameter iterators
  AstList::const_iterator pi, pe;
  pi = fun->GetParameters()->begin();
  pe = fun->GetParameters()->end();

  // Create parameter scope
  AutoScope ps(*this, new Scope(m_global));

  // Enumerate parameters
  size_t index = 0;
  for(; pi != pe; ++pi, ++index)
  {
    m_scope->AddVar((*pi)->m_a1, args[index]);
  }

  // Create function execution scope
  AutoScope es(*this, new Scope(m_scope));

  // Execute expression
  EvalStatement(fun->GetNode()->m_a3);

  // No return value
  return Variant();
}

Instance* 
Evaluator::EvalInstance(Ast* node)
{
  // Evaluate expression
  VariantRef instVar = EvalExpression(node);

  // Depending on type, box the return value
  switch(instVar->GetType())
  {
  case Variant::stResource:
    break;
  case Variant::stBool:
  case Variant::stInt:
  case Variant::stString:
  case Variant::stAssoc:
//    return instVar;
  default:
    throw std::runtime_error("Expression does not yield an object");
  }

  // Must be a resource
  if(instVar->GetType() != Variant::stResource)
  {
    throw std::runtime_error("Expression does not yield a class instance");
  }
  
  // Must be an instance
  Instance* instPtr = dynamic_cast<Instance*>(instVar->GetResource());
  if(instPtr == 0)
  {
    throw std::runtime_error("Expression does not yield a class instance");
  }

  // Done
  return instPtr;
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
    EvalStatement(varNode);
    varName = varNode->m_a1.GetString();
  }
  else
  {
    varName = node->m_a1.GetString();
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
  Class* c = 0;
  if(!m_scope->FindClass(node->m_a1, c))
  {
    throw std::runtime_error("Undefined class '" + 
                    node->m_a1.GetString() + "'");
  }

  // Instantiate
  return c->CreateInstance(*this);
}

VariantRef 
Evaluator::EvalMemberExpression(Ast* node)
{
  // Evaluate left side
  Instance* instPtr = EvalInstance(node->m_a1);

  // Retrieve value
  VariantRef ref;
  if(!instPtr->FindVar(node->m_a2, ref))
  {
    throw std::runtime_error("Class has no member '" + node->m_a2.GetString() + "'");
  }
  return ref;
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
