#include "scope.h"
#include "eval.h"
#include "ast.h"
#include "function.h"
#include "class.h"

void 
GlobalScope::AddFun(Function* fun)
{
  if(m_funs.count(fun->GetName()))
  {
    throw std::runtime_error("Function already declared");
  }
  m_funs[fun->GetName()] = fun;
}

void 
GlobalScope::AddClass(Class* c)
{
  if(m_classes.count(c->GetName()))
  {
    throw std::runtime_error("Class already declared");
  }
  m_classes[c->GetName()] = c;
}

bool 
ClassScope::FindVarLocal(String const& name, VariantRef& ref) const
{
  return m_inst->FindVar(name, ref);
}

bool 
ClassScope::FindFunLocal(String const& name, Function*& fun) const
{
  return m_inst->FindFun(name, fun);
}
