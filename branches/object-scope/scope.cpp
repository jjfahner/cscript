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
#include "scope.h"
#include "eval.h"
#include "ast.h"
#include "function.h"
#include "instance.h"

int Scope::g_scopeId = 0;

Scope::Scope(Scope* parent) :
m_parent  (parent),
m_scopeId (++g_scopeId)
{
}

void 
Scope::Attach()
{
  if(m_parent == 0)
  {
    return;
  }

  RValue*& ptr = m_parent->m_variables["scope::child"];
  
  if(ptr != 0)
  {
    throw std::runtime_error("Attaching to parent with previous child");
  }
  
  ptr = new ROVariable(this);

  Scope* p = this;
  Scope* s = m_parent;
  while(s)
  {
    if(s->m_variables.count("scope::child") == 0)
    {
      break;
    }
    if(&s->m_variables["scope::child"]->GetObject() != p)
    {
      break;
    }
    s = s->m_parent;
  }
}

void 
Scope::Detach()
{
  if(m_parent)
  {
    delete m_parent->m_variables["scope::child"];
    m_parent->m_variables.erase("scope::child");
  }
}


void 
GlobalScope::AddFun(Function* fun)
{
  // Check name
  if(m_variables.count(fun->GetName()))
  {
    throw std::runtime_error("Variable or function already declared");
  }

  // Register function
  m_variables[fun->GetName()] = new RWVariable(fun);
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
ClassScope::FindVarLocal(String const& name, RValue*& ptr) const
{
  return m_inst->FindVar(name, ptr);
}

bool 
ClassScope::FindFunLocal(String const& name, Function*& fun) const
{
  MemberFunction* memfun;
  if(m_inst->FindMethod(name, memfun))
  {
    fun = memfun;
    return true;
  }
  return false;
}
