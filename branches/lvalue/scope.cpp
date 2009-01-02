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
