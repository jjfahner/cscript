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

Scope::~Scope()
{
  Variables::iterator it = m_vars.begin();
  for(; it != m_vars.end(); ++it)
  {
    delete it->second;
  }
}

void 
GlobalScope::AddFun(Function* fun)
{
  // Register function in variable list to prevent collection
  m_vars[fun->GetName()] = new RWVariable(fun);
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
ClassScope::FindLocal(String const& name, RValue*& ptr) const
{
  return m_inst->Find(name, ptr);
}

