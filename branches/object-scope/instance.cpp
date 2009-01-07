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
#include "instance.h"
#include "eval.h"

/*static*/ Instance* 
Instance::Create(Evaluator* eval, Class const* c)
{
  return new Instance(eval, c);
}

Instance::Instance(Evaluator* eval, Class const* c) :
m_class     (c)
{
  // Enumerate variable definitions
  Class::NamedNodeMap::const_iterator it, ie;
  it = m_class->GetMembers().begin();
  ie = m_class->GetMembers().end();
  for(; it != ie; ++it)
  {
    // Evaluate initial value
    Value value;
    if(it->second->m_a2.Empty())
    {
      value.Clear();
    }
    else
    {
      value.SetValue(eval->EvalExpression(it->second->m_a2));
    }

    // Instantiate member variable
    GetMembers()[it->first] = new RWVariable(this, value);
  }

  // Add functions
  Class::FunctionMap::const_iterator fi, fe;
  fi = m_class->GetMethods().begin();
  fe = m_class->GetMethods().end();
  for(; fi != fe; ++fi)
  {
    GetMembers()[fi->first] = new ROVariable(this, fi->second);
  }
}

