//////////////////////////////////////////////////////////////////////////
//
// This file is � 2007 JJ Fahner <jan-jaap@jan-jaap.net>
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
#include "class.h"
#include "ast.h"
#include "eval.h"

Instance* 
Class::CreateInstance(Evaluator* eval) const
{
  // Create new instance
  Instance* inst = new Instance(this);

  // Enumerate variable definitions
  try
  {
    NamedNodeMap::const_iterator it, ie;
    it = m_vars.begin();
    ie = m_vars.end();
    for(; it != ie; ++it)
    {
      // Evaluate initial value
      VariantRef value;
      if(it->second->m_a2.Empty())
      {
        value = Variant::Null;
      }
      else
      {
        value = eval->EvalExpression(it->second->m_a2);
      }

      // Instantiate member variable
      inst->m_vars[it->first] = value;
    }

    // Return new instance
    return inst;
  }
  catch(...)
  {
    // Do cleanup
    delete inst;
    throw;
  }
}

