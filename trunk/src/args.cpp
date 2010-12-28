//////////////////////////////////////////////////////////////////////////
//
// This file is © 2007 - 2011 JJ Fahner <jan-jaap@jan-jaap.net>
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
#include "args.h"
#include "object.h"
#include "astnode.h"

Value const& 
Arguments::at(String const& name) const
{
  Enumerator* pEnum = m_parameters->GetEnumerator();
  size_t index = 0;

  Value parameter;
  while(pEnum->GetNext(parameter))
  {
    if(Ast_A1(parameter).GetString() == name)
    {
      return at(index);
    }
    ++index;
  }

  throw std::runtime_error("Function has no parameter '" + name + "'");
}

//////////////////////////////////////////////////////////////////////////
