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
#include "object.h"
#include "datatype.h"
#include "function.h"

DataType* 
Object::GetType()
{
  return ObjectType::Instance();
}

bool
Object::TryEval(Value const& key, Arguments& arguments, Value& result)
{
  // Find method
  Value method;
  if(!TryGet(key, method))
  {
    return false;
  }

  // Try to convert to function
  Function* fun = dynamic_cast<Function*>(method.GetObject());
  if(fun == 0)
  {
    return false;
  }

  // Evaluate the function
  result = fun->Execute(arguments);

  // Success
  return true;
}
