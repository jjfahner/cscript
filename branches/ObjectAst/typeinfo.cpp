//////////////////////////////////////////////////////////////////////////
//
// This file is © 2007 - 2009 JJ Fahner <jan-jaap@jan-jaap.net>
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
#include "typeinfo.h"
#include "ast.h"
#include "variable.h"

/*static*/ String 
TypeInfo::GetTypeName(Value const& value)
{
  switch(value.Type())
  {
  case Value::tNull:    return "null";
  case Value::tBool:    return "bool";
  case Value::tInt:     return "int";
  case Value::tString:  return "string";
  case Value::tObject:  return "object";
  }
  throw std::runtime_error("Unknown type");
}

TypeInfo::TypeInfo(Value::Types type, String const& name) :
m_type (type),
m_name (name)
{
}

TypeInfo::TypeInfo(Object& node) :
m_type ((Value::Types)node["a1"].GetInt()),
m_name (node["a2"])
{
}

TypeInfo::TypeInfo(Value const& value) :
m_type (value.Type()),
m_name (GetTypeName(value))
{
}
