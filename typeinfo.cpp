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
#include "typeinfo.h"
#include "class.h"
#include "ast.h"

/*static*/ String 
TypeInfo::GetTypeName(Variant const& value)
{
  switch(value.GetType())
  {
  case Variant::stNull:     return "null";
  case Variant::stBool:     return "bool";
  case Variant::stInt:      return "int";
  case Variant::stString:   return "string";
  case Variant::stAssoc:    return "assoc";
  case Variant::stResource:
    if(Instance* inst = dynamic_cast<Instance*>(value.GetResource()))
    {
      return inst->GetClass()->GetName();
    }
  }
  throw std::runtime_error("Unknown type");
}

TypeInfo::TypeInfo(Variant::SubTypes type, String const& name) :
m_type (type),
m_name (name)
{
}

TypeInfo::TypeInfo(Ast* node) :
m_type ((Variant::SubTypes)node->m_a1.GetNumber()),
m_name (node->m_a2.GetString())
{
}

TypeInfo::TypeInfo(Variant const& value) :
m_type (value.GetType()),
m_name (GetTypeName(value))
{
}

TypeInfo::TypeInfo(VariantRef const& value) :
m_type (value->GetType()),
m_name (GetTypeName(*value))
{
}
