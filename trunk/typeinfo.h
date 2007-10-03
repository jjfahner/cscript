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
#ifndef CSCRIPT_TYPEINFO_H
#define CSCRIPT_TYPEINFO_H

#include "var.h"

//
// Type information
//
class TypeInfo
{
public:

  //
  // Retrieve string name from variant
  //
  static String GetTypeName(Variant const& value);

  //
  // Construct explicitly
  //
  TypeInfo(Variant::SubTypes type, String const& name);

  //
  // Construct from node
  //
  TypeInfo(Ast* node);
  
  //
  // Construct from variant
  //
  TypeInfo(Variant const& value);

  //
  // Construct from variant ref
  //
  TypeInfo(VariantRef const& value);

  //
  // Retrieve subtype
  //
  Variant::SubTypes GetType() const
  {
    return m_type;
  }

  //
  // Retrieve name
  //
  String const& GetName() const
  {
    return m_name;
  }

  //
  // Automatic conversion to type
  //
  operator Variant::SubTypes () const
  {
    return m_type;
  }

  //
  // Automatic conversion to string
  //
  operator String const& () const
  {
    return m_name;
  }

protected:

  //
  // Members
  //
  Variant::SubTypes m_type;
  String            m_name;

};

inline bool 
operator == (TypeInfo const& lhs, TypeInfo const& rhs)
{
  if(lhs.GetType() != rhs.GetType())
  {
    return false;
  }
  if(lhs.GetType() != Variant::stInstance)
  {
    return true;
  }
  return lhs.GetName() == rhs.GetName();
}

#endif // CSCRIPT_TYPEINFO_H