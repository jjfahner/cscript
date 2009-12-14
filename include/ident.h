//////////////////////////////////////////////////////////////////////////
//
// This file is © 2009 JJ Fahner <jan-jaap@jan-jaap.net>
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
#ifndef CSCRIPT_IDENT_H
#define CSCRIPT_IDENT_H

#include <cscript.h>
#include <value.h>

typedef unsigned int IdentId;

class Identifier
{
public:

  //
  // Resolve id to string
  //
  static String const& Lookup(IdentId id);

  //
  // Resolve string to id
  //
  static IdentId Lookup(String const& name);

  //
  // Default constructor
  //
  Identifier();

  //
  // Construct from IdentId
  //
  Identifier(IdentId id);

  //
  // Construct from name
  //
  Identifier(String const& name);

  //
  // Construct from name
  //
  Identifier(char const* name);

  //
  // Construction from Value
  //
  Identifier(Value const& value);

  //
  // Retrieve id
  //
  IdentId GetId() const;

  //
  // Retrieve name
  //
  String const& GetName() const;

  //
  // Convert to id
  //
  operator IdentId () const;

  //
  // Convert to name
  //
  operator String const& () const;

  //
  // Convert to value
  //
  operator Value () const;

private:

  //
  // Members
  //
  IdentId m_id;
  mutable String const* m_name;

};

//////////////////////////////////////////////////////////////////////////

inline Identifier::Identifier()
: m_id   (0),
  m_name (0)
{
}

inline Identifier::Identifier(IdentId id)
: m_id   (id),
  m_name (0)
{
}

inline Identifier::Identifier(String const& name)
: m_id   (Lookup(name)),
  m_name (0)
{
}

inline Identifier::Identifier(char const* name)
: m_id   (Lookup(name)),
  m_name (0)
{
}

inline Identifier::Identifier(Value const& value)
: m_id   (0),
  m_name (0)
{
  switch (value.Type())
  {
  case Value::tInt:
    m_id = (IdentId) value.GetInt();
    break;
  case Value::tString:
    m_id = Lookup(value.GetString());
    break;
  default:
    throw std::runtime_error("Invalid identifier initializer");
  }
}

inline IdentId Identifier::GetId() const
{
  return m_id;
}

inline String const& Identifier::GetName() const
{
  if (m_name == 0)
  {
    m_name = &Lookup(m_id);
  }
  return *m_name;
}

inline Identifier::operator IdentId () const
{
  return GetId();
}

inline Identifier::operator String const& () const
{
  return GetName();
}

inline Identifier::operator Value () const
{
  return Value(m_id);
}

//////////////////////////////////////////////////////////////////////////

inline bool operator < (Identifier const& lhs, Identifier const& rhs)
{
  return lhs.GetId() < rhs.GetId();
}

#endif // CSCRIPT_IDENT_H
