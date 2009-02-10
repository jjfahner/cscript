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
#ifndef CSCRIPT_ASTDATA_H
#define CSCRIPT_ASTDATA_H

#include "types.h"
#include "value.h"

class AstData
{
public:

  AstData(Value value = Value()) : m_value (value)
  {
  }

  bool Empty() const
  {
    return m_value.Empty();
  }

  operator bool () const
  {
    return !Empty();
  }

  bool operator ! () const
  {
    return Empty();
  }

  Value::Types Type() const
  {
    return m_value.Type();
  }

  String const& GetString() const
  {
    return m_value.GetString();
  }

  Value const& GetValue() const
  {
    return m_value;
  }

  Value::Int GetInt() const
  {
    return m_value.GetInt();
  }

  Object* GetObject() const
  {
    return m_value.GetObject();
  }

  Ast* GetNode() const;

  operator Ast* () const
  {
    return GetNode();
  }

  operator String const& () const
  {
    return GetString();
  }

  operator Value::Int () const
  {
    return GetInt();
  }

  Ast* operator -> () const
  {
    return GetNode();
  }

  void Append(Value const& value);

private:

  Value m_value;

};

#endif // CSCRIPT_ASTDATA_H
