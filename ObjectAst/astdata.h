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
#include "object.h"
#include "variable.h"

class Ast;

class AstData
{
public:

  AstData(Object* obj, Value const& key) : 
  m_obj   (obj),
  m_key   (key)
  {
  }

  bool Empty() const
  {
    return !m_obj->Contains(m_key);
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
    return Empty() ? Value::tNull : (*m_obj)[m_key].Type();
  }

  void AssertType(Value::Types type) const
  {
    if(Type() != type)
    {
      throw std::runtime_error("Invalid type");
    }
  }

  Value::Int GetNumber() const
  {
    AssertType(Value::tInt);
    return (*m_obj)[m_key].GetInt();
  }

  String const& GetString() const
  {
    AssertType(Value::tString);
    return (*m_obj)[m_key].GetString();
  }

  Object* GetNode() const
  {
    AssertType(Value::tObject);
    return (*m_obj)[m_key].GetObject();
  }

  Value const& GetValue() const
  {
    return (*m_obj)[m_key];
  }

  operator Object* () const
  {
    return GetNode();
  }

  operator Value::String const& () const
  {
    return GetString();
  }

  operator Value const& () const
  {
    return GetValue();
  }

  operator Value::Int () const
  {
    return GetNumber();
  }

  Ast operator -> () const;

private:

  friend class Ast;
  Object*   m_obj;
  Value     m_key;

};

#endif // CSCRIPT_ASTDATA_H
