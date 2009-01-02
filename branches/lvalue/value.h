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
#ifndef CSCRIPT_VALUE_H
#define CSCRIPT_VALUE_H

#include "types.h"

class Object;

class Value
{
public:

  enum Types
  {
    tNull,
    tBool,
    tInt,
    tReal,
    tString,
    tObject
  };

  typedef bool          Bool;
  typedef int64         Int;
  typedef std::string   String;

  Value()
  {
    m_ldata.m_type = tNull;
    m_ldata.m_int = 0;
  }

  Value(Value const& rhs)
  {
    m_ldata.m_type = tNull;
    m_ldata.m_int  = 0;
    *this = rhs;
  }

  Value(Bool val)
  {
    m_ldata.m_type = tBool;
    m_ldata.m_bool = val;
  }

  Value(Int val)
  {
    m_ldata.m_type = tInt;
    m_ldata.m_int = val;
  }
  
  Value(int val)
  {
    m_ldata.m_type = tInt;
    m_ldata.m_int = val;
  }

  Value(size_t val)
  {
    m_ldata.m_type = tInt;
    m_ldata.m_int  = (Int) val;
  }

  Value(String val)
  {
    m_ldata.m_type = tString;
    m_ldata.m_string = new String(val);
  }

  Value(char const* val)
  {
    m_ldata.m_type = tString;
    m_ldata.m_string = new String(val);
  }

  Value(Object* obj)
  {
    m_ldata.m_type = tObject;
    m_ldata.m_object = obj;
  }

  ~Value()
  {
    Clear();
  }

  void Clear()
  {
    if(m_ldata.m_type == tString)
    {
      delete m_ldata.m_string;
    }
    m_ldata.m_type = tNull;
    m_ldata.m_int  = 0;
  }

  Types Type() const
  {
    return m_ldata.m_type;
  }

  bool Empty() const
  {
    return m_ldata.m_type == tNull;
  }

  Bool GetBool() const
  {
    AssertType(tBool);
    return m_ldata.m_bool;
  }

  Int GetInt() const
  {
    AssertType(tInt);
    return m_ldata.m_int;
  }

  String const& GetString() const
  {
    AssertType(tString);
    return *m_ldata.m_string;
  }

  Object& GetObject() const
  {
    AssertType(tObject);
    return *m_ldata.m_object;
  }

  //
  // Set value
  //
  void SetValue(Value const& rhs)
  {
    Clear();
    switch(rhs.m_ldata.m_type)
    {
    case tBool:   m_ldata.m_bool   = rhs.m_ldata.m_bool; break;
    case tInt:    m_ldata.m_int    = rhs.m_ldata.m_int; break;
    case tString: m_ldata.m_string = new String(*rhs.m_ldata.m_string); break;
    case tObject: m_ldata.m_object = rhs.m_ldata.m_object; break;
    }
    m_ldata.m_type = rhs.m_ldata.m_type;
  }

  //
  // Assignment
  //
  Value const& operator = (Value const& rhs)
  {
    SetValue(rhs);
    return *this;
  }

private:

  //
  // Not allowed
  //
  bool operator == (Value const& rhs);

  //
  // Type check
  //
  void AssertType(Types type) const
  {
    if(m_ldata.m_type != type)
    {
      throw std::runtime_error("Value is not of expected type");
    }
  }

  // Member data
  struct Data
  {
    Types     m_type;
    union 
    {
      Bool    m_bool;
      Int     m_int;
      String* m_string;
      Object* m_object;
      Value*  m_value;
    };
  };

  //
  // Members
  //
  Data  m_ldata;

};

//////////////////////////////////////////////////////////////////////////

class RValue
{
public:

  operator Value const& () const
  {
    return GetValue();
  }

  virtual Value const& GetValue() const = 0;

  class LValue& LVal();

protected:

  RValue() {}
  RValue(RValue const&);
  RValue const& operator = (RValue const&);

};

class LValue : public RValue
{
public:

  RValue& operator = (Value const& rhs)
  {
    SetValue(rhs);
    return *this;
  }

  virtual void SetValue(Value const& rhs) = 0;

};

inline LValue& 
RValue::LVal()
{
  LValue* lval = dynamic_cast<LValue*>(this);
  if(lval == 0)
  {
    throw std::runtime_error("Left-hand value cannot be assigned to");
  }
  return *lval;
}

class Variable : public LValue
{
  Value m_value;

public:

  Variable(Value const& value) :
  m_value (value)
  {
  }

  virtual Value const& GetValue() const
  {
    return m_value;
  }

  virtual void SetValue(Value const& rhs)
  {
    m_value = rhs;
  }

};

#endif // CSCRIPT_VALUE_H
