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
    m_type = tNull;
    m_int = 0;
  }

  Value(Value const& rhs)
  {
    m_type = tNull;
    m_int  = 0;
    *this = rhs;
  }

  Value(Bool val)
  {
    m_type = tBool;
    m_bool = val;
  }

  Value(Int val)
  {
    m_type = tInt;
    m_int = val;
  }
  
  Value(int val)
  {
    m_type = tInt;
    m_int = val;
  }

  Value(size_t val)
  {
    m_type = tInt;
    m_int  = (Int) val;
  }

  Value(String val)
  {
    m_type = tString;
    m_string = new String(val);
  }

  Value(char const* val)
  {
    m_type = tString;
    m_string = new String(val);
  }

  Value(Object* obj)
  {
    m_type = tObject;
    m_object = obj;
  }

  ~Value()
  {
    Clear();
  }

  void Clear()
  {
    if(m_type == tString)
    {
      delete m_string;
    }
    m_type = tNull;
    m_int  = 0;
  }

  Types Type() const
  {
    return m_type;
  }

  bool Empty() const
  {
    return m_type == tNull;
  }

  Bool GetBool() const
  {
    AssertType(tBool);
    return m_bool;
  }

  Int GetInt() const
  {
    AssertType(tInt);
    return m_int;
  }

  operator Int () const
  {
    return GetInt();
  }

  String const& GetString() const
  {
    AssertType(tString);
    return *m_string;
  }

  operator String const& () const
  {
    return GetString();
  }

  Object* GetObject() const
  {
    AssertType(tObject);
    return m_object;
  }

  Object* operator -> () const
  {
    return GetObject();
  }

  operator Object * () const
  {
    return GetObject();
  }

  //
  // Set value
  //
  void SetValue(Value const& rhs)
  {
    Clear();
    switch(rhs.m_type)
    {
    case tBool:   m_bool   = rhs.m_bool; break;
    case tInt:    m_int    = rhs.m_int; break;
    case tString: m_string = new String(*rhs.m_string); break;
    case tObject: m_object = rhs.m_object; break;
    }
    m_type = rhs.m_type;
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
  // Meant to prevent conversion from pointer to boolean 
  // when a Value is constructed from an invalid pointer type
  //
  Value(void*);

  //
  // Type check
  //
  void AssertType(Types type) const
  {
    if(m_type != type)
    {
      throw std::runtime_error("Value is not of expected type");
    }
  }

  // Member data
  Types     m_type;
  union 
  {
    String* m_string;
    Int     m_int;
    Bool    m_bool;
    Object* m_object;
  };

};

//////////////////////////////////////////////////////////////////////////

template <typename T>
inline bool ValueHasType(Value const& v)
{
  if(v.Type() != Value::tObject)
  {
    return false;
  }
  return dynamic_cast<T*>(&v.GetObject()) != 0;
}

template <typename T>
inline T* ValueToType(Value const& v)
{
  if(v.Type() != Value::tObject)
  {
    return 0;
  }
  return dynamic_cast<T*>(v.GetObject());
}

//////////////////////////////////////////////////////////////////////////

Value::Bool ValBool(Value const& val);
Value::Int ValInt(Value const& val);
Value::String ValString(Value const& val);

Value ValNeg(Value const& lhs);
Value ValNot(Value const& lhs);

Value ValAdd(Value const& lhs, Value const& rhs);
Value ValSub(Value const& lhs, Value const& rhs);
Value ValMul(Value const& lhs, Value const& rhs);
Value ValDiv(Value const& lhs, Value const& rhs);
Value ValMod(Value const& lhs, Value const& rhs);

int ValCmp(Value const& lhs, Value const& rhs);

//////////////////////////////////////////////////////////////////////////

inline bool 
operator < (Value const& lhs, Value const& rhs)
{
  return ValCmp(lhs, rhs) < 0;
}

//////////////////////////////////////////////////////////////////////////

#endif // CSCRIPT_VALUE_H
