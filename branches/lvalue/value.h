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

  Value() : m_pdata(&m_ldata)
  {
    m_pdata->m_type = tNull;
    m_pdata->m_int = 0;
  }

  Value(Value const& rhs) : m_pdata(&m_ldata)
  {
    m_ldata.m_type = tNull;
    m_ldata.m_int  = 0;
    if(rhs.IsReference())
    {
      m_pdata = rhs.m_pdata;
    }
    else
    {
      *this = rhs;
    }
  }

  Value(Value* v) : m_pdata(v->m_pdata)
  {
    m_ldata.m_type = tNull;
    m_ldata.m_int  = 0;
  }

  Value(Bool val) : m_pdata(&m_ldata)
  {
    m_pdata->m_type = tBool;
    m_pdata->m_bool = val;
  }

  Value(Int val) : m_pdata(&m_ldata)
  {
    m_pdata->m_type = tInt;
    m_pdata->m_int = val;
  }
  
  Value(int val) : m_pdata(&m_ldata)
  {
    m_pdata->m_type = tInt;
    m_pdata->m_int = val;
  }

  Value(size_t val) : m_pdata(&m_ldata)
  {
    m_pdata->m_type = tInt;
    m_pdata->m_int  = (Int) val;
  }

  Value(String val) : m_pdata(&m_ldata)
  {
    m_pdata->m_type = tString;
    m_pdata->m_string = new String(val);
  }

  Value(char const* val) : m_pdata(&m_ldata)
  {
    m_pdata->m_type = tString;
    m_pdata->m_string = new String(val);
  }

  Value(Object* obj) : m_pdata(&m_ldata)
  {
    m_pdata->m_type = tObject;
    m_pdata->m_object = obj;
  }

  ~Value()
  {
    Clear();
  }

  void Clear(bool deep = false)
  {
    if(!deep && IsReference())
    {
      m_pdata = &m_ldata;
    }
    else
    {
      switch(m_pdata->m_type)
      {
      case tString: delete m_pdata->m_string; break;
      }
    }
    m_pdata->m_type = tNull;
    m_pdata->m_int  = 0;
  }

  Types Type() const
  {
    return m_pdata->m_type;
  }

  bool Empty() const
  {
    return m_pdata->m_type == tNull;
  }

  bool IsReference() const
  {
    return m_pdata != &m_ldata;
  }

  void* GetIdentity() const
  {
    return m_pdata;
  }

  void Dereference()
  {
    if(IsReference())
    {
      Value v(this);
      Clear(false);
      SetValue(v);
    }
  }

  Bool GetBool() const
  {
    AssertType(tBool);
    return m_pdata->m_bool;
  }

  Int GetInt() const
  {
    AssertType(tInt);
    return m_pdata->m_int;
  }

  String const& GetString() const
  {
    AssertType(tString);
    return *m_pdata->m_string;
  }

  Object& GetObject() const
  {
    AssertType(tObject);
    return *m_pdata->m_object;
  }

  //
  // Set value
  //
  void SetValue(Value const& rhs)
  {
    if(m_pdata != rhs.m_pdata)
    {
      Clear(true);
      switch(rhs.m_pdata->m_type)
      {
      case tBool:   m_pdata->m_bool   = rhs.m_pdata->m_bool; break;
      case tInt:    m_pdata->m_int    = rhs.m_pdata->m_int; break;
      case tString: m_pdata->m_string = new String(*rhs.m_pdata->m_string); break;
      case tObject: m_pdata->m_object = rhs.m_pdata->m_object; break;
      }
      m_pdata->m_type = rhs.m_pdata->m_type;
    }
  }

  //
  // Assignment
  //
  Value const& operator = (Value const& rhs)
  {
    SetValue(rhs);
    return *this;
  }

  //
  // Set reference
  //
  void SetRef(Value const& rhs)
  {
    m_pdata = rhs.m_pdata;
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
    if(m_pdata->m_type != type)
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
  Data* m_pdata;

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
