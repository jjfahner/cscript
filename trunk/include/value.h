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

#include <cscript.h>
#include <gcstring.h>

class Object;
class DataType;

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
    tObject,
  };

  typedef bool        Bool;
  typedef int64       Int;
  typedef std::string String;

  static String TypeToString(Types);
  static Types StringToType(String);

  Value() :
  m_type (tNull),
  m_int  (0)
  {
  }

  Value(Value const& rhs) :
  m_type (rhs.m_type),
  m_int  (rhs.m_int)
  {
  }

  Value(Bool val) :
  m_type (tBool),
  m_bool (val)
  {
  }

  Value(Int val) :
  m_type (tInt),
  m_int  (val)
  {
  }
  
  Value(int val) :
  m_type (tInt),
  m_int  (val)
  {
  }

  Value(size_t val) :
  m_type (tInt),
  m_int  ((Int)val)
  {
  }

  Value(String const& val) :
  m_type    (tString),
  m_string  (new GCString(val))
  {
  }

  Value(char const* val) :
  m_type    (tString),
  m_string  (new GCString(val))
  {
  }

  Value(GCString const& str) :
  m_type    (tString),
  m_string  (&str)
  {
  }

  Value(GCString const* str) :
  m_type    (tString),
  m_string  (str)
  {
  }

  Value(Object* obj) :
  m_type    (obj ? tObject : tNull),
  m_object  (obj)
  {
  }

  void Clear()
  {
    m_type = tNull;
    m_int  = 0;
  }

  Types Type() const
  {
    return m_type;
  }

  DataType* GetDataType() const;

  bool Empty() const
  {
    return m_type == tNull;
  }

  Bool GetBool() const
  {
    if(m_type == tBool) return m_bool;
    throw std::runtime_error("Value is not of type bool");
  }

  Int GetInt() const
  {
    if(m_type == tInt) return m_int;
    throw std::runtime_error("Value is not of type int");
  }

  GCString const& GetString() const
  {
    if(m_type == tString) return *m_string;
    throw std::runtime_error("Value is not of type string");
  }

  Object* GetObject() const
  {
    if(m_type == tObject) return m_object;
    throw std::runtime_error("Value is not of type object");
  }

  operator Int () const
  {
    return GetInt();
  }

  operator String const& () const
  {
    return GetString();
  }

  operator Object * () const
  {
    return GetObject();
  }

  Object* operator -> () const
  {
    return GetObject();
  }

  GC::Object* GetGCObject() const;

  //
  // Set value
  //
  void SetValue(Value const& rhs)
  {
    m_type = rhs.m_type;
    m_int  = rhs.m_int;
  }

  //
  // Assignment
  //
  Value const& operator = (Value const& rhs)
  {
    m_type = rhs.m_type;
    m_int  = rhs.m_int;
    return *this;
  }

private:

  //
  // Not allowed
  //
  bool operator == (Value const& rhs);

  //
  // Allow member access for performance
  //
  friend int ValCmp(Value const& lhs, Value const& rhs);

  //
  // Meant to prevent conversion from pointer to boolean 
  // when a Value is constructed from an invalid pointer type
  //
  Value(void*);

  // Member data
  Types     m_type;
  union 
  {
    GCString const* m_string;
    Int             m_int;
    Bool            m_bool;
    Object*         m_object;
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
String ValString(Value const& val);
Object* ValObject(Value const& val);

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
