//////////////////////////////////////////////////////////////////////////
//
// This file is © 2008 JJ Fahner <jan-jaap@jan-jaap.net>
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
#ifndef CSCRIPT_VARIABLE_H
#define CSCRIPT_VARIABLE_H

#include "object.h"

//////////////////////////////////////////////////////////////////////////

class Enumerator;

class RValue
{
public:

  //
  // Setup virtual destruction
  //
  virtual ~RValue() {}

  //
  // Automatic conversion to Value
  //
  operator Value const& () const
  {
    return GetValue();
  }

  //
  // Wrappers for Value interface
  //
  Value::Types          Type()      const { return GetValue().Type();      }
  bool                  Empty()     const { return GetValue().Empty();     }
  Value::Bool           GetBool()   const { return GetValue().GetBool();   }
  Value::Int            GetInt()    const { return GetValue().GetInt();    }
  Value::String const&  GetString() const { return GetValue().GetString(); }
  Object*               GetObject() const { return GetValue().GetObject(); }

  //
  // Actual value retrieval implemented in derived class
  //
  virtual Value const& GetValue() const = 0;

  //
  // Convert RValue to LValue. Throws if not an LValue
  //
  class LValue& LVal();

  //
  // Retrieve an enumerator for this variable
  //
  virtual Enumerator* GetEnumerator() const;

protected:

  //
  // Construction and assignment reserved for derived classes
  //
  RValue() {}
  RValue(RValue const&) {}
  RValue const& operator = (RValue const&) { return *this; }

};

//////////////////////////////////////////////////////////////////////////

class LValue : public RValue
{
public:

  //
  // Assignment operator for Value
  //
  RValue& operator = (Value const& rhs)
  {
    SetValue(rhs);
    return *this;
  }

  //
  // Implementation of assignment in derived class
  //
  virtual void SetValue(Value const& rhs) = 0;

};

//////////////////////////////////////////////////////////////////////////

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

//////////////////////////////////////////////////////////////////////////

class Enumerator 
{
public:

  //
  // Setup virtual destruction
  //
  virtual ~Enumerator() {}

  //
  // Reset enumerator to first entry
  //
  virtual void Reset() = 0;

  //
  // Retrieve next value
  //
  virtual bool GetNext(Value& value) = 0;

};

//////////////////////////////////////////////////////////////////////////

class ObjectEnumerator : public Enumerator
{
  typedef Members::const_iterator Iterator;

  Object*  m_obj;
  Iterator m_cur;

public:

  ObjectEnumerator(Object* object) :
  m_obj (object)
  {
    // Initialize iterator
    Reset();
  }

  virtual void Reset()
  {
    // Set iterator to start
    m_cur = m_obj->GetMembers().begin();
  }

  virtual bool GetNext(Value& value)
  {
    // Check current position
    if(m_cur == m_obj->GetMembers().end())
    {
      return false;
    }

    // Retrieve value from iterator
    value = m_cur->second->GetValue();

    // Advance to next position
    ++m_cur;

    // Succeeded
    return true;
  }

};

inline Enumerator* 
RValue::GetEnumerator() const
{
  Value const& v = GetValue();
  if(v.Type() == Value::tObject)
  {
    return new ObjectEnumerator(v.GetObject());
  }
  return 0;
}

//////////////////////////////////////////////////////////////////////////
//
// Members
//

class Variable
{
};

class ROVariable : public Variable, public RValue
{
  Value m_value;

public:

  ROVariable(Value const& value) :
  m_value (value)
  {
  }

  virtual Value const& GetValue() const
  {
    return m_value;
  }

};

class RWVariable : public Variable, public LValue
{
  Value m_value;

public:

  RWVariable(Value const& value) :
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

//////////////////////////////////////////////////////////////////////////

class Temporary : public RValue 
{
  Value m_value;

public:

  Temporary()
  {
  }

  Temporary(Temporary const& rhs) :
  m_value (rhs.m_value)
  {
  }

  Temporary(Value const& value) :
  m_value (value)
  {
  }

  Value const& GetValue() const
  {
    return m_value;
  }

};

//////////////////////////////////////////////////////////////////////////

class Constant : public RValue
{
  Value m_value;

public:

  Constant(Value const& value) :
  m_value (value)
  {
  }

  Value const& GetValue() const
  {
    return m_value;
  }

};

//////////////////////////////////////////////////////////////////////////

class BoundValue
{
  Value   m_object;

public:

  //
  // Factory
  //
  static RValue* Create(RValue& value, Object* object);

  //
  // Construction
  //
  BoundValue(Value const& object) :
  m_object  (object)
  {
  }

  //
  // Get object
  //
  Object* GetBoundObject() const
  {
    return m_object.GetObject();
  }

};

class BoundRValue : public RValue, public BoundValue
{
  RValue& m_value;

public:

  //
  // Construction
  //
  BoundRValue(RValue& value, Value const& object) :
  BoundValue (object),
  m_value   (value)
  {
  }

  //
  // Value retrieval
  //
  Value const& GetValue() const
  {
    return m_value.GetValue();
  }

  //
  // Retrieve an enumerator for this variable
  //
  Enumerator* GetEnumerator() const
  {
    return m_value.GetEnumerator();
  }

};

//////////////////////////////////////////////////////////////////////////

class BoundLValue : public LValue, public BoundValue
{
  LValue& m_value;

public:

  //
  // Construction
  //
  BoundLValue(LValue& value, Value const& object) :
  BoundValue (object),
  m_value   (value)
  {
  }

  //
  // Value retrieval
  //
  Value const& GetValue() const
  {
    return m_value.GetValue();
  }

  //
  // Value 
  //
  void SetValue(Value const& value)
  {
    m_value = value;
  }

  //
  // Retrieve an enumerator for this variable
  //
  Enumerator* GetEnumerator() const
  {
    return m_value.GetEnumerator();
  }

};

//////////////////////////////////////////////////////////////////////////

/*static*/ inline RValue* 
BoundValue::Create(RValue& value, Object* object)
{
  if(LValue* lval = dynamic_cast<LValue*>(&value))
  {
    return new BoundLValue(*lval, object);
  }
  return new BoundRValue(value, object);
}

#endif // CSCRIPT_VARIABLE_H
