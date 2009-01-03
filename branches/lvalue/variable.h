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
  // Automatic conversion to Value
  //
  operator Value const& () const
  {
    return GetValue();
  }

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
  // Reset enumerator to first entry
  //
  virtual void Reset() = 0;

  //
  // Retrieve next value
  //
  virtual bool GetNext(Value& value) = 0;

  //
  // Enumerator at end
  //
  virtual bool AtEnd() const = 0;

};

//////////////////////////////////////////////////////////////////////////

class ObjectEnumerator : public Enumerator
{
  typedef Variables::const_iterator Iterator;

  Object&  m_obj;
  Iterator m_cur;

public:

  ObjectEnumerator(Object& object) :
  m_obj (object)
  {
    // Initialize iterator
    Reset();
  }

  virtual void Reset()
  {
    // Set iterator to start
    m_cur = m_obj.GetVariables().begin();
  }

  virtual bool GetNext(Value& value)
  {
    // Check current position
    if(AtEnd())
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

  virtual bool AtEnd() const
  {
    // Compare iterator to end
    return m_cur == m_obj.GetVariables().end();
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
// Variables
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

#endif // CSCRIPT_VARIABLE_H
