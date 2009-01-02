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

#include "value.h"

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

//////////////////////////////////////////////////////////////////////////

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

//////////////////////////////////////////////////////////////////////////

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

#endif // CSCRIPT_VARIABLE_H
