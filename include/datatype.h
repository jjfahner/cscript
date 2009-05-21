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
#ifndef CSCRIPT_TYPE_H
#define CSCRIPT_TYPE_H

#include <cscript.h>
#include <native.h>
#include <eval.h>

class Evaluator;
class Arguments;

class DataType : public Object
{
public:

  //
  // Type name as string
  //
  virtual String Name() const = 0;

  //
  // Box value
  //
  virtual DataType* Box(Value const& value) = 0;

  //
  // Add method wrappers
  //
  virtual void UpdateMembers() 
  {
    NATIVE_METHOD(DataType, NM_Name, "Name()");
  }

  Value NM_Name(Evaluator*, Arguments const&)
  {
    return Name();
  }

};

//////////////////////////////////////////////////////////////////////////

class VoidType : public DataType
{
public:

  static DataType* Instance()
  {
    static VoidType m_type;
    GC::Pin(&m_type);
    return &m_type;
  }

  virtual String Name() const
  {
    return "void";
  }

  virtual DataType* Box(Value const&)
  {
    throw std::runtime_error("Attempt to box a VoidType");
  }

};

//////////////////////////////////////////////////////////////////////////

class UnknownType : public DataType
{
public:

  static DataType* Instance()
  {
    static UnknownType m_type;
    GC::Pin(&m_type);
    return &m_type;
  }

  virtual String Name() const
  {
    return "unknown";
  }

  virtual DataType* Box(Value const&)
  {
    throw std::runtime_error("Attempt to box an UnknownType");
  }

};

//////////////////////////////////////////////////////////////////////////

class ScalarType : public DataType
{
};

class BooleanType : public ScalarType
{
public:

  typedef bool ValueType;

  static DataType* Instance()
  {
    static BooleanType m_type;
    GC::Pin(&m_type);
    return &m_type;
  }

  BooleanType(ValueType value = ValueType()) :
  m_value (value)
  {
  }

  virtual String Name() const
  {
    return "bool";
  }

  virtual String ToString() const
  {
    return m_value ? "true" : "false";
  }

  virtual BooleanType* Box(Value const& value)
  {
    return new BooleanType(value.GetBool());
  }

  virtual void UpdateMembers()
  {
    ScalarType::UpdateMembers();
    NATIVE_METHOD(BooleanType, NM_ToString, "ToString()");
  }

  Value NM_ToString(Evaluator*, Arguments const&)
  {
    return ToString();
  }

protected:

  ValueType m_value;

};

class IntegerType : public ScalarType
{
public:

  typedef __int64 ValueType;

  static DataType* Instance()
  {
    static IntegerType m_type;
    GC::Pin(&m_type);
    return &m_type;
  }

  IntegerType(ValueType value = ValueType()) :
  m_value (value)
  {
  }

  virtual String Name() const
  {
    return "int";
  }

  virtual String ToString() const
  {
    char buf[25];
    sprintf(buf, "%d", m_value);
    return buf;
  }

  virtual IntegerType* Box(Value const& value)
  {
    return new IntegerType(value.GetInt());
  }

  virtual void UpdateMembers()
  {
    ScalarType::UpdateMembers();
    NATIVE_METHOD(IntegerType, NM_ToString, "ToString()");
  }

  Value NM_ToString(Evaluator*, Arguments const&)
  {
    return ToString();
  }

protected:

  ValueType m_value;

};

class StringType : public ScalarType
{
public:

  typedef std::string ValueType;

  static DataType* Instance()
  {
    static StringType m_type;
    GC::Pin(&m_type);
    return &m_type;
  }

  StringType(ValueType const& value = ValueType()) :
  m_value (value)
  {
  }

  virtual String Name() const
  {
    return "string";
  }

  virtual StringType* Box(Value const& value)
  {
    return new StringType(value.GetString());
  }

protected:

  ValueType m_value;

};

//////////////////////////////////////////////////////////////////////////

class ComplexType : public DataType
{
public:

  virtual DataType* Box(Value const&)
  {
    throw std::runtime_error("Attempt to box a complex type");
  }

};

class ObjectType : public ComplexType
{
public:

  static DataType* Instance()
  {
    static ObjectType m_type;
    GC::Pin(&m_type);
    return &m_type;
  }

  virtual String Name() const
  {
    return "object";
  }

};

class NullType : public ObjectType
{
public:

  static DataType* Instance()
  {
    static NullType m_type;
    GC::Pin(&m_type);
    return &m_type;
  }

  virtual String Name() const
  {
    return "null";
  }

  virtual DataType* Box(Value const&)
  {
    return new NullType();
  }

};

class FunctionType : public ObjectType
{
public:

  static DataType* Instance()
  {
    static FunctionType m_type;
    GC::Pin(&m_type);
    return &m_type;
  }

  virtual String Name() const
  {
    return "function";
  }

};

class NativeFunctionType : public FunctionType
{
public:

  static DataType* Instance()
  {
    static NativeFunctionType m_type;
    GC::Pin(&m_type);
    return &m_type;
  }

  virtual String Name() const
  {
    return "native_function";
  }

};

#endif // CSCRIPT_TYPE_H
