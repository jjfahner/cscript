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
#include "datatype.h"
#include "variable.h"
#include "native.h"
#include "eval.h"

// warning C4355: 'this' : used in base member initializer list
#pragma warning(disable:4355)

//////////////////////////////////////////////////////////////////////////

DataType::DataType() :
Object(this)
{
}

//////////////////////////////////////////////////////////////////////////

class VoidTypeImpl : public VoidType
{
public:

  String TypeName()
  {
    return "void";
  }

};

DataType* VoidType::Instance()
{
  static VoidTypeImpl m_type;
  GC::Pin(&m_type);
  return &m_type;
}

//////////////////////////////////////////////////////////////////////////

class UnknownTypeImpl : public UnknownType
{
public:

  String TypeName()
  {
    return "unknown";
  }

};

DataType* UnknownType::Instance()
{
  static UnknownTypeImpl m_type;
  GC::Pin(&m_type);
  return &m_type;
}

//////////////////////////////////////////////////////////////////////////

class NullTypeImpl : public NullType
{
public:

  String TypeName()
  {
    return "null";
  }

  String ToString()
  {
    return "null";
  }

  DataType* Box(Value const&)
  {
    return new NullTypeImpl();
  }

};

DataType* NullType::Instance()
{
  static NullTypeImpl m_type;
  GC::Pin(&m_type);
  return &m_type;
}

//////////////////////////////////////////////////////////////////////////

class BooleanTypeImpl : public BooleanType
{
public:

  BooleanTypeImpl(DeclType value = DeclType()) :
  m_value     (value)
  {
  }

  String TypeName()
  {
    return "bool";
  }

  String ToString()
  {
    return m_value ? "true" : "false";
  }

  BooleanType* Box(Value const& value)
  {
    return new BooleanTypeImpl(value.GetBool());
  }

protected:

  DeclType    m_value;

};

DataType* BooleanType::Instance()
{
  static BooleanTypeImpl m_type;
  GC::Pin(&m_type);
  return &m_type;
}

//////////////////////////////////////////////////////////////////////////

class IntegerTypeImpl : public IntegerType
{
public:

  IntegerTypeImpl(DeclType value = DeclType()) :
  m_value (value)
  {
  }

  String TypeName()
  {
    return "int";
  }

  String ToString()
  {
    char buf[25];
    sprintf(buf, "%d", m_value);
    return buf;
  }

  IntegerType* Box(Value const& value)
  {
    return new IntegerTypeImpl(value.GetInt());
  }

  DeclType ParseInt(StringCRef source)
  {
    int v;
    sscanf(source.c_str(), "%d", &v);
    return v;
  }

protected:

  DeclType m_value;

};

DataType* IntegerType::Instance()
{
  static IntegerTypeImpl m_type;
  GC::Pin(&m_type);
  return &m_type;
}

//////////////////////////////////////////////////////////////////////////

class StringTypeImpl : public StringType
{
public:

  StringTypeImpl(DeclType const& value = DeclType()) :
  m_value (value)
  {
  }

  String TypeName()
  {
    return "string";
  }

  String ToString()
  {
    return m_value;
  }

  StringType* Box(Value const& value)
  {
    return new StringTypeImpl(value.GetString());
  }

protected:

  DeclType m_value;

};

DataType* StringType::Instance()
{
  static StringTypeImpl m_type;
  GC::Pin(&m_type);
  return &m_type;
}

//////////////////////////////////////////////////////////////////////////

class ObjectTypeImpl : public ObjectType
{
public:

  String TypeName()
  {
    return "object";
  }

};

DataType* ObjectType::Instance()
{
  static ObjectTypeImpl m_type;
  GC::Pin(&m_type);
  return &m_type;
}

//////////////////////////////////////////////////////////////////////////

class FunctionTypeImpl : public FunctionType
{
public:

  String TypeName()
  {
    return "function";
  }

};

DataType* FunctionType::Instance()
{
  static FunctionTypeImpl m_type;
  GC::Pin(&m_type);
  return &m_type;
}

//////////////////////////////////////////////////////////////////////////

class NativeFunctionTypeImpl : public NativeFunctionType
{
public:

  String TypeName()
  {
    return "native_function";
  }

};

DataType* NativeFunctionType::Instance()
{
  static NativeFunctionTypeImpl m_type;
  GC::Pin(&m_type);
  return &m_type;
}

//////////////////////////////////////////////////////////////////////////
