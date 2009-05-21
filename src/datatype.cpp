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

//////////////////////////////////////////////////////////////////////////

bool 
DataType::ContainsKey(Value const& key, bool checkProto) const
{
  return false;
}

bool 
DataType::Find(Value const& key, RValue*& pValue, bool checkProto) const
{
  return false;
}

//////////////////////////////////////////////////////////////////////////

/*static*/ DataType* 
VoidType::Instance()
{
  static VoidType m_type;
  GC::Pin(&m_type);
  return &m_type;
}

String 
VoidType::TypeName() const
{
  return "void";
}

//////////////////////////////////////////////////////////////////////////

/*static*/ DataType* 
UnknownType::Instance()
{
  static UnknownType m_type;
  GC::Pin(&m_type);
  return &m_type;
}

String 
UnknownType::TypeName() const
{
  return "unknown";
}

//////////////////////////////////////////////////////////////////////////

/*static*/ DataType* 
NullType::Instance()
{
  static NullType m_type;
  GC::Pin(&m_type);
  return &m_type;
}

String 
NullType::TypeName() const
{
  return "null";
}

DataType* 
NullType::Box(Value const&)
{
  return new NullType();
}

//////////////////////////////////////////////////////////////////////////

/*static*/ DataType* 
BooleanType::Instance()
{
  static BooleanType m_type;
  GC::Pin(&m_type);
  return &m_type;
}

BooleanType::BooleanType(DeclType value) :
m_value (value)
{
}

String 
BooleanType::TypeName() const
{
  return "bool";
}

String 
BooleanType::ToString() const
{
  return m_value ? "true" : "false";
}

BooleanType* 
BooleanType::Box(Value const& value)
{
  return new BooleanType(value.GetBool());
}

//////////////////////////////////////////////////////////////////////////

/*static*/ DataType* 
IntegerType::Instance()
{
  static IntegerType m_type;
  GC::Pin(&m_type);
  return &m_type;
}

IntegerType::IntegerType(DeclType value) :
m_value (value)
{
}

String 
IntegerType::TypeName() const
{
  return "int";
}

String 
IntegerType::ToString() const
{
  char buf[25];
  sprintf(buf, "%d", m_value);
  return buf;
}

IntegerType* 
IntegerType::Box(Value const& value)
{
  return new IntegerType(value.GetInt());
}

//////////////////////////////////////////////////////////////////////////

/*static*/ DataType* 
StringType::Instance()
{
  static StringType m_type;
  GC::Pin(&m_type);
  return &m_type;
}

StringType::StringType(DeclType const& value) :
m_value (value)
{
}

String 
StringType::TypeName() const
{
  return "string";
}

StringType* 
StringType::Box(Value const& value)
{
  return new StringType(value.GetString());
}

//////////////////////////////////////////////////////////////////////////

/*static*/ DataType* 
ObjectType::Instance()
{
  static ObjectType m_type;
  GC::Pin(&m_type);
  return &m_type;
}

//
// Type name as string
//
String 
ObjectType::TypeName() const
{
  return "object";
}

//////////////////////////////////////////////////////////////////////////

/*static*/ DataType* 
FunctionType::Instance()
{
  static FunctionType m_type;
  GC::Pin(&m_type);
  return &m_type;
}

String 
FunctionType::TypeName() const
{
  return "function";
}

//////////////////////////////////////////////////////////////////////////

/*static*/ DataType* 
NativeFunctionType::Instance()
{
  static NativeFunctionType m_type;
  GC::Pin(&m_type);
  return &m_type;
}

String 
NativeFunctionType::TypeName() const
{
  return "native_function";
}

//////////////////////////////////////////////////////////////////////////
