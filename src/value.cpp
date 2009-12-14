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
#include "value.h"
#include "object.h"
#include "datatype.h"
#include "gc.h"

#include <typeinfo>
#include <cstdio>

//////////////////////////////////////////////////////////////////////////

GCObject* 
Value::GetGCObject() const
{
  switch(m_type)
  {
  case tString: return const_cast<GCString*>(m_string);
  case tObject: return m_object;
  default:      return 0;
  }
}

DataType* 
Value::GetDataType() const
{
  switch(m_type)
  {
  case Value::tNull:    return NullType::Instance();
  case Value::tBool:    return BooleanType::Instance();
  case Value::tInt:     return IntegerType::Instance();
  case Value::tString:  return StringType::Instance();
  case Value::tObject:  return GetObject()->GetType();
  }
  throw std::runtime_error("Invalid type");
}

/*static*/ Value::String 
Value::TypeToString(Types type)
{
  switch(type)
  {
  case Value::tNull:    return "null";
  case Value::tBool:    return "bool";
  case Value::tInt:     return "int";
  case Value::tString:  return "string";
  case Value::tObject:  return "object";
  }
  throw std::runtime_error("Invalid type");
}

/*static*/ Value::Types 
Value::StringToType(String type)
{
  if(!strcmp_nocase(type.c_str(), "bool"))    return Value::tBool;
  if(!strcmp_nocase(type.c_str(), "int"))     return Value::tInt;
  if(!strcmp_nocase(type.c_str(), "string"))  return Value::tString;
  if(!strcmp_nocase(type.c_str(), "object"))  return Value::tObject;
  throw std::runtime_error("Invalid type");
}

int 
ValCmp(Value const& lhs, Value const& rhs)
{
  // Compare different types by comparing type ids
  if(lhs.Type() != rhs.Type())
  {
    return lhs.Type() - rhs.Type();
  }

  // Type-based compare
  switch(lhs.Type())
  {
  case Value::tNull:   
    return 0;

  case Value::tBool:   
    return int(lhs.GetBool()) - int(rhs.GetBool());

  case Value::tInt:    
    return int(lhs.GetInt() - rhs.GetInt());

  case Value::tString: 
    return lhs.m_string == rhs.m_string ? 0 :
            strcmp(lhs.GetString().c_str(), 
                   rhs.GetString().c_str());

  case Value::tObject:
    return int(lhs.GetObject() - rhs.GetObject());
  }

  // Invalid type
  throw std::runtime_error("Unsupported value type for comparison");
}


Value::Bool   
ValBool(Value const& val)
{
  switch(val.Type())
  {
  case Value::tNull:    return false;
  case Value::tBool:    return val.GetBool();
  case Value::tInt:     return val.GetInt() != 0;
  case Value::tString:  return val.GetString().length() != 0;
  case Value::tObject:  break;
  }
  throw std::runtime_error("Cannot convert between types");
}

Value::Int    
ValInt(Value const& val)
{
  switch(val.Type())
  {
  case Value::tNull:    return 0;
  case Value::tBool:    return val.GetBool() ? 1 : 0;
  case Value::tInt:     return val.GetInt();
  case Value::tString:  return atoi(val.GetString().c_str());
  case Value::tObject:  break; // TODO
  }
  throw std::runtime_error("Cannot convert between types");
}

inline 
Value::String IntToString(Value::Int val)
{
  char buf[25];
  sprintf(buf, "%d", val);
  return buf;
}

Value::String 
ValString(Value const& val)
{
  switch(val.Type())
  {
  case Value::tNull:    return Value::String();
  case Value::tBool:    return val.GetBool() ? "true" : "false";
  case Value::tInt:     return IntToString(val.GetInt());
  case Value::tString:  return val.GetString();
  case Value::tObject:  return typeid(val.GetObject()).name();
  }
  throw std::runtime_error("Cannot convert between types");
}

Object* 
ValObject(Value const& val)
{
  if(val.Type() == Value::tObject)
  {
    return val.GetObject();
  }
  return static_cast<ScalarType*>(val.GetDataType())->Box(val);
}

Value 
ValAdd(Value const& lhs, Value const& rhs)
{
  switch(lhs.Type())
  {
  case Value::tInt:     return lhs.GetInt() + ValInt(rhs);
  case Value::tString:  return lhs.GetString() + ValString(rhs);
  case Value::tObject:  break; // TODO
  }
  throw std::runtime_error("Invalid type(s) for addition operator");
}

Value 
ValSub(Value const& lhs, Value const& rhs)
{
  switch(lhs.Type())
  {
  case Value::tInt:     return lhs.GetInt() - ValInt(rhs);
  case Value::tObject:  break; // TODO
  }
  throw std::runtime_error("Invalid type(s) for subtraction operator");
}

Value 
ValMul(Value const& lhs, Value const& rhs)
{
  switch(lhs.Type())
  {
  case Value::tInt:     return lhs.GetInt() * ValInt(rhs);
  case Value::tObject:  break; // TODO
  }
  throw std::runtime_error("Invalid type(s) for multiplication operator");
}

Value 
ValDiv(Value const& lhs, Value const& rhs)
{
  switch(lhs.Type())
  {
  case Value::tInt:     return lhs.GetInt() / ValInt(rhs);
  case Value::tObject:  break; // TODO
  }
  throw std::runtime_error("Invalid type(s) for division operator");
}

Value 
ValMod(Value const& lhs, Value const& rhs)
{
  switch(lhs.Type())
  {
  case Value::tInt:     return lhs.GetInt() % ValInt(rhs);
  case Value::tObject:  break; // TODO
  }
  throw std::runtime_error("Invalid type(s) for modulo operator");
}

Value 
ValShl(Value const& lhs, Value const& rhs)
{
  switch(lhs.Type())
  {
  case Value::tInt:     return (int64)((uint64)lhs.GetInt() << (uint64)ValInt(rhs));
  case Value::tObject:  break; // TODO
  }
  throw std::runtime_error("Invalid type(s) for left shift operator");
}

Value 
ValShr(Value const& lhs, Value const& rhs)
{
  switch(lhs.Type())
  {
  case Value::tInt:     return (int64)((uint64)lhs.GetInt() >> (uint64)ValInt(rhs));
  case Value::tObject:  break; // TODO
  }
  throw std::runtime_error("Invalid type(s) for right shift operator");
}

Value
ValNeg(Value const& lhs)
{
  switch(lhs.Type())
  {
  case Value::tInt:     return -lhs.GetInt();
  case Value::tObject:  break; // TODO
  }
  throw std::runtime_error("Invalid type(s) for negation operator");
}

Value 
ValNot(Value const& lhs)
{
  switch(lhs.Type())
  {
  case Value::tBool:    return lhs.GetBool() == false;
  case Value::tInt:     return lhs.GetInt()  == 0;
  case Value::tObject:  break; // TODO
  }
  throw std::runtime_error("Invalid type(s) for negation operator");
}
