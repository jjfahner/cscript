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

#include <native.h>
#include <args.h>

//
// Types of stubs
//
enum StubType
{
  stEmpty  = 0,
  stMethod = 1,
  stRoProp = 2,
  stRwProp = 3
};

//
// Stub signatures
//
typedef Value (*MethodStub)(Evaluator*, Object*, Arguments const&);
typedef Value (*RoPropStub)(Object*);
typedef void  (*RwPropStub)(Object*, Value const&);

//
// Native call information
//
struct NativeCall
{
  StubType    m_type;
  char const* m_name;
  MethodStub  m_method;
  RoPropStub  m_roprop;
  RwPropStub  m_rwprop;
  Value       m_var;
};

//////////////////////////////////////////////////////////////////////////
//
// Resolver implementation
//

bool 
NativeCallTryGet(NativeCall* pTable, 
                 Object* instance, 
                 Value const& key, 
                 Value& pValue)
{
  if(key.Type() != Value::tString)
  {
    throw std::runtime_error(
      "Invalid key type for object");
  }

  while(pTable->m_name)
  {
    if(key.GetString() == pTable->m_name)
    {
      if(pTable->m_roprop)
      {
        pValue = pTable->m_roprop(instance);
        return true;
      }
      return false;
    }
    ++pTable;
  }
  return false;
}

bool 
NativeCallTrySet(NativeCall* pTable, 
                 Object* instance, 
                 Value const& key, 
                 Value const& value)
{
  if(key.Type() != Value::tString)
  {
    throw std::runtime_error(
      "Invalid key type for object");
  }

  while(pTable->m_name)
  {
    if(key.GetString() == pTable->m_name)
    {
      if(pTable->m_rwprop)
      {
        pTable->m_rwprop(instance, value);
        return true;
      }
      return false;
    }
    ++pTable;
  }
  return false;
}

bool
NativeCallTryEval(NativeCall* pTable, 
                  Object* instance, 
                  Value const& key, 
                  Evaluator* evaluator, 
                  Arguments& arguments, 
                  Value& result)
{
  if(key.Type() != Value::tString)
  {
    throw std::runtime_error(
      "Invalid key type for object");
  }

  while(pTable->m_name)
  {
    if(key.GetString() == pTable->m_name)
    {
      if(pTable->m_method)
      {
        result = pTable->m_method(evaluator, instance, arguments);
        return true;
      }
      return false;
    }
    ++pTable;
  }
  return false;
}

//////////////////////////////////////////////////////////////////////////
//
// Native call conversion functions
//

inline StringCRef 
cscript_arg_to_StringCRef(Value const& value)
{
  return value.GetString();
}

inline String
cscript_arg_to_String(Value const& value)
{
  if(value.Type() == Value::tString)
  {
    return value.GetString();
  }
  return ValString(value);
}

inline ValueCRef
cscript_arg_to_ValueCRef(Value const& value)
{
  return value;
}

inline int64
cscript_arg_to_int64(Value const& value)
{
  return value.GetInt();
}

inline bool
cscript_arg_to_bool(Value const& value)
{
  return value.GetBool();
}

//////////////////////////////////////////////////////////////////////////
//
// Include generated call stubs
//

#include "native.gen.cpp"

// EOF