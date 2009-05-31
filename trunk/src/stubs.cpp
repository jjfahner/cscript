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
#include <stubs.h>
#include <args.h>
#include <native.h>
#include <native/socket.h>
#include <native/file.h>
#include <native/xml.h>
#include <dict.h>
#include <native/path.h>
#include <native/consio.h>
#include <native/standard.h>

//////////////////////////////////////////////////////////////////////////

//
// Types of stubs
//
enum StubType
{
  stMethod = 0,
  stRoProp = 1,
  stRwProp = 2
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
// Native call conversion functions
//

inline String const& __stub_arg_to_String(Value const& v) {
  return v.GetString();
}

inline String const& __stub_arg_to_StringCRef(Value const& v) {
  return v.GetString();
}

inline Value const& __stub_arg_to_Value(Value const& v) {
  return v;
}

inline int __stub_arg_to_int(Value const& v) {
  return (int) v.GetInt();
}

inline int64 __stub_arg_to_int64(Value const& v) {
  return v.GetInt();
}

inline bool __stub_arg_to_bool(Value const& v) {
  return v.GetBool();
}

inline ValueCRef __stub_arg_to_ValueCRef(Value const& v) {
  return v;
}

//////////////////////////////////////////////////////////////////////////
//
// Include generated call stubs
//

#include "stubs.gen.cpp"

//////////////////////////////////////////////////////////////////////////
//
// Resolver implementation
//

bool 
NativeCallTryGet(struct NativeCall* pTable, Object* instance, Value const& key, Value& pValue)
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
NativeCallTrySet(struct NativeCall* pTable, Object* instance, Value const& key, Value const& value)
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
NativeCallTryEval(struct NativeCall* pTable, Object* instance, Value const& key, Evaluator* evaluator, Arguments& arguments, Value& result)
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
