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
#include <variable.h>
#include <args.h>
#include <native.h>
#include <native/socket.h>
#include <native/file.h>

class Value;
class Evaluator;

enum StubType
{
  stMethod = 0,
  stRoProp = 1,
  stRwProp = 2
};

struct NativeCall
{
  char const* m_name;
  void*       m_stub;
  StubType    m_type;
  ROVariable* m_var;
};


inline String const& __stub_arg_to_String(Value const& v)
{
  return v.GetString();
}

inline Value const& __stub_arg_to_Value(Value const& v)
{
  return v;
}

inline int __stub_arg_to_int(Value const& v)
{
  return (int) v.GetInt();
}

inline int64 __stub_arg_to_int64(Value const& v)
{
  return v.GetInt();
}

inline bool __stub_arg_to_bool(Value const& v)
{
  return v.GetBool();
}

#include "stubs.gen.cpp"

//////////////////////////////////////////////////////////////////////////

typedef Value (*MethodStub)(Evaluator*, Arguments const&);
typedef Value (*RoPropStub)(Object*);

//
// NativeRoProp implements the RValue semantics of
// read-only native properties. It's derived from
// Object to make it garbage-collected.
//
class NativeRoProp : public RValue, public Object
{
  NativeCall* m_call;
  Object* m_instance;
  mutable Value m_value;

public:

  NativeRoProp(NativeCall* call, Object* instance) :
  m_call (call),
  m_instance (instance)
  {
  }

  virtual Value const& GetValue() const
  {
    m_value = ((RoPropStub)m_call->m_stub)(m_instance);
    return m_value;
  }
};

//////////////////////////////////////////////////////////////////////////

bool 
NativeCallContainsKey(NativeCall* pTable, Object* instance, String const& key, bool checkProto)
{
  while(pTable->m_name)
  {
    if(key == pTable->m_name)
    {
      return true;
    }
    ++pTable;
  }
  return false;
}

bool 
NativeCallFind(NativeCall* pTable, Object* instance, String const& key, RValue*& pValue, bool checkProto)
{
  while(pTable->m_name)
  {
    if(key == pTable->m_name)
    {
      switch(pTable->m_type)
      {
      case stMethod:
        {
          if(pTable->m_var == 0)
          {
            NativeFunction* pFun = new NativeFunction("", (MethodStub)pTable->m_stub);
            GC::Pin(pFun);
            pTable->m_var = new ROVariable(pFun);
          }
          pValue = pTable->m_var;
          return true;
        }
        break;

      case stRoProp:
        {
          pValue = new NativeRoProp(pTable, instance);
          return true;
        }
        break;

      default:
        throw std::runtime_error("Invalid native stubtype");
      }
    }
    ++pTable;
  }
  return false;
}

