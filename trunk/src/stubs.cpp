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
#include <native/xml.h>
#include <dict.h>

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
typedef Value (*MethodStub)(Evaluator*, Arguments const&);
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
  RValue*     m_var;
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
// Native method wrapper
//

class NativeMethod : public Function, public RValue
{
  NativeCall* m_call;
  Value m_value;

public:

  NativeMethod(NativeCall* call) :
  Function  (call->m_name),
  m_call    (call)
  {
    m_value = this;
    GC::Pin(this);
  }

  virtual List* GetParameters() const
  {
    return 0;
  }

  virtual Value Execute(Evaluator* evaluator, Arguments& args)
  {
    return m_call->m_method(evaluator, args);
  }

  virtual Value const& GetValue() const
  {    
    return m_value;
  }
};

//////////////////////////////////////////////////////////////////////////
//
// Native RO property wrapper
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
    m_value = m_call->m_roprop(m_instance);
    return m_value;
  }
};

//////////////////////////////////////////////////////////////////////////
//
// Native RW property wrapper
//

class NativeRwProp : public LValue, public Object
{
  NativeCall* m_call;
  Object* m_instance;
  mutable Value m_value;

public:

  NativeRwProp(NativeCall* call, Object* instance) :
  m_call (call),
  m_instance (instance)
  {
  }

  virtual Value const& GetValue() const
  {
    m_value = m_call->m_roprop(m_instance);
    return m_value;
  }

  virtual void SetValue(Value const& rhs)
  {
    m_call->m_rwprop(m_instance, rhs);
  }
};

//////////////////////////////////////////////////////////////////////////
//
// Resolver implementation
//

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
            pTable->m_var = new NativeMethod(pTable);
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

      case stRwProp:
        {
          pValue = new NativeRwProp(pTable, instance);
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

