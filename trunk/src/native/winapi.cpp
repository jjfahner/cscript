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
#include "native/winapi.h"
#include <exceptions.h>

#include <list>

#include <windows.h>
#undef GetObject

DEF_EXCEPTION(WinapiStructError, "Cannot convert object to struct");
DEF_EXCEPTION(WinapiStructOverwrite, "Memory structure overwritten");

bool 
Winapi::TryGet(Value const& key, Value& value)
{
  HMODULE hModule = LoadLibrary(key.GetString().c_str());
  if(hModule == 0)
  {
    return false;
  }

  value = new WinapiModule(key.GetString(), hModule);

  return true;
}

//////////////////////////////////////////////////////////////////////////

WinapiModule::WinapiModule(String const& name, void* hModule) 
: m_name    (name),
  m_hModule (hModule)
{
}

WinapiModule::~WinapiModule()
{
  FreeLibrary((HMODULE)m_hModule);
}

bool 
WinapiModule::TryGet(Value const& key, Value& value)
{
  void* hProc = GetProcAddress((HMODULE)m_hModule, key.GetString().c_str());
  if(hProc == 0)
  {
    return false;
  }

  value = new WinapiFunction(this, key, hProc);

  return true;
}

//////////////////////////////////////////////////////////////////////////

WinapiFunction::WinapiFunction(WinapiModule* module, String const& name, void* hProc)
: Function  (name),
  m_module  (module),
  m_name    (name),
  m_hProc   (hProc)
{
}

List* 
WinapiFunction::GetParameters() const
{
  return new List();
}

void 
WinapiFunction::MarkObjects(GCObjectVec& grey)
{
  GC::Mark(grey, m_module);
}

//////////////////////////////////////////////////////////////////////////


struct Member
{
  Member(Object* obj, Value const& key, Value const& val) 
    : m_obj (obj), 
      m_key (key), 
      m_val (val), 
      m_ptr (0), 
      m_len (0) 
  {
  }

  Object*         m_obj;
  Value           m_key;
  Value           m_val;
  size_t          m_len;
  unsigned char*  m_ptr;
};


typedef std::vector<Member> Members;

struct BindInfo
{
  Members         m_members;
  unsigned char*  m_buffer;
  size_t          m_size;
};

BindInfo*
BindObject(Object* obj)
{
  Members s;
  size_t size = 0;

  std::list<Enumerator*> stack;
  stack.push_back(obj->GetEnumerator());

  while(stack.size())
  {
    Enumerator* p = stack.back();
    stack.pop_back();

    Value key, val;
    while(p->GetNext(key, val))
    {
      switch(val.Type())
      {
      case Value::tBool:
      case Value::tInt:
        s.push_back(Member(p->GetSource(), key, val));
        s.back().m_len = 4;
        size += s.back().m_len;
        break;

      case Value::tString:
        s.push_back(Member(p->GetSource(), key, val));
        s.back().m_len = val.GetString().length() + 1;
        size += s.back().m_len;
        break;

      case Value::tObject:
        stack.push_back(p);
        p = val->GetEnumerator();
        break;
      }
    }
  }

  size += 4;

  unsigned char* buffer = new unsigned char [size];
  unsigned char* p = buffer;

  for(size_t i = 0; i < s.size(); ++i)
  {
    switch(s[i].m_val.Type())
    {
    case Value::tBool:
      *(unsigned int*)p = (unsigned int) s[i].m_val.GetInt();
      break;
    case Value::tInt:
      *(unsigned int*)p = (unsigned int) s[i].m_val.GetInt();
      break;
    case Value::tString:
      strcpy((char*)p, s[i].m_val.GetString().c_str());
      break;
    default:
      throw WinapiStructError();
    }
    s[i].m_ptr = p;
    p += s[i].m_len;
  }

  *(unsigned int*)p = 0xbaadf00d;

  BindInfo* bi = new BindInfo;
  bi->m_buffer = buffer;
  bi->m_size   = size;
  bi->m_members.swap(s);

  return bi;
}

void
UnbindObject(BindInfo* bi)
{
  if(*(unsigned int*)(bi->m_buffer + bi->m_size - 4) != 0xbaadf00d)
  {
    delete bi;
    throw WinapiStructOverwrite();
  }

  Members::const_iterator it = bi->m_members.begin();
  for(; it != bi->m_members.end(); ++it)
  {
    Member const& m = *it;
    switch(m.m_val.Type())
    {
    case Value::tBool:
      m.m_obj->Set(m.m_key, *(unsigned int*)m.m_ptr ? true : false);
      break;
    case Value::tInt:
      m.m_obj->Set(m.m_key, (int)*(unsigned int*)m.m_ptr);
      break;
    case Value::tString:
      m.m_obj->Set(m.m_key, (char const*)m.m_ptr);
      break;
    }
  }
  
  delete bi;
}

Value 
WinapiFunction::Execute(Arguments& args)
{
  // Allocate memory for arguments
  size_t argbytes = args.size() * sizeof(int);
  intptr_t * stack = new intptr_t[args.size()];

  // Object structures to bind
  std::vector<BindInfo*> bindInfoVec;
  BindInfo* bindInfo;

  // Copy arguments into buffer stack
  size_t index = 0;
  for(size_t index = 0; index < args.size(); ++index)
  {
    size_t aindex = args.size() - 1 - index;
    switch(args[aindex].Type())
    {
    case Value::tInt:   // int
      stack[index] = (int)args[aindex].GetInt();
      break;

    case Value::tString:   // string
      stack[index] = (intptr_t)args[aindex].GetString().c_str();
      break;

    case Value::tObject:
      bindInfo = BindObject(args[aindex]);
      bindInfoVec.push_back(bindInfo);
      stack[index] = (intptr_t)bindInfo->m_buffer;
      break;

    default:
      delete [] stack;
      throw std::runtime_error("Invalid argument type");
    }
  }

  intptr_t dst;
  intptr_t res;
  intptr_t proc = (intptr_t)m_hProc;

  // Make space on stack and copy address
  __asm sub esp, argbytes;
  __asm mov dst, esp;

  // Copy and delete arguments
  memmove((void*)dst, stack, argbytes);
  delete [] stack;

  // Invoke native function
  __asm call proc;

  // Copy return value
  __asm mov res, eax;

  // Unbind objects
  for(size_t i = 0; i < bindInfoVec.size(); ++i)
  {
    UnbindObject(bindInfoVec[i]);
  }

  // Done
  return Value((Value::Int)res);
}
