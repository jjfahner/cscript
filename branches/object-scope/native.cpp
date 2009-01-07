//////////////////////////////////////////////////////////////////////////
//
// This file is © 2007 JJ Fahner <jan-jaap@jan-jaap.net>
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
#include "native.h"
#include "eval.h"
#include "function.h"

#if defined(_MSC_VER)
#define strdup _strdup
#elif defined(__GNUC__)
#include <stdlib.h>
#endif

struct CallInfo
{
  String      m_decl;
  NativeCall  m_call;

  CallInfo(String const& decl = String(), NativeCall call = 0) :
  m_decl (decl),
  m_call (call)
  {
  }
};

typedef std::list<CallInfo> CallInfoList;

CallInfoList& GetCallInfoList()
{
  static CallInfoList list;
  return list;
}

//
// Register a native call
//
NativeCallRegistrar::NativeCallRegistrar(String const& decl, NativeCall call)
{
  GetCallInfoList().push_back(CallInfo(decl, call));
}

/*static*/ void 
NativeCallRegistrar::RegisterCalls()
{
  std::list<CallInfo>::iterator it, ie;
  it = GetCallInfoList().begin();
  ie = GetCallInfoList().end();
  for(; it != ie; ++it)
  {
    Function* fun = new NativeFunction(it->m_decl, it->m_call);
    Evaluator::GetGlobalScope().AddROVariable(fun->GetName(), fun);
  }
}

//////////////////////////////////////////////////////////////////////////
//
// Native call implementations
//

void PrintValue(Value const& val, bool recurse = false)
{
  switch(val.Type())
  {
  case Value::tNull:      
    csout << "null"; 
    return;

  case Value::tBool:      
    csout << (val.GetBool() ? "true" : "false");
    return;

  case Value::tInt:       
  {
    csout << val.GetInt();
    return;
  }

  case Value::tString:    
    csout << val.GetString();
    return;

  case Value::tObject:
    break;

  default: 
    throw std::runtime_error("Invalid subtype");
  }

  Members::const_iterator it, ie;
  it = val.GetObject().GetMembers().begin();
  ie = val.GetObject().GetMembers().end();

  csout << "[";

  String sep;
  for(; it != ie; ++it)
  {
    csout << sep;
    sep = ",";
    PrintValue(it->first);
    csout << ":";
    if(it->second->Type() == Value::tObject && !recurse)
    {
      csout << it->second->GetObject().GetTypeName();
    }
    else
    {
      PrintValue(it->second->GetValue());
    }
  }

  csout << "]";
}

NATIVE_CALL("__native print(value, recurse = true)")
{
  PrintValue(args[0], args[1].GetBool());
  return args[0];
}

NATIVE_CALL("__native exit(int exitcode = 0)")
{
  // Exit with specified exit code
  exit((int) args[0].GetInt());

  // Never executed
  throw std::runtime_error("exit() failed");
}

NATIVE_CALL("__native quit(int exitcode = 0)")
{
  // Exit with specified exit code
  exit((int) args[0].GetInt());

  // Never executed
  throw std::runtime_error("exit() failed");
}

NATIVE_CALL("__native read()")
{
  String line;
  csin >> line;
  return Value(line);
}

NATIVE_CALL("__native count(arg)")
{
  if(args[0].Type() != Value::tObject)
  {
    throw std::runtime_error("Invalid type for count");
  }
  return Value(args[0].GetObject().GetMembers().size());
}

struct objprinter {
  void operator () (Object const* obj) {
    Value v(const_cast<Object*>(obj));
    csout << obj->GetTypeName() << " ";
    PrintValue(v, false); csout << "\n";
  }
};

NATIVE_CALL("__native dump()")
{
  std::for_each(Object::GetObjects().begin(), 
                Object::GetObjects().end(), 
                objprinter());
  return Value();
}

NATIVE_CALL("__native getstack()")
{
  return evaluator->GetCurrentScope();
}

//////////////////////////////////////////////////////////////////////////
//
// String functions
//

NATIVE_CALL("__native strlen(string value)")
{
  return Value((Value::Int)args[0].GetString().length());
}

NATIVE_CALL("__native substr(string data, int start, int len = 0)")
{
  String::size_type off = (String::size_type)args[1].GetInt();
  String::size_type len = (String::size_type)args[2].GetInt();
  if(len == 0) len = String::npos;
  return Value(args[0].GetString().substr(off, len));
}

NATIVE_CALL("__native strstr(string data, string what, int start = 0)")
{
  String const& src = args[0].GetString();
  String const& str = args[1].GetString();
  int32 offset = (int32)args[2].GetInt();
  if(str.length() < 1)
  {
    throw std::runtime_error("Empty string in call to strstr");
  }
  size_t res = src.find(str, offset);
  if(res == String::npos)
  {
    return Value(-1);
  }
  return Value((Value::Int)res);
}

NATIVE_CALL("__native strchr(string data, string char, int start = 0)")
{
  String const& src = args["data"].GetString();
  String const& chr = args["char"].GetString();
  int32 offset = (int32)args["start"].GetInt();
  if(chr.length() < 1)
  {
    throw std::runtime_error("Empty string in call to strchr");
  }
  size_t res = src.find(chr[0], offset);
  if(res == String::npos)
  {
    return Value(-1);
  }
  return Value((Value::Int)res);
}

NATIVE_CALL("__native isset(ojb, what)")
{
  return args[0].Type() == Value::tObject && 
    args[0].GetObject().Contains(args[1]);
}

NATIVE_CALL("__native eval(string code)")
{
  return evaluator->Eval(args[0].GetString());
}

NATIVE_CALL("__native reset()")
{
  throw reset_exception();
}

NATIVE_CALL("__native collect()")
{
  evaluator->Collect();
  return Value();
}

#if defined(WIN32) && !defined(_WIN32_WCE)

#include <windows.h>

NATIVE_CALL("__native exec(string command)")
{
  // Pass to system
# if defined(_WIN32_WCE)
  return Value();
# else
  return Value(system(args[0].GetString().c_str()));
# endif
}

NATIVE_CALL("__native native(string libname, string fun, arglist...)")
{
  // Load library
  HMODULE hModule = LoadLibrary(args[0].GetString().c_str());
  if(hModule == 0)
  {
    throw std::runtime_error("Failed to load library");
  }

  // Find function address
  FARPROC proc = GetProcAddress(hModule, args[1].GetString().c_str());
  if(proc == 0)
  {
    throw std::runtime_error("Failed to retrieve function pointer");
  }

//   // Retrieve map
//   Value::AssocType& map = args[2].GetMap();
// 
//   // Allocate memory for arguments
//   size_t argbytes = map.size() * sizeof(int);
//   intptr_t * stack = new intptr_t[map.size()];
// 
//   // Copy arguments into buffer stack
//   for(size_t index = 0; index < map.size(); ++index)
//   {
//     VariantRef& arg = map[Value(index)];
//     switch(arg->GetType())
//     {
//     case Value::stInt:   // int
//       stack[index] = (int)arg->GetInt();
//       break;
// 
//     case Value::stString:   // string
//       stack[index] = (intptr_t)arg->GetString().c_str();
//       break;
// 
//     default:
//       delete [] stack;
//       throw std::runtime_error("Invalid argument type");
//     }
//   }

//   intptr_t dst;
  intptr_t res = 0;
// 
//   // Make space on stack and copy address
//    __asm sub esp, argbytes;
//    __asm mov dst, esp
// 
//   // Copy and delete arguments
//   memmove((void*)dst, stack, argbytes);
//   delete [] stack;
// 
// 	// Invoke native function
//   __asm call proc;
// 
//   // Copy return value
//   __asm mov res, eax;

  // Free the library
  FreeLibrary(hModule);

  // Done
  return Value((Value::Int)res);
}

#endif
