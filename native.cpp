#include "native.h"
#include "machine.h"

#include <vector>
#include <map>
#include <iostream>

#include <process.h>

//
// Native call type
//
typedef void (*NativeCall)(StackMachine&);

//
// Mapping between name and index
//
typedef std::map<std::wstring, Quad> NativeCallMap;
NativeCallMap& getNativeCallMap()
{
  static NativeCallMap nativeCallMap;
  return nativeCallMap;
}

//
// Mapping between index and call
//
typedef std::vector<NativeCall> NativeCallVec;
NativeCallVec& getNativeCalls()
{
  static NativeCallVec nativeCalls;
  return nativeCalls;
}

//
// Register a native call
//
struct NativeCallRegistrar
{
  NativeCallRegistrar(std::wstring const& name, NativeCall call)
  {
    static NativeCallMap& ncm = getNativeCallMap();
    static NativeCallVec& ncv = getNativeCalls();
    ncv.push_back(call);
    ncm[name] = (Quad) (ncv.size() - 1);
  }
};

#define NATIVE_CALL(name)                                       \
  void Native_##name(StackMachine& machine);                    \
  NativeCallRegistrar register_##name(L#name, Native_##name);   \
  void Native_##name(StackMachine& machine)

//
// Find a native call index
//
Quad 
FindNative(std::wstring const& name)
{
  static NativeCallMap& ncm = getNativeCallMap();

  NativeCallMap::iterator it = ncm.find(name);
  if(it == ncm.end())
  {
    return -1;
  }

  return it->second;
}

//
// Execute a native call
//
void 
ExecNative(Quad index, StackMachine& machine)
{
  static NativeCallVec& ncv = getNativeCalls();
  return ncv[index](machine);
}

//////////////////////////////////////////////////////////////////////////
//
// Native call implementations
//

NATIVE_CALL(print)
{
  std::wcout << machine.StackTop()->AsString();
}

NATIVE_CALL(exit)
{
  int ret = 0;
  if(machine.StackSize())
  {
    ret = (int)machine.StackTop()->AsInt();
  }
  exit(ret);
}

NATIVE_CALL(quit)
{
  int ret = 0;
  if(machine.StackSize())
  {
    ret = (int)machine.StackTop()->AsInt();
  }
  exit(ret);
}

NATIVE_CALL(read)
{
  std::wstring line;
  std::wcin >> line;
  machine.PushStack(line);
}

NATIVE_CALL(length)
{
  machine.PushStack(machine.PopStack()->AsString().length());
}

NATIVE_CALL(exec)
{
  std::wstring cmd = machine.PopStack()->AsString();
  int result = (int)_wspawnlp(_P_NOWAIT, cmd.c_str(), cmd.c_str(), 0);
  machine.PushStack(result);
}
