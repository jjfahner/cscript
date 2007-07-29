#include "native.h"
#include "machine.h"

#include <vector>
#include <map>
#include <iostream>

#include <process.h>

//
// Mapping between index and call
//
typedef std::vector<Function*> NativeCalls;
NativeCalls& getNativeCalls()
{
  static NativeCalls nativeCalls;
  return nativeCalls;
}

//
// Register a native call
//
struct NativeCallRegistrar
{
  NativeCallRegistrar(String const& name, NativeCall call, Quad minPar, Quad maxPar)
  {
    static NativeCalls& ncv = getNativeCalls();
    Function* fun = new Function;
    fun->m_name   = name;
    fun->m_native = true;
    fun->m_offset = (Quad)ncv.size();
    fun->m_minPar = minPar;
    fun->m_maxPar = maxPar;
    fun->m_funPtr = call;
    ncv.push_back(fun);
  }
};

#define NATIVE_CALL(name,minPar,maxPar)         \
  void Native_##name(StackMachine& machine);    \
  NativeCallRegistrar register_##name(L#name,   \
    Native_##name, minPar, maxPar);             \
  void Native_##name(StackMachine& machine)

//
// Find a native call index
//
Function* 
FindNative(String const& name)
{
  static NativeCalls& ncv = getNativeCalls();

  size_t i = 0, n = ncv.size();
  for(; i < n; ++i)
  {
    if(ncv[i]->m_name == name)
    {
      return ncv[i];
    }
  }

  return 0;
}

//
// Execute a native call
//
void 
ExecNative(Quad index, StackMachine& machine)
{
  static NativeCalls& ncv = getNativeCalls();  
  ncv[index]->m_funPtr(machine);
}

//////////////////////////////////////////////////////////////////////////
//
// Native call implementations
//

NATIVE_CALL(print, 1, 2)
{
  std::wcout << machine.StackTop()->AsString();
}

NATIVE_CALL(exit, 0, 1)
{
  int ret = 0;
  if(machine.StackSize())
  {
    ret = (int)machine.StackTop()->AsInt();
  }
  exit(ret);
}

NATIVE_CALL(quit, 0, 1)
{
  int ret = 0;
  if(machine.StackSize())
  {
    ret = (int)machine.StackTop()->AsInt();
  }
  exit(ret);
}

NATIVE_CALL(read, 0, 0)
{
  String line;
  std::wcin >> line;
  machine.PushStack(line);
}

NATIVE_CALL(length, 1, 1)
{
  VariantRef ref = machine.PopStack();
  if(ref->GetType() != Variant::stString)
  {
    throw std::runtime_error("Invalid type for length");
  }
  machine.PushStack(ref->AsString().length());
}

NATIVE_CALL(exec, 1, 1)
{
  String cmd = machine.PopStack()->AsString();
  int result = (int)_wspawnlp(_P_NOWAIT, cmd.c_str(), cmd.c_str(), 0);
  machine.PushStack(result);
}
