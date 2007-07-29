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

#define NATIVE_CALL(name,minPar,maxPar)                     \
  void Native_##name(StackMachine& machine, Word numArgs);  \
  NativeCallRegistrar register_##name(L#name,               \
    Native_##name, minPar, maxPar);                         \
  void Native_##name(StackMachine& machine, Word numArgs)

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
ExecNative(Quad index, StackMachine& machine, Word numArgs)
{
  static NativeCalls& ncv = getNativeCalls();  
  ncv[index]->m_funPtr(machine, numArgs);
}

//////////////////////////////////////////////////////////////////////////
//
// Native call implementations
//

void PrintVariant(VariantRef const& ref)
{
  switch(ref->GetType())
  {
  case Variant::stNull:   std::wcout << "null"; return;
  case Variant::stBool:   std::wcout << (ref->GetBool() ? "true" : "false"); return;
  case Variant::stInt:    std::wcout << ref->GetInt(); return;
  case Variant::stString: std::wcout << ref->GetString(); return;
  case Variant::stMap:    break;
  default: throw std::runtime_error("Invalid subtype");
  }

  Variant::MapType::const_iterator it, ie;
  it = ref->GetMap().begin();
  ie = ref->GetMap().end();
  std::wcout << "[";
  std::wstring sep;
  for(; it != ie; ++it)
  {
    std::wcout << sep;
    sep = L",";
    PrintVariant(it->second);
  }
  std::wcout << "]";
}

NATIVE_CALL(print, 1, 2)
{
  PrintVariant(machine.StackTop());
}

NATIVE_CALL(exit, 0, 1)
{
  int ret = 0;
  if(numArgs == 1)
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
  machine.PushStack(ref->GetString().length());
}

NATIVE_CALL(count, 1, 1)
{
  VariantRef ref = machine.PopStack();
  if(ref->GetType() != Variant::stMap)
  {
    throw std::runtime_error("Invalid type for count");
  }
  machine.PushStack(ref->GetMap().size());
}

NATIVE_CALL(exec, 1, -1)
{
  // Init argument list
  wchar_t* argv[1024];
  memset(argv, 0, sizeof(argv));

  // Copy arguments
  for(int i = numArgs - 1; i >= 0; --i)
  {
    argv[i] = _wcsdup(machine.PopStack()->AsString().c_str());
  }

  // Execute command
  int result = (int)_wspawnv(_P_NOWAIT, argv[0], argv);

  // Free argument strings
  for(Word i = 0; i < numArgs; ++i)
  {
    free(argv[i]);
  }

  // Put result back on stack
  machine.PushStack(result);
}

