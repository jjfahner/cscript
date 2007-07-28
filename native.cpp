#include "native.h"

#include <vector>
#include <map>
#include <iostream>

//
// Native call type
//
typedef void (*NativeCall)(std::stack<VariantRef>&);

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
  void Native_##name(std::stack<VariantRef>& stack);            \
  NativeCallRegistrar register_##name(L#name, Native_##name);   \
  void Native_##name(std::stack<VariantRef>& stack)

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
ExecNative(Quad index, std::stack<VariantRef>& stack)
{
  static NativeCallVec& ncv = getNativeCalls();
  return ncv[index](stack);
}

//////////////////////////////////////////////////////////////////////////
//
// Native call implementations
//

NATIVE_CALL(print)
{
  std::wcout << stack.top()->AsString();
}

NATIVE_CALL(exit)
{
  int ret = 0;
  if(stack.size())
  {
    ret = (int)stack.top()->AsInt();
  }
  exit(ret);
}

NATIVE_CALL(quit)
{
  int ret = 0;
  if(stack.size())
  {
    ret = (int)stack.top()->AsInt();
  }
  exit(ret);
}

NATIVE_CALL(read)
{
  std::wstring line;
  std::wcin >> line;
  stack.push(VariantRef(new Variant(line)));
}