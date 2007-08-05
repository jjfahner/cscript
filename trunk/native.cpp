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
#include "machine.h"

#ifdef _MSC_VER
#include <process.h>
#define strdup _strdup
#elif defined(__GNUC__)
#include <stdlib.h>
#endif

//
// Mapping between index and call
//
typedef std::vector<NativeCallInfo*> NativeCalls;
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
    NativeCallInfo* fun = new NativeCallInfo;
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
  VariantRef Native_##name(std::vector<VariantRef>&, Quad); \
  NativeCallRegistrar register_##name(#name,                \
    Native_##name, minPar, maxPar);                         \
  VariantRef Native_##name(std::vector<VariantRef>& args, Quad numArgs)

//
// Find a native call index
//
NativeCallInfo* 
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
ExecNative(Quad index, Quad numArgs, std::vector<VariantRef>& stack, Quad SP)
{
  static NativeCalls& ncv = getNativeCalls();

  // Create argument vector
  std::vector<VariantRef> args;
  args.resize(numArgs);

  // Copy arguments
  for(Quad i = 0; i < numArgs; ++i)
  {
    args[i] = stack[SP-2-i];
  }

  // Call function
  stack[SP-1] = ncv[index]->m_funPtr(args, numArgs);
}

//////////////////////////////////////////////////////////////////////////
//
// Native call implementations
//

void PrintVariant(VariantRef const& ref)
{
  switch(ref->GetType())
  {
  case Variant::stNull:   cout << "null"; return;
  case Variant::stBool:   cout << (ref->GetBool() ? "true" : "false"); return;
  case Variant::stInt:    cout << ref->GetInt(); return;
  case Variant::stString: cout << ref->GetString(); return;
  case Variant::stAssoc:  break;
  default: throw std::runtime_error("Invalid subtype");
  }

  Variant::AssocType::const_iterator it, ie;
  it = ref->GetMap().begin();
  ie = ref->GetMap().end();
  cout << "[";
  String sep;
  for(; it != ie; ++it)
  {
    cout << sep;
    sep = ",";
    PrintVariant(it->second);
  }
  std::wcout << "]";
}

NATIVE_CALL(print, 1, 2)
{
  PrintVariant(args[0]);
  return args[0];
}

NATIVE_CALL(exit, 0, 1)
{
  int ret = 0;
  if(numArgs == 1)
  {
    ret = (int) args[0]->AsInt();
  }

  exit(ret);

  // Never executed
  return args[0];
}

NATIVE_CALL(quit, 0, 1)
{
  int ret = 0;
  if(numArgs == 1)
  {
    ret = (int) args[0]->AsInt();
  }

  exit(ret);
  
  // Never executed
  return args[0];
}

NATIVE_CALL(read, 0, 0)
{
  String line;
  cin >> line;
  return line;
}

NATIVE_CALL(length, 1, 1)
{
  if(args[0]->GetType() != Variant::stString)
  {
    throw std::runtime_error("Invalid type for length");
  }
  return Variant(args[0]->GetString().length());
}

NATIVE_CALL(count, 1, 1)
{
  if(args[0]->GetType() != Variant::stAssoc)
  {
    throw std::runtime_error("Invalid type for count");
  }
  return Variant(args[0]->GetMap().size());
}

NATIVE_CALL(exec, 1, 1)
{
  // Pass to system
  return Variant(system(args[0]->GetString().c_str()));
}
