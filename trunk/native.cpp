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
NativeCallRegistrar::NativeCallRegistrar(String const& name, NativeCall call, int32 minPar, int32 maxPar)
{
  static NativeCalls& ncv = getNativeCalls();
  NativeCallInfo* fun = new NativeCallInfo;
  fun->m_name   = name;
  fun->m_minPar = minPar;
  fun->m_maxPar = maxPar;
  fun->m_funPtr = call;
  ncv.push_back(fun);
}

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

void 
AssertType(Arguments const& args, size_t index, Variant::SubTypes type, char const* function)
{
  if(index >= args.size())
  {
    throw std::runtime_error("Invalid number of arguments passed to " + String(function));
  }
  if(args[index].Empty() || args[index]->GetType() != type)
  {
    throw std::runtime_error("Invalid argument passed to " + String(function));
  }
}

//////////////////////////////////////////////////////////////////////////
//
// Native call implementations
//

void PrintVariant(VariantRef const& ref)
{
  switch(ref->GetType())
  {
  case Variant::stNull:       std::cout << "null"; return;
  case Variant::stBool:       std::cout << (ref->GetBool() ? "true" : "false"); return;
  case Variant::stInt:        std::cout << ref->GetInt(); return;
  case Variant::stString:     std::cout << ref->GetString(); return;
  case Variant::stAssoc:      break;
  case Variant::stResource:   std::cout << "resource"; return;
  default: throw std::runtime_error("Invalid subtype");
  }

  Variant::AssocType::const_iterator it, ie;
  it = ref->GetMap().begin();
  ie = ref->GetMap().end();
  std::cout << "[";
  String sep;
  for(; it != ie; ++it)
  {
    std::cout << sep;
    sep = ",";
    PrintVariant(it->second);
  }
  std::cout << "]";
}

NATIVE_CALL(print, 1, 1)
{
  PrintVariant(args[0]);
  return args[0];
}

NATIVE_CALL(exit, 0, 1)
{
  int ret = 0;
  if(args.size() == 1)
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
  if(args.size() == 1)
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
  std::cin >> line;
  return Variant(line);
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

//////////////////////////////////////////////////////////////////////////
//
// String functions
//

NATIVE_CALL(strlen, 1, 1)
{
  ASSERT_TYPE(0, stString);
  return Variant(args[0]->GetString().length());
}

NATIVE_CALL(substr, 2, 3)
{
  ASSERT_TYPE(0, stString);
  ASSERT_TYPE(1, stInt);
  if(args.size() == 2)
  {
    return Variant(
      args[0]->GetString().substr(
      (int)args[1]->GetInt()));
  }
  ASSERT_TYPE(2, stInt);
  return Variant(
    args[0]->GetString().substr(
    (int)args[1]->GetInt(), 
    (int)args[2]->GetInt()));
}

NATIVE_CALL(strstr, 2, 3)
{
  ASSERT_TYPE(0, stString);
  ASSERT_TYPE(1, stString);
  int32 offset = 0;
  if(args.size() == 3)
  {
    ASSERT_TYPE(2, stInt);
    offset = (int32)args[2]->GetInt();
  }
  String const& src = args[0]->GetString();
  String const& str = args[1]->GetString();
  if(str.length() < 1)
  {
    throw std::runtime_error("Empty string in call to strstr");
  }
  size_t res = src.find(str, offset);
  if(res == String::npos)
  {
    return Variant(-1);
  }
  return Variant(res);
}

NATIVE_CALL(strchr, 2, 3)
{
  ASSERT_TYPE(0, stString);
  ASSERT_TYPE(1, stString);
  int32 offset = 0;
  if(args.size() == 3)
  {
    ASSERT_TYPE(2, stInt);
    offset = (int32)args[2]->GetInt();
  }
  String const& src = args[0]->GetString();
  String const& chr = args[1]->GetString();
  if(chr.length() < 1)
  {
    throw std::runtime_error("Empty string in call to strchr");
  }
  size_t res = src.find(chr[0], offset);
  if(res == String::npos)
  {
    return Variant(-1);
  }
  return Variant(res);
}
