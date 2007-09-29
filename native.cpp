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

#ifdef _MSC_VER
#include <process.h>
#define strdup _strdup
#elif defined(__GNUC__)
#include <stdlib.h>
#endif

//
// Register a native call
//
NativeCallRegistrar::NativeCallRegistrar(String const& decl, NativeCall call)
{
  Evaluator::GetGlobalScope().AddFun(new NativeFunction(decl, call));
}

//
// Check argument type for a native call
//
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

NATIVE_CALL("function print(value)")
{
  PrintVariant(args[0]);
  return args[0];
}

NATIVE_CALL("function exit(int exitcode = 0)")
{
  // Exit with specified exit code
  exit((int) args[0]->AsInt());

  // Never executed
  throw std::runtime_error("exit() failed");
}

NATIVE_CALL("function quit(int exitcode = 0)")
{
  // Exit with specified exit code
  exit((int) args[0]->AsInt());

  // Never executed
  throw std::runtime_error("exit() failed");
}

NATIVE_CALL("function read()")
{
  String line;
  std::cin >> line;
  return Variant(line);
}

NATIVE_CALL("function count(arg)")
{
  if(args[0]->GetType() != Variant::stAssoc)
  {
    throw std::runtime_error("Invalid type for count");
  }
  return Variant(args[0]->GetMap().size());
}

NATIVE_CALL("function exec(string command)")
{
  // Pass to system
  return Variant(system(args[0]->GetString().c_str()));
}

//////////////////////////////////////////////////////////////////////////
//
// String functions
//

NATIVE_CALL("function strlen(string value)")
{
  ASSERT_TYPE(0, stString);
  return Variant(args[0]->GetString().length());
}

NATIVE_CALL("function substr(string data, int start, int len = 0)")
{
  ASSERT_TYPE(0, stString);
  ASSERT_TYPE(1, stInt);
  ASSERT_TYPE(2, stInt);
  String::size_type off = (String::size_type)args[1]->GetInt();
  String::size_type len = (String::size_type)args[2]->GetInt();
  if(len == 0) len = String::npos;
  return Variant(args[0]->GetString().substr(off, len));
}

NATIVE_CALL("function strstr(string data, string what, int start = 0)")
{
  ASSERT_TYPE(0, stString);
  ASSERT_TYPE(1, stString);
  ASSERT_TYPE(2, stInt);
  String const& src = args[0]->GetString();
  String const& str = args[1]->GetString();
  int32 offset = (int32)args[2]->GetInt();
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

NATIVE_CALL("function strchr(string data, string char, int start = 0)")
{
  ASSERT_TYPE(0, stString);
  ASSERT_TYPE(1, stString);
  ASSERT_TYPE(2, stInt);
  String const& src = args["data"]->GetString();
  String const& chr = args["char"]->GetString();
  int32 offset = (int32)args["start"]->GetInt();
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

NATIVE_CALL("function eval(string code)")
{
  return evaluator->Eval(args[0]->AsString());
}

NATIVE_CALL("function reset()")
{
  throw reset_exception();
}
