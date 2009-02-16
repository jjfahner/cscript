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
#include <native/winapi.h>
#include <eval.h>
#include <windows.h>

DEFINE_NATIVE_LINKAGE(Winapi)

//////////////////////////////////////////////////////////////////////////

NATIVE_CALL("exec(string command)")
{
# if defined(_WIN32_WCE)
  return Value();
# else
  return Value(system(args[0].GetString().c_str()));
# endif
}

//////////////////////////////////////////////////////////////////////////

NATIVE_CALL("winapi(string libname, string fun, arglist...)")
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
