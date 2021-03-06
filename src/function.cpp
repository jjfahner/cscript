//////////////////////////////////////////////////////////////////////////
//
// This file is � 2007 - 2011 JJ Fahner <jan-jaap@jan-jaap.net>
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
#include "function.h"
#include "eval.h"

DataType* 
Function::GetType()
{
  return FunctionType::Instance();
}


Value 
ScriptFunction::Execute(Arguments& args)
{
  return CurEval.EvalScriptCall(this, args);
}

List* 
ExternFunction::GetParameters() const
{
  return AstList_A2(GetNode());
}

#ifdef WIN32

#include <windows.h>

Value
ExternFunction::Execute(Arguments& args)
{
  throw std::runtime_error("External functions are disabled");

  Object* node = GetNode();

  // Load library
  HMODULE hModule = LoadLibrary(Ast_A3(node).GetString().c_str());
  if(hModule == 0)
  {
    throw std::runtime_error("Failed to load library");
  }

  // Find function address
  FARPROC proc = GetProcAddress(hModule, Ast_A1(node).GetString().c_str());
  if(proc == 0)
  {
    throw std::runtime_error("Failed to retrieve function pointer");
  }

  // Allocate memory for arguments
  size_t argbytes = args.size() * sizeof(int);
  intptr_t * stack = new intptr_t[args.size()];

  // Copy arguments into buffer stack
//   size_t index = 0;
//   AstList::const_reverse_iterator pi, pe;
//   AstList list(Ast_A2(node));
//   pi = list.rbegin();
//   pe = list.rend();
//   for(; pi != pe; ++pi, ++index)
//   {
//     switch(Ast_A2(Ast_A1(*pi)).GetInt())
//     {
//     case Value::tInt:   // int
//       stack[index] = (int)args[index].GetInt();
//       break;
// 
//     case Value::tString:   // string
//       stack[index] = (intptr_t)args[index].GetString().c_str();
//       break;
// 
//     default:
//       delete [] stack;
//       throw std::runtime_error("Invalid argument type");
//     }
//   }

  intptr_t dst;
  intptr_t res;

  // Make space on stack and copy address
   __asm sub esp, argbytes;
   __asm mov dst, esp

  // Copy and delete arguments
  memmove((void*)dst, stack, argbytes);
  delete [] stack;

	// Invoke native function
  __asm call proc;

  // Copy return value
  __asm mov res, eax;

  // Done
  return Value((Value::Int)res);
}

#else

Value
ExternFunction::Execute(Arguments& args)
{
  throw std::runtime_error("Extern calls not implemented on this platform");
}

#endif
