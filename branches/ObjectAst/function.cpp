//////////////////////////////////////////////////////////////////////////
//
// This file is � 2007 - 2009 JJ Fahner <jan-jaap@jan-jaap.net>
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
#include "astlist.h"

Object const* 
ScriptFunction::GetParameters() const
{
  return (*m_node)["a2"];
}

Value 
ScriptFunction::Execute(Evaluator* evaluator, Arguments& args)
{
  return evaluator->EvalScriptCall(this, args);
}

NativeFunction::NativeFunction(String decl, NativeCall call) :
Function  (""),
m_call    (call),
m_pars    (0)
{
  // Create parser
  Evaluator eval;

  // Parse declaration
  Object* node = eval.ParseNativeCall(decl);
  if(node == 0)
  {
    throw std::runtime_error("Failed to register native call '" + decl + "'");
  }

  // Extract name and parameter list
  m_name = (*node)["a1"];

  // TODO ast
  m_pars = (*node)["a2"];
}

Value
NativeFunction::Execute(Evaluator* evaluator, Arguments& args)
{
  return m_call(evaluator, args);
}

Object const* 
ExternFunction::GetParameters() const
{
  return (*m_node)["a2"];
}

#ifdef WIN32

#include <windows.h>

Value
ExternFunction::Execute(Evaluator* evaluator, Arguments& args)
{
  // Load library
  HMODULE hModule = LoadLibrary((*m_node)["a3"].GetString().c_str());
  if(hModule == 0)
  {
    throw std::runtime_error("Failed to load library");
  }

  // Find function address
  FARPROC proc = GetProcAddress(hModule, (*m_node)["a1"].GetString().c_str());
  if(proc == 0)
  {
    throw std::runtime_error("Failed to retrieve function pointer");
  }

  // Allocate memory for arguments
  size_t argbytes = args.size() * sizeof(int);
  intptr_t * stack = new intptr_t[args.size()];

  // Copy arguments into buffer stack
  size_t index = 0;
  AstList::const_reverse_iterator pi, pe;
  // TODO ast
//   pi = m_node->["a2"].GetList()->rbegin();
//   pe = m_node->["a2"].GetList()->rend();
  for(; pi != pe; ++pi, ++index)
  {
    Ast* par = (*pi);
    Ast* typ = 0; // (*par)["a2"];
    switch((*typ)["a1"].GetInt())
    {
    case Value::tInt:   // int
      stack[index] = (int)args[index].GetInt();
      break;

    case Value::tString:   // string
      stack[index] = (intptr_t)args[index].GetString().c_str();
      break;

    default:
      delete [] stack;
      throw std::runtime_error("Invalid argument type");
    }
  }

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
ExternFunction::Execute(Evaluator* evaluator, Arguments& args)
{
  throw std::runtime_error("Extern calls not implemented on this platform");
}

#endif