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
#include "function.h"
#include "eval.h"
#include "astlist.h"

AstList const* 
ScriptFunction::GetParameters() const
{
  return m_node->m_a2;
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
  Ast* node = eval.ParseNativeCall(decl);
  if(node == 0)
  {
    throw std::runtime_error("Failed to register native call '" + decl + "'");
  }

  // Extract name and parameter list
  m_name = node->m_a1.GetString();
  m_pars = node->m_a2.GetList();
}

Value
NativeFunction::Execute(Evaluator* evaluator, Arguments& args)
{
  return m_call(evaluator, args);
}

AstList const* 
ExternFunction::GetParameters() const
{
  return m_node->m_a2;
}

#ifdef WIN32

#include <windows.h>

Value
ExternFunction::Execute(Evaluator* evaluator, Arguments& args)
{
  // Load library
  HMODULE hModule = LoadLibrary(m_node->m_a3.GetString().c_str());
  if(hModule == 0)
  {
    throw std::runtime_error("Failed to load library");
  }

  // Find function address
  FARPROC proc = GetProcAddress(hModule, m_node->m_a1.GetString().c_str());
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
  pi = m_node->m_a2.GetList()->rbegin();
  pe = m_node->m_a2.GetList()->rend();
  for(; pi != pe; ++pi, ++index)
  {
    Ast* par = (*pi);
    Ast* typ = par->m_a2;
    switch(typ->m_a1.GetNumber())
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
