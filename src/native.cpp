//////////////////////////////////////////////////////////////////////////
//
// This file is © 2007 - 2009 JJ Fahner <jan-jaap@jan-jaap.net>
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
#include <native.h>
#include <eval.h>
#include <function.h>
#include <scope.h>

#include <list>
#include <algorithm>
#include <iostream>

struct CallInfo
{
  String                  m_decl;
  NativeFunction::FunPtr  m_call;

  CallInfo(String const& decl = String(), NativeFunction::FunPtr call = 0) :
  m_decl (decl),
  m_call (call)
  {
  }
};

typedef std::list<CallInfo> CallInfoList;

CallInfoList& GetCallInfoList()
{
  static CallInfoList list;
  return list;
}

//
// Register a native call
//
NativeCallRegistrar::NativeCallRegistrar(String const& decl, NativeFunction::FunPtr call)
{
  GetCallInfoList().push_back(CallInfo(decl, call));
}

/*static*/ void 
NativeCallRegistrar::RegisterCalls()
{
  std::list<CallInfo>::iterator it, ie;
  it = GetCallInfoList().begin();
  ie = GetCallInfoList().end();
  for(; it != ie; ++it)
  {
    Function* fun = new NativeFunction(it->m_decl, it->m_call);
    Evaluator::GetGlobalScope()->Add(fun->GetName(), fun);
  }
}

//////////////////////////////////////////////////////////////////////////

NativeFunction::NativeFunction(String decl, FunPtr funPtr) :
Function  (""),
m_funPtr  (funPtr)
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
  m_name = Ast_A1(node).GetString();

  // Store the ast node
  (*this)["__ast"] = node;
}

Value
NativeFunction::Execute(Evaluator* evaluator, Arguments& args)
{
  return m_funPtr(evaluator, args);
}
