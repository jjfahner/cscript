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
#include "scope.h"
#include "codegen.h"
#include "report.h"
#include "native.h"

Scope::Scope(Ast* node, Scope* parent) :
m_node    (node),
m_parent  (parent)
{
  if(m_parent && m_parent->m_node->m_type == class_declaration)
  {
    m_node->m_props["parcount"] = Quad(1);
  }
}

VarInfo 
Scope::DeclareParameter(String const& name)
{
  VarInfo vi;
  vi.m_type   = varParam;
  vi.m_offset = MakeParameterId();

  m_variables[name] = vi;

  return vi;
}

VarInfo 
Scope::DeclareVariable(String const& name)
{
  VarInfo vi;
  vi.m_offset = MakeVariableId();
  vi.m_type   = varLocal;
  if(m_node->m_type == class_declaration)
  {
    vi.m_type = varMember;
  }
  else if(m_parent == 0)
  {
    vi.m_type = varGlobal;
  }
  m_variables[name] = vi;
  return vi;
}

String
Scope::DeclareFunction(String const& name, Ast* node)
{
  // Determine function info
  FunInfo fi;
  fi.m_node = node;
  fi.m_type = funGlobal;
  if(m_node->m_type == class_declaration)
  {
    fi.m_type = funMember;
  }

  // Register function
  m_functions[name] = fi;

  // Connect to scope
  node->m_props["inscope"] = m_node;

  // Connect to class
  if(m_node->m_type == class_declaration)
  {
    node->m_props["class"] = m_node;
  }

  // Determine mangled name
  String mangled = name;
  if(m_node->m_type == class_declaration)
  {
    mangled += "@" + String(m_node->m_a1);
  }

  // Return mangled name
  return mangled;
}

bool
Scope::LookupFun(String const& name, FunInfo& info) const
{
  // Find in local scope
  Functions::const_iterator it = m_functions.find(name);
  if(it != m_functions.end())
  {
    info = it->second;
    return true;
  }

  // Find in parent scope
  if(m_parent)
  {
    return m_parent->LookupFun(name, info);
  }

  // Find native call
  NativeCallInfo* nc = FindNative(name);
  if(nc != 0)
  {
    FunInfo fi;
    fi.m_node = 0;
    fi.m_type = funNative;
    return true;
  }

  // Unknown function
  return false;
}

bool
Scope::LookupVar(String const& name, VarInfo& vi) const
{
  // Find in local scope
  Variables::const_iterator it;
  if((it = m_variables.find(name)) != m_variables.end())
  {
    vi = it->second;
    return true;
  }

  // Find in parent/find in global
  Scope* next = m_parent;
  bool findGlobal = false;

  // Special node types
  if(m_node->m_type == function_declaration)
  {
    if(next->m_node->m_type != class_declaration)
    {
      findGlobal = true;
    }
  }
  else if(m_node->m_type == class_declaration)
  {
    findGlobal = true;
  }

  // Determine next node in hierarchy
  if(findGlobal)
  {
    while(next->m_parent)
    {
      next = next->m_parent;
    }
  }

  // Proceed with lookup
  if(next)
  {
    return next->LookupVar(name, vi);
  }
  
  // Failed
  return false;
}

int 
Scope::MakeParameterId()
{
  if(m_node->m_type == function_declaration)
  {
    Quad parcount = (Quad)m_node->m_props["parcount"] + 1;
    m_node->m_props["parcount"] = parcount;
    return -int(parcount);
  }
  throw std::logic_error("Invalid node for parameter declaration");
}

int 
Scope::MakeVariableId()
{
  switch(m_node->m_type)
  {
  case translation_unit:
  case function_declaration:
  case class_declaration:
    break;
  default:
    return m_parent->MakeVariableId();
  }

  Quad framesize = m_node->m_props["framesize"];
  m_node->m_props["framesize"] = framesize + 1;
  return framesize;
}
