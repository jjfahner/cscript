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

Scope::Scope(Ast* node, Scope* parent) :
m_node    (node),
m_parent  (parent)
{
}

VarInfo 
Scope::DeclareParameter(String const& name)
{
  VarInfo vi;
  vi.m_type   = varParam;
  vi.m_offset = MakeParameterId();

  m_names[name] = vi;

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

  m_names[name] = vi;

  return vi;
}

bool
Scope::Lookup(String const& name, VarInfo& vi) const
{
  // Find in local scope
  Names::const_iterator it;
  if((it = m_names.find(name)) != m_names.end())
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
    return next->Lookup(name, vi);
  }
  
  // Failed
  return false;
}

int 
Scope::MakeParameterId()
{
  if(m_node->m_type == function_declaration)
  {
    // Return negative paramcount - 1 to accomodate
    // the return value that is stored at [ST-1], so
    // that argument n is found at [ST-1-n]
    Quad parcount = (Quad)m_node->m_props["parcount"] + 1;
    m_node->m_props["parcount"] = parcount;
    return -int(parcount) - 1;
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
    break;
  default:
    return m_parent->MakeVariableId();
  }

  Quad framesize = m_node->m_props["framesize"];
  m_node->m_props["framesize"] = framesize + 1;
  return framesize;
}
