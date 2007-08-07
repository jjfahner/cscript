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

Scope::Scope(CodeGenerator& cg, Ast* node, Scope* parent) :
m_cg      (cg),
m_node    (node),
m_parent  (parent)
{
}

int 
Scope::DeclareParameter(String const& name)
{
  int id = MakeParameterId();
  m_names[name] = id;
  return id;
}

int 
Scope::DeclareVariable(String const& name)
{
  int id = MakeVariableId();
  m_names[name] = id;
  return id;
}

int 
Scope::Lookup(String const& name, bool& global) const
{
  // Find in local scope
  Names::const_iterator it;
  if((it = m_names.find(name)) != m_names.end())
  {
    if(m_parent == 0)
    {
      global = true;
    }
    return it->second;
  }

  // Look in parent scope up to function boundary
  if(m_node->m_type != function_declaration && m_parent)
  {
    return m_parent->Lookup(name, global);
  }

  // Find global scope
  Scope* parent = m_parent;
  while(parent)
  {
    if(parent->m_parent == 0)
    {
      return parent->Lookup(name, global);
    }
    parent = parent->m_parent;
  }
  
  m_cg.ReportError("variable or parameter '" + name + "' undefined");
  return 0;
}

int 
Scope::MakeParameterId()
{
  if(m_node->m_type == function_declaration)
  {
    // Return negative paramcount - 1 to accomodate
    // the return value that is stored at [ST-1], so
    // that argument n is found at [ST-1-n]
    return -int(++m_node->m_parcount) - 1;
  }
  throw std::logic_error("Invalid node for parameter declaration");
}

int 
Scope::MakeVariableId()
{
  if(m_node->m_type == function_declaration)
  {
    return m_node->m_framesize++;
  }
  if(m_parent == 0)
  {
    return m_node->m_framesize++;
  }
  return m_parent->MakeVariableId();
}
