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
#ifndef CSCRIPT_SCOPE_H
#define CSCRIPT_SCOPE_H

#include "types.h"
#include "ast.h"

class CodeGenerator;
class Reporter;

enum VarTypes
{
  varLocal,
  varParam,
  varMember,
  varGlobal,
};

struct VarInfo
{
  VarTypes  m_type;
  int       m_offset;
};

class Scope 
{
public:

  //
  // Construction
  //
  Scope(Ast* node, Scope* parent);

  //
  // Scope parent
  //
  Scope* GetParent() const;

  //
  // Scope node
  //
  Ast* GetNode() const;

  //
  // Register a name
  //
  VarInfo DeclareParameter(String const& name);

  //
  // Declare a name
  //
  VarInfo DeclareVariable(String const& name);

  //
  // Find a name
  //
  bool Lookup(String const& name, VarInfo& info) const;

private:

  int MakeParameterId();

  int MakeVariableId();

  typedef std::map<String, VarInfo> Names;

  //
  // Members
  //
  Scope*      m_parent;
  Ast*        m_node;
  Names       m_names;

};

inline Scope* 
Scope::GetParent() const
{
  return m_parent;
}

inline Ast* 
Scope::GetNode() const
{
  return m_node;
}

#endif // CSCRIPT_SCOPE_H
