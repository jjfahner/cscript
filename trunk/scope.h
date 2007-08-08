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

class Scope 
{
public:

  //
  // Construction
  //
  Scope(Reporter& reporter, Ast* node, Scope* parent);

  //
  // Register a name
  //
  int DeclareParameter(String const& name);

  //
  // Declare a name
  //
  int DeclareVariable(String const& name);

  //
  // Find a name
  //
  int Lookup(String const& name, bool& global) const;

private:

  int MakeParameterId();

  int MakeVariableId();

  typedef std::map<String, int> Names;

  //
  // Members
  //
  Reporter&   m_reporter;
  Scope*      m_parent;
  Ast*        m_node;
  Names       m_names;

};

#endif // CSCRIPT_SCOPE_H
