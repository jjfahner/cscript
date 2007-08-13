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
#ifndef CSCRIPT_AST_H
#define CSCRIPT_AST_H

#include "props.h"
#include "astdata.h"
#include "opcodes.h"
#include "any.h"

class Ast;
class File;

//////////////////////////////////////////////////////////////////////////
//
// Ast node types
//

enum AstTypes
{
  invalid,
  translation_unit,
  statement_sequence,
  expression_statement,
  assignment_expression,
  binary_expression,
  ternary_expression,
  prefix_expression,
  postfix_expression,
  member_expression,
  index_expression,
  function_call,
  literal,
  lvalue,
  list_literal,
  list_content,
  list_entry,
  argument_list,
  argument,
  function_declaration,
  parameter,
  parameter_list,
  variable_declaration,
  declaration_sequence,
  empty_statement,
  include_statement,
  for_statement,
  foreach_statement,
  if_statement,
  while_statement,
  return_statement,
  compound_statement,
  break_statement,
  continue_statement,
  switch_statement,
  switch_body,
  switch_case,
  default_case,
  struct_declaration,
  struct_members,
  struct_member,
  new_expression,
};

//////////////////////////////////////////////////////////////////////////
//
// Ast base type
//

class Ast 
{
public:

  typedef std::map<String, Quad> Annotations;

  //
  // Construction
  //
  Ast(AstTypes type);
  Ast(AstTypes type, AstData const& a1);
  Ast(AstTypes type, AstData const& a1, AstData const& a2);
  Ast(AstTypes type, AstData const& a1, AstData const& a2, AstData const& a3);
  Ast(AstTypes type, AstData const& a1, AstData const& a2, AstData const& a3, AstData const& a4);

  //
  // Destructor
  //
  virtual ~Ast();

  //
  // Ast structure
  //
  AstTypes  m_type;
  AstData   m_a1;
  AstData   m_a2;
  AstData   m_a3;
  AstData   m_a4;

  //
  // Annotations
  //
  typedef PropertiesT<Quad> Properties;
  Properties m_props;

  //
  // Refcount
  //
  friend class AstData;
  int m_refs;

  //
  // File position
  //
  FilePos   m_pos;

};

//////////////////////////////////////////////////////////////////////////
//
// Ast-derived types
//

class SwitchStatement : public Ast
{
public:

  //
  // Construction
  //
  SwitchStatement(AstData const& a1, AstData const& a2) :
  Ast(switch_statement, a1, a2)
  {
  }

  //
  // Members
  //
  AstList* m_breaks;

};


#endif // CSCRIPT_AST_H
