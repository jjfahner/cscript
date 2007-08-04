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

#include "types.h"
#include "any.h"
#include "var.h"

#include "opcodes.h"

class Ast;
typedef std::vector<any> AnyVec;
typedef std::list<Ast*> AstList;

//////////////////////////////////////////////////////////////////////////
//
// Ast node types
//

enum AstTypes
{
  invalid,
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
  compound_statement
};

//////////////////////////////////////////////////////////////////////////
//
// Ast generator class
//

class AstGen
{
public:

  //
  // Entry point
  //
  static int main(int argc, char** argv);

  //
  // Construction
  //
  AstGen();

  //
  // Parse a file
  //
  void Parse(String const& filename);

  //
  // Root node
  //
  Ast* GetRoot() const;
  void SetRoot(Ast* root);

  //
  // Error handlers
  //
  void OnParseFailure();
  void OnSyntaxError();

private:

  //
  // Members
  //
  Ast* m_root;

};

//////////////////////////////////////////////////////////////////////////
//
// Ast node class
//

class Ast 
{
public:

  //
  // Construction
  //
  Ast(AstTypes type);
  Ast(AstTypes type, any const& a1);
  Ast(AstTypes type, any const& a1, any const& a2);
  Ast(AstTypes type, any const& a1, any const& a2, any const& a3);
  Ast(AstTypes type, any const& a1, any const& a2, any const& a3, any const& a4);

  //
  // Ast structure
  //
  AstTypes  m_type;
  any       m_a1;
  any       m_a2;
  any       m_a3;
  any       m_a4;

  //
  // Annotations
  //
  bool      m_idempotent;   // Content is idempotent
  Quad      m_varcount;     // Number of declared variables
  Quad      m_argcount;     // Number of argument in call
  Quad      m_parcount;     // Number of declared parameters
  Quad      m_framesize;    // Size of stack frame
  int       m_stackpos;     // Offset against stack frame (may be negative for parameters)

};

//////////////////////////////////////////////////////////////////////////
//
// Constructor implementation
//

inline 
Ast::Ast(AstTypes type) :
m_type (type),
m_idempotent(false),
m_varcount(0),
m_argcount(0),
m_parcount(0),
m_framesize(0),
m_stackpos(0)
{
}

inline 
Ast::Ast(AstTypes type, any const& a1) :
m_type (type),
m_idempotent (false),
m_varcount(0),
m_argcount(0),
m_parcount(0),
m_framesize(0),
m_stackpos(0),
m_a1   (a1)
{
}

inline 
Ast::Ast(AstTypes type, any const& a1, any const& a2) :
m_type (type),
m_idempotent (false),
m_varcount(0),
m_argcount(0),
m_parcount(0),
m_framesize(0),
m_stackpos(0),
m_a1   (a1),
m_a2   (a2)
{
}

inline 
Ast::Ast(AstTypes type, any const& a1, any const& a2, any const& a3) :
m_type (type),
m_idempotent (false),
m_varcount(0),
m_argcount(0),
m_parcount(0),
m_framesize(0),
m_stackpos(0),
m_a1   (a1),
m_a2   (a2),
m_a3   (a3)
{
}

inline 
Ast::Ast(AstTypes type, any const& a1, any const& a2, any const& a3, any const& a4) :
m_type (type),
m_idempotent (false),
m_varcount(0),
m_argcount(0),
m_parcount(0),
m_framesize(0),
m_stackpos(0),
m_a1   (a1),
m_a2   (a2),
m_a3   (a3),
m_a4   (a4)
{
}


#endif // CSCRIPT_AST_H
