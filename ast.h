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
  Quad      m_parcount;     // Number of declared parameters
  int       m_stackpos;     // Offset against stack frame (may be negative)

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
m_parcount(0),
m_stackpos(0)
{
}

inline 
Ast::Ast(AstTypes type, any const& a1) :
m_type (type),
m_idempotent (false),
m_varcount(0),
m_parcount(0),
m_stackpos(0),
m_a1   (a1)
{
}

inline 
Ast::Ast(AstTypes type, any const& a1, any const& a2) :
m_type (type),
m_idempotent (false),
m_varcount(0),
m_parcount(0),
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
m_parcount(0),
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
m_parcount(0),
m_stackpos(0),
m_a1   (a1),
m_a2   (a2),
m_a3   (a3),
m_a4   (a4)
{
}


#endif // CSCRIPT_AST_H
