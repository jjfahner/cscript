#ifndef CSCRIPT_AST_H
#define CSCRIPT_AST_H

#include "types.h"
#include "any.h"

typedef std::vector<any> AnyVec;

class Ast;

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
  // Set the root node
  //
  void SetRoot(Ast* root);

  //
  // Enumerate contents of ast node
  //
  void Enumerate(Ast* node);

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
  integer,
  real,
  string,
  boolean,
  null,
  identifier,
  argument_list,
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
  Ast(int type);
  Ast(int type, any const& a1);
  Ast(int type, any const& a1, any const& a2);
  Ast(int type, any const& a1, any const& a2, any const& a3);

  //
  // Destruction
  //
  ~Ast();


  //
  // Members
  //
  int     m_type;
  any     m_a1;
  any     m_a2;
  any     m_a3;

};

//////////////////////////////////////////////////////////////////////////
//
// Constructor implementation
//

inline 
Ast::Ast(int type) :
m_type (type)
{
}

inline 
Ast::Ast(int type, any const& a1) :
m_type (type),
m_a1   (a1)
{
}

inline 
Ast::Ast(int type, any const& a1, any const& a2) :
m_type (type),
m_a1   (a1),
m_a2   (a2)
{
}

inline 
Ast::Ast(int type, any const& a1, any const& a2, any const& a3) :
m_type (type),
m_a1   (a1),
m_a2   (a2),
m_a3   (a3)
{
}

inline 
Ast::~Ast()
{
}

#endif // CSCRIPT_AST_H
