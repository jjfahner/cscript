#ifndef CSCRIPT_PARSE_H
#define CSCRIPT_PARSE_H

#include "types.h"
#include "ast.h"

class File;
class Lexer;
class Reporter;

class Parser
{
public:

  //
  // Construction
  //
  Parser(Reporter& reporter);

  //
  // Destruction
  //
  ~Parser();

  //
  // Parse a file
  //
  void Parse(File& file);
  void Parse(String const& filename);

  //
  // Root node
  //
  Ast* GetRoot() const;
  void SetRoot(Ast* root);

  //
  // Allocate node
  //
  Ast* AllocAst(AstTypes type);
  Ast* AllocAst(AstTypes type, AstData const& a1);
  Ast* AllocAst(AstTypes type, AstData const& a1, AstData const& a2);
  Ast* AllocAst(AstTypes type, AstData const& a1, AstData const& a2, AstData const& a3);
  Ast* AllocAst(AstTypes type, AstData const& a1, AstData const& a2, AstData const& a3, AstData const& a4);

  //
  // Error handlers
  //
  void OnParseFailure();
  void OnSyntaxError();

private:

  //
  // Members
  //
  Ast*                m_root;
  Reporter&           m_reporter;
  File*               m_file;
  Lexer*              m_lexer;

};

#endif // CSCRIPT_PARSE_H
