#ifndef CSCRIPT_LEXER_H
#define CSCRIPT_LEXER_H

#include "types.h"

struct Token;

class Lexer 
{
public:

  //
  // Construction
  //
  Lexer();

  //
  // Lex from string
  //
  void SetText(char* text);

  //
  // Fetch next token
  //
  bool Lex(Token& token);

private:

  //
  // Specialized lexing
  //
  bool LexString(Token& token);

  //
  // Members
  //
  Char*  m_source;
  size_t    m_length;
  Char*  m_strptr;

};

#endif // #ifndef CSCRIPT_LEXER_H
