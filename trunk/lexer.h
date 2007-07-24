#ifndef CSCRIPT_LEXER_H
#define CSCRIPT_LEXER_H

#include <string>
#include <iosfwd>

#include "tokens.h"

class Lexer 
{
public:

  //
  // Construction
  //
  Lexer(std::wistream& stream);

  //
  // Fetch next token
  //
  bool Lex(Token& token);

private:

  //
  // Members
  //
  std::wstring    m_source;
  wchar_t const*  m_strptr;

};

#endif // #ifndef CSCRIPT_LEXER_H
