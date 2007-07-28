#ifndef CSCRIPT_LEXER_H
#define CSCRIPT_LEXER_H

#include <string>

#include "tokens.h"

class Lexer 
{
public:

  //
  // Construction
  //
  Lexer();

  //
  // Lex from stream
  //
  void SetFile(std::wstring const& filename);

  //
  // Lex from string
  //
  void SetText(std::wstring const& text);

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
  wchar_t*  m_source;
  size_t    m_length;
  wchar_t*  m_strptr;

};

#endif // #ifndef CSCRIPT_LEXER_H
