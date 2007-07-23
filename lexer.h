#ifndef CSCRIPT_LEXER_H
#define CSCRIPT_LEXER_H

#include <string>
#include <iosfwd>

//////////////////////////////////////////////////////////////////////////
//
// Token
//

struct Token
{
  //
  // Construction
  //
  Token();

  //
  // Clear members
  //
  void clear();

  //
  // Add character to text
  //
  Token& operator += (wchar_t);

  //
  // Members
  //
  int           m_type;
  std::wstring  m_text;
  size_t        m_line;
  size_t        m_char;

};

//////////////////////////////////////////////////////////////////////////
//
// Lexer
//

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
