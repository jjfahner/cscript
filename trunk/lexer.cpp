#define _CRT_SECURE_NO_WARNINGS
#include "lexer.h"
#include "tokens.h"
#include "lexer.gen"

//////////////////////////////////////////////////////////////////////////
//
// Token implementation
//

Token::Token()
{
  clear();
}

void 
Token::clear()
{
  m_text.clear();
  m_type = 0;
  m_line = 0;
  m_char = 0;
}

Token& 
Token::operator += (wchar_t wch)
{
  m_text += wch;
  return *this;
}

//////////////////////////////////////////////////////////////////////////
//
// Lexer implementation
//

Lexer::Lexer(std::wistream& stream) :
m_strptr (0)
{
  // Read first byte to check file
  stream.peek();
  if(!stream.good())
  {
    throw std::runtime_error("Failed to read from file");
  }

  // Read source string
  wchar_t line[4000];
  for(;;)
  {
    // Read line
    stream.getline(line, 4000);
    if(!stream.good())
    {
      break;
    }

    // Append newline
    wcscat(line, L"\n");

    // Append to input
    m_source += line;
  }

  // Point to offset of string
  m_strptr = m_source.c_str();
}

bool
Lexer::Lex(Token& token)
{
  // Reset token
  token.clear();

  // End of input
  if(*m_strptr == 0)
  {
    return false;
  }

  // Parse next token
  wchar_t const* start = m_strptr;
  wchar_t const* end   = start;
  token.m_type = parseNextToken(start, end);
  if(token.m_type == 0)
  {
    return false;
  }

  // Resize string buffer
  size_t len = end - start;
  token.m_text.resize(len + 1, 0);

  // Copy string into buffer
  wcsncpy(const_cast<wchar_t*>(token.m_text.c_str()), start, len);

  // Move pointer
  m_strptr = end;

  // Succeeded
  return true;
}
