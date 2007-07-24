#define _CRT_SECURE_NO_WARNINGS
#include "lexer.h"
#include "tokens.h"
#include "lexer.gen"

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
    // End of file
    if(stream.eof())
    {
      break;
    }

    // Read line
    stream.getline(line, 4000);
    if(stream.bad())
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

  // Copy into token
  token.m_text = start;
  token.m_size = end - start;

  // Cut off string start/end
  if(token.m_type == TOK_STRING)
  {
    token.m_text += 1;
    token.m_size -= 2;
  }

  // Move pointer
  m_strptr = end;

  // Succeeded
  return true;
}
