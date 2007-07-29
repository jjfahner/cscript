#define _CRT_SECURE_NO_WARNINGS
#include "lexer.h"
#include "tokens.h"
#include "lexer.gen"

#include <fstream>

//////////////////////////////////////////////////////////////////////////
//
// Lexer implementation
//

Lexer::Lexer() :
m_strptr (0)
{
}


void 
Lexer::SetText(wchar_t* text)
{
  m_source = text;
  m_strptr = text; 
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
  wchar_t* start = m_strptr;
  wchar_t* end   = start;
  int type = parseNextToken(start, end);
  
  // Depending on token, do specialized parsing
  switch(type)
  {
  case 0:          return false;
  case TOK_STRING: m_strptr = end; return LexString(token);
  }

  // Copy into token
  token.m_type = type;
  token.m_text = start;
  token.m_size = end - start;

  // Move pointer
  m_strptr = end;

  // Succeeded
  return true;
}

bool 
Lexer::LexString(Token& token)
{
  wchar_t* dst = m_strptr;
  wchar_t  wch;

  // Init token
  token.m_type = TOK_STRING;
  token.m_text = m_strptr;
  token.m_size = 0;

  // Parse string, translating escapes in-place
  for(;; ++m_strptr, ++dst)
  {
    switch(*m_strptr)
    {
    case 0:     
      throw std::runtime_error("Unterminated string constant");
    
    case '"':
      token.m_size = dst - token.m_text;
      ++m_strptr;
      return true;

    case '\\':
      switch(wch = *++m_strptr)
      {
      case 'r': wch = '\r'; break;
      case 'n': wch = '\n'; break;
      case 't': wch = '\t'; break;
      case 'b': wch = '\b'; break;
      }
      *dst = wch;
      break;

    default:
      *dst = *m_strptr;
      break;
    }
  }
}