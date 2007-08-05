//////////////////////////////////////////////////////////////////////////
//
// This file is � 2007 JJ Fahner <jan-jaap@jan-jaap.net>
// This file is part of the cscript interpreter.
// CScript can be found at http://svn.jan-jaap.net/
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//
//////////////////////////////////////////////////////////////////////////
#include "lexer.h"
#include "tokens.h"
#include "lexer.gen"

#include <fstream>

//////////////////////////////////////////////////////////////////////////
//
// Lexer implementation
//

Lexer::Lexer() :
m_strptr  (0),
m_line    (0)
{
}


void 
Lexer::SetText(Char* text)
{
  m_source = text;
  m_strptr = text;
  m_line   = 1;
}

bool
Lexer::Lex(Token& token)
{
  for(;;)
  {
    // End of input
    if(m_strptr == 0 || *m_strptr == 0)
    {
      return false;
    }

    // Parse next token
    Char* start = m_strptr;
    Char* end   = start;
    int type = parseNextToken(start, end);
    
    // Depending on token, do specialized parsing
    switch(type)
    {
    case 0:               return false;
    case TOK_WHITESPACE:  m_strptr = end; continue;
    case TOK_STRING:      m_strptr = end; return LexString(token);
    case TOK_NEWLINE:     m_strptr = end; ++m_line; continue;
    case TOK_COMMENT:     LexComment(); continue;
    }

    // Copy into token
    token.m_type = type;
    token.m_text = start;
    token.m_size = end - start;

    //std::cout << "'" << std::string(token.m_text, token.m_size) << "'  ";

    // Move pointer
    m_strptr = end;
    return true;
  }
}

bool 
Lexer::LexString(Token& token)
{
  Char* dst = m_strptr;
  Char  wch;

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

      // TODO
//     case '{':
//       while(++m_strptr)
//       {
// 
//       }

    default:
      *dst = *m_strptr;
      break;
    }
  }
}

bool 
Lexer::LexComment()
{
  // Single-line comment
  if(*++m_strptr == '/')
  {
    while(*m_strptr && *m_strptr != '\n')
    {
      ++m_strptr;
    }
  }
  else // Multiline comment
  {
    while(*m_strptr)
    {
      if(*m_strptr == '\n')
      {
        ++m_line;
      }
      else if(*m_strptr == '*' && *(m_strptr+1) == '/')
      {
        m_strptr += 2;
        break;
      }
      ++m_strptr;
    }
  }
  return true;
}
