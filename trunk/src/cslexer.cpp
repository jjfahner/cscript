//////////////////////////////////////////////////////////////////////////
//
// This file is © 2007 - 2009 JJ Fahner <jan-jaap@jan-jaap.net>
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
#include "cslexer.h"
#include "tokens.h"
#include "eval.h"
#include "lexstream.h"

#include "csparser.gen.h"
#include "cslexer.gen.c"

#include <fstream>

//////////////////////////////////////////////////////////////////////////
//
// CSLexer implementation
//

CSLexer::CSLexer(LexStream& stream) :
m_stream  (stream),
m_strptr  (0),
m_line    (0),
m_string  (0)
{
}


void 
CSLexer::SetText(Char* text)
{
  m_source = text;
  m_strptr = text;
  m_line   = 1;
}

bool
CSLexer::Lex(Token& token)
{
  // String interpolation
  if(m_string == 1 || m_string == 3)
  {
    token.m_type = CS_ADDOP;
    token.m_text = new GCString("+");
    m_string = m_string == 1 ? 2 : 4;
    return true;
  }
  if(m_string == 4)
  {
    m_string = 0;
    return LexString(token);
  }

  // Prepare token
  token.m_type = 0;
  token.m_text = new GCString();

  // Parse next token
  for(;;)
  {
    // End of input
    if(m_stream.Eof())
    {
      token.m_type = CS_EOF;
      return true;
    }

    // Start lexer
    token.m_text->clear();
    m_stream.Start(token.m_text);

    // Parse next token
    token.m_type = ParseNextToken();

    // Depending on token, do specialized parsing
    switch(token.m_type)
    {
    case CS_NEWLINE:    ++m_line; continue;
    case CS_WHITESPACE: continue;
    case CS_LIT_STRING: return LexString(token);
    case CS_SLCOMMENT:  LexComment(CS_SLCOMMENT); continue;
    case CS_MLCOMMENT:  LexComment(CS_MLCOMMENT); continue;
    default:            m_stream.Flush(); break;
    }
 
    // End of embedded variable
    if(m_string == 2)
    {
      if(*m_stream.m_cursor != '}')
      {
        // TODO
        return false;
      }
      ++m_stream.m_cursor;
      m_string = 3;
      token.m_type = CS_IDENTIFIER;
    }

    // Done
    return true;
  }
}

bool 
CSLexer::LexString(Token& token)
{
  // Restart stream
  token.m_type = CS_LIT_STRING;
  m_stream.Start(token.m_text);

  // Parse to end of string
  while(true)
  {
    while(m_stream.m_cursor < m_stream.m_bufend)
    {
      if(*m_stream.m_cursor == '"')
      {
        m_stream.Flush();
        ++m_stream.m_cursor;
        return true; // TODO escape characters
      }
      ++m_stream.m_cursor;
    }
    if(m_stream.FillBuffer() == 0)
    {
      throw std::runtime_error("Unterminated string constant");
    }
  }
// 
// 
//   // Parse string, translating escapes in-place
//   char const*& cur = m_stream.m_cursor;
//   char const*& end = m_stream.m_bufend;
//   for(;; ++cur, )
//   {
//     switch(*m_strptr)
//     {
//     case 0:     
//       throw std::runtime_error("Unterminated string constant");
//     
//     case '"':
//       token.m_size = dst - token.m_text;
//       ++m_strptr;
//       return true;
// 
//     case '\\':
//       switch(wch = *++m_strptr)
//       {
//       case 'r': wch = '\r'; break;
//       case 'n': wch = '\n'; break;
//       case 't': wch = '\t'; break;
//       case 'b': wch = '\b'; break;
//       }
//       *dst = wch;
//       break;
// 
//     case '{':
//       m_string = 1;
//       token.m_size = dst - token.m_text;
//       ++m_strptr;
//       return true;
// 
//     default:
//       *dst = *m_strptr;
//       break;
//     }
//   }
}

bool 
CSLexer::LexComment(int type)
{
  if(type == CS_SLCOMMENT)
  {
    while(true)
    {
      while(m_stream.m_cursor < m_stream.m_bufend)
      {
        if(*m_stream.m_cursor == '\n')
        {
          ++m_stream.m_cursor;
          return true;
        }
        ++m_stream.m_cursor;
      }
      if(m_stream.FillBuffer() == 0)
      {
        return true;
      }
    }
  }
  else
  {
    while(true)
    {
      while(m_stream.m_bufend - m_stream.m_cursor >= 2)
      {
        if(*m_stream.m_cursor == '*' && *(m_stream.m_cursor+1) == '/')
        {
          m_stream.m_cursor += 2;
          return true;
        }
        ++m_stream.m_cursor;
      }
      if(m_stream.FillBuffer() < 2)
      {
        throw std::runtime_error("Unexpected end of file");
      }
    }
  }
}
