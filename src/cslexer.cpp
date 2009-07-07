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
#include "value.h"
#include "tokens.h"
#include "lexstream.h"
#include "eval.h"

#include "csparser.gen.h"
#include "cslexer.gen.c"

#include <fstream>

//////////////////////////////////////////////////////////////////////////
//
// CSLexer implementation
//

CSLexer::CSLexer(LexStream& stream) :
m_stream  (stream),
m_string  (0),
m_regex   (0)
{
}

void
CSLexer::AfterBinop()
{
  --m_regex;
}

bool
CSLexer::Lex(Token& token)
{
  // String interpolation
  if(m_string == 1 || m_string == 3)
  {
    token.m_type = CS_ADDOP;
    token.m_text = GCString::Create("+");
    m_string = m_string == 1 ? 2 : 4;
    return true;
  }
  if(m_string == 4)
  {
    m_string = 0;
    token.m_text = GCString::Create();
    return LexString(token);
  }

  // Regex char
  if(m_regex > 0 && LexRegex(token))
  {
    return true;
  }
  m_regex = 0;

  // Prepare token
  token.m_type = 0;
  token.m_text = GCString::Create();

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
    case CS_NEWLINE:    /*++m_line; */continue;
    case CS_WHITESPACE: continue;
    case CS_LIT_STRING: return LexString(token);
    case CS_SLCOMMENT:  LexComment(CS_SLCOMMENT); continue;
    case CS_MLCOMMENT:  LexComment(CS_MLCOMMENT); continue;
    case CS_DIVOP:      ++m_regex;
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
  m_stream.Start();

  // Parse string, translating escapes in-place
  char const*& cur = m_stream.m_cursor;
  char const*& end = m_stream.m_bufend;
  char wch;
  while(true)
  {
    while(end - cur > 1)
    {
      switch(*cur)
      {
      case '"':
        ++cur;
        return true;

      case '\\':
        switch(wch = *++cur)
        {
        case 'r': wch = '\r'; break;
        case 'n': wch = '\n'; break;
        case 't': wch = '\t'; break;
        case 'b': wch = '\b'; break;
        }
        *token.m_text += wch;
        break;

      case '{':
        m_string = 1;
        ++cur;
        return true;

      default:
        *token.m_text += *cur;
        break;
      }

      ++cur;
    }

    m_stream.m_marker = m_stream.m_cursor;
    if(m_stream.FillBuffer(2) < 2)
    {
      throw std::runtime_error("Unterminated string constant");
    }
  }
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
      m_stream.m_marker = m_stream.m_cursor;
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
      m_stream.m_marker = m_stream.m_cursor;
      if(m_stream.FillBuffer() < 2)
      {
        throw std::runtime_error("Unexpected end of file");
      }
    }
  }
}

bool 
CSLexer::LexRegex(Token& token)
{
  // Retrieve first character from stream
  char ch = *m_stream.m_cursor++;

  // Ref token members
  int&    type =  token.m_type;
  String& text = *token.m_text;

  // Find character code
  type = CS_REGEX_CHAR;
  switch(ch)
  {
  case '/':  type = CS_DIVOP; m_regex = 0; break;
  case '(':  type = CS_LPAREN; break;
  case ')':  type = CS_RPAREN; break;
  case '[':  type = CS_LBRACKET; ++m_regex; break;
  case ']':  type = CS_RBRACKET; --m_regex; break;
  case '{':  type = CS_LBRACE; break;
  case '}':  type = CS_RBRACE; break;
  case '.':  type = CS_REGEX_ANY; break;
  case '*':  type = CS_REGEX_ZERO_OR_MORE; break;
  case '?':  type = CS_REGEX_ZERO_OR_ONE; break;
  case '+':  type = CS_REGEX_ONE_OR_MORE; break;
  case '^':  type = CS_REGEX_ANCHOR_LEFT; break;
  case '$':  type = CS_REGEX_ANCHOR_RIGHT; break;
  case '-':  if(m_regex == 2) { type = CS_SUBOP; break; }
  }

  // Complete token
  
  if(m_regex == 0)
  {
    text = text.substr(1, text.length() - 1);
  }
  else
  {
    text += ch;
  }
  return true;
}
