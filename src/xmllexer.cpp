//////////////////////////////////////////////////////////////////////////
//
// This file is © 2009 JJ Fahner <jan-jaap@jan-jaap.net>
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
#include <cscript.h>
#include <xmllexer.h>
#include <lexstream.h>
#include <ctype.h>

#include "xmlparser.gen.h"
#include "xmllexer.gen.c"

XmlLexer::XmlLexer(LexStream& stream) :
m_stream  (stream),
m_inNode  (false)
{
}

void 
XmlLexer::SetState(XmlLexerStates state)
{
  m_state = state;
}

bool 
XmlLexer::Eof()
{
  return m_stream.Eof();
}

//#define XMLPARSER_DEBUG

bool 
XmlLexer::Lex(XmlToken& token)
{
  bool result = LexImpl(token);
#ifdef XMLPARSER_DEBUG
  std::cout << "Lexer: " << token.m_type << "'" << *token.m_text << "'\n";
#endif
  return result;
}

bool
XmlLexer::LexImpl(XmlToken& token)
{
  // Create token string buffer
  token.m_type = 0;
  token.m_text = new GCString();
  m_token = token.m_text;

  // End of input
  if(m_stream.Eof())
  {
    token.m_type = XML_EOF;
    return true;
  }

  // Start next token
  m_stream.Start(m_token);

  // Try to parse a text node
  if(!m_inNode)
  {
    token.m_type = ParseTextNode();
  }

  // Retrieve next tokon
  if(token.m_type == 0)
  {
    token.m_type = ParseNextToken();
    m_stream.Flush();
  }

  // Do special handling
  switch(token.m_type)
  {
    // Start of element
  case XML_DECL:
  case XML_PRO_I:
  case XML_LT_SL:
  case XML_LT:
    m_inNode = true;
    break;
  
    // End of element
  case XML_PRO_E:
  case XML_SL_GT:
  case XML_GT:
    m_inNode = false;
    break;

    // Whitespace
  case XML_WS:
    if(m_inNode) return Lex(token);
    break;

    // Strings
  case XML_STRING:
    token.m_type = ParseString();
    break;
  }

  // Done
  return true;
}

int 
XmlLexer::ParseTextNode()
{
  bool space = true;
  bool foundLt = false;
  while(!foundLt)
  {
    if(m_stream.FillBuffer() == 0)
    {
      break;
    }
    
    while(m_stream.m_cursor != m_stream.m_bufend) 
    {
      // Start of next tag
      if(*m_stream.m_cursor == '<')
      {
        foundLt = true;
        break;
      }
      
      // End of non-empty whitepace
      if(space && !isspace(*m_stream.m_cursor))
      {
        if(m_state == xmllsEpilog)
        {
          m_stream.Start();
          return XML_EOF;
        }
        space = false;
      }

      // Proceeed
      ++m_stream.m_cursor;
    }
  }

  // Flush rest of token
  m_stream.Flush();

  // Empty token
  if(m_token->empty())
  {
    return 0;
  }

  // Only whitespace
  if(space)
  {
    return XML_WS;
  }

  // XML text
  return XML_TEXT;
}

int 
XmlLexer::ParseString()
{
  // Determine quote type and reset
  char quote = m_token->at(0);
  m_token->clear();

  // Parse string
  while(!m_stream.Eof())
  {
    // Where to start copying
    char const* ptr = m_stream.m_cursor;

    // Walk through currently available data
    while(m_stream.m_cursor != m_stream.m_bufend) 
    {
      if(*m_stream.m_cursor == quote)
      {
        break;
      }
      ++m_stream.m_cursor;
    }

    // End of string
    if(*m_stream.m_cursor == quote)
    {
      m_stream.Flush();
      ++m_stream.m_cursor;
      return XML_STRING;
    }
  }

  // TODO notify unterminated string constant

  // Failed
  return XML_EOF;
}
