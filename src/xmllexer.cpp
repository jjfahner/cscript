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

#include "xmlparser.gen.h"
#include "xmllexer.gen.c"

#include <iostream>

XmlLexer::XmlLexer(std::istream& stream) :
m_stream  (stream),
m_bufend  (m_buffer + bufsize),
m_cursor  (m_buffer + bufsize),
m_marker  (0),
m_tokpos  (0),
m_inNode  (false)
{
}

bool 
XmlLexer::AtEof()
{
  // Check buffer
  if(m_cursor != m_bufend)
  {
    return false;
  }

  // Try to fill buffer
  FillInputBuffer(bufsize);

  // Check buffer size again
  return m_cursor == m_bufend;
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
  // Record start pos
  m_tokpos = m_cursor;

  // Create token string buffer
  token.m_text = new GCString();
  m_token = token.m_text;

  // Refill buffer
  if(AtEof())
  {
    token.m_type = XML_EOF;
    return true;
  }

  // Handle node content
  if(!m_inNode)
  {
    // Try to parse a text node
    token.m_type = ParseTextNode();
    if(token.m_type)
    {
      return true;
    }
  }

  // Retrieve next tokn
  token.m_type = ParseNextToken();

  // Append parse data
  m_token->append(m_tokpos, m_cursor - m_tokpos);

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
  while(!foundLt && !AtEof())
  {
    // Remember start position
    char const* start = m_cursor;
    
    // Walk through available data
    while(m_cursor != m_bufend) 
    {
      // Start of next tag
      if(*m_cursor == '<')
      {
        foundLt = true;
        break;
      }
      
      // End of non-empty whitepace
      if(space && !isspace(*m_cursor))
      {
        space = false;
      }

      // Proceeed
      ++m_cursor;
    }
    
    // Append up to here
    m_token->append(start, m_cursor - start);
  }

  // Done
  if(m_token->empty())
  {
    return 0;
  }
  if(space)
  {
    return XML_WS;
  }
  return XML_TEXT;
}

int 
XmlLexer::ParseString()
{
  // Determine quote type and reset
  char quote = m_token->at(0);
  m_token->clear();

  // Parse string
  while(!AtEof())
  {
    // Where to start copying
    char const* ptr = m_cursor;

    // Walk through currently available data
    while(m_cursor != m_bufend) 
    {
      if(*m_cursor == quote)
      {
        break;
      }
      ++m_cursor;
    }

    // Copy into string
    m_token->append(ptr, m_cursor - ptr);

    // End of string
    if(*m_cursor == quote)
    {
      ++m_cursor;
      return XML_STRING;
    }
  }

  // TODO notify unterminated string constant

  // Failed
  return XML_EOF;
}

void 
XmlLexer::FillInputBuffer(size_t size)
{
  // Append currently parsed data to token
  if(m_cursor > m_tokpos)
  {
    m_token->append(m_tokpos, m_cursor - m_tokpos);
  }

  // Move remainder to start of buffer
  if(m_cursor < m_bufend)
  {
    size_t avail = m_bufend - m_cursor;
    memmove(m_buffer, m_cursor, avail);
    m_cursor = m_buffer;
    m_bufend = m_cursor + avail;
  }
  else
  {
    m_cursor = m_buffer;
    m_bufend = m_buffer;
  }

  // Reset token start
  m_tokpos = m_buffer;

  // Check for end of file
  if(m_stream.eof())
  {
    return;
  }

  // Read from stream
  size_t offset = m_bufend - m_buffer;
  size_t length = bufsize - offset;
  m_stream.read(m_buffer + offset, length);

  // Determine new buffer end
  m_bufend += m_stream.gcount();
}
