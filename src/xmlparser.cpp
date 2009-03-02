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
#include <xmlparser.h>
#include <xmllexer.h>

#include <iostream>

#include "xmlparser.gen.h"
#include "xmlparser.gen.c"

//////////////////////////////////////////////////////////////////////////
//
// Lemon parser wrapper
//

class XmlParserImpl
{
public:

  //
  // Allocate parser
  //
  XmlParserImpl(XmlParser* pParser, bool debug = false) :
  m_pParser (pParser),
  m_hParser (0)
  {
    m_hParser = XmlParseAlloc(malloc);
    if(debug)
    {
      XmlParseTrace(stdout, "XmlParse: ");
    }
  }

  //
  // Free parser
  //
  ~XmlParserImpl()
  {
    if(m_hParser)
    {
      XmlParseFree(m_hParser, free);
    }
  }

  //
  // Parse next token
  //
  void operator () (int type, XmlToken token)
  {
    XmlParse(m_hParser, type, token, m_pParser);
  }

  //
  // Flush parser
  //
  void operator () ()
  {
    XmlParse(m_hParser, 0, XmlToken(), m_pParser);
  }

private:

  XmlParser*  m_pParser;
  void*       m_hParser;

};

//////////////////////////////////////////////////////////////////////////
//
// XmlParser implementation 
//

void 
XmlParser::Parse(std::istream& is)
{
  // Create lexer
  XmlLexer lexer(is);

  // Allocate parser
  XmlParserImpl parser(this);

  // Parse tokens
  XmlToken token;
  while(lexer.Lex(token))
  {
    // End of input
    if(token.m_type == XML_EOF)
    {
      break;
    }

    // Push token to parser
    parser(token.m_type, token);
  }

  // Empty token to finalize parse
  parser();
}

void 
XmlParser::OnParseFailure()
{
  throw std::runtime_error("XmlParser: Failed to parse input");
}

void 
XmlParser::OnSyntaxError()
{
  throw std::runtime_error("XmlParser: Syntax error");
}
