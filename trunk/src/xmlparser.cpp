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
#include <object.h>

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

Object*
XmlParser::Parse(std::istream& is)
{
  // Create lexer
  XmlLexer lexer(is);

  // Allocate parser
  XmlParserImpl parser(this);

  // Start document
  startDocument();

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

  // Finish document
  endDocument();

  // Done
  return m_document;
}

//////////////////////////////////////////////////////////////////////////
//
// Error handlers
//

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

//////////////////////////////////////////////////////////////////////////
//
// Content handlers
//

void 
XmlParser::startDocument()
{
  // Create document
  m_document = new Object();
  (*m_document)["nodeTypeName"] = "#document";
  (*m_document)["nodeName"]     = "#document";
  (*m_document)["childNodes"]   = new Object();
  (*m_document)["attributes"]   = new Object();

  // Set as current node
  m_curNode = m_document;
}

void
XmlParser::endDocument()
{
  if(m_curNode != m_document)
  {
    throw std::runtime_error("Invalid document structure");
  }
}

//#define XMLPARSER_DEBUG

void 
XmlParser::processingInstruction(XmlName const& name)
{
#ifdef XMLPARSER_DEBUG
  std::cout << "<?" << *name.m_localName << "?>";
#endif

  Object* node = new Object();
  (*node)["nodeTypeName"] = "#processing-instruction";
  (*node)["nodeName"]     = "#processing-instruction";
  (*node)["target"]       = name.m_localName;
  (*node)["attributes"]   = new Object();

  (*m_curNode)["childNodes"].GetObject()->Add(node);
}

void 
XmlParser::startElement(XmlName const& name)
{
#ifdef XMLPARSER_DEBUG
  std::cout << "<" << *name.m_localName << ">";
#endif

  // Create element
  Object* node = new Object();
  (*node)["nodeTypeName"]   = "#element";
  (*node)["nodeName"]       = name.m_localName;
  (*node)["localName"]      = name.m_localName;
  (*node)["qualifiedName"]  = name.m_localName;
  (*node)["namespace"]      = name.m_namespace;
  (*node)["childNodes"]     = new Object();
  (*node)["attributes"]     = new Object();

  // Add to parent node
  (*node)["parentNode"] = m_curNode;
  (*m_curNode)["childNodes"].GetObject()->Add(node);

  // Set as current element
  m_curNode = node;
}

void 
XmlParser::endElement(XmlName const& name)
{
#ifdef XMLPARSER_DEBUG
  std::cout << "</" << *name.m_localName << ">";
#endif

  // Check node name
  String nodeName = (*m_curNode)["localName"].GetString();
  if(nodeName != *name.m_localName)
  {
    throw std::runtime_error("Invalid document structure");
  }

  // Move back up in tree
  m_curNode = (*m_curNode)["parentNode"].GetObject();
}

void 
XmlParser::ignorableWhitespace(GCString* text)
{
#ifdef XMLPARSER_DEBUG
  std::cout << *text;
#endif

  Object* node = new Object();
  (*node)["nodeTypeName"] = "#text";
  (*node)["nodeName"]     = "#text";
  (*node)["data"]         = text;

  (*m_curNode)["childNodes"].GetObject()->Add(Value(text));
}

void 
XmlParser::characters(GCString* text)
{
#ifdef XMLPARSER_DEBUG
  std::cout << *text;
#endif

  Object* node = new Object();
  (*node)["nodeTypeName"] = "#text";
  (*node)["nodeName"]     = "#text";
  (*node)["data"]         = text;

  (*m_curNode)["childNodes"].GetObject()->Add(Value(text));
}
