//////////////////////////////////////////////////////////////////////////
//
// This file is © 2009 - 2011 JJ Fahner <jan-jaap@jan-jaap.net>
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
#include <lexstream.h>
#include <lemon.h>
#include <iostream>
#include <scriptobj.h>

#include "xmlparser.gen.h"
#include "xmlparser.gen.c"

//////////////////////////////////////////////////////////////////////////

enum XmlNodeTypes
{
  xmlUnknown = 0,
  xmlElement = 1,
  xmlAttribute = 2, 	
  xmlText = 3,
  xmlCDATASection = 4,
  xmlEntityReference = 5, 	
  xmlEntity = 6,
  xmlProcessingInstruction = 7,
  xmlComment = 8,
  xmlDocument = 9,
  xmlDocumentType = 10,
  xmlDocumentFragment = 11,
  xmlNotation = 12
};

static const Value
XmlNodeName(XmlParser::XmlNodeTypes nodeType)
{
  switch(nodeType)
  {
  case xmlElement:                return "#element";
  case xmlAttribute:              return "#attribute";
  case xmlText:                   return "#text";
  case xmlCDATASection:           return "#cdata-section";
  case xmlEntityReference:        return "#entity-reference";
  case xmlEntity:                 return "#entity";
  case xmlProcessingInstruction:  return "#processing-instruction";
  case xmlComment:                return "#comment";
  case xmlDocument:               return "#document";
  case xmlDocumentType:           return "#document-type";
  case xmlDocumentFragment:       return "#document-fragment";
  case xmlNotation:               return "#notation";
  default:                        throw std::runtime_error("Invalid XML node type");
  }
}

//////////////////////////////////////////////////////////////////////////
//
// Lemon parser wrapper
//

void XmlParseTraceDummy(FILE*, char*) {}
typedef LemonParser<
  XmlParser, 
  XmlToken, 
  XmlParseAlloc, 
  XmlParseFree, 
#ifdef _DEBUG
  XmlParseTrace, 
#else
  XmlParseTraceDummy,
#endif
  XmlParse> XmlParserImpl;

//////////////////////////////////////////////////////////////////////////
//
// XmlParser implementation 
//

Object*
XmlParser::Parse(LexStream& stream)
{
  // Create lexer
  XmlLexer lexer(stream);
  m_lexer = &lexer;

  // Allocate parser
  XmlParserImpl parser(this, "XML Parser: ");

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
  throw std::runtime_error("XmlParser: Parse error");
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

Object*
XmlParser::createNode(XmlNodeTypes type)
{
  Object* node = new ScriptObject();
  node->Set("ownerDocument",  m_document);
  node->Set("nodeType",  (int)type);
  node->Set("nodeName",  XmlNodeName(type));
  node->Set("nodeTypeName",  XmlNodeName(type));

  // Add child nodes and attributes
  switch(type)
  {
  case xmlDocument:
  case xmlDocumentFragment:
  case xmlElement:
    node->Set("childNodes",  new ScriptObject());
    node->Set("attributes",  new ScriptObject());
    break;
  }

  // Attach to parent node
  if(m_curNode)
  {
    node->Set("parentNode",  m_curNode);
    if(type == xmlAttribute)
    {
      // FIXME
      //(*m_curNode)["attributes"].GetObject()->Add(node);
    }
    else
    {
      // FIXME
      //(*m_curNode)["childNodes"].GetObject()->Add(node);
    }
  }

  // Done
  ++m_nodeCount;
  return node;
}

void 
XmlParser::startDocument()
{
  // Initialize state
  m_document  = 0;
  m_curNode   = 0;
  m_rootNode  = 0;
  m_nodeCount = 0;

  // Create document
  m_document = createNode(xmlDocument);

  // Set as current node
  m_curNode = m_document;

  // Set lexer state
  m_lexer->SetState(xmllsElement);
}

void
XmlParser::endDocument()
{
  // Check that we've reached the root
  if(m_curNode != m_document)
  {
    throw std::runtime_error("Invalid document structure");
  }

  // Store node count
  m_document->Set("nodeCount",  m_nodeCount);
}

//#define XMLPARSER_DEBUG

void 
XmlParser::processingInstruction(XmlName const& target, GCString* data)
{
#ifdef XMLPARSER_DEBUG
  std::cout << "<?" << *target.m_localName << "?>";
#endif

  // Create processing instruction
  Object* node = createNode(xmlProcessingInstruction);
  node->Set("target",  target.m_localName);
  node->Set("data",  data);
}

void 
XmlParser::startElement(XmlName const& name)
{
#ifdef XMLPARSER_DEBUG
  std::cout << "<" << *name.m_localName << ">";
#endif

  // Create element
  Object* node = createNode(xmlElement);
  node->Set("localName",  name.m_localName);
  node->Set("qualifiedName",  name.m_localName);
  node->Set("namespace",  name.m_namespace);

  // Set as current element
  m_curNode = node;

  // Set root node
  if(m_rootNode == 0)
  {
    m_rootNode = node;
    m_document->Set("documentElement",  node);
  }

  // Set lexer state
  m_lexer->SetState(xmllsElement);
}

void 
XmlParser::endElement(XmlName const& name)
{
#ifdef XMLPARSER_DEBUG
  std::cout << "</" << *name.m_localName << ">";
#endif

  // Check node name
  String nodeName = m_curNode->Get("localName");
  if(nodeName != *name.m_localName)
  {
    throw std::runtime_error("Invalid document structure");
  }

  // Move back up in tree
  m_curNode = m_curNode->Get("parentNode");

  // Root node
  if(m_curNode == m_rootNode)
  {
    m_lexer->SetState(xmllsEpilog);
  }
}

void 
XmlParser::attribute(XmlName const& name, GCString* value)
{
#ifdef XMLPARSER_DEBUG
  std::cout << " " << *name.m_localName << "=" << *value;
#endif

  // Add attribute to current node
  Object* node = createNode(xmlAttribute);
  node->Set("localName",  name.m_localName);
  node->Set("qualifiedName",  name.m_localName);
  node->Set("namespace",  name.m_namespace);
  node->Set("value",  value);
}

void 
XmlParser::ignorableWhitespace(GCString* text)
{
#ifdef XMLPARSER_DEBUG
  std::cout << *text;
#endif

  // Create text node
  Object* node = createNode(xmlText);
  node->Set("data",  text);
}

void 
XmlParser::characters(GCString* text)
{
#ifdef XMLPARSER_DEBUG
  std::cout << *text;
#endif

  // Create text node
  Object* node = createNode(xmlText);
  node->Set("data",  text);
}
