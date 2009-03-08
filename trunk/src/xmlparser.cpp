//////////////////////////////////////////////////////////////////////////
//
// This file is � 2009 JJ Fahner <jan-jaap@jan-jaap.net>
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

#include "xmlparser.gen.h"
#include "xmlparser.gen.c"

GCSTR(str_nodeType,         "nodeType");
GCSTR(str_nodeName,         "nodeName");
GCSTR(str_nodeTypeName,     "nodeTypeName");
GCSTR(str_childNodes,       "childNodes");
GCSTR(str_attributes,       "attributes");
GCSTR(str_parentNode,       "parentNode");
GCSTR(str_target,           "target");
GCSTR(str_nodeCount,        "nodeCount");
GCSTR(str_localName,        "localName");
GCSTR(str_qualifiedName,    "qualifiedName");
GCSTR(str_namespace,        "namespace");
GCSTR(str_data,             "data");
GCSTR(str_value,            "value");
GCSTR(str_ownerDocument,    "ownerDocument");
GCSTR(str_documentElement,  "documentElement");

GCSTR(str_xmlElement,               "#element");
GCSTR(str_xmlAttribute,             "#attribute");
GCSTR(str_xmlText,                  "#text");
GCSTR(str_xmlCDATASection,          "#cdata-section");
GCSTR(str_xmlEntityReference,       "#entity-reference");
GCSTR(str_xmlEntity,                "#entity");
GCSTR(str_xmlProcessingInstruction, "#processing-instruction");
GCSTR(str_xmlComment,               "#comment");
GCSTR(str_xmlDocument,              "#document");
GCSTR(str_xmlDocumentType,          "#document-type");
GCSTR(str_xmlDocumentFragment,      "#document-fragment");
GCSTR(str_xmlNotation,              "#notation");

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

static const GCString*
XmlNodeName(XmlParser::XmlNodeTypes nodeType)
{
  switch(nodeType)
  {
  case xmlElement:                return str_xmlElement;
  case xmlAttribute:              return str_xmlAttribute;
  case xmlText:                   return str_xmlText;
  case xmlCDATASection:           return str_xmlCDATASection;
  case xmlEntityReference:        return str_xmlEntityReference;
  case xmlEntity:                 return str_xmlEntity;
  case xmlProcessingInstruction:  return str_xmlProcessingInstruction;
  case xmlComment:                return str_xmlComment;
  case xmlDocument:               return str_xmlDocument;
  case xmlDocumentType:           return str_xmlDocumentType;
  case xmlDocumentFragment:       return str_xmlDocumentFragment;
  case xmlNotation:               return str_xmlNotation;
  default:                        throw std::runtime_error("Invalid XML node type");
  }
}

//////////////////////////////////////////////////////////////////////////
//
// Lemon parser wrapper
//

typedef LemonParser<
  XmlParser, 
  XmlToken, 
  XmlParseAlloc, 
  XmlParseFree, 
#ifdef _DEBUG
  XmlParseTrace, 
#else
  0,
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
  Object* node = new Object();
  (*node)[str_ownerDocument]  = m_document;
  (*node)[str_nodeType]       = (int)type;
  (*node)[str_nodeName]       = XmlNodeName(type);
  (*node)[str_nodeTypeName]   = XmlNodeName(type);

  // Add child nodes and attributes
  switch(type)
  {
  case xmlDocument:
  case xmlDocumentFragment:
  case xmlElement:
    (*node)[str_childNodes]   = new Object();
    (*node)[str_attributes]   = new Object();
    break;
  }

  // Attach to parent node
  if(m_curNode)
  {
    (*node)[str_parentNode] = m_curNode;
    if(type == xmlAttribute)
    {
      (*m_curNode)[str_attributes].GetObject()->Add(node);
    }
    else
    {
      (*m_curNode)[str_childNodes].GetObject()->Add(node);
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
  (*m_document)["nodeCount"] = m_nodeCount;
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
  (*node)[str_target] = target.m_localName;
  (*node)[str_data]   = data;
}

void 
XmlParser::startElement(XmlName const& name)
{
#ifdef XMLPARSER_DEBUG
  std::cout << "<" << *name.m_localName << ">";
#endif

  // Create element
  Object* node = createNode(xmlElement);
  (*node)[str_localName]      = name.m_localName;
  (*node)[str_qualifiedName]  = name.m_localName;
  (*node)[str_namespace]      = name.m_namespace;

  // Set as current element
  m_curNode = node;

  // Set root node
  if(m_rootNode == 0)
  {
    m_rootNode = node;
    (*m_document)[str_documentElement] = node;
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
  String nodeName = (*m_curNode)["localName"].GetString();
  if(nodeName != *name.m_localName)
  {
    throw std::runtime_error("Invalid document structure");
  }

  // Move back up in tree
  m_curNode = (*m_curNode)["parentNode"].GetObject();

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
  (*node)[str_localName]      = name.m_localName;
  (*node)[str_qualifiedName]  = name.m_localName;
  (*node)[str_namespace]      = name.m_namespace;
  (*node)[str_value]          = value;
}

void 
XmlParser::ignorableWhitespace(GCString* text)
{
#ifdef XMLPARSER_DEBUG
  std::cout << *text;
#endif

  // Create text node
  Object* node = createNode(xmlText);
  (*node)[str_data] = text;
}

void 
XmlParser::characters(GCString* text)
{
#ifdef XMLPARSER_DEBUG
  std::cout << *text;
#endif

  // Create text node
  Object* node = createNode(xmlText);
  (*node)[str_data] = text;
}
