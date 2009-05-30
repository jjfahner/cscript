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
#ifndef CSCRIPT_XML_PARSER_H
#define CSCRIPT_XML_PARSER_H

class GCString;
class Object;
class LexStream;
class XmlLexer;

class XmlParser
{
public:

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

  //
  // XML qualified name
  //
  struct XmlName
  {
    GCString* m_namespace;
    GCString* m_localName;
  };

  //
  // Parse contents of stream
  //
  Object* Parse(LexStream&);

  //
  // Error conditions raised by parser
  //
  void OnParseFailure();
  void OnSyntaxError();

  //
  // Create a node
  //
  Object* createNode(enum XmlNodeTypes);

  //
  // Called for the document
  //
  void startDocument();
  void endDocument();

  //
  // Called for a processing instruction
  //
  void processingInstruction(XmlName const& target, GCString* data);

  //
  // Called when an element is parsed
  //
  void startElement(XmlName const& name);
  void endElement(XmlName const& name);

  //
  // Called when an attribute is parsed
  //
  void attribute(XmlName const& name, GCString* value);

  //
  // Called for ignorable whitespace
  //
  void ignorableWhitespace(GCString* text);

  //
  // Called when a text data is parsed
  //
  void characters(GCString* text);

private:

  XmlLexer* m_lexer;
  Object*   m_document;
  Object*   m_curNode;
  Object*   m_rootNode;
  size_t    m_nodeCount;

};

#endif // CSCRIPT_XML_PARSER_H
