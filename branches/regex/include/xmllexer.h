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
#ifndef CSCRIPT_XML_LEXER_H
#define CSCRIPT_XML_LEXER_H

#include <value.h>
#include <iosfwd>

class LexStream;

struct XmlToken
{
  size_t    m_type;
  GCString* m_text;
  size_t    m_line;
};

enum XmlLexerStates
{
  xmllsProlog,
  xmllsElement,
  xmllsContent,
  xmllsEpilog
};

class XmlLexer
{
public:

  //
  // Construction
  //
  XmlLexer(LexStream& stream);

  //
  // Set parser state
  //
  void SetState(XmlLexerStates state);

  //
  // At end of input
  //
  bool Eof();

  //
  // Retrieve next token
  //
  bool Lex(XmlToken&);

private:

  //
  // Retrieve next token
  //
  bool LexImpl(XmlToken&);

  //
  // Parse text node
  //
  int ParseTextNode();

  //
  // Parse string constant
  //
  int ParseString();

  //
  // This function is generated by re2c
  //
  int ParseNextToken();

  //
  // Members
  //
  LexStream&      m_stream;
  GCString*       m_token;
  bool            m_inNode;
  XmlLexerStates  m_state;

};

#endif // CSCRIPT_XML_LEXER_H
