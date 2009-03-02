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

struct XmlToken
{
  size_t    m_type;
  GCString* m_text;
  size_t    m_line;
};

class XmlLexer
{
public:

  //
  // Construction
  //
  XmlLexer(std::istream& is);

  //
  // At end of input
  //
  bool AtEof();

  //
  // Retrieve next token
  //
  bool Lex(XmlToken&);

private:

  //
  // Parse text node
  //
  int ParseTextNode();

  //
  // This function is generated by re2c
  //
  int ParseNextToken();

  //
  // This function is called by re2c to refill the buffer
  //
  void FillInputBuffer(size_t);

  //
  // Buffer size
  //
  enum { bufsize = 1024 };

  //
  // Members
  //
  std::istream& m_stream;
  char          m_buffer[bufsize];
  char const*   m_cursor;
  char const*   m_bufend;
  char const*   m_marker;
  char const*   m_tokpos;
  GCString*     m_token;
  bool          m_inNode;

};

#endif // CSCRIPT_XML_LEXER_H
