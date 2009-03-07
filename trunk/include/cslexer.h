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
#ifndef CSCRIPT_CSLEXER_H
#define CSCRIPT_CSLEXER_H

#include <cscript.h>

struct Token;
class LexStream;

class CSLexer 
{
public:

  //
  // Construction
  //
  CSLexer(LexStream& stream);

  //
  // Lex from string
  //
  void SetText(Char* text);

  //
  // Fetch next token
  //
  bool Lex(Token& token);

  //
  // Current line
  //
  int32 GetLine() const {
    return m_line;
  }

private:

  //
  // Generated from cslexer.gen.in
  //
  int ParseNextToken();
  
  //
  // Specialized lexing
  //
  bool LexString(Token& token);
  bool LexComment(int type);

  //
  // MemberMap
  //
  LexStream&  m_stream;
  Char*   m_source;
  size_t  m_length;
  Char*   m_strptr;
  int32   m_line;
  int     m_string;

};

#endif // #ifndef CSCRIPT_CSLEXER_H
