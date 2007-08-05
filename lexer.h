//////////////////////////////////////////////////////////////////////////
//
// This file is � 2007 JJ Fahner <jan-jaap@jan-jaap.net>
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
#ifndef CSCRIPT_LEXER_H
#define CSCRIPT_LEXER_H

#include "types.h"

struct Token;

class Lexer 
{
public:

  //
  // Construction
  //
  Lexer();

  //
  // Lex from string
  //
  void SetText(char* text);

  //
  // Fetch next token
  //
  bool Lex(Token& token);

  //
  // Current line
  //
  Quad GetLine() const {
    return m_line;
  }

private:

  //
  // Specialized lexing
  //
  bool LexString(Token& token);
  bool LexComment();

  //
  // Members
  //
  Char*   m_source;
  size_t  m_length;
  Char*   m_strptr;
  Quad    m_line;

};

#endif // #ifndef CSCRIPT_LEXER_H