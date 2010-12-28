//////////////////////////////////////////////////////////////////////////
//
// This file is © 2007 - 2011 JJ Fahner <jan-jaap@jan-jaap.net>
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
  // Fetch next token
  //
  bool Lex(Token& token);

  //
  // Called after a binop is parsed
  //
  void AfterBinop();

//   //
//   // Start/end regex mode
//   //
//   void StartRegex();
//   void EndRegex();

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
  bool LexRegex(Token& token);

  //
  // MemberMap
  //
  LexStream&      m_stream;
  int             m_string;
  int             m_regex;

};

#endif // #ifndef CSCRIPT_CSLEXER_H
