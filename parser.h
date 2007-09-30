//////////////////////////////////////////////////////////////////////////
//
// This file is © 2007 JJ Fahner <jan-jaap@jan-jaap.net>
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
#ifndef CSCRIPT_PARSE_H
#define CSCRIPT_PARSE_H

#include "types.h"
#include "ast.h"

class File;
class Lexer;
class Reporter;

class Parser
{
public:

  //
  // Construction
  //
  Parser(Reporter& reporter);

  //
  // Destruction
  //
  ~Parser();

  //
  // Parse a string
  //
  void ParseText(char const* text);

  //
  // Parse a file
  //
  void Parse(File& file);
  void Parse(String const& filename);

  //
  // Root node
  //
  Ast* GetRoot() const;
  void SetRoot(Ast* root);

  //
  // Allocate node
  //
  Ast* AllocAst(AstTypes type, 
    AstData const& a1 = AstData(), 
    AstData const& a2 = AstData(), 
    AstData const& a3 = AstData(), 
    AstData const& a4 = AstData());

  //
  // Error handlers
  //
  void OnParseFailure();
  void OnSyntaxError();

private:

  //
  // Members
  //
  Ast*                    m_root;
  Reporter&               m_reporter;
  File*                   m_file;
  Lexer*                  m_lexer;

public:

  std::map<String, Ast*>  m_types;

};

#endif // CSCRIPT_PARSE_H
