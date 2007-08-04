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
#include "ast.h"
#include "astgen.c"
#include "file.h"
#include "lexer.h"
#include "codegen.h"

int
AstGen::main(int argc, char** argv)
{
  AstGen astGen;

  // Parse file
  astGen.Parse("testsuite.csc");

  // Generate code
  CodeGenerator cg;
  cg.Generate(astGen.m_root, true);
  cg.Write();
  cg.Execute();

  // Wait for input
  std::cin.get();

  // Done
  return 0;
}

AstGen::AstGen() :
m_root (0)
{

}

void
AstGen::Parse(String const& filename)
{
  // Create file
  File file;
  file.Open(filename);

  // Check type
  if(file.GetType() != File::source)
  {
    throw std::runtime_error("Invalid file");
  }

  // Create lexer for file
  Lexer lexer;
  lexer.SetText((Char*)file.GetData());

  // Allocate parser
  void *pParser = AstGenParseAlloc(malloc);

  //AstGenParseTrace(stdout, "> ");

  // Try block for parser memory management
  try 
  {
    // Run parser loop
    Token token;
    while(lexer.Lex(token))
    {
      AstGenParse(pParser, token.m_type, token, this);
    }
    
    // Empty token to finalize parse
    AstGenParse(pParser, 0, token, this);

    // Destroy parser
    AstGenParseFree(pParser, free);
  }
  catch(...)
  {
    // Destroy parser
    AstGenParseFree(pParser, free);
  
    // Rethrow exception
    throw;
  }
}

void 
AstGen::OnParseFailure()
{
  throw std::runtime_error("Parse failure");
}

void 
AstGen::OnSyntaxError()
{
  throw std::runtime_error("Syntax error");
}

Ast*
AstGen::GetRoot() const
{
  return m_root;
}

void 
AstGen::SetRoot(Ast* root)
{
  m_root = root;
}
