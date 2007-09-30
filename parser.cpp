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
#include "parser.h"
#include "file.h"
#include "lexer.h"
#include "cscript.c"
#include "report.h"

Parser::Parser(Reporter& reporter) :
m_reporter  (reporter),
m_root      (0),
m_lexer     (0),
m_file      (0)
{

}

Parser::~Parser()
{
  delete m_root;
}

void
Parser::Parse(String const& filename)
{
  // Create file
  File file;
  file.Open(filename);

  // Parse file
  Parse(file);
}

void
Parser::Parse(File& file)
{
  // Check type
  if(file.GetType() != File::source)
  {
    throw std::runtime_error("Invalid file");
  }

  // Push file on stack
  File* prevfile = m_file;
  m_file = &file;

  // Try block for file pointer stacking
  try
  {
    ParseText((char const*)file.GetData());
    m_file = prevfile;
  }
  catch(...)
  {
    m_file = prevfile;
  }
}

void 
Parser::ParseText(char const* text)
{
  // Create lexer for file
  Lexer lexer(*this);
  lexer.SetText((char*)text);

  // Push lexer on stack
  Lexer* prevlexer = m_lexer;
  m_lexer = &lexer;

  // Allocate parser
  void *pParser = CScriptParseAlloc(malloc);

  // Try block for parser memory management
  try 
  {
    // Run parser loop
    Token token;
    while(lexer.Lex(token))
    {
      CScriptParse(pParser, token.m_type, token, this);
    }

    // Empty token to finalize parse
    CScriptParse(pParser, 0, Token(), this);

    // Destroy parser
    CScriptParseFree(pParser, free);

    // Remove lexer
    m_lexer = prevlexer;
  }
  catch(...)
  {
    // Destroy parser
    CScriptParseFree(pParser, free);

    // Remove lexer
    m_lexer = prevlexer;

    // Rethrow exception
    throw;
  }
}

void 
Parser::OnParseFailure()
{
  FilePos pos;
  pos.m_file = m_file ? m_file->GetPath() : "";
  pos.m_line = m_lexer->GetLine();
  m_reporter.ReportError(E0012, &pos);
}

void 
Parser::OnSyntaxError()
{
  FilePos pos;
  pos.m_file = m_file ? m_file->GetPath() : "";
  pos.m_line = m_lexer->GetLine();
  m_reporter.ReportError(E0013, &pos);
}

Ast*
Parser::GetRoot() const
{
  return m_root;
}

void 
Parser::SetRoot(Ast* root)
{
  m_root = root;
}

Ast* 
Parser::AllocAst(AstTypes type, AstData const& a1, AstData const& a2, AstData const& a3, AstData const& a4)
{
  // Set file position
  FilePos pos;
  pos.m_file = m_file ? m_file->GetPath() : "";
  pos.m_line = m_lexer->GetLine();

  // Create node
  Ast* node = new Ast(type, a1, a2, a3, a4);  
  node->m_pos = pos;
  
  // Register new types
  if(type == class_declaration)
  {
    m_types[a1] = node;
  }
  
  // Return the new node
  return node;
}

