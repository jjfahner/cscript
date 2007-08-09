#include "parser.h"
#include "file.h"
#include "lexer.h"
#include "astgen.c"

Parser::Parser() :
m_root (0)
{

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

  // Create lexer for file
  Lexer lexer;
  lexer.SetText((Char*)file.GetData());

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
    CScriptParse(pParser, 0, token, this);

    // Destroy parser
    CScriptParseFree(pParser, free);
  }
  catch(...)
  {
    // Destroy parser
    CScriptParseFree(pParser, free);

    // Rethrow exception
    throw;
  }
}

void 
Parser::OnParseFailure()
{
  throw std::runtime_error("Parse failure");
}

void 
Parser::OnSyntaxError()
{
  throw std::runtime_error("Syntax error");
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
