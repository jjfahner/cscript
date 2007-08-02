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
  astGen.Parse("test.csc");

  // Generate code
  CodeGenerator cg;
  cg.Generate(astGen.m_root);
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
