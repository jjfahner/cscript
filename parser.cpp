#include "parser.h"
#include "file.h"
#include "lexer.h"
#include "astgen.c"
#include "report.h"

Parser::Parser(Reporter& reporter) :
m_reporter  (reporter),
m_root      (0),
m_lexer     (0)
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

  // Create lexer for file
  Lexer lexer;
  lexer.SetText((Char*)file.GetData());

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
    CScriptParse(pParser, 0, token, this);

    // Destroy parser
    CScriptParseFree(pParser, free);

    // Remove lexer and file
    m_lexer = prevlexer;
    m_file = prevfile;
  }
  catch(...)
  {
    // Destroy parser
    CScriptParseFree(pParser, free);

    // Remove lexer
    m_lexer = prevlexer;
    m_file = prevfile;

    // Rethrow exception
    throw;
  }
}

void 
Parser::OnParseFailure()
{
  FilePos pos;
  pos.m_file = m_file->GetPath();
  pos.m_line = m_lexer->GetLine();
  m_reporter.ReportError(E0012, &pos);
}

void 
Parser::OnSyntaxError()
{
  FilePos pos;
  pos.m_file = m_file->GetPath();
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
  FilePos pos;
  pos.m_file = m_file->GetPath();
  pos.m_line = m_lexer->GetLine();

  Ast* node = new Ast(type, a1, a2, a3, a4);  
  node->m_pos = pos;
  return node;
}

