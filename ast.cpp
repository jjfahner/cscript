#include "ast.h"
#include "astgen.c"
#include "file.h"
#include "lexer.h"

int
AstGen::main(int argc, char** argv)
{
  AstGen astGen;

  // Parse file
  astGen.Parse("test.csc");

  // Enumerate contents
  astGen.Enumerate(astGen.m_root);

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
  //Variant error("Parse failed on line ");
  //error += m_lexer->GetLine();
  //throw std::runtime_error(error.GetString());
}

void 
AstGen::OnSyntaxError()
{
  //Variant error("Syntax error on line ");
  //error += m_lexer->GetLine();
  //throw std::runtime_error(error.GetString());
}

void 
AstGen::SetRoot(Ast* root)
{
  if(m_root)
  {
    delete m_root;
    m_root = 0;
  }
  m_root = root;
}

void 
AstGen::Enumerate(Ast* node)
{
  if(node == 0)
  {
    return;
  }

  std::cout << "(";
  switch(node->m_type)
  {
  case expression_statement:
    Enumerate(any_cast<Ast*>(node->m_a1));
    break;

  case statement_sequence:
    Enumerate(any_cast<Ast*>(node->m_a1)); std::cout << ",";
    Enumerate(any_cast<Ast*>(node->m_a2));
    break;

  case assignment_expression:
  case binary_expression:
    Enumerate(any_cast<Ast*>(node->m_a2)); std::cout << ",";
    Enumerate(any_cast<Ast*>(node->m_a3));
    break;

  case ternary_expression:
    Enumerate(any_cast<Ast*>(node->m_a1)); std::cout << ",";
    Enumerate(any_cast<Ast*>(node->m_a2)); std::cout << ",";
    Enumerate(any_cast<Ast*>(node->m_a2));
    break;

  case index_expression:
    Enumerate(any_cast<Ast*>(node->m_a1)); std::cout << ",";
    Enumerate(any_cast<Ast*>(node->m_a2));
    break;

  case function_call:
    Enumerate(any_cast<Ast*>(node->m_a1)); std::cout << ",";
    Enumerate(any_cast<Ast*>(node->m_a2));
    break;

  
//   prefix_expression,
//   postfix_expression,
//   member_expression,
//   index_expression,
//   function_call,
 //argument_list,


  case integer:
    std::cout << any_cast<String>(node->m_a1);
    break;

  case real:
    std::cout << any_cast<String>(node->m_a1);
    break;

  case string:
    std::cout << any_cast<String>(node->m_a1);
    break;

  case boolean:
    std::cout << any_cast<String>(node->m_a1);
    break;

  case null:
    std::cout << any_cast<String>(node->m_a1);
    break;

  case identifier:
    std::cout << any_cast<String>(node->m_a1);
    break;

  default:
    std::cout << "?" << node->m_type;
  }

  std::cout << ")";
}

