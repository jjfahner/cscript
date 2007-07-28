#include <iostream>
#include <fstream>
#include "lexer.h"
#include "cscript.c"
#include "machine.h"

void run()
{
  // Start tokenizer
  std::wifstream stream("test.csc", std::ios::binary);
  Lexer lexer(stream);

  // Parse info
  ParseContext context;

  // Allocate parser
  void *pParser = ParseAlloc(malloc);

  //ParseTrace(stdout, "->");

  // Invoke parser for every token
  Token token;
  while(lexer.Lex(token))
  {
    Parse(pParser, token.m_type, token, &context);
  }
  
  // Empty token to finalize parse
  Parse(pParser, 0, token, &context);

  // Destroy parser
  ParseFree(pParser, free);

  // Write code to file
  std::ofstream of("test.csb", std::ios::binary);
  of.write((char*)context.GetCode(), context.GetSize());

  // Execute code
  StackMachine machine(context);
  machine.Execute();
}

int main()
{
	std::cout << "CScript 0.1 started\n\n";

  try
  {
    run();
  }
  catch(std::exception const& e)
  {
    std::cout << "\nException: " << e.what() << "\n";
  }
  catch(...)
  {
    std::cout << "\nUnexpected exception\n";
  }

  std::cout << "\n\nPress enter to quit";
  std::cin.get();

	return 0;
}
