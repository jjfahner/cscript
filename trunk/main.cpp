#include <iostream>
#include <fstream>

#include "parser.h"
#include "machine.h"

void run()
{
  // Parse input
  Parser parser;
  parser.Parse(L"test.csc");

  // Write code to file
  std::ofstream of("test.csb", std::ios::binary);
  of.write((char*)parser.GetCode(), parser.GetSize());

  // Execute code
  StackMachine machine(parser);
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
