#include <iostream>
#include <fstream>

#include "parser.h"
#include "machine.h"

//
// Print version
//
void version()
{
  std::wcout << "CScript 0.2 interpreter\n";
  std::wcout << "Written by Jan-Jaap Fahner\n\n";
}

//
// Print command line parameters
//
void usage()
{
  version();
  std::wcout
    << "Usage: cscript <scriptname>\n\n";
}

//
// Unicode entry point
//
int wmain(int argc, wchar_t** argv)
{
  // Check arguments
  if(argc != 2)
  {
    usage();
    return EXIT_FAILURE;
  }

  // Run program
  try
  {
    // Parse input
    Parser parser;
    parser.Parse(argv[1]);

    // Write code to file
  #ifdef _DEBUG
    std::ofstream of("out.csb", std::ios::binary);
    of.write((char*)parser.GetCode(), parser.GetSize());
  #endif

    // Execute code
    StackMachine machine;
    machine.Execute(parser.GetCode());
  }
  catch(std::exception const& e)
  {
    std::cout << "\nException: " << e.what() << "\n";
  }
  catch(...)
  {
    std::cout << "\nUnexpected exception\n";
  }

  // Wait for user input
#ifdef _DEBUG
  std::cout << "\n\nPress enter to quit";
  std::cin.get();
#endif

  // Program succeeded
	return EXIT_SUCCESS;
}
