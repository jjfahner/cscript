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
  std::wcout << 
    "Usage: cscript [options] file\n"
//     "Options:\n"
//     "-i interactive\t\tRuns cscript in interactive mode\n"
//     "-c compile <src> <dst>\tCompiles src to dst\n"
    ;
}

//
// Interactive mode
//
int interactive()
{
  Parser parser;
  StackMachine machine;

  // Welcome message
  version();
  std::wcout << "CScript is running in interactive mode.\n\n";

  // Run interactive loop
  for(;;)
  {
    wchar_t buf[4000];
    std::streamsize len = 4000;

    std::wcout << L"> ";
    std::wcin.getline(buf, len);

    try
    {
      Quad offset = parser.ParseText(buf);
      machine.Execute(parser.GetCode(), offset);
    }
    catch(std::exception const& e)
    {
      std::cout << e.what() << std::endl;
    }
  }
}

//
// Unicode entry point
//
int cscript_main(int argc, wchar_t** argv)
{
  // Check arguments
  if(argc == 1)
  {
    interactive();
    return EXIT_SUCCESS;
  }

  // Run program
  try
  {
    // Parse input
    Parser parser;
    parser.ParseFile(argv[1]);

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

  // Program succeeded
	return EXIT_SUCCESS;
}

//
// Unicode entry point
//
int wmain(int argc, wchar_t** argv)
{
  // Run cscript
  int result = cscript_main(argc, argv);

  // Wait for user input
#ifdef _DEBUG
  std::cout << "\n\nPress enter to quit";
  std::cin.get();
#endif

  // Program succeeded
	return result;
}
