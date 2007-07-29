#include "parser.h"
#include "machine.h"
#include "file.h"

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
    Char buf[4000];
    std::streamsize len = 4000;

    cout << L"\n> ";
    cin.getline(buf, len);

    try
    {
      Quad offset = parser.ParseText(buf);
      machine.Execute(parser.GetCode(), offset);
    }
    catch(std::exception const& e)
    {
      cout << e.what() << std::endl;
    }
  }
}

//
// Unicode entry point
//
int cscript_main(int argc, Char** argv)
{
  // Check arguments
  if(argc == 1)
  {
    interactive();
    return EXIT_SUCCESS;
  }

  // Open the file
  File file;
  file.Open(argv[1]);

  // Fetch code pointer from file
  Byte* code = file.GetData();

  // Compile code if required
  if(file.GetType() == File::source)
  {
    // Let parser parse input
    Parser parser;
    parser.ParseText((Char*)file.GetData());
    
    // Write code to file
#   ifdef _DEBUG
    ofstream of("out.csb", std::ios::binary);
    of.write("\xce\xec", 2);
    of.write((char*)parser.GetCode(), parser.GetSize());
#   endif

    // Take code from parser
    code = parser.ReleaseCode();
  }

  // Execute code
  StackMachine machine;
  machine.Execute(code);

  // Program succeeded
	return EXIT_SUCCESS;
}

//
// Unicode entry point
//
int main(int argc, Char** argv)
{
  int result = EXIT_FAILURE;
  try
  {
    result = cscript_main(argc, argv);
  }
  catch(std::exception const& e)
  {
    cout << "\nException: " << e.what() << "\n";
  }
  catch(...)
  {
    cout << "\nUnexpected exception\n";
  }

#ifdef _DEBUG
  cout << "\n\nPress enter to quit";
  cin.get();
#endif

	return result;
}
