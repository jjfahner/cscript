#include "parser.h"
#include "machine.h"
#include "file.h"
#include "cmdargs.h"

//////////////////////////////////////////////////////////////////////////
//
// Print version
//
void version()
{
  cout << "CScript 0.2 interpreter\n";
  cout << "Written by Jan-Jaap Fahner\n\n";
}

//////////////////////////////////////////////////////////////////////////
//
// Print command line parameters
//

void usage()
{
  version();
  cout << 
    "Usage: cscript [options] file\n"
    "Options:\n"
    "-i interactive\t\tRuns cscript in interactive mode\n"
    "-c compile <src> <dst>\tCompiles src to dst\n"
    ;
}

//////////////////////////////////////////////////////////////////////////
//
// Compile mode
//

int compile(CmdArgs const& args)
{
  return 0;
}

//////////////////////////////////////////////////////////////////////////
//
// Interactive mode
//

int interactive(CmdArgs const& args)
{
  Parser parser;
  StackMachine machine;

  // Welcome message
  version();
  cout << "CScript is running in interactive mode.\n";

  // Run interactive loop
  for(;;)
  {
    Char buf[4000];
    std::streamsize len = 4000;

    cout << "\n> ";
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

//////////////////////////////////////////////////////////////////////////
//
// Application entry point
//

int cscript_main(int argc, Char** argv)
{
  CmdArgs args(argc, argv);
  StringList files = args.GetValues();
  StringList opts = args.GetOpts();

  // Compile files
  if(args.IsSet("-c"))
  {
    return compile(args);
  }
  
  // Interactive mode
  if(args.IsSet("-i"))
  {
    return interactive(args);
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
// CScript entry point. Exception handling root.
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

  // Keep console running under MSC devenv
#if defined(_MSC_VER) && defined(_DEBUG)
  cout << "\n\nPress enter to quit";
  cin.get();
#endif

	return result;
}
