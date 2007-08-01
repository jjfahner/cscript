//////////////////////////////////////////////////////////////////////////
//
// This file is © 2007 JJ Fahner <jan-jaap@jan-jaap.net>
// This file is part of the cscript interpreter.
// CScript can be found at http://svn.jan-jaap.net/
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//
//////////////////////////////////////////////////////////////////////////
#include "parser.h"
#include "machine.h"
#include "file.h"
#include "cmdargs.h"
#include "ast.h"

//////////////////////////////////////////////////////////////////////////
//
// Print version
//
void version()
{
  cout << "CScript 0.3  Copyright (C) 2007  Jan-Jaap Fahner.\n\n";
}

//////////////////////////////////////////////////////////////////////////
//
// Print command line parameters
//

int usage()
{
  version();
  cout << 
    "Usage: cscript [options] [file]\n\n"
    "Options:\n\n"
    "-i --interactive   Run interpreter in interactive mode\n"
    "-c --compile=FILE  Compile source file\n"
    "-o --output=FILE   Name of the compiled output file\n"
    "-e --execute=FILE  Name of a file to execute\n"
    "\n"
    "This program comes with ABSOLUTELY NO WARRANTY.\n"
    "This is free software, and you are welcome to redistribute it\n"
    "under certain conditions; type `--info' for details.\n\n";
    ;
  return EXIT_SUCCESS;
}

//////////////////////////////////////////////////////////////////////////
//
// Compile mode
//

int compile(CmdArgs const& args)
{
  // Check output filename
  String outFile;
  if(args.IsSet("-o"))
  {
    outFile = args["-o"];
  }
  else if(args.IsSet("--output"))
  {
    outFile = args["--output"];
  }
  if(outFile.empty())
  {
    usage();
    return EXIT_FAILURE;
  }

  // Determine source filename
  StringMap files = args.GetValues();
  if(files.size() != 1)
  {
    usage();
    return EXIT_FAILURE;
  }

  // Parse the input file
  Parser parser;
  parser.ParseFile(files.begin()->first);

  // Create output file
  std::ofstream ofs(outFile.c_str(), std::ios::binary);

  // Write file header
  ofs.write("\xce\xec", 2);

  // Write code
  ofs.write((char*)parser.GetCode(), parser.GetSize());

  // Close file
  ofs.close();

  // Succeeded
  return EXIT_SUCCESS;
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
// Execute a file
//

int execute(CmdArgs const& args)
{
  // Find files to execute
  StringMap files = args.GetValues();
  if(files.size() != 1)
  {
    usage();
    return EXIT_FAILURE;
  }

  // Open the file
  File file;
  file.Open(files.begin()->first);

  // Fetch code pointer from file
  Byte* code = file.GetData();

  // Compile code if required
  if(file.GetType() == File::source)
  {
    // Let parser parse input
    Parser parser;
    parser.ParseText((Char*)file.GetData());
    
    // Take code from parser
    code = parser.ReleaseCode();
  }

  // Execute code
  StackMachine machine;
  machine.Execute(code);

  // Program succeeded
	return EXIT_SUCCESS;
}

//////////////////////////////////////////////////////////////////////////
//
// Application entry point
//

int cscript_main(int argc, Char** argv)
{
  CmdArgs args(argc, argv);

  // Version info
  if(args.IsSet("-v") || args.IsSet("--version"))
  {
    return usage();
  }

  // Interactive mode
  if(args.IsSet("-i"))
  {
    return interactive(args);
  }

  // Compile mode
  if(args.IsSet("-c") || args.IsSet("--compile"))
  {
    return compile(args);
  }

  // Execute mode
  return execute(args);
}

//
// CScript entry point. Exception handling root.
//
int main(int argc, Char** argv)
{
  int result = EXIT_FAILURE;
  try
  {
#   ifdef AST_IMPL
    return AstGen::main(argc, argv);
#   else
    result = cscript_main(argc, argv);
#   endif
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
