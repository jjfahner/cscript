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
#include "file.h"
#include "cmdargs.h"
#include "ast.h"
#include "parser.h"
#include "codegen.h"
#include "machine.h"
#include "annotate.h"
#include "optimize.h"

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
    "-a --annotate=FILE   Annotate source file (requires -o)\n"
    "-c --compile=FILE    Compile source file (requires -o)\n"
    "-d --decompile=FILE  Decompile binary code (requires -o)\n"
    "-e --execute=FILE    Name of a file to execute\n"
    "-i --interactive     Run interpreter in interactive mode\n"
    "-o --output=FILE     Name of output file\n"
    "\n"
    "This program comes with ABSOLUTELY NO WARRANTY.\n"
    "This is free software, and you are welcome to redistribute it\n"
    "under certain conditions; type `--info' for details.\n\n";
    ;
  return EXIT_SUCCESS;
}

//////////////////////////////////////////////////////////////////////////
//
// Annotation mode
//

int annotate(CmdArgs const& args)
{
  Reporter reporter;

  // Check input
  String srcFile;
  if(args.IsSet("-a"))
  {
    srcFile = args["-a"];
  }
  else if(args.IsSet("--annotate"))
  {
    srcFile = args["--annotate"];
  }
  if(srcFile.empty())
  {
    usage();
    return EXIT_FAILURE;
  }

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

  Parser parser(reporter);
  CodeGenerator cg(reporter);

  // Generate ast
  parser.Parse(srcFile);
  
  // Optimize code
  Optimizer optimizer;
  Ast* root = optimizer.Optimize(parser.GetRoot());

  // Annotate code
  Annotator annotator(reporter);
  annotator.Annotate(root);

  // Print annotated code
  cg.Print(outFile, root);

  // Done
  return 0;

  // Succeeded
  return EXIT_SUCCESS;
}

//////////////////////////////////////////////////////////////////////////
//
// Compile mode
//

int compile(CmdArgs const& args)
{
  Reporter reporter;

  // Check input
  String srcFile;
  if(args.IsSet("-c"))
  {
    srcFile = args["-c"];
  }
  else if(args.IsSet("--compile"))
  {
    srcFile = args["--compile"];
  }
  if(srcFile.empty())
  {
    usage();
    return EXIT_FAILURE;
  }

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

  // Generate ast
  Parser parser(reporter);
  parser.Parse(srcFile);

  // Generate code
  CodeGenerator cg(reporter);
  cg.Generate(parser.GetRoot(), true);

  // Write file
  std::ofstream ofs(outFile.c_str(), std::ios::binary);
  ofs.write((char*)cg.GetCode(), cg.GetSize());
  ofs.close();
  
  // Done
  return 0;

  // Succeeded
  return EXIT_SUCCESS;
}

//////////////////////////////////////////////////////////////////////////
//
// Decompile binary code
//

int decompile(CmdArgs const& args)
{
  Reporter reporter;

  // Check input
  String srcFile;
  if(args.IsSet("-d"))
  {
    srcFile = args["-d"];
  }
  else if(args.IsSet("--decompile"))
  {
    srcFile = args["--decompile"];
  }
  if(srcFile.empty())
  {
    usage();
    return EXIT_FAILURE;
  }

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

  // Open input file
  File file;
  file.Open(srcFile);

  // Check for binary file
  if(file.GetType() != File::binary)
  {
    std::cout << "Error: invalid file format\n";
    return EXIT_FAILURE;
  }

  // Create output file
  std::ofstream ofs(outFile.c_str());

  // Decompile source code
  CodeGenerator cg(reporter);
  cg.Decompile(file.GetData(),
               file.GetHeader()->m_codeseg, 
               file.GetHeader()->m_codelen + file.GetHeader()->m_proclen, 
               ofs);

  // Close file
  ofs.close();

  // Done
  return 0;

  // Succeeded
  return EXIT_SUCCESS;
}

//////////////////////////////////////////////////////////////////////////
//
// Interactive mode
//

int interactive(CmdArgs const& args)
{
  return EXIT_FAILURE;
}

//////////////////////////////////////////////////////////////////////////
//
// Execute a file
//

int execute(CmdArgs const& args)
{
  Reporter reporter;

  // Find files to execute
  StringMap files = args.GetValues();
  if(files.size() != 1)
  {
    usage();
    return EXIT_FAILURE;
  }

  // Store filename
  String filename = files.begin()->first;

  // Open the file
  File file;
  file.Open(filename);

  // Fetch code pointer from file
  Byte* code = 0;
  Quad  offset = 0;
  if(file.GetType() == File::binary)
  {
    // Take pointer to code segment
    code = file.GetData();
    offset = file.GetHeader()->m_codeseg;
  }
  else
  {
    // Generate ast
    Parser parser(reporter);
    parser.Parse(file);

    // Generate code
    CodeGenerator cg(reporter);
    cg.Generate(parser.GetRoot(), true);

    // Check whether compilation succeeded
    if(cg.GetCode() == 0)
    {
      return EXIT_FAILURE;
    }

    // Write to file
#ifdef _DEBUG
    std::ofstream ofs((filename + ".csb").c_str(), std::ios::binary);
    ofs.write((char*)cg.GetCode(), cg.GetSize());
    ofs.close();
#endif

    // Take code from parser
    BinHeader* header = (BinHeader*) cg.ReleaseCode();
    code = (Byte*)header;
    offset = header->m_codeseg;

    // Decompile
#ifdef _DEBUG
    ofs.open((filename + ".txt").c_str());
    cg.Decompile(code,
                 header->m_codeseg, 
                 header->m_codelen + header->m_proclen, 
                 ofs);
#endif
  }

  // Execute code
  Machine machine;
  machine.Execute(code, offset);

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

  // Annotate mode
  if(args.IsSet("-a") || args.IsSet("--annotate"))
  {
    return annotate(args);
  }

  // Compile mode
  if(args.IsSet("-c") || args.IsSet("--compile"))
  {
    return compile(args);
  }

  // Decompile
  if(args.IsSet("-d") || args.IsSet("--decompile"))
  {
    return decompile(args);
  }

  // Execute mode
  return execute(args);
}

//
// For keeping debug window open
//
#if defined(_MSC_VER) && defined(_DEBUG)
#define _WIN32_WINNT 0x0400
#include <windows.h>
#endif

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
  if(IsDebuggerPresent())
  {
    cout << "\n\nPress enter to quit";
    cin.get();
  }
#endif

	return result;
}
