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
#include "report.h"
#include "parser.h"
#include "eval.h"
#include "ast.h"

//////////////////////////////////////////////////////////////////////////
//
// Print version
//
void banner()
{
  cout << "CScript 0.5  Copyright (C) 2007  Jan-Jaap Fahner.\n\n";
}

//////////////////////////////////////////////////////////////////////////
//
// Print command line parameters
//

int usage()
{
  banner();
  cout << 
    "Usage: cscript [options] [file]\n\n"
    "Options:\n\n"
    "-q --quiet           Don't display banner"
    "-e --execute=FILE    Name of a file to execute\n"
    "-i --interactive     Run interpreter in interactive mode\n"
    "\n"
    "This program comes with ABSOLUTELY NO WARRANTY.\n"
    "This is free software, and you are welcome to redistribute it\n"
    "under certain conditions; type `--info' for details.\n\n";
    ;
  return EXIT_SUCCESS;
}

//////////////////////////////////////////////////////////////////////////
//
// Interactive mode
//

int interactive(CmdArgs const& args)
{
  // Not quiet
  if(!(args.IsSet("-q") || args.IsSet("--quiet")))
  {
    banner();
  }

  // Create evaluator instance
  Evaluator eval;

  // Line buffer
  char line[4097];

  // Code buffer
  String code;

  // Continuous evaluation
  for(;;)
  {
    // Prompt for line
    std::cout << "> ";
    std::cin.getline(line, 4096);

    // Empty lines. TODO: trim line
    size_t len = strlen(line);
    if(len == 0)
    {
      continue;
    }

    // Line continuation
    bool continuation = false;
    if(line[len - 1] == '\\')
    {
      line[len - 1] = 0;
      continuation = true;
    }

    // Add to code
    code += line;
    if(continuation)
    {
      continue;
    }

    // Execute input
    eval.Eval(code);
    code.clear();

    // Ensure new line
    std::cout << "\n";
  }

  // Never reached
  return 0;
}

//////////////////////////////////////////////////////////////////////////
//
// Execute file
//

int execute(CmdArgs const&)
{
  return 0;
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

  // No args
  
  if(args.GetCount() == 0)
  {
    return interactive(args);
  }

  // Interactive mode
  if(args.IsSet("-i") ||args.IsSet("--interactive"))
  {
    return interactive(args);
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
