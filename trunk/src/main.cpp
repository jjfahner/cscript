//////////////////////////////////////////////////////////////////////////
//
// This file is © 2007 - 2009 JJ Fahner <jan-jaap@jan-jaap.net>
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
#include <cmdargs.h>
#include <eval.h>

// Standard library stuff
#include <iostream>

//////////////////////////////////////////////////////////////////////////
//
// Print version
//
void banner()
{
  std::cout << "CScript 0.9 Copyright (C) 2007-2009  Jan-Jaap Fahner.\n\n";
}

//////////////////////////////////////////////////////////////////////////
//
// Print command line parameters
//

int usage()
{
  banner();
  std::cout << 
    "Usage: cscript [options] [file]\n\n"
    "Options:\n\n"
    "-q --quiet                    Don't display banner\n"
    "-e --execute \"expression\"   Execute expression\n"
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

int execute(String const& file)
{
  // Execute code in file
  Evaluator eval;
  Value result = eval.Eval(file, true);

  // Determine exit code
  int code = (int)ValInt(result);

  // Exit
  return code;
}

//////////////////////////////////////////////////////////////////////////
//
// Execute command line as expression
//

int commandline(int argc, Char** argv)
{
  // Execute code in file
  Evaluator eval;
  Value result = eval.Eval(argv[2], false);

  // Determine exit code
  int code = (int)ValInt(result);

  // Exit
  return code;
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
  if(args.IsSet("-i") || args.IsSet("--interactive"))
  {
    return interactive(args);
  }

  // Expression is on command line
  if(args.IsSet("-e") || args.IsSet("--execute"))
  {
    return commandline(argc, argv);
  }

  // Execute mode
  return execute(argv[1]);
}

//
// CScript entry point. Exception handling root.
//
int main(int argc, Char** argv)
{
#if defined(_MSC_VER) && defined(_DEBUG)
  banner();
#endif

  int result = EXIT_FAILURE;
  try
  {
    result = cscript_main(argc, argv);
  }
  catch(std::exception const& e)
  {
    std::cout << "\nException: " << e.what() << "\n";
  }
  catch(...)
  {
    std::cout << "\nUnexpected exception\n";
  }

	return result;
}
