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

// Assert whether a result matches the expectation
function assert(description, result, expected)
{
  print(description + "...");
  if(result == expected)
  {
    print("Ok\n");
    return 0;
  }
  else
  {
    print("Failed\n");
    return 1;
  }
}

// Test cases
function main()
{
  print("Welcome to the cscript test suite.\n");
 
  var errors = 0;
  errors += assert("Add", 2 + 2, 4);
  errors += assert("Add", -2 + 4, 2);
  errors += assert("Subtract", 2 - 2, 0);
  errors += assert("Subtract", 2 - 4, -2);
  errors += assert("Subtract", 4 - 2, 2);
  errors += assert("Subtract", 2 - 2, 0);
  errors += assert("Multiply", 2 * 2, 4);
  errors += assert("Multiply", 1 * 4, 4);
  errors += assert("Multiply", 1 * 0, 0);
  
  // Print result
  if(errors == 0)
  {
    print("\nAll tests succeeded!\n");
  }
  else
  {
    print("\n" + errors + " tests failed.\n");
  }
}

function foo() {}

// Start by invoking main()
main();
