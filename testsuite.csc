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



//////////////////////////////////////////////////////////////////////////
//
// Assert whether a result matches the expectation
//
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

//////////////////////////////////////////////////////////////////////////
//
// Test cases
//
function main()
{
  print("cscript test suite\n\n");
 
  // Error count
  var errors = 0;
  
  // Arithmetic operations
  errors += assert("Add", 2 + 2, 4);
  errors += assert("Add", -2 + 4, 2);
  errors += assert("Subtract", 2 - 2, 0);
  errors += assert("Subtract", 2 - 4, -2);
  errors += assert("Subtract", 4 - 2, 2);
  errors += assert("Subtract", 2 - 2, 0);
  errors += assert("Multiply", 2 * 2, 4);
  errors += assert("Multiply", 1 * 4, 4);
  errors += assert("Multiply", 1 * 0, 0);
  errors += assert("Modulo",   3 % 2, 1);
  errors += assert("Modulo",   2 % 2, 0);
  
  // String functionality
  errors += assert("String length", length(""), 0);
  errors += assert("String length", length("Hello"), 5);  

  // Inline variable initialization
  var a = 10, b = 12;
  errors += assert("Variable initialization", a, 10);
  errors += assert("Variable initialization", b, 12);

  // Conditional operator
  errors += assert("Conditional", true  ? 1 : 2, 1);  
  errors += assert("Conditional", false ? 1 : 2, 2);

  // Associative arrays
  errors += assert("Array", [1,2][0], 1);
  errors += assert("Array", [1,2][1], 2);
  errors += assert("Array", count([1,2]), 2);
  errors += assert("Nested array", [1,[2,3]][0],    1);
  errors += assert("Nested array", [1,[2,3]][1][0], 2);
  errors += assert("Nested array", [1,[2,3]][1][1], 3);
  
  // For loops
  for(a = 0; a < 10; ++a) {}
  errors += assert("For", a, 10);
  
  // Conditions
  if(true) a = 1; else a = 2;
  errors += assert("If", a, 1);
  if(false) a = 1; else a = 2;
  errors += assert("If", a, 2);
  
  // Substring
  a = "Hello world";
  errors += assert("Substring", substr(a, 0, 5), "Hello");
  errors += assert("Substring", substr(a, 6),    "world");
  errors += assert("Strchr",    strchr(a, "H"),     0);
  errors += assert("Strchr",    strchr(a, "h"),    -1);
  errors += assert("Strchr",    strchr(a, "o"),     4);
  errors += assert("Strchr",    strchr(a, "o", 5),  7);
  errors += assert("Strstr",    strstr(a, "He"),    0);
  errors += assert("Strstr",    strstr(a, "wo"),    6);
  errors += assert("Strstr",    strstr(a, "wa"),   -1);
  errors += assert("Strstr",    strstr(a, "wo", 2), 6);


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


//////////////////////////////////////////////////////////////////////////
//
// Start by invoking main()
//

main();

// EOF