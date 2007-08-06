
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

//
// Error count
//
var errors = 0;

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
  }
  else
  {
    print("Failed\n");
    ++errors;
  }
}

//////////////////////////////////////////////////////////////////////////
//
// Test cases
//
function main()
{
  print("cscript test suite\n\n");

  // To fool the optimizer, use variables
  // instead of literals in expressions.
  var a0 = 0;
  var a1 = 1;
  var a2 = 2;
  var a3 = 3;
  var a4 = 4;

  // Arithmetic operations
  assert("Add",       a2 + a2, a4);
  assert("Add",      -a2 + a4, a2);
  assert("Subtract",  a2 - a2, a0);
  assert("Subtract",  a2 - a4, -a2);
  assert("Subtract",  a4 - a2, a2);
  assert("Subtract",  a2 - a2, a0);
  assert("Multiply",  a2 * a2, a4);
  assert("Multiply",  a1 * a4, a4);
  assert("Multiply",  a1 * a0, a0);
  assert("Modulo",    a3 % a2, a1);
  assert("Modulo",    a2 % a2, a0);
  
  // Inline variable initialization
  var a = 10, b = 12;
  assert("Variable initialization", a, 10);
  assert("Variable initialization", b, 12);

  // Conditional operator
  assert("Conditional", true  ? 1 : 2, 1);  
  assert("Conditional", false ? 1 : 2, 2);

  // Associative arrays
  assert("Array", [1,2][0], 1);
  assert("Array", [1,2][1], 2);
  assert("Array", count([1,2]), 2);
  assert("Nested array", [1,[2,3]][0],    1);
  assert("Nested array", [1,[2,3]][1][0], 2);
  assert("Nested array", [1,[2,3]][1][1], 3);
  
  // For loops
  for(a = 0; a < 10; ++a) {}
  assert("For", a, 10);
  
  // Conditions
  if(true) a = 1; else a = 2;
  assert("If", a, 1);
  if(false) a = 1; else a = 2;
  assert("If", a, 2);
  
  // Substring
  a = "Hello world";
  assert("substr", substr(a, 0, 5), "Hello");
  assert("substr", substr(a, 6),    "world");
  assert("strlen", strlen(a),         11);
  assert("strchr", strchr(a, "H"),     0);
  assert("strchr", strchr(a, "h"),    -1);
  assert("strchr", strchr(a, "o"),     4);
  assert("strchr", strchr(a, "o", 5),  7);
  assert("strstr", strstr(a, "He"),    0);
  assert("strstr", strstr(a, "wo"),    6);
  assert("strstr", strstr(a, "wa"),   -1);
  assert("strstr", strstr(a, "wo", 2), 6);
  
  // Logical or
  assert("Logical or", true  || false, true);
  assert("Logical or", false || true,  true);
  assert("Logical or", true  || true,  true);
  assert("Logical or", false || false, false);
  
  // Logical and
  assert("Logical and", true  && true,  true);
  assert("Logical and", true  && false, false);
  assert("Logical and", false && true,  false);
  assert("Logical and", false && false, false);
  
  // Short-circuited logical or/and
  a = 0; ++a || ++a;
  assert("short-circuit or", a, 1);
  a = 0; a++ && a++;
  assert("short-circuit and", a, 1);
  
  // Switch statement
  switch(0) {
  case 0 : a = 0;
  case 1 : a = 1;
  default: a = 2;
  }
  assert("Switch", a, 0);
  switch(1) {
  case 0 : a = 0;
  case 1 : a = 1;
  default: a = 2;
  }
  assert("Switch", a, 1);
  switch(2) {
  case 0 : a = 0;
  case 1 : a = 1;
  default: a = 2;
  }
  assert("Switch", a, 2);
  
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