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

struct Info
{
  var a, b;
  var d = 1;
};

struct Info2
{
};

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
  var test = null;
  print("{description}...");
  if(result == expected)
  {
    print("Ok\n");
  }
  else
  {
    print("Failed - expected '{expected}', got '{result}'\n");
    ++errors;
  }
}

//////////////////////////////////////////////////////////////////////////
//
// Test sockets functionality
//
function sockets()
{
  // Create socket
  var s = socket("jan-jaap.net", 80);
  if(s === false)
  {
    return 0;
  }

  // Build request
  var req = "GET / HTTP/1.0\r\n";
  req += "Host: jan-jaap.net\r\n";
  req += "User-Agent: cscript\r\n";
  req += "Connection: close\r\n\r\n";

  // Send request
  if(send(s, req) === false)
  {
    return 0;
  }

  // Receive response
  var r = recv(s, 100000);
  if(r === false)
  {
    return 0;
  }

  // Disconnect socket
  closesocket(s);

  // Done
  return 1;
}

//////////////////////////////////////////////////////////////////////////
//
// Test file functionality
//
function files()
{
  // Define some values
  var fn = "testfile.txt";
  var to = "Hello world";

  // Open the file
  var f = fopen(fn, "w");
  if(f === false)
  {
    return false;
  }

  // Write and close
  var res = fwrite(f, to);
  fclose(f);

  // Check write result
  if(res === false)
  {
    return false;
  }

  // Reopen the file
  f = fopen(fn, "r");
  if(f === false)
  {
    return false;
  }

  // Read and close
  var tr = fread(f, strlen(to));
  fclose(f);

  // Check read result
  if(tr === false)
  {
    return false;
  }

  // Compare written and read values
  return to == tr;
}

//////////////////////////////////////////////////////////////////////////
//
// Test cases
//
function main()
{
  print("cscript test suite\n\n");

  // Number formats
  assert("Binary literal", 0b0000000, 0);
  assert("Binary literal", 0b0000001, 1);
  assert("Binary literal", 0b0000010, 2);
  assert("Binary literal", 0b0000011, 3);
  assert("Binary literal", 0b0000111, 7);  
  assert("Hex literal",    0x0000000, 0);
  assert("Hex literal",    0x0000001, 1);
  assert("Hex literal",    0x0000010, 16);
  assert("Hex literal",    0x0000011, 17);
  assert("Hex literal",    0x000000a, 10);
  assert("Roman literal",  0rIIII, 4);
  assert("Roman literal",  0rIV, 4);
  assert("Roman literal",  0rMCMXCIX, 1999);
  assert("Roman literal",  0rMCMLXXXIV, 1984);
  assert("Roman literal",  0rMDCCCLXXXVIII, 1888);

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
  
  // For loop
  for(a = 0; a < 10; ++a) {}
  assert("For", a, 10);

  // Break in for loop
  for(a = 0; a < 10; ++a)
  {
    if(a == 5) break;
  }
  assert("For break", a, 5);
  
  // Continue in for loop
  var b = 0;
  for(a = 0; a < 10; ++a)
  {
    if(a >= 5) continue;
    ++b;
  }
  assert("For continue", b, 5);

  // Foreach loop
  a = 0;
  for(var it in [0, 1, 2, 3, 4, 5])
  {
    a += it;
  }
  assert("For each", a, 15);

  // Foreach break
  a = 0;
  for(var it in [0, 1, 2, 3, 4, 5])
  {
    if(it == 2) break;
    a += it;
  }
  assert("For each break", a, 1);

  // Foreach continue
  a = 0;
  for(var it in [0, 1, 2, 3, 4, 5])
  {
    if(it == 2) continue;
    a += it;
  }
  assert("For each continue", a, 13);

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

  // Boolean inversion
  var t = true;
  var f = false;
  assert("Not", !t, false);
  assert("Not", !f, true);
  
  // Short-circuited logical or/and
  a = 0; ++a || ++a;
  assert("short-circuit or", a, 1);
  a = 0; a++ && a++;
  assert("short-circuit and", a, 1);
  
  // Switch statement
  a = -1;
  switch(0) {
  case 0 : a = 0; break;
  case 1 : a = 1; break;
  default: a = 2; break;
  }
  assert("Switch", a, 0);
  a = -1;
  switch(1) {
  case 0 : a = 0; break;
  case 1 : a = 1; break;
  default: a = 2; break;
  }
  assert("Switch", a, 1);
  a = -1;
  switch(2) {
  case 0 : a = 0; break;
  case 1 : a = 1; break;
  default: a = 2; break;
  }
  assert("Switch", a, 2);
  a = -1;
  switch(1)
  {
  case 0 : a = 0; break;
  case 1 : a = 1; break; a = 3; break;
  default: a = 2; break;
  }
  assert("Break", a, 1);

  // Member syntax
  a.foo = 2;
  a.bar = 4;
  assert("Member", a.foo * a.bar, 8);

  // Files
  assert("Files", files(), true);
  
  // Test sockets
  assert("Socket", sockets(), 1);

  // Test regular expressions
  assert("Regex", match("aa", "aaa"), "aa");
  assert("Regex", match("^aa$", "aaa"), "");
  assert("Regex", match("aa.*zz", "aabbcczzaa"), "aabbcczz");
  assert("Regex", match("[a-zA-Z-]+", "jan-jaap"), "jan-jaap");
  assert("Regex", match("^[a-zA-Z-.]+(\\.[a-zA-Z-.])*@([a-zA-Z-]+\\.)+[a-zA-Z]+$", "jan-jaap@jan-jaap.net@"), "");
  assert("Regex", match("^[a-zA-Z-.]+(\\.[a-zA-Z-.])*@([a-zA-Z-]+\\.)+[a-zA-Z]+$", "jan-jaap@jan-jaap.net"), "jan-jaap@jan-jaap.net");

  // Print result
  if(errors == 0)
  {
    print("\nAll tests succeeded!\n");
  }
  else
  {
    print("\n{errors} tests failed.\n");
  }
}


//////////////////////////////////////////////////////////////////////////
//
// Start by invoking main()
//

main();

// EOF
