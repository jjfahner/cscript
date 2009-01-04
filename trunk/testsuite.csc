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

//
// Includes
//
include "socket.csi";
include "file.csi";

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
  try
  {
    // Connect socket
    var sock = new Socket;
    sock.m_socket = 1;

    sock.Connect("jan-jaap.net", 80);

    // Build request
    var req = "GET / HTTP/1.0\r\n";
    req += "Host: jan-jaap.net\r\n";
    req += "User-Agent: cscript\r\n";
    req += "Connection: close\r\n\r\n";

    // Send data
    sock.Send(req, strlen(req));

    // Receive response
    var r = sock.Recv(100000, 5);

    // Disconnect socket
    sock.Close();

    // Succeeded
    return true;
  }
  catch(e)
  {
    return false;
  }
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

  // Create file
  var file = new File;

  // Open for write
  if(file.Open(fn, "w") === false)
  {
    return "Failed to open file for writing";
  }

  // Write string
  if(file.Write(to, strlen(to)) === false)
  {
    return "Failed to write to file";
  }

  // Close
  file.Close();

  // Open for read
  if(file.Open(fn, "r") === false)
  {
    return "Failed to open file for reading";
  }

  // Read string
  var res = file.Read(strlen(to));
  if(res === false)
  {
    return "Failed to read from file";
  }

  // Close the file
  file.Close();

  // Check read result
  return to == res;
}


//////////////////////////////////////////////////////////////////////////
//
// Exception handling
//

function try_catch()
{
  try
  {
    throw 1;
    return false;
  }
  catch(e)
  {
    return e == 1;
  }
  return false;
}

function try_finally()
{
  try
  {
    throw 1;
    return false;
  }
  finally
  {
    return true;
  }
  return false;
}

function try_catch_finally()
{
  var i = 0;
  try
  {
    throw 1;
    return false;
  }
  catch(e)
  {
    i += e;
  }
  finally
  {
    i += 1;
  }
  return i == 2;
}

//////////////////////////////////////////////////////////////////////////
//
// Test cases
//
function main()
{
  print("cscript test suite\n\n");

  // Reset error count
  errors = 0;

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
    b = 0;
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
  a = new File;
  a.m_file = 1;
  assert("Member", a.m_file, 1);

  // Files
  assert("Files", files(), true);
  
  // Test sockets
  //assert("Socket", sockets(), true);

  // Test regular expressions
  assert("Regex", match("aa", "aaa"), "aa");
  assert("Regex", match("^aa$", "aaa"), "");
  assert("Regex", match("aa.*zz", "aabbcczzaa"), "aabbcczz");
  assert("Regex", match("[a-zA-Z-]+", "jan-jaap"), "jan-jaap");
  assert("Regex", match("^[a-zA-Z-.]+(\\.[a-zA-Z-.])*@([a-zA-Z-]+\\.)+[a-zA-Z]+$", "jan-jaap@jan-jaap.net@"), "");
  assert("Regex", match("^[a-zA-Z-.]+(\\.[a-zA-Z-.])*@([a-zA-Z-]+\\.)+[a-zA-Z]+$", "jan-jaap@jan-jaap.net"), "jan-jaap@jan-jaap.net");

  // Exception handling
  assert("Try/catch", try_catch(), true);
  assert("Try/finally", try_finally(), true);
  assert("Try/catch/finally", try_catch_finally(), true);
  
  // Print result
  if(errors == 0)
  {
    print("\nAll tests succeeded!\n");
  }
  else
  {
    print("\n{errors} tests failed.\n");
  }

  // Print objects
  //print("\n\nBefore collect():\n");
  //dump();
  //collect();
  //print("\n\nAfter collect():\n");
  //dump();
}


//////////////////////////////////////////////////////////////////////////
//
// Start by invoking main()
//

main();

// Run garbage collector
print("Collecting...");
collect();
print("Done\n");

// EOF
