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

// Execution parameters
var path = "testcases/";
var eval  = true;
var print = !eval;
var count = 100;

// Test function
function RunTests()
{
  // Store start time
  var ticks = CScript.Ticks;
  
  // Enumerate all files in the test path
  var number = 0;
  var errors = 0;
  for(var file in Path.GetFiles(path + "*.csc"))
  {
    // Build path
    if(print)
    {
      Console.Write("{file}: ");
    }
  
    // Execute the script
    var res;
    if(eval)
    {
      res = CScript.Eval("{path}{file}", true);
    }
    else
    {
      var exe = CScript.IsDebugBuild ? "testd.exe" : "test.exe";
      res = CScript.Exec("..\\bin\\{exe} \"{path}{file}\"");
    }
  
    // Check result
    if(res == 1)
    {
      if(print)
      {
        Console.WriteLn("Ok");
      }
    }
    else
    {
      ++errors;
      if(print)
      {
        Console.WriteLn("Failed");
      }
    }
    ++number;
  }
  
  // Store start time
  ticks = CScript.Ticks - ticks;
  
  // Done
  Console.WriteLn("\nTest suite complete");
  Console.WriteLn("Total:  {number}");
  Console.WriteLn("Errors: {errors}");
  Console.WriteLn("Time:   {ticks} ms");
  
  // Return ticks
  return ticks;
}

// Start
Console.WriteLn("Starting test suite...\n");

// Run test suite
for(var i = 0; i < count; ++i)
{
  RunTests();
}

// Run garbage collector
Console.WriteLn("\nCollecting...");
Console.WriteLn(CScript.Collect());

// Wait for input
Console.WriteLn("\nDone. Press any key to quit");
Console.ReadChar();

// EOF