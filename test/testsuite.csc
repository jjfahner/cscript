// Start
Console.WriteLn("Starting test suite...\n");

// Location of scripts
var path = "testcases/";
var eval = true;

// Test function
function RunTests(doPrint)
{
  // Store start time
  var ticks = CScript.Ticks;
  
  // Enumerate all files in the test path
  var number = 0;
  var errors = 0;
  for(var file in Path.GetFiles(path + "*.csc"))
  {
    // Build path
    if(doPrint)
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
      if(doPrint)
      {
        Console.WriteLn("Ok");
      }
    }
    else
    {
      ++errors;
      if(doPrint)
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

// Run test suite
var n = CScript.IsDebugBuild ? 10 : 100;
for(var i = 0; i < n; ++i)
{
  RunTests(false);
}

// Run garbage collector
Console.WriteLn("\nCollecting...");
Console.WriteLn(CScript.Collect());

// Wait for input
Console.WriteLn("\nDone. Press any key to quit");
Console.ReadChar();

// EOF