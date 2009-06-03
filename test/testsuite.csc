// Start
Console.WriteLn("Starting test suite...\n");

// Location of scripts
var path = "../test/testcases/";

// Enumerate all files in the test path
var number = 0;
var errors = 0;
for(var file in Path.GetFiles(path + "*.csc"))
{
  // Build path
  Console.Write("Executing {file}... ");

  // Execute the script
  var res = CScript.Exec("test.exe \"{path}{file}\"");
  if(res == 1)
  {
    Console.WriteLn("Ok");
  }
  else
  {
    ++errors;
    Console.WriteLn("Failed");
  }
  ++number;
}

// Done
Console.WriteLn("\nTest suite complete");
Console.WriteLn("Total:  {number}");
Console.WriteLn("Errors: {errors}");

Console.ReadChar();
// EOF