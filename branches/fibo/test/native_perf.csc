var total = 0;
var count = 100;

// Run native script 10 times
for (var i = 0; i < count; ++i)
{
  var ticks = CScript.Ticks;
  CScript.Exec("..\\bin\\test.exe native.csc > native.csc.gen.cpp");
  ticks = CScript.Ticks - ticks;
  total += ticks;
  Console.WriteLn(i, " : ", ticks, " ms, avg ", total / (i+1), " ms");
}

Console.WriteLn("Average: ", total / count, " ms");
