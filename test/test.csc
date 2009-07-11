function TestRE(re, st, ex)
{
  var mr = re.Match(st);
  if(mr.Success)
  {
    Console.WriteLn("Matched '", mr.Text, "' in '", st, "' at offset ", mr.Offset, " using pattern '", re.Pattern, "'");
    for(var c in mr.Captures) 
    {
      Console.WriteLn("  Capture: ", c);
    }
  }
  else
  {
    Console.WriteLn("Failed to match in '", st, "'");
  }
  Console.WriteLn("");
}

TestRE(/(.@)+|(.\d)+/, "a1@2@3%4@5@6@7@");
TestRE(/(ab|.)*/, "ababc");
TestRE(/(x+x+)+y/, "xxxxxxxxxy");
TestRE(/<a>(.*?)<\/a>/, "<a>blabla</a>blabla</a>");
TestRE(/[a-zA-Z0-9_]*/, "%abcAb0_1%");
TestRE(/(.@)+|(.\d)+/, "a1@2@3%4@5@6@7@", "a1@2@3%4@5@6@7");
TestRE(/[:digit:]+/, "abc123456789def");