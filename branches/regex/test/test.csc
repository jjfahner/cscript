function TestRE(re, st)
{
  var mr = re.Match(st);
  if(mr.Success)
  {
    Console.WriteLn("Matched '", mr.Text, "' in '", st, "'");
  }
  else
  {
    Console.WriteLn("Failed to match in '", st, "'");
  }
}

TestRE(/(.@)+|(.\d)+/, "a1@2@3%4@5@6@7@");
TestRE(/(ab|.)*/, "ababc");
TestRE(/(x+x+)+y/, "xxxxxxxxxy");
TestRE(/<a>.*?<\/a>/, "<a>blabla</a>blabla</a>");
TestRE(/[a-zA-Z0-9_]*/, "%abcAb0_1%");
