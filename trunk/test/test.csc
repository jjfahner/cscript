Console.WriteLn("'", /foo.bar/.Match("blafoo@barbla").Text, "'");

var match = /(.@)*|(.\d)*/.Match("a1@2@3%4@5@6@7@");
Console.WriteLn("'", match.Text, "'");
for(var m in match.Matches)
{
  Console.WriteLn("'", m, "'");
}

var f = new File();
f.Open("testsuite.csc", "r", false, false, false);

while(true)
{
  var line = f.ReadLn();
  if(f.Eof)
  {
    break;
  }
  
  if(/if/.IsMatch(line))
  {
    Console.WriteLn(line);
  }
}
