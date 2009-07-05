Console.WriteLn("'", /foo.bar/.Match("blafoo@barbla").Text, "'");

var match = /(.@)*/.Match("a1@2@3%4@5@6@7@");
Console.WriteLn("'", match.Text, "'");
for(var m in match.Matches)
{
  Console.WriteLn("'", m, "'");
}
