Console.WriteLn("'", /foo.bar/.Match("blafoo@barbla").Text, "'");

var match = /(.@)*/.Match("a1@2@1");
Console.WriteLn("'", match.Text, "'");
for(var m in match.Matches)
{
  Console.WriteLn("'", m, "'");
}
