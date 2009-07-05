Console.WriteLn(/foo.bar/.Match("blafoo@barbla").Text);

var match = /(.@.)*/.Match("a1@1@2");
Console.WriteLn(match.Text);
for(var m in match.Matches)
{
  Console.WriteLn(m);
}
