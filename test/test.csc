// var re = /(.@)+|(.\d)+/;
// var mr = re.Match("a1@2@3%4@5@6@7@");

// var re = /(ab|.)*/;
// var mr = re.Match("ababc");

// var re = /(x+x+)+y/;
// var mr = re.Match("xxxxxxxxxy");

var re = /<a>.*?<\/a>/;
var mr = re.Match("<a>blabla</a>blabla</a>");

//Console.WriteLn(re.TableToString());

if(mr == null)
{
  Console.WriteLn("Failed to match input");
}
else
{
  // "a1@2@3%4@5@6@7@"
  Console.WriteLn("Matched '", mr.Text, "'");
}
