function TestRE(re, st, ex)
{
  //Console.WriteLn(re.TableToString());
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
    Console.WriteLn("Failed to match '", st, "' using pattern '", re.Pattern, "'");
  }
  Console.WriteLn("");
}

TestRE(/(.@)+|(.\d)+/, "a1@2@3%4@5@6@7@");
TestRE(/(ab|.)*/, "ababc");
TestRE(/(x+x+)+y/, "xxxxxxxxxy");
TestRE(/<([a-zA-Z]+)>(.*?)<\/\1>/, "blabla<div>blabla</div>blabla</div>");
TestRE(/[a-zA-Z0-9_]+/, "%abcAb0_1%");
TestRE(/(.@)+|(.\d)+/, "a1@2@3%4@5@6@7@", "a1@2@3%4@5@6@7");
TestRE(/[:digit:]+/, "abc123456789def");
TestRE(/^1?$|^(11+?)\1+$/, "11111111111");
TestRE(/^\s*(?:(__native_construct)\s+)?class\s+([a-zA-Z_][a-zA-Z0-9_]*)/, "class foo");
TestRE(/^\s*(?:__native_construct\s+)?class\s+?/, "  __native_method Value Eval(StringCRef code, bool isFile = false);");
TestRE(/^\s*__native_method\s+?/, "  __native_method Value Eval(StringCRef code, bool isFile = false);");
TestRE(/^\s*__native_method\s+(?:([a-zA-Z_][a-zA-Z0-9_]*|\d+|\(|\)|,|=|".*?"|:)\s*)+;$/, "  __native_method void Write(ArgsCRef args);");

var tab = [
  0  : "[ \t]+",
  1  : "if", 
  2  : "for", 
  3  : "while",
  4  : "do",
  5  : "class",
  6  : "[a-zA-Z_][a-zA-Z0-9_]*",
  7  : "[0-9]+",
  8  : "\\-",
  9  : "\\*",
  10 : "\\+",
  11 : "\\/",
  12 : "\\%",
  13 : "\\(",
  14 : "\\)",
  15 : ";",
  16 : "==",
  17 : "="
];

var re = new Regex;
re.Compile(tab);

var pos = 0;
var exp = "if(i + 1 == 2) j = 3;";
while(pos < exp.Length)
{
  var mr = re.Match(exp, pos);
  if(!mr.Success)
  {
    Console.WriteLn("Syntax error");
    break;
  }
  
  Console.WriteLn("Matched '", mr.Text, "' (id=", mr.MatchId, ")");
  pos += mr.Text.Length;
}
