// Open input file
var f = new File();
f.Open("../src/csparser.gen.in", "r", false, false, false);

// Read file contents
while(true)
{
  // Read next line
  var line = f.ReadLn();
  if(f.Eof) {
    break;
  }
  
  // Match definition lines
  if(!/::=/.IsMatch(line)) {
    continue;
  }

  /^(\s+|[a-zA-Z0-9_]+)*/.Match(line);

  // Split into tokens
//   var match = /^(\s+|::=|\(|\)|\.|\w+)*/.Match(line);
//   for(var m in match.Matches) {
//     Console.Write(m + " ");
//   }
//   Console.WriteLn("");
}
