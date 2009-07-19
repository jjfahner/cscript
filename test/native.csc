//////////////////////////////////////////////////////////////////////////
//
// This file is © 2009 JJ Fahner <jan-jaap@jan-jaap.net>
// This file is part of the cscript interpreter.
// CScript can be found at http://svn.jan-jaap.net/
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//
//////////////////////////////////////////////////////////////////////////


//////////////////////////////////////////////////////////////////////////
//
// Class structure
//

var Class = 
{
  name          : "",
  constructable : false,
  members       : null,

  constructor   : function()
  {
    this.members = new Dictionary;
  }
};

//////////////////////////////////////////////////////////////////////////
//
// Member structure
//

var Member = 
{
  name        : "",
  type        : "",
  returns     : "",
  parameters  : null,

  constructor : function()
  {
    this.parameters = new List;
  }
};

//////////////////////////////////////////////////////////////////////////
//
// Parameter structure
//

var Parameter = 
{
  name : "",
  type : "",
  def  : ""
};


//////////////////////////////////////////////////////////////////////////
//
// Class list
//

var classes = {};

//////////////////////////////////////////////////////////////////////////
//
// Enumerate path recursively, pass every .h to ParseFile
//

function ParseFiles(path)
{
  for(var d in Path.GetDirectories(path))
  {
    if(d.Substr(0, 1) == ".")
    {
      continue;
    }
    ParseFiles(Path.Combine(path, d));
  }
  for(var f in Path.GetFiles(path))
  {
    if(f ~ /\.h$/)
    {
      ParseFile(Path.Combine(path, f));
    }
  }
}

//////////////////////////////////////////////////////////////////////////
//
// Parse single file for class and class member declarations
//

function ParseFile(name)
{
  //Console.WriteLn("Parsing ", name);
  
  // Current class instance
  var class;
  
  // Open file
  var file = new File;
  file.Open(name);

  // Read through file
  for(var line in file.Lines)
  {
    if(line ~ /^\s*(?:__native_construct\s+)?class\s+?/)
    {
      ParseClass(line);
    }
    else if(line ~ /^\s*__native_method\s+?/)
    {
      ParseMethod(line);
    }
    else if(line ~ /^\s*__native_roprop\s+?/)
    {
      ParseRoProp(line);
    }
    else if(line ~ /^\s*__native_rwprop\s+?/)
    {
      ParseRwProp(line);
    }
  }
}

//////////////////////////////////////////////////////////////////////////
//
// Parse method
//

function ParseClass(line)
{
  // Line parser
  var re = /^\s*(?:(__native_construct)\s+)?class\s+([a-zA-Z_][a-zA-Z0-9_]*)/;

  // Split current line  
  var mr = re.Match(line);
  if(!mr.Success)
  {
    return;
  }
  
  // Create class instance
  class = new Class;

  // Handle constructable
  var ci = 0;
  if(mr.Captures[ci] == "__native_construct")
  {
    class.constructable = true;
    ++ci;
  }

  // Store class name
  class.name = mr.Captures[ci];

  // Add to classes
  classes[class.name] = class;
}

function ParseMethod(line)
{
  // Split line
  var re = /^\s*__native_method\s+(?:([a-zA-Z_][a-zA-Z0-9_]*|\d+|\(|\)|,|=|".*?")\s*)+/;
  var mr = re.Match(line);
  if(!mr.Success)
  {
    return;
  }
  
  // Skip modifiers
  var i = 1;
  while(mr.Captures[i] ~ /virtual|static/)
  {
    ++i;
  }
  
  // Create method
  var member = new Member;
  member.type = "method";
  
  // Find return type and name
  while(mr.Captures[i] != "(")
  {
    member.type += member.name;
    member.name = mr.Captures[i++];
  }

  // Append to class
  class.members[member.name] = member;  
  
  // Parse parameters
  for(++i; mr.Captures[i] != ")";)
  {
    // Create new parameter
    var par = new Parameter;
    member.parameters.Append(par);
    
    // Read type and name
    while(mr.Captures[i] !~ /[,)=]/)
    {
      par.type += par.name;
      par.name = mr.Captures[i++];
    }
    
    // Read default value
    if(mr.Captures[i] == "=")
    {
      var numparens = 0;
      for(++i; mr.Captures[i] !~ /[,)]/; ++i)
      {
        if(mr.Captures[i] == "(") {
          ++numparens;
        }
        if(mr.Captures[i] == ")") {
          if(--numparens < 0) {
            break;
          }
        }
        par.def += mr.Captures[i];
      }
    }
    
    // Skip comma
    if(mr.Captures[i] == ",") {
      ++i;
    }
  }
}

//////////////////////////////////////////////////////////////////////////
//
// Parse method
//

function ParseRoProp(line)
{
}

//////////////////////////////////////////////////////////////////////////
//
// Parse method
//

function ParseRwProp(line)
{
}

//////////////////////////////////////////////////////////////////////////
//
// Global code
//

var class;

// Parse files
var ticks = CScript.Ticks;
ParseFiles("../include");

for(var c in classes)
{
  if(c.members.Length == 0)
  {
    continue;
  }
 
  Console.WriteLn(c.name);
 
  for(var m in c.members)
  {
     Console.WriteLn("  ", m.name);
     for(var p in m.parameters)
     {
       Console.WriteLn("    ", p.type, " ", p.name, " ", p.def);
     }
  }
}

Console.WriteLn("\nExecuted in ", CScript.Ticks - ticks, " ms");
Console.ReadChar();
