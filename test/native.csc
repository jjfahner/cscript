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
  Console.WriteLn("Parsing ", name);
  
  // Line parser
  var re = /^\s*(class|__native_[a-z]+)\s*(?:([a-zA-Z_][a-zA-Z0-9_]*|\d+|\(|\)|,|=|".*?")\s*)+/;

  // Current class instance
  var class;
  
  // Open file
  var f = new File;
  f.Open(name);

  // Read through file
  for(var l in f.Lines)
  {
    // Match line or continue
    var mr = re.Match(l);
    if(!mr.Success)
    {
      continue;
    }

    // Start of new class
    if(mr.Captures[0] == "__native_construct" ||
       mr.Captures[0] == "class")
    {
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
      class.name = mr.Captures[++ci];

      // Add to classes
      classes[class.name] = class;
      
      // Next iteration
      continue;
    }
    
    // Handle supported member types
    if(mr.Captures[0] == "__native_method")
    {
      ParseMethod(class, mr.Captures);
      continue;
    }
    if(mr.Captures[0] == "__native_roprop")
    {
      ParseRoProp(class, mr.Captures);
      continue;
    }
    if(mr.Captures[0] == "__native_rwprop")
    {
      ParseRwProp(class, mr.Captures);
      continue;
    }
    
    // Unknown native member type
    Console.WriteLn("Error: matched invalid native declaration '", mr.Text, "'");
  }
}

//////////////////////////////////////////////////////////////////////////
//
// Parse method
//

function ParseMethod(class, tokens)
{
  // Skip modifiers
  var i = 1;
  while(tokens[i] ~ /virtual|static/)
  {
    ++i;
  }
  
  // Create method
  var member = new Member;
  member.type = "method";
  
  // Find return type and name
  while(tokens[i] != "(")
  {
    member.type += member.name;
    member.name = tokens[i++];
  }

  // Append to class
  class.members[member.name] = member;  
  
  // Parse parameters
  for(++i; tokens[i] != ")";)
  {
    // Create new parameter
    var par = new Parameter;
    member.parameters.Append(par);
    
    // Read type and name
    while(tokens[i] !~ /[,)=]/)
    {
      par.type += par.name;
      par.name = tokens[i++];
    }
    
    // Read default value
    if(tokens[i] == "=")
    {
      var numparens = 0;
      for(++i; tokens[i] !~ /[,)]/; ++i)
      {
        if(tokens[i] == "(") {
          ++numparens;
        }
        if(tokens[i] == ")") {
          if(--numparens < 0) {
            break;
          }
        }
        par.def += tokens[i];
      }
    }
    
    // Skip comma
    if(tokens[i] == ",") {
      ++i;
    }
  }
}

//////////////////////////////////////////////////////////////////////////
//
// Parse method
//

function ParseRoProp(class, tokens)
{
}

//////////////////////////////////////////////////////////////////////////
//
// Parse method
//

function ParseRwProp(class, tokens)
{
}

//////////////////////////////////////////////////////////////////////////
//
// Global code
//

// Parse files
ParseFiles("../include");

for(var c in classes)
{
//   if(c.members.Length == 0)
//   {
//     continue;
//   }
  
  Console.WriteLn(c.name);
  
//   for(var m in c.members)
//   {
//      Console.WriteLn("  ", m.name);
//      for(var p in m.parameters)
//      {
//        Console.WriteLn("    ", p.type, " ", p.name, " ", p.def);
//      }
//   }
}
