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
  // Class name
  name : "",

  // Class is constructable
  constructable : false,

  // Class members
  members : []

};

//////////////////////////////////////////////////////////////////////////
//
// Member structure
//

var Member = 
{
  // Member name
  name : "",
  
  // Member type
  type    : "",
  
  // Return value
  returns : "",
  
  // Parameters
  parameters : []

};

//////////////////////////////////////////////////////////////////////////
//
// Class list
//

var classes = [];

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
  while(true)
  {
    // Read next line
    var l = f.ReadLn();
    if(f.Eof) 
    {
      f.Close();
      break;
    }

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
      var ci = 1;
      if(mr.Captures[0] == "__native_construct")
      {
        class.constructable = true;
        ++ci;
      }

      // Store class name
      class.name = mr.Captures[ci++];

      // Add to classes
      classes.Append(class);      
    }
    
    
  }
}

ParseFiles("../include");

for(var c in classes)
{
  Console.WriteLn(c.name);
}