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
  file          : "",
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
// Fatal error
//

function Error(message)
{
  Console.WriteLn("Error: ", message);
  CScript.Exit(1);
}

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

function ParseFile(filename)
{
  // Open file
  var file = new File;
  file.Open(filename);
  
  // Current class
  var class;

  // Read through file
  for(var line in file.Lines)
  {
    if(line ~ /^\s*(?:__native_construct\s+)?class\s+?/)
    {
      class = ParseClass(filename, line);
    }
    else if(line ~ /^\s*__native_method\s+?/)
    {
      ParseMethod(class, line);
    }
    else if(line ~ /^\s*__native_roprop\s+?/)
    {
      ParseRoProp(class, line);
    }
    else if(line ~ /^\s*__native_rwprop\s+?/)
    {
      ParseRwProp(class, line);
    }
  }
}

//////////////////////////////////////////////////////////////////////////
//
// Parse method
//

function ParseClass(file, line)
{
  // Extract name
  var name;
  if(line !~ /class\s+(?name:[a-zA-Z_][a-zA-Z0-9_]*)/)
  {
    return null;
  }

  // Create class instance
  class = new Class;
  class.name = name;
  class.file = file;
  class.constructable = line ~ /__native_construct/;

  // Add to classes
  classes[class.name] = class;
  
  // Return new class
  return class;
}

function ParseMethod(class, line)
{
  // Remove expected prefix strings
  line = /\s*(?:__native_method|virtual|static)\s*/.Replace(line, "");
  
  // Extract name and type
  var type, name;
  if(line !~ /^\s*(?type:[a-zA-Z_][a-zA-Z0-9_]*)
               \s+(?name:[a-zA-Z_][a-zA-Z0-9_]*)/)
  {
    return;
  }
  
  // Create method
  var member = new Member;
  member.type = "method";
  member.name = name;
  member.returns = type;
  
  // Append to class
  class.members[member.name] = member;
  
  // Parse parameters
  ParseParameters(line, member);
}

//////////////////////////////////////////////////////////////////////////
//
// Parse method
//

function ParseRoProp(class, line)
{
  // Remove expected prefix strings
  line = /\s*(?:__native_roprop|virtual|static)\s*/.Replace(line, "");
  
  // Extract name and type
  var type, name;
  if(line !~ /^\s*(?type:[a-zA-Z_][a-zA-Z0-9_]*)
               \s+(?name:[a-zA-Z_][a-zA-Z0-9_]*)/)
  {
    return;
  }
  
  // Create member
  var member = new Member;
  member.type = "roprop";
  member.name = name;
  member.returns = type;

  // Append to class
  class.members[member.name] = member;
}

//////////////////////////////////////////////////////////////////////////
//
// Parse method
//

function ParseRwProp(class, line)
{
  // Remove expected prefix strings
  line = /\s*(?:__native_rwprop|virtual|static)\s*/.Replace(line, "");
  
  // Extract name and type
  var type, name;
  if(line !~ /^\s*(?type:[a-zA-Z_][a-zA-Z0-9_]*)
               \s+(?name:[a-zA-Z_][a-zA-Z0-9_]*)/)
  {
    return;
  }
  
  // Create member
  var member = new Member;
  member.type = "rwprop";
  member.name = name;
  member.returns = type;

  // Append to class
  class.members[member.name] = member;
  
  // Parse parameters
  ParseParameters(line, member);
}

//////////////////////////////////////////////////////////////////////////
//
// Parse parameter block
//

function ParseParameters(line, member)
{
  // Extract parameters with default values
  var re = /\((?:
        \s*([a-zA-Z0-9_]+)\s+([a-zA-Z0-9_]+)\s*
        (?:(=)\s*((?:[a-zA-Z0-9_]+|\(.*?\)|".*?"|\s+)+))?
        ,?)*\)/;
  var mr = re.Match(line);

  // Append parameters
  var i = 0;
  while(i < mr.Captures.Length)
  {
    // Create new parameter
    var par = new Parameter;
    member.parameters.Append(par);
    
    // Set name and type
    par.type = mr.Captures[i++];
    par.name = mr.Captures[i++];

    // Set default value
    if(mr.Captures.Length > i && mr.Captures[i] == "=")
    {
      par.def = mr.Captures[++i];
      ++i;
    }
  }
}

//////////////////////////////////////////////////////////////////////////
//
// Code generation
//

function GenerateCode()
{
  // Generate file prolog
  Console.WriteLn("//////////////////////////////////////////////////////////////////////////");
  Console.WriteLn("//");
  Console.WriteLn("// Generated by native.csc for cscript");
  Console.WriteLn("//\n");  
  
  // Enumerate classes
  for(var c in classes)
  {
    // Skip classes with no members
    if(c.members.Length == 0)
    {
      continue;
    }

    // Write header
    Console.WriteLn("//////////////////////////////////////////////////////////////////////////");
    Console.WriteLn("//");
    Console.WriteLn("// Class ", c.name);
    Console.WriteLn("//\n");
    
    // Write header include
    Console.WriteLn("#include <", c.file.Replace("\\", "/"), ">\n");
   
    // Generate member stubs
    for(var m in c.members)
    {
      switch(m.type)
      {
      case "method":
        GenerateMethod(c, m);
        break;
      case "roprop":
        GenerateRoProp(c, m);
        break;
      case "rwprop":
        GenerateRwProp(c, m);
        break;
      default:
        Error("Invalid member type");
      }
    }
  }
}

//////////////////////////////////////////////////////////////////////////
//
// Generate method
//

function GenerateMethod(c, m)
{
  // Generate method prolog
  Console.WriteLn("static Value cscript_native_method_", c.name, "_", m.name);
  Console.WriteLn("  (Object* instance, Arguments const& arguments)");
  Console.WriteLn("\{");
  
  // Generate method call prolog
  Console.Write(m.returns == "void" ? "  " : "  return ");
  Console.Write("static_cast<", c.name, "*>(instance)->", m.name, "(");
  
  // Generate argument list
  var sep = " ";
  var arg = 0;
  for(var p in m.parameters)
  {
    if(p.type == "ArgsCRef")
    {
      Console.Write("\n    arguments", sep);
    }
    else if(p.def == "")
    {
      Console.Write("\n    ", sep, "cscript_arg_to_", p.type, "(arguments[", arg++, "])");
    }
    else
    {
      Console.Write("\n    ", sep, "arguments.size() < ", arg + 1, " ? ", 
          p.def, " : cscript_arg_to_", p.type, "(arguments[", arg, "])");
    }
    arg++;
    sep = ",";
  }
  Console.Write(");\n");
  
  // Generate method call epilog
  if(m.returns == "void")
  {
    Console.WriteLn("  return Value();");
  }
  
  // Generate method epilog
  Console.WriteLn("\}\n");
}

//////////////////////////////////////////////////////////////////////////
//
// Generate RO property
//

function GenerateRoProp(c, m)
{
  // Generate method prolog
  Console.WriteLn("static Value cscript_native_roprop_", c.name, "_", m.name);
  Console.WriteLn("  (Object* instance, Arguments const& arguments)");
  Console.WriteLn("\{");
  
  // Generate method call
  Console.WriteLn("  return static_cast<", c.name, "*>(instance)->", m.name, "();");
  
  // Generate method epilog
  Console.WriteLn("\}\n");
}

//////////////////////////////////////////////////////////////////////////
//
// Generate RW property
//

function GenerateRwProp(c, m)
{
  // Generate the getter
  GenerateRoProp(c, m);
  
  // Generate method prolog
  Console.WriteLn("static Value cscript_native_rwprop_", c.name, "_", m.name);
  Console.WriteLn("  (Object* instance, Arguments const& arguments)");
  Console.WriteLn("\{");
  
  // Generate method call
  Console.WriteLn("  static_cast<", c.name, "*>(instance)->Set", m.name, "(");
  Console.WriteLn("    cscript_arg_to_", m.returns, "(arguments[0]));");
  
  // Generate method epilog
  Console.WriteLn("  return Value();");
  Console.WriteLn("\}\n");
}

//////////////////////////////////////////////////////////////////////////
//
// Global code
//

//
// Dictionary of classes
//
var classes = {};

// Measure ticks
var ticks = CScript.Ticks;

try
{
  // Parse files
  ParseFiles("../include");

  // Generate output
  GenerateCode();
}
catch(e)
{
  // Write exception to output and exit
  Error(e.ToString());
}

// Write time elapsed and end of file marker
Console.WriteLn("// Generated in ", CScript.Ticks - ticks, " ms");
Console.WriteLn("// EOF");
