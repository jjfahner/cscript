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
#include <native/console.h>
#include <args.h>
#include <eval.h>
#include <enumerator.h>
#include <datatype.h>
#include <iostream>

void PrintValue(std::ostream& stream, Value const& val, bool recurse = true);

Value
Console::ReadChar()
{
  // Read character
  char ch;
  std::cin.get(ch);

  // Check result
  if(std::cin.bad())
  {
    throw StreamBadState();
  }

  // Return char as integer
  return (int64) ch;
}

Value
Console::ReadString()
{
  // Read string
  char buf[4098];
  std::cin.get(buf, 4096);

  // Check result
  if(std::cin.bad())
  {
    throw StreamBadState();
  }

  // Create new string
  return GCString::Create(buf, std::cin.gcount());
}

Value 
Console::ReadLn()
{
  // Read string
  char buf[4098];
  std::cin.getline(buf, 4096);

  // Check result
  if(std::cin.bad())
  {
    throw StreamBadState();
  }

  // Create new string
  return GCString::Create(buf, std::cin.gcount());
}

void 
Console::WriteChar(ValueCRef value)
{
  // Write character
  std::cout.put((char)value.GetInt());

  // Check write result
  if(std::cout.bad())
  {
    throw StreamWriteFail();
  }
}

void 
Console::Write(ArgsCRef args)
{
  // Enumerate arguments
  Arguments::const_iterator it;
  for(it = args.begin(); it != args.end(); ++it)
  {
    // Write argument
    PrintValue(std::cout, *it);

    // Check write result
    if(std::cout.bad())
    {
      throw StreamWriteFail();
    }
  }
}

void 
Console::WriteLn(ArgsCRef args)
{
  // Delegate to Write
  Write(args);

  // Send newline
  std::cout << std::endl;

  // Check write result
  if(std::cout.bad())
  {
    throw StreamWriteFail();
  }
}

//////////////////////////////////////////////////////////////////////////
//
// Native call implementations
//

void PrintValue(std::ostream& stream, Value const& val, bool recurse)
{
  switch(val.Type())
  {
  case Value::tNull:      
    stream << "null"; 
    return;

  case Value::tBool:      
    stream << (val.GetBool() ? "true" : "false");
    return;

  case Value::tInt:       
    {
      stream << val.GetInt();
      return;
    }

  case Value::tString:    
    stream << val.GetString();
    return;

  case Value::tObject:
    if(!recurse)
    {
      stream << "[" << val.GetDataType()->TypeName() << " ]";
      return;
    }
    break;

  default: 
    throw std::runtime_error("Invalid subtype");
  }

  Value key, value;

  Arguments args;
  if(val->TryEval("ToString", args, value))
  {
    stream << value.GetString();
  }

  stream << "[";

  String sep;
  Enumerator* pEnum = val->GetEnumerator();
  while(pEnum->GetNext(key, value))
  {
    stream << sep;
    PrintValue(stream, key, false);
    stream << ":";
    PrintValue(stream, value, false);
    sep = ", ";
  }
  
  stream << "]";
}
