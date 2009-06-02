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

void PrintValue(Value const& val, EvalRef evaluator, bool recurse);

void 
Console::Write(ArgsCRef args, EvalRef evaluator)
{
  for(size_t i = 0; i < args.size(); ++i)
  {
    PrintValue(args[i], evaluator, true);
  }
}

void 
Console::WriteLn(ArgsCRef args, EvalRef evaluator)
{
  for(size_t i = 0; i < args.size(); ++i)
  {
    PrintValue(args[i], evaluator, true);
  }
  std::cout << "\n";
}

String 
Console::Read()
{
  String line;
  std::cin >> line;
  return Value(line);
}

String 
Console::ReadChar()
{
  char c[2];
  std::cin.read(c, 1);
  c[1] = 0;
  return c;
}

//////////////////////////////////////////////////////////////////////////
//
// Native call implementations
//

void PrintValue(Value const& val, EvalRef evaluator, bool recurse)
{
  switch(val.Type())
  {
  case Value::tNull:      
    std::cout << "null"; 
    return;

  case Value::tBool:      
    std::cout << (val.GetBool() ? "true" : "false");
    return;

  case Value::tInt:       
    {
      std::cout << val.GetInt();
      return;
    }

  case Value::tString:    
    std::cout << val.GetString();
    return;

  case Value::tObject:
    if(!recurse)
    {
      std::cout << "[" << val.GetDataType()->TypeName() << " ]";
      return;
    }
    break;

  default: 
    throw std::runtime_error("Invalid subtype");
  }

  Value key, value;

  Arguments args;
  if(val->TryEval("ToString", &evaluator, args, value))
  {
    std::cout << value.GetString();
  }

  std::cout << "[";

  String sep;
  Enumerator* pEnum = val->GetEnumerator();
  while(pEnum->GetNext(key, value))
  {
    std::cout << sep;
    PrintValue(key, evaluator, false);
    std::cout << ":";
    PrintValue(value, evaluator, false);
    sep = ", ";
  }
  
  std::cout << "]";
}
