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
#include <native/consio.h>
#include <eval.h>

#include <set>
#include <list>
#include <iostream>

DEFINE_NATIVE_LINKAGE(Consio)

//////////////////////////////////////////////////////////////////////////
//
// Native call implementations
//

void PrintValue(Value const& val)
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
    break;

  default: 
    throw std::runtime_error("Invalid subtype");
  }

  Object::MemberIterator it = val->Begin();
  Object::MemberIterator ie = val->End();

  std::cout << "[";

  String sep;
  for(; it != ie; ++it)
  {
    std::cout << sep;
    sep = ",";
    PrintValue(it->first);
    std::cout << ":";
    if(it->second.Type() == Value::tObject)
    {
      std::cout << it->second.GetObject()->GetTypeName();
    }
    else
    {
      PrintValue(it->second.GetValue());
    }
  }

  std::cout << "]";
}

NATIVE_CALL("print(value)")
{
  PrintValue(args[0]);
  return args[0];
}

NATIVE_CALL("read()")
{
  String line;
  std::cin >> line;
  return Value(line);
}

//////////////////////////////////////////////////////////////////////////

void print_ast(Object& node, std::set<Object*>& done, int level)
{
  for(int i = 0; i < level; ++i)
  {
    std::cout << "  ";
  }

  std::list<Object*> next;

  std::cout << std::ios::hex << &node;
  for(int i = 0; i < 5; ++i)
  {
    // This is broken; members are now called "a{n}"
    if(node.ContainsKey(ValString(i)))
    {
      Value const& v = node[ValString(i)];
      std::cout << " [" << i << "] : ";
      switch(v.Type())
      {
      case Value::tNull:
        std::cout << "<null>";
        break;
      case Value::tBool:
        std::cout << v.GetBool();
        break;
      case Value::tInt:
        std::cout << v.GetInt();
        break;
      case Value::tString:
        std::cout << '"' << v.GetString() << '"';
        break;
      case Value::tObject:
        next.push_back(v.GetObject());
        std::cout << std::ios::hex << v.GetObject();
        break;
      }
    }
  }
  std::cout << "\n";

  std::list<Object*>::iterator it, ie;
  it = next.begin();
  ie = next.end();
  for(; it != ie; ++it)
  {
    if(!done.count(*it))
    {
      done.insert(*it);
      print_ast(**it, done, level + 1);
    }
  }
}

NATIVE_CALL("print_ast(node)")
{
  Object* root = args[0].GetObject();

  std::set<Object*> done;
  print_ast(*root, done, 0);

  return Value();
}
