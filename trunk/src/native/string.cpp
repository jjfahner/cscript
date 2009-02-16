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
#include <native/string.h>
#include <eval.h>

DEFINE_NATIVE_LINKAGE(String)

//////////////////////////////////////////////////////////////////////////

NATIVE_CALL("__native strlen(string value)")
{
  return Value((Value::Int)args[0].GetString().length());
}

NATIVE_CALL("__native substr(string data, int start, int len = 0)")
{
  String::size_type off = (String::size_type)args[1].GetInt();
  String::size_type len = (String::size_type)args[2].GetInt();
  if(len == 0) len = String::npos;
  return Value(args[0].GetString().substr(off, len));
}

NATIVE_CALL("__native strstr(string data, string what, int start = 0)")
{
  String const& src = args[0].GetString();
  String const& str = args[1].GetString();
  int32 offset = (int32)args[2].GetInt();
  if(str.length() < 1)
  {
    throw std::runtime_error("Empty string in call to strstr");
  }
  size_t res = src.find(str, offset);
  if(res == String::npos)
  {
    return Value(-1);
  }
  return Value((Value::Int)res);
}

NATIVE_CALL("__native strchr(string data, string char, int start = 0)")
{
  String const& src = args[0].GetString();
  String const& chr = args[1].GetString();
  int32 offset = (int32)args[2].GetInt();
  if(chr.length() < 1)
  {
    throw std::runtime_error("Empty string in call to strchr");
  }
  size_t res = src.find(chr[0], offset);
  if(res == String::npos)
  {
    return Value(-1);
  }
  return Value((Value::Int)res);
}
