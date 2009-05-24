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
#include <stubs.h>
#include <variable.h>
#include <args.h>
#include <native.h>

class Value;
class Evaluator;

struct NativeCall
{
  typedef Value (*StubType)(Evaluator*, Arguments const&);

  char const* m_name;
  StubType    m_stub;
  ROVariable* m_var;
};


inline String const& __stub_arg_to_String(Value const& v)
{
  return v.GetString();
}

inline Value const& __stub_arg_to_Value(Value const& v)
{
  return v;
}

inline int __stub_arg_to_int(Value const& v)
{
  return (int) v.GetInt();
}

#include "stubs.gen.cpp"

bool 
NativeCallContainsKey(NativeCall* pTable, String const& key, bool checkProto)
{
  while(pTable->m_name)
  {
    if(key == pTable->m_name)
    {
      return true;
    }
    ++pTable;
  }
  return false;
}

bool 
NativeCallFind(NativeCall* pTable, String const& key, RValue*& pValue, bool checkProto)
{
  while(pTable->m_name)
  {
    if(key == pTable->m_name)
    {
      if(pTable->m_var == 0)
      {
        pTable->m_var = new ROVariable(
          new NativeFunction("", pTable->m_stub));
      }
      pValue = pTable->m_var;
      return true;
    }
    ++pTable;
  }
  return false;
}

