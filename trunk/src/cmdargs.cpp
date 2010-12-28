//////////////////////////////////////////////////////////////////////////
//
// This file is © 2007 - 2009 JJ Fahner <jan-jaap@jan-jaap.net>
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
#include <cmdargs.h>
#include <cstring>

CmdArgs::CmdArgs(int argc, Char** argv)
{
  Char opt[3] = { '-', 0, 0 };

  String* val = 0;
  for(int i = 1; i < argc; ++i)
  {
    Char* ptr = argv[i];
    if(*ptr == '-')
    {
      if(*ptr == '-')
      {
        if(char* eq = strchr(ptr, '='))
        {
          m_args[String(ptr, eq - ptr)] = eq + 1;
        }
        else
        {
          val = &m_args[ptr];
        }
      }
      else
      {
        while(*ptr)
        {
          opt[1] = *ptr++;
          val = &m_args[opt];
        }
      }
    }
    else
    {
      if(val) 
      {
        *val = ptr;
         val = 0;
      }
      else
      {
        m_args[ptr];
      }
    }
  }
}

struct IsOpt {
  typedef std::pair<String const, String> argument_type;
  bool operator () (std::pair<String const, String> const& pair) const {
    return pair.first[0] == '-';
  }
};

StringMap
CmdArgs::GetValues() const
{
  StringMap result;
  std::remove_copy_if(m_args.begin(), m_args.end(), 
    std::inserter(result, result.end()), IsOpt());
  return result;
}

StringMap 
CmdArgs::GetOpts() const
{
  StringMap result;
  std::remove_copy_if(m_args.begin(), m_args.end(), 
    std::inserter(result, result.end()), std::not1(IsOpt()));
  return result;
}
