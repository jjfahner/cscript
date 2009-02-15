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
#ifndef CSCRIPT_CMDARGS_H
#define CSCRIPT_CMDARGS_H

#include "cscript.h"

typedef std::map<String, String> StringMap;

class CmdArgs
{
public:

  //
  // Construction
  //
  CmdArgs(int argc, Char** argv);

  //
  // Number of arguments
  //
  size_t GetCount() const;

  //
  // Is option set
  //
  bool IsSet(String const& name) const;

  //
  // Option value
  //
  String const& GetValue(String const& name) const;

  //
  // Shortcurt for GetValue
  //
  String const& operator [] (String const& name) const;

  //
  // List of regular args
  //
  StringMap GetValues() const;

  //
  // List of option names
  //
  StringMap GetOpts() const;
  
private:

  //
  // MemberMap
  //
  StringMap m_args;

};

inline size_t 
CmdArgs::GetCount() const
{
  return m_args.size();
}

inline bool 
CmdArgs::IsSet(String const& name) const
{
  return m_args.count(name) > 0;
}

inline String const&
CmdArgs::GetValue(String const& name) const
{
  static const String dummy;
  StringMap::const_iterator it;
  if((it = m_args.find(name)) == m_args.end())
  {
    return dummy;
  }
  return it->second;
}

inline String const& 
CmdArgs::operator [] (String const& name) const
{
  return GetValue(name);
}

#endif // #ifndef CSCRIPT_CMDARGS_H
