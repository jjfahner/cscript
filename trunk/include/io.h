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
#ifndef CSCRIPT_IO_H
#define CSCRIPT_IO_H

#include "cscript.h"

//////////////////////////////////////////////////////////////////////////
//
// Base class for input methods
//
class Input 
{
public:

  //
  // Read a string
  //
  virtual std::string Read() = 0;

};

inline Input&
operator >> (Input& in, std::string& s)
{
  s = in.Read();
  return in;
}

//////////////////////////////////////////////////////////////////////////
//
// Base class for output methods
//
class Output 
{
public:

  //
  // Write a number.
  //
  virtual void Write(int64);

  //
  // Write a string
  //
  virtual void Write(char const*) = 0;

};

inline Output& 
operator << (Output& out, std::string const& s)
{
  out.Write(s.c_str());
  return out;
}

inline Output& 
operator << (Output& out, char const* s)
{
  out.Write(s);
  return out;
}

inline Output& 
operator << (Output& out, int64 n)
{
  out.Write(n);
  return out;
}

//////////////////////////////////////////////////////////////////////////
//
// Global stream objects
//

extern Input&   csin;
extern Output&  csout;
extern Output&  cserr;

#endif // CSCRIPT_IO_H
