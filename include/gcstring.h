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
#ifndef CSCRIPT_CSTRING_H
#define CSCRIPT_CSTRING_H

#include <cscript.h>
#include <gcobj.h>

//
// Garbage collected string class
//
class GCString : public GCSimpleObject, public String
{
public:
  
  //
  // Empty string
  //
  GCString() {}

  //
  // String from literal
  //
  GCString(char const* str) : String(str) 
  {
  }

  //
  // Construct from regular string
  //
  GCString(String const& str) : String(str) 
  {
  }

  //
  // Construct from literal, pin in gc
  //
  GCString(char const* str, bool);

  //
  // Construct from regular string, pin in gc
  //
  GCString(String const& str, bool);

};

//
// Macro for easily declaring pinned static strings
//
#define GCSTR(name, value) static GCString const * name = new GCString(value, true)

#endif // CSCRIPT_CSTRING_H
