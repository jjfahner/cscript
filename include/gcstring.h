//////////////////////////////////////////////////////////////////////////
//
// This file is © 2009 - 2011 JJ Fahner <jan-jaap@jan-jaap.net>
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
  // Perform garbage collection
  //
  static void Collect(bool full = false);

  //
  // Construct empty
  //
  static GCString* Create();

  //
  // Construct from char*
  //
  static GCString* Create(char const* str, size_t len = 0, bool pin = false);

  //
  // Construct from String&
  //
  static GCString* Create(String const& str, bool pin = false);

  //
  // Implementation
  //
private:
  
  friend class StringTable;

  //
  // Empty string
  //
  GCString();

  //
  // Construct from literal, pin in gc
  //
  GCString(char const* str, size_t len, bool pin);

};

#endif // CSCRIPT_CSTRING_H
