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
#ifndef CSCRIPT_NATIVE_PATH_H
#define CSCRIPT_NATIVE_PATH_H

#include <cscript.h>
#include <object.h>
#include <stubs.h>

class Path : public Object
{
public:

  DEF_NATIVE_CALLS(Path, Object);

  //
  // Path exists
  //
  __native_method static bool Exists(StringCRef path);

  //
  // Path refers to file
  //
  //_native_method static bool IsFile(StringCRef path);

  //
  // Path is absolute
  //
  __native_method static bool IsAbsolute(StringCRef path);

  //
  // Combine two path strings
  //
  __native_method static String Combine(StringCRef left, StringCRef right);

  //
  // Retrieve directory part of path
  //
  __native_method static String DirectoryPart(StringCRef path);

  //
  // Current working directory
  //
  __native_roprop static String WorkingDirectory();

};

#endif // CSCRIPT_NATIVE_PATH_H
