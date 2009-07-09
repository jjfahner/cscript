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
#ifndef CSCRIPT_REGEX_H
#define CSCRIPT_REGEX_H

#include <cscript.h>
#include <native.h>

class RegexImpl;
class RegexMatch;

//////////////////////////////////////////////////////////////////////////
//
// The main Regex class is a wrapper around a RegexImpl object. This
// makes it much easier to cache compiled regular expressions.
//

__native_construct class Regex : public Object
{
public:

  //
  // Construction
  //
  Regex();

  //
  // Compile a regular expression
  //
  __native_method void Compile(StringCRef pattern);
  
  //
  // Match a string
  //
  __native_method ObjectPtr Match(StringCRef text);

  //
  // Simple match
  //
  __native_method bool IsMatch(StringCRef text);

private:

  //
  // Object marking
  //
  virtual void MarkObjects(GCObjectVec& grey);

  //
  // Members
  //
  RegexImpl* m_impl;

};

//////////////////////////////////////////////////////////////////////////
//
// RegexImpl implements the regular expression matching logic.
//

class RegexImpl : public Object
{
public:

  //
  // Compile a pattern from source
  //
  static RegexImpl* Compile(String const& pattern);

  //
  // Match a string
  //
  ObjectPtr Match(StringCRef text);

  //
  // Simple match
  //
  bool IsMatch(StringCRef text);

};

#endif // CSCRIPT_REGEX_H
