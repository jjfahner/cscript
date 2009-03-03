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
#include <gc.h>

class Value;

//
// Garbage collected string class
//
class GCString : public GC::Object, public String
{
public:
  
  //
  // Empty string
  //
  GCString() : GC::Object(false), m_refs(0) 
  {
  }

  //
  // String from literal
  //
  GCString(char const* str) : GC::Object(false), String(str), m_refs(0)
  {
  }

  //
  // Construct from regular string
  //
  GCString(String const& str) : GC::Object(false), String(str), m_refs(0)
  {
  }

  //
  // Construct from literal, pin in gc
  //
  GCString(char const* str, bool) : GC::Object(false), String(str), m_refs(0)
  { 
    GC::Pin(this); 
  }

  //
  // Construct from regular string, pin in gc
  //
  GCString(String const& str, bool) : GC::Object(false), String(str), m_refs(0)
  {
    GC::Pin(this);  
  }

private:

  friend class Value;

  void AddRef() const
  {
    if(++m_refs == 2)
    {
      Register();
    }
  }

  //
  // References
  //
  mutable size_t m_refs;

};

//
// Macro for easily declaring pinned static strings
//
#define GCSTR(name, value) static GCString const * name = new GCString(value, true)

#endif // CSCRIPT_CSTRING_H
