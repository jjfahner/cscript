//////////////////////////////////////////////////////////////////////////
//
// This file is � 2009 - 2011 JJ Fahner <jan-jaap@jan-jaap.net>
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
#include <list.h>

class RegexData;
class LexStream;
class MatchResult;

//////////////////////////////////////////////////////////////////////////
//
// The main Regex class is a wrapper around a RegexImpl object. This
// makes it much easier to cache compiled regular expressions.
//

__native_construct class Regex : public Object
{
public:

  DEF_NATIVE_CALLS(Regex, Object);

  //
  // Construction
  //
  Regex(RegexData* rd = 0);

  //
  // Retrieve the pattern
  //
  __native_roprop StringCRef Pattern();

  //
  // Compile a regular expression
  //
  __native_method void Compile(ValueCRef pattern);
  
  //
  // Match a string
  //
  __native_method ObjectPtr Match(StringCRef text, int64 offset = 0);

  //
  // Simple match
  //
  __native_method bool IsMatch(StringCRef text, int64 offset = 0);

  //
  // Replace a substring
  //
  __native_method String Replace(StringCRef source, StringCRef replaceBy);

  //
  // Write table
  //
  __native_method String TableToString();

private:

  struct ImplResult
  {
    bool m_success;
    MatchResult* m_result;
  };

  //
  // Match implementation
  //
  ImplResult MatchImpl(StringCRef text, int64 offset, bool createMatchResult);

  //
  // Match a back reference
  //
  char const* MatchBackref(struct ReFrame& frame, size_t index);

  //
  // Object marking
  //
  virtual void MarkObjects(GCObjectVec& grey);

  //
  // Members
  //
  RegexData* m_rd;

};

//////////////////////////////////////////////////////////////////////////

class MatchResult : public Object
{
public:

  DEF_NATIVE_CALLS(MatchResult, Object);

  //
  // Construction
  //
  MatchResult() :
  m_success (false),
  m_matchId (0),
  m_offset  (0)
  {
  }

  //
  // Match result
  //
  __native_roprop bool Success()
  {
    return m_success;
  }

  //
  // Match id
  //
  __native_roprop int64 MatchId()
  {
    return m_matchId;
  }

  //
  // Full text
  //
  __native_roprop StringCRef Text()
  {
    return m_text;
  }

  //
  // Start of match
  //
  __native_roprop int64 Offset()
  {
    return m_offset;
  }

  //
  // List of captures
  //
  __native_roprop ObjectPtr Captures()
  {
    return m_captures;
  }
  
private:

  //
  // Garbage collection
  //
  virtual void MarkObjects(GCObjectVec& grey)
  {
    Object::MarkObjects(grey);
    GC::Mark(grey, m_captures);
  }

  //
  // Class is initialized by regex matcher
  //
  friend class Regex;

  //
  // Members
  //
  bool m_success;
  int64 m_matchId;
  String m_text;
  int64 m_offset;
  List* m_captures;

};

#endif // CSCRIPT_REGEX_H
