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
#ifndef REGEX_H
#define REGEX_H

#pragma once 

#include <map>
#include <list>
#include <vector>
#include <string>

//////////////////////////////////////////////////////////////////////////
//
// Class Regex implements a hybrid NFA/DFA regular expression evaluator.
// The regular expression is converted to an NFA, which is then evaluated
// through an algorithm similar to the NFA -> DFA reduction algorithm.
// This approach avoids backtracking without the complexity of DFA subset
// construction. State for matching is kept strictly on the heap
// (no recursion is required at all), which means that there is hardly 
// any relevant intrinsic limit on the complexity of the expression.
//

class Regex 
{
public:

  //
  // Public types
  //
  typedef std::string         String;
  typedef std::vector<String> StringVec;

  //
  // Match results
  //
  struct MatchResults
  {
    String      m_complete;
    StringVec   m_backrefs;
  };

  //
  // Construction
  //
  Regex(char const* pattern = 0);

  //
  // Destruction
  //
  virtual ~Regex();

  //
  // Reset state
  //
  virtual void Reset();

  //
  // Match a string
  //
  virtual MatchResults Match(char const* source);

  //
  // Parse an expression
  //
  virtual void Parse(char const* pattern);

  //
  // Informational
  //
  size_t GetTransitionCount() const;

protected:

  //
  // Possible match types
  //
  enum MatchTypes
  {
    mtEmpty,    // Empty transition
    mtAnchor,   // Moving startpoint
    mtNestS,    // Start of group
    mtNestE,    // End of group
    mtChar,     // Match single char
    mtString,   // Match string
    mtAnyChar,  // Match any char
    mtEndLine,  // Match end of line/input
    mtBegLine,  // Match start of line/input
    mtRange,    // Match a range
    mtUCase,    // [A-Z]
    mtLCase,    // [a-z]
    mtAlpha,    // [A-Za-z]
    mtAlnum,    // [A-Za-z0-9]
    mtDigit,    // [0-9]
    mtXigit,    // [0-9a-fA-F]
    mtBlank,    // [ \t]
    mtSpace,    // [ \t\r\n]
    mtCntrl,    // Control characters
    mtGraph,    // [A-Za-z0-9] + punctuation
    mtPrint,    // Printable character
    mtRef1,     // Backref 1
    mtRef2,     // Backref 2
    mtRef3,     // Backref 3
    mtRef4,     // Backref 4
    mtRef5,     // Backref 5
    mtRef6,     // Backref 6
    mtRef7,     // Backref 7
    mtRef8,     // Backref 8
    mtRef9,     // Backref 9
  };

  //
  // State
  //
  typedef size_t State;

  //
  // Character range
  //
  struct CharRange
  {
    typedef std::pair<char, char> CharPair;
    typedef std::list<CharPair> PairList;
    
    bool      m_invert;
    PairList  m_pairs;
  };

  //
  // State transition
  //
  struct Transition
  {
    Transition(size_t outState, MatchTypes type = mtEmpty) :
      m_outState (outState), m_type (type), m_range (0) { }
    
    Transition(size_t outState, char wch) :
      m_outState (outState), m_type (mtChar), m_char (wch) { }

    size_t      m_outState;
    MatchTypes  m_type;
    union {
      char     m_char;
      CharRange*  m_range;
    };
  };

  //
  // Stack frame
  //
  struct StackFrame
  {
    StackFrame(State state = 0, char const* offset = 0, char const* source = 0) : 
      m_state   (state),
      m_offset  (offset),
      m_source  (source), 
      m_build   (0) 
    { 
    }

    void AddBackref() {
      m_backrefs.resize(m_backrefs.size() + 1);
    }
    void AppendBackref(char wch) {
      m_backrefs[m_backrefs.size() - 1] += wch;
    }

    State     m_state;
    char const*   m_offset;
    char const*   m_source;
    bool      m_build;
    StringVec m_backrefs;
  };

  //
  // Internal types
  //
  typedef std::list<Transition>             TransitionList;
  typedef std::vector<TransitionList*>      TransitionVec;
  typedef std::list<StackFrame>             Stack;
  typedef std::list<CharRange*>             CharRangeList;

  //
  // Parser methods
  //
  virtual void ParseExpression(char const*& pattern, State& is, State& os);
  virtual void ParseBranch(char const*& pattern, State& is, State& os);
  virtual void ParsePiece(char const*& pattern, State& is, State& os);
  virtual void ParseAtom(char const*& pattern, State& is, State& os);
  virtual void ParseNested(char const*& pattern, State& is, State& os);
  virtual void ParseQuantifier(char const*& pattern, size_t& min, size_t& max);
  virtual void ParseRange(char const*& pattern, State& min, State& max);

  //
  // Matcher methods
  //
  virtual bool MatchRange(CharRange const& range, char wch);
  virtual bool MatchCharClass(MatchTypes type, char wch);
  virtual char const* MatchBackref(MatchTypes backref, StackFrame const& frame);

  //
  // Add a transition
  //
  virtual void AddTransition(State inState, State outState, MatchTypes type = mtEmpty);
  virtual void AddCharTransition(State& inState, State& outState, char wch);
  virtual void AddTypeTransition(State& inState, State& outState, MatchTypes type);
  virtual TransitionList& GetTransListAt(size_t pos);


  //
  // Statics
  //
  static char const*    MatchTypeString(MatchTypes type);
  static MatchTypes ParseCharClass(char const* charClass);

  //
  // MemberMap
  //
  State           m_start;
  State           m_final;
  TransitionVec   m_transVec;
  State           m_seq;
  CharRangeList   m_ranges;

};

inline size_t 
Regex::GetTransitionCount() const
{
  return m_transVec.size();
}

inline
Regex::TransitionList&
Regex::GetTransListAt(size_t pos)
{
  if(m_transVec.size() <= pos)
  {
    size_t newsize = pos + 25;
    m_transVec.resize(newsize);
  }
  TransitionList*& list = m_transVec[pos];
  if(list == 0)
  {
    list = new TransitionList;
  }
  return *list;
}

#endif // #ifndef REGEX_H
