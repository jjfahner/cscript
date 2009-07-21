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
#ifndef CSCRIPT_REGEX_COMPILER_H
#define CSCRIPT_REGEX_COMPILER_H

#include <cscript.h>
#include <object.h>
#include <gc.h>

#include <vector>
#include <list>
#include <map>

class Dictionary;
class LexStream;

enum TransitionTypes
{
  ttNone,
  ttEmpty,
  ttFinal,
  ttOffset,
  ttAnchorL,
  ttAnchorR,
  ttCaptureL,
  ttCaptureR,
  ttBackref,
  ttPushNum,
  ttTestNum,
  ttIncNum,
  ttPopNum,
  ttAny,
  ttChar,
  ttRange,
  ttNChar,
  ttNRange,
  ccAlnum,
  ccAlpha,
  ccBlank,
  ccCntrl,
  ccDigit,
  ccGraph,
  ccLower,
  ccPrint,
  ccPunct,
  ccSpace,
  ccUpper,
  ccXdigit
};

struct Pair
{
  size_t m_min;
  size_t m_max;
};

typedef size_t State;

//////////////////////////////////////////////////////////////////////////
//
// Transition to next state over match
//

struct Transition
{
  //
  // Construction
  //
  Transition(State out = 0, TransitionTypes type = ttEmpty, char min = 0, char max = 0) :
  m_type  (type), 
  m_out   (out),
  m_min   (min),
  m_max   (max)
  {
  }

  //
  // Compare transition type
  //
  bool operator == (TransitionTypes type)
  {
    return m_type == type;
  }

  //
  // Compare transition type
  //
  bool operator != (TransitionTypes type)
  {
    return m_type != type;
  }

  //
  // Compare to other transition
  //
  bool operator == (Transition const& rhs)
  {
    return m_type == rhs.m_type &&
           m_out  == rhs.m_out  &&           
           m_min  == rhs.m_min  &&
           m_max  == rhs.m_max  ;
  }

  //
  // Compare to other transition
  //
  bool operator != (Transition const& rhs)
  {
    return ! (*this == rhs);
  }

  //
  // Convert to string
  //
  String ToString() const;

  //
  // Members
  //
  State           m_out  : 16;
  TransitionTypes m_type : 8;
  unsigned char   m_min  : 8;  
  unsigned char   m_max  : 8;

};

typedef std::vector<Transition> TransitionVec;
typedef std::list<Transition> TransitionList;
typedef std::vector<TransitionList> StateVec;
typedef std::map<int64, String> NamedCaptureMap;

//////////////////////////////////////////////////////////////////////////
//
// Compiled regular expression
//

class RegexData : public GCSimpleObject
{
public:

  // Pattern string
  String m_pattern;

  // Transitions table
  TransitionVec m_table;

  // Named captures
  NamedCaptureMap m_namedCaptures;

};

//////////////////////////////////////////////////////////////////////////
//
// Regular expression compiler
//

class RegexCompiler
{
public:

  //
  // Construction
  //
  RegexCompiler();

  //
  // Compile a regular expression from string
  //
  RegexData* Compile(String const& string, int64 exId = 0);

  //
  // Compile a regular expression from stream
  //
  RegexData* Compile(LexStream& stream, int64 exId = 0);

  //
  // Compile a set of regular expressions
  //
  RegexData* Compile(Dictionary* dict);

  //
  // Create a new state
  //
  State AddState();

  //
  // Add transition
  //
  void AddTransition(State in, State out, TransitionTypes type = ttEmpty, char min = 0, char max = 0, bool append = true);

  //
  // Add an atom
  //
  void AddAtom(Pair& result, TransitionTypes type, char min = 0, char max = 0);

  //
  // Add a sequence between lhs and rhs
  //
  void AddSequence(Pair const& lhs, Pair const& rhs, Pair& result);

  //
  // Add an alternation between lhs and rhs
  //
  void AddAlternation(Pair const& lhs, Pair const& rhs, Pair& result);

  //
  // Add a capturing subexpression
  //
  void AddCapture(Pair const& e, Pair& result);

  //
  // Add a named capture
  //
  void AddNamedCapture(Pair const& e, Pair& r);

  //
  // Add a back reference
  //
  void AddBackref(char num, Pair& result);

  //
  // Add a character to the current identifier
  //
  void AddIdentifierChar(char ch);

  //
  // Quantify an expression
  //
  void AddQuantifier(Pair const& expression, int min, int max, bool greedy, Pair& result);

  //
  // Finalize the expression
  //
  void Finalize(Pair const& result);

  //
  // Called when there is a syntax error
  //
  void OnSyntaxError(char ch);

private:

  //
  // Compile the current stream into the table
  //
  void CompileImpl(LexStream& stream, int64 exId = 0);

  //
  // Optimize the transition table
  //
  void Optimize();

  //
  // Find all non-empty transitions
  //
  void FindTransitions(TransitionList const& in, TransitionVec& out);

  //
  // Members
  //
  String          m_pattern;
  String          m_ident;
  StateVec        m_table;
  TransitionVec   m_vec;
  int64           m_exId;
  int64           m_numCaptures;
  NamedCaptureMap m_namedCaptures;
};

#endif // CSCRIPT_REGEX_COMPILER_H

