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
  ttAny,
  ttChar,
  ttRange,
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
  Transition(State out = 0, TransitionTypes type = ttEmpty, char min = 0, char max = 0) :
    m_type  (type), 
    m_out   (out),
    m_min   (min),
    m_max   (max),
    m_next  (0)
  {
  }

  Transition(Transition const& rhs)
  {
    *this = rhs;
    m_next = 0;
  }

  Transition* Copy(Transition* next)
  {
    Transition* t = new Transition(*this);

    Transition* c = t;
    while(c->m_next)
    {
      c->m_next = new Transition(*c->m_next);
      c = c->m_next;
    }
    
    return t;
  }

  void Append(Transition* p)
  {
    Transition* t = this;
    while(t->m_next)
    {
      t = t->m_next;
    }
    t->m_next = p;
  }

  //
  // Find a transition type in the list
  //
  Transition* Find(TransitionTypes type)
  {
    Transition* t = this;
    while(t && t->m_type != type)
    {
      t = t->m_next;
    }
    return t;
  }

  //
  // Find a transition in the list
  //
  Transition* Find(Transition const& p)
  {
    Transition* t = this;
    while(t && *t != p)
    {
      t = t->m_next;
    }
    return t;
  }

  bool operator == (TransitionTypes type)
  {
    return m_type == type;
  }

  bool operator != (TransitionTypes type)
  {
    return m_type != type;
  }

  bool operator == (Transition const& rhs)
  {
    // Don't compare next pointer
    return m_type == rhs.m_type &&
           m_out  == rhs.m_out  &&           
           m_min  == rhs.m_min  &&
           m_max  == rhs.m_max  ;
  }

  bool operator != (Transition const& rhs)
  {
    return ! (*this == rhs);
  }

  String ToString() const;

  TransitionTypes m_type;
  State       m_out;
  char        m_min;
  char        m_max;
  Transition* m_next;
};

typedef std::vector<Transition> TransitionVec;

//
// Wrapper around TransitionVec for memory cleanup
//
class Transitions : public std::vector<Transition*>
{
public:

  ~Transitions()
  {
    for(size_t i = 0; i < size(); ++i)
    {
      for(Transition* t = at(i); t;)
      {
        Transition* p = t->m_next;
        delete t;
        t = p;
      }
    }
  }

};

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

};

//////////////////////////////////////////////////////////////////////////
//
// Regular expression compiler
//

class RegexCompiler
{
public:

  //
  // Compile a regular expression from stream
  //
  static RegexData* Compile(LexStream& stream);

  //
  // Compile a regular expression from string
  //
  static RegexData* Compile(String const& string);

  //
  // Construction
  //
  RegexCompiler();

  //
  // Create a new state
  //
  State AddState();

  //
  // Add transition
  //
  void AddTransition(State in, State out, TransitionTypes type = ttEmpty, char min = 0, char max = 0, bool append = true);

  //
  // Add an alternation between lhs and rhs
  //
  void AddAlternation(Pair const& lhs, Pair const& rhs, Pair& result);

  //
  // Add a sequence between lhs and rhs
  //
  void AddSequence(Pair const& lhs, Pair const& rhs, Pair& result);

  //
  // Add a left anchor
  //
  void AddLeftAnchor(Pair& result);

  //
  // Add a right anchor
  //
  void AddRightAnchor(Pair& result);

  //
  // Add an any char
  //
  void AddAnyChar(Pair& result);

  //
  // Add a char
  //
  void AddChar(char ch, Pair& result);

  //
  // Add a char class
  //
  void AddCharClass(char ch, Pair& result);

  //
  // Add a range
  //
  void AddRange(char min, char max, Pair& result);

  //
  // Add a capturing subexpression
  //
  void AddCapture(bool start, Pair& result);

  //
  // Add a back reference
  //
  void AddBackref(char num, Pair& result);

  //
  // Quantify zero or one (?)
  //
  void ZeroOrOne(Pair const& expression, bool greedy, Pair& result);

  //
  // Quantify zero or more (*)
  //
  void ZeroOrMore(Pair const& expression, bool greedy, Pair& result);

  //
  // Quantify one or more (+)
  //
  void OneOrMore(Pair const& expression, bool greedy, Pair& result);

  //
  // Quantify an expression
  //
  void Quantify(Pair const& expression, Pair const& quantifier, bool greedy, Pair& result);

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
  // Optimize the transition table
  //
  void Optimize();

  //
  // Find all non-empty transitions
  //
  void FindTransitions(Transition* source, std::vector<Transition>& transitions);

  //
  // Pattern string
  //
  String m_pattern;

  // State sequence number
  size_t m_stateSeq;

  // Regular expression table
  Transitions m_table;
  TransitionVec m_vec;

};

#endif // CSCRIPT_REGEX_COMPILER_H

