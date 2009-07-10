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
  ttEmpty,
  ttNext,
  ttOffset,
  ttAnchorL,
  ttAnchorR,
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
  Transition(State out, TransitionTypes type, char min = 0, char max = 0) :
    m_type  (type), 
    m_out   (out),
    m_min   (min),
    m_max   (max)
  {
  }

  TransitionTypes m_type;
  State m_out;
  char  m_min;
  char  m_max;
};

// Transition types
typedef std::vector<Transition> TransitionVec;
typedef std::vector<size_t> SizeVec;
typedef std::vector<SizeVec> TransitionTable;

//////////////////////////////////////////////////////////////////////////
//
// Compiled regular expression
//

class RegexData : public GCSimpleObject
{
public:

  // Start and final state
  State m_start;
  State m_final;

  // Transitions
  TransitionVec m_transitions;
  TransitionTable m_table;

};

//////////////////////////////////////////////////////////////////////////
//
// Regular expression compiler
//

class RegexCompiler
{
public:

  //
  // Compile a regular expression
  //
  static RegexData* Compile(LexStream& stream);

  //
  // Construction
  //
  RegexCompiler();

  //
  // Create a new state
  //
  State AddState();

  //
  // Add an empty transition
  //
  void AddTransition(State in, State out, TransitionTypes type = ttEmpty, char min = 0, char max = 0);

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
  // Quantify zero or one (?)
  //
  void ZeroOrOne(Pair const& expression, Pair& result);

  //
  // Quantify zero or more (*)
  //
  void ZeroOrMore(Pair const& expression, Pair& result);

  //
  // Quantify one or more (+)
  //
  void OneOrMore(Pair const& expression, Pair& result);

  //
  // Quantify an expression
  //
  void Quantify(Pair const& expression, Pair const& quantifier, Pair& result);

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
  // Push a character to the parser
  //
  void PushChar(char ch);

  // State sequence number
  size_t m_stateSeq;

  // Current regex
  RegexData* m_rd;

};

#endif // CSCRIPT_REGEX_COMPILER_H


