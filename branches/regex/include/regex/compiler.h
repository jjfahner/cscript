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
#ifndef CSCRIPT_RE_H
#define CSCRIPT_RE_H

#include <cscript.h>
#include <object.h>
#include <gc.h>

#include <vector>

class LexStream;

enum SpecialChars
{
  StartAnchor = 1,
  EndAnchor = 2,
  AnyChar = 3
};

enum TransitionTypes
{
  ttEmpty,
  ttNext,
  ttStartPos,
  ttAnchorL,
  ttAnchorR,
  ttAny,
  ttChar,
  ttRange,
  ttNRange,
};

struct Pair
{
  size_t m_min;
  size_t m_max;
};

typedef size_t State;

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

//
// Compiler context
//
class RegexCompiler
{
public:

  //
  // Compile a regular expression
  //
  static void Compile(LexStream& stream);

  //
  // Construction
  //
  RegexCompiler();

  //
  // Create a new state
  //
  State AddState();

  //
  // Called when there is a syntax error
  //
  void OnSyntaxError(char ch);

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
  // Quantify zero or one (?)
  //
  void AddZeroOrOne(Pair const& expression, Pair& result);

  //
  // Quantify zero or more (*)
  //
  void AddZeroOrMore(Pair const& expression, Pair& result);

  //
  // Quantify one or more (+)
  //
  void AddOneOrMore(Pair const& expression, Pair& result);

  //
  // Quantify an expression
  //
  void Quantify(Pair const& expression, Pair const& quantifier, Pair& result);

  //
  // Finalize the expression
  //
  void Finalize(Pair const& result);

  //
  // Add an empty transition
  //
  void AddTransition(State in, State out, TransitionTypes type = ttEmpty, char min = 0, char max = 0)
  {
    m_transitions.push_back(Transition(out, type, min, max));
    m_table[in].push_back(m_transitions.size() - 1);
  }

  //private:

  // Transitions
  TransitionVec m_transitions;
  TransitionTable m_table;

  // State sequence number
  size_t m_stateSeq;

  // Start and final state
  State m_start;
  State m_final;

};

struct Frame
{
  void Set(State state = 0, char const* start = 0, char const* ptr = 0, Frame* next = 0)
  {
    m_state = state;
    m_start = start;
    m_ptr   = ptr;
    m_next  = next;
  }

  State       m_state;
  char const* m_start;
  char const* m_ptr;
  Frame*      m_next;

};

////////////////////////////////////////////////////////////////////////

// namespace Ast
// {
//   class Expression : public Object
//   {
//   public:
// 
//     Expression() :
//     m_min (1),
//     m_max (1)
//     {
//     }
// 
//     void SetQuantifier(Pair const& q)
//     {
//       m_min = q.m_min;
//       m_max = q.m_max;
//     }
// 
//     virtual Pair Compile(CompileContext&) = 0;
// 
//   protected:
// 
//     int m_min;
//     int m_max;
// 
//   };
// 
//   //////////////////////////////////////////////////////////////////////
// 
//   class Alternation : public Expression
//   {
//   public:
// 
//     Alternation(Expression* lhs, Expression* rhs) :
//     m_lhs (lhs),
//     m_rhs (rhs)
//     {
//     }
// 
//     virtual Pair Compile(CompileContext&);
// 
//   protected:
// 
//     virtual void MarkObjects(GCObjectVec& grey)
//     {
//       Expression::MarkObjects(grey);
//       GC::Mark(grey, m_lhs);
//       GC::Mark(grey, m_rhs);
//     }
// 
//     Expression* m_lhs;
//     Expression* m_rhs;
// 
//   };
// 
//   //////////////////////////////////////////////////////////////////////
// 
//   class Sequence : public Expression
//   {
//   public:
// 
//     void Append(Expression* expression)
//     {
//       m_expressions.push_back(expression);
//     }
// 
//     virtual Pair Compile(CompileContext&);
// 
//   protected:
// 
//     typedef std::vector<Expression*> Expressions;
// 
//     virtual void MarkObjects(GCObjectVec& grey)
//     {
//       Expression::MarkObjects(grey);
// 
//       Expressions::iterator it, ie;
//       it = m_expressions.begin();
//       ie = m_expressions.end();
// 
//       for(; it != ie; ++it)
//       {
//         GC::Mark(grey, *it);
//       }
//     }
// 
//     Expressions m_expressions;
// 
//   };
// 
//   //////////////////////////////////////////////////////////////////////
// 
//   class Range : public Expression
//   {
//   public:
// 
//     Range() : m_invert (false)
//     {
//     }
// 
//     void Append(Pair const& part)
//     {
//       m_parts.push_back(part);
//     }
// 
//     void Invert(bool invert)
//     {
//       m_invert = invert;
//     }
// 
//     virtual Pair Compile(CompileContext&);
// 
//   protected:
// 
//     typedef std::vector<Pair> Parts;
// 
//     Parts m_parts;
//     bool m_invert;
// 
//   };
// 
//   //////////////////////////////////////////////////////////////////////
// 
//   class Char : public Expression
//   {
//   public:
// 
//     Char(char value) : 
//     m_value (value)
//     {
//     }
// 
//     virtual Pair Compile(CompileContext&);
// 
//   protected:
// 
//     char m_value;
// 
//   };
// 
// 
// } // namespace Ast
// 
// //
// // Compile a regular expression
// //
// void Compile(Ast::Expression*);
// 
// //
// // Match a regular expression
// //
// void Match(CompileContext const& ctx, char const* text);


#endif // CSCRIPT_RE_H
