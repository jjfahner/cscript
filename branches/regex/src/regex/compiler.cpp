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
#include "regex/compiler.h"
#include "lexstream.h"
#include "lemon.h"

#include <iostream>
#include <list>

//////////////////////////////////////////////////////////////////////////

RegexCompiler::RegexCompiler() :
m_stateSeq(0),
m_rd      (0)
{
  m_rd = new RegexData;
}

inline State 
RegexCompiler::AddState()
{
  // Fetch next state
  State state = ++m_stateSeq;

  // Resize table
  size_t size = m_rd->m_table.size();
  if(size <= state)
  {
    m_rd->m_table.resize(size + 10);
  }

  // Done
  return state;
}

inline void 
RegexCompiler::AddTransition(State in, State out, TransitionTypes type, char min, char max, bool append)
{
  // Create transition
  Transition* t = new Transition(out, type, min, max);

  // Take pointer to first entry
  Transition** p = &m_rd->m_table[in];

  // Handle append/prepend
  if(append)
  {
    // Find last entry for append
    for(; append && *p; p = &((*p)->m_next));
  }
  else
  {
    // Append first transition
    t->m_next = *p;
  }

  // Store transition
  *p = t;
}

//////////////////////////////////////////////////////////////////////////

inline void
RegexCompiler::AddAlternation(Pair const& lhs, Pair const& rhs, Pair& r)
{
  r.m_min = AddState();
  r.m_max = AddState();
  AddTransition(r.m_min, lhs.m_min);
  AddTransition(r.m_min, rhs.m_min);
  AddTransition(lhs.m_max, r.m_max);
  AddTransition(rhs.m_max, r.m_max);
}

inline void
RegexCompiler::AddSequence(Pair const& lhs, Pair const& rhs, Pair& r)
{
  r.m_min = lhs.m_min;
  r.m_max = rhs.m_max;
  AddTransition(lhs.m_max, rhs.m_min);
}

inline void
RegexCompiler::AddLeftAnchor(Pair& r)
{
  r.m_min = AddState();
  r.m_max = AddState();
  AddTransition(r.m_min, r.m_max, ttAnchorL);
}

inline void
RegexCompiler::AddRightAnchor(Pair& r)
{
  r.m_min = AddState();
  r.m_max = AddState();
  AddTransition(r.m_min, r.m_max, ttAnchorR);
}

inline void
RegexCompiler::AddAnyChar(Pair& r)
{
  r.m_min = AddState();
  r.m_max = AddState();
  AddTransition(r.m_min, r.m_max, ttAny);
}

inline void
RegexCompiler::AddChar(char ch, Pair& r)
{
  r.m_min = AddState();
  r.m_max = AddState();
  AddTransition(r.m_min, r.m_max, ttChar, ch);
}

inline void 
RegexCompiler::AddCharClass(char ch, Pair& r)
{
  r.m_min = AddState();
  r.m_max = AddState();
  AddTransition(r.m_min, r.m_max, (TransitionTypes)ch);
}

inline void 
RegexCompiler::AddRange(char min, char max, Pair& r)
{
  r.m_min = AddState();
  r.m_max = AddState();
  AddTransition(r.m_min, r.m_max, ttRange, 
                min <= max ? min : max, 
                min <= max ? max : min);
}

//////////////////////////////////////////////////////////////////////////

inline void 
RegexCompiler::ZeroOrOne(Pair const& e, bool greedy, Pair& r)
{
  r.m_min = e.m_min;
  r.m_max = e.m_max;
  AddTransition(e.m_min, e.m_max, ttEmpty, 0, 0, greedy);
}

inline void 
RegexCompiler::ZeroOrMore(Pair const& e, bool greedy, Pair& r)
{
  r.m_min = e.m_min;
  r.m_max = AddState();
  AddTransition(e.m_min, r.m_max, ttEmpty, 0, 0, greedy);
  AddTransition(e.m_max, greedy ? e.m_min : r.m_max);
  AddTransition(e.m_max, greedy ? r.m_max : e.m_min);
}

inline void 
RegexCompiler::OneOrMore(Pair const& e, bool greedy, Pair& r)
{
  r.m_min = e.m_min;
  r.m_max = AddState();
  AddTransition(e.m_max, greedy ? e.m_min : r.m_max);
  AddTransition(e.m_max, greedy ? r.m_max : e.m_min);
}

inline void
RegexCompiler::Quantify(Pair const& e, Pair const& q, bool greedy, Pair& r)
{
  throw std::runtime_error("Free quantifiers not implemented");
}

inline void
RegexCompiler::Finalize(Pair const& r)
{
  // Set start and final state
  m_rd->m_start = 0;
  m_rd->m_final = r.m_max;

  // Add moving start point
  AddTransition(m_rd->m_start, r.m_min, ttEmpty);
  AddTransition(m_rd->m_start, m_rd->m_start, ttOffset);
}

//////////////////////////////////////////////////////////////////////////

#include "parser.gen.c"
#include "parser.gen.h"

void ReParseTraceDummy(FILE*, char*) {}

typedef LemonParser<
  RegexCompiler, 
  char, 
  ReParseAlloc, 
  ReParseFree, 
  #ifdef _DEBUG
  ReParseTrace, 
  #else
  ReParseTraceDummy,
  #endif
  ReParse> ReParserImpl;

/*static*/ RegexData*
RegexCompiler::Compile(LexStream& stream)
{
  // Create compiler instance
  RegexCompiler instance;

  // Create parser instance
  ReParserImpl parse(&instance, "Regex: ");

  // Push characters
  for(;;)
  {
    // Read next character
    char c = *stream.m_cursor++;
    if(c == 0 || c == '/')
    {
      break;
    }
    
    // Select token type
    int type = RE_CHAR;
    switch(c)
    {
    case '(': type = RE_LPAREN; break;
    case ')': type = RE_RPAREN; break;
    case '[': type = RE_LBRACKET; break;
    case ']': type = RE_RBRACKET; break;
    case '{': type = RE_LBRACE; break;
    case '}': type = RE_RBRACE; break;
    case '.': type = RE_ANY; break;
    case '*': type = RE_STAR; break;
    case '?': type = RE_QUESTION; break;
    case '+': type = RE_PLUS; break;
    case '^': type = RE_ANCHOR_L; break;
    case '$': type = RE_ANCHOR_R; break;
    case '|': type = RE_ALTERNATE; break;
    case ',': type = RE_COMMA; break;
    case '-': type = RE_DASH; break;
    case '0': type = RE_INT; break;
    case '1': type = RE_INT; break;
    case '2': type = RE_INT; break;
    case '3': type = RE_INT; break;
    case '4': type = RE_INT; break;
    case '5': type = RE_INT; break;
    case '6': type = RE_INT; break;
    case '7': type = RE_INT; break;
    case '8': type = RE_INT; break;
    case '9': type = RE_INT; break;
    case '\\':
      c = *stream.m_cursor++;
      type = RE_CLASS;
      switch(c)
      {
      case 'a': c = ccAlnum;  break;
      case 'c': c = ccAlpha;  break;
      case 'b': c = ccBlank;  break;
      case 'n': c = ccCntrl;  break;
      case 'd': c = ccDigit;  break;
      case 'g': c = ccGraph;  break;
      case 'l': c = ccLower;  break;
      case 'p': c = ccPrint;  break;
      case 't': c = ccPunct;  break;
      case 's': c = ccSpace;  break;
      case 'u': c = ccUpper;  break;
      case 'x': c = ccXdigit; break;
      default: type = RE_CHAR; break;
      }
      break;
    }

    // Push to parser
    parse(type, c);
  }

  // Flush parser
  parse();

  // Return regex struct
  return instance.m_rd;
}

void
RegexCompiler::OnSyntaxError(char ch)
{
  throw std::runtime_error("Invalid regular expression");
}
