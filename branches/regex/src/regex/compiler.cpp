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
  State state = m_stateSeq++;

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
RegexCompiler::AddTransition(State in, State out, TransitionTypes type, char min, char max)
{
  m_rd->m_transitions.push_back(Transition(out, type, min, max));
  m_rd->m_table[in].push_back(m_rd->m_transitions.size() - 1);
}

inline void
RegexCompiler::AddAlternation(Pair const& lhs, Pair const& rhs, Pair& result)
{
  result.m_min = AddState();
  result.m_max = AddState();
  AddTransition(result.m_min, lhs.m_min);
  AddTransition(result.m_min, rhs.m_min);
  AddTransition(lhs.m_max, result.m_max);
  AddTransition(rhs.m_max, result.m_max);
}

inline void
RegexCompiler::AddSequence(Pair const& lhs, Pair const& rhs, Pair& result)
{
  result.m_min = lhs.m_min;
  result.m_max = rhs.m_max;
  AddTransition(lhs.m_max, rhs.m_min);
}

inline void
RegexCompiler::AddLeftAnchor(Pair& result)
{
  result.m_min = AddState();
  result.m_max = AddState();
  AddTransition(result.m_min, result.m_max, ttAnchorL);
}

inline void
RegexCompiler::AddRightAnchor(Pair& result)
{
  result.m_min = AddState();
  result.m_max = AddState();
  AddTransition(result.m_min, result.m_max, ttAnchorR);
}

inline void
RegexCompiler::AddAnyChar(Pair& result)
{
  result.m_min = AddState();
  result.m_max = AddState();
  AddTransition(result.m_min, result.m_max, ttAny);
}

inline void
RegexCompiler::AddChar(char ch, Pair& result)
{
  result.m_min = AddState();
  result.m_max = AddState();
  AddTransition(result.m_min, result.m_max, ttChar, ch);
}

inline void 
RegexCompiler::ZeroOrOne(Pair const& e, Pair& r)
{
  r.m_min = e.m_min;
  r.m_max = e.m_max;
  AddTransition(e.m_min, e.m_max);
}

inline void 
RegexCompiler::ZeroOrMore(Pair const& e, Pair& r)
{
  r.m_min = e.m_min;
  r.m_max = AddState();
  AddTransition(e.m_min, r.m_max);
  AddTransition(e.m_max, r.m_max);
  AddTransition(e.m_max, e.m_min);
}

inline void 
RegexCompiler::OneOrMore(Pair const& e, Pair& r)
{
  r.m_min = e.m_min;
  r.m_max = e.m_max;
  AddTransition(e.m_max, e.m_min);
}

inline void
RegexCompiler::Quantify(Pair const& e, Pair const& q, Pair& r)
{
  throw std::runtime_error("Free quantifiers not implemented");
}

inline void
RegexCompiler::Finalize(Pair const& result)
{
  m_rd->m_start = result.m_min;
  m_rd->m_final = result.m_max;
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
    case '*': type = RE_ZERO_OR_MORE; break;
    case '?': type = RE_ZERO_OR_ONE; break;
    case '+': type = RE_ONE_OR_MORE; break;
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
