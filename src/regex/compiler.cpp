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

#include <sstream>
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

void 
RegexCompiler::AddCapture(bool start, Pair& r)
{
  r.m_min = AddState();
  r.m_max = AddState();
  AddTransition(r.m_min, r.m_max, start ? ttCaptureL : ttCaptureR);
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

/*static*/ RegexData* 
RegexCompiler::Compile(String const& string)
{
  // Construct string stream
  std::istringstream istream(string + "//");

  // Construct lex stream
  LexStream stream(istream);

  // Compile from stream
  return Compile(stream);
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
    // Make sure the buffer's ok
    if(stream.m_cursor == stream.m_bufend && stream.FillBuffer(2) < 2)
    {
      throw std::runtime_error("Unexpected end of file in regular expression");
    }

    // Read next character
    char c = *stream.m_cursor++;
    if(c == 0 || c == '/')
    {
      break;
    }

    // Append to pattern
    instance.m_rd->m_pattern += c;
    
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
    case ':': type = RE_COLON; break;

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

    case 'a': type = RE_LC_A; break;
    case 'b': type = RE_LC_B; break;
    case 'c': type = RE_LC_C; break;
    case 'd': type = RE_LC_D; break;
    case 'e': type = RE_LC_E; break;
    case 'f': type = RE_LC_F; break;
    case 'g': type = RE_LC_G; break;
    case 'h': type = RE_LC_H; break;
    case 'i': type = RE_LC_I; break;
    case 'j': type = RE_LC_J; break;
    case 'k': type = RE_LC_K; break;
    case 'l': type = RE_LC_L; break;
    case 'm': type = RE_LC_M; break;
    case 'n': type = RE_LC_N; break;
    case 'o': type = RE_LC_O; break;
    case 'p': type = RE_LC_P; break;
    case 'q': type = RE_LC_Q; break;
    case 'r': type = RE_LC_R; break;
    case 's': type = RE_LC_S; break;
    case 't': type = RE_LC_T; break;
    case 'u': type = RE_LC_U; break;
    case 'v': type = RE_LC_V; break;
    case 'w': type = RE_LC_W; break;
    case 'x': type = RE_LC_X; break;
    case 'y': type = RE_LC_Y; break;
    case 'z': type = RE_LC_Z; break;

    case 'A': type = RE_UC_A; break;
    case 'B': type = RE_UC_B; break;
    case 'C': type = RE_UC_C; break;
    case 'D': type = RE_UC_D; break;
    case 'E': type = RE_UC_E; break;
    case 'F': type = RE_UC_F; break;
    case 'G': type = RE_UC_G; break;
    case 'H': type = RE_UC_H; break;
    case 'I': type = RE_UC_I; break;
    case 'J': type = RE_UC_J; break;
    case 'K': type = RE_UC_K; break;
    case 'L': type = RE_UC_L; break;
    case 'M': type = RE_UC_M; break;
    case 'N': type = RE_UC_N; break;
    case 'O': type = RE_UC_O; break;
    case 'P': type = RE_UC_P; break;
    case 'Q': type = RE_UC_Q; break;
    case 'R': type = RE_UC_R; break;
    case 'S': type = RE_UC_S; break;
    case 'T': type = RE_UC_T; break;
    case 'U': type = RE_UC_U; break;
    case 'V': type = RE_UC_V; break;
    case 'W': type = RE_UC_W; break;
    case 'X': type = RE_UC_X; break;
    case 'Y': type = RE_UC_Y; break;
    case 'Z': type = RE_UC_Z; break;

    case '\\':
      c = *stream.m_cursor++;
      instance.m_rd->m_pattern += c;
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
