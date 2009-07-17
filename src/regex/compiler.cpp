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
#include <iostream>
#include <algorithm>

//////////////////////////////////////////////////////////////////////////

String
Transition::ToString() const
{
  std::ostringstream r;

  switch(m_type)
  {
  case ttNone:     r << "";         break;
  case ttEmpty:    r << "Empty";    break;
  case ttFinal:    r << "Final";    break;
  case ttOffset:   r << "Offset";   break;
  case ttAnchorL:  r << "AnchorL";  break;
  case ttAnchorR:  r << "AnchorR";  break;
  case ttCaptureL: r << "CaptureL"; break;
  case ttCaptureR: r << "CaptureR"; break;
  case ttBackref:  r << "Backref " << (char)(m_min + '0'); break;
  case ttAny:      r << "Any";      break;
  case ttChar:     r << "Char '"   << m_min << "'"; break;
  case ttRange:    r << "Range '"  << m_min << "', '" << m_max << "'"; break;
  case ttNRange:   r << "NRange '" << m_min << "', '" << m_max << "'"; break;
  case ccAlnum:    r << "Alnum";    break;
  case ccAlpha:    r << "Alpha";    break;
  case ccBlank:    r << "Blank";    break;
  case ccCntrl:    r << "Cntrl";    break;
  case ccDigit:    r << "Digit";    break;
  case ccGraph:    r << "Graph";    break;
  case ccLower:    r << "Lower";    break;
  case ccPrint:    r << "Print";    break;
  case ccPunct:    r << "Punct";    break;
  case ccSpace:    r << "Space";    break;
  case ccUpper:    r << "Upper";    break;
  case ccXdigit:   r << "XDigit";   break;
  }

  switch(m_type)
  {
  case ttNone:
  case ttFinal:
    break;
  default:
    r << " -> " << m_out;
    break;
  }

  return r.str();
}

//////////////////////////////////////////////////////////////////////////

RegexCompiler::RegexCompiler()
{
  // Create initial state
  AddState();
}

inline State 
RegexCompiler::AddState()
{
  // Reserve space in chunks
  if(m_table.size() % 10 == 0)
  {
    m_table.reserve(m_table.size() + 10);
  }

  // Add a slot
  m_table.resize(m_table.size() + 1);

  // Done
  return m_table.size() - 1;
}

inline void 
RegexCompiler::AddTransition(State in, State out, TransitionTypes type, char min, char max, bool append)
{
  m_table[in].insert(append ? m_table[in].end() : m_table[in].begin(), Transition(out, type, min, max));
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

void 
RegexCompiler::AddBackref(char num, Pair& r)
{
  r.m_min = AddState();
  r.m_max = AddState();
  AddTransition(r.m_min, r.m_max, ttBackref, num);
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
  // Add final transition
  AddTransition(r.m_max, 0, ttFinal);

  // Add empty transition to initial state
  AddTransition(0, r.m_min, ttEmpty);

  // Add moving start point if there's no anchor
  TransitionVec transitions;
  FindTransitions(m_table[0], transitions);
  if(transitions.size() > 1 || transitions[0] != ttAnchorL)
  {
    AddTransition(0, 0, ttOffset);
  }

  // Rebuild the table
  Optimize();
}

//////////////////////////////////////////////////////////////////////////

inline void 
RegexCompiler::FindTransitions(TransitionList const& in, TransitionVec& out)
{
  for(TransitionList::const_iterator it = in.begin(); it != in.end(); ++it)
  {
    if(it->m_type == ttEmpty)
    {
      FindTransitions(m_table[it->m_out], out);
    }
    else if(std::find(out.begin(), out.end(), *it) == out.end())
    {      
      out.push_back(*it);
    }
  }
}

void 
RegexCompiler::Optimize()
{
  std::vector<size_t> offsets;
  TransitionVec table;

  // Make sure all old offsets fit
  offsets.resize(m_table.size() + 1, -1);

  // Enumerate states in old table
  for(size_t in = 0; in < m_table.size(); ++in)
  {
    std::vector<Transition> temptable;

    // Find all reachable non-empty transitions
    FindTransitions(m_table[in], temptable);
    temptable.push_back(Transition(0, ttNone));

    // Find first duplicate set
    size_t dup = -1;
    for(size_t i = 0; dup == -1 && offsets[i] != -1; ++i)
    {
      for(size_t j = 0; ; ++j)
      {
        if(table[j + offsets[i]] != temptable[j])
        {
          break;
        }
        if(table[j + offsets[i]] == ttNone)
        {
          dup = offsets[i];
          break;
        }
      }
    }

    // Found offset, state is duplicate
    if(dup != -1)
    {
      offsets[in] = dup;
      continue;
    }
    
    // Store offset of first state in table
    offsets[in] = table.size();

    // Copy states into array
    table.insert(table.end(), temptable.begin(), temptable.end());
  }

  // Update transitions
  for(size_t i = 0; i < table.size(); ++i)
  {
    if(table[i].m_type != ttNone)
    {
      table[i].m_out = offsets[table[i].m_out];
    }
  }

  // Reachable states
  std::vector<int> reachable;
  reachable.push_back(0);

  // Create new table and swap with old
  TransitionVec oldtable;
  oldtable.swap(table);

  // Clear and size offsets table
  offsets.clear();
  offsets.resize(oldtable.size(), -1);

  // Copy reachable states from old to new
  while(reachable.size())
  {
    // Fetch offset to copy from
    size_t i = reachable.back();
    reachable.pop_back();
    
    // Store new offset
    if(offsets[i] == -1)
    {
      offsets[i] = table.size();
    }
    else
    {
      // Already done
      continue;
    }

    // Copy transitions for state
    while(true)
    {
      // Add transition to new table
      table.push_back(oldtable[i]);

      // End of state reached
      if(oldtable[i] == ttNone)
      {
        break;
      }

      // Mark out state reachable
      reachable.push_back(oldtable[i++].m_out);
    }
  }

  // Update transitions
  for(size_t i = 0; i < table.size(); ++i)
  {
    if(table[i].m_type != ttNone)
    {
      table[i].m_out = offsets[table[i].m_out];
    }
  }

  // Store the new table
  m_vec.swap(table);
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
    instance.m_pattern += c;
    
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
      instance.m_pattern += c;
      
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

      case '1': case '2': case '3': 
      case '4': case '5': case '6': 
      case '7': case '8': 
      case '9': 
        type = RE_BACKREF;
        c = c - '0';
        break;
      
      default: 
        type = RE_CHAR; 
        break;
      }
      break;
    }

    // Push to parser
    parse(type, c);
  }

  // Flush parser
  parse();

  // Build return value
  RegexData* rd = new RegexData;
  rd->m_pattern = instance.m_pattern;
  rd->m_table.swap(instance.m_vec);

  // Return regex struct
  return rd;
}

void
RegexCompiler::OnSyntaxError(char ch)
{
  throw std::runtime_error("Invalid regular expression");
}
