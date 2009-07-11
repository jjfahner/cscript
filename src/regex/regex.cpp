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
#include "regex/regex.h"
#include "regex/compiler.h"
#include "gc.h"
#include "exceptions.h"

#include <sstream>

//////////////////////////////////////////////////////////////////////////

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

//////////////////////////////////////////////////////////////////////////

Regex::Regex(RegexData* rd) :
m_rd (rd)
{
}

void 
Regex::MarkObjects(GCObjectVec& grey)
{
  Object::MarkObjects(grey);
  GC::Mark(grey, m_rd);
}

void 
Regex::Compile(StringCRef pattern)
{
  // TODO
  //m_rd = RegexCompiler::Compile(pattern);
}

ObjectPtr
Regex::Match(StringCRef text)
{
  if(m_rd == 0)
  {
    throw CatchableException("Invalid Regex object");
  }
  return MatchImpl(text, true).m_result;
}

bool 
Regex::IsMatch(StringCRef text)
{
  if(m_rd == 0)
  {
    throw CatchableException("Invalid Regex object");
  }
  return MatchImpl(text, false).m_success;
}

String
Regex::TableToString()
{
  std::ostringstream r;

  r << "Start: " << m_rd->m_start << "\nFinal: " << m_rd->m_final << "\n\n";
  for(size_t state = 0; state <= m_rd->m_final; ++state)
  {
    r << "State " << state << ":\n";
    for(Transition* t = m_rd->m_table[state]; t; t = t->m_next)
    {
      r << "  ";
      switch(t->m_type)
      {
      case ttEmpty:   r << "Empty"; break;
      case ttNext:    r << "Next"; break;
      case ttOffset:  r << "Offset"; break;
      case ttAnchorL: r << "AnchorL"; break;
      case ttAnchorR: r << "AnchorR"; break;
      case ttAny:     r << "Any"; break;
      case ttChar:    r << "Char '" << t->m_min << "'"; break;
      case ttRange:   r << "Range '" << t->m_min << "', '" << t->m_max << "'"; break;
      case ttNRange:  r << "NRange '" << t->m_min << "', '" << t->m_max << "'"; break;
      case ccAlnum:   r << "Alnum"; break;
      case ccAlpha:   r << "Alpha"; break;
      case ccBlank:   r << "Blank"; break;
      case ccCntrl:   r << "Cntrl"; break;
      case ccDigit:   r << "Digit"; break;
      case ccGraph:   r << "Graph"; break;
      case ccLower:   r << "Lower"; break;
      case ccPrint:   r << "Print"; break;
      case ccPunct:   r << "Punct"; break;
      case ccSpace:   r << "Space"; break;
      case ccUpper:   r << "Upper"; break;
      case ccXdigit:  r << "XDigit"; break;
      }
      r << " -> " << t->m_out << "\n";
    }
  }

  return r.str();
}

//////////////////////////////////////////////////////////////////////////

inline bool isblank(int ch)
{
  return ch == ' ' || ch == '\t';
}

struct BTInfo
{
  BTInfo(State state, 
         Transition* trans, 
         char const* start, 
         char const* cur, 
         BTInfo* prev)
  {
    m_state = state;
    m_trans = trans;
    m_start = start;
    m_cur   = cur;
    m_prev  = prev;
  }

  State       m_state;
  Transition* m_trans;
  char const* m_start;
  char const* m_cur;
  BTInfo*     m_prev;
};

Regex::ImplResult 
Regex::MatchImpl(StringCRef input, bool createMatchResult)
{
  // Setup pointer to string
  char const* text = input.c_str();

  // Create inital stack frame
  BTInfo* pbt = new BTInfo(
    m_rd->m_start, 
    m_rd->m_table[m_rd->m_start], 
    input.c_str(), 
    input.c_str(), 
    0);

  // Size of backtracking stack
  size_t stacksize = 1;

  // Main match loop
  while(pbt)
  {
    // Setup some pointers
    char const* o = pbt->m_start;
    char const* s = pbt->m_cur;
    char const* p = 0;
    char const* n = s + 1;

    // Match transition
    Transition* tr = pbt->m_trans;
    switch(tr->m_type)
    {
    case ttEmpty:   p = s; break;
    case ttNext:    p = s + 1; break;
    case ttOffset:  ++o; if(*o) p = o; break;
    case ttAnchorL: p = s == text ? s : 0; break;
    case ttAnchorR: p = *s ? 0 : s; break;
    case ttAny:     p = *s ? n : 0; break;
    case ttChar:    p = *s == tr->m_min ? n : 0; break;
    case ttRange:   p = *s >= tr->m_min && *s <= tr->m_max ? n : 0; break;
    case ttNRange:  p = *s >= tr->m_min && *s <= tr->m_max ? 0 : s; break;
    case ccAlnum:   p = isalnum(*s) ? n : 0;  break;
    case ccAlpha:   p = isalpha(*s) ? n : 0;  break;
    case ccBlank:   p = isblank(*s) ? n : 0;  break;
    case ccCntrl:   p = iscntrl(*s) ? n : 0;  break;
    case ccDigit:   p = isdigit(*s) ? n : 0;  break;
    case ccGraph:   p = isgraph(*s) ? n : 0;  break;
    case ccLower:   p = islower(*s) ? n : 0;  break;
    case ccPrint:   p = isprint(*s) ? n : 0;  break;
    case ccPunct:   p = ispunct(*s) ? n : 0;  break;
    case ccSpace:   p = isspace(*s) ? n : 0;  break;
    case ccUpper:   p = isupper(*s) ? n : 0;  break;
    case ccXdigit:  p = isxdigit(*s) ? n : 0; break;
    default:        throw std::runtime_error("Invalid transition type");
    }

    // Determine next step
    if(p == 0)
    {
      // Delete frame and backtrack
      --stacksize;
      BTInfo* t = pbt;
      pbt = pbt->m_prev;
      delete t;

      // Next iteration
      continue;
    }

    // Update pointer
    pbt->m_cur = p;

    // On final state, stop
    if(tr->m_out == m_rd->m_final)
    {
      break;
    }

    // Record backtrack for next transition
    if(tr->m_next)
    {
      ++stacksize;
      BTInfo* nbt = new BTInfo(
        pbt->m_state,
        pbt->m_trans->m_next,
        pbt->m_start,
        pbt->m_cur,
        pbt->m_prev);
      pbt->m_prev = nbt;
    }

    // Advance to next state
    pbt->m_start = o;
    pbt->m_state = tr->m_out;
    pbt->m_trans = m_rd->m_table[pbt->m_state];
  }

  // Create result
  ImplResult result;
  result.m_success = pbt != 0;
  result.m_result  = 0;

  // Create match result
  if(createMatchResult && result.m_success)
  {
    result.m_result = new MatchResult;
    result.m_result->m_text = String(pbt->m_start, pbt->m_cur);
    result.m_result->m_offset = pbt->m_start - text;
  }
  
  // Delete stack frames
  while(pbt)
  {
    BTInfo* p = pbt->m_prev;
    delete pbt;
    pbt = p;
  }

  // Done
  return result;
}
