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
#include <iostream>

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

StringCRef 
Regex::Pattern()
{
  if(m_rd == 0)
  {
    throw CatchableException("Invalid Regex object");
  }
  return m_rd->m_pattern;
}

void 
Regex::Compile(StringCRef pattern)
{
  m_rd = RegexCompiler::Compile(pattern);
}

ObjectPtr
Regex::Match(StringCRef text, int64 offset)
{
  if(m_rd == 0)
  {
    throw CatchableException("Invalid Regex object");
  }
  return MatchImpl(text, offset, true).m_result;
}

bool 
Regex::IsMatch(StringCRef text, int64 offset)
{
  if(m_rd == 0)
  {
    throw CatchableException("Invalid Regex object");
  }
  return MatchImpl(text, offset, false).m_success;
}

String
Regex::TableToString()
{
  std::ostringstream r;

  std::cout << "State 0\n";
  for(size_t i = 0; i < m_rd->m_table.size() - 1; ++i)
  {
    if(m_rd->m_table[i] == ttNone)
    {
      r << "State " << i + 1 << "\n";
    }
    else
    {
      r << "  " << m_rd->m_table[i].ToString() << "\n"; 
    }
  }

  return r.str();
}

//////////////////////////////////////////////////////////////////////////

struct Capture
{
  Capture(Capture* link, char const* ptr)
  {
    m_link = link;
    m_ptr  = ptr;
    m_end  = ptr;
  }

  Capture* Copy()
  {
    Capture* p = new Capture(*this);
    if(m_link)
    {
      p->m_link = m_link->Copy();
    }
    return p;
  }

  void Insert(List* list)
  {
    list->Append(String(m_ptr, m_end));
    if(m_link)
    {
      m_link->Insert(list);
    }
  }

  void Delete()
  {
    if(m_link)
    {
      m_link->Delete();
    }
    delete this;
  }

  Capture*    m_link;
  char const* m_ptr;
  char const* m_end;
};

//////////////////////////////////////////////////////////////////////////

struct ReFrame
{
  ReFrame(State state, 
          size_t trans, 
          char const* start, 
          char const* cur,
          ReFrame* prev = 0)
  {
    m_state    = state;
    m_trans    = trans;
    m_start    = start;
    m_cur      = cur;
    m_prev     = prev;
    m_captures = 0;
    m_capstack = 0;
  }

  ~ReFrame()
  {
    if(m_captures) m_captures->Delete();
    if(m_capstack) m_capstack->Delete();
  }

  void Delete()
  {
    if(m_prev)
    {
      m_prev->Delete();
    }
    delete this;
  }

  State       m_state;
  size_t      m_trans;
  char const* m_start;
  char const* m_cur;
  ReFrame*    m_prev;
  Capture*    m_captures;
  Capture*    m_capstack;
};

//////////////////////////////////////////////////////////////////////////

class ReStack
{
public:

  ReStack() : m_last (0), m_size (0), m_max (0)
  {
  }

  ~ReStack()
  {
    if(m_last) m_last->Delete();
  }

  size_t Size()
  {
    return m_size;
  }

  size_t Max()
  {
    return m_max;
  }

  ReFrame* Push(State state, size_t trans, 
           char const* start, char const* cur)
  {
    m_max = ++m_size > m_max ? m_size : m_max;    
    return m_last = new ReFrame(state, 
           trans, start, cur, m_last);
  }

  ReFrame* Pop()
  {
    --m_size;
    ReFrame* pbt = m_last;
    m_last = pbt ? pbt->m_prev : 0;
    return pbt;
  }

private:

  ReFrame* m_last;
  size_t m_size;
  size_t m_max;

};

//////////////////////////////////////////////////////////////////////////

#ifndef __GNUC__
inline bool isblank(int ch)
{
  return ch == ' ' || ch == '\t';
}
#endif

//////////////////////////////////////////////////////////////////////////

Regex::ImplResult 
Regex::MatchImpl(StringCRef input, int64 offset, bool createMatchResult)
{
  typedef unsigned char uchar;

  Capture* br;
  Capture** pbr;

  // Alias for table
  TransitionVec& table = m_rd->m_table;
  
  // Check offset
  offset = offset < input.length() ? offset : input.length();

  // Setup pointer to string
  char const* text = input.c_str();

  // Backtracking stack
  ReStack stack;

  // Create initial stack frame
  ReFrame* pbt = new ReFrame(0, 0, text + offset, text + offset);

  // Main match loop
  while(pbt)
  {
    // Create some aliases
    Transition& tr = table[pbt->m_trans];
    char const*& p = pbt->m_cur;
    char const*  n = p + 1;

    // Record backtrack for next transition
    if(table[pbt->m_trans + 1] != ttNone)
    {
      ReFrame* f = stack.Push(
        pbt->m_state, 
        pbt->m_trans + 1, 
        pbt->m_start, 
        pbt->m_cur);
      f->m_captures = pbt->m_captures ? pbt->m_captures->Copy() : 0;
      f->m_capstack = pbt->m_capstack ? pbt->m_capstack->Copy() : 0;
    }

    // Match transition
    switch(tr.m_type)
    {
    case ttFinal:
      break;

    case ttEmpty:
      break;

    case ttOffset:   
      p = ++pbt->m_start; 
      p = *p ? p : 0; 
      break;
    
    case ttCaptureL: 
      pbt->m_capstack = new Capture(pbt->m_capstack, pbt->m_cur); 
      break;

    case ttCaptureR:
      br = pbt->m_capstack;
      pbt->m_capstack = br->m_link;
      pbr = &pbt->m_captures;
      while(*pbr) pbr = &(*pbr)->m_link;
      *pbr = br;
      br->m_link = 0;
      break;

    case ttBackref:
      pbt->m_cur = MatchBackref(pbt);
      break;

    case ttAnchorL:  p = p == text ? p : 0; break;
    case ttAnchorR:  p = *p ? 0 : p; break;
    case ttAny:      p = *p ? n : 0; break;
    case ttChar:     p = *p == tr.m_min ? n : 0; break;
    case ttRange:    p = *p >= tr.m_min && *p <= tr.m_max ? n : 0; break;
    case ttNRange:   p = *p >= tr.m_min && *p <= tr.m_max ? 0 : p; break;
    case ccAlnum:    p = isalnum((uchar)*p)  ? n : 0; break;
    case ccAlpha:    p = isalpha((uchar)*p)  ? n : 0; break;
    case ccBlank:    p = isblank((uchar)*p)  ? n : 0; break;
    case ccCntrl:    p = iscntrl((uchar)*p)  ? n : 0; break;
    case ccDigit:    p = isdigit((uchar)*p)  ? n : 0; break;
    case ccGraph:    p = isgraph((uchar)*p)  ? n : 0; break;
    case ccLower:    p = islower((uchar)*p)  ? n : 0; break;
    case ccPrint:    p = isprint((uchar)*p)  ? n : 0; break;
    case ccPunct:    p = ispunct((uchar)*p)  ? n : 0; break;
    case ccSpace:    p = isspace((uchar)*p)  ? n : 0; break;
    case ccUpper:    p = isupper((uchar)*p)  ? n : 0; break;
    case ccXdigit:   p = isxdigit((uchar)*p) ? n : 0; break;
    default:         throw std::runtime_error("Invalid transition type");
    }

    // Final state reached
    if(pbt->m_cur && tr.m_type == ttFinal)
    {
      if(pbt->m_cur > pbt->m_start)
      {
        break;
      }
      pbt->m_cur = 0;
    }

    // Test match
    if(pbt->m_cur == 0)
    {
      // Failed; delete this frame
      delete pbt;

      // Backtrack to previous frame
      pbt = stack.Pop();
    }
    else
    {
      // Advance current backrefs
      for(Capture* pbr = pbt->m_capstack; pbr; pbr = pbr->m_link)
      {
        pbr->m_end = pbt->m_cur;
      }

      // Advance to next state
      pbt->m_state = tr.m_out;
      pbt->m_trans = pbt->m_state;
    }
  }

  // Create result
  ImplResult result;
  result.m_success = pbt != 0;
  result.m_result  = 0;

  // Create match result
  if(createMatchResult)
  {
    MatchResult* mr = new MatchResult;
    result.m_result = mr;
    if(pbt)
    {
      mr->m_success = true;
      mr->m_text = String(pbt->m_start, pbt->m_cur);
      mr->m_offset = pbt->m_start - text;
      mr->m_captures = new List;
      if(pbt->m_captures)
      {
        pbt->m_captures->Insert(mr->m_captures);
      }
    }
  }

  // Delete current frame
  if(pbt)
  {
    delete pbt;
  }

  // Done
  return result;
}

char const* 
Regex::MatchBackref(struct ReFrame* frame)
{
  // Retrieve index
  int i = m_rd->m_table[frame->m_state].m_min - 1;

  // Find capture
  Capture* c = frame->m_captures;
  while(c && i)
  {
    c = c->m_link;
  }

  // Match the strings
  char const* p1 = frame->m_cur;
  char const* p2 = c->m_ptr;
  char const* p3 = c->m_end;
  for(; *p1 && p2 != p3; ++p1, ++p2)
  {
    if(*p1 != *p2)
    {
      return 0;
    }
  }
  
  // End of backref
  return p2 == p3 ? p1 : 0;
}
