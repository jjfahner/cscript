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
  Capture(char const* ptr)
  {
    m_ptr  = ptr;
    m_end  = ptr;
  }

  char const* m_ptr;
  char const* m_end;
};

//////////////////////////////////////////////////////////////////////////

struct ReFrame
{
  ReFrame(size_t trans, 
          char const* start, 
          char const* cur,
          ReFrame* prev = 0)
  {
    m_trans    = trans;
    m_ptr    = start;
    m_end      = cur;
    m_prev     = prev;
  }

  void Delete()
  {
    if(m_prev)
    {
      m_prev->Delete();
    }
    delete this;
  }

  size_t      m_trans;
  char const* m_ptr;
  char const* m_end;
  ReFrame*    m_prev;
  std::vector<Capture> m_captures;
  std::vector<Capture> m_capstack;
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

  ReFrame* Push(size_t trans, 
           char const* start, 
           char const* cur)
  {
    m_max = ++m_size > m_max ? m_size : m_max;    
    return m_last = new ReFrame(trans, start, cur, m_last);
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

  // Alias for table
  TransitionVec& table = m_rd->m_table;
  
  // Check offset
  offset = offset < input.length() ? offset : input.length();

  // Setup pointer to string
  char const* text = input.c_str();

  // Backtracking stack
  ReStack stack;

  // Create initial stack frame
  ReFrame* pbt = new ReFrame(0, text + offset, text + offset);

  // Main match loop
  while(pbt)
  {
    // Create some aliases
    Transition& tr = table[pbt->m_trans];
    char const*& p = pbt->m_end;
    char const*  n = p + 1;

    // Record backtrack for next transition
    if(table[pbt->m_trans + 1] != ttNone)
    {
      ReFrame* f = stack.Push(
        pbt->m_trans + 1, 
        pbt->m_ptr, 
        pbt->m_end);
      f->m_captures = pbt->m_captures;
      f->m_capstack = pbt->m_capstack;
    }

    // Match transition
    switch(tr.m_type)
    {
    case ttFinal:
      break;

    case ttEmpty:
      break;

    case ttOffset:   
      p = ++pbt->m_ptr; 
      p = *p ? p : 0; 
      break;
    
    case ttCaptureL: 
      pbt->m_capstack.push_back(pbt->m_end); 
      break;

    case ttCaptureR:
      pbt->m_captures.push_back(pbt->m_capstack.back());
      pbt->m_capstack.pop_back();
      break;

    case ttBackref:
      pbt->m_end = MatchBackref(pbt);
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
    if(pbt->m_end && tr.m_type == ttFinal)
    {
      if(pbt->m_end > pbt->m_ptr)
      {
        break;
      }
      pbt->m_end = 0;
    }

    // Test match
    if(pbt->m_end == 0)
    {
      // Failed; delete this frame
      delete pbt;

      // Backtrack to previous frame
      pbt = stack.Pop();
    }
    else
    {
      // Advance current backrefs
      for(size_t i = 0; i < pbt->m_capstack.size(); ++i)
      {
        pbt->m_capstack[i].m_end = pbt->m_end;
      }

      // Advance to next state
      pbt->m_trans = tr.m_out;
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
      mr->m_text = String(pbt->m_ptr, pbt->m_end);
      mr->m_offset = pbt->m_ptr - text;
      mr->m_captures = new List;
      for(size_t i = 0; i < pbt->m_captures.size(); ++i)
      {
        mr->m_captures->Append(String(
          pbt->m_captures[i].m_ptr, 
          pbt->m_captures[i].m_end));
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
  int i = m_rd->m_table[frame->m_trans].m_min - 1;

  // Check capture index
  if((size_t)i >= frame->m_captures.size())
  {
    // This is really an error in the expression
    return 0;
  }

  // Match the strings
  char const* p1 = frame->m_end;
  char const* p2 = frame->m_captures[i].m_ptr;
  char const* p3 = frame->m_captures[i].m_end;
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
