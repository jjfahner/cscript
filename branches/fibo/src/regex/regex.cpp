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
#include "dict.h"
#include "context.h"
#include "exceptions.h"
#include "eval.h"
#include "scope.h"

#include <sstream>
#include <iomanip>
#include <iostream>

//////////////////////////////////////////////////////////////////////////

DEF_EXCEPTION(RegexEmptyInstance, "Regex: instance contains no pattern");
DEF_EXCEPTION(RegexInvalidArgument, "Regex: invalid argument type");
DEF_EXCEPTION(RegexStackOverflow, "Regex: stack overflow");
DEF_EXCEPTION(RegexInvalidTransition, "Regex: invalid transition type");

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
    throw RegexEmptyInstance();
  }
  return m_rd->m_pattern;
}

void 
Regex::Compile(ValueCRef pattern)
{
  if(pattern.Type() == Value::tString)
  {
    RegexCompiler rc;
    m_rd = rc.Compile(pattern.GetString());
    return;
  }

  if(pattern.Type() == Value::tObject)
  {
    Dictionary* dict = dynamic_cast<Dictionary*>(pattern.GetObject());
    if(dict != 0)
    {
      RegexCompiler rc;
      m_rd = rc.Compile(dict);
      return;
    }
  }

  throw RegexInvalidArgument();
}

ObjectPtr
Regex::Match(StringCRef text, int64 offset)
{
  if(m_rd == 0)
  {
    throw RegexEmptyInstance();
  }
  return MatchImpl(text, offset, true).m_result;
}

bool 
Regex::IsMatch(StringCRef text, int64 offset)
{
  if(m_rd == 0)
  {
    throw RegexEmptyInstance();
  }
  return MatchImpl(text, offset, false).m_success;
}

String
Regex::Replace(StringCRef source, StringCRef replaceBy)
{
  // Copy string
  String result = source;
  while(true)
  {
    // Find next match
    MatchResult* res = MatchImpl(result, 0, true).m_result;
    if(!res->m_success)
    {
      break;
    }
    
    // Replace substring
    result = result.replace(
      (size_t)res->m_offset, 
      res->m_text.length(), 
      replaceBy);
  }

  // Done
  return result;
}

String
Regex::TableToString()
{
  std::ostringstream r;

  for(size_t i = 0; i < m_rd->m_table.size() - 1; ++i)
  {
    r << std::setw(4) << i << " " << m_rd->m_table[i].ToString() << "\n"; 
  }

  return r.str();
}

//////////////////////////////////////////////////////////////////////////

struct ReCapture
{
  ReCapture(char const* ptr) :
  m_ptr (ptr),
  m_end (ptr)
  {
  }

  char const* m_ptr;
  char const* m_end;
};

//////////////////////////////////////////////////////////////////////////

struct ReFrame
{
  typedef std::vector<size_t>    NumStack;
  typedef std::vector<ReCapture> CapStack;

  ReFrame(size_t trans, char const* ptr, char const* end) :
  m_trans     (trans),
  m_ptr       (ptr),
  m_end       (end),
  m_counters  (0),
  m_captures  (0),
  m_capstack  (0)
  {
  }

  ReFrame(ReFrame const& rhs)
  {
    *this = rhs;
  }

  ~ReFrame()
  {
    if(m_counters) delete m_counters;
    if(m_captures) delete m_captures;
    if(m_capstack) delete m_capstack;
  }

  ReFrame const& operator = (ReFrame const& rhs)
  {
    if(this != &rhs)
    {
      memcpy(this, &rhs, sizeof(ReFrame));
      m_counters = m_counters ? new NumStack(*m_counters) : 0;
      m_captures = m_captures ? new CapStack(*m_captures) : 0;
      m_capstack = m_capstack ? new CapStack(*m_capstack) : 0;
    }
    return *this;
  }

  void Clear()
  {
    if(m_counters) delete m_counters;
    if(m_captures) delete m_captures;
    if(m_capstack) delete m_capstack;
    memset(this, 0, sizeof(ReFrame));
  }

  void Swap(ReFrame& rhs)
  {
    char buf[sizeof(ReFrame)];
    memcpy(buf, &rhs, sizeof(ReFrame));
    memcpy(&rhs, this, sizeof(ReFrame));
    memcpy(this, buf, sizeof(ReFrame));
  }

  void PushCapStack()
  {
    if(m_capstack == 0) 
    {
      m_capstack = new ReFrame::CapStack;
    }
    m_capstack->push_back(m_end); 
  }

  void PopCapStack()
  {
    if(m_captures == 0) 
    {
      m_captures = new ReFrame::CapStack;
    }
    m_captures->push_back(m_capstack->back());
    m_capstack->pop_back();
  }

  void PushNum()
  {
    if(m_counters == 0)
    {
      m_counters = new NumStack;
    }
    m_counters->push_back(0);
  }

  bool TestNum(size_t num)
  {
    return m_counters->back() >= num;
  }

  void IncNum()
  {
    ++m_counters->back();
  }

  void PopNum()
  {
    m_counters->pop_back();
  }

  size_t      m_trans;
  char const* m_ptr;
  char const* m_end;

  NumStack*   m_counters;
  CapStack*   m_captures;
  CapStack*   m_capstack;
};

typedef std::vector<ReFrame> ReStack;

//////////////////////////////////////////////////////////////////////////

#ifndef __GNUC__
inline bool isblank(int ch)
{
  return ch == ' ' || ch == '\t';
}
#endif

inline bool isbegl(char const* p, char const* s)
{
  return p <= s || *(p-1) == '\n';
}

inline bool isendl(char const* p)
{
  return !*p || *p == '\n' || *p == '\r';
}

//////////////////////////////////////////////////////////////////////////

inline char const* 
Regex::MatchBackref(ReFrame& frame, size_t index)
{
  // Check capture index
  if(frame.m_captures == 0 || index >= frame.m_captures->size())
  {
    // This is really an error in the expression
    return 0;
  }

  // Setup pointers
  char const* p1 = frame.m_end;
  char const* p2 = (*frame.m_captures)[index].m_ptr;
  char const* p3 = (*frame.m_captures)[index].m_end;

  // Match strings
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
  char const* s = input.c_str() + offset;

  // Backtracking stack
  ReStack stack;
  stack.reserve(1000);

  // Create initial stack frame
  ReFrame frame(0, s, s);

  // Main match loop
  bool success;
  while(true)
  {
    // Create some aliases
    Transition& tr = table[frame.m_trans];
    char const*& p = frame.m_end;
    char const*  n = p + 1;

    // Record backtracking info for next transition
    if(table[frame.m_trans + 1].m_type != ttNone)
    {
      stack.push_back(frame);
      stack.back().m_trans++;
      if(stack.size() >= 10000)
      {
        throw RegexStackOverflow();
      }
    }

    // Match transition
    switch(tr.m_type)
    {
    case ttFinal:    break;
    case ttEmpty:    break;
    case ttOffset:   p = ++frame.m_ptr; p = *p ? p : 0; break;
    case ttCaptureL: frame.PushCapStack(); break;
    case ttCaptureR: frame.PopCapStack(); break;
    case ttBackref:  frame.m_end = MatchBackref(frame, tr.m_min - 1); break;
    case ttPushNum:  frame.PushNum(); break;
    case ttTestNum:  p = frame.TestNum(tr.m_min) ? p : 0; break;
    case ttIncNum:   frame.IncNum(); break;
    case ttPopNum:   frame.PopNum(); break;
    case ttAnchorL:  p = isbegl(p, s) ? p : 0; break;
    case ttAnchorR:  p = isendl(p)    ? p : 0; break;
    case ttAny:      p = *p ? n : 0; break;
    case ttChar:     p = *p == tr.m_min ? n : 0; break;
    case ttNChar:    p = *p == tr.m_min ? 0 : n; break;
    case ttRange:    p = *p >= tr.m_min && *p <= tr.m_max ? n : 0; break;
    case ttNRange:   p = *p >= tr.m_min && *p <= tr.m_max ? 0 : n; break;
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
    default:         throw RegexInvalidTransition();
    }

    // Final state reached
    if(frame.m_end && tr.m_type == ttFinal)
    {
      // Ignore if the empty string was matched
      if(frame.m_end <= frame.m_ptr)
      {
        frame.m_end = 0;
      }
      else
      {
        // Match succeeded, stop
        success = true;
        break;
      }
    }

    // Test match
    if(frame.m_end == 0)
    {
      // Out of options, match failed
      if(stack.size() == 0)
      {
        success = false;
        break;
      }
      
      // Backtrack to previous frame
      frame.Swap(stack.back());
      stack.pop_back();
    }
    else
    {
      // Advance current backrefs
      if(frame.m_capstack)
      {
        for(size_t i = 0; i < frame.m_capstack->size(); ++i)
        {
          (*frame.m_capstack)[i].m_end = frame.m_end;
        }
      }

      // Advance to next state
      frame.m_trans = tr.m_out;
    }
  }

  // Create result
  ImplResult result;
  result.m_success = success;
  result.m_result  = 0;

  // Create match result
  if(createMatchResult)
  {
    // Create match result object
    MatchResult* mr = new MatchResult;
    result.m_result = mr;
    mr->m_captures  = new List;

    // Fill object on success
    if(success)
    {
      mr->m_success = true;
      mr->m_matchId = m_rd->m_table[frame.m_trans].m_min;
      mr->m_text = String(frame.m_ptr, frame.m_end);
      mr->m_offset = frame.m_ptr - input.c_str();

      // Copy captures
      if(frame.m_captures)
      {
        for(size_t i = 0; i < frame.m_captures->size(); ++i)
        {
          mr->m_captures->Append(String(
            (*frame.m_captures)[i].m_ptr, 
            (*frame.m_captures)[i].m_end));
        }
      }
    }
  }

  // Apply named captures
  if(m_rd->m_namedCaptures.size())
  {
    // Create iterators
    NamedCaptureMap::iterator it, ie;
    it = m_rd->m_namedCaptures.begin();
    ie = m_rd->m_namedCaptures.end();

    // Enumerate captures
    Evaluator& eval = CurEval;
    for(; it != ie; ++it)
    {
      // Check index
      size_t index = (size_t)it->first;
      if(index >= frame.m_captures->size())
      {
        continue;
      }

      // Build captured string
      String capture = String(
        (*frame.m_captures)[index].m_ptr, 
        (*frame.m_captures)[index].m_end);

      // Apply to current scope
      eval.GetScope()->Set(it->second, capture);
    }
  }

  // Done
  return result;
}
