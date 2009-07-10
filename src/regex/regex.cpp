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

#include <iostream>

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

Regex::ImplResult 
Regex::MatchImpl(StringCRef input, bool createMatchResult)
{
  // Create initial frame
  Frame* cur = new Frame;
  char const* text = input.c_str();
  cur->Set(m_rd->m_start, text, text);

  bool optLeftmost = false;
  bool optShortest = false;

  // Free frames
  Frame* freelist = 0;

  // Temp pointer
  Frame* t;

  // Current match
  char const* matchStart = (char const*) -1;
  char const* matchPtr   = 0;

  // Main match loop
  size_t iterations = 0;
  while(cur)
  {
    // Next frame list
    Frame* next = 0;

    // Size of next frame list
    size_t next_size = 0;

    // Enumerate frames
    for(Frame* f = cur; f; )
    {
      // Enumerate transitions with this in state
      SizeVec const& tv = m_rd->m_table[f->m_state];
      for(size_t i = 0; i < tv.size(); ++i)
      {
        char const* o = f->m_start;
        char const* s = f->m_ptr;
        char const* p = 0;
        char const* n = s + 1;

        // Try transion
        Transition const& tr = m_rd->m_transitions[tv[i]];
        switch(tr.m_type)
        {
        case ttEmpty:
          p = s;
          break;
        case ttNext:
          p = s + 1;
          break;
        case ttOffset:
          ++o;
          if(*o) p = o;
          break;
        case ttAnchorL:
          p = s == text ? s : 0;
          break;
        case ttAnchorR:
          p = *s ? 0 : s;
          break;
        case ttAny:
          p = *s ? n : 0;
          break;
        case ttChar:
          p = *s == tr.m_min ? n : 0;
          break;
        case ttRange:
          p = *s >= tr.m_min && 
              *s <= tr.m_max ? n : 0;
          break;
        case ttNRange:
          p = *s >= tr.m_min && 
              *s <= tr.m_max ? 0 : s;
          break;
        case ccAlnum:
          p = isalnum(*s) ? n : 0;
          break;
        case ccAlpha:
          p = isalpha(*s) ? n : 0;
          break;
        case ccBlank:
          // TODO
          p = *s == ' ' ? n : 0; 
          break;
        case ccCntrl:
          p = iscntrl(*s) ? n : 0;
          break;
        case ccDigit:
          p = isdigit(*s) ? n : 0;
          break;
        case ccGraph:
          p = isgraph(*s) ? n : 0;
          break;
        case ccLower:
          p = islower(*s) ? n : 0;
          break;
        case ccPrint:
          p = isprint(*s) ? n : 0;
          break;
        case ccPunct:
          p = ispunct(*s) ? n : 0;
          break;
        case ccSpace:
          p = isspace(*s) ? n : 0;
          break;
        case ccUpper:
          p = isupper(*s) ? n : 0;
          break;
        case ccXdigit:
          p = isxdigit(*s) ? n : 0;
          break;
        }

        // Create new frame for match
        if(p)
        {
          // Final state
          if(tr.m_out == m_rd->m_final)
          {
            // Calculate current and existing match lengths
            size_t extLen = matchPtr - matchStart;
            size_t curLen = p - f->m_start;

            // Determine whether to accept this match
            bool acceptMatch = 
              matchPtr == 0 ? true :
              optShortest && curLen < extLen ? true :
             !optShortest && curLen > extLen ? true :
              optLeftmost && f->m_start < matchStart ? true :
              false;

            if(acceptMatch)
            {
              matchStart = f->m_start;
              matchPtr   = p;
            }
          }
          else
          {
            // Create or retrieve a frame
            Frame* n;
            if(freelist)
            {
              n = freelist;
              freelist = n->m_next;
            }
            else
            {
              n = new Frame;
            }

            // Initialize and link into list
            n->Set(tr.m_out, o, p, next);
            next = n;
            
            // Check for limit
            if(++next_size > 10000)
            {
              throw std::runtime_error("Regular expression stack overflow");
            }
          }
        }
      }

      // Point to next frame
      t = f;
      f = f->m_next;
      
      // Add old frame to the free list
      t->m_next = freelist;
      freelist = t;
    }

    // Print size
    // std::cout << "Iteration " << ++iterations << " created " << next_size << " frames\n";

    // Next iteration
    cur = next;
  }

  // Delete freelist
  while(freelist)
  {
    t = freelist;
    freelist = t->m_next;
    delete t;
  }

  // Create result
  ImplResult result;
  result.m_success = matchPtr != 0;
  result.m_result  = 0;

  // Create match result
  if(createMatchResult && result.m_success)
  {
    result.m_result = new MatchResult;
    result.m_result->m_text = String(matchStart, matchPtr);
    result.m_result->m_offset = matchStart - text;
  }

  // Done
  return result;
}
