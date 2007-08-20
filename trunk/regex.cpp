//////////////////////////////////////////////////////////////////////////
//
// This file is © 2007 JJ Fahner <jan-jaap@jan-jaap.net>
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
#include "regex.h"

//////////////////////////////////////////////////////////////////////////
//
// Helpers
//

inline bool isany(int c)
{
  switch(c)
  {
  case '\0':
  case '\n':
  case '\r':
    return false;
  }
  return true;
}

#ifdef _MSC_VER
inline bool isblank(int c)
{
  return c == ' ' || c == '\t';
}
#endif

//////////////////////////////////////////////////////////////////////////
//
// Regex implementation
//

Regex::Regex(char const* pattern) :
m_seq   (0),
m_start (0)
{
  if(pattern)
  {
    Parse(pattern);
  }
}

Regex::~Regex()
{
  Reset();
}

void 
Regex::Reset()
{
  m_seq   = 0;
  m_start = 0;
  m_final = 0;

  // Clear transition table
  for(size_t i = 0; i < m_transVec.size(); ++i)
  {
    delete m_transVec[i];
  }
  m_transVec.resize(0);

  // Delete ranges in table
  while(m_ranges.size())
  {
    delete m_ranges.front();
    m_ranges.pop_front();
  }
}

//////////////////////////////////////////////////////////////////////////
//
// Pattern matching
//

Regex::MatchResults 
Regex::Match(char const* start)
{
  // Stack size limit
  static const size_t stackLimit = 10000;

  // Stack for current/next states
  Stack cur, next;

  // Pointer to the longest match
  char const* matchPtr = 0;
  char const* matchOff = 0;

  // Match results
  MatchResults results;

  // Initialize the stack from the start state
  cur.push_back(StackFrame(m_start, start, start));

  // Run until no more matches can be found
  for(;;)
  {
    // Handle stack states
    Stack::iterator it = cur.begin();
    Stack::iterator ie = cur.end();
    for(; it != ie; ++it)
    {
      // Alias some variables
      StackFrame& frame   = *it;
      char const*     source  = frame.m_source;
      State       inState = frame.m_state;

      // Iterators to transitions for state
      TransitionList::iterator ti, te;
      ti = m_transVec[inState]->begin();
      te = m_transVec[inState]->end();
      
      // Iterate through transitions
      for(; ti != te; ++ti)
      {
        // Transition
        Transition& trans = *ti;

        // Modified by match rules
        char const* ptr = 0;
        char const* offset = frame.m_offset;
        State outState = trans.m_outState;

        // Perform match on transition char
        switch(trans.m_type)
        {
          // Empty transition
        case mtEmpty: 
          ptr = source;
          break;

          // Moving start point
        case mtAnchor:
          if(*(source+1))
          {
            ptr      = source + 1;
            offset   = offset + 1;
            outState = m_start;
          }
          break;

          // Start of backref
        case mtNestS:
          ptr = source;
          frame.AddBackref();
          frame.m_build = true;
          break;

          // End of backref
        case mtNestE:
          ptr = source;
          frame.m_build = false;
          break;

          // Single character
        case mtChar:
          ptr = *source == trans.m_char ? source + 1 : 0;
          break;

          // Match string
        case mtString:
          break;

          // Any character
        case mtAnyChar:
          ptr = isany(*source) ? source + 1 : 0;
          break;

          // Range
        case mtRange:
          ptr = MatchRange(*trans.m_range, *source) ? source + 1 : 0;
          break;

          // Markers
        case mtBegLine:
          ptr = source == start ? source :
            *(source-1) == '\n' ? source : 0;
          break;
        case mtEndLine:
          ptr = *source == 0 ? source : 0;
          break;

          // Character classes
        case mtUCase: case mtLCase: case mtAlpha: 
        case mtAlnum: case mtDigit: case mtXigit: 
        case mtBlank: case mtSpace: case mtCntrl: 
        case mtGraph: case mtPrint:
          ptr = MatchCharClass(trans.m_type, *source) ? source + 1 : 0;
          break;

          // Backreferences
        case mtRef1: case mtRef2: case mtRef3:
        case mtRef4: case mtRef5: case mtRef6: 
        case mtRef7: case mtRef8: case mtRef9:
          ptr = MatchBackref(trans.m_type, frame);
          break;

          // Unknown code
        default:
          throw std::runtime_error("Invalid match type");
        }

        // No match; next iteration
        if(ptr == 0)
        {
          continue;
        }

        // Store match if leftmost and longer than previous match
        if(outState == m_final)
        {
          if(matchOff == 0      || // No previous matches
             offset < matchOff  || // Leftmost match wins
             offset == matchOff && ptr > matchPtr)
          {
          }
          if((matchOff == 0 || offset <= matchOff) && ptr >= matchPtr)
          {
            matchOff = offset;
            matchPtr = ptr;
            results.m_backrefs = frame.m_backrefs;
          }
          continue;
        }
        
        // Append new stack frame
        StackFrame& nextFrame = *next.insert(
                   next.end(), StackFrame());

        // Copy members
        nextFrame.m_state     = outState;
        nextFrame.m_offset    = offset;
        nextFrame.m_source    = ptr;
        nextFrame.m_build     = frame.m_build;
        nextFrame.m_backrefs  = frame.m_backrefs;

        // Append to active backref
        if(ptr > source && nextFrame.m_build)
        {
          // Append all characters in the diff
          while(source < ptr)
          {
            nextFrame.AppendBackref(*source++);
          }
        }
      }
    }

    // Empty stack means we're done
    if(next.size() == 0)
    {
      break;
    }

    // Guard against stack overflow - this
    // usually means exponential matching.
    if(next.size() > stackLimit)
    {
      //std::cout << "Aborted: stack limit reached\n";
      return MatchResults();
    }

    // Swap stacks
    cur.swap(next);
    next.clear();
  }

  // Copy complete match
  if(matchPtr)
  {
    results.m_complete = matchOff;
    results.m_complete = results.m_complete.substr(0, matchPtr - matchOff);
  }

  // Return results
  return results;
}

bool 
Regex::MatchRange(CharRange const& range, char wch)
{
  CharRange::PairList::const_iterator it, ie;
  it = range.m_pairs.begin();
  ie = range.m_pairs.end();

  // Never match end of line
  if(wch == 0)
  {
    return false;
  }

  // Iterate over entries in range
  for(; it != ie; ++it)
  {
    // Perform match
    bool match = false;
    if(it->first == 0)
    {
      match = MatchCharClass((MatchTypes)it->second, wch);
    }
    else
    {
      match = wch >= it->first && wch <= it->second;
    }

    // Inversion
    if(match)
    {
      return !range.m_invert;
    }
  }

  // No matches found
  return range.m_invert;
}

bool 
Regex::MatchCharClass(MatchTypes type, char wch)
{
  switch(type)
  {
  case mtUCase: return isupper(wch)  ? true : false;
  case mtLCase: return islower(wch)  ? true : false;
  case mtAlpha: return isalpha(wch)  ? true : false;
  case mtAlnum: return isalnum(wch)  ? true : false;
  case mtDigit: return isdigit(wch)  ? true : false;
  case mtXigit: return isxdigit(wch) ? true : false;
  case mtBlank: return isblank(wch)  ? true : false;
  case mtSpace: return isspace(wch)  ? true : false;
  case mtCntrl: return iscntrl(wch)  ? true : false;
  case mtGraph: return isgraph(wch)  ? true : false;
  case mtPrint: return isprint(wch)  ? true : false;
  default:      throw std::logic_error("Invalid pattern");
  }
}

char const* 
Regex::MatchBackref(MatchTypes backref, StackFrame const& frame)
{
  // Backref index
  size_t index = backref - mtRef1;
  if(frame.m_backrefs.size() <= index)
  {
    throw std::runtime_error("Matching empty back reference");
  }

  // Compare the string
  size_t len = frame.m_backrefs[index].length();
  if(strncmp(frame.m_backrefs[index].c_str(), frame.m_source, len))
  {
    return 0;
  }

  // Succeeded
  return frame.m_source + len;
}

//////////////////////////////////////////////////////////////////////////
//
// Pattern parsing
//

void
Regex::Parse(char const* pattern)
{
  // Clear current state
  Reset();

  // Parse the expression
  State cin, con;
  ParseExpression(pattern, cin, con);

  // Check end of expression
  if(*pattern)
  {
    throw std::runtime_error("Invalid pattern");
  }

  // Wrap in empty transitions
  m_start = ++m_seq;
  m_final = ++m_seq;
  AddTransition(m_start, cin);
  AddTransition(con, m_final);

  // Create transition for moving startpoint
  GetTransListAt(m_start).push_back(
      Transition(m_start, mtAnchor));
}

void 
Regex::ParseExpression(char const*& pattern, State& is, State& os)
{
  // Parse a branch
  ParseBranch(pattern, is, os);

  // Alternation
  if(*pattern == '|')
  {
    // Parse right-hand side
    State cis, cos;
    ParseExpression(++pattern, cis, cos);

    // Create intermediates
    State tis = ++m_seq;
    State tos = ++m_seq;

    // Create transitions
    AddTransition(tis, is);
    AddTransition(tis, cis);
    AddTransition(os,  tos);
    AddTransition(cos, tos);

    // Replace results
    is = tis;
    os = tos;
  }
}

void 
Regex::ParseBranch(char const*& pattern, State& is, State& os)
{
  // Reset result states
  is = 0;
  os = 0;

  // Parse branch content
  for(;;)
  {
    // End of branch
    switch(*pattern)
    {
    case '\0': 
    case '|' : 
    case ')' :
      return;
    }

    // Next piece
    State cis, cos;
    ParsePiece(pattern, cis, cos);
    if(is == 0)
    {
      // First in branch
      is = cis;
      os = cos;
    }
    else
    {
      // Next in branch
      AddTransition(os, cis);
      os = cos;
    }
  }
}

void 
Regex::ParsePiece(char const*& pattern, State& is, State& os)
{
  // Store starting point
  char const* pattern_start = pattern;

  // Parse atom
  State cis, cos;
  ParseAtom(pattern, cis, cos);

  // Parse quantifier
  size_t min, max;
  ParseQuantifier(pattern, min, max);

  // Zero or more, aka '*'
  if(min == 0 && max == 0)
  {
    is = ++m_seq;
    os = ++m_seq;
    AddTransition(is,  os);
    AddTransition(is,  cis);
    AddTransition(cos, is);
    return;
  }

  // Zero or one, aka '?'
  if(min == 0 && max == 1)
  {
    is = ++m_seq;
    os = ++m_seq;
    AddTransition(is,  os);
    AddTransition(is,  cis);
    AddTransition(cos, os);
    return;
  }

  // Exactly one
  if(min == 1 && max == 1)
  {
    is = cis;
    os = cos;
    return;
  }

  // One or more, aka '+'
  if(min == 1 && max == 0)
  {
    AddTransition(cos, cis);
    is = cis;
    os = cos;
    return;
  }

  // Not supported
  throw std::runtime_error("Extended quantifier not supported");

  // Initialize loop
  State ts;
  ts = cos;
  is = cis;
  os = ++m_seq;

  // Upper bound check
  for(size_t cur = 0; ; ++cur)
  {
    // Lower bound
    if(cur == min && max == 0)
    {
    }

    // Generate instance
    if(cur)
    {
      // Generate new instance
      char const* pt = pattern_start;
      ParseAtom(pt, cis, cos);

      // Intermediate transitions
      AddTransition(ts, cis);
      ts = cos;
    }

    // Add output transition
    if(cur >= min)
    {
      AddTransition(cis, os);
    }
  }
}

void 
Regex::ParseAtom(char const*& pattern, State& is, State& os)
{
  switch(*pattern)
  {
    // Start of line
  case '^':
    AddTypeTransition(is, os, mtBegLine);
    break;

    // End of line
  case '$':
    AddTypeTransition(is, os, mtEndLine);
    break;

    // Any character
  case '.':   
    AddTypeTransition(is, os, mtAnyChar);
    break;

    // Grouped subexpression
  case '(':
    ParseNested(pattern, is, os);
    break;

    // Character range
  case '[':
    ParseRange(pattern, is, os);
    break;
  
    // Escape character
  case '\\': 
    switch(*++pattern)
    {
    case 'n': AddCharTransition(is, os, '\n');     break;
    case 'r': AddCharTransition(is, os, '\r');     break;
    case 't': AddCharTransition(is, os, '\t');     break;
    case 'd': AddTypeTransition(is, os, mtDigit);  break;
    case 'l': AddTypeTransition(is, os, mtLCase);  break;
    case 'u': AddTypeTransition(is, os, mtUCase);  break;
    case 's': AddTypeTransition(is, os, mtSpace);  break;
    case 'w': AddTypeTransition(is, os, mtAlnum);  break;
    case '1': AddTypeTransition(is, os, mtRef1);   break;
    case '2': AddTypeTransition(is, os, mtRef2);   break;
    case '3': AddTypeTransition(is, os, mtRef3);   break;
    case '4': AddTypeTransition(is, os, mtRef4);   break;
    case '5': AddTypeTransition(is, os, mtRef5);   break;
    case '6': AddTypeTransition(is, os, mtRef6);   break;
    case '7': AddTypeTransition(is, os, mtRef7);   break;
    case '8': AddTypeTransition(is, os, mtRef8);   break;
    case '9': AddTypeTransition(is, os, mtRef9);   break;
    default : AddCharTransition(is, os, *pattern); break;
    }
    break;

    // Regular character
  default:
    AddCharTransition(is, os, *pattern);
    break;
  }
  
  // Move pattern
  ++pattern;
}


void 
Regex::ParseNested(char const*& pattern, State& is, State& os)
{
  // Sanity check
  if(*pattern++ != '(')
  {
    throw std::logic_error("Missing '(' in Regex::ParseNested()");
  }

  // Check for ignored backref
  bool backref = true;
  if(*pattern == '?' && *(pattern+1) == ':')
  {
    backref = false;
    pattern += 2;
  }

  // Parse nested expression
  State cis, cos;
  ParseExpression(pattern, cis, cos);

  // Validity check (do not remove ')', ParseAtom will)
  if(*pattern != ')')
  {
    throw std::runtime_error("Invalid pattern: missing ')'");
  }

  // Generate new sequence numbers if remembering
  if(backref)
  {
    is = ++m_seq;
    os = ++m_seq;

    // Generate nesting transitions
    // These will be used for backrefs
    AddTransition(is,  cis, mtNestS);
    AddTransition(cos, os,  mtNestE);
  }
  else
  {
    // Simply return the inner expression
    is = cis;
    os = cos;
  }
}

void 
Regex::ParseRange(char const*& pattern, State& is, State& os)
{
  // Sanity check
  if(*pattern++ != '[')
  {
    throw std::logic_error("Missing '[' in Regex::ParseRange()");
  }

  // Create range
  CharRange* range = new CharRange;
  range->m_invert  = false;
  m_ranges.push_back(range);

  // Check inversion
  if(*pattern == '^')
  {
    range->m_invert = true;
    ++pattern;
  }

  // Handle contents
  for(;;)
  {
    // End of range
    if(*pattern == ']')
    {
      break;
    }

    // Init pair
    CharRange::CharPair pair;
    pair.first  = 0;
    pair.second = 0;

    // Character classes
    if(*pattern == '[' && *(pattern+1) == ':')
    {
      pattern += 2;

      // Parse name
      std::string name;
      while(isalpha(*pattern))
      {
        name += *pattern++;
      }

      // Check closure
      if(*pattern != ':' || *(pattern+1) != ']')
      {
        throw std::runtime_error("Invalid pattern: expected ']'");
      }
      pattern += 2;

      // Determine value
      pair.second = ParseCharClass(name.c_str());
      if(pair.second == mtEmpty)
      {
        throw std::runtime_error("Invalid pattern: unknown character class");
      }
    }
    else
    {
      // Range values
      pair.first  = *pattern++;
      pair.second = pair.first;

      // Range marker
      if(*pattern == '-')
      {
        ++pattern;
        pair.second = *pattern++;
      }

      // Check ordering
      if(pair.first > pair.second)
      {
        std::swap(pair.first, pair.second);
      }
    }

    // Add pair to range
    range->m_pairs.push_back(pair);
  }

  // Insert into transition table
  is = ++m_seq;
  os = ++m_seq;
  Transition trans(os, mtRange);
  trans.m_range = range;

  GetTransListAt(is).push_back(trans);

  // Validity check (do not remove ']', ParseAtom will)
  if(*pattern != ']')
  {
    throw std::runtime_error("Invalid pattern: missing ')'");
  }
}

size_t 
ParseNumber(char const*& pattern, size_t defval = 0)
{
  // No digit - return default
  if(!isdigit(*pattern))
  {
    return defval;
  }

  // Parse number
  size_t num = 0;
  for(;;)
  {
    if(isdigit(*pattern))
    {
      num *= 10;
      num += *pattern - '0';
      ++pattern;
    }
    else
    {
      return num;
    }
  }
}

void 
Regex::ParseQuantifier(char const*& pattern, size_t& min, size_t& max)
{
  // Initialize quantifier
  min = 1;
  max = 1;

  // Standard quantifiers
  switch(*pattern)
  {
  case '*': min = 0; max = 0; ++pattern; return;
  case '?': min = 0; max = 1; ++pattern; return;
  case '+': min = 1; max = 0; ++pattern; return;
  case '{': ++pattern; break;
  default : return;
  }

  // TODO implement non-greedy?

  // Extended quantifier
  min = max = ParseNumber(pattern, 0);
  if(*pattern == ',')
  {
    max = ParseNumber(++pattern, 0);
  }
  if(*pattern != '}')
  {
    throw std::runtime_error("Invalid pattern: missing '}'");
  }

  // Sanity check
  if(min && max && min > max)
  {
    throw std::runtime_error("Invalid pattern: incorrect quantifier");
  }
}

void
Regex::AddTransition(State inState, State outState, MatchTypes type)
{
  GetTransListAt(inState).push_back(Transition(outState, type));
}

void 
Regex::AddCharTransition(State& inState, State& outState, char over)
{
  inState  = ++m_seq;
  outState = ++m_seq;
  GetTransListAt(inState).push_back(Transition(outState, over));
}

void 
Regex::AddTypeTransition(State& inState, State& outState, MatchTypes type)
{
  inState  = ++m_seq;
  outState = ++m_seq;
  GetTransListAt(inState).push_back(Transition(outState, type));
}

//////////////////////////////////////////////////////////////////////////
//
// Mapping between character class names and constants
//
// TODO: This should be optimized
//

/*static*/ Regex::MatchTypes 
Regex::ParseCharClass(char const* charClass)
{
  static std::string s_upper ("upper");
  static std::string s_lower ("lower");
  static std::string s_alpha ("alpha");
  static std::string s_alnum ("alnum");
  static std::string s_digit ("digit");
  static std::string s_xdigit("xdigit");
  static std::string s_punct ("punct");
  static std::string s_blank ("blank");
  static std::string s_space ("space");
  static std::string s_cntr  ("cntr");
  static std::string s_graph ("graph");
  static std::string s_print ("print");

  if(charClass == s_upper)  return mtUCase;
  if(charClass == s_lower)  return mtLCase;
  if(charClass == s_alpha)  return mtAlpha;
  if(charClass == s_alnum)  return mtAlnum;
  if(charClass == s_digit)  return mtDigit;
  if(charClass == s_xdigit) return mtXigit;
  if(charClass == s_blank)  return mtBlank;
  if(charClass == s_space)  return mtSpace;
  if(charClass == s_cntr)   return mtCntrl;
  if(charClass == s_graph)  return mtGraph;
  if(charClass == s_print)  return mtPrint;
  
  return mtEmpty;
}

/*static*/ char const*
Regex::MatchTypeString(MatchTypes type)
{
  switch(type)
  {
  case mtEmpty: return "mtEmpty";
  case mtAnchor: return "mtAnchor";
  case mtNestS: return "mtNestS";
  case mtNestE: return "mtNestE";
  case mtChar: return "mtChar";
  case mtString: return "mtString";
  case mtAnyChar: return "mtAnyChar";
  case mtEndLine: return "mtEndLine";
  case mtBegLine: return "mtBegLine";
  case mtRange: return "mtRange";
  case mtUCase: return "mtUCase";
  case mtLCase: return "mtLCase";
  case mtAlpha: return "mtAlpha";
  case mtAlnum: return "mtAlnum";
  case mtDigit: return "mtDigit";
  case mtXigit: return "mtXigit";
  case mtBlank: return "mtBlank";
  case mtSpace: return "mtSpace";
  case mtCntrl: return "mtCntr";
  case mtGraph: return "mtGraph";
  case mtPrint: return "mtPrint";
  case mtRef1: return "mtRef1";
  case mtRef2: return "mtRef2";
  case mtRef3: return "mtRef3";
  case mtRef4: return "mtRef4";
  case mtRef5: return "mtRef5";
  case mtRef6: return "mtRef6";
  case mtRef7: return "mtRef7";
  case mtRef8: return "mtRef8";
  case mtRef9: return "mtRef9";
  default: return "Unknown match type";
  }
}

//////////////////////////////////////////////////////////////////////////
//
// Native call interface
//
#include "native.h"
#include "var.h"
#include "types.h"

NATIVE_CALL(match, 2, 3)
{
  // Check types
  ASSERT_TYPE(0, stString);
  ASSERT_TYPE(1, stString);
  
  // Fetch string pointers
  char const* pat = args[0]->GetString().c_str();
  char const* src = args[1]->GetString().c_str();

  // Retrieve offset
  int offset = 0;
  if(args.size() == 3)
  {
    ASSERT_TYPE(2, stInt);
    offset = (int) args[2]->GetInt();
  }

  // Compile pattern
  Regex r(pat);

  // Match source string
  Regex::MatchResults mr = r.Match(src + offset);

  // Return matched result
  return mr.m_complete;
}