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
m_start   (0),
m_final   (0)
{
}

inline State 
RegexCompiler::AddState()
{
  State state = m_stateSeq++;
  if(m_table.size() <= state)
  {
    m_table.resize(m_table.size() + 10);
  }
  return state;
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
RegexCompiler::AddZeroOrOne(Pair const& e, Pair& r)
{
  r.m_min = e.m_min;
  r.m_max = e.m_max;
  AddTransition(e.m_min, e.m_max);
}

inline void 
RegexCompiler::AddZeroOrMore(Pair const& e, Pair& r)
{
  r.m_min = e.m_min;
  r.m_max = AddState();
  AddTransition(e.m_min, r.m_max);
  AddTransition(e.m_max, r.m_max);
  AddTransition(e.m_max, e.m_min);
}

inline void 
RegexCompiler::AddOneOrMore(Pair const& e, Pair& r)
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
  m_start = result.m_min;
  m_final = result.m_max;
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

/*static*/ void 
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
}

void
RegexCompiler::OnSyntaxError(char ch)
{
}

//////////////////////////////////////////////////////////////////////////

// void 
// Regex::Compile(Ast::Expression* ex)
// {
//   CompileContext ctx;
// 
//   Pair result = ex->Compile(ctx);
// 
//   ctx.m_start = result.m_min;
//   ctx.m_finish = ctx.NewState();
//   ctx.AddTransition(result.m_max, ctx.m_finish);
// 
//   Match(ctx, "aaaxxxxxxxxxxy");
// }
// 
// Regex::Pair 
// Regex::Ast::Alternation::Compile(CompileContext& ctx)
// {
//   Pair result = { 0, 0 }, p;
// 
//   // Create wrapper states
//   result.m_min = ctx.NewState();
//   result.m_max = ctx.NewState();
// 
//   // Compile left subexpression and add transitions
//   p = m_lhs->Compile(ctx);
//   ctx.AddTransition(result.m_min, p.m_min);
//   ctx.AddTransition(p.m_max, result.m_max);
// 
//   // Compile right subexpression and add transitions
//   p = m_rhs->Compile(ctx);
//   ctx.AddTransition(result.m_min, p.m_min);
//   ctx.AddTransition(p.m_max, result.m_max);
//   
//   // Quantify the result
//   ctx.Quantify(result, m_min, m_max);
// 
//   // Done
//   return result;
// }
// 
// Regex::Pair 
// Regex::Ast::Sequence::Compile(CompileContext& ctx)
// {
//   Pair result = { 0, 0 };
// 
//   // Setup iterators
//   Expressions::iterator it, ie;
//   it = m_expressions.begin();
//   ie = m_expressions.end();
// 
//   // Compile and chain subexpressions
//   for(bool first = true; it != ie; ++it)
//   {
//     // Compile expression
//     Pair p = (*it)->Compile(ctx);
// 
//     // Chain expression
//     if(first)
//     {
//       first = false;
//       result = p;
//     }
//     else
//     {
//       ctx.AddTransition(result.m_max, p.m_min);
//       result.m_max = p.m_max;
//     }
//   }
// 
//   // Quantify the result
//   ctx.Quantify(result, m_min, m_max);
// 
//   // Done
//   return result;
// }
// 
// Regex::Pair 
// Regex::Ast::Range::Compile(CompileContext& ctx)
// {
//   // Setup iterators
//   Parts::iterator it, ie;
//   it = m_parts.begin();
//   ie = m_parts.end();
// 
//   // Create initial and terminal states
//   Pair result;
// 
//   // Normal and inverted ranges differ completely
//   if(m_invert)
//   {
//     // Create in state
//     result.m_min = ctx.NewState();
//     State min = result.m_min;
// 
//     // Create transitions for range pairs
//     for(; it != ie; ++it)
//     {
//       // Create out state
//       result.m_max = ctx.NewState();
// 
//       // Create negative range transition
//       ctx.AddTransition(
//         min, 
//         result.m_max, 
//         ttNRange, 
//         it->m_min, 
//         it->m_max);
// 
//       // Next min is current max
//       min = result.m_max;
//     }
// 
//     // Advance position in string
//     result.m_max = ctx.NewState();
//     ctx.AddTransition(min, result.m_max, ttNext);
//   }
//   else
//   {
//     // Create in and out states
//     result.m_min = ctx.NewState();
//     result.m_max = ctx.NewState();
// 
//     // Create transitions for range pairs
//     for(; it != ie; ++it)
//     {
//       ctx.AddTransition(
//         result.m_min, 
//         result.m_max, 
//         ttRange, 
//         it->m_min, 
//         it->m_max);
//     }
//   }
// 
//   // Quantify
//   ctx.Quantify(result, m_min, m_max);
// 
//   // Done
//   return result;
// }
// 
// Regex::Pair 
// Regex::Ast::Char::Compile(CompileContext& ctx)
// {
//   Pair result = { ctx.NewState(), ctx.NewState() };
//   
//   TransitionTypes type;
//   char value = 0;
//   switch(m_value)
//   {
//   case StartAnchor:
//     type = ttAnchorL;
//     break;
//   case EndAnchor:
//     type = ttAnchorR;
//     break;
//   case AnyChar:
//     type = ttAny;
//     break;
//   default:
//     type = ttChar;
//     value = m_value;
//     break;
//   }
// 
//   // Create simple transition over character match
//   ctx.AddTransition(
//     result.m_min, 
//     result.m_max, 
//     type, 
//     value, 
//     value);
//   
//   // Quantify
//   ctx.Quantify(result, m_min, m_max);
// 
//   // Done
//   return result;
// }
// 
// //////////////////////////////////////////////////////////////////////////
// 
// struct Frame
// {
//   void Set(State state = 0, char const* start = 0, char const* ptr = 0, Frame* next = 0)
//   {
//     m_state = state;
//     m_start = start;
//     m_ptr   = ptr;
//     m_next  = next;
//   }
// 
//   State       m_state;
//   char const* m_start;
//   char const* m_ptr;
//   Frame*      m_next;
// 
// };
// void 
// Regex::Match(CompileContext const& ctx, char const* text)
// {
//   // Create initial frame
//   Frame* cur = new Frame;
//   cur->Set(ctx.m_start, text, text);
// 
//   // Free frames
//   Frame* freelist = 0;
// 
//   // Temp pointer
//   Frame* t;
// 
//   // Current match
//   char const* matchStart = (char const*) -1;
//   char const* matchPtr   = 0;
// 
//   // Main match loop
//   size_t iterations = 0;
//   while(cur)
//   {
//     // Next frame list
//     Frame* next = 0;
// 
//     // Size of next frame list
//     size_t next_size = 0;
// 
//     // Enumerate frames
//     for(Frame* f = cur; f; )
//     {
//       // Enumerate transitions with this in state
//       SizeVec const& tv = ctx.m_table[f->m_state];
//       for(size_t i = 0; i < tv.size(); ++i)
//       {
//         char const* s = f->m_ptr;
//         char const* p = 0;
// 
//         // Try transion
//         Transition const& tr = ctx.m_transitions[tv[i]];
//         switch(tr.m_type)
//         {
//         case ttEmpty:
//           p = s;
//           break;
//         case ttNext:
//           p = s + 1;
//           break;
//         case ttStartPos:
//           break;
//         case ttAnchorL:
//           p = s == text ? s : 0;
//           break;
//         case ttAnchorR:
//           p = *s ? 0 : s;
//           break;
//         case ttAny:
//           p = *s ? s + 1 : 0;
//           break;
//         case ttChar:
//           p = *s == tr.m_min ? s + 1 : 0;
//           break;
//         case ttRange:
//           p = *s >= tr.m_min && *s <= tr.m_max ? s + 1 : 0;
//           break;
//         case ttNRange:
//           p = *s >= tr.m_min && *s <= tr.m_max ? 0 : s;
//           break;
//         }
// 
//         // Create new frame for match
//         if(p)
//         {
//           // Final state
//           if(tr.m_out == ctx.m_finish)
//           {
//             bool leftmost = f->m_start <= matchStart;
//             bool notshort = matchPtr == 0 || p - f->m_start >= matchPtr - matchStart;
//             if(leftmost && notshort)
//             {
//               matchStart = f->m_start;
//               matchPtr   = p;
//             }
//           }
//           else
//           {
//             // Create or retrieve a frame
//             Frame* n;
//             if(freelist)
//             {
//               n = freelist;
//               freelist = n->m_next;
//             }
//             else
//             {
//               n = new Frame;
//             }
// 
//             // Initialize and link into list
//             n->Set(tr.m_out, f->m_start, p, next);
//             next = n;
//             
//             // Check for limit
//             if(++next_size > 10000)
//             {
//               throw std::runtime_error("Regular expression stack overflow");
//             }
//           }
//         }
//       }
// 
//       // Point to next frame
//       t = f;
//       f = f->m_next;
//       
//       // Add old frame to the free list
//       t->m_next = freelist;
//       freelist = t;
//     }
// 
//     // Print size
//     std::cout << "Iteration " << ++iterations << " created " << next_size << " frames\n";
// 
//     // Next iteration
//     cur = next;
//   }
// 
//   // Delete freelist
//   while(freelist)
//   {
//     t = freelist;
//     freelist = t->m_next;
//     delete t;
//   }
// 
//   // Check match
//   if(matchPtr == 0)
//   {
//     std::cout << "\nFailed to match input string\n";
//   }
//   else
//   {
//     String result(matchStart, matchPtr);
//     std::cout << "\nMatched " << result << "\n";
//   }
// }
// 
