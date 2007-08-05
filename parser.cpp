//////////////////////////////////////////////////////////////////////////
//
// This file is � 2007 JJ Fahner <jan-jaap@jan-jaap.net>
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
#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif

#include "parser.h"
#include "cscript.c"
#include "lexer.h"
#include "native.h"
#include "file.h"

Parser::Parser() : 
m_code (0),
m_size (0),
m_used (0),
m_vnum (0),
m_fun  (0),
m_depth(0),
m_lexer(0)
{
  // Base frame
  PushFrame();
}

Parser::~Parser()
{
  // Pop base frame
  PopFrame();

  // Check stack size
  if(m_stack.size() && !std::uncaught_exception())
  {
    throw std::runtime_error("Stack not empty after parse");
  }
}

Quad 
Parser::ParseFile(String const& filename)
{
  // Create file
  File file;
  file.Open(filename);

  // Check type
  if(file.GetType() != File::source)
  {
    throw std::runtime_error("Invalid file");
  }

  // Create lexer for file
  Lexer lexer;
  lexer.SetText((Char*)file.GetData());

  // Push lexer on stack
  Lexer* prevlexer = m_lexer;
  m_lexer = &lexer;

  // Parse
  try
  {
    Quad result = ParseImpl(lexer);
    m_lexer = prevlexer;
    return result;
  }
  catch(...)
  {
    m_lexer = prevlexer;
    throw;
  }
}

Quad
Parser::ParseText(Char* text)
{
  // Create lexer for text
  Lexer lexer;
  lexer.SetText(text);

  // Push lexer on stack
  Lexer* prevlexer = m_lexer;
  m_lexer = &lexer;

  // Parse
  try
  {
    Quad result = ParseImpl(lexer);
    m_lexer = prevlexer;
    return result;
  }
  catch(...)
  {
    m_lexer = prevlexer;
    throw;
  }
}

Quad
Parser::ParseImpl(Lexer& lexer)
{
  // Add nesting level
  ++m_depth;

  // In case of start, clean up
  if(m_depth == 1)
  {
    // Clear literals
    m_literals.clear();

    // Reset function pointer
    m_fun = 0;
  }

  // Allocate parser
  void *pParser = CScriptParseAlloc(malloc);

  //CScriptParseTrace(stdout, "> ");

  // Store buffer position
  Quad offset = GetPos();

  // Try block for parser memory management
  try 
  {
    // Run parser loop
    Token token;
    while(lexer.Lex(token))
    {
      CScriptParse(pParser, token.m_type, token, this);
    }
    
    // Empty token to finalize parse
    CScriptParse(pParser, 0, token, this);

    // Destroy parser
    CScriptParseFree(pParser, free);
  }
  catch(...)
  {
    // Destroy parser
    CScriptParseFree(pParser, free);
  
    // Rethrow exception
    throw;
  }

  // End of code
  if(--m_depth == 0)
  {
    PushByte(TOK_HALT); 

    // Write literals to output
    WriteLiterals();
  }

  // Done
  return offset;
}

void 
Parser::OnParseFailure()
{
  Variant error("Parse failed on line ");
  error += m_lexer->GetLine();
  throw std::runtime_error(error.GetString());
}

void 
Parser::OnSyntaxError()
{
  Variant error("Syntax error on line ");
  error += m_lexer->GetLine();
  throw std::runtime_error(error.GetString());
}

Byte* 
Parser::GetCode() const
{
  return m_code;
}

Quad 
Parser::GetSize() const
{
  return (Quad)m_used;
}

Byte* 
Parser::ReleaseCode()
{
  Byte* code = m_code;
  m_code = 0;
  m_used = 0;
  m_size = 0;
  return code;
}

void 
Parser::PushByte(Byte ch)
{
  Reserve(m_used + 1);
  *(m_code+m_used) = ch;
  m_used += 1;
}

void 
Parser::PushWord(Word sh)
{
  Reserve(m_used + 2);
  *(Word*)(m_code+m_used) = sh;
  m_used += 2;
}

void 
Parser::PushQuad(Quad ln)
{
  Reserve(m_used + 4);
  *(Quad*)(m_code+m_used) = ln;
  m_used += 4;
}

void 
Parser::PushRVal(Variant const& value)
{
  PushByte(TOK_RVALUE);
  m_literals[value].push_back(GetPos());
  PushQuad(0);
}

void 
Parser::SetQuad(Quad offset, Quad value)
{
  *(Quad*)(m_code+offset) = value;
}

void 
Parser::Reserve(size_t size)
{
  if(m_size < size)
  {
    m_code = (Byte*) realloc(m_code, size * 2);
    m_size = size * 2;
  }
}

Quad 
Parser::GetPos() const
{
  return (Quad) m_used;
}

Quad 
Parser::AddVar(String const& name)
{
  StackFrame& frame = m_stack.front();
  
  // Check whether it exists
  if(frame.m_vars.count(name))
  {
    throw std::runtime_error("Duplicate variable name " + name);
  }

  // Insert into stack frame
  Quad index = ++m_vnum;
  frame.m_vars[name] = index;

  // Return index
  return index;
}

Quad 
Parser::GetVar(String const& name)
{
  Stack::iterator frame = m_stack.begin();
  for(; frame != m_stack.end(); ++frame)
  {
    FrameVars::iterator it = frame->m_vars.find(name);
    if(it != frame->m_vars.end())
    {
      return it->second;
    }
    if(frame->m_boundary)
    {
      break;
    }
  }
  throw std::runtime_error("Undeclared variable " + name);
}

void 
Parser::PushFrame(bool boundary)
{
  // Push new frame on the stack
  m_stack.push_front(StackFrame(boundary));
}

void 
Parser::PopFrame()
{
  StackFrame& frame = m_stack.front();
  
  // Generate stackframe cleanup
  FrameVars::iterator it = frame.m_vars.begin();
  FrameVars::iterator ie = frame.m_vars.end();
  for(; it != ie; ++it)
  {
    PushByte(TOK_UNDEF);
    PushQuad(it->second);
  }
  
  // Remove frame from stack
  m_stack.pop_front();
}

void 
Parser::PushOffset(String const& name)
{
  m_labels[name].push(GetPos());
}

Quad 
Parser::PopOffset(String const& name)
{
  std::stack<Quad>& stack = m_labels[name];
  Quad offset = stack.top();
  stack.pop();
  return offset;
}

void 
Parser::PushFunction(String const& name)
{
  // Disallow nested functions
  if(m_fun)
  {
    throw std::runtime_error("Function declarations may not be nested");
  }
  
  // Check for declaration of function
  if(m_functions.count(name))
  {
    throw std::runtime_error("Duplicate function declaration " + name);
  }

  // Generate a jump to bypass function code
  PushByte(TOK_JMP);
  PushOffset("function_declaration");
  PushQuad(0);

  // Create new function
  m_fun = &m_functions[name];
  m_fun->m_native = false;
  m_fun->m_offset = GetPos();
}

void 
Parser::GenFunProlog()
{
  // Start new stackframe
  PushFrame(true);

  // Generate variables for parameters
  StringList::reverse_iterator it, ie;
  it = m_fun->m_params.rbegin();
  ie = m_fun->m_params.rend();
  for(; it != ie; ++it)
  {
    PushByte(TOK_VARINIT);
    PushQuad(AddVar(*it));
  }
}

void 
Parser::PopFunction()
{
  // Check function
  if(m_fun == 0)
  {
    throw std::runtime_error("Attempt to leave a nonexistent function");
  }

  // Remove stackframe
  PopFrame();

  // Generate return value
  PushRVal(Variant());
  PushByte(TOK_RET);

  // Resolve the jump
  SetQuad(PopOffset("function_declaration"), GetPos());

  // Finalize function
  m_fun->m_minPar = m_fun->m_maxPar = (Quad) m_fun->m_params.size();

  // Release function
  m_fun = 0;
}

Quad 
Parser::GetFunction(String const& name)
{
  // Find function
  FunctionMap::iterator it = m_functions.find(name);
  if(it == m_functions.end())
  {
    throw std::runtime_error("Function " + name + " undefined");
  }

  // Return offset
  return it->second.m_offset;
}

Function* 
Parser::FindFunction(String const& name)
{
  // Find function
  FunctionMap::iterator it = m_functions.find(name);
  if(it != m_functions.end())
  {
    return &it->second;
  }

  // Find native function
  return FindNative(name);
}

void 
Parser::PushCall(String const& name)
{
  // Build call
  FnCall call;
  call.m_fn = FindFunction(name);

  // Unknown function
  if(call.m_fn == 0)
  {
    throw std::runtime_error("Function " + name + " undefined");
  }

  // Put new call on stack
  m_calls.push(call);
}

void 
Parser::PopCall()
{
  // Take call off stack
  FnCall call = m_calls.top();
  m_calls.pop();

  // Check argument count
  if(call.m_fn->m_minPar != -1 && call.m_args < call.m_fn->m_minPar)
  {
    throw std::runtime_error("Not enough arguments for call to " + call.m_fn->m_name);
  }
  if(call.m_fn->m_maxPar != -1 && call.m_args > call.m_fn->m_maxPar)
  {
    throw std::runtime_error("Too many arguments for call to " + call.m_fn->m_name);
  }

  // Generate code
  if(call.m_fn->m_native)
  {
    PushByte(TOK_CALLN);
    PushWord(call.m_fn->m_offset);
    PushWord(call.m_args);
  }
  else
  {
    PushByte(TOK_CALL);
    PushQuad(call.m_fn->m_offset);
  }
}

void 
Parser::PushArg()
{
  // Increase argument count
  ++m_calls.top().m_args;
}

void 
Parser::AddParam(String const& name)
{
  // Check for duplicates
  if(std::find(m_fun->m_params.begin(), 
               m_fun->m_params.end(), name) != 
               m_fun->m_params.end())
  {
    throw std::runtime_error("Duplicate parameter name " + name);
  }

  // Add to list of parameters
  m_fun->m_params.push_back(name);
}

void 
Parser::WriteLiterals()
{
  Literals::iterator it, ie;
  it = m_literals.begin();
  ie = m_literals.end();
  for(; it != ie; ++it)
  {
    // Reserve space for literal
    size_t len = it->first.WriteLength();
    Reserve(m_used + len);

    // Write value
    Quad pos = GetPos();
    it->first.Write(m_code + m_used);

    // Advance position
    m_used += len;

    // Patch into code
    std::list<Quad>::iterator pi, pe;
    pi = it->second.begin();
    pe = it->second.end();
    for(; pi != pe; ++pi)
    {
      SetQuad(*pi, pos);
    }
  }
}