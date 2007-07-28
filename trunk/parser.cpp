#ifdef _MSC_VER
#define _CRT_SECURE_NO_WARNINGS
#endif

#include "parser.h"
#include "cscript.c"
#include "lexer.h"
#include "native.h"

Parser::Parser() : 
m_code (0),
m_size (0),
m_used (0),
m_vnum (0),
m_fun  (0),
m_depth(0)
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
Parser::ParseFile(std::wstring const& filename)
{
  // Create lexer for file
  Lexer lexer;
  lexer.SetFile(filename);

  // Parse
  return ParseImpl(lexer);
}

Quad
Parser::ParseText(std::wstring const& text)
{
  // Create lexer for text
  Lexer lexer;
  lexer.SetText(text);

  // Parse
  return ParseImpl(lexer);
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
Parser::AddVar(std::wstring const& name)
{
  StackFrame& frame = m_stack.front();
  
  // Check whether it exists
  if(frame.m_vars.count(name))
  {
    throw std::runtime_error("Duplicate variable name");
  }

  // Insert into stack frame
  Quad index = ++m_vnum;
  frame.m_vars[name] = index;

  // Return index
  return index;
}

Quad 
Parser::GetVar(std::wstring const& name)
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
  throw std::runtime_error("Undeclared variable");
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
Parser::PushOffset(std::wstring const& name)
{
  m_labels[name].push(GetPos());
}

Quad 
Parser::PopOffset(std::wstring const& name)
{
  std::stack<Quad>& stack = m_labels[name];
  Quad offset = stack.top();
  stack.pop();
  return offset;
}

void 
Parser::PushFunction(std::wstring const& name)
{
  // Disallow nested functions
  if(m_fun)
  {
    throw std::runtime_error("Function declarations may not be nested");
  }
  
  // Check for declaration of function
  if(m_functions.count(name))
  {
    throw std::runtime_error("Duplicate function declaration");
  }

  // Generate a jump to bypass function code
  PushByte(TOK_JMP);
  PushOffset(L"function_declaration");
  PushQuad(0);

  // Create new function
  m_fun = &m_functions[name];
  m_fun->m_offset = GetPos();
}

void 
Parser::GenFunProlog()
{
  // Start new stackframe
  PushFrame(true);

  // Generate variables for parameters
  Function::Names::reverse_iterator it, ie;
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
  SetQuad(PopOffset(L"function_declaration"), GetPos());

  // Release function
  m_fun = 0;
}

Quad 
Parser::GetFunction(std::wstring const& name)
{
  // Find function
  FunctionMap::iterator it = m_functions.find(name);
  if(it == m_functions.end())
  {
    throw std::runtime_error("Function undefined");
  }

  // Return offset
  return it->second.m_offset;
}

void
Parser::CallFunction(std::wstring const& name)
{
  // Find function
  FunctionMap::iterator it = m_functions.find(name);
  if(it != m_functions.end())
  {
    // Function call
    PushByte(TOK_CALL); 
    PushQuad(it->second.m_offset);
    return;
  }

  // Find native function
  Quad index = FindNative(name);
  if(index != -1)
  {
    // Native call
    PushByte(TOK_CALLN);
    PushQuad(index);
    return;
  }

  // Unknown function
  throw std::runtime_error("Function undefined");
}

void 
Parser::AddParam(std::wstring const& name)
{
  // Check for duplicates
  if(std::find(m_fun->m_params.begin(), 
               m_fun->m_params.end(), name) != 
               m_fun->m_params.end())
  {
    throw std::runtime_error("Duplicate parameter name");
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
