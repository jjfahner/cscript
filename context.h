#ifndef CSCRIPT_CONTEXT_H
#define CSCRIPT_CONTEXT_H

#include <list>
#include <stack>
#include <algorithm>

#include "var.h"
#include "tokens.h"

typedef unsigned __int8   Byte;
typedef unsigned __int16  Word;
typedef unsigned __int32  Quad;

struct Function 
{
  typedef std::list<std::wstring> Names;

  Function() :
  m_offset (0)
  {
  }

  Quad    m_offset;
  Names   m_params;
};

class ParseContext
{
public:

  ParseContext() : 
  m_code (0),
  m_size (0),
  m_used (0),
  m_vnum (0),
  m_fun  (0)
  {
    // Base frame
    PushFrame();
  }

  //
  // Destructor
  //
  ~ParseContext()
  {
    // Pop base frame
    PopFrame();

    // Check stack size
    if(m_stack.size() && !std::uncaught_exception())
    {
      throw std::runtime_error("Stack not empty after parse");
    }
  }

  //////////////////////////////////////////////////////////////////////////
  //
  // Bytecode management
  //

  Byte* GetCode() const
  {
    return m_code;
  }

  Quad GetSize() const
  {
    return (Quad)m_used;
  }

  void PushByte(Byte ch)
  {
    Reserve(m_used + 1);
    *(m_code+m_used) = ch;
    m_used += 1;
  }

  void PushWord(Word sh)
  {
    Reserve(m_used + 2);
    *(Word*)(m_code+m_used) = sh;
    m_used += 2;
  }

  void PushQuad(Quad ln)
  {
    Reserve(m_used + 4);
    *(Quad*)(m_code+m_used) = ln;
    m_used += 4;
  }

  void SetQuad(Quad offset, Quad value)
  {
    *(Quad*)(m_code+offset) = value;
  }

  void Reserve(size_t size)
  {
    if(m_size < size)
    {
      m_code = (Byte*) realloc(m_code, size * 2);
      m_size = size * 2;
    }
  }

  Quad GetPos() const
  {
    return (Quad) m_used;
  }

  //////////////////////////////////////////////////////////////////////////
  //
  // Stack management
  //
  
  Quad AddLiteral(Variant const& value)
  {
    unsigned long index = (Quad) m_literals.size();
    m_literals[index] = value;
    return index;
  }
  Quad AddLiteral(std::wstring const& value, Variant::SubTypes type)
  {
    unsigned long index = (Quad) m_literals.size();
    m_literals[index] = Variant(value, type);
    return index;
  }
  
  Variant const& GetLiteral(Quad id) const
  {
    Literals::const_iterator it;
    if((it = m_literals.find(id)) == m_literals.end())
    {
      throw std::runtime_error("Unknown literal");
    }
    return it->second;
  }

  Quad AddVar(std::wstring const& name)
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

  Quad GetVar(std::wstring const& name)
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

  //////////////////////////////////////////////////////////////////////////
  //
  // Stack frames
  //

  void PushFrame(bool boundary = false)
  {
    // Push new frame on the stack
    m_stack.push_front(StackFrame(boundary));
  }

  void PopFrame()
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

  void PushOffset(std::wstring const& name)
  {
    m_labels[name].push(GetPos());
  }

  Quad PopOffset(std::wstring const& name)
  {
    std::stack<Quad>& stack = m_labels[name];
    Quad offset = stack.top();
    stack.pop();
    return offset;
  }

  //////////////////////////////////////////////////////////////////////////
  //
  // Functions
  //

  void PushFunction(std::wstring const& name)
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

    // Generate a jump for top-level code
    PushByte(TOK_JMP);
    PushOffset(L"function_declaration");
    PushQuad(0);

    // Create new function
    m_fun = &m_functions[name];

    // Set offset
    m_fun->m_offset = GetPos();
  }

  void GenFunProlog()
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

  void PopFunction()
  {
    // Check function
    if(m_fun == 0)
    {
      throw std::runtime_error("Attempt to leave a nonexistent function");
    }

    // Remove stackframe
    PopFrame();

    // Push return code
    PushByte(TOK_RVALUE);
    PushQuad(AddLiteral(Variant()));
    PushByte(TOK_RET);

    // Resolve the jump
    SetQuad(PopOffset(L"function_declaration"), GetPos());

    // Release function
    m_fun = 0;
  }

  Quad GetFunction(std::wstring const& name)
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

  void AddParam(std::wstring const& name)
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

private:

  //
  // Machine code
  //
  Byte*   m_code;
  size_t  m_size;
  size_t  m_used;
  Quad    m_vnum;

  //
  // Constants
  //
  typedef std::map<Quad, Variant> Literals;
  Literals m_literals;

  //
  // Stack frame
  //
  typedef std::map<std::wstring, Quad> FrameVars;
  struct StackFrame
  {
    StackFrame(bool boundary) : 
    m_boundary (boundary)
    {
    }
    bool      m_boundary;
    FrameVars m_vars;
  };
  typedef std::list<StackFrame> Stack;
  Stack m_stack;

  //
  // Label stack
  //
  typedef std::map<std::wstring, std::stack<Quad> > LabelStack;
  LabelStack m_labels;

  //
  // Functions
  //
  typedef std::map<std::wstring, Function> FunctionMap;
  FunctionMap m_functions;
  Function*   m_fun;

 };

#endif // #ifndef CSCRIPT_CONTEXT_H
