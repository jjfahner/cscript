#ifndef CSCRIPT_CONTEXT_H
#define CSCRIPT_CONTEXT_H

#include <list>
#include <stack>

#include "var.h"
#include "tokens.h"

typedef unsigned __int8   Byte;
typedef unsigned __int16  Word;
typedef unsigned __int32  Quad;

class ParseContext
{
public:

  ParseContext() : 
  m_code (0),
  m_size (0),
  m_used (0),
  m_vnum (0)
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
    if(frame.count(name))
    {
      throw std::runtime_error("Duplicate variable name");
    }

    // Insert into stack frame
    Quad index = ++m_vnum;
    frame[name] = index;

    // Return index
    return index;
  }

  Quad GetVar(std::wstring const& name)
  {
    Stack::iterator frame = m_stack.begin();
    for(; frame != m_stack.end(); ++frame)
    {
      StackFrame::iterator it = frame->find(name);
      if(it != frame->end())
      {
        return it->second;
      }
    }
    throw std::runtime_error("Undeclared variable");
  }

  //////////////////////////////////////////////////////////////////////////
  //
  // Stack frames
  //

  void PushFrame()
  {
    // Push new frame on the stack
    m_stack.push_front(StackFrame());
  }

  void PopFrame()
  {
    StackFrame& frame = m_stack.front();
    
    // Generate stackframe cleanup
    StackFrame::iterator it = frame.begin();
    StackFrame::iterator ie = frame.end();
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
  typedef std::map<std::wstring, Quad> StackFrame;
  typedef std::list<StackFrame> Stack;
  Stack m_stack;

  //
  // Label stack
  //
  typedef std::map<std::wstring, std::stack<Quad> > LabelStack;
  LabelStack m_labels;

 };

#endif // #ifndef CSCRIPT_CONTEXT_H
