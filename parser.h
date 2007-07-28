#ifndef CSCRIPT_CONTEXT_H
#define CSCRIPT_CONTEXT_H

#include <list>
#include <stack>
#include <algorithm>

#include "var.h"
#include "types.h"
#include "tokens.h"

//
// Forward declares
//
class Lexer;

//
// Parser implementation
//
class Parser
{
public:

  //
  // Construction
  //
  Parser();

  //
  // Destruction
  //
  ~Parser();

  //
  // Parse file contents. Function is reentrant.
  // Code is added to existing buffer.
  // Returns offset of first instruction.
  //
  Quad ParseFile(std::wstring const& filename);

  //
  // Parse string contents. Function is reentrant.
  // Code is added to existing buffer.
  // Returns offset of first instruction.
  //
  Quad ParseText(std::wstring const& text);

  //
  // Bytecode info
  //
  Byte* GetCode() const;
  Quad GetSize() const;
  Quad GetPos() const;

  //
  // Bytecode generation
  //
  void PushByte(Byte ch);
  void PushWord(Word sh);
  void PushQuad(Quad ln);
  void PushRVal(Variant const&);
  void SetQuad(Quad offset, Quad value);

  //
  // Variables
  //
  Quad AddVar(std::wstring const& name);
  Quad GetVar(std::wstring const& name);

  //
  // Stack frames
  //
  void PushFrame(bool boundary = false);
  void PopFrame();

  //
  // Offsets
  //
  void PushOffset(std::wstring const& name);
  Quad PopOffset(std::wstring const& name);

  //
  // Functions
  //
  void PushFunction(std::wstring const& name);
  void PopFunction();
  Quad GetFunction(std::wstring const& name);
  void GenFunProlog();
  void AddParam(std::wstring const& name);

private:

  //
  // Compile-time stack frame
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

  //
  // Function information
  //
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

  //
  // Local typedefs
  //
  typedef std::map<Variant, std::list<Quad>, Variant::LessExact> Literals;
  typedef std::list<StackFrame> Stack;
  typedef std::map<std::wstring, std::stack<Quad> > LabelStack;
  typedef std::map<std::wstring, Function> FunctionMap;

  //
  // Parse lexer contents. Returns offset of first instruction.
  //
  Quad ParseImpl(Lexer& lexer);

  //
  // Write literals to file
  //
  void WriteLiterals();

  //
  // Resize code buffer
  //
  void Reserve(size_t size);

  //
  // Code buffer
  //
  Byte*         m_code;
  size_t        m_size;
  size_t        m_used;

  //
  // Variable numbering
  //
  Quad          m_vnum;

  //
  // Code info
  //
  size_t        m_depth;
  Literals      m_literals;
  Stack         m_stack;
  LabelStack    m_labels;
  FunctionMap   m_functions;
  Function*     m_fun;

 };

#endif // #ifndef CSCRIPT_CONTEXT_H
