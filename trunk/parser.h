#ifndef CSCRIPT_CONTEXT_H
#define CSCRIPT_CONTEXT_H

#include <list>
#include <stack>
#include <algorithm>

#include "var.h"
#include "types.h"
#include "tokens.h"

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

struct PredVarLessExact
{
  bool operator () (Variant const& lhs, Variant const& rhs) const
  {
    return lhs.Compare(rhs, true) < 0;
  }
};

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
  // Parse file contents. Function is reentrant
  //
  void Parse(std::wstring const& filename);

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
  // Local typedefs
  //
  typedef std::map<Variant, std::list<Quad>, PredVarLessExact> Literals;
  typedef std::list<StackFrame> Stack;
  typedef std::map<std::wstring, std::stack<Quad> > LabelStack;
  typedef std::map<std::wstring, Function> FunctionMap;

  //
  // Write literals to file
  //
  void WriteLiterals();
  void WriteBool(Variant::BoolType const&);
  void WriteInt(Variant::IntType const&);
  void WriteString(Variant::StringType const&);

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
