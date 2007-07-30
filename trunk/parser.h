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
#ifndef CSCRIPT_CONTEXT_H
#define CSCRIPT_CONTEXT_H

#include "types.h"
#include "tokens.h"
#include "var.h"

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
  Quad ParseFile(String const& filename);

  //
  // Parse string contents. Function is reentrant.
  // Code is added to existing buffer.
  // Returns offset of first instruction.
  //
  Quad ParseText(Char* text);

  //
  // Bytecode info
  //
  Byte* GetCode() const;
  Quad GetSize() const;
  Quad GetPos() const;

  //
  // Take over code ownership
  //
  Byte* ReleaseCode();

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
  Quad AddVar(String const& name);
  Quad GetVar(String const& name);

  //
  // Stack frames
  //
  void PushFrame(bool boundary = false);
  void PopFrame();

  //
  // Offsets
  //
  void PushOffset(String const& name);
  Quad PopOffset(String const& name);

  //
  // Functions
  //
  void PushFunction(String const& name);
  void PopFunction();
  Quad GetFunction(String const& name);
  void GenFunProlog();
  void AddParam(String const& name);

  //
  // Function calls
  //
  Function* FindFunction(String const& name);
  void PushCall(String const& name);
  void PopCall();
  void PushArg();

  //
  // Error handlers
  //
  void OnParseFailure();
  void OnSyntaxError();

private:

  //
  // Compile-time stack frame
  //
  typedef std::map<String, Quad> FrameVars;
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
  // Function call
  //
  struct FnCall
  {
    FnCall() : m_fn (0), m_args (0) 
    {
    }

    Function* m_fn;
    Quad      m_args;
  };

  //
  // Local typedefs
  //
  typedef std::map<Variant, std::list<Quad>, Variant::LessExact> Literals;
  typedef std::list<StackFrame> Stack;
  typedef std::map<String, std::stack<Quad> > LabelStack;
  typedef std::map<String, Function> FunctionMap;
  typedef std::stack<FnCall> CallStack;

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
  // Members
  //
  Byte*         m_code;
  size_t        m_size;
  size_t        m_used;
  Quad          m_vnum;
  size_t        m_depth;
  Literals      m_literals;
  Stack         m_stack;
  LabelStack    m_labels;
  FunctionMap   m_functions;
  Function*     m_fun;
  CallStack     m_calls;
  Lexer*        m_lexer;

 };

#endif // #ifndef CSCRIPT_CONTEXT_H
