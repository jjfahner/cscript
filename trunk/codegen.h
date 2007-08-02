#ifndef CSCRIPT_CODEGEN_H
#define CSCRIPT_CODEGEN_H

#include "ast.h"
#include "var.h"
#include "scope.h"

class CodeGenerator
{
public:

  //
  // Construction
  //
  CodeGenerator();

  //
  // Destruction
  //
  ~CodeGenerator();

  //
  // Generate code in parse tree
  //
  void Generate(Ast*);

  //
  // Write to output
  //
  void Write();

private:

  //
  // High-level code generation
  //
  void GenerateCode(Ast*);

  //
  // Low-level code generation
  //
  void PushData(Byte* data, Quad size);
  void PushByte(Byte);
  void PushWord(Word);
  void PushQuad(Quad);
  
  //
  // Push call to function
  //
  void PushCall(String const& name);
  
  //
  // Push reference to literal
  //
  void PushLiteral(Variant const&);

  //
  // Push empty quad, return offset
  //
  Quad PushPatch();
  void FixPatch(Quad);

  //
  // Reserve space
  //
  void Reserve(Quad);

  //
  // Containers
  //
  typedef std::list<Quad> QuadList;
  typedef std::map<String, QuadList> CallList;
  typedef std::pair<Ast*, Quad> Function;
  typedef std::map<String, Function> Functions;
  typedef std::pair<Variant, Quad> Literal;
  typedef std::list<Literal> Literals;

  //
  // Member data
  //
  Byte*     m_code;
  Quad      m_size;
  Quad      m_used;
  Functions m_funs;
  CallList  m_calls;
  Literals  m_literals;
  Frame*    m_globals;
  Scope*    m_scope;

};


#endif // CSCRIPT_CODEGEN_H
