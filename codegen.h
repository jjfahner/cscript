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
  void Generate(Ast* root, bool release);

  //
  // Write to output
  //
  void Write();

  //
  // Execute code
  //
  void Execute();

private:

  //
  // Validation
  //
  void Validate(Ast*);
  
  //
  // Annotation
  //
  void Annotate(Ast*);
  void AnnotateStatementSequence(Ast*);

  //
  // Optimization
  //
  Ast* Optimize(Ast*);
  Ast* OptimizeIfStatement(Ast*);
  Ast* OptimizeForStatement(Ast*);
  Ast* OptimizeBinaryExpression(Ast*);
  Ast* OptimizeTernaryExpression(Ast*);
  Ast* OptimizeStatementSequence(Ast*);
  Ast* OptimizeExpressionStatement(Ast*);
  Ast* OptimizeCompoundStatement(Ast*);
  Ast* OptimizeAssignmentExpression(Ast*);

  //
  // High-level code generation
  //
  void GenerateCode(Ast*);
  Quad GenerateFunction(Ast*);
  void GenerateFunctionCall(Ast*);

  //
  // Low-level code generation
  //
  void PushData(Byte* data, Quad size);
  void PushByte(Byte);
  void PushWord(Word);
  void PushQuad(Quad);
  void FillQuad(Quad offset, Quad data);

  //
  // Push reference to literal
  //
  void PushLiteral(Variant const&);

  //
  // Stack frames
  //
  void PushFrame();
  void PopFrame();

  //
  // Scopes
  //
  void PushScope();
  void PopScope();

  //
  // Push empty quad, return offset
  //
  Quad PushPatch();
  void FixPatch(Quad);

  //
  // Reserve space for code
  //
  void Reserve(Quad);

  //
  // Containers
  //
  typedef std::list<Quad> QuadList;
  typedef std::map<String, QuadList> CallList;
  typedef std::map<String, Ast*> Functions;
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
