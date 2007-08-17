#ifndef CSCRIPT_ANNOTATE_H
#define CSCRIPT_ANNOTATE_H

#include "types.h"
#include "ast.h"
#include "astlist.h"

class Scope;
class Reporter;

class Annotator
{
public:

  //
  // Construction
  //
  Annotator(Reporter& reporter);

  //
  // Annotate tree
  //
  void Annotate(Ast* root);

private:

  //
  // Annotation implementation
  //
  void AnnotateImpl(Ast* node);
  void AnnotateFunction(Ast* node);
  void AnnotateLValue(Ast* node);
  void AnnotateTranslationUnit(Ast* node);
  void AnnotateStatementSequence(Ast* node);
  void AnnotateVariableDeclaration(Ast* node);
  void AnnotateStructDeclaration(Ast* node);
  void AnnotateNewExpression(Ast* node);
  void AnnotateMemberExpression(Ast* node);
  void AnnotateBreakStatement(Ast* node);
  void AnnotateContinueStatement(Ast* node);
  void AnnotateSwitchCase(Ast* node);

  //
  // Function call resolving
  //
  void ResolveCalls();

  //
  // Scope handling
  //
  void    PushScope(Ast* node);
  Scope*  PopScope(bool deleteScope = true);

  //
  // Reporter
  //
  Reporter& m_reporter;

  //
  // Stack for scoping. Used during annotation phase.
  //
  typedef std::stack<Scope*> VarIdStack;
  VarIdStack m_scopeStack;
  Scope* m_scope;

  //
  // Map of ast nodes
  //
  typedef std::map<String, Ast*> AstMap;

  //
  // List of function calls
  //
  AstList m_funcalls;
  
  //
  // List of declared functions
  //
  AstMap m_functions;

  //
  // List of declared structures
  //
  AstMap m_structs;

};

#endif // CSCRIPT_ANNOTATE_H
