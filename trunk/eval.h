#ifndef CSCRIPT_EVAL_H
#define CSCRIPT_EVAL_H

#include "types.h"
#include "var.h"
#include "report.h"

class Ast;

class Evaluator
{
public:

  //
  // Construction
  //
  Evaluator();

  //
  // Evaluation
  //
  void Eval(String code);

protected:

  //
  // Scope handling
  //
  struct Scope;
  struct AutoScope;
  void PushScope(Ast*);
  void PopScope();

  //
  // Implementation
  //
  void        EvalStatement(Ast* node);
  VariantRef  EvalExpression(Ast* node);
  VariantRef  EvalLValue(Ast* node);
  void        EvalVarDecl(Ast* node);
  void        EvalFunDecl(Ast* node);
  VariantRef  EvalFunctionCall(Ast* node);
  void        EvalForStatement(Ast* node);
  void        EvalForeachStatement(Ast* node);
  void        EvalWhileStatement(Ast* node);

  //
  // Members
  //
  Reporter    m_reporter;
  Scope*      m_global;
  Scope*      m_scope;

};

#endif // CSCRIPT_EVAL_H
