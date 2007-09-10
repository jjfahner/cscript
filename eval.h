#ifndef CSCRIPT_EVAL_H
#define CSCRIPT_EVAL_H

#include "types.h"
#include "var.h"
#include "report.h"

class Ast;
struct NativeCallInfo;

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
  // Classes
  //
  struct Class;

  //
  // Statement handlers
  //
  void EvalStatement(Ast* node);
  void EvalStatementSeq(Ast* node);
  void EvalVarDecl(Ast* node);
  void EvalFunDecl(Ast* node);
  void EvalClassDecl(Ast* node);
  void EvalForStatement(Ast* node);
  void EvalForeachStatement(Ast* node);
  void EvalWhileStatement(Ast* node);
  void EvalSwitchStatement(Ast* node);
  void EvalIfStatement(Ast* node);
  void EvalReturnStatement(Ast* node);

  //
  // Expression handlers
  //
  VariantRef EvalExpression(Ast* node);
  VariantRef EvalLValue(Ast* node);
  VariantRef EvalListLiteral(Ast* node);
  VariantRef EvalFunctionCall(Ast* node);
  VariantRef EvalNativeCall(NativeCallInfo* fun, Ast* call);
  VariantRef EvalScriptCall(Ast* fun, Ast* call);

  //
  // Members
  //
  Reporter    m_reporter;
  Scope*      m_global;
  Scope*      m_scope;

};

#endif // CSCRIPT_EVAL_H
