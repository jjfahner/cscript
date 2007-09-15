#ifndef CSCRIPT_EVAL_H
#define CSCRIPT_EVAL_H

#include "types.h"
#include "var.h"
#include "report.h"
#include "rtscope.h"

class Ast;
struct NativeCallInfo;

class Evaluator
{
public:

  //
  // Run
  //
  static void Run();

  //
  // Construction
  //
  Evaluator();

  //
  // Evaluation of single code line
  //
  void Eval(String code);

  //
  // Evaluation of an expression
  //
  VariantRef EvalExpression(Ast* node);

protected:

  //
  // Scope handling
  //
  struct AutoScope;
  void PushScope(Scope*);
  void PopScope();
  typedef std::list<Scope*> ScopeList;
  
  //
  // Classes
  //
  typedef std::map<String, Class*> Classes;

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
  void EvalParameter(Ast* node);

  //
  // Expression handlers
  //
  VariantRef EvalLValue(Ast* node);
  VariantRef EvalListLiteral(Ast* node);
  VariantRef EvalFunctionCall(Ast* node);
  VariantRef EvalNativeCall(NativeCallInfo* fun, Ast* call);
  VariantRef EvalScriptCall(Ast* fun, Ast* call);
  VariantRef EvalNewExpression(Ast* node);
  VariantRef EvalMemberExpression(Ast* node);
  VariantRef EvalMemberCall(Ast* node);
  VariantRef EvalThisExpression(Ast* node);

  //
  // Members
  //
  Reporter      m_reporter;
  GlobalScope*  m_global;
  ScopeList     m_scopes;
  Scope*        m_scope;
  Classes       m_classes;

};

#endif // CSCRIPT_EVAL_H
