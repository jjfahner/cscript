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
  struct Instance;
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

  //
  // Expression handlers
  //
  VariantRef EvalExpression(Ast* node);
  VariantRef EvalLValue(Ast* node);
  VariantRef EvalListLiteral(Ast* node);
  VariantRef EvalFunctionCall(Ast* node);
  VariantRef EvalNativeCall(NativeCallInfo* fun, Ast* call);
  VariantRef EvalScriptCall(Ast* fun, Ast* call);
  VariantRef EvalNewExpression(Ast* node);
  VariantRef EvalMemberExpression(Ast* node);
  VariantRef EvalMemberCall(Ast* node);

  //
  // Members
  //
  Reporter    m_reporter;
  Scope*      m_global;
  Scope*      m_scope;
  Classes     m_classes;

};

#endif // CSCRIPT_EVAL_H
