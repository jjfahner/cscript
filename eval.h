#ifndef CSCRIPT_EVAL_H
#define CSCRIPT_EVAL_H

#include "types.h"
#include "var.h"
#include "report.h"
#include "scope.h"

class Ast;
struct NativeCallInfo;

typedef std::vector<VariantRef> Arguments;

class Evaluator
{
public:

  //
  // Static top-level scope, used by 
  // all instances of Evaluator
  //
  static GlobalScope& GetGlobalScope();

  //
  // Construction
  //
  Evaluator();

  //
  // Evaluation of single code line
  //
  VariantRef Eval(String code);

  //
  // Evaluation of an expression
  //
  VariantRef EvalExpression(Ast* node);

  //
  // Reset the evaluator
  //
  void Reset();

  //
  // Retrieve list of classes
  //
  VariantRef GetClassList() const;

  //
  // Retrieve list of functions
  //
  VariantRef GetFunctionList() const;

protected:

  //
  // Scope handling
  //
  class AutoScope;
  void PushScope(Scope*);
  void PopScope(bool doDelete = true);
  typedef std::list<Scope*> ScopeList;
  
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
  void EvalExternDecl(Ast* node);

  //
  // Expression handlers
  //
  VariantRef EvalLValue(Ast* node);
  VariantRef EvalListLiteral(Ast* node);
  VariantRef EvalNewExpression(Ast* node);
  VariantRef EvalMemberExpression(Ast* node);
  VariantRef EvalThisExpression();
  VariantRef EvalAssignment(Ast* node);
  VariantRef EvalBinary(Ast* node);
  VariantRef EvalTernary(Ast* node);
  VariantRef EvalPrefix(Ast* node);
  VariantRef EvalPostfix(Ast* node);
  VariantRef EvalIndex(Ast* node);

  //
  // Function handlers
  //
  VariantRef EvalFunctionCall(Ast* node);
  friend class ScriptFunction;
  VariantRef EvalScriptCall(ScriptFunction* fun, Arguments& args);
  friend class MemberFunction;
  VariantRef EvalMemberCall(MemberFunction* fun, Arguments& args);

  //
  // Evaluate expression expecting an instance
  //
  Instance* EvalInstance(Ast* node);

  //
  // Evaluate argument list
  //
  void EvalPositionalArguments(Function* fun, AstList const* arglist, Arguments& args);
  void EvalNamedArguments(Function* fun, AstList const* arglist, Arguments& args);

  //
  // Members
  //
  Reporter      m_reporter;
  GlobalScope*  m_global;
  ScopeList     m_scopes;
  Scope*        m_scope;

};

//////////////////////////////////////////////////////////////////////////
//
// Control flow exceptions
//

struct return_exception
{
  Ast* m_node;
  VariantRef m_value;
  return_exception(Ast* node) : m_node (node) {}
  return_exception(Ast* node, VariantRef const& value) : m_node (node), m_value (value) {}
};

struct break_exception
{
  Ast* m_node;
  break_exception(Ast* node) : m_node (node) {}
};

struct continue_exception
{
  Ast* m_node;
  continue_exception(Ast* node) : m_node (node) {}
};

struct reset_exception
{
};

#endif // CSCRIPT_EVAL_H
