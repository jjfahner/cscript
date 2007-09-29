#ifndef CSCRIPT_EVAL_H
#define CSCRIPT_EVAL_H

#include "types.h"
#include "var.h"
#include "report.h"
#include "scope.h"
#include "args.h"
#include "ast.h"
#include "function.h"

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

protected:

  //
  // Scope handling
  //
  class AutoScope;

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
  void EvalTryStatement(Ast* node);

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
  GlobalScope*  m_global;
  Scope*        m_scope;

};

//////////////////////////////////////////////////////////////////////////
//
// Control flow exceptions
//

//
// Base class for all script exceptions
//
struct script_exception : public std::exception
{
  Ast* m_node;
  script_exception(Ast* node) : std::exception(), m_node (node) {}
};

struct break_exception : public script_exception
{
  break_exception(Ast* node) : script_exception (node) {}
  ~break_exception() throw () {}
};

struct continue_exception : public script_exception
{
  continue_exception(Ast* node) : script_exception (node) {}
  ~continue_exception() throw () {}
};

struct reset_exception : public script_exception
{
  Ast m_node;
  reset_exception() : script_exception (&m_node), m_node (empty_statement) {}
  ~reset_exception() throw () {}
};

struct return_exception : public script_exception
{
  VariantRef m_value;
  return_exception(Ast* node) : script_exception (node) {}
  return_exception(Ast* node, VariantRef const& value) : script_exception (node), m_value (value) {}
  ~return_exception() throw() {}
};

struct user_exception : public script_exception
{
  VariantRef m_value;
  user_exception(Ast* node) : script_exception (node) {}
  user_exception(Ast* node, VariantRef const& value) : script_exception (node), m_value (value) {}
  ~user_exception() throw() {}
};

#endif // CSCRIPT_EVAL_H
