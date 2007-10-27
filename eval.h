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
#ifndef CSCRIPT_EVAL_H
#define CSCRIPT_EVAL_H

#include "types.h"
#include "report.h"
#include "scope.h"
#include "args.h"
#include "ast.h"
#include "function.h"
#include "typeinfo.h"
#include "value.h"
#include "object.h"

class Lexer;

//
// Evaluator
//
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
  Value Eval(String code, bool isFileName = false);

  //
  // Evaluation of an expression
  //
  Value EvalExpression(Ast* node);
  void  EvalStatement(Ast* node);

  //
  // Reset the evaluator
  //
  void Reset();

  //
  // Parse a string
  //
  void ParseText(char const* text);

  //
  // Parse a file
  //
  void ParseFile(String const& filename);

  //
  // Parse native call
  //
  Ast* ParseNativeCall(String const& declaration);

  //
  // Find a declared type
  //
  Class* FindType(String const& name);

  //
  // Allocate node
  //
  Ast* AllocAst(AstTypes type, 
    AstData const& a1 = AstData(), 
    AstData const& a2 = AstData(), 
    AstData const& a3 = AstData(), 
    AstData const& a4 = AstData());

  //
  // Error handlers
  //
  void OnParseFailure();
  void OnSyntaxError();

  //
  // Run garbage collector
  //
  void Collect();

  //
  // Value conversions
  //
  Value::Bool   ValBool  (Value const& val);
  Value::Int    ValInt   (Value const& val);
  Value::String ValString(Value const& val);

  //
  // Value operations
  //
  int   Compare(Value const& lhs, Value const& rhs);
  Value ValAdd(Value const& lhs, Value const& rhs);
  Value ValSub(Value const& lhs, Value const& rhs);
  Value ValMul(Value const& lhs, Value const& rhs);
  Value ValDiv(Value const& lhs, Value const& rhs);
  Value ValMod(Value const& lhs, Value const& rhs);
  Value ValNeg(Value const& lhs);
  Value ValNot(Value const& lhs);

  //
  // Public members
  //
  // TODO move to protected
  //
  Reporter  m_reporter;
  File*     m_file;
  Lexer*    m_lexer;
  Ast*      m_native;

protected:

  //
  // Scope handling
  //
  class AutoScope;

  //
  // Statement handlers
  //
  void EvalStatementSeq(Ast* node);
  void EvalVarDecl(Ast* node);
  void EvalFunDecl(Ast* node);
  void EvalExpStmt(Ast* node);
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
  Value EvalLValue(Ast* node);
  Value EvalListLiteral(Ast* node);
  Value EvalNewExpression(Ast* node);
  Value EvalMemberExpression(Ast* node);
  Value EvalThisExpression();
  Value EvalAssignment(Ast* node);
  Value EvalBinary(Ast* node);
  Value EvalTernary(Ast* node);
  Value EvalPrefix(Ast* node);
  Value EvalPostfix(Ast* node);
  Value EvalIndex(Ast* node);
  Value EvalConversion(Ast* node);

  //
  // Function handlers
  //
  friend class ScriptFunction;
  Value EvalFunctionCall(Ast* node);
  Value EvalScriptCall(ScriptFunction* fun, Arguments& args);

  //
  // Convert a value in-place
  //
  void PerformConversion(Value& value, TypeInfo const& newType);

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
  size_t        m_allocs;

  typedef std::vector<Object*> ObjectVec;
  ObjectVec     m_temporaries;

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
  Value m_value;
  return_exception(Ast* node) : script_exception (node) {}
  return_exception(Ast* node, Value const& value) : script_exception (node), m_value (value) {}
  ~return_exception() throw() {}
};

struct user_exception : public script_exception
{
  Value m_value;
  user_exception(Ast* node) : script_exception (node) {}
  user_exception(Ast* node, Value const& value) : script_exception (node), m_value (value) {}
  ~user_exception() throw() {}
};

#endif // CSCRIPT_EVAL_H
