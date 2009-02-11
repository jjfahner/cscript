//////////////////////////////////////////////////////////////////////////
//
// This file is � 2007 - 2009 JJ Fahner <jan-jaap@jan-jaap.net>
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
#include "io.h"
#include "file.h"

class Lexer;
struct script_exception;

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
  static Scope& GetGlobalScope();

  //
  // Construction
  //
  Evaluator();

  //
  // Retrieve current lexer
  //
  Lexer* GetLexer() const {
    return m_lexer;
  }

  //
  // Evaluation of string/file
  //
  Value Eval(String code, bool isFileName = false);

  // Evaluation of ast tree
  void Eval(Object* astRoot);

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
  Object* ParseNativeCall(String const& declaration);

  //
  // Allocate node
  //
  Object* AllocNode(AstTypes type, 
    Value const& a1 = Value(), 
    Value const& a2 = Value(), 
    Value const& a3 = Value(), 
    Value const& a4 = Value());

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
  static Value::Bool   ValBool  (Value const& val);
  static Value::Int    ValInt   (Value const& val);
  static Value::String ValString(Value const& val);

  //
  // Value operations
  //
  static int  Compare(Value const& lhs, Value const& rhs);
  static Value ValAdd(Value const& lhs, Value const& rhs);
  static Value ValSub(Value const& lhs, Value const& rhs);
  static Value ValMul(Value const& lhs, Value const& rhs);
  static Value ValDiv(Value const& lhs, Value const& rhs);
  static Value ValMod(Value const& lhs, Value const& rhs);
  static Value ValNeg(Value const& lhs);
  static Value ValNot(Value const& lhs);

  //
  // Public members
  //
  // TODO move to protected
  //
  Reporter  m_reporter;
  File*     m_file;
  Lexer*    m_lexer;
  Object*   m_native;

private:

  //
  // Scope handling
  //
  class AutoScope;

  //
  // Evaluation of expression/statement
  //
  RValue& EvalExpression(Object* node);
  void    EvalStatement(Object* node);

  //
  // Statement handlers
  //
  void EvalStatementSeq(Object* node);
  void EvalVarDecl(Object* node);
  void EvalFunDecl(Object* node);
  void EvalIncludeStatement(Object* node);
  void EvalForStatement(Object* node);
  void EvalForeachStatement(Object* node);
  void EvalWhileStatement(Object* node);
  void EvalSwitchStatement(Object* node);
  void EvalIfStatement(Object* node);
  void EvalReturnStatement(Object* node);
  void EvalExternDecl(Object* node);
  void EvalTryStatement(Object* node);

  //
  // Expression handlers
  //
  RValue& EvalLValue(Object* node);
  RValue& EvalListLiteral(Object* node);
  RValue& EvalJsonLiteral(Object* node);
  RValue& EvalNewExpression(Object* node);
  RValue& EvalMemberExpression(Object* node);
  RValue& EvalThisExpression(Object* node);
  RValue& EvalAssignment(Object* node);
  RValue& EvalBinary(Object* node);
  RValue& EvalTernary(Object* node);
  RValue& EvalPrefix(Object* node);
  RValue& EvalPostfix(Object* node);
  RValue& EvalIndex(Object* node);
  RValue& EvalConversion(Object* node);
  RValue& EvalClosure(Object* node);
  RValue& EvalXmlExpression(Object* node);
  RValue& EvalFunctionMember(Object* node);
  RValue& EvalFunctionIndex(Object* node);

  //
  // Function handlers
  //
  friend class ScriptFunction;
  RValue& EvalFunctionCall(Object* node);
  RValue& EvalFunctionCall(Object* node, Function* fun, Object* owner, Object* arguments);
  RValue& EvalScriptCall(ScriptFunction* fun, Arguments& args);

  //
  // Convert a value in-place
  //
  void PerformConversion(Value& value, TypeInfo const& newType);

  //
  // Evaluate argument list
  //
  void EvalPositionalArguments(Object* node, Function* fun, Object* arglist, Arguments& args);
  void EvalNamedArguments(Object* node, Function* fun, Object* arglist, Arguments& args);

  //
  // Create a temporary
  //
  RValue& MakeTemp(Value const& value);
  
  //
  // Store a temporary
  //
  RValue& StoreTemp(RValue* rval);

  //
  // Build xml tree
  //
  Object* BuildXmlTree(Object* ast, Object* node);

  //
  // Scopes
  //
  Scope* m_global;
  Scope* m_scope;

  //
  // Allocations
  //
  size_t        m_allocs;

  //
  // Temporaries
  //
  typedef std::vector<RValue*> TempVec;
  TempVec m_temporaries;

};

//////////////////////////////////////////////////////////////////////////
//
// Control flow exceptions
//

//
// Base class for all script exceptions
//
struct script_exception : public std::runtime_error
{
  Object* m_node;
  script_exception(Object* node, char const* message = "") : std::runtime_error(message), m_node (node) {}
  script_exception(Object* node, String const& message) : std::runtime_error(message.c_str()), m_node (node) {}
  ~script_exception() throw () {}
};

struct break_exception : public script_exception
{
  break_exception(Object* node) : script_exception (node) {}
  ~break_exception() throw () {}
};

struct continue_exception : public script_exception
{
  continue_exception(Object* node) : script_exception (node) {}
  ~continue_exception() throw () {}
};

struct reset_exception : public std::exception
{
  ~reset_exception() throw () {}
};

struct return_exception : public script_exception
{
  Value m_value;
  return_exception(Object* node) : script_exception (node) {}
  return_exception(Object* node, Value const& value) : script_exception (node), m_value (value) {}
  ~return_exception() throw() {}
};

struct user_exception : public script_exception
{
  Value m_value;
  user_exception(Object* node) : script_exception (node) {}
  user_exception(Object* node, Value const& value) : script_exception (node), m_value (value) {}
  ~user_exception() throw() {}
};

//////////////////////////////////////////////////////////////////////////

inline RValue& 
Evaluator::MakeTemp(Value const& value)
{
  Temporary* temp = new Temporary(value);
  return StoreTemp(temp);
}

inline RValue& 
Evaluator::StoreTemp(RValue* rval)
{
  m_temporaries.push_back(rval);
  return *rval;
}

#endif // CSCRIPT_EVAL_H
