//////////////////////////////////////////////////////////////////////////
//
// This file is © 2007 - 2009 JJ Fahner <jan-jaap@jan-jaap.net>
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

#include "cscript.h"
#include "report.h"
#include "variable.h"
#include "ast.h"

#include <vector>

class Object;
class Lexer;
class Scope;
class Function;
class ScriptFunction;
class Arguments;
class SourceFile;
class NamespaceScope;

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
  static Scope* GetGlobalScope();

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
  // Public members
  //
  // TODO move to protected
  //
  Reporter    m_reporter;
  SourceFile* m_file;
  Lexer*      m_lexer;
  Object*     m_native;

private:

  //
  // Error reporting
  //
  void ReportError(String text, Object* source = 0);

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
  void EvalNamespace(Object* node);
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
  RValue& EvalUnqualifiedId(Object* node);
  RValue& EvalQualifiedId(Object* node);
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
  void PerformConversion(Value& value, Value::Types newType);

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
  // Open a file
  //
  bool OpenFile(String const& path, SourceFile& file);

  //
  // Scopes
  //
  NamespaceScope* m_global;
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

struct CatchableException : public script_exception
{
  Value m_value;
  CatchableException(Object* node) : script_exception (node) {}
  CatchableException(Object* node, Value const& value) : script_exception (node), m_value (value) {}
  ~CatchableException() throw() {}
};

struct UserException : public CatchableException
{
  UserException(Object* node) : CatchableException (node) {}
  UserException(Object* node, Value const& value) : CatchableException (node, value) {}
  ~UserException() throw() {}
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
