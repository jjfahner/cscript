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

#include <cscript.h>
#include <report.h>
#include <variable.h>
#include <ast.h>
#include <gc.h>

#include <vector>
#include <iosfwd>

class Object;
class List;
class CSLexer;
class Scope;
class Function;
class ScriptFunction;
class Arguments;
class SourceFile;
class NamespaceScope;

struct ScriptException;

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
  // Evaluation of string/file
  //
  Value Eval(String code, bool isFileName = false);

  //
  // Reset the evaluator
  //
  void Reset();

  //
  // Run garbage collector
  //
  GC::CollectInfo Collect();

  //
  // Retrieve current scope
  //
  Scope* GetScope() const {
    return m_scope;
  }

  //////////////////////////////////////////////////////////////////////////
  //
  // Public to allow access from parser
  //

  //
  // Evaluation of ast tree
  //
  Value Eval(Object* astRoot);

  //
  // Set result node from parsing
  //
  void SetResultNode(Object* node) {
    m_resultNode = node;
  }

  //
  // Enable parser debugging
  //
  void DebugParser(bool debug) {
    m_debugParser = true;
  }

  //
  // Parse native call
  //
  static Object* ParseNativeCall(String const& declaration);

  //
  // Error handlers
  //
  void OnParseFailure();
  void OnSyntaxError();

  //
  // Create a temporary
  //
  void MakeTemp(Value const& value);

private:

  //
  // Parse a string
  //
  Value ParseText(String const& text, bool executeImmediate = true);

  //
  // Parse a file
  //
  Value ParseFile(String const& filename, bool executeImmediate = true);

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
  Value EvalExpression(Object* node);
  Value EvalStatement(Object* node);

  //
  // Statement handlers
  //
  void EvalStatementSeq(Object* node);
  void EvalNamespace(Object* node);
  void EvalVariableDeclaration(Object* node);
  void EvalFunctionDeclaration(Object* node);
  void EvalNativeDeclaration(Object* node);
  void EvalIncludeStatement(Object* node);
  void EvalForStatement(Object* node);
  void EvalForeachStatement(Object* node);
  void EvalWhileStatement(Object* node);
  void EvalSwitchStatement(Object* node);
  void EvalIfStatement(Object* node);
  void EvalReturnStatement(Object* node);
  void EvalExternDeclaration(Object* node);
  void EvalTryStatement(Object* node);
  void EvalUnsetStatement(Object* node);

  //
  // Expression handlers
  //
  Value EvalAssignment(Object* node);
  Value EvalUnqualifiedId(Object* node);
  Value EvalQualifiedId(Object* node);
  Value EvalListLiteral(Object* node);
  Value EvalMapLiteral(Object* node);
  Value EvalJsonLiteral(Object* node);
  Value EvalNewExpression(Object* node);
  Value EvalMemberExpression(Object* node);
  Value EvalThisExpression(Object* node);
  Value EvalBinary(Object* node);
  Value EvalTernary(Object* node);
  Value EvalPrefix(Object* node);
  Value EvalPostfix(Object* node);
  Value EvalTypeOf(Object* node);
  Value EvalConversion(Object* node);
  Value EvalClosure(Object* node);
  Value EvalXmlExpression(Object* node);
  Value EvalFunctionMember(Object* node);
  Value EvalFunctionIndex(Object* node);
  Value EvalShellCommand(Object* node);
  Value EvalTypeConversion(Object* node);
  Value EvalOperatorDeclaration(Object* node);
  Value EvalIndex(Object* node);
  Value EvalObjectIndex(Object* node, Object* lhs);
  Value EvalListIndex(Object* node, List* lhs);
  Value EvalListAppend(Object* node);

  void EvalLValue(Object* node, Object*& obj, Value& key, bool scopeIsOwner = true);

  //
  // Function handlers
  //
  friend class ScriptFunction;
  Value EvalFunctionCall(Object* node);
  Value EvalFunctionCall(Object* node, Function* fun, Object* owner, Object* arguments);
  Value EvalScriptCall(ScriptFunction* fun, Arguments& args);

  //
  // Convert a value in-place
  //
  void ConvertInPlace(Object* node, Value& value, Value::Types newType);

  //
  // Convert a value
  //
  Value Convert(Object* node, Value const& value, Value::Types newType);

  //
  // Evaluate argument list
  //
  void EvalArguments(Object* node, /*Function* fun, */Object* arglist, Arguments& args);

  //
  // Build xml tree
  //
  Object* BuildXmlTree(Object* ast, Object* node);

  //
  // Open a file
  //
  bool OpenFile(String const& filename, std::ifstream& file);

  typedef std::vector<Value> TempVec;

  typedef std::vector<String> StringVec;

  //
  // Members
  //
  Object*         m_resultNode;
  NamespaceScope* m_global;
  Scope*          m_scope;
  size_t          m_allocs;
  TempVec         m_temporaries;
  Reporter        m_reporter;
  bool            m_debugParser;
  StringVec       m_fileNames;

};

inline void
Evaluator::MakeTemp(Value const& value)
{
  m_temporaries.push_back(value);
}

//////////////////////////////////////////////////////////////////////////
//
// Control flow exceptions
//

//
// Base class for all script exceptions
//
struct ScriptException : public std::runtime_error
{
  Object* m_node;
  ScriptException(Object* node, char const* message = "") : std::runtime_error(message), m_node (node) {}
  ScriptException(Object* node, String const& message) : std::runtime_error(message.c_str()), m_node (node) {}
  ~ScriptException() throw () {}
};

struct BreakException : public ScriptException
{
  BreakException(Object* node) : ScriptException (node) {}
  ~BreakException() throw () {}
};

struct ContinueException : public ScriptException
{
  ContinueException(Object* node) : ScriptException (node) {}
  ~ContinueException() throw () {}
};

struct ResetException : public std::exception
{
  ~ResetException() throw () {}
};

struct ReturnException : public ScriptException
{
  Value m_value;
  ReturnException(Object* node) : ScriptException (node) {}
  ReturnException(Object* node, Value value) : ScriptException (node), m_value (value) {}
  ~ReturnException() throw() {}
};

struct CatchableException : public ScriptException
{
  Value m_value;
  CatchableException(Value value) : ScriptException(0), m_value (value) {}
  CatchableException(Object* node) : ScriptException (node) {}
  CatchableException(Object* node, Value value) : ScriptException (node), m_value (value) {}
  ~CatchableException() throw() {}
};

struct UserException : public CatchableException
{
  UserException(Object* node) : CatchableException (node) {}
  UserException(Object* node, Value value) : CatchableException (node, value) {}
  ~UserException() throw() {}
};

#endif // CSCRIPT_EVAL_H
