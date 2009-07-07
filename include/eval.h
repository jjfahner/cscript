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
#include <value.h>
#include <context.h>
#include <gc.h>

#include <vector>
#include <iosfwd>

class Object;
class List;
class CSLexer;
class Function;
class ScriptFunction;
class Arguments;
class SourceFile;

class Scope;
class NamespaceScope;

struct ScriptException;

//
// Evaluator
//
class Evaluator
{
public:

  //
  // Construction
  //
  Evaluator();

  //
  // Destruction
  //
  ~Evaluator();

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
  // Enable parser debugging
  //
  void DebugParser(bool debug) {
    m_debugParser = true;
  }

  //
  // Error handlers
  //
  void OnParseFailure();
  void OnSyntaxError();

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
  void EvalExpression(Object* node);
  void EvalStatement(Object* node);

  //
  // Statement handlers
  //
  //void EvalNamespace(Object* node);
  void EvalVariableDeclaration(Object* node);
  void EvalFunctionDeclaration(Object* node);
  void EvalIncludeStatement(Object* node);
  void EvalForStatement(Object* node);
  void EvalForeachStatement(Object* node);
  void EvalWhileStatement(Object* node);
  void EvalSwitchStatement(Object* node);
  void EvalReturnStatement(Object* node);
  void EvalExternDeclaration(Object* node);
  void EvalTryStatement(Object* node);
  void EvalUnsetStatement(Object* node);

  //
  // Expression handlers
  //
  void EvalAssignment(Object* node);
  //void EvalQualifiedId(Object* node);
  void EvalListLiteral(Object* node);
  void EvalMapLiteral(Object* node);
  void EvalJsonLiteral(Object* node);
  void EvalNewExpression(Object* node);
  void EvalMemberExpression(Object* node);
  void EvalThisExpression(Object* node);
  void EvalBinary(Object* node);
  void EvalPrefix(Object* node);
  void EvalPostfix(Object* node);
  void EvalConversion(Object* node);
  void EvalClosure(Object* node);
  void EvalXmlExpression(Object* node);
  void EvalFunctionMember(Object* node);
  void EvalFunctionIndex(Object* node);
  void EvalShellCommand(Object* node);
  void EvalTypeConversion(Object* node);
  void EvalOperatorDeclaration(Object* node);
  void EvalIndex(Object* node);
  void EvalListAppend(Object* node);

  //
  // Eval expression to yield lvalue
  //
  void EvalLValue(Object* node, Object*& obj, Value& key);

  //
  // Function handlers
  //
  friend class ScriptFunction;
  void EvalFunctionCall(Object* node);
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

  //
  // Create a scope
  //
  Scope* NewScope(Scope* parent = 0, Object* object = 0);

  //
  // More types
  //
  typedef std::vector<String> StringVec;
  typedef std::vector<Scope*> ScopeCache;


  //
  // Members
  //
  NamespaceScope* m_global;
  Scope*          m_scope;
  size_t          m_allocs;
  Reporter        m_reporter;
  bool            m_debugParser;
  StringVec       m_fileNames;
  ScopeCache      m_scopeCache;

  //
  // Keep track of instances
  //
  static size_t g_instanceCount;

};

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
