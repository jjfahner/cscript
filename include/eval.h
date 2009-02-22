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
  void Collect();

  //////////////////////////////////////////////////////////////////////////
  //
  // Public to allow access from parser
  //

  //
  // Evaluation of ast tree
  //
  void Eval(Object* astRoot);

  //
  // Set native call
  //
  void SetNativeCall(Object* node) {
    m_native = node;
  }

  //
  // Retrieve lexer
  //
  Lexer* GetLexer() const {
    return m_lexer;
  }

  //
  // Parse native call
  //
  Object* ParseNativeCall(String const& declaration);

  //
  // Allocate node
  //
  Object* AllocNode(AstTypes type);
  Object* AllocNode(AstTypes type, Value const& a1);
  Object* AllocNode(AstTypes type, Value const& a1, Value const& a2);
  Object* AllocNode(AstTypes type, Value const& a1, Value const& a2, Value const& a3);
  Object* AllocNode(AstTypes type, Value const& a1, Value const& a2, Value const& a3, Value const& a4);

  //
  // Error handlers
  //
  void OnParseFailure();
  void OnSyntaxError();

private:

  //
  // Parse a string
  //
  void ParseText(char const* text);

  //
  // Parse a file
  //
  void ParseFile(String const& filename);

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
  RValue& EvalShellCommand(Object* node);
  RValue& EvalTypeConversion(Object* node);
  RValue& EvalOperatorDeclaration(Object* node);

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
  void ConvertInPlace(Object* node, Value& value, Value::Types newType);

  //
  // Convert a value
  //
  Value Convert(Object* node, Value const& value, Value::Types newType);

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

  typedef std::vector<RValue*> TempVec;

  //
  // Members
  //
  SourceFile*     m_file;
  Lexer*          m_lexer;
  Object*         m_native;
  NamespaceScope* m_global;
  Scope*          m_scope;
  size_t          m_allocs;
  TempVec         m_temporaries;
  Reporter        m_reporter;


};

//////////////////////////////////////////////////////////////////////////
//
// Node allocation
//

inline Object* 
Evaluator::AllocNode(AstTypes type, Value const& a1)
{
  Object* obj = AllocNode(type);
  (*obj)["a1"] = a1;
  return obj;
}

inline Object* 
Evaluator::AllocNode(AstTypes type, Value const& a1, Value const& a2)
{
  Object* obj = AllocNode(type);
  (*obj)["a1"] = a1;
  (*obj)["a2"] = a2;
  return obj;
}

inline Object* 
Evaluator::AllocNode(AstTypes type, Value const& a1, Value const& a2, Value const& a3)
{
  Object* obj = AllocNode(type);
  (*obj)["a1"] = a1;
  (*obj)["a2"] = a2;
  (*obj)["a3"] = a3;
  return obj;
}

inline Object* 
Evaluator::AllocNode(AstTypes type, Value const& a1, Value const& a2, Value const& a3, Value const& a4)
{
  Object* obj = AllocNode(type);
  (*obj)["a1"] = a1;
  (*obj)["a2"] = a2;
  (*obj)["a3"] = a3;
  (*obj)["a4"] = a4;
  return obj;
}

inline RValue& 
Evaluator::MakeTemp(Value const& value)
{
  RValue* temp = new ROVariable(value);
  return StoreTemp(temp);
}

inline RValue& 
Evaluator::StoreTemp(RValue* rval)
{
  m_temporaries.push_back(rval);
  return *rval;
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
  ReturnException(Object* node, Value const& value) : ScriptException (node), m_value (value) {}
  ~ReturnException() throw() {}
};

struct CatchableException : public ScriptException
{
  Value m_value;
  CatchableException(Object* node) : ScriptException (node) {}
  CatchableException(Object* node, Value const& value) : ScriptException (node), m_value (value) {}
  ~CatchableException() throw() {}
};

struct UserException : public CatchableException
{
  UserException(Object* node) : CatchableException (node) {}
  UserException(Object* node, Value const& value) : CatchableException (node, value) {}
  ~UserException() throw() {}
};

#endif // CSCRIPT_EVAL_H
