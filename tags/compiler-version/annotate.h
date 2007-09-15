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
#ifndef CSCRIPT_ANNOTATE_H
#define CSCRIPT_ANNOTATE_H

#include "types.h"
#include "ast.h"
#include "astlist.h"

class Scope;
class Reporter;

class Annotator
{
public:

  //
  // Construction
  //
  Annotator(Reporter& reporter);

  //
  // Annotate tree
  //
  void Annotate(Ast* root);

private:

  //
  // Annotation implementation
  //
  void AnnotateImpl(Ast* node);
  void AnnotateLValue(Ast* node);
  void AnnotateTranslationUnit(Ast* node);
  void AnnotateStatementSequence(Ast* node);
  void AnnotateNewExpression(Ast* node);
  void AnnotateMemberExpression(Ast* node);
  void AnnotateBreakStatement(Ast* node);
  void AnnotateContinueStatement(Ast* node);
  void AnnotateSwitchCase(Ast* node);
  void AnnotateClassDeclaration(Ast* node);
  void AnnotateVariableDeclaration(Ast* node);
  void AnnotateFunctionCall(Ast* node);
  void AnnotateMemberCall(Ast* node);

  //
  // Annotate global structure
  //
  void AnnotateStructure(Ast* node);
  void AnnotateFunction(String fullName, Ast* node);

  //
  // Function call resolving
  //
  void ResolveCalls();

  //
  // Scope handling
  //
  void PushScope(Ast* node);
  void PopScope();

  //
  // Reporter
  //
  Reporter& m_reporter;

  //
  // Types
  //
  typedef std::map<String, Ast*> AstMap;
  typedef std::stack<Scope*> VarIdStack;

  //
  // Members
  //
  VarIdStack  m_scopeStack;
  Scope*      m_scope;
  AstList     m_calls;
  AstMap      m_functions;
  AstMap      m_structs;
  AstMap      m_classes;

};

#endif // CSCRIPT_ANNOTATE_H
