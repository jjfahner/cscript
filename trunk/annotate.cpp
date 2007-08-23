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
#include "annotate.h"
#include "report.h"
#include "native.h"
#include "error.h"
#include "scope.h"

//////////////////////////////////////////////////////////////////////////
//
// Helpers
//

inline Quad VarCount(Ast* ast)
{
  return ast->m_props["varcount"];
}

inline Quad ParCount(Ast* ast)
{
  return ast->m_props["parcount"];
}

inline Quad ArgCount(Ast* ast)
{
  return ast->m_props["argcount"];
}

//////////////////////////////////////////////////////////////////////////
//
// Implementation
//

Annotator::Annotator(Reporter& reporter) :
m_reporter  (reporter),
m_scope     (0)
{
}

void
Annotator::Annotate(Ast* node)
{
  // Annotate tree
  AnnotateImpl(node);

  // Resolve and check function calls
  ResolveCalls();  
}

void
Annotator::PushScope(Ast* node)
{
  // Check current scope
  if(m_scopeStack.top() != m_scope)
  {
    throw std::runtime_error("Invalid scope on stack");
  }

  // Create new scope
  Scope* scope = new Scope(node, m_scope);

  // Push it on the stack
  m_scopeStack.push(scope);

  // Set as current scope
  m_scope = scope;
}

Scope*
Annotator::PopScope(bool deleteScope)
{
  // Retrieve scope from stack
  Scope* scope = m_scopeStack.top();
  if(scope != m_scope)
  {
    throw std::runtime_error("Invalid scope on stack");
  }

  // Remove from stack
  m_scopeStack.pop();
  m_scope = m_scopeStack.top();

  // Delete if specified
  if(deleteScope)
  {
    delete scope;
    scope = 0;
  }

  // Return scope (or null)
  return scope;
}

void
Annotator::AnnotateImpl(Ast* node)
{
  switch(node->m_type)
  {
  case translation_unit:
    AnnotateTranslationUnit(node);
    break;

  case statement_sequence:
    AnnotateStatementSequence(node);
    break;

  case expression_statement:
    node->m_props["varcount"] = Quad(0);
    AnnotateImpl(node->m_a1);
    break;

  case assignment_expression:
    AnnotateImpl(node->m_a2);
    AnnotateImpl(node->m_a3);
    break;

  case binary_expression:
    AnnotateImpl(node->m_a2);
    AnnotateImpl(node->m_a3);
    break;

  case ternary_expression:
    AnnotateImpl(node->m_a1);
    AnnotateImpl(node->m_a2);
    AnnotateImpl(node->m_a3);
    break;

  case prefix_expression:
    AnnotateImpl(node->m_a2);
    break;

  case postfix_expression:
    AnnotateImpl(node->m_a2);
    break;

  case member_expression:
    AnnotateMemberExpression(node);
    break;

  case index_expression:
    AnnotateImpl(node->m_a1);
    AnnotateImpl(node->m_a2);
    break;

  case literal:
    break;

  case lvalue:
    AnnotateLValue(node);
    break;

  case list_literal:
    AnnotateImpl(node->m_a1);
    break;
    
  case list_content:
    AnnotateImpl(node->m_a1);
    if(node->m_a2)
    {
      AnnotateImpl(node->m_a2);
    }
    break;
    
  case list_entry:
    AnnotateImpl(node->m_a1);
    break;

  case function_call:
    node->m_props["argcount"] = Quad(0);
    if(node->m_a2)
    {
      AnnotateImpl(node->m_a2);
      node->m_props["argcount"] = ArgCount(node->m_a2);
    }
    m_funcalls.push_back(node);
    break;

  case argument_list:
    AnnotateImpl(node->m_a1);
    AnnotateImpl(node->m_a2);
    node->m_props["argcount"] = ArgCount(node->m_a1) + ArgCount(node->m_a2);
    break;

  case argument:
    AnnotateImpl(node->m_a1);
    node->m_props["argcount"] = Quad(1);
    break;

  case function_declaration:
    AnnotateFunctionDeclaration(node);
    break;

  case parameter:
    node->m_props["varinfo"] = m_scope->DeclareParameter(node->m_a1);
    break;

  case parameter_list:
    AnnotateImpl(node->m_a1);
    AnnotateImpl(node->m_a2);
    break;

  case variable_declaration:
    AnnotateVariableDeclaration(node);
    break;

  case declaration_sequence:
    node->m_a1->m_a3 = node->m_a3;
    node->m_a2->m_a3 = node->m_a3;
    AnnotateImpl(node->m_a1);
    AnnotateImpl(node->m_a2);
    node->m_props["varcount"] = 
         VarCount(node->m_a1) + 
         VarCount(node->m_a2) ;
    break;

  case empty_statement:
    node->m_props["varcount"] = Quad(0);
    break;

  case include_statement:
    node->m_props["varcount"] = Quad(0);
    break;

  case for_statement:
    node->m_props["varcount"] = 0;
    node->m_props["break"]    = AstList();
    node->m_props["continue"] = AstList();
    PushScope(node);
    AnnotateImpl(node->m_a1);
    AnnotateImpl(node->m_a2);
    AnnotateImpl(node->m_a3);
    PushScope(node);
    AnnotateImpl(node->m_a4);
    PopScope();
    PopScope();
    node->m_props["varcount"] = 
         VarCount(node->m_a1) + 
         VarCount(node->m_a4) ;
    break;

  case foreach_statement:
    node->m_props["varcount"] = 0;
    node->m_props["break"]    = AstList();
    node->m_props["continue"] = AstList();
    PushScope(node);
    AnnotateImpl(node->m_a1);
    AnnotateImpl(node->m_a2);
    PushScope(node);
    AnnotateImpl(node->m_a3);
    PopScope();
    PopScope();
    node->m_props["varcount"] = 
         VarCount(node->m_a1) + 
         VarCount(node->m_a3) ;
    break;

  case if_statement:
    node->m_props["varcount"] = 0;
    AnnotateImpl(node->m_a1);
    PushScope(node);
    AnnotateImpl(node->m_a2);
    if(node->m_a3)
    {
      AnnotateImpl(node->m_a3);
    }
    PopScope();
    node->m_props["varcount"] = VarCount(node->m_a2);
    break;

  case while_statement:
    node->m_props["varcount"] = 0;
    node->m_props["break"]    = AstList();
    node->m_props["continue"] = AstList();
    AnnotateImpl(node->m_a1);
    PushScope(node);
    AnnotateImpl(node->m_a2);
    PopScope();
    node->m_props["varcount"] = VarCount(node->m_a2);
    break;

  case return_statement:
    node->m_props["varcount"] = Quad(0);
    if(node->m_a1)
    {
      AnnotateImpl(node->m_a1);
    }
    break;

  case compound_statement:
    PushScope(node);
    AnnotateImpl(node->m_a1);
    PopScope();
    node->m_props["varcount"] = VarCount(node->m_a1);
    break;

  case switch_statement:
    {
      node->m_props["varcount"] = Quad(0);
      node->m_props["break"]    = AstList();
      AnnotateImpl(node->m_a1);
      AstList* list = node->m_a2;
      AstList::iterator it, ie;
      it = list->begin();
      ie = list->end();
      PushScope(node);
      for(; it != ie; ++it)
      {
        AnnotateImpl(*it);
      }
    }
    break;

  case switch_case:
    AnnotateSwitchCase(node);
    break;

  case default_case:
    AnnotateImpl(node->m_a1);
    break;

  case break_statement:
    AnnotateBreakStatement(node);
    break;

  case continue_statement:
    AnnotateContinueStatement(node);
    break;

  case struct_declaration:
    AnnotateStructDeclaration(node);
    break;

  case struct_members:
    AnnotateImpl(node->m_a1);
    AnnotateImpl(node->m_a2);
    break;

  case new_expression:
    AnnotateNewExpression(node);
    break;

  case class_declaration:
    AnnotateClassDeclaration(node);
    break;

  }
}

void 
Annotator::AnnotateTranslationUnit(Ast* node)
{
  // Initialize scope stack
  m_scopeStack.push(0);

  // Push initial frame
  PushScope(node);

  // Initialize properties
  node->m_props["framesize"] = Quad(0);
  node->m_props["varcount"]  = Quad(0);

  // Annotate contents
  AnnotateImpl(node->m_a1);
}

void 
Annotator::AnnotateFunctionDeclaration(Ast* node)
{
  // Check whether the name is in use
  String name = node->m_a1;
  if(m_functions.count(name))
  {
    m_reporter.ReportError(E0002, &node->m_pos, name.c_str());
  }

  // Initialize annotations
  node->m_props["framesize"] = Quad(0);
  node->m_props["varcount"]  = Quad(0);
  node->m_props["parcount"]  = Quad(0);

  // Create new scope for function
  PushScope(node);

  // Annotate parameter list
  if(node->m_a2)
  {
    AnnotateImpl(node->m_a2);
  }

  // Annotate function content
  AnnotateImpl(node->m_a3);

  // Clean up the stack
  PopScope();

  // Store and check number of variables
  node->m_props["varcount"] = VarCount(node->m_a3);
  if(node->m_props["varcount"] != node->m_props["framesize"])
  {
    throw std::logic_error("Invalid frame size");
  }
  
  // Store the function call
  m_functions[name] = node;
}

void
Annotator::AnnotateStatementSequence(Ast* node)
{
  // Determine list
  AstList* list = node->m_a1;

  // Initialize varcount
  Quad varcount = 0;

  // Enumerate statements
  AstList::iterator si, se;
  si = list->begin();
  se = list->end();
  for(; si != se; ++si)
  {
    // AnnotateImpl statement
    AnnotateImpl(*si);

    // Add to count
    varcount += VarCount(*si);
  }    

  // Store varcount
  node->m_props["varcount"] = varcount;
}

void 
Annotator::AnnotateLValue(Ast* node)
{
  // Find lvalue on stack
  VarInfo vi;
  if(!m_scope->Lookup(node->m_a1, vi))
  {
    m_reporter.ReportError(E0003, &node->m_pos, node->m_a1.GetString().c_str());
  }

  // Store offset
  node->m_props["varcount"] = (Quad)0;
  node->m_props["varinfo"]  = vi;
}

void 
Annotator::ResolveCalls()
{
  // Enumerate calls
  AstList::iterator it = m_funcalls.begin();
  AstList::iterator ie = m_funcalls.end();
  for(; it != ie; ++it)
  {
    // Function name
    String name = (*it)->m_a1;

    // Locate user function
    AstMap::iterator decl = m_functions.find(name);
    if(decl != m_functions.end())
    {
      // Check parameter count
      if((*it)->m_props["argcount"] != decl->second->m_props["parcount"])
      {
        m_reporter.ReportError(E0004, &(*it)->m_pos, name.c_str());
      }

      // Point call to function
      (*it)->m_a3 = decl->second;

      // Next
      continue;
    }

    // Locate native call
    NativeCallInfo* nci = FindNative(name);
    if(nci != 0)
    {
      // Check parameter count
      if(ArgCount(*it) < nci->m_minPar ||
         ArgCount(*it) > nci->m_maxPar )
      {
        m_reporter.ReportError(E0004, &(*it)->m_pos, name.c_str());
      }

      // Point call to function
      (*it)->m_props["offset"] = nci->m_offset;

      // Next
      continue;
    }

    // No such function
    m_reporter.ReportError(E0005, &(*it)->m_pos, name.c_str());
    continue;
  }
}

void 
Annotator::AnnotateVariableDeclaration(Ast* node)
{
  // Init expression
  if(node->m_a2)
  {
    // Annotate init expresion *before* declaring the variable,
    // to make sure that the init expresion uses the previously
    // declared variable when initializing a shadowing variable.
    AnnotateImpl(node->m_a2);
  }

  // TODO Check for locally declared variable with same name

  // Allocate slot
  node->m_props["varcount"] = Quad(1);
  node->m_props["varinfo"] = m_scope->DeclareVariable(node->m_a1);
}

void 
Annotator::AnnotateStructDeclaration(Ast* node)
{
  // Check whether name is in use
  if(m_structs.count(node->m_a1))
  {
    m_reporter.ReportError(E0008, &node->m_pos, 
              node->m_a1.GetString().c_str());
  }

  // Store struct node
  m_structs[node->m_a1] = node;

  // Create new scope
  PushScope(node);

  // Handle member declarations
  if(node->m_a2)
  {
    AnnotateImpl(node->m_a2);
  }

  // TODO Store scope with node
  Scope* structScope = PopScope(false);
  delete structScope;
}

void 
Annotator::AnnotateNewExpression(Ast* node)
{
  // Check whether the type exists
  if(m_classes.count(node->m_a1) == 0)
  {
    m_reporter.ReportError(E0006, &node->m_pos, 
              node->m_a1.GetString().c_str());    
  }
}

void 
Annotator::AnnotateMemberExpression(Ast* node)
{
  // Create literal for right-hand side
  Ast* lit = new Ast(literal, Variant(
    node->m_a2.GetString(), Variant::stString));

  // Replace right-hand side with literal
  node->m_a2 = lit;

  // Change type to index expression
  node->m_type = index_expression;

  // Annotate as index expression
  AnnotateImpl(node);
}

void 
Annotator::AnnotateBreakStatement(Ast* node)
{
  Scope* scope = m_scope;

  // Find valid target scope for break
  while(scope)
  {
    // Function declaration is scope boundary
    if(scope->GetNode()->m_type == function_declaration)
    {
      scope = 0;
      break;
    }

    // Breakable statement
    if(scope->GetNode()->m_props.contains("break"))
    {
      break;
    }

    // Next scope
    scope = scope->GetParent();
  }

  // No valid scope found
  if(scope == 0)
  {
    m_reporter.ReportError(E0014, &node->m_pos);
  }

  // Link break statement to node
  node->m_props["scope"] = scope->GetNode();

  // Add break statement to scope
  any_cast<AstList>(scope->GetNode()->m_props["break"]).push_back(node);

  // Store varcount
  node->m_props["varcount"] = Quad(0);
}

void 
Annotator::AnnotateContinueStatement(Ast* node)
{
  Scope* scope = m_scope;

  // Find valid target scope for continue
  while(scope)
  {
    // Function declaration is scope boundary
    if(scope->GetNode()->m_type == function_declaration)
    {
      scope = 0;
      break;
    }

    // Continuable statement
    if(scope->GetNode()->m_props.contains("continue"))
    {
      break;
    }

    // Next scope
    scope = scope->GetParent();
  }

  // No valid scope found
  if(scope == 0)
  {
    m_reporter.ReportError(E0015, &node->m_pos);
  }

  // Link break statement to node
  node->m_props["scope"] = scope->GetNode();

  // Add break statement to scope
  any_cast<AstList>(scope->GetNode()->m_props["continue"]).push_back(node);

  // Store varcount
  node->m_props["varcount"] = Quad(0);
}

void 
Annotator::AnnotateSwitchCase(Ast* node)
{
  // Append a break statement
  AstList* list = new AstList;
  list->push_back(node->m_a2);
  list->push_back(new Ast(break_statement));
  node->m_a2 = new Ast(statement_sequence, list);

  // Annotate contents
  AnnotateImpl(node->m_a2);
}

void
Annotator::AnnotateClassDeclaration(Ast* node)
{
  AstList::iterator it, ie;

  // Init var count
  node->m_props["varcount"] = (Quad)0;
  
  // TODO Check class name
  m_classes[node->m_a1] = node;

  // Retrieve member list
  AstList* list = node->m_a2;

  // Create class scope
  PushScope(node);

  // Enumerate members to handle variables
  it = list->begin();
  ie = list->end();
  for(; it != ie; ++it)
  {
    Ast* node = *it;
    if(node->m_type == variable_declaration)
    {
      AnnotateVariableDeclaration(node);
    }
  }

  // Enumerate functions
  it = list->begin();
  ie = list->end();
  for(; it != ie; ++it)
  {
    Ast* node = *it;
    if(node->m_type == function_declaration)
    {
      AnnotateFunctionDeclaration(node);
    }
  }

  // Pop class scope
  PopScope();
}
