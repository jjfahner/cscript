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
m_reporter (reporter)
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
    node->m_props["varcount"] = 0;
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
    node->m_props["argcount"] = 0;
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
    node->m_props["argcount"] = 1;
    break;

  case function_declaration:
    AnnotateFunction(node);
    break;

  case parameter:
    node->m_props["stackpos"] = m_scopeStack.top().DeclareParameter(node->m_a1);
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
    node->m_props["varcount"] = VarCount(node->m_a1) + VarCount(node->m_a2);
    break;

  case empty_statement:
    node->m_props["varcount"] = 0;
    break;

  case include_statement:
    node->m_props["varcount"] = 0;
    break;

  case for_statement:
    m_scopeStack.push(Scope(m_reporter, node, &m_scopeStack.top()));
    AnnotateImpl(node->m_a1);
    AnnotateImpl(node->m_a2);
    AnnotateImpl(node->m_a3);
    m_scopeStack.push(Scope(m_reporter, node, &m_scopeStack.top()));
    AnnotateImpl(node->m_a4);
    m_scopeStack.pop();
    m_scopeStack.pop();
    node->m_props["varcount"] = VarCount(node->m_a1) + VarCount(node->m_a4);
    break;

  case foreach_statement:
    break;

  case if_statement:
    AnnotateImpl(node->m_a1);
    m_scopeStack.push(Scope(m_reporter, node, &m_scopeStack.top()));
    AnnotateImpl(node->m_a2);
    if(node->m_a3)
    {
      AnnotateImpl(node->m_a3);
    }
    m_scopeStack.pop();
    node->m_props["varcount"] = VarCount(node->m_a2);
    break;

  case while_statement:
    AnnotateImpl(node->m_a1);
    m_scopeStack.push(Scope(m_reporter, node, &m_scopeStack.top()));
    AnnotateImpl(node->m_a2);
    m_scopeStack.pop();
    node->m_props["varcount"] = VarCount(node->m_a2);
    break;

  case return_statement:
    node->m_props["varcount"] = 0;
    if(node->m_a1)
    {
      AnnotateImpl(node->m_a1);
    }
    break;

  case compound_statement:
    m_scopeStack.push(Scope(m_reporter, node, &m_scopeStack.top()));
    AnnotateImpl(node->m_a1);
    m_scopeStack.pop();
    node->m_props["varcount"] = VarCount(node->m_a1);
    break;

  case switch_statement:
    {
      node->m_props["varcount"] = 0;
      AnnotateImpl(node->m_a1);
      AstList* list = node->m_a2;
      AstList::iterator it, ie;
      it = list->begin();
      ie = list->end();
      for(; it != ie; ++it)
      {
        AnnotateImpl(*it);
      }
    }
    break;

  case switch_case:
    AnnotateImpl(node->m_a2);
    break;

  case default_case:
    AnnotateImpl(node->m_a1);
    break;

  case struct_declaration:
    m_structs[node->m_a1] = node;
    break;
  }
}

void 
Annotator::AnnotateTranslationUnit(Ast* node)
{
  // Push initial frame
  m_scopeStack.push(Scope(m_reporter, node, 0));

  // Initialize properties
  node->m_props["framesize"] = 0;
  node->m_props["varcount"]  = 0;

  // Annotate contents
  AnnotateImpl(node->m_a1);
}

void 
Annotator::AnnotateFunction(Ast* node)
{
  // Check whether the name is in use
  String name = node->m_a1;
  if(m_functions.count(name))
  {
    m_reporter.ReportError(node->m_pos, "Function '" + name + "' is already defined");
  }

  // Initialize annotations
  node->m_props["framesize"] = 0;
  node->m_props["varcount"]  = 0;
  node->m_props["parcount"]  = 0;

  // Create new scope for function
  m_scopeStack.push(Scope(m_reporter, node, &m_scopeStack.top()));

  // Annotate parameter list
  if(node->m_a2)
  {
    AnnotateImpl(node->m_a2);
  }

  // Annotate function content
  AnnotateImpl(node->m_a3);

  // Clean up the stack
  m_scopeStack.pop();

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

  // Initialize annotations
  node->m_props["varcount"] = 0;

  // Enumerate statements
  AstList::iterator si, se;
  si = list->begin();
  se = list->end();
  for(; si != se; ++si)
  {
    // AnnotateImpl statement
    AnnotateImpl(*si);

    // Add to count
    node->m_props["varcount"] += VarCount(*si);
  }    
}

void 
Annotator::AnnotateLValue(Ast* node)
{
  // Find lvalue on stack
  bool global = false;
  int  offset = 0;
  if(!m_scopeStack.top().Lookup(node->m_a1, offset, global))
  {
    m_reporter.ReportError(node->m_pos, "undefined variable '" + node->m_a1.GetString() + "'");
  }

  // Store offset
  node->m_props["stackpos"] = offset;
  node->m_props["isglobal"] = global;
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
        m_reporter.ReportError((*it)->m_pos, "Invalid number of arguments in call to function '" + name + "'");
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
        m_reporter.ReportError((*it)->m_pos, "Invalid number of arguments in call to function '" + name + "'");
      }

      // Point call to function
      (*it)->m_props["offset"] = nci->m_offset;

      // Next
      continue;
    }

    // No such function
    m_reporter.ReportError((*it)->m_pos, "Function '" + name + "' not found");
    continue;
  }
}

void 
Annotator::AnnotateVariableDeclaration(Ast* node)
{
  // Struct
  Ast* type = 0;

  // Find declared type
  if(node->m_a3)
  {
    AstMap::iterator it = m_structs.find(node->m_a3);
    if(it == m_structs.end())
    {
      m_reporter.ReportError(node->m_pos, "undeclared type '" + node->m_a2.GetString() + "'");
    }
    else
    {
      type = it->second;
    }
  }

  // Init expression
  if(node->m_a2)
  {
    // TODO Not valid for types unless array
    if(type)
    {
      m_reporter.ReportError(node->m_pos, "invalid initializer for variable '" + 
        node->m_a1.GetString() + "' of type '" + node->m_a3.GetString() + "'");
    }

    // Annotate init expresion *before* declaring the variable,
    // to make sure that the init expresion uses the previously
    // declared variable when initializing a shadowing variable.
    AnnotateImpl(node->m_a2);
  }

  // Allocate slot
  node->m_props["varcount"] = 1;
  node->m_props["stackpos"] = m_scopeStack.top().DeclareVariable(node->m_a1);
}
