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
#include "codegen.h"

//////////////////////////////////////////////////////////////////////////
//
// Helpers
//

inline Quad VarCount(Ast* ast)
{
  return ast->m_varcount;
}

inline Quad ParCount(Ast* ast)
{
  return ast->m_parcount;
}

inline Quad ArgCount(Ast* ast)
{
  return ast->m_argcount;
}

//////////////////////////////////////////////////////////////////////////
//
// Annotation
//

void
CodeGenerator::Annotate(Ast* node)
{
  // Push initial frame
  m_scopeStack.push(Scope(*this, node, 0));

  // Annotate tree
  AnnotateImpl(node);
}

void
CodeGenerator::AnnotateImpl(Ast* node)
{
  switch(node->m_type)
  {
  case statement_sequence:
    AnnotateStatementSequence(node);
    break;

  case expression_statement:
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
    node->m_stackpos = m_scopeStack.top().Lookup(node->m_a1, node->m_globalvar);
    break;

  case list_literal:
    AnnotateImpl(node->m_a1);
    break;
    
  case list_content:
    AnnotateImpl(node->m_a1);
    if(!node->m_a2.empty())
    {
      AnnotateImpl(node->m_a2);
    }
    break;
    
  case list_entry:
    node->m_a1 = Optimize(node->m_a1);
    break;

  case function_call:
    if(!node->m_a2.empty())
    {
      AnnotateImpl(node->m_a2);
      node->m_argcount = ArgCount(node->m_a2);
    }
    break;

  case argument_list:
    AnnotateImpl(node->m_a1);
    AnnotateImpl(node->m_a2);
    node->m_argcount = 
      ArgCount(node->m_a1) +
      ArgCount(node->m_a2) ;
    break;

  case argument:
    AnnotateImpl(node->m_a1);
    node->m_argcount = 1;
    break;

  case function_declaration:
    m_scopeStack.push(Scope(*this, node, &m_scopeStack.top()));
    if(!node->m_a2.empty())
    {
      AnnotateImpl(node->m_a2);
    }
    AnnotateImpl(node->m_a3);
    node->m_varcount = VarCount(node->m_a3);
    m_scopeStack.pop();
    if(node->m_varcount != node->m_framesize)
    {
      throw std::logic_error("Invalid frame size");
    }
    break;

  case parameter:
    node->m_stackpos = m_scopeStack.top().DeclareParameter(node->m_a1);
    break;

  case parameter_list:
    AnnotateImpl(node->m_a1);
    AnnotateImpl(node->m_a2);
    break;

  case variable_declaration:
    // Annotate init expresion *before* declaring the variable,
    // to make sure that the init expresion uses the previously
    // declared variable when initializing a shadowing variable.
    if(!node->m_a2.empty())
    {
      AnnotateImpl(node->m_a2);
    }
    node->m_varcount = 1;
    node->m_stackpos = m_scopeStack.top().DeclareVariable(node->m_a1);
    break;

  case declaration_sequence:
    AnnotateImpl(node->m_a1);
    AnnotateImpl(node->m_a2);
    node->m_varcount = 
      VarCount(node->m_a1) + 
      VarCount(node->m_a2);
    break;

  case empty_statement:
    break;

  case include_statement:
    break;

  case for_statement:
    m_scopeStack.push(Scope(*this, node, &m_scopeStack.top()));
    AnnotateImpl(node->m_a1);
    AnnotateImpl(node->m_a2);
    AnnotateImpl(node->m_a3);
    m_scopeStack.push(Scope(*this, node, &m_scopeStack.top()));
    AnnotateImpl(node->m_a4);
    m_scopeStack.pop();
    m_scopeStack.pop();
    node->m_varcount = 
      VarCount(node->m_a1) + 
      VarCount(node->m_a4);
    break;

  case foreach_statement:
    break;

  case if_statement:
    AnnotateImpl(node->m_a1);
    m_scopeStack.push(Scope(*this, node, &m_scopeStack.top()));
    AnnotateImpl(node->m_a2);
    if(!node->m_a3.empty())
    {
      AnnotateImpl(node->m_a3);
    }
    m_scopeStack.pop();
    node->m_varcount = VarCount(node->m_a2);
    break;

  case while_statement:
    AnnotateImpl(node->m_a1);
    m_scopeStack.push(Scope(*this, node, &m_scopeStack.top()));
    AnnotateImpl(node->m_a2);
    m_scopeStack.pop();
    node->m_varcount = VarCount(node->m_a2);
    break;

  case return_statement:
    if(!node->m_a1.empty())
    {
      AnnotateImpl(node->m_a1);
    }
    break;

  case compound_statement:
    m_scopeStack.push(Scope(*this, node, &m_scopeStack.top()));
    AnnotateImpl(node->m_a1);
    m_scopeStack.pop();
    node->m_varcount = VarCount(node->m_a1);
    break;

  case switch_statement:
    {
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
  }
}

void
CodeGenerator::AnnotateStatementSequence(Ast* node)
{
  // Determine list
  AstList* list = any_cast<AstList*>(node->m_a1);

  // Enumerate statements
  AstList::iterator si, se;
  si = list->begin();
  se = list->end();
  for(; si != se; ++si)
  {
    // AnnotateImpl statement
    AnnotateImpl(*si);

    // Add to count
    node->m_varcount += (*si)->m_varcount;
  }    
}
