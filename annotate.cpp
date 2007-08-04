#include "codegen.h"

//////////////////////////////////////////////////////////////////////////
//
// Helpers
//

inline Quad VarCount(Ast* ast)
{
  return ast->m_varcount;
}

//////////////////////////////////////////////////////////////////////////
//
// Main annotation driver
//

void
CodeGenerator::Annotate(Ast* node)
{
  if(m_scopeStack.empty())
  {
    m_scopeStack.push(Scope(node, 0));
  }

  switch(node->m_type)
  {
  case statement_sequence:
    AnnotateStatementSequence(node);
    break;

  case expression_statement:
    Annotate(node->m_a1);
    node->m_varcount = VarCount(node->m_a1);
    break;

  case assignment_expression:
    Annotate(node->m_a2);
    Annotate(node->m_a3);
    break;

  case binary_expression:
    Annotate(node->m_a2);
    Annotate(node->m_a3);
    break;

  case ternary_expression:
    Annotate(node->m_a1);
    Annotate(node->m_a2);
    Annotate(node->m_a3);
    break;

  case prefix_expression:
    Annotate(node->m_a2);
    break;

  case postfix_expression:
    Annotate(node->m_a2);
    break;

  case member_expression:
    break;

  case index_expression:
    Annotate(node->m_a1);
    Annotate(node->m_a2);
    break;

  case function_call:
    if(!node->m_a2.empty())
    {
      Annotate(node->m_a2);
    }
    break;

  case literal:
    break;

  case lvalue:
    node->m_stackpos = m_scopeStack.top().Lookup(node->m_a1);
    break;

  case list_literal:
    break;

  case argument_list:
    Annotate(node->m_a2);
    Annotate(node->m_a1);
    break;

  case function_declaration:
    m_scopeStack.push(Scope(node, &m_scopeStack.top()));
    if(!node->m_a2.empty())
    {
      Annotate(node->m_a2);
    }
    Annotate(node->m_a3);
    node->m_varcount = VarCount(node->m_a3);
    m_scopeStack.pop();
    break;

  case parameter:
    node->m_stackpos = m_scopeStack.top().DeclareParameter(node->m_a1);
    break;

  case parameter_list:
    Annotate(node->m_a1);
    Annotate(node->m_a2);
    break;

  case variable_declaration:
    if(!node->m_a2.empty())
    {
      Annotate(node->m_a2);
    }
    node->m_varcount = 1;
    node->m_stackpos = m_scopeStack.top().DeclareVariable(node->m_a1);
    break;

  case declaration_sequence:
    Annotate(node->m_a1);
    Annotate(node->m_a2);
    node->m_varcount = 
      VarCount(node->m_a1) + 
      VarCount(node->m_a2);
    break;

  case empty_statement:
    break;

  case include_statement:
    break;

  case for_statement:
    Annotate(node->m_a1);
    Annotate(node->m_a4);
    node->m_varcount = 
      VarCount(node->m_a1) + 
      VarCount(node->m_a4);
    break;

  case foreach_statement:
    break;

  case if_statement:
    Annotate(node->m_a1);
    Annotate(node->m_a2);
    node->m_varcount = VarCount(node->m_a2);
    break;

  case while_statement:
    Annotate(node->m_a2);
    node->m_varcount = VarCount(node->m_a2);
    break;

  case return_statement:
    if(!node->m_a1.empty())
    {
      Annotate(node->m_a1);
    }
    break;

  case compound_statement:
    Annotate(node->m_a1);
    node->m_varcount = VarCount(node->m_a1);
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
    // Annotate statement
    Annotate(*si);

    // Add to count
    node->m_varcount += (*si)->m_varcount;
  }    
}
