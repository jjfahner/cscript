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
    break;

  case binary_expression:
    break;

  case ternary_expression:
    break;

  case prefix_expression:
    break;

  case postfix_expression:
    break;

  case member_expression:
    break;

  case index_expression:
    break;

  case function_call:
    break;

  case literal:
    break;

  case lvalue:
    break;

  case list_literal:
    break;

  case argument_list:
    break;

  case function_declaration:
    break;

  case parameter:
    break;

  case parameter_list:
    break;

  case variable_declaration:
    node->m_varcount = 1;
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
    Annotate(node->m_a2);
    node->m_varcount = VarCount(node->m_a2);
    break;

  case while_statement:
    Annotate(node->m_a2);
    node->m_varcount = VarCount(node->m_a2);
    break;

  case return_statement:
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
