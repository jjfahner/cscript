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
#include "optimize.h"
#include "ast.h"
#include "astlist.h"

//////////////////////////////////////////////////////////////////////////
//
// Helpers
//

inline bool IsType(Ast* node, AstTypes type)
{
  return node->m_type == type;
}

inline bool IsIdempotent(Ast* node)
{
  return false;
  //return node->m_props["idempotent"];
}

//////////////////////////////////////////////////////////////////////////
//
// Top-level recursive optimizer
//

Ast*
Optimizer::Optimize(Ast* node)
{
  switch(node->m_type)
  {
  case translation_unit:
    node = Optimize(node->m_a1);
    break;

  case statement_sequence:
    node = OptimizeStatementSequence(node);
    break;

  case expression_statement:
    node = OptimizeExpressionStatement(node);
    break;

  case assignment_expression:
    node = OptimizeAssignmentExpression(node);
    break;

  case binary_expression:
    node = OptimizeBinaryExpression(node);
    break;

  case ternary_expression:
    node = OptimizeTernaryExpression(node);
    break;

  case prefix_expression:
    node = OptimizePrefixExpression(node);
    break;

  case postfix_expression:
    node->m_a2 = Optimize(node->m_a2);
    break;

  case member_expression:
    break;

  case index_expression:
    node->m_a1 = Optimize(node->m_a1);
    node->m_a2 = Optimize(node->m_a2);
    break;

  case function_call:
    if(node->m_a2)
    {
      node->m_a2 = Optimize(node->m_a2);
    }
    break;

  case argument_list:
    node->m_a1 = Optimize(node->m_a1);
    node->m_a2 = Optimize(node->m_a2);
    break;

  case argument:
    node->m_a1 = Optimize(node->m_a1);
    break;

  case function_declaration:
    node->m_a3 = Optimize(node->m_a3);
    break;

  case list_literal:
    node->m_a1 = Optimize(node->m_a1);
    break;
    
  case list_content:
    node->m_a1 = Optimize(node->m_a1);
    if(node->m_a2)
    {
      node->m_a2 = Optimize(node->m_a2);
    }
    break;
    
  case list_entry:
    node->m_a1 = Optimize(node->m_a1);
    break;

  case parameter:
  case parameter_list:
    break;

  case literal:
  case lvalue:
    node->m_props["idempotent"] = true;
    break;

  case variable_declaration:
    node = OptimizeVariableDeclaration(node);
    break;

  case declaration_sequence:
    node->m_a1 = Optimize(node->m_a1);
    node->m_a2 = Optimize(node->m_a2);
    break;

  case include_statement:
    break;

  case for_statement:
    node = OptimizeForStatement(node);
    break;

  case foreach_statement:
    node = OptimizeForeachStatement(node);
    break;

  case if_statement:
    node = OptimizeIfStatement(node);
    break;

  case while_statement:
    node->m_a1 = Optimize(node->m_a1);
    node->m_a2 = Optimize(node->m_a2);
    break;

  case return_statement:
    if(node->m_a1)
    {
      node->m_a1 = Optimize(node->m_a1);
    }
    break;

  case compound_statement:
    node = OptimizeCompoundStatement(node);
    break;

  case switch_statement:
    node = OptimizeSwitchStatement(node);
    break;

  case empty_statement:
    break;

  case struct_declaration:
    node = OptimizeStructDeclaration(node);
    break;
  
  case new_expression:
    break;

  case break_statement:
    break;

  case continue_statement:
    break;

  case class_declaration:
    node = OptimizeClassDeclaration(node);
    break;

  case member_call:
    break;

  case pause_statement:
    break;

  case this_expression:
    break;

  default:
    throw std::runtime_error("Unknown node type");
  }

  return node;
}

//////////////////////////////////////////////////////////////////////////
//
// Optimizer methods
//

Ast* 
Optimizer::OptimizeIfStatement(Ast* node)
{
  // Optimize condition expression
  node->m_a1 = Optimize(node->m_a1);
  
  // If the condition is constant, reduce to the corresponding branch
  if(IsType(node->m_a1, literal))
  {
    // Use true branch
    if(node->m_a1->m_a1.GetValue())
    {
      Ast* res = Optimize(node->m_a2);
      return res;
    }

    // Use false branch
    if(node->m_a3)
    {
      // Use 'false' branch
      Ast* res = Optimize(node->m_a3);
      return res;
    }

    // Optimize to null statement
    return new Ast(empty_statement);
  }

  // Optimize true branch
  node->m_a2 = Optimize(node->m_a2);

  // Emptyness
  bool le = IsType(node->m_a2, empty_statement);
  bool re = true;

  // Optimize false branch
  if(node->m_a3)
  {
    node->m_a3 = Optimize(node->m_a3);
    re = IsType(node->m_a3, empty_statement);
  }

//   // Empty substatements
//   if(le && re)
//   {
//     return 
//   }

  // Return original node
  return node;
}

Ast* 
Optimizer::OptimizeForStatement(Ast* node)
{
  // Optimize subexpressions
  node->m_a1 = Optimize(node->m_a1);
  node->m_a2 = Optimize(node->m_a2);
  node->m_a3 = Optimize(node->m_a3);
  node->m_a4 = Optimize(node->m_a4);
  return node;
}

Ast* 
Optimizer::OptimizeForeachStatement(Ast* node)
{
  // Optimize subexpressions
  node->m_a2 = Optimize(node->m_a2);
  node->m_a3 = Optimize(node->m_a3);
  return node;
}

Ast* 
Optimizer::OptimizeAssignmentExpression(Ast* node)
{
  // Optimize left side
  node->m_a2 = Optimize(node->m_a2);

  // Optimize right side
  node->m_a3 = Optimize(node->m_a3);

  // Done
  return node;
}

Ast* 
Optimizer::OptimizeBinaryExpression(Ast* node)
{
  // Optimize left-hand side
  node->m_a2 = Optimize(node->m_a2);

  // Or with literal true on left side
  if(node->m_a1.GetNumber() == op_logor && 
     IsType(node->m_a2, literal) && 
     node->m_a2->m_a1.GetValue().AsBool() )
  {
    node = new Ast(literal, Variant(true));
    node->m_props["idempotent"] = true;
    return node;
  }

  // And with literal false on left side
  if(node->m_a1.GetNumber() == op_logand &&
     IsType(node->m_a2, literal) &&
     !node->m_a2->m_a1.GetValue().AsBool() )
  {
    node = new Ast(literal, Variant(false));
    node->m_props["idempotent"] = true;
    return node;
  }

  // Optimize right side
  node->m_a3 = Optimize(node->m_a3);

  // Determine idempotence
  node->m_props["idempotent"] = 
    IsIdempotent(node->m_a2) && 
    IsIdempotent(node->m_a3) ;

  // Check whether both sides are literals
  if(!IsType(node->m_a2, literal) || 
     !IsType(node->m_a3, literal) )
  {
    // Nothing to do here
    return node;
  }

  // Extract values
  Variant lhs = node->m_a2->m_a1;
  Variant rhs = node->m_a3->m_a1;

  // Calculate new value
  Ast* rep = 0;
  switch(node->m_a1.GetNumber())
  {
  case op_add:    rep = new Ast(literal, lhs +  rhs); break;
  case op_sub:    rep = new Ast(literal, lhs -  rhs); break;
  case op_mul:    rep = new Ast(literal, lhs *  rhs); break;
  case op_div:    rep = new Ast(literal, lhs /  rhs); break;
  case op_mod:    rep = new Ast(literal, lhs %  rhs); break;
  case op_logor:  rep = new Ast(literal, lhs || rhs); break;
  case op_logand: rep = new Ast(literal, lhs && rhs); break;
  case op_eq:     rep = new Ast(literal, lhs == rhs); break;
  case op_ne:     rep = new Ast(literal, lhs != rhs); break;
  case op_lt:     rep = new Ast(literal, lhs <  rhs); break;
  case op_le:     rep = new Ast(literal, lhs <= rhs); break;
  case op_gt:     rep = new Ast(literal, lhs >  rhs); break;
  case op_ge:     rep = new Ast(literal, lhs >= rhs); break;
  default:        std::cout << "Cannot Optimize unknown binary operator\n"; break;
  }

  // Replace previous node
  if(rep)
  {
    rep->m_props["idempotent"] = true;
    node = rep;
  }
  
  // Return (replaced) node
  return node;
}

Ast* 
Optimizer::OptimizeTernaryExpression(Ast* node)
{
  // Optimize nodes
  node->m_a1 = Optimize(node->m_a1);
  node->m_a2 = Optimize(node->m_a2);
  node->m_a3 = Optimize(node->m_a3);

  // Optimize for literal condition
  if(IsType(node->m_a1, literal))
  {
    // Decide which branch to pick
    if(node->m_a1->m_a1.GetValue())
    {
      Ast* res = node->m_a2;
      node = res;
    }
    else
    {
      Ast* res = node->m_a3;
      node = res;
    }
  }
  else
  {
    // Determine idempotence
    node->m_props["idempotent"] = 
      IsIdempotent(node->m_a1) &&
      IsIdempotent(node->m_a2) &&
      IsIdempotent(node->m_a3) ;
  }

  // Succeeded
  return node;
}


Ast* 
Optimizer::OptimizeStatementSequence(Ast* node)
{
  // Take over old list
  AstList* old = node->m_a1;

  // Create new list
  AstList* rep = new AstList;

  // Check for idempotence
  bool idempotent = false;

  // Enumerate statements
  AstList::iterator si, se;
  si = old->begin();
  se = old->end();
  for(; si != se; ++si)
  {
    // Optimize the statement
    Ast* opt = Optimize(*si);

    // Add to list
    if(!IsType(opt, empty_statement))
    {
      rep->push_back(opt);
      //idempotent &= (bool)opt->m_props["idempotent"];
    }
  }

  // The new list is empty
  if(rep->size() == 0)
  {
    delete rep;
    node = new Ast(empty_statement);
    node->m_props["idempotent"] = true;
    return node;
  }

  // The new list contains one statement
  if(rep->size() == 1)
  {
    node = *rep->begin();
    delete rep;
    return node;
  }

  // Replace old list
  node->m_a1 = rep;
  node->m_props["idempotent"] = idempotent;
  return node;
}

Ast*
Optimizer::OptimizeExpressionStatement(Ast* node)
{
  // Optimize expression
  node->m_a1 = Optimize(node->m_a1);

  // Replace idempotent expression with empty statement
  if(IsIdempotent(node->m_a1))
  {
    node = new Ast(empty_statement);
    node->m_props["idempotent"] = true;
  }

  // Done
  return node;
}

Ast* 
Optimizer::OptimizeCompoundStatement(Ast* node)
{
  // If empty, return empty statement
  if(!node->m_a1)
  {
    node = new Ast(empty_statement);
    node->m_props["idempotent"] = true;
    return node;
  }

  // Optimize content
  if(node->m_a1)
  {
    node->m_a1 = Optimize(node->m_a1);
  }

  // If empty, Optimize further
  if(IsType(node->m_a1, empty_statement))
  {
    Ast* res = node->m_a1;
    node = res;
  }

  // Done
  return node;
}

Ast* 
Optimizer::OptimizePrefixExpression(Ast* node)
{
  // Optimize subexpression
  node->m_a2 = Optimize(node->m_a2);

  // Reduce prefix expression on literal
  if(IsType(node->m_a2, literal))
  {
    switch(node->m_a1.GetNumber())
    {
    case op_negate:
      node = new Ast(literal, -node->m_a2->m_a1.GetValue());
      break;
    case op_not:
      node = new Ast(literal, !node->m_a2->m_a1.GetValue());
      break;
    }
  }

  // Done
  return node;
}

Ast* 
Optimizer::OptimizeSwitchStatement(Ast* node)
{
  return node;
}

Ast* 
Optimizer::OptimizeStructDeclaration(Ast* node)
{
  node->m_props["varcount"] = Quad(0);
  return node;
}

Ast* 
Optimizer::OptimizeVariableDeclaration(Ast* node)
{
  if(node->m_a2.Type() == AstData::Node)
  {
    node->m_a2 = Optimize(node->m_a2);
  }
  return node;
}

Ast* 
Optimizer::OptimizeClassDeclaration(Ast* node)
{
  AstList* list = node->m_a2;
  AstList::iterator it = list->begin();
  AstList::iterator ie = list->end();
  for(; it != ie; ++it)
  {
    Ast* node = *it;
    if(node->m_type == function_declaration)
    {
      Optimize(node);
    }
  }
  return node;
}
