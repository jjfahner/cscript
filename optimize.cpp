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

inline Variant LiteralAsVariant(Ast* node)
{
  return any_cast<Variant>(node->m_a1);
}

inline bool LiteralAsBool(Ast* node)
{
  return any_cast<Variant>(node->m_a1).AsBool();
}

inline bool IsType(Ast* node, AstTypes type)
{
  return node->m_type == type;
}

inline bool IsIdempotent(Ast* node)
{
  return node->m_idempotent;
}

//////////////////////////////////////////////////////////////////////////
//
// Top-level recursive optimizer
//

Ast*
CodeGenerator::Optimize(Ast* node)
{
  switch(node->m_type)
  {
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
    node->m_a2 = Optimize(node->m_a2);
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
    if(!node->m_a2.empty())
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
  case parameter:
  case parameter_list:
    break;

  case literal:
  case lvalue:
    node->m_idempotent = true;
    break;

  case variable_declaration:
    if(!node->m_a2.empty())
    {
      node->m_a2 = Optimize(node->m_a2);
    }
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
    break;

  case if_statement:
    node = OptimizeIfStatement(node);
    break;

  case while_statement:
    node->m_a1 = Optimize(node->m_a1);
    node->m_a2 = Optimize(node->m_a2);
    break;

  case return_statement:
    if(!node->m_a1.empty())
    {
      node->m_a1 = Optimize(node->m_a1);
    }
    break;

  case compound_statement:
    node = OptimizeCompoundStatement(node);
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
CodeGenerator::OptimizeIfStatement(Ast* node)
{
  // Optimize condition expression
  node->m_a1 = Optimize(node->m_a1);
  
  // If the condition is constant, Optimize to corresponding branch
  if(any_cast<Ast*>(node->m_a1)->m_type == literal)
  {
    // Use if branch
    if(any_cast<Variant>(any_cast<Ast*>(node->m_a1)->m_a1).AsBool())
    {
      Ast* res = Optimize(node->m_a2);
      delete node;
      return res;
    }

    // Use else branch
    if(!node->m_a3.empty())
    {
      // Use 'false' branch
      Ast* res = Optimize(node->m_a3);
      delete node;
      return res;
    }

    // Optimize to null statement
    delete node;
    return new Ast(empty_statement);
  }

  // Optimize if branch
  node->m_a2 = Optimize(node->m_a2);

  // Emptyness
  bool le = IsType(node->m_a2, empty_statement);
  bool re = true;

  // Optimize else branch
  if(!node->m_a3.empty())
  {
    node->m_a3 = Optimize(node->m_a3);
    re = IsType(node->m_a3, empty_statement);
  }

//   // Empty substatements
//   if(le && re)
//   {
//     delete node;
//     return 
//   }

  // Return original node
  return node;
}

Ast* 
CodeGenerator::OptimizeForStatement(Ast* node)
{
  // Optimize subexpressions
  node->m_a1 = Optimize(node->m_a1);
  node->m_a2 = Optimize(node->m_a2);
  node->m_a3 = Optimize(node->m_a3);
  node->m_a4 = Optimize(node->m_a4);

  // Determine condition
  Ast* cond;
  if(node->m_a2.empty())
  {
    cond = new Ast(literal, Variant::True);
  }
  else
  {
    cond = node->m_a2;
  }

  // Create body statement
  Ast* body = 0;
  if(IsType(node->m_a4, empty_statement))
  {
    if(node->m_a3.empty())
    {
      // Use empty statement for body
      body = node->m_a4;
    }
    else
    {
      // Use post-expression for body
      body = new Ast(expression_statement, node->m_a3);
    }
  }
  else
  {
    // Put body and post-expression in list
    AstList* list = new AstList;
    list->push_back(node->m_a4);
    list->push_back(new Ast(expression_statement, node->m_a3));

    // Make statement sequence
    body = new Ast(statement_sequence, list);

    // Wrap in compound statement
    body = new Ast(compound_statement, body);
  }

  // Create while statement
  Ast* root = new Ast(while_statement, cond, body);

  // Empty init statement means we're done
  if(!IsType(node->m_a1, empty_statement))
  {
    // Create sequence for init-expression and while
    AstList* list = new AstList;
    list->push_back(node->m_a1);
    list->push_back(root);

    // Replace root
    root = new Ast(statement_sequence, list);

    // Generate compound for declarations
    if(IsType(node->m_a1, variable_declaration) || 
       IsType(node->m_a1, declaration_sequence) )
    {
      root = new Ast(compound_statement, root);
    }
  }

  // Done
  return Optimize(root);
}

Ast* 
CodeGenerator::OptimizeAssignmentExpression(Ast* node)
{
  // Optimize left side
  node->m_a2 = Optimize(node->m_a2);

  // Optimize right side
  node->m_a3 = Optimize(node->m_a3);

  // Done
  return node;
}

Ast* 
CodeGenerator::OptimizeBinaryExpression(Ast* node)
{
  // Optimize left-hand side
  node->m_a2 = Optimize(node->m_a2);

  // Or with literal true on left side
  if(node->m_a1 == op_logor     && 
     IsType(node->m_a2, literal)&& 
     LiteralAsBool(node->m_a2)  )
  {
    delete node;
    node = new Ast(literal, Variant(true));
    node->m_idempotent = true;
    return node;
  }

  // And with literal false on left side
  if(node->m_a1 == op_logand    &&
     IsType(node->m_a2, literal)&&
     !LiteralAsBool(node->m_a2) )
  {
    delete node;
    node = new Ast(literal, Variant(false));
    node->m_idempotent = true;
    return node;
  }

  // Optimize right side
  node->m_a3 = Optimize(node->m_a3);

  // Determine idempotence
  node->m_idempotent = IsIdempotent(node->m_a2) && 
                       IsIdempotent(node->m_a3) ;

  // Check whether both sides are literals
  if(!IsType(node->m_a2, literal) || 
     !IsType(node->m_a3, literal) )
  {
    // Nothing to do here
    return node;
  }

  // Extract values
  Variant lhs = LiteralAsVariant(node->m_a2);
  Variant rhs = LiteralAsVariant(node->m_a3);

  // Calculate new value
  Ast* rep = 0;
  switch((opcodes)node->m_a1)
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
    rep->m_idempotent = true;
    delete node;
    node = rep;
  }
  
  // Return (replaced) node
  return node;
}

Ast* 
CodeGenerator::OptimizeTernaryExpression(Ast* node)
{
  // Optimize nodes
  node->m_a1 = Optimize(node->m_a1);
  node->m_a2 = Optimize(node->m_a2);
  node->m_a3 = Optimize(node->m_a3);

  // Optimize for literal condition
  if(IsType(node->m_a1, literal))
  {
    // Decide which branch to pick
    if(LiteralAsBool(node->m_a1))
    {
      Ast* res = node->m_a2;
      delete node;
      node = res;
    }
    else
    {
      Ast* res = node->m_a3;
      delete node;
      node = res;
    }
  }
  else
  {
    // Determine idempotence
    node->m_idempotent = 
      IsIdempotent(node->m_a1) &&
      IsIdempotent(node->m_a2) &&
      IsIdempotent(node->m_a3) ;
  }

  // Succeeded
  return node;
}


Ast* 
CodeGenerator::OptimizeStatementSequence(Ast* node)
{
  // Determine old and new list
  AstList* old = any_cast<AstList*>(node->m_a1);
  AstList* rep = new AstList;

  // Check for idempotence
  bool idempotent = true;

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
      idempotent &= opt->m_idempotent;
    }
  }

  // The new list is empty
  if(rep->size() == 0)
  {
    delete node;
    delete old;
    delete rep;
    node = new Ast(empty_statement);
    node->m_idempotent = true;
    return node;
  }

  // The new list contains one statement
  if(rep->size() == 1)
  {
    delete node;
    node = *rep->begin();
    delete old;
    delete rep;
    return node;
  }

  // Replace old list
  delete old;
  node->m_a1 = rep;
  node->m_idempotent = idempotent;
  return node;
}

Ast*
CodeGenerator::OptimizeExpressionStatement(Ast* node)
{
  // Optimize expression
  node->m_a1 = Optimize(node->m_a1);

  // Replace idempotent expression with empty statement
  if(IsIdempotent(node->m_a1))
  {
    delete node;
    node = new Ast(empty_statement);
    node->m_idempotent = true;
  }

  // Done
  return node;
}

Ast* 
CodeGenerator::OptimizeCompoundStatement(Ast* node)
{
  // If empty, return empty statement
  if(node->m_a1.empty())
  {
    delete node;
    node = new Ast(empty_statement);
    node->m_idempotent = true;
    return node;
  }

  // Optimize content
  if(!node->m_a1.empty())
  {
    node->m_a1 = Optimize(node->m_a1);
  }

  // If empty, Optimize further
  if(IsType(node->m_a1, empty_statement))
  {
    Ast* res = node->m_a1;
    delete node;
    node = res;
  }

  // Done
  return node;
}
