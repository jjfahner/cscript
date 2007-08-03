#include "codegen.h"

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

Ast*
CodeGenerator::Optimize(Ast* node)
{
  switch(node->m_type)
  {
  case statement_sequence:
    ReduceStatementSequence(node);
    break;

  case expression_statement:
    node = ReduceExpressionStatement(node);
    break;

  case assignment_expression:
    node->m_a2 = Optimize(node->m_a2);
    node->m_a3 = Optimize(node->m_a3);
    break;

  case binary_expression:
    node = ReduceBinaryExpression(node);
    break;

  case ternary_expression:
    node = ReduceTernaryExpression(node);
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
    node->m_a2 = Optimize(node->m_a2);
    break;

  case argument_list:
    node->m_a1 = Optimize(node->m_a1);
    node->m_a2 = Optimize(node->m_a2);
    break;

  case function_declaration:
    node->m_a3 = Optimize(node->m_a3);
    break;

  case literal:
  case list_literal:
  case lvalue:
  case parameter:
  case parameter_list:
    break;

  case variable_declaration:
    if(!node->m_a2.empty())
    {
      node->m_a2 = Optimize(node->m_a2);
    }
    break;

  case declaration_sequence:
    node->m_a1 = Optimize(node->m_a1);
    node->m_a2 = Optimize(node->m_a1);
    break;

  case include_statement:
    break;

  case for_statement:
    node = ReduceForStatement(node);
    break;

  case foreach_statement:
    break;

  case if_statement:
    node = ReduceIfStatement(node);
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
    node = ReduceCompoundStatement(node);
    break;

  default:
    throw std::runtime_error("Unknown node type");
  }

  return node;
}

Ast* 
CodeGenerator::ReduceIfStatement(Ast* node)
{
  // Reduce condition expression
  node->m_a1 = Optimize(node->m_a1);
  
  // If the condition is constant, reduce to corresponding branch
  if(any_cast<Ast*>(node->m_a1)->m_type == literal)
  {
    std::cout << "Reducing if statement\n";

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

    // Reduce to null statement
    delete node;
    return new Ast(empty_statement);
  }

  // Reduce if branch
  node->m_a2 = Optimize(node->m_a2);

//   // Emptyness
//   bool le = IsType(node->m_a2, empty_statement);
//   bool re = true;

  // Reduce else branch
  if(!node->m_a3.empty())
  {
    node->m_a3 = Optimize(node->m_a3);
//     re = IsType(node->m_a3, empty_statement);
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
CodeGenerator::ReduceForStatement(Ast* node)
{
  // Reduce subexpressions
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

    // Use statement sequence as body
    body = new Ast(statement_sequence, list);
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
CodeGenerator::ReduceBinaryExpression(Ast* node)
{
  // Reduce left-hand side
  node->m_a2 = Optimize(node->m_a2);

  // Reduce left side of short-circuited logical operators
  if(node->m_a1 == op_logor     && 
     IsType(node->m_a2, literal)&& 
     LiteralAsBool(node->m_a2)  )
  {
    std::cout << "Reducing binary expression\n";
    delete node;
    return new Ast(literal, Variant(true));
  }
  if(node->m_a1 == op_logand    &&
     IsType(node->m_a2, literal)&&
     !LiteralAsBool(node->m_a2) )
  {
    std::cout << "Reducing binary expression\n";
    delete node;
    return new Ast(literal, Variant(false));
  }

  // Reduce right side
  node->m_a3 = Optimize(node->m_a3);

  // Check whether both sides are literals
  if(!IsType(node->m_a2, literal) || !IsType(node->m_a3, literal))
  {
    // Nothing to do here
    return node;
  }

  std::cout << "Reducing binary expression\n";

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
  default:        std::cout << "Cannot reduce unknown binary operator\n"; break;
  }

  // Replace previous node
  if(rep)
  {
    delete node;
    node = rep;
  }
  
  // Return (replaced) node
  return node;
}

Ast* 
CodeGenerator::ReduceTernaryExpression(Ast* node)
{
  // Reduce condition node
  node->m_a1 = Optimize(node->m_a1);

  // Reduce for literal condition
  if(IsType(node->m_a1, literal))
  {
    std::cout << "Reducing ternary expression\n";

    // Decide which branch to pick
    if(LiteralAsBool(node->m_a2))
    {
      Ast* res = Optimize(node->m_a2);
      delete node;
      node = res;
    }
    else
    {
      Ast* res = Optimize(node->m_a3);
      delete node;
      node = res;
    }
  }
  else
  {
    // Reduce both branches
    node->m_a2 = Optimize(node->m_a2);
    node->m_a3 = Optimize(node->m_a3);
  }

  // Succeeded
  return node;
}


Ast* 
CodeGenerator::ReduceStatementSequence(Ast* node)
{
  // Determine old and new list
  AstList* old = any_cast<AstList*>(node->m_a1);
  AstList* rep = new AstList;

  // Enumerate statements
  AstList::iterator si, se;
  si = old->begin();
  se = old->end();
  for(; si != se; ++si)
  {
    // Optimize the statement
    Ast* opt = Optimize(*si);

    // If not empty, place in new list
    if(!IsType(opt, empty_statement))
    {
      rep->push_back(opt);
    }
  }

  // The new list is empty
  if(rep->size() == 0)
  {
    delete node;
    delete old;
    delete rep;
    return new Ast(empty_statement);
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
  return node;
}

Ast*
CodeGenerator::ReduceExpressionStatement(Ast* node)
{
  // Reduce expression
  node->m_a1 = Optimize(node->m_a1);

  // Replace literal with empty statement
  if(IsType(node->m_a1, literal))
  {
    delete node;
    node = new Ast(empty_statement);
  }

  // Done
  return node;
}

Ast* 
CodeGenerator::ReduceCompoundStatement(Ast* node)
{
  // If empty, return empty statement
  if(node->m_a1.empty())
  {
    std::cout << "Reducing compound statement\n";
    delete node;
    return new Ast(empty_statement);
  }

  // Reduce content
  node->m_a1 = Optimize(node->m_a1);

  // If empty, reduce further
  if(IsType(node->m_a1, empty_statement))
  {
    std::cout << "Reducing compound statement\n";
    Ast* res = node->m_a1;
    delete node;
    node = res;
  }

  // Done
  return node;
}

