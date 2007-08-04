#include "codegen.h"

//////////////////////////////////////////////////////////////////////////
//
// Helpers
//

String OpString(opcodes op)
{
  switch(op)
  {
  case op_add:      return "+";
  case op_sub:      return "-";
  case op_mul:      return "*";
  case op_div:      return "/";
  case op_mod:      return "%";
  case op_logor:    return "||";
  case op_logand:   return "&&";
  case op_bitor:    return "|";
  case op_bitxor:   return "^";
  case op_bitand:   return "&";
  case op_eq:       return "==";
  case op_ne:       return "!=";
  case op_lt:       return "<";
  case op_le:       return "<=";
  case op_gt:       return ">";
  case op_ge:       return ">=";
  case op_assign:   return "=";
  case op_assadd:   return "+=";
  case op_asssub:   return "-=";
  case op_assmul:   return "*=";
  case op_assdiv:   return "/=";
  case op_assmod:   return "%=";
  default:          return "??";
  }
}

//////////////////////////////////////////////////////////////////////////
//
// Print implementation
//

void
CodeGenerator::Print(String filename, Ast* node)
{
  std::ofstream s(filename.c_str());
  PrintImpl(node, 0, s);
}

void 
CodeGenerator::PrintImpl(Ast* node, int level, std::ostream& s)
{
  String indent;
  for(int i = 0; i < level; ++i)
  {
    indent += " ";
  }

  AstList::iterator si, se;
  switch(node->m_type)
  {
  case statement_sequence:
    si = any_cast<AstList*>(node->m_a1)->begin();
    se = any_cast<AstList*>(node->m_a1)->end();
    for(; si != se; ++si)
    {
      PrintImpl(*si, level, s);
    }
    break;

  case expression_statement:
    s << indent;
    PrintImpl(node->m_a1, level, s);
    s << ";\n";
    break;

  case assignment_expression:
    PrintImpl(node->m_a2, level, s);
    s << OpString(node->m_a1);
    PrintImpl(node->m_a3, level, s);
    break;

  case binary_expression:
    s << "(";
    PrintImpl(node->m_a2, level, s);
    s << OpString(node->m_a1);
    PrintImpl(node->m_a3, level, s);
    s << ")";
    break;

  case ternary_expression:
    PrintImpl(node->m_a1, level, s);
    s << " ? ";
    PrintImpl(node->m_a2, level, s);
    s << " : ";
    PrintImpl(node->m_a3, level, s);
    break;

  case prefix_expression:
    s << "++";
    PrintImpl(node->m_a2, level, s);
    break;

  case postfix_expression:
    PrintImpl(node->m_a2, level, s);
    s << "++";
    break;

  case member_expression:
    break;

  case index_expression:
    PrintImpl(node->m_a1, level, s);
    s << "[";
    PrintImpl(node->m_a2, level, s);
    s << "]";
    break;

  case function_call:
    s << any_cast<String>(node->m_a1);
    s << "(";
    PrintImpl(node->m_a2, level, s);
    s << ")";
    break;

  case literal:
    if(any_cast<Variant>(node->m_a1).GetType() == Variant::stString)
    {
      String temp = any_cast<Variant>(node->m_a1).AsString();
      size_t pos;
      while((pos = temp.find("\n")) != String::npos)
      {
        temp.replace(pos, 1, "\\n");
      }
      s << "\"" << temp << "\"";
    }
    else
    {
      s << any_cast<Variant>(node->m_a1).AsString();
    }
    break;

  case lvalue:
    s << any_cast<String>(node->m_a1);
    break;

  case list_literal:
    {
      s << "[";
      AstList* list = any_cast<AstList*>(node->m_a1);
      AstList::iterator si, se;
      si = list->begin();
      se = list->end();
      String sep = "";
      for(; si != se; ++si)
      {
        s << sep;
        sep = ",";
        PrintImpl(*si, level, s);
      }
      s << "]";
    }
    break;
    return;

  case argument_list:
    PrintImpl(node->m_a1, level, s);
    s << ", ";
    PrintImpl(node->m_a2, level, s);
    break;

  case function_declaration:
    s << "function ";
    s << any_cast<String>(node->m_a1);
    s << "(";
    if(!node->m_a2.empty())
    {
      PrintImpl(node->m_a2, level, s);
    }
    s << ")\n";
    PrintImpl(node->m_a3, level, s);
    break;

  case parameter:
    s << any_cast<String>(node->m_a1);
    break;

  case parameter_list:
    PrintImpl(node->m_a1, level, s);
    s << ", ";
    PrintImpl(node->m_a2, level, s);
    break;

  case variable_declaration:
    s << indent << "var ";
    s << any_cast<String>(node->m_a1);
    if(!node->m_a2.empty())
    {
      s << " = ";
      PrintImpl(node->m_a2, level, s);
    }
    s << ";\n";
    break;

  case declaration_sequence:
    PrintImpl(node->m_a1, level, s);
    PrintImpl(node->m_a2, level, s);
    return;

  case empty_statement:
    s << indent << ";\n";
    break;

  case include_statement:
    s << indent << "include \"" << any_cast<String>(node->m_a1) << "\";\n";
    break;

  case for_statement:
    s << indent << "for(";
    PrintImpl(node->m_a1, level, s);
    s << ";";
    PrintImpl(node->m_a2, level, s);
    s << ";";
    PrintImpl(node->m_a3, level, s);
    s << ")\n";
    PrintImpl(node->m_a4, level, s);
    break;

  case foreach_statement:
    break;

  case if_statement:
    s << indent << "if(";
    PrintImpl(node->m_a1, level, s);
    s << ")\n";
    PrintImpl(node->m_a2, level, s);
    break;

  case while_statement:
    s << indent << "while(";
    PrintImpl(node->m_a1, level, s);
    s << ")\n";
    PrintImpl(node->m_a2, level, s);
    break;

  case return_statement:
    break;

  case compound_statement:
    s << indent << "{\n";
    PrintImpl(node->m_a1, level + 2, s);
    s << indent << "}\n";
    break;

  }
}
