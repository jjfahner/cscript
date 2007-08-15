//////////////////////////////////////////////////////////////////////////
//
// This file is � 2007 JJ Fahner <jan-jaap@jan-jaap.net>
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

String OpString(Quad op)
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
  case translation_unit:
    PrintImpl(node->m_a1, level, s);
    break;

  case statement_sequence:
    si = node->m_a1.GetList()->begin();
    se = node->m_a1.GetList()->end();
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
    s << node->m_a1.GetString();
    s << "(";
    if(node->m_a2)
    {
      PrintImpl(node->m_a2, level, s);
    }
    s << ")";
    break;

  case literal:
    if(node->m_a1.Type() == AstData::Text)
    {
      String temp = node->m_a1.GetValue().GetString();
      size_t pos;
      while((pos = temp.find("\n")) != String::npos)
      {
        temp.replace(pos, 1, "\\n");
      }
      s << "\"" << temp << "\"";
    }
    else
    {
      s << node->m_a1.GetValue().GetString();
    }
    break;

  case lvalue:
    s << "@" << (int)node->m_props["stackpos"];
    break;

  case list_literal:
    {
      s << "[";
      PrintImpl(node->m_a1, level, s);
      s << "]";
    }
    break;

  case list_content:
    PrintImpl(node->m_a1, level, s);
    if(node->m_a2)
    {
      s << ",";
      PrintImpl(node->m_a2, level, s);
    }
    break;

  case list_entry:
    PrintImpl(node->m_a1, level, s);
    break;

  case argument_list:
    PrintImpl(node->m_a1, level, s);
    s << ", ";
    PrintImpl(node->m_a2, level, s);
    break;

  case argument:
    PrintImpl(node->m_a1, level, s);
    break;

  case function_declaration:
    s << "function ";
    s << node->m_a1.GetString();
    s << "(";
    if(node->m_a2)
    {
      PrintImpl(node->m_a2, level, s);
    }
    s << ") [" << (String)node->m_props["varcount"] << "]\n";
    PrintImpl(node->m_a3, level, s);
    break;

  case parameter:
    s << "@" << (int)(node->m_props["stackpos"]);
    break;

  case parameter_list:
    PrintImpl(node->m_a1, level, s);
    s << ", ";
    PrintImpl(node->m_a2, level, s);
    break;

  case variable_declaration:
    s << indent << "var ";
    s << "@" << (Quad)node->m_props["stackpos"];
    if(node->m_a2)
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
    s << indent << "include \"" << node->m_a1.GetValue().GetString() << "\";\n";
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
    if(node->m_a3)
    {
      s << indent << "else\n";
      PrintImpl(node->m_a3, level, s);
    }
    break;

  case while_statement:
    s << indent << "while(";
    PrintImpl(node->m_a1, level, s);
    s << ")\n";
    PrintImpl(node->m_a2, level, s);
    break;

  case return_statement:
    s << indent << "return ";
    if(node->m_a1)
    {
      PrintImpl(node->m_a1, level, s);
    }
    s << ";\n";
    break;

  case compound_statement:
    s << indent << "{\n";
    PrintImpl(node->m_a1, level + 2, s);
    s << indent << "}\n";
    break;

  }
}
