//////////////////////////////////////////////////////////////////////////
//
// This file is © 2009 - 2011 JJ Fahner <jan-jaap@jan-jaap.net>
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
#include <ast.h>

String AstTypeToString(AstTypes type)
{
  switch(type)
  {
  default: return "unknown";
  case invalid: return "invalid";
  case translation_unit: return "translation_unit";
  case statement_sequence: return "statement_sequence";
  case expression_statement: return "expression_statement";
  case assignment_expression: return "assignment_expression";
  case binary_expression: return "binary_expression";
  case ternary_expression: return "ternary_expression";
  case prefix_expression: return "prefix_expression";
  case postfix_expression: return "postfix_expression";
  case typeof_expression: return "typeof_expression";
  case member_expression: return "member_expression";
  case index_expression: return "index_expression";
  case function_call: return "function_call";
  case literal_value: return "literal_value";
  case null_literal: return "null_literal";
  case shell_command: return "shell_command";
  case unqualified_id: return "unqualified_id";
  case qualified_id_g: return "qualified_id_g";
  case qualified_id_l: return "qualified_id_l";
  case list_literal: return "list_literal";
  case list_content: return "list_content";
  case list_append: return "list_append";
  case list_entry: return "list_entry";
  case map_literal: return "map_literal";
  case map_content: return "map_content";
  case map_entry: return "map_entry";
  case json_literal: return "json_literal";
  case json_content: return "json_content";
  case json_entry: return "json_entry";
  case function_declaration: return "function_declaration";
  case parameter: return "parameter";
  case parameter_list: return "parameter_list";
  case namespace_declaration: return "namespace_declaration";
  case variable_declaration: return "variable_declaration";
  case operator_declaration: return "operator_declaration";
  case declarator_sequence: return "declarator_sequence";
  case declaration_sequence: return "declaration_sequence";
  case include_statement: return "include_statement";
  case empty_statement: return "empty_statement";
  case for_statement: return "for_statement";
  case foreach_statement: return "foreach_statement";
  case if_statement: return "if_statement";
  case while_statement: return "while_statement";
  case return_statement: return "return_statement";
  case compound_statement: return "compound_statement";
  case break_statement: return "break_statement";
  case continue_statement: return "continue_statement";
  case switch_statement: return "switch_statement";
  case switch_body: return "switch_body";
  case switch_case: return "switch_case";
  case default_case: return "default_case";
  case new_expression: return "new_expression";
  case this_expression: return "this_expression";
  case type_conversion: return "type_conversion";
  case extern_declaration: return "extern_declaration";
  case extern_parameter: return "extern_parameter";
  case type_specifier: return "type_specifier";
  case arguments: return "arguments";
  case try_statement: return "try_statement";
  case catch_block: return "catch_block";
  case finally_block: return "finally_block";
  case throw_expression: return "throw_expression";
  case unset_statement: return "unset_statement";
  case conversion_expression: return "conversion_expression";
  case closure_expression: return "closure_expression";
  case function_member_expression: return "function_member_expression";
  case function_index_expression: return "function_index_expression";
  case logical_or_expression: return "logical_or_expression";
  case logical_and_expression: return "logical_and_expression";
  case lambda_expression: return "lambda_expression";
  }
}
