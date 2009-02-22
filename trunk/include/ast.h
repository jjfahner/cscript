//////////////////////////////////////////////////////////////////////////
//
// This file is © 2007 - 2009 JJ Fahner <jan-jaap@jan-jaap.net>
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
#ifndef CSCRIPT_AST_H
#define CSCRIPT_AST_H

#include "opcodes.h"

//
// Ast node types
//
enum AstTypes
{
  invalid,
  translation_unit,
  statement_sequence,
  expression_statement,
  assignment_expression,
  binary_expression,
  ternary_expression,
  prefix_expression,
  postfix_expression,
  member_expression,
  index_expression,
  function_call,
  literal_value,
  shell_command,
  unqualified_id,
  qualified_id_g,
  qualified_id_l,
  list_literal,
  list_content,
  list_entry,
  json_literal,
  json_content,
  json_entry,
  function_declaration,
  parameter,
  parameter_list,
  namespace_declaration,
  variable_declaration,
  operator_declaration,
  declarator_sequence,
  declaration_sequence,
  include_statement,
  empty_statement,
  for_statement,
  foreach_statement,
  if_statement,
  while_statement,
  return_statement,
  compound_statement,
  break_statement,
  continue_statement,
  switch_statement,
  switch_body,
  switch_case,
  default_case,
  new_expression,
  this_expression,
  type_conversion,
  extern_declaration,
  extern_parameter,
  type_specifier,
  positional_arguments,
  named_arguments,
  named_argument,
  try_statement,
  catch_block,
  finally_block,
  throw_statement,
  conversion_expression,
  closure_declaration,
  function_member_expression,
  function_index_expression,
  xml_expression,
  xml_processing_instruction,
  xml_elements,
  xml_open_tag,
  xml_close_tag,
  xml_closed_tag,
  xml_text,
  xml_attribute,
  xml_attributes,
  xml_uname,
  xml_qname
};

enum ParameterTypes
{
  ptByVal,
  ptByRef,
  ptVariadic,
};

//////////////////////////////////////////////////////////////////////////
//
// Ast accessors
//

#include "object.h"
#include "variable.h"

inline AstTypes Ast_Type(Object* obj) {
  return (AstTypes)(*obj)["type"].GetInt();
}
inline LValue& Ast_A1(Object* arg) {
  return ((*arg)["a1"]);
}
inline LValue& Ast_A2(Object* arg) {
  return ((*arg)["a2"]);
}
inline LValue& Ast_A3(Object* arg) {
  return ((*arg)["a3"]);
}
inline LValue& Ast_A4(Object* arg) {
  return ((*arg)["a4"]);
}

#endif // CSCRIPT_AST_H
