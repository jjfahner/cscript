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

#include <opcodes.h>

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
  typeof_expression,
  member_expression,
  index_expression,
  function_call,
  literal_value,
  null_literal,
  shell_command,
  unqualified_id,
  qualified_id_g,
  qualified_id_l,
  list_literal,
  list_content,
  list_append,
  list_entry,
  map_literal,
  map_content,
  map_entry,
  json_literal,
  json_content,
  json_entry,
  function_declaration,
  native_declaration,
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
  arguments,
  try_statement,
  catch_block,
  finally_block,
  throw_expression,
  unset_statement,
  conversion_expression,
  closure_expression,
  function_member_expression,
  function_index_expression,
  logical_or_expression,
  logical_and_expression,
  lambda_expression
};

enum ParameterTypes
{
  ptByVal,
  ptByRef,
  ptVariadic,
};

String AstTypeToString(AstTypes type);

#endif // CSCRIPT_AST_H
