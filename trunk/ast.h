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

class Ast;
class File;

//////////////////////////////////////////////////////////////////////////
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
  lvalue,
  list_literal,
  list_content,
  list_entry,
  json_literal,
  json_content,
  json_entry,
  function_declaration,
  parameter,
  parameter_list,
  variable_declaration,
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

enum AccessTypes
{
  accessNone,
  accessDefault,
  accessPrivate,
  accessProtected,
  accessPublic,
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

#define ATYPE(arg)  ((*arg)["type"].GetInt())
#define A1(arg)     ((*arg)["a1"])
#define A2(arg)     ((*arg)["a2"])
#define A3(arg)     ((*arg)["a3"])
#define A4(arg)     ((*arg)["a4"])

#endif // CSCRIPT_AST_H
