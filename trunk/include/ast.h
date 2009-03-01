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

#include <object.h>
#include <variable.h>

inline AstTypes Ast_Type(Object* obj) {
  return (AstTypes)(*obj)[0].GetInt();
}
inline LValue& Ast_A1(Object* arg) {
  return ((*arg)[1]);
}
inline LValue& Ast_A2(Object* arg) {
  return ((*arg)[2]);
}
inline LValue& Ast_A3(Object* arg) {
  return ((*arg)[3]);
}
inline LValue& Ast_A4(Object* arg) {
  return ((*arg)[4]);
}

//////////////////////////////////////////////////////////////////////////
//
// Ast recursor. Recurse through right-recursive ast structure.
//

class AstIterator
{
  Object*   m_root;
  Object*   m_iter;
  AstTypes  m_type;

public:

  AstIterator() :
  m_root (0),
  m_iter (0),
  m_type (invalid)
  {
  }

  AstIterator(AstIterator const& rhs) :
  m_root (rhs.m_root),
  m_iter (rhs.m_iter),
  m_type (rhs.m_type)
  {
  }

  AstIterator(Object* root, AstTypes recursionType) :
  m_root (root),
  m_iter (root),
  m_type (recursionType)
  {
  }

  friend bool operator == (AstIterator const& lhs, AstIterator const& rhs)
  {
    if(lhs.m_root && rhs.m_root && lhs.m_root != rhs.m_root)
    {
      throw std::runtime_error("Comparing iterators from different subtrees");
    }
    return lhs.m_iter == rhs.m_iter;
  }

  friend bool operator != (AstIterator const& lhs, AstIterator const& rhs)
  {
    return !(lhs == rhs);
  }

  bool AtEnd() const
  {
    return m_iter == 0;
  }

  operator bool () const
  {
    return !AtEnd();
  }

  bool operator ! () const
  {
    return AtEnd();
  }

  Object* GetValue() const
  {
    if(m_iter == 0)
    {
      throw std::runtime_error("Dereferencing invalid ast iterator");
    }

    if(Ast_Type(m_iter) == m_type)
    {
      return Ast_A1(m_iter);
    }
    
    return m_iter;
  }

  Object* operator * () const
  {
    return GetValue();
  }

  Object* operator -> () const
  {
    return GetValue();
  }

  void Next()
  {
    if(m_iter == 0)
    {
      throw std::runtime_error("Dereferencing invalid ast iterator");
    }

    if(Ast_Type(m_iter) == m_type)
    {
      m_iter = Ast_A2(m_iter);
    }
    else
    {
      m_iter = 0;
    }
  }

  AstIterator const& operator ++ () 
  {
    Next();
    return *this;
  }

  AstIterator operator ++ (int)
  {
    AstIterator temp(*this);
    Next();
    return temp;
  }

};

#endif // CSCRIPT_AST_H
