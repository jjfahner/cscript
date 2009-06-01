//////////////////////////////////////////////////////////////////////////
//
// This file is © 2009 JJ Fahner <jan-jaap@jan-jaap.net>
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
#include "csparser.h"
#include "tokens.h"
#include "lemon.h"
#include "cslexer.h"
#include "xmlparser.h"
#include "lexstream.h"
#include "datatype.h"
#include "timer.h"

#include "csparser.gen.h"
#include "csparser.gen.c"

//////////////////////////////////////////////////////////////////////////

void CSParseTraceDummy(FILE*, char*) {}

typedef LemonParser<
  CSParser, 
  Token, 
  CSParseAlloc, 
  CSParseFree, 
#ifdef _DEBUG
  CSParseTrace, 
#else
  CSParseTraceDummy,
#endif
  CSParse> CSParserImpl;

//////////////////////////////////////////////////////////////////////////

Object* 
CSParser::Parse(LexStream& stream, bool debug)
{
  Timer timer;

  m_root = 0;

  // Store stream
  m_stream = &stream;

  // Initialize lexer
  CSLexer lexer(stream);

  // Allocate parser
  CSParserImpl parser(this, "CScript Parser: ", debug);

  // Parse tokens
  Token token;
  while(lexer.Lex(token))
  {
    // Push token to parser
    parser(token.m_type, token);

    // End of input
    if(token.m_type == CS_EOF)
    {
      parser();
      break;
    }
  }

  // Swap root node
  Object* root = 0;
  std::swap(root, m_root);

  // Forget stream
  m_stream = 0;

  // Store parse time
  m_elapsed = timer.Elapsed();

  // Done
  return root;
}

Object* 
CSParser::ParseXml()
{
  // Create xml parser
  XmlParser parser;

  // Backup two chars in the stream
  m_stream->m_cursor -= 2;

  // Parse from current stream
  return parser.Parse(*m_stream);
}

void 
CSParser::OnParseFailure()
{
  throw std::runtime_error("CSParser: Parse error");
}

void 
CSParser::OnSyntaxError()
{
  throw std::runtime_error("CSParser: Syntax error");
}

void 
CSParser::SetRoot(Object* root)
{
   m_root = root;
}

DataType* 
CSParser::GetDataType(AstNode* node)
{
  switch(node->m_type)
  {
  default: 
    return VoidType::Instance();

  case expression_statement: 
    return AstNode_A1(node)->m_dataType;

  case assignment_expression: 
    return AstNode_A2(node)->m_dataType;

  case binary_expression:
    switch(Ast_A1(node).GetInt())
    {
    case op_add:
    case op_sub:
    case op_mul:
    case op_div:
    case op_mod:
      return AstNode_A2(node)->m_dataType; 
    case op_logor:
    case op_logand:
      return BooleanType::Instance();
    case op_bitor:
    case op_bitxor:
    case op_bitand:
      return IntegerType::Instance();
    case op_seq:
    case op_sne:
    case op_eq:
    case op_ne:
    case op_lt:
    case op_le:
    case op_gt:
    case op_ge:
      return BooleanType::Instance();
    default:
      throw std::runtime_error("Invalid binary operator");
    }

  case ternary_expression:
    if(AstNode_A2(node)->m_dataType != AstNode_A3(node)->m_dataType)
    {
      throw std::runtime_error("Invalid expression");
    }
    return AstNode_A2(node)->m_dataType;

  case prefix_expression: 
    switch(Ast_A1(node).GetInt())
    {
    case op_preinc:
    case op_predec:
      return AstNode_A2(node)->m_dataType;
    case op_negate:
    case op_not:
      return BooleanType::Instance();
    default:
      throw std::runtime_error("Invalid prefix operator");
    }

  case postfix_expression: 
    switch(Ast_A1(node).GetInt())
    {
    case op_postinc:
    case op_postdec:
      return AstNode_A2(node)->m_dataType;
    default:
      throw std::runtime_error("Invalid postfix operator");
    }

  case typeof_expression:
    return ObjectType::Instance();

  case member_expression:
    return UnknownType::Instance();

  case index_expression:
    return UnknownType::Instance();

  case function_call:
    return UnknownType::Instance();

  case literal_value:
    return node->m_a1.GetDataType();

  case null_literal:
    return NullType::Instance();

  case shell_command:
    return IntegerType::Instance();

  case unqualified_id:
    return UnknownType::Instance();

  case qualified_id_g:
    return UnknownType::Instance();

  case qualified_id_l:
    return UnknownType::Instance();

  case list_literal:
    return ObjectType::Instance();

  case list_content:
  case list_entry:
    return UnknownType::Instance();

  case map_literal:
    return ObjectType::Instance();

  case map_content:
  case map_entry:
    return UnknownType::Instance();

  case json_literal:
    return ObjectType::Instance();

  case json_content:
  case json_entry:
    return UnknownType::Instance();

  case function_declaration:
    return FunctionType::Instance();

  case native_declaration:
    return NativeFunctionType::Instance();

  /*
  case parameter: break;
  case parameter_list: break;
  case variable_declaration: break;
  case operator_declaration: break;
  case return_statement: break;
  */

  case new_expression:
    return ObjectType::Instance();

  case this_expression:
    return ObjectType::Instance();

    /*
  case type_conversion: break;
  case extern_declaration: break;
  case extern_parameter: break;
  case type_specifier: break;
  case arguments: break;
  case throw_expression: break;
  case unset_statement: break;
  case conversion_expression: break;
    */

  case closure_expression:
    return FunctionType::Instance();

    /*
  case function_member_expression: break;
  case function_index_expression: break;
  case xml_expression: break;
  case xml_processing_instruction: break;
  case xml_elements: break;
  case xml_open_tag: break;
  case xml_close_tag: break;
  case xml_closed_tag: break;
  case xml_text: break;
  case xml_attribute: break;
  case xml_attributes: break;
  case xml_uname: break;
  case xml_qname: break;
    */
  }
}
