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

#include <algorithm>
#include <iostream>

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

  // Initialize stack
  m_scopes.clear();
  EnterScope(0);

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

  // Leave stack
  LeaveScope();

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

//////////////////////////////////////////////////////////////////////////

void 
CSParser::OnParseFailure()
{
  char buf[100];
  sprintf(buf, "Parse error on line %d, char %d", m_stream->m_line, m_stream->m_char);
  throw std::runtime_error(buf);
}

void 
CSParser::OnSyntaxError()
{
  char buf[100];
  sprintf(buf, "Parse error on line %d, char %d", m_stream->m_line, m_stream->m_char);
  throw std::runtime_error(buf);
}

//////////////////////////////////////////////////////////////////////////

void 
CSParser::EnterScope(AstNode* node, String name)
{
  // Build name for unnamed nodes
  if(node && name.empty())
  {
    int64 id = ++m_autoIds[node->m_type];
    name = AstTypeToString(node->m_type) + "_" + ValString(id);
  }

  // Create scope
  LexScope scope;
  scope.m_node = node;
  scope.m_name = name;

  // Build full name
  if(node)
  {
    scope.m_full = m_scopes.back().m_full + "::" + name;
  }
  
  // Append scope
  m_scopes.push_back(scope);
}

void 
CSParser::LeaveScope()
{
  m_scopes.pop_back();
}

void
CSParser::AddVar(AstNode* node, String name)
{
  // Check for duplicates
  LexScope& scope = m_scopes.back();
  for(size_t i = 0; i < scope.m_vars.size(); ++i)
  {
    if(scope.m_vars[i].m_name == name)
    {
      throw std::runtime_error("Variable '" + name + "' already declared\n");
    }
  }

  // Create variable
  Variable var;
  var.m_node = node;
  var.m_name = name;
  var.m_full = m_scopes.back().m_full + "::" + name;

  // Add to list
  scope.m_vars.push_back(var);
}

AstNode* 
CSParser::GetVar(String name)
{
  // Walk scopes backwards to find name
  for(size_t s = m_scopes.size(); s--; )
  {
    LexScope& scope = m_scopes[s];
    for(size_t i = 0; i < scope.m_vars.size(); ++i)
    {
      if(scope.m_vars[i].m_name == name)
      {
        return scope.m_vars[i].m_node;
      }
    }
  }
  
  // Name not found
  //std::cout << "Variable '" << name << "' not declared\n";
  return 0;
  //throw std::runtime_error("Variable '" + name + "' not declared");
}

//////////////////////////////////////////////////////////////////////////

void 
CSParser::SetRoot(Object* root)
{
   m_root = root;
}

//////////////////////////////////////////////////////////////////////////

void
CSParser::SetNodeAttributes(AstNode& node)
{
  // Initialize attributes
  node[aaResultType]  = VoidType::Instance();
  node[aaSideEffects] = false;

  // Set attributes based on type
  switch(node.m_type)
  {

  case expression_statement: 
    node[aaResultType]  = AstNode_A1(node)[aaResultType];
    node[aaSideEffects] = AstNode_A1(node)[aaSideEffects];
    if(!ValBool(node[aaSideEffects]))
    {
      throw std::runtime_error("Invalid expression statement");
    }
    break;

  case assignment_expression: 
    node[aaResultType]  = AstNode_A2(node)[aaResultType];
    node[aaSideEffects] = true;
    break;

  case binary_expression:
    switch(Ast_A1(node).GetInt())
    {
    case op_add:
    case op_sub:
    case op_mul:
    case op_div:
    case op_mod:
      node[aaResultType]  = AstNode_A2(node)[aaResultType];
      break;
    case op_logor:
    case op_logand:
      node[aaResultType] = BooleanType::Instance();
      break;
    case op_bitor:
    case op_bitxor:
    case op_bitand:
      node[aaResultType] = IntegerType::Instance();
      break;
    case op_seq:
    case op_sne:
    case op_eq:
    case op_ne:
    case op_lt:
    case op_le:
    case op_gt:
    case op_ge:
      node[aaResultType] = BooleanType::Instance();
      break;
    }
    node[aaSideEffects] = 
      ValBool(AstNode_A2(node)[aaSideEffects]) ||
      ValBool(AstNode_A3(node)[aaSideEffects]) ;
    break;

  case ternary_expression:
//     if(AstNode_A2(node)[aaResultType].GetObject() != 
//        AstNode_A3(node)[aaResultType].GetObject() )
//     {
//       throw std::runtime_error("Invalid expression");
//     }
    node[aaResultType]  = AstNode_A2(node)[aaResultType];
    node[aaSideEffects] = 
      ValBool(AstNode_A1(node)[aaSideEffects]) ||
      ValBool(AstNode_A2(node)[aaSideEffects]) ||
      ValBool(AstNode_A3(node)[aaSideEffects]) ;
    break;

  case prefix_expression:
    switch(Ast_A1(node).GetInt())
    {
    case op_add:
    case op_sub:
      node[aaResultType]  = AstNode_A2(node)[aaResultType];
      node[aaSideEffects] = true;
      break;
    case op_negate:
    case op_not:
      node[aaResultType]  = BooleanType::Instance();
      node[aaSideEffects] = false;
      break;
    default:
      throw std::runtime_error("Invalid prefix operator");
    }
    break;

  case postfix_expression: 
    switch(Ast_A1(node).GetInt())
    {
    case op_add:
    case op_sub:
      node[aaResultType] = AstNode_A2(node)[aaResultType];
      node[aaSideEffects] = true;
      break;
    default:
      throw std::runtime_error("Invalid postfix operator");
    }
    break;

  case typeof_expression:
    node[aaResultType]  = ObjectType::Instance();
    node[aaSideEffects] = AstNode_A1(node)[aaSideEffects];
    break;

  case member_expression:
    node[aaResultType]  = UnknownType::Instance();
    node[aaSideEffects] = false;
    break;

  case index_expression:
    node[aaResultType]  = UnknownType::Instance();
    node[aaSideEffects] = true; // The element may be created here
    break;

  case function_call:
    node[aaResultType]  = UnknownType::Instance();
    node[aaSideEffects] = true; // Don't know for sure yet
    break;

  case literal_value:
    node[aaResultType]  = Ast_A1(node).GetDataType();
    node[aaSideEffects] = false;
    break;

  case null_literal:
    node[aaResultType]  = NullType::Instance();
    node[aaSideEffects] = false;
    break;

  case shell_command:
    node[aaResultType]  = IntegerType::Instance();
    node[aaSideEffects] = true;
    break;

  case unqualified_id:
    node[aaResultType]  = UnknownType::Instance();
    node[aaSideEffects] = false;
    break;

  case qualified_id_g:
    node[aaResultType]  = UnknownType::Instance();
    node[aaSideEffects] = false;
    break;

  case qualified_id_l:
    node[aaResultType]  = UnknownType::Instance();
    node[aaSideEffects] = false;
    break;

  case list_literal:
    node[aaResultType]  = ObjectType::Instance();
    node[aaSideEffects] = false; // Not always true: [0, 1, a++]
    break;

  case list_content:
  case list_entry:
    node[aaResultType]  = UnknownType::Instance();
    node[aaSideEffects] = false;
    break;

  case map_literal:
    node[aaResultType]  = ObjectType::Instance();
    node[aaSideEffects] = false; // Same as for list_literal
    break;

  case map_content:
  case map_entry:
    node[aaResultType]  = UnknownType::Instance();
    node[aaSideEffects] = false;
    break;

  case json_literal:
    node[aaResultType]  = ObjectType::Instance();
    node[aaSideEffects] = false; // Same as for list_literal
    break;

  case json_content:
  case json_entry:
    node[aaResultType]  = UnknownType::Instance();
    node[aaSideEffects] = false;
    break;

  case function_declaration:
    node[aaResultType]  = FunctionType::Instance();
    node[aaSideEffects] = false;
    break;

  /*
  case parameter: break;
  case parameter_list: break;
  case variable_declaration: break;
  case operator_declaration: break;
  case return_statement: break;
  */

  case new_expression:
    node[aaResultType]  = ObjectType::Instance();
    node[aaSideEffects] = true;
    break;

  case this_expression:
    node[aaResultType]  = ObjectType::Instance();
    node[aaSideEffects] = false;
    break;

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
    node[aaResultType]  = FunctionType::Instance();
    node[aaSideEffects] = false;
    break;

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
