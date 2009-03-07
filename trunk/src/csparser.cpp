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
#include "cslexer.h"
#include "lemon.h"

#include "csparser.gen.h"
#include "csparser.gen.c"

//////////////////////////////////////////////////////////////////////////

typedef LemonParser<
  CSParser, 
  Token, 
  CSParseAlloc, 
  CSParseFree, 
  CSParseTrace, 
  CSParse> CSParserImpl;

//////////////////////////////////////////////////////////////////////////

Object* 
CSParser::Parse(LexStream& stream)
{
  m_root = 0;

  // Initialize lexer
  CSLexer lexer(stream);

  // Allocate parser
  CSParserImpl parser(this);

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

  // Done
  return root;
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

Object* 
CSParser::AllocNode(AstTypes type)
{
  // Create node
  Object* obj = new Object;
  (*obj)[0] = type;

//   // Set file position
//   if(false && m_file /*&& m_debug*/)
//   {
//     (*obj)[10] = m_file->GetPath();
//     (*obj)[11] = m_lexer->GetLine();
//   }

  // Done
  return obj;
}

void 
CSParser::SetRoot(Object* root)
{
   m_root = root;
}
