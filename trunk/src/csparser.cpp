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

#include "csparser.gen.h"
#include "csparser.gen.c"

//////////////////////////////////////////////////////////////////////////

typedef LemonParser<
  CSParser, 
  Token, 
  CSParseAlloc, 
  CSParseFree, 
#ifdef _DEBUG
  CSParseTrace, 
#else
  0,
#endif
  CSParse> CSParserImpl;

//////////////////////////////////////////////////////////////////////////

Object* 
CSParser::Parse(LexStream& stream, bool debug)
{
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
