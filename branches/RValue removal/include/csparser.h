//////////////////////////////////////////////////////////////////////////
//
// This file is � 2009 JJ Fahner <jan-jaap@jan-jaap.net>
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
#ifndef CSCRIPT_CS_PARSER_H
#define CSCRIPT_CS_PARSER_H

#include <cscript.h>
#include <object.h>
#include <astnode.h>

enum AstTypes;
class LexStream;

class CSParser
{
public:

  //
  // Parse contents of stream
  //
  Object* Parse(LexStream&, bool debug = false);

  //
  // Parse xml sub-expression
  //
  Object* ParseXml();

  //
  // Errors raised by parser
  //
  void OnParseFailure();
  void OnSyntaxError();

  //
  // Used by the parser to allocate nodes
  //
  AstNode* AllocNode(AstTypes type);
  AstNode* AllocNode(AstTypes type, Value const& a1);
  AstNode* AllocNode(AstTypes type, Value const& a1, Value const& a2);
  AstNode* AllocNode(AstTypes type, Value const& a1, Value const& a2, Value const& a3);
  AstNode* AllocNode(AstTypes type, Value const& a1, Value const& a2, Value const& a3, Value const& a4);

  //
  // Used by the parser to store the root node
  //
  void SetRoot(Object*);

private:

  //
  // Determine node data type
  //
  DataType* GetDataType(AstNode* node);

  //
  // Members
  //
  Object*     m_root;
  LexStream*  m_stream;

};


//////////////////////////////////////////////////////////////////////////
//
// Node allocation
//

inline AstNode* 
CSParser::AllocNode(AstTypes type)
{
  AstNode* node = new AstNode(type);
  node->m_dataType = GetDataType(node);
  return node;
}

inline AstNode* 
CSParser::AllocNode(AstTypes type, Value const& a1)
{
  AstNode* node = new AstNode(type, a1);
  node->m_dataType = GetDataType(node);
  return node;
}

inline AstNode* 
CSParser::AllocNode(AstTypes type, Value const& a1, Value const& a2)
{
  AstNode* node = new AstNode(type, a1, a2);
  node->m_dataType = GetDataType(node);
  return node;
}

inline AstNode* 
CSParser::AllocNode(AstTypes type, Value const& a1, Value const& a2, Value const& a3)
{
  AstNode* node = new AstNode(type, a1, a2, a3);
  node->m_dataType = GetDataType(node);
  return node;
}

inline AstNode* 
CSParser::AllocNode(AstTypes type, Value const& a1, Value const& a2, Value const& a3, Value const& a4)
{
  AstNode* node = new AstNode(type, a1, a2, a3, a4);
  node->m_dataType = GetDataType(node);
  return node;
}

#endif // CSCRIPT_CS_PARSER_H