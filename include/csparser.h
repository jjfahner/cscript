//////////////////////////////////////////////////////////////////////////
//
// This file is � 2009 - 2011 JJ Fahner <jan-jaap@jan-jaap.net>
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

#include <vector>
#include <map>

enum AstTypes;
struct Token;
class LexStream;
class CSLexer;

class CSParser
{
public:

  struct Variable
  {
    AstNode* m_node;
    String   m_full;
    String   m_name;
  };

  typedef std::vector<Variable> VarVec;

  struct LexScope
  {
    AstNode*  m_node;
    String    m_name;
    String    m_full;
    VarVec    m_vars;
  };

  typedef std::vector<LexScope> LexScopes;

  //
  // Parse contents of stream
  //
  Object* Parse(LexStream&, bool debug = false);

  //
  // Parse xml sub-expression
  //
  Object* ParseXml();

  //
  // Retrieve lexer
  //
  CSLexer* GetLexer();

  //
  // Retrieve the elapsed parse time
  //
  uint64 Elapsed() const;

  //
  // Errors raised by parser
  //
  void OnSyntaxError(Token const& token);

  //
  // Called by parser when entering/exiting lexical scopes
  //
  void EnterScope(AstNode* node, String name = "");
  void LeaveScope();

  //
  // Add a variable to the current scope
  //
  void AddVar(AstNode* node, String name);
  AstNode* GetVar(String name);

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

  typedef std::map<AstTypes, int64> AutoIds;

  //
  // Set attributes on node
  //
  void SetNodeAttributes(AstNode& node);

  //
  // Determine node data type
  //
  DataType* GetDataType(AstNode* node);

  //
  // Members
  //
  Object*     m_root;
  LexStream*  m_stream;
  CSLexer*    m_lexer;
  uint64      m_elapsed;
  LexScopes   m_scopes;
  AutoIds     m_autoIds;

};


//////////////////////////////////////////////////////////////////////////
//
// Node allocation
//

inline CSLexer* 
CSParser::GetLexer()
{
  return m_lexer;
}

inline AstNode* 
CSParser::AllocNode(AstTypes type)
{
  AstNode* node = new AstNode(type);
  SetNodeAttributes(*node);
  return node;
}

inline AstNode* 
CSParser::AllocNode(AstTypes type, Value const& a1)
{
  AstNode* node = new AstNode(type, a1);
  SetNodeAttributes(*node);
  return node;
}

inline AstNode* 
CSParser::AllocNode(AstTypes type, Value const& a1, Value const& a2)
{
  AstNode* node = new AstNode(type, a1, a2);
  SetNodeAttributes(*node);
  return node;
}

inline AstNode* 
CSParser::AllocNode(AstTypes type, Value const& a1, Value const& a2, Value const& a3)
{
  AstNode* node = new AstNode(type, a1, a2, a3);
  SetNodeAttributes(*node);
  return node;
}

inline AstNode* 
CSParser::AllocNode(AstTypes type, Value const& a1, Value const& a2, Value const& a3, Value const& a4)
{
  AstNode* node = new AstNode(type, a1, a2, a3, a4);
  SetNodeAttributes(*node);
  return node;
}

inline uint64 
CSParser::Elapsed() const
{
  return m_elapsed;
}

#endif // CSCRIPT_CS_PARSER_H
