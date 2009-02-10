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
#include "ast.h"
#include "file.h"
#include "lexer.h"
#include "io.h"
#include "variable.h"
/*
Ast::Ast(AstTypes type) :
m_type(type)
{
}

Ast::Ast(AstTypes type, Value const& a1) :
m_type(type),
["a1"]  (a1)
{
  (*this)m_type = type;
  (*this)["a1"]   = a1;
}

Ast::Ast(AstTypes type, Value const& a1, Value const& a2) :
m_type(type),
["a1"](a1),
["a2"](a2)
{
  (*this)m_type = type;
  (*this)["a1"]   = a1;
  (*this)["a2"]   = a2;
}

Ast::Ast(AstTypes type, Value const& a1, Value const& a2, Value const& a3) :
m_type(type),
["a1"](a1),
["a2"](a2),
["a3"](a3)
{
  (*this)m_type = type;
  (*this)["a1"]   = a1;
  (*this)["a2"]   = a2;
  (*this)["a3"]   = a3;
}

Ast::Ast(AstTypes type, Value const& a1, Value const& a2, Value const& a3, Value const& a4) :
m_type(type),
["a1"](a1),
["a2"](a2),
["a3"](a3),
["a4"](a4)
{
  (*this)m_type = type;
  (*this)["a1"]   = a1;
  (*this)["a2"]   = a2;
  (*this)["a3"]   = a3;
  (*this)["a4"]   = a4;
}
*/