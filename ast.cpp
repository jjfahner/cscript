//////////////////////////////////////////////////////////////////////////
//
// This file is © 2007 JJ Fahner <jan-jaap@jan-jaap.net>
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
#include "codegen.h"
#include "machine.h"


//////////////////////////////////////////////////////////////////////////

Ast::Ast(AstTypes type) :
m_type (type),
m_refs (0)
{
}

Ast::Ast(AstTypes type, AstData const& a1) :
m_type(type),
m_a1(a1),
m_refs(0)
{
}

Ast::Ast(AstTypes type, AstData const& a1, AstData const& a2) :
m_type(type),
m_a1(a1),
m_a2(a2),
m_refs(0)
{
}

Ast::Ast(AstTypes type, AstData const& a1, AstData const& a2, AstData const& a3) :
m_type(type),
m_a1(a1),
m_a2(a2),
m_a3(a3),
m_refs(0)
{
}

Ast::Ast(AstTypes type, AstData const& a1, AstData const& a2, AstData const& a3, AstData const& a4) :
m_type(type),
m_a1(a1),
m_a2(a2),
m_a3(a3),
m_a4(a4),
m_refs(0)
{
}
