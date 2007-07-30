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
#ifndef CSCRIPT_TOKENS_H
#define CSCRIPT_TOKENS_H

#pragma once

//
// Include lemon-generated symbols
//
#include "cscript.h"

//
// Types
//
#include "types.h"

//
// Lexer-specific tokens
//
#define TOK_COMMENT       80
#define TOK_NEWLINE       81
#define TOK_WHITESPACE    82

//
// Define additional instruction values
//
#define TOK_LVALUE        100
#define TOK_RVALUE        101
#define TOK_PREINC        102
#define TOK_PRESUB        103
#define TOK_POSTINC       104
#define TOK_POSTSUB       105
#define TOK_HALT          106
#define TOK_VARINIT       107
#define TOK_POP           108
#define TOK_UNDEF         109
#define TOK_JMP           110
#define TOK_JZ            111
#define TOK_JNZ           112
#define TOK_INDEX         113
#define TOK_CALL          114
#define TOK_CALLN         115
#define TOK_RET           116
#define TOK_ARRAY         117

//
// Define token structure
//

struct Token
{
  Char const*  m_text;
  size_t          m_size;
  int             m_type;

  operator String () const
  {
    return String(m_text, m_size);
  }
};



#endif // #ifndef CSCRIPT_TOKENS_H
