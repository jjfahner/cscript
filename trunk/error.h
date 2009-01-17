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
#ifndef CSCRIPT_ERROR_H
#define CSCRIPT_ERROR_H

//
// Notice information
//
struct Notice
{
  int         m_number;
  char const* m_notice;
};

//
// Errors
//
#define ERROR_EXPANSION_LIST  \
ERROR_EXPANSION(E0001,     1, "internal compiler error in file %s on line %d."); \
ERROR_EXPANSION(E0002,     2, "function '%s' already defined."); \
ERROR_EXPANSION(E0003,     3, "undefined variable '%s'."); \
ERROR_EXPANSION(E0004,     4, "not valid number of arguments in call to function '%s'."); \
ERROR_EXPANSION(E0005,     5, "function '%s' not found."); \
ERROR_EXPANSION(E0006,     6, "undeclared type '%s'."); \
ERROR_EXPANSION(E0007,     7, "not valid initializer for variable '%s'."); \
ERROR_EXPANSION(E0008,     8, "struct '%s' already declared."); \
ERROR_EXPANSION(E0010,    10, "switch statement contains more than one default case."); \
ERROR_EXPANSION(E0011,    11, "switch statement contains duplicate case value."); \
ERROR_EXPANSION(E0012,    12, "unrecoverable syntax error."); \
ERROR_EXPANSION(E0013,    13, "syntax error."); \
ERROR_EXPANSION(E0014,    14, "break statement not valid here."); \
ERROR_EXPANSION(E0015,    15, "continue statement not valid here."); \
ERROR_EXPANSION(E0016,    16, "class declaration not valid here"); \
ERROR_EXPANSION(E0017,    17, "function declaration not valid here");

//
// Expand errors to extern declarations
//
#define ERROR_EXPANSION(name,number,notice) \
  extern Notice name
ERROR_EXPANSION_LIST
#undef ERROR_EXPANSION

#endif // CSCRIPT_ERROR_H
