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
#include "cscript.h"
#include "opcodes.h"

String 
OpcodeToString(opcodes opcode)
{
  switch(opcode)
  {
  case op_negate: return "!";
  case op_preinc: return "++";
  case op_predec: return "--";
  case op_postinc: return "++";
  case op_postdec: return "--";
  case op_not: return "!";
  case op_add: return "+";
  case op_sub: return "-";
  case op_mul: return "*";
  case op_div: return "/";
  case op_mod: return "%";
  case op_logor: return "||";
  case op_logand: return "&&";
  case op_bitor: return "|";
  case op_bitxor: return "^";
  case op_bitand: return "&";
  case op_seq: return "===";
  case op_sne: return "!==";
  case op_eq: return "==";
  case op_ne: return "!=";
  case op_lt: return "<";
  case op_le: return "<=";
  case op_gt: return ">";
  case op_ge: return ">=";
  case op_assign: return "=";
  case op_assadd: return "+=";
  case op_asssub: return "-=";
  case op_assmul: return "*=";
  case op_assdiv: return "/=";
  case op_assmod: return "%=";
  default: throw std::runtime_error("Invalid operator");
  }
}
