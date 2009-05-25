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
#ifndef CSCRIPT_OPCODES_H
#define CSCRIPT_OPCODES_H

enum opcodes
{
  op_negate,
  op_preinc,
  op_predec,
  op_postinc,
  op_postdec,
  op_not,
  op_add,
  op_sub,
  op_mul,
  op_div,
  op_mod,
  op_logor,
  op_logand,
  op_bitor,
  op_bitxor,
  op_bitand,
  op_seq,
  op_sne,
  op_eq,
  op_ne,
  op_lt,
  op_le,
  op_gt,
  op_ge,
  op_assign,
  op_assadd,
  op_asssub,
  op_assmul,
  op_assdiv,
  op_assmod,
  op_function
};

String OpcodeToString(opcodes opcode);

#endif // CSCRIPT_OPCODES_H
