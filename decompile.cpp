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
#include "codegen.h"

inline Byte NextByte(Byte*& code)
{
  Byte b = *code;
  code += 1;
  return b;
}

inline Word NextWord(Byte*& code)
{
  Word b = *(Word*)code;
  code += 2;
  return b;
}

inline Quad NextQuad(Byte*& code)
{
  Quad b = *(Quad*)code;
  code += 4;
  return b;
}

inline Variant ReadLiteral(Byte* code)
{
  Variant v;
  v.Read(code);
  return v;
}

inline String Hex(Quad value)
{
  char buf[50];
  sprintf(buf, "%08X", value);
  return buf;
}

inline String Var(Byte* code, Quad offset)
{
  Variant v;
  v.Read(code + offset);
  String s = v.AsString();
  size_t pos;
  while((pos = s.find("\n")) != String::npos)
  {
    s.replace(pos, 1, "\\n");
  }
  return s;
}

void
CodeGenerator::Decompile(Byte* source, Quad len, std::ostream& ofs)
{
  // Instruction pointers
  #define ipb (NextByte(code))
  #define ipw (NextWord(code))
  #define ipq (NextQuad(code))

  // Decompile code
  Byte* code = source;
  Byte* end = code + len;
  while(code < end)
  {
    // Write line marker
    ofs << Hex(Quad(code - source)) << "  ";

    // Write instruction
    switch(ipb)
    {
    case op_halt:   ofs << "halt    "; break;
    case op_pushl:  ofs << "pushl   "; ofs << Var(source, ipq); break;
    case op_pushv:  ofs << "pushv   "; ipq; break;
    case op_pushi:  ofs << "pushi   "; break;
    case op_pop:    ofs << "pop     "; break;
    case op_jmp:    ofs << "jmp     "; ofs << Hex(ipq); break;
    case op_jz:     ofs << "jz      "; ofs << Hex(ipq); break;
    case op_jnz:    ofs << "jnz     "; ofs << Hex(ipq); break;
    case op_call:   ofs << "call    "; ofs << Hex(ipq); break;
    case op_ret:    ofs << "ret     "; break;
    case op_stackg: ofs << "stackg  "; ofs << ipq; break;
    case op_stacks: ofs << "stacks  "; ofs << ipq; break;
    case op_stackt: ofs << "stackt  "; ofs << (int)ipq; break;
    case op_store:  ofs << "store   "; ofs << (int)ipq; break;
    case op_inc:    ofs << "inc     "; break;
    case op_dec:    ofs << "dec     "; break;
    case op_add:    ofs << "add     "; break;
    case op_sub:    ofs << "sub     "; break;
    case op_mul:    ofs << "mul     "; break;
    case op_div:    ofs << "div     "; break;
    case op_mod:    ofs << "mod     "; break;    
    case op_logor:  ofs << "logor   "; break;
    case op_logand: ofs << "logand  "; break;
    case op_bitor:  ofs << "bitor   "; break;
    case op_bitxor: ofs << "bitxor  "; break;
    case op_bitand: ofs << "bitand  "; break;    
    case op_eq:     ofs << "eq      "; break;
    case op_ne:     ofs << "ne      "; break;
    case op_lt:     ofs << "lt      "; break;
    case op_le:     ofs << "le      "; break;
    case op_gt:     ofs << "gt      "; break;
    case op_ge:     ofs << "ge      "; break;
    case op_assign: ofs << "assign  "; break;
    case op_assadd: ofs << "assadd  "; break;
    case op_asssub: ofs << "asssub  "; break;
    case op_assmul: ofs << "assmul  "; break;
    case op_assdiv: ofs << "assdiv  "; break;
    case op_assmod: ofs << "assmod  "; break;
    default:        ofs << "unknown "; break;
    }
    ofs << "\n";
  }
}
