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
#include "opcodes.h"
#include "native.h"

CodeGenerator::CodeGenerator() :
m_code    (0),
m_size    (0),
m_used    (0)
{
}

CodeGenerator::~CodeGenerator()
{
  if(m_code)
  {
    free(m_code);
  }
}

void 
CodeGenerator::Write()
{
  std::ofstream ofs("out.csb", std::ios::binary);
  ofs.write((char*)m_code, m_used);
  ofs.close();
}

void 
CodeGenerator::Reserve(Quad size)
{
  if(m_size - m_used < size)
  {
    m_code = (Byte*)realloc(m_code, size * 2);
    m_size = size * 2;
  }
}

void 
CodeGenerator::PushData(Byte* data, Quad size)
{
  Reserve(m_used + size);
  memmove(m_code + m_used, data, size);
  m_used += size;
}

void 
CodeGenerator::PushByte(Byte data)
{
  Reserve(m_used + sizeof(data));
  *(Byte*)(m_code + m_used) = data;
  m_used += sizeof(Byte);
}

void 
CodeGenerator::PushWord(Word data)
{
  Reserve(m_used + sizeof(data));
  *(Word*)(m_code + m_used) = data;
  m_used += sizeof(Word);
}

void 
CodeGenerator::PushQuad(Quad data)
{
  Reserve(m_used + sizeof(data));
  *(Quad*)(m_code + m_used) = data;
  m_used += sizeof(Quad);
}

void 
CodeGenerator::FillQuad(Quad offset, Quad data)
{
  *(Quad*)(m_code + offset) = data;
}

Quad 
CodeGenerator::PushPatch()
{
  Quad pos = m_used;
  PushQuad(0);
  return pos;
}

void
CodeGenerator::FixPatch(Quad pos)
{
  *(Quad*)(m_code + pos) = m_used;
}

void 
CodeGenerator::PushLiteral(Variant const& value)
{
  m_literals[value].push_back(m_used);
  PushQuad(0);
}
