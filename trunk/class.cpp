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
#include "class.h"
#include "codegen.h"

Class::Class(String const& name) :
m_name (name),
m_vars (0)
{
}

Class::~Class()
{

}

Quad 
Class::Read(Byte const* code)
{
  Byte const* start = code;

  // Read class name
  m_name = (char const*)code;
  code += m_name.length() + 1;

  // Read variable count
  m_vars = *(Quad*)code;
  code += 4;

  // Read functions
  for(;;)
  {
    // End of table
    if(*code == 0)
    {
      ++code;
      break;
    }

    // Function name
    String name((char const*)code);
    code += name.length() + 1;
    
    // Params and offset
    Quad params = *(Quad*)code;
    code += 4;
    Quad offset = *(Quad*)code;
    code += 4;

    // Insert into table
    AddFun(name, params, offset);
  }

  // Done
  return code - start;
}

void
Class::Write(CodeGenerator& cg)
{
  // Write name
  cg.PushData((Byte*)m_name.c_str(), 
              m_name.length() + 1);
  cg.PushQuad(m_vars);

  // Write members
  MemberFuns::iterator it, ie;
  it = m_funs.begin();
  ie = m_funs.end();
  for(; it != ie; ++it)
  {
    cg.PushData((Byte*)it->first.c_str(), 
                 it->first.length() + 1);
    cg.PushQuad(it->second.m_params);
    cg.PushQuad(it->second.m_offset);
  }

  // End of table
  cg.PushByte(0);
}

void 
Class::AddFun(String const& name, Quad params, Quad offset)
{
  MemFunInfo mf;
  mf.m_params = params;
  mf.m_offset = offset;
  m_funs[name] = mf;
}

void 
Class::AddVar()
{
  ++m_vars;
}

Instance* 
Class::CreateInstance()
{
  return new Instance(this);
}

Quad 
Class::Lookup(String const& name)
{
  MemberFuns::iterator it = m_funs.find(name);
  if(it == m_funs.end())
  {
    throw std::runtime_error("Class '" + m_name + "' does not support a method '" + name + "'");
  }
  return it->second.m_offset;
}

//////////////////////////////////////////////////////////////////////////
//
// Instance implementation
//

Instance::Instance(Class* c) : 
m_class (c)
{
  m_vars.resize(c->GetVars());
}

Instance::~Instance()
{
}

Quad 
Instance::Lookup(String const& name)
{
  return m_class->Lookup(name);
}

VariantRef const& 
Instance::GetVar(Quad index)
{
  VariantRef& ref = m_vars[index];
  if(ref.Empty())
  {
    ref = VariantRef(new Variant);
  }
  return ref;
}
