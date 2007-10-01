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
#include "var.h"
#include "class.h"

/*static*/ const Variant Variant::Null;
/*static*/ const Variant Variant::True(true);
/*static*/ const Variant Variant::False(false);

Variant::Variant(Instance* inst) :
m_type (stNull),
m_inst (0)
{
  if(m_inst = inst)
  {
    m_type = stInstance;
    ++m_inst->m_refs;
  }
}

Variant const& 
Variant::operator = (Variant const& rhs)
{
  // Assignment to self
  if(&rhs == this)
  {
    return *this;
  }

  // Clear self
  Clear();

  // Copy type and data
  m_type = rhs.m_type;
  memcpy(&m_int, &rhs.m_int, sizeof(m_int));

  // Copy dynamic types
  switch(m_type)
  {
  case stString:   m_str = new StringType(*m_str); break;
  case stAssoc:    m_map = new AssocType(*m_map);  break;
  case stInstance: m_inst->m_refs++;               break;
  case stResource: m_res->m_refs++;                break;
  }

  // Done
  return *this;
}

void 
Variant::Clear()
{
  // Destroy complex types
  switch(m_type)
  {
  case stString:   
    delete m_str; 
    break;
  case stAssoc:    
    delete m_map; 
    break;
  case stInstance:
    if(--m_res->m_refs == 0) delete m_inst;
    break;
  case stResource: 
    if(--m_res->m_refs == 0) delete m_res; 
    break;
  }

  // Clear data and type
  m_type = stNull;
  m_int  = 0;
}

void 
Variant::SetType(SubTypes type)
{
  if(type == m_type)
  {
    return;
  }
  switch(type)
  {
  case stNull:    Clear();      break;
  case stBool:    MakeBool();   break;
  case stInt:     MakeInt();    break;
  case stString:  MakeString(); break;
  case stAssoc:   MakeMap();    break;
  default: throw std::runtime_error("Invalid conversion type");
  }
}

void 
Variant::MakeBool()
{
  if(m_type != stInt)
  {
    MakeInt();
  }
  m_bln = m_int ? true : false;
  m_type = stBool;
}

void 
Variant::MakeInt()
{
  IntType value = 0;
  switch(m_type)
  {
  case stBool:    value = m_bln ? 1 : 0; break;
  case stString:  value = atoi(m_str->c_str()); break;
  default:        throw std::runtime_error("Invalid conversion");
  }
  Clear();
  m_int  = value;
  m_type = stInt;
}

void 
Variant::MakeString()
{
  switch(m_type)
  {
  case stNull:  
    *this = "";
    return;
  
  case stBool:  
    *this = m_bln ? "true" : "false"; 
    return;

  case stInt:   
    {
      Char buf[50];
      sprintf(buf, "%d", (int)m_int); 
      *this = buf;
      return;
    }

  case stAssoc:
    {
      AssocType::const_iterator it, ie;
      it = GetMap().begin();
      ie = GetMap().end();
      String sep, val;
      val = "[";
      for(; it != ie; ++it)
      {
        val += sep;
        sep = ",";
        val += it->second->AsString();
      }
      val += "]";
      *this = val;
      return;
    }

  }
  throw std::runtime_error("Invalid conversion");
}

void 
Variant::MakeMap()
{
  if(m_type != stAssoc)
  {
    Clear();
    m_map  = new AssocType;
    m_type = stAssoc;
  }
}

int 
Variant::Compare(Variant const& rhs, bool exact) const
{
  // Check type for exact match
  if(exact && m_type != rhs.m_type)
  {
    return m_type - rhs.m_type;
  }

  // Nullness
  if(m_type == stNull && rhs.m_type == stNull)
  {
    return 0;
  }
  if(m_type == stNull)
  {
    return -1;
  }
  if(rhs.m_type == stNull)
  {
    return 1;
  }

  // Boolean
  if(m_type == stBool)
  {
    BoolType rbool = rhs.AsBool();
    if(m_bln == rbool) return 0;
    if(m_bln) return 1;
    return -1;
  }

  // Integer
  if(m_type == stInt)
  {
    IntType diff = m_int - rhs.AsInt();
    if(diff < 0) return -1;
    if(diff > 0) return  1;
    return 0;
  }

  // String
  if(m_type == stString)
  {
    int diff = strcmp(m_str->c_str(), rhs.AsString().c_str());
    if(diff < 0) return -1;
    if(diff > 0) return  1;
    return 0;
  }

  // TODO compare instances?

  // Cannot compare
  return -1;
  //throw std::runtime_error("Cannot compare types");
}

Variant const& 
Variant::operator += (Variant const& value)
{
  switch(m_type)
  {
  case stNull  : *this = value; break;
  case stBool  : m_bln = m_bln ? true : value.AsBool(); break;
  case stInt   : m_int += value.AsInt(); break;
  case stString: *m_str += value.AsString(); break;
  default: throw std::runtime_error("Invalid subtype");
  }
  return *this;
}

Variant const& 
Variant::operator -= (Variant const& value)
{
  switch(m_type)
  {
  case stInt: m_int -= value.AsInt(); break;
  default   : throw std::runtime_error("Invalid type for operation");
  }
  return *this;
}

Variant const& 
Variant::operator *= (Variant const& value)
{
  switch(m_type)
  {
  case stInt: m_int *= value.AsInt(); break;
  default   : throw std::runtime_error("Invalid type for operation");
  }
  return *this;
}

Variant const& 
Variant::operator /= (Variant const& value)
{
  switch(m_type)
  {
  case stInt: m_int /= value.AsInt(); break;
  default   : throw std::runtime_error("Invalid type for operation");
  }
  return *this;
}

Variant const& 
Variant::operator %= (Variant const& value)
{
  switch(m_type)
  {
  case stInt: m_int %= value.AsInt(); break;
  default   : throw std::runtime_error("Invalid type for operation");
  }
  return *this;
}

void 
Variant::Append(VariantRef const& ref)
{
  if(m_type != stAssoc)
  {
    MakeMap();
  }
  size_t key = m_map->size();
  (*m_map)[Variant(key)] = ref;
}
