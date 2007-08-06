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

/*static*/ const Variant Variant::Null;
/*static*/ const Variant Variant::True(true);
/*static*/ const Variant Variant::False(false);


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
  case stResource: m_res = m_res->Clone();         break;
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
  case stString:   delete m_str; break;
  case stAssoc:    delete m_map; break;
  case stResource: delete m_res; break;
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
  Char buf[50];
  switch(m_type)
  {
  case stNull:  buf[0] = 0; break;
  case stBool:  strcpy(buf, m_bln ? "true" : "false"); break;
  case stInt:   sprintf(buf, "%d", (int)m_int); break;
  default:      throw std::runtime_error("Invalid conversion");
  }
  Clear();
  m_str = new String(buf);
  m_type   = stString;
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

  // Cannot compare
  throw std::runtime_error("Cannot compare types");
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
  (*m_map)[m_map->size()] = ref;
}

void 
Variant::Read(unsigned char* address)
{
  Clear();

  // Read type
  unsigned char type = *address++;

  // Empty
  if(type == stNull)
  {
    m_type = stNull;
    return;
  }

  // Boolean
  if(type == stBool)
  {
    m_type = stBool;
    m_bln = *address ? true : false;
    return;
  }

  // Integer
  if(type == stInt)
  {
    m_type = stInt;
    m_int  = *(IntType*)address;
    return;
  }

  // String
  if(type == stString)
  {
    m_type = stString;
    m_str = new StringType((Char const*)address);
    return;
  }

  // Not supported
  throw std::runtime_error("Invalid subtype");
}

size_t 
Variant::WriteLength() const
{
  switch(m_type)
  {
  case stNull:    return 1;
  case stBool:    return 2;
  case stInt:     return 1 + sizeof(IntType);
  case stString:  return 1 + m_str->length() + 1;
  default: throw std::runtime_error("Invalid subtype");
  }
}

void
Variant::Write(unsigned char* address) const
{
  // Write type
  *address++ = m_type;

  // Null writes no data
  if(m_type == stNull)
  {
    return;
  }

  // Boolean
  if(m_type == stBool)
  {
    *address = m_bln ? 1 : 0;
    return;
  }

  // Integer
  if(m_type == stInt)
  {
    *((IntType*)address) = m_int;
    return;
  }

  // String
  if(m_type == stString)
  {
    strcpy((Char*)address, m_str->c_str());
    return;
  }

  // Not supported
  throw std::runtime_error("Invalid subtype");
}
