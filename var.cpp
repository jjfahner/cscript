#include "var.h"

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

  // Binary copy
  memcpy(this, &rhs, sizeof(Variant));

  // Copy dynamic types
  switch(m_type)
  {
  case stString:  m_string = new StringType(*m_string); break;
  case stMap:     m_map = new MapType(*m_map);          break;
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
  case stString: delete m_string; break;
  case stMap:     delete m_map;   break;
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
  case stMap:     MakeMap();    break;
  default: throw std::runtime_error("Invalid conversion type");
  }
}

void 
Variant::MakeBool()
{
  MakeInt();
  m_bool = m_int ? true : false;
  m_type = stBool;
}

void 
Variant::MakeInt()
{
  IntType value = 0;
  switch(m_type)
  {
  case stBool:    value = m_bool ? 1 : 0; break;
  case stString:  value = _wtoi64(m_string->c_str()); break;
  default:        throw std::runtime_error("Invalid conversion");
  }
  Clear();
  m_int  = value;
  m_type = stInt;
}

void 
Variant::MakeString()
{
  wchar_t buf[50];
  switch(m_type)
  {
  case stBool:  wcscpy(buf, m_bool ? L"true" : L"false"); break;
  case stInt:   _i64tow(m_int, buf, 10); break;
  case stRef:   wcscpy(buf, m_ref->AsString().c_str()); break;
  default:      throw std::runtime_error("Invalid conversion");
  }
  Clear();
  m_string = new std::wstring(buf);
  m_type   = stString;
}

void 
Variant::MakeMap()
{
  throw std::runtime_error("Invalid conversion");
}

int 
Variant::Compare(Variant const& rhs, bool exact) const
{
  // Comparing to ref
  if(m_type == stRef)
  {
    return m_ref->Compare(rhs, exact);
  }
 
  // Check type for exact match
  if(exact && m_type != rhs.m_type)
  {
    return false;
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
    if(m_bool == rbool) return 0;
    if(m_bool) return 1;
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
    int diff = wcscmp(m_string->c_str(), rhs.AsString().c_str());
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
  case stRef   : *m_ref += value; break;
  case stBool  : m_bool = m_bool ? true : value.AsBool(); break;
  case stInt   : m_int += value.AsInt(); break;
  case stString: *m_string += value.AsString(); break;
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

