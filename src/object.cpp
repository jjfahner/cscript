//////////////////////////////////////////////////////////////////////////
//
// This file is � 2007 - 2009 JJ Fahner <jan-jaap@jan-jaap.net>
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
#include "object.h"
#include "datatype.h"
#include "variable.h"
#include "function.h"
#include "enumerator.h"

#include <algorithm>

// Literals
static const String g_prototype("prototype");

//////////////////////////////////////////////////////////////////////////

class ObjectEnumerator : public Enumerator
{
  typedef MemberMap::iterator Iterator;

  Object*  m_obj;
  Iterator m_cur;

public:

  ObjectEnumerator(Object* object) :
  m_obj (object)
  {
    // Initialize iterator
    Reset();
  }

  virtual void Reset()
  {
    // Set iterator to start
    m_cur = m_obj->m_members.begin();
  }

  virtual bool GetNext(Value& value)
  {
    // Check current position
    if(m_cur == m_obj->m_members.end())
    {
      return false;
    }

    // Retrieve value from iterator
    value = m_cur->second;

    // Advance to next position
    ++m_cur;

    // Succeeded
    return true;
  }

};

//////////////////////////////////////////////////////////////////////////

Object::Object(DataType* dataType) :
m_dataType (dataType ? dataType : ObjectType::Instance())
{
}

DataType* 
Object::GetType() const
{
  return m_dataType;
}

size_t 
Object::Count() const 
{
  // Return size
  return m_members.size();
}

Enumerator* 
Object::GetEnumerator()
{
  return new ObjectEnumerator(this);
}

bool 
Object::ContainsKey(String const& key, bool checkProto) const
{
  MemberMap::const_iterator it;

  // Find in own members
  if(m_members.count(key))
  {
    return true;
  }

  // Prototype checking
  if(!checkProto)
  {
    return false;
  }

  // Find prototype object
  it = m_members.find(g_prototype);
  if(it == m_members.end())
  {
    return false;
  }

  // Lookup in prototype
  return it->second->ContainsKey(key, true);
}

bool 
Object::Find(String const& key, RValue*& pValue, bool checkProto) const
{
  MemberMap::iterator it;

  // Initialize return value
  pValue = 0;
  
  // Find in own members
  it = m_members.find(key);
  if(it != m_members.end())
  {
    pValue = &it->second;
    return true;
  }

  // Prototype checking
  if(!checkProto)
  {
    return false;
  }
  
  // Find prototype
  it = m_members.find(g_prototype);
  if(it == m_members.end())
  {
    return false;
  }
  
  // Lookup in prototype
  if(!it->second->Find(key, pValue, true))
  {
    return false;
  }

  // Clone value
  pValue = &m_members.insert(std::make_pair(key, pValue->GetValue())).first->second;

  // Done
  return true;
}

Value const& 
Object::Get(Value const& key)
{
  if(key.Type() != Value::tString)
  {
    throw std::runtime_error(
      "Invalid key type for object");
  }

  RValue* pValue;
  if(Find(key, pValue, true))
  {
    return *pValue;
  }
  
  return m_members[key];
}

bool 
Object::TryGet(Value const& key, Value const*& value)
{
  if(key.Type() != Value::tString)
  {
    throw std::runtime_error(
      "Invalid key type for object");
  }

  RValue* pValue;
  if(Find(key, pValue, true))
  {
    value = &pValue->GetValue();
    return true;
  }

  return false;
}

Value const&
Object::Set(Value const& key, Value const& value)
{
  if(key.Type() != Value::tString)
  {
    throw std::runtime_error(
      "Invalid key type for object");
  }
  
  MemberVariable& result = m_members[key];
  result.SetValue(value);

  return result;
}

bool
Object::TrySet(Value const& key, Value const& value)
{
  RValue* pValue;
  if(Find(key, pValue, true))
  {
    //pValue->GetLValue() = value;
    return true;
  }

  return false;
}

void 
Object::Unset(Value const& key)
{
  m_members.erase(key);
}

void 
Object::MarkObjects(GC::ObjectVec& grey)
{
  MemberMap::iterator it;
  for(it = m_members.begin(); it != m_members.end(); ++it)
  {
    if(GC::Object* o = it->second.GetGCObject()) {
      GC::Mark(grey, o);
    }
  }
}
