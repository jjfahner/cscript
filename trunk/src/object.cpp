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
  typedef Object::ValueIterator Iterator;

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
    m_cur = m_obj->ValueBegin();
  }

  virtual bool GetNext(Value& value)
  {
    // Check current position
    if(m_cur == m_obj->ValueEnd())
    {
      return false;
    }

    // Retrieve value from iterator
    value = *m_cur;

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
Object::GetDataType() const
{
  return m_dataType;
}

size_t 
Object::Count() const 
{
  // Update members
  const_cast<Object*>(this)->UpdateMembers();

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

  // Update members
  const_cast<Object*>(this)->UpdateMembers();

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

  // Update members
  const_cast<Object*>(this)->UpdateMembers();

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

RValue& 
Object::GetAt(Value const& index)
{
  RValue* pValue;
  if(Find(index, pValue, true))
  {
    return *pValue;
  }
  return m_members[index];
}

RValue& 
Object::GetRValue(String const& key)
{
  // Find member
  RValue* pValue;
  if(!Find(key, pValue))
  {
    // Add member
    pValue = &m_members[key];
  }

  // Done
  return *pValue;
}

LValue& 
Object::GetLValue(String const& key)
{
  return GetRValue(key).GetLValue();
}

RValue& 
Object::Add(Value const& value)
{
  // TODO this is really, really bad. Should be replaced
  // (or simply removed) as soon as statement_seq and its
  // ilk have been moved to an array-like object
  char buf[10];
  sprintf(buf, "%08d", m_members.size());
  return Add(buf, value);
}

RValue& 
Object::Add(String const& key, Value const& value)
{
  // Update members first
  //UpdateMembers();

  // Insert new variable
  typedef std::pair<MemberIterator, bool> InsertResult;
  InsertResult const& res = m_members.insert(std::make_pair(key, value));

  // Check insert result
  if(!res.second)
  {
    throw std::runtime_error("Variable already declared");
  }

  // Done
  return res.first->second;
}

RValue& 
Object::Add(String const& key, RValue* value)
{
  throw std::runtime_error("Custom object members are currently not implemented");
}

void 
Object::AddMembers(Object* source)
{
  // Update members
  UpdateMembers();

  // Copy members
  MemberIterator it = source->Begin();
  MemberIterator ie = source->End();
  for(; it != ie; ++it)
  {
    Add(it->second.GetValue());
  }
}

void 
Object::Remove(String const& key)
{
  // Update members
  UpdateMembers();

  // Erase from members
  MemberIterator it = m_members.find(key);
  if(it != m_members.end())
  {
    m_members.erase(it);
  }
}

void 
Object::MarkObjects(GC::ObjectVec& grey)
{
  // Iterate over members
  ValueIterator mi, me;
  mi = ValueBegin();
  me = ValueEnd();
  for(; mi != me; ++mi)
  {
    if(GC::Object* o = mi->GetGCObject())
    {
      GC::Mark(grey, o);
    }
  }
}
