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
#include "scriptobj.h"

#include "datatype.h"
#include "function.h"
#include "enumerator.h"

#include <algorithm>

// Literals
static const String g_prototype("prototype");

//////////////////////////////////////////////////////////////////////////

class ScriptObject::MemberEnumerator : public Enumerator
{
  ScriptObject* m_obj;
  ScriptObject::MemberIterator m_cur;

public:

  MemberEnumerator(ScriptObject* object) :
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

// TODO Implement GetType

// ScriptObject::ScriptObject(DataType* dataType) :
// Object (dataType)
// {
// }
// 
Enumerator* 
ScriptObject::GetEnumerator()
{
  return new MemberEnumerator(this);
}

bool 
ScriptObject::TryGet(Value const& key, Value& value)
{
  if(key.Type() != Value::tString)
  {
    throw std::runtime_error(
      "Invalid key type for object");
  }

  // Find locally
  MemberIterator it = m_members.find(key);
  if(it != m_members.end())
  {
    value = it->second;
    return true;
  }

  // Find in prototype
  it = m_members.find(g_prototype);
  if(it != m_members.end())
  {
    return it->second->TryGet(key, value);
  }

  // Not found
  return false;
}

bool
ScriptObject::TrySet(Value const& key, Value const& value)
{
  if(key.Type() != Value::tString)
  {
    throw std::runtime_error(
      "Invalid key type for object");
  }

  // Find locally
  MemberIterator it = m_members.find(key);
  if(it != m_members.end())
  {
    it->second = value;
    return true;
  }

  // Find in prototype
  it = m_members.find(g_prototype);
  if(it != m_members.end())
  {
    return it->second->TrySet(key, value);
  }

  // Not found
  return false;
}

void 
ScriptObject::MarkObjects(GC::ObjectVec& grey)
{
  MemberMap::iterator it;
  for(it = m_members.begin(); it != m_members.end(); ++it)
  {
    if(GC::Object* o = it->second.GetGCObject()) {
      GC::Mark(grey, o);
    }
  }
}
