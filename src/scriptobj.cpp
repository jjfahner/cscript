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
    Reset();
  }

  virtual Object* GetSource() const
  {
    return m_obj;
  }

  virtual void Reset()
  {
    m_cur = m_obj->m_members.begin();
  }

  virtual bool GetNext(Value& value)
  {
    if(m_cur == m_obj->m_members.end())
    {
      return false;
    }

    value = m_cur->second;

    ++m_cur;

    return true;
  }

  virtual bool GetNext(Value& key, Value& value)
  {
    if(m_cur == m_obj->m_members.end())
    {
      return false;
    }
    
    key   = Identifier::Lookup(m_cur->first);
    value = m_cur->second;

    ++m_cur;

    return true;
  }

};

//////////////////////////////////////////////////////////////////////////

DataType* 
ScriptObject::GetType()
{
  return ObjectType::Instance();
}

Enumerator* 
ScriptObject::GetEnumerator()
{
  return new MemberEnumerator(this);
}

bool 
ScriptObject::TryGet(Value const& vKey, Value& value)
{
  Identifier key(vKey);

  // Find locally
  MemberIterator it = m_members.find(key);
  if(it != m_members.end())
  {
    value = it->second;
    return true;
  }

  // Find in prototype
  it = m_members.find(Identifier::Lookup(g_prototype));
  if(it != m_members.end())
  {
    return it->second->TryGet(vKey, value);
  }

  // Not found
  return false;
}

bool
ScriptObject::TrySet(Value const& vKey, Value const& value)
{
  Identifier key(vKey);

  // Find locally
  MemberIterator it = m_members.find(key);
  if(it != m_members.end())
  {
    it->second = value;
    return true;
  }

  // Find in prototype
  it = m_members.find(Identifier::Lookup(g_prototype));
  if(it != m_members.end())
  {
    // On-demand created
    m_members[key] = value;
  }

  // Not found
  return false;
}

void 
ScriptObject::MarkObjects(GCObjectVec& grey)
{
  MemberMap::iterator it;
  for(it = m_members.begin(); it != m_members.end(); ++it)
  {
    GC::Mark(grey, it->second);
  }
}
