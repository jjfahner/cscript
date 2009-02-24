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
#include "variable.h"
#include "function.h"

#include <algorithm>
#include <typeinfo>

// Global object list
static Objects g_objects;

// Literals
static const Value g_prototype("prototype");

//////////////////////////////////////////////////////////////////////////

/*static*/ Objects const& 
Object::GetObjects()
{
  return g_objects;
}

//////////////////////////////////////////////////////////////////////////

Object::Object() 
{
  g_objects.insert(this);
}

Object::~Object()
{
  MemberMap::iterator it = m_members.begin();
  for(; it != m_members.end(); ++it)
  {
    delete it->second;
  }
}

String 
Object::GetTypeName() const
{
  String type = typeid(*this).name();
  if(type.substr(0, 6) == "class ")
  {
    type = type.substr(6);
  }
  return type;
}

bool 
Object::ContainsKey(Value const& key, bool checkProto)
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
  return (*it->second)->ContainsKey(key, true);
}

bool 
Object::Find(Value const& key, RValue*& pValue, bool checkProto)
{
  MemberMap::const_iterator it;

  // Initialize return value
  pValue = 0;
  
  // Find in own members
  it = m_members.find(key);
  if(it != m_members.end())
  {
    pValue = it->second;
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
  if(!(*it->second)->Find(key, pValue, true))
  {
    return false;
  }

  // Clone value
  pValue = new RWVariable(*pValue);
  m_members[key] = pValue;

  // Done
  return true;
}

RValue& 
Object::GetRValue(Value const& key)
{
  RValue* pValue;
  if(!Find(key, pValue))
  {
    pValue = new RWVariable();
    m_members[key] = pValue;
  }

  return *pValue;
}

LValue& 
Object::GetLValue(Value const& key)
{
  return GetRValue(key).GetLValue();
}

RValue& 
Object::Add(Value const& value)
{
  return Add(m_members.size(), value).GetLValue();
}

RValue& 
Object::Add(Value const& key, Value const& value)
{
  return Add(key, new RWVariable(value));
}

RValue& 
Object::Add(Value const& key, RValue* value)
{
  // Insert new variable
  typedef std::pair<MemberIterator, bool> InsertResult;
  InsertResult const& res = m_members.insert(std::make_pair(key, value));

  // Check insert result
  if(!res.second)
  {
    delete value;
    throw std::runtime_error("Variable already declared");
  }

  // Done
  return *value;
}

void 
Object::AddMembers(Object* source)
{
  // Copy members
  MemberIterator it = source->Begin();
  MemberIterator ie = source->End();
  for(; it != ie; ++it)
  {
    Add(it->second->GetValue());
  }
}

void 
Object::Remove(Value const& key)
{
  // Erase from members
  MemberIterator it = m_members.find(key);
  if(it != m_members.end())
  {
    delete it->second;
    m_members.erase(it);
  }
}

//////////////////////////////////////////////////////////////////////////
//
// Helpers for garbage collector
//

class ObjectDeleter
{
public:

  void operator () (Object* ptr) const
  {
    try 
    {
      delete ptr;
    }
    catch(...) 
    {
      // TODO
    }
  }

};

template <typename T, typename U>
void erase_from(T& cont, U from, U const& to) {
  for(; from != to; ++from) {
    cont.erase(*from);
  }
}

//////////////////////////////////////////////////////////////////////////
/*

The collection algorithm follows the classic tricolor method, with one
important modification: the traditional algorithm specifies that every
object is part of one set, whereas in this implementation, objects that
are reachable are temporarily in both the white and the grey set.

Invariants:

- White objects may be candidates for collection.
- Grey objects are reachable and may point to white objects.
- Black objects are reachable and may not point to white objects.
- Any object is either grey/white, or it is black.

Algorithm:

1.  Consider all objects white.
2.  Consider root objects grey.
3.  Let the black set be empty.
3.  For each grey object:
4.    Remove the grey object from the white set.
5.    Insert the grey object into the black set.
6.    Add objects referred to by the now black object to the grey set.
7.  Move black objects to the global list.
8.  Delete white objects.

*/
//////////////////////////////////////////////////////////////////////////

void 
MarkObjects(Objects& white, Objects& grey, Objects& black)
{
  // Walk stack until empty
  while(grey.size())
  {
    // Remove entry from grey list
    Object* obj = *grey.begin();
    grey.erase(grey.begin());

    // Avoid repeating cycles
    if(black.count(obj))
    {
      continue;
    }

    // Move from white to black
    white.erase(obj);
    black.insert(obj);

    // Walk object members
    Object::MemberIterator it = obj->Begin();
    Object::MemberIterator ie = obj->End();
    for(; it != ie; ++it)
    {
      if(it->first.Type() == Value::tObject)
      {
        grey.insert(it->first.GetObject());
      }
      if(it->second->Type() == Value::tObject)
      {
        grey.insert(it->second->GetObject());
      }
    }
  }
}

/*static*/ void 
Object::Collect(Objects grey)
{
  Objects black;
  Objects white;
  Objects final;
  
  // Move globals into white set
  white.swap(g_objects);

  // Move reachable objects into black set
  MarkObjects(white, grey, black);

  // Move black list to global list
  black.swap(g_objects);

  // Delete white objects
  std::for_each(white.begin(), white.end(), ObjectDeleter());
}