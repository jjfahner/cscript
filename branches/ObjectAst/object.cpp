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
#include "variable.h"
#include "function.h"

#include <typeinfo>
#include <set>
#include <deque>
#include <algorithm>

// Global object list
static Objects g_objects;

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

Object* 
Object::Clone(Object* into) const
{
  // Ensure there's an instance
  if(into == 0)
  {
    into = new Object();
  }

  // Copy members
  MemberMap::const_iterator it;
  for(it = m_members.begin(); it != m_members.end(); ++it)
  {
    into->m_members[it->first] = it->second->Clone();
  }

  // Done
  return into;
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

RValue& 
Object::RVal(Value const& key)
{
  RValue*& rval = m_members[key];
  if(rval == 0)
  {
    rval = new RWVariable();
  }
  return *rval;
}

LValue& 
Object::LVal(Value const& key)
{
  return RVal(key).LVal();
}

LValue& 
Object::Add(Value const& value)
{
  LValue& lval = LVal(m_members.size());
  lval = value;
  return lval;
}

void 
Object::Add(MemberMap const& source)
{
  MemberMap::const_iterator it;
  for(it = source.begin(); it != source.end(); ++it)
  {
    Add(it->second->GetValue());
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
    MemberMap::const_iterator it, ie;
    it = obj->Members().begin();
    ie = obj->Members().end();
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
