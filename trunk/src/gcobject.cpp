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
#include <cscript.h>
#include <gcobject.h>
#include <variable.h>
#include <object.h>

// Global object list
static ObjectVec g_objects;

//////////////////////////////////////////////////////////////////////////

/*static*/ size_t 
GCObject::ObjectCount()
{
  return g_objects.size();
}

GCObject::GCObject()
{
  // Set collectible
  m_collect = true;

  // Reserve space efficiently
  size_t reserve = (g_objects.size() + 1023) / 1024 * 1024;
  g_objects.reserve(reserve);

  // Add object to object list
  g_objects.push_back(this);
}

//////////////////////////////////////////////////////////////////////////
/*

Collection is fairly simple: all objects start life as collectible,
and are inserted at the end of g_objects. Collection then proceeds
as follows:

1. Add root objects to the grey set.
2. For each object in the grey set:
a. Mark object non-collectible
b. Append all referred objects to the grey set
3. Repeat 2. until grey set is empty.
4. For each object in g_objects:
If collectible, delete it.
Else, copy it to the first available empty slot,
and mark it collectible for the next cycle.
5. Resize g_objects to fit the remaining set.

*/
//////////////////////////////////////////////////////////////////////////

/*static*/ void
GCObject::Collect(ObjectVec const& roots)
{
  ObjectVec grey, next;
  ObjectVec::iterator bit, bie, bci;

  // Copy objects into grey set
  grey = roots;

  // Mark objects
  while(grey.size())
  {
    bit = grey.begin();
    bie = grey.end();

    for(; bit != bie; ++bit)
    {
      // Set object as non-collectible
      GCObject* gcObj = *bit;
      gcObj->m_collect = false;

      // Complex object
      Object* obj = dynamic_cast<Object*>(gcObj);
      if(obj == 0)
      {
        continue;
      }

      // Iterate over members
      Object::MemberIterator mi, me;
      mi = obj->Begin();
      me = obj->End();
      for(; mi != me; ++mi)
      {
        // Check key content
        if(GCObject* o = mi->first.GetGCObject())
        {
          if(o->m_collect)
          {
            next.push_back(o);
          }
        }

        // Check value content
        if(GCObject* o = mi->second->GetGCObject())
        {
          if(o->m_collect)
          {
            next.push_back(o);
          }
        }
      }
    }

    // Swap to next grey set
    grey.swap(next);
    next.clear();
  }

  // Now delete objects
  size_t pos = 0, ins = 0, len = g_objects.size();
  for(; pos < len; ++pos)
  {
    GCObject*& obj = g_objects[pos];
    if(obj->m_collect)
    {
      delete obj;
    }
    else
    {
      obj->m_collect = true;
      g_objects[ins++] = obj;
    }
  }

  // Resize the array
  g_objects.resize(ins);
}
