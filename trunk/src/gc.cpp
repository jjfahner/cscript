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
#include "cscript.h"
#include "gc.h"
#include "object.h"
#include "timer.h"
#include "object.h"
#include "value.h"

//
// Global object list
//
static GC::ObjectVec& GetObjects()
{
  static GC::ObjectVec g_objects;
  return g_objects;
}

/*static*/ size_t 
GC::ObjectCount()
{
  return GetObjects().size();
}

void 
GC::Pin(Object* obj)
{
//   if(!obj->m_complex)
//   {
    static_cast<SimpleObject*>(obj)->m_pinned = true;
//   }
}

void 
GC::Unpin(Object* obj)
{
//   if(!obj->m_complex)
//   {
    static_cast<SimpleObject*>(obj)->m_pinned = false;
//   }
}

void 
GC::Mark(ObjectVec& vec, Object* obj)
{
  if(obj->m_collect)
  {
    vec.push_back(obj);
  }
}

GC::Object::Object(bool complex)
{
  // Store reference to objects list
  static ObjectVec& g_objects = GetObjects();

  // Set collectable
  m_collect = true;
  m_pinned  = false;
  m_complex = complex;

  // Reserve space in blocks
  size_t current = g_objects.size();
  size_t reserve = (current + 1023) / 1024 * 1024;
  if(reserve > g_objects.capacity())
  {
    g_objects.reserve(reserve);
  }

  // Add object to object list
  g_objects.push_back(this);
}

//////////////////////////////////////////////////////////////////////////
/*

Collection is fairly simple: all objects start life as collectable,
and are inserted at the end of g_objects. Collection then proceeds
as follows:

1. Add root objects to the grey set.
2. For each object in the grey set:
a. Mark object non-collectable
b. Append all referred objects to the grey set
3. Repeat 2. until grey set is empty.
4. For each object in g_objects:
If collectable, delete it.
Else, copy it to the first available empty slot,
and mark it collectable for the next cycle.
5. Resize g_objects to fit the remaining set.

*/
//////////////////////////////////////////////////////////////////////////

/*static*/ GC::CollectInfo 
GC::Collect(ObjectVec const& roots)
{
  ObjectVec grey, next;
  ObjectVec::iterator it, ie;

  // Store reference to objects list
  static ObjectVec& g_objects = GetObjects();

  // Init collect information
  CollectInfo ci;
  memset(&ci, 0, sizeof(ci));
  ci.m_numRemaining = g_objects.size();

  // Start with the root objects
  grey = roots;

  // Run until no more objects are grey
  ci.m_markPhase = Timer::Ticks();
  while(grey.size())
  {
    // Next cycle
    ++ci.m_numCycles;

    // Mark reachable objects
    it = grey.begin();
    ie = grey.end();
    for(; it != ie; ++it)
    {
      Object* obj = *it;
      if(obj->m_collect)
      {
        // Set object as non-collectable
        obj->m_collect = false;

        // Mark subobjects
        if(obj->m_complex)
        {
          static_cast<ComplexObject*>(obj)->MarkObjects(next);
        }
      }
    }

    // Swap to next grey set
    grey.swap(next);
    next.clear();
  }

  // TODO Resurrect pinned object structures

  // Record time again
  ci.m_markPhase = Timer::Ticks() - ci.m_markPhase;
  ci.m_deletePhase = Timer::Ticks();

  // Now delete objects and compact array
  size_t pos = 0, ins = 0, len = g_objects.size();
  for(; pos < len; ++pos)
  {
    GC::Object*& obj = g_objects[pos];
    if(obj->m_collect && !obj->m_pinned)
    {
      obj->Delete();
    }
    else
    {
      obj->m_collect = true;
      g_objects[ins++] = obj;
    }
  }

  // Resize the array
  g_objects.resize(ins);

  // Record time for deletion
  ci.m_deletePhase = Timer::Ticks() - ci.m_deletePhase;

  // Record number collected
  ci.m_numCollected = ci.m_numRemaining - g_objects.size();
  ci.m_numRemaining = g_objects.size();

  // Done
  return ci;
}
