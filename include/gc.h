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
#ifndef CSCRIPT_GCOBJECT_H
#define CSCRIPT_GCOBJECT_H

#include <cscript.h>
#include <gcobj.h>
#include <vector>
#include <value.h>

class Stack;

class GC
{
public:

  //
  // Vector of collectable objects
  //
  typedef std::vector<GCObject*> GCObjectVec;

  //
  // Collect results
  //
  struct CollectInfo
  {
    uint64 m_markPhase;
    uint64 m_deletePhase;
    uint64 m_numCycles;
    uint64 m_numRemaining;
    uint64 m_numCollected;
  };

  //
  // Invoke collection cycle
  //
  static CollectInfo Collect(Stack const& stack);

  //
  // Current number of objects
  //
  static size_t ObjectCount();

  //
  // Pin an object to avoid it being collected
  //
  static void Pin(GCObject* obj);
  static void Unpin(GCObject* obj);

  //
  // Mark an object into a set
  //
  static void Mark(GCObjectVec& vec, GCObject* obj);
  static void Mark(GCObjectVec& vec, Value const& obj);

}; // class GC

inline void 
GC::Mark(GCObjectVec& vec, GCObject* obj)
{
  if(obj && obj->m_collect)
  {
    vec.push_back(obj);
  }
}

inline void 
GC::Mark(GCObjectVec& vec, Value const& val)
{
  if(GCObject* obj = val.GetGCObject())
  {
    Mark(vec, obj);
  }
}

#endif // CSCRIPT_GCOBJECT_H
