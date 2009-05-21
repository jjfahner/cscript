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
#include <vector>

class GC
{
public:

  //
  // Forward declare Object class
  //
  class Object;

  //
  // Vector of collectable objects
  //
  typedef std::vector<Object*> ObjectVec;

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
  static CollectInfo Collect(ObjectVec const& roots);

  //
  // Current number of objects
  //
  static size_t ObjectCount();

  //
  // Pin an object to avoid it being collected
  //
  static void Pin(Object* obj);
  static void Unpin(Object* obj);

  //////////////////////////////////////////////////////////////////////////
  //
  // Base class for collectable objects
  //

  class Object
  {
  protected:

    //
    // Virtual destructor
    //
    virtual ~Object() {}

    //
    // Virtual destruction
    //
    virtual void Delete() { 
      delete this; 
    }

    //
    // Assignment
    //
    Object& operator = (Object&) { return *this; }

  private:

    //
    // Friends
    //
    friend class GC;
    friend class SimpleObject;
    friend class ComplexObject;

    //
    // Construction
    //
    Object(bool complex);

    //
    // Copy construction not allowed
    //
    Object(Object&);

    //
    // Members
    //
    unsigned m_complex : 1;
    unsigned m_collect : 1;
    unsigned m_pinned  : 1;

  };

  //////////////////////////////////////////////////////////////////////////
  //
  // Simple 
  //

  class SimpleObject : public Object
  {
  protected:

    //
    // Construction
    //
    SimpleObject() : Object(false) {}

    //
    // Copy construction
    //
    SimpleObject(SimpleObject const&) : Object(false) {}

  private:

    //
    // Friends
    //
    friend class GC;
    
  };

  ///////////////////////////////////////////////////////////////////////////
  //
  // Complex objects
  //
  
  class ComplexObject : public Object
  {
  protected:

    //
    // Construction
    //
    ComplexObject() : Object(true) {}

    //
    // Copy construction
    //
    ComplexObject(ComplexObject const&) : Object(true) {}

    //
    // Mark subobjects
    //
    virtual void MarkObjects(ObjectVec& grey) {}

    //
    // Friends
    //
    friend class GC;

  };

}; // class GC

#endif // CSCRIPT_GCOBJECT_H
