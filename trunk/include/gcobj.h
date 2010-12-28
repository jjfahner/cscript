//////////////////////////////////////////////////////////////////////////
//
// This file is © 2009 - 2011 JJ Fahner <jan-jaap@jan-jaap.net>
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
#ifndef CSCRIPT_GCOBJ_H
#define CSCRIPT_GCOBJ_H

#include <vector>

//
// Forward declare GCObject
//
class GCObject;

//
// Make typedef for vector of GCObjects
//
typedef std::vector<GCObject*> GCObjectVec;

//
// GCObject class
//
class GCObject
{
protected:

  //
  // Virtual destructor
  //
  virtual ~GCObject() {}

  //
  // Virtual destruction
  //
  virtual void Delete() { 
    delete this; 
  }

  //
  // Assignment
  //
  GCObject& operator = (GCObject&) { return *this; }

private:

  //
  // Friends
  //
  friend class GC;
  friend class GCSimpleObject;
  friend class GCComplexObject;

  //
  // Construction
  //
  GCObject(bool complex);

  //
  // Copy construction not allowed
  //
  GCObject(GCObject&);

  //
  // Members
  //
  unsigned m_complex : 1;
  unsigned m_collect : 1;
  unsigned m_pinned  : 1;

};

//////////////////////////////////////////////////////////////////////////
//
// Simple GCObject
//

class GCSimpleObject : public GCObject
{
protected:

  //
  // Construction
  //
  GCSimpleObject() : GCObject(false) {}

  //
  // Copy construction
  //
  GCSimpleObject(GCSimpleObject const&) : GCObject(false) {}

private:

  //
  // Friends
  //
  friend class GC;

};

///////////////////////////////////////////////////////////////////////////
//
// Complex GCObject
//

class GCComplexObject : public GCObject
{
protected:

  //
  // Construction
  //
  GCComplexObject() : GCObject(true) {}

  //
  // Copy construction
  //
  GCComplexObject(GCComplexObject const&) : GCObject(true) {}

  //
  // Mark subobjects
  //
  virtual void MarkObjects(GCObjectVec& grey) {}

  //
  // Friends
  //
  friend class GC;

};

#endif // CSCRIPT_GCOBJ_H
