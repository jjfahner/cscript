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

namespace GC
{
  //
  // Forward declare Object class
  //
  class Object;

  //
  // Vector of collectable objects
  //
  typedef std::vector<Object*> ObjectVec;

  //
  // Invoke collection cycle
  //
  void Collect(ObjectVec const& roots);

  //
  // Current number of objects
  //
  size_t ObjectCount();

  //
  // Base class for collectible objects
  //
  class Object
  {
  public:

    //
    // Virtual destruction
    //
    virtual ~Object() {}

  protected:

    //
    // Construction
    //
    Object();

  private:

    //
    // Allow the collector access
    //
    friend void GC::Collect(ObjectVec const&);

    //
    // Whether to collect this object
    //
    bool m_collect;

  };

} // namespace GC

#endif // CSCRIPT_GCOBJECT_H
