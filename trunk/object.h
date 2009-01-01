//////////////////////////////////////////////////////////////////////////
//
// This file is � 2007 JJ Fahner <jan-jaap@jan-jaap.net>
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
#ifndef CSCRIPT_OBJECT_H
#define CSCRIPT_OBJECT_H

#include <map>
#include <set>

#include "value.h"
#include "valuemap.h"

//
// List of root objects
//
typedef std::set<Object*> Objects;

//
// Object class
//
class Object 
{
public:

  //////////////////////////////////////////////////////////////////////////
  //
  // Static members
  //

  //
  // Object factory
  //
  static Object* Create(Evaluator* eval);

  //
  // Invoke garbage collection.
  //
  static void Collect(Objects valid);

  //
  // Retrieve list of all objects
  //
  static Objects const& GetObjects();

  //////////////////////////////////////////////////////////////////////////
  //
  // Instance members
  //

  //
  // Virtual destruction
  //
  virtual ~Object()
  {
  }

  //
  // Object type
  //
  virtual String GetTypeName() const;

  //
  // Member values
  //
  ValueMap& GetMembers()
  {
    return m_members;
  }
  ValueMap const& GetMembers() const
  {
    return m_members;
  }

  //
  // Finalization
  //
  virtual bool FinalizeRequired() const
  {
    return false;
  }
  virtual void Finalize() 
  {
  }

protected:

  //
  // Protected construction
  //
  Object(Evaluator* eval);

  //
  // Object members
  //
  Evaluator*  m_evaluator;
  ValueMap    m_members;

};

#endif // CSCRIPT_OBJECT_H