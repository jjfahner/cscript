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
#ifndef CSCRIPT_OBJECT_H
#define CSCRIPT_OBJECT_H

#include <map>
#include <set>

#include "value.h"

class Object;
class RValue;
class Function;

//
// List of root objects
//
typedef std::set<Object*> Objects;

//
// Member map
//
typedef std::map<Value, RValue*, ValueComparatorLess> Members;

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
  static Object* Create();

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
  virtual ~Object();

  //
  // Clone the object
  //
  virtual Object* Clone(Object* into = 0) const;

  //
  // Object type
  //
  virtual String GetTypeName() const;

  //
  // Retrieve variables
  //
  virtual Members& GetMembers()
  {
    return m_members;
  }

  //
  // Retrieve const variables
  //
  Members const& GetMembers() const
  {
    return const_cast<Object*>(this)->GetMembers();
  }

  //
  // Add a member
  //
  virtual RValue* Add(String const& key, RValue* value)
  {
    // This overload is basically meant to prevent the
    // automatic conversion from AstData to string
    return Add(Value(key), value);
  }
  virtual RValue* Add(Value const& key, RValue* value)
  {
    if(Contains(key))
    {
      throw std::runtime_error("Variable already declared");
    }
    m_members[key] = value;
    return value;
  }


  //
  // Contains a member
  //
  virtual bool Contains(Value const& key) const
  {
    return m_members.count(key) != 0;
  }

  //
  // Find a member
  //
  virtual bool Find(Value const& key, RValue*& pValue) const
  {
    Members::const_iterator it = m_members.find(key);
    if(it == m_members.end())
    {
      return false;
    }
    pValue = it->second;
    return true;
  }

protected:

  //
  // Protected construction
  //
  Object();
  Object(Object const&);
  Object& operator = (Object const&);

  //
  // Object members
  //
  Members m_members;

};

#endif // CSCRIPT_OBJECT_H
