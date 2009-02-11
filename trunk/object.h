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
#include "map_iter.h"

class Object;
class RValue;
class LValue;
class Function;
class Variable;

//
// List of root objects
//
typedef std::set<Object*> Objects;

//
// Member map
//
typedef std::map<Value, RValue*, ValueComparatorLess> MemberMap;

//
// Value list
//
typedef std::vector<Value> ValueVec;


//////////////////////////////////////////////////////////////////////////
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
  // Construction
  //
  Object();

  //
  // Clone the object
  //
  virtual Object* Clone(Object* into = 0) const;

  //
  // Object type
  //
  virtual String GetTypeName() const;

  //////////////////////////////////////////////////////////////////////////

  typedef map_iterator_t<MemberMap, key_accessor>     KeyIterator;
  typedef map_iterator_t<MemberMap, pointer_accessor> ValueIterator;

  //
  // Contains a member
  //
  virtual bool Contains(Value const& key) const
  {
    return m_members.count(key) != 0;
  }

  //
  // Retrieve variables
  //
  virtual MemberMap& Members()
  {
    return m_members;
  }

  //
  // Iterator for values in member list
  //
  KeyIterator KeysBegin()
  {
    return KeyIterator(m_members.begin());
  }
  KeyIterator KeysEnd()
  {
    return KeyIterator(m_members.end());
  }

  //
  // Iterator for values in member list
  //
  ValueIterator ValuesBegin()
  {
    return ValueIterator(m_members.begin());
  }
  ValueIterator ValuesEnd()
  {
    return ValueIterator(m_members.end());
  }

  //
  // Retrieve const variables
  //
  MemberMap const& Members() const
  {
    return const_cast<Object*>(this)->Members();
  }

  //
  // Add new item to end
  //
  LValue& Add(Value const& value);

  //
  // Add members in member list
  //
  void Add(MemberMap const& source);

  //
  // Add a member
  //
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
  // Retrieve variable as rvalue
  //
  virtual RValue& RVal(Value const& key);

  //
  // Retrieve variable as lvalue
  //
  virtual LValue& LVal(Value const& key);

  //
  // Use index operator to retrieve lvalue
  //
  LValue& operator [] (Value const& key)
  {
    return LVal(key);
  }

  //
  // Find a member
  //
  virtual bool Find(Value const& key, RValue*& pValue) const
  {
    MemberMap::const_iterator it = m_members.find(key);
    if(it == m_members.end())
    {
      return false;
    }
    pValue = it->second;
    return true;
  }

  //
  // Retrieve all keys/values
  //
  ValueVec Keys()   const;
  ValueVec Values() const;

protected:

  //
  // Protected construction
  //
  Object(Object const&);
  Object& operator = (Object const&);

  //
  // Virtual destruction
  //
  friend class ObjectDeleter;
  virtual ~Object();

  //
  // Object members
  //
  MemberMap m_members;

};

#endif // CSCRIPT_OBJECT_H
