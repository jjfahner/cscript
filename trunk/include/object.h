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
typedef std::map<Value, RValue*> MemberMap;

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
  // Object type
  //
  virtual String GetTypeName() const;

  //////////////////////////////////////////////////////////////////////////

  typedef MemberMap::iterator                         MemberIterator;
  typedef map_iterator_t<MemberMap, key_accessor>     KeyIterator;
  typedef map_iterator_t<MemberMap, pointer_accessor> ValueIterator;

  //
  // Member count
  //
  size_t Count() const {
    return m_members.size();
  }

  //
  // Is a certain key present
  //
  virtual bool ContainsKey(Value const& key) const;

  //
  // Iterator for members
  //
  MemberIterator Begin() {
    return m_members.begin();
  }
  MemberIterator End() {
    return m_members.end();
  }

  //
  // Iterator for values in member list
  //
  KeyIterator KeyBegin() {
    return KeyIterator(m_members.begin());
  }
  KeyIterator KeyEnd() {
    return KeyIterator(m_members.end());
  }

  //
  // Iterator for values in member list
  //
  ValueIterator ValueBegin() {
    return ValueIterator(m_members.begin());
  }
  ValueIterator ValueEnd() {
    return ValueIterator(m_members.end());
  }

  //
  // Add new item to end
  //
  LValue& Add(Value const& value);

  //
  // Add a member
  //
  LValue& Add(Value const& key, Value const& value);

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
  // Add members in member list
  //
  void AddMembers(Object* source);

  //
  // Find a member
  //
  virtual bool Find(Value const& key, RValue*& pValue) const;

  //
  // Remove a member
  //
  virtual void Remove(Value const& key);

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
