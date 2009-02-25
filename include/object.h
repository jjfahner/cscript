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

#include <gc.h>
#include <value.h>
#include <map_iter.h>

#include <map>

class Object;
class RValue;
class LValue;

//
// Member map
//
typedef std::map<Value, RValue*> MemberMap;

//////////////////////////////////////////////////////////////////////////
//
// Object class
//
class Object : public GC::Object
{
public:

  //
  // Construction
  //
  Object() {}

  //
  // Object type
  //
  virtual String GetTypeName() const;

  //////////////////////////////////////////////////////////////////////////
  //
  // Iterators
  //

  typedef MemberMap::iterator                         MemberIterator;
  typedef map_iterator_t<MemberMap, key_accessor>     KeyIterator;
  typedef map_iterator_t<MemberMap, pointer_accessor> ValueIterator;

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

  //////////////////////////////////////////////////////////////////////////
  //
  // Members
  //

  //
  // Member count
  //
  size_t Count() const {
    return m_members.size();
  }

  //
  // Is a certain key present
  //
  virtual bool ContainsKey(Value const& key, bool checkProto = true);

  //
  // Find a member
  //
  virtual bool Find(Value const& key, RValue*& pValue, bool checkProto = true);

  //
  // Add new item to end
  //
  RValue& Add(Value const& value);

  //
  // Add a member
  //
  RValue& Add(Value const& key, Value const& value);

  //
  // Add a member with a custom RValue-derived type
  //
  RValue& Add(Value const& key, RValue* value);

  //
  // Retrieve variable as rvalue
  //
  virtual RValue& GetRValue(Value const& key);

  //
  // Retrieve variable as lvalue
  //
  virtual LValue& GetLValue(Value const& key);

  //
  // Use index operator to retrieve lvalue
  //
  LValue& operator [] (Value const& key)
  {
    return GetLValue(key);
  }

  //
  // Add members in member list
  //
  void AddMembers(Object* source);

  //
  // Remove a member
  //
  virtual void Remove(Value const& key);

protected:

  //
  // Virtual destruction
  //
  virtual ~Object();

  //
  // Protected construction forms
  //
  Object(Object const&) {}
  Object& operator = (Object const&) {}

private:

  //
  // Object members
  //
  MemberMap m_members;

};

#endif // CSCRIPT_OBJECT_H
