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
#include <map>
#include <map_iter.h>

class Object;
class RValue;
class LValue;
class Value;
class DataType;
class ROVariable;
class MemberVariable;

//
// Member map
//
typedef std::map<String, MemberVariable> MemberMap;

//////////////////////////////////////////////////////////////////////////
//
// Object class
//
class Object : public GC::ComplexObject
{
public:

  //
  // Construction
  //
  Object();

  //
  // Object type
  //
  virtual DataType* GetDataType() const;

  //
  // Object type name
  //
  virtual String GetTypeName() const;

  //////////////////////////////////////////////////////////////////////////
  //
  // Iterators
  //

  typedef MemberMap::iterator                       MemberIterator;
  typedef map_iterator_t<MemberMap, key_accessor>   KeyIterator;
  typedef map_iterator_t<MemberMap, value_accessor> ValueIterator;

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
  // Update member map. Implemented by derived classes.
  //
  virtual void UpdateMembers() {}

  //
  // Member count
  //
  virtual size_t Count() const;

  //
  // Is a certain key present
  //
  virtual bool ContainsKey(String const& key, bool checkProto = true) const;

  //
  // Find a member
  //
  virtual bool Find(String const& key, RValue*& pValue, bool checkProto = true) const;

  //
  // Add new item to end
  //
  RValue& Add(Value const& value);

  //
  // Add a member
  //
  RValue& Add(String const& key, Value const& value);

  //
  // Add a member with a custom RValue-derived type
  //
  RValue& Add(String const& key, RValue* value);

  //
  // Retrieve variable as rvalue
  //
  virtual RValue& GetRValue(String const& key);

  //
  // Retrieve variable as lvalue
  //
  virtual LValue& GetLValue(String const& key);

  //
  // Use index operator to retrieve lvalue
  //
  LValue& operator [] (String const& key)
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
  virtual void Remove(String const& key);

protected:

  //
  // Protected construction forms
  //
  Object(Object const&) {}
  Object& operator = (Object const&) {}

  //
  // Mark subobjects
  //
  virtual void MarkObjects(GC::ObjectVec& grey);

private:

  //
  // Object members
  //
  mutable ROVariable* m_dataType;
  mutable MemberMap m_members;

};

#endif // CSCRIPT_OBJECT_H
