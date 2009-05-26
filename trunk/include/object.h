//////////////////////////////////////////////////////////////////////////
//
// This file is � 2007 - 2009 JJ Fahner <jan-jaap@jan-jaap.net>
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
class Enumerator;
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
  Object(DataType* dataType = 0);

  //
  // Object type
  //
  virtual DataType* GetType() const;

  //
  // Member count
  //
  virtual size_t Count() const;

  //
  // Generic enumerator object
  //
  virtual Enumerator* GetEnumerator();

  //
  // Retrieve a member by key
  //
  virtual Value const& Get(Value const& key);

  //
  // Try to retrieve a member by key
  //
  virtual bool TryGet(Value const& key, Value const*& value);

  //
  // Set a member
  //
  virtual Value const& Set(Value const& key, Value const& value);

  //
  // Set a member
  //
  virtual bool TrySet(Value const& key, Value const& value);

  //
  // Unset a member
  //
  virtual void Unset(Value const& key);

  //////////////////////////////////////////////////////////////////////////
  //
  // Deprecated interface
  //

  //
  // Is a certain key present
  //
  virtual bool ContainsKey(String const& key, bool checkProto = true) const;

  //
  // Find a member
  //
  virtual bool Find(String const& key, RValue*& pValue, bool checkProto = true) const;

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

  friend class ObjectEnumerator;

  //
  // Object members
  //
  mutable DataType* m_dataType;
  mutable MemberMap m_members;

};

#endif // CSCRIPT_OBJECT_H
