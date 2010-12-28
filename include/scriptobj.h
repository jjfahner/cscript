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
#ifndef CSCRIPT_SCRIPTOBJ_H
#define CSCRIPT_SCRIPTOBJ_H

#include <object.h>
#include <map>
#include <ident.h>

class ScriptObject : public Object
{
public:

  //
  // Object type
  //
  virtual DataType* GetType();

  //
  // Member count
  //
  virtual int64 Count();

  //
  // Generic enumerator object
  //
  virtual Enumerator* GetEnumerator();

  //
  // Retrieve a member by key
  //
  virtual Value Get(Value const& key);

  //
  // Try to retrieve a member by key
  //
  virtual bool TryGet(Value const& key, Value& value);

  //
  // Set a member
  //
  virtual Value const& Set(Value const& key, Value const& value);

  //
  // Try to set a member
  //
  virtual bool TrySet(Value const& key, Value const& value);

  //
  // Unset a member
  //
  virtual bool Unset(Value const& key);

protected:

  //
  // Enumerator class
  //
  class MemberEnumerator;

  //
  // Member map
  //
  typedef std::map<IdentId, Value> MemberMap;
  typedef MemberMap::iterator MemberIterator;

  //
  // Mark subobjects
  //
  virtual void MarkObjects(GCObjectVec& grey);

  //
  // Object members
  //
  MemberMap m_members;

};

//////////////////////////////////////////////////////////////////////////

inline int64
ScriptObject::Count()
{
  return m_members.size();
}

inline bool 
ScriptObject::Unset(Value const& key)
{
  return m_members.erase(Identifier(key)) > 0;
}

inline Value
ScriptObject::Get(Value const& key)
{
  Value value;
  if(TryGet(key, value))
  {
    return value;
  }
  throw std::runtime_error("Property not found");
}

inline Value const&
ScriptObject::Set(Value const& key, Value const& value)
{
  if(!TrySet(key, value))
  {
    m_members[Identifier(key)] = value;
  }
  return value;
}

#endif // CSCRIPT_SCRIPTOBJ_H
