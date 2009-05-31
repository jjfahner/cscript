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
#include <value.h>

class DataType;
class Arguments;
class Evaluator;
class Enumerator;

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
  virtual void Unset(Value const& key);

  //
  // Evaluate a method
  //
  virtual Value Eval(Value const& key, Evaluator* evaluator, Arguments& arguments);

  //
  // Try to evaluate a method
  //
  virtual bool TryEval(Value const& key, Evaluator* evaluator, Arguments& arguments, Value& result);

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
  // Enumerator class
  //
  class ObjectEnumerator;

  //
  // Member map
  //
  typedef std::map<String, Value> MemberMap;
  typedef MemberMap::iterator MemberIterator;

  //
  // Object members
  //
  DataType* m_dataType;
  MemberMap m_members;

};

//////////////////////////////////////////////////////////////////////////

inline DataType* 
Object::GetType() const
{
  return m_dataType;
}

inline size_t 
Object::Count() const 
{
  // Return size
  return m_members.size();
}

inline void 
Object::Unset(Value const& key)
{
  m_members.erase(key);
}

inline Value
Object::Get(Value const& key)
{
  Value value;
  if(TryGet(key, value))
  {
    return value;
  }
  throw std::runtime_error("Property not found");
}

inline Value const&
Object::Set(Value const& key, Value const& value)
{
  if(!TrySet(key, value))
  {
    m_members[key] = value;
  }
  return value;
}

inline Value 
Object::Eval(Value const& key, Evaluator* evaluator, Arguments& arguments)
{
  Value result;
  if(TryEval(key, evaluator, arguments, result))
  {
    return result;
  }
  throw std::runtime_error("Method not found");
}

#endif // CSCRIPT_OBJECT_H
