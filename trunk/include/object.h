//////////////////////////////////////////////////////////////////////////
//
// This file is © 2007 - 2011 JJ Fahner <jan-jaap@jan-jaap.net>
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

#include <cscript.h>
#include <value.h>
#include <gcobj.h>

class DataType;
class Arguments;
class Evaluator;
class Enumerator;

//////////////////////////////////////////////////////////////////////////
//
// Object class
//
class Object : public GCComplexObject
{
protected:

  //
  // Construction
  //
  Object() {}

public:

  //
  // Object type
  //
  virtual DataType* GetType();

  //
  // Generic enumerator object
  //
  virtual Enumerator* GetEnumerator()
  {
    return 0;
  }

  //
  // Member count
  //
  virtual int64 Count()
  {
    return 0;
  }

  //
  // Unset a member
  //
  virtual bool Unset(Value const& key)
  {
    // Nothing to do
    return false;
  }

  //
  // Retrieve a member by key
  //
  virtual Value Get(Value const& key)
  {
    Value value;
    if (TryGet(key, value))
    {
      return value;
    }
    throw std::runtime_error("Member '" + ValString(key) + "' not found");
  }

  //
  // Try to retrieve a member by key
  //
  virtual bool TryGet(Value const& key, Value& value)
  {
    return false;
  }

  //
  // Set a member
  //
  virtual Value const& Set(Value const& key, Value const& value)
  {
    if (TrySet(key, value))
    {
      return value;
    }
    throw std::runtime_error("Member '" + ValString(key) + "' not found");
  }

  //
  // Try to set a member
  //
  virtual bool TrySet(Value const& key, Value const& value)
  {
    return false;
  }

  //
  // Retrieve an object by index
  //
  virtual Value GetAt(Value const& index)
  {
    Value value;
    if (TryGetAt(index, value))
    {
      return value;
    }
    throw std::runtime_error("Invalid index");
  }

  //
  // Try to retrieve an object by index
  //
  virtual bool TryGetAt(Value const& index, Value& value)
  {
    return TryGet(index, value);
  }

  //
  // Set an object by index
  //
  virtual Value const& SetAt(Value const& index, Value const& value)
  {
    if (TrySetAt(index, value))
    {
      return value;
    }
    throw std::runtime_error("Invalid index");
  }

  //
  // Try to set an object by index
  //
  virtual bool TrySetAt(Value const& index, Value const& value)
  {
    return TrySet(index, value);
  }

  //
  // Evaluate a method
  //
  Value Eval(Value const& key, Arguments& arguments)
  {
    Value result;
    if(TryEval(key, arguments, result))
    {
      return result;
    }
    throw std::runtime_error("Method '" + ValString(key) + "' not found");
  }

  //
  // Try to evaluate a method
  //
  virtual bool TryEval(Value const& key, Arguments& arguments, Value& result);

protected:

  //
  // Protected construction forms
  //
  Object(Object const&) {}
  Object& operator = (Object const&) {}

};

#endif // CSCRIPT_OBJECT_H
