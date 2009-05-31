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
  virtual void Unset(Value const& key)
  {
    // Nothing to do
  }

  //
  // Retrieve a member by key
  //
  virtual Value Get(Value const& key)
  {
    throw std::runtime_error("Member '" + key.GetString() + "' not found");
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
    throw std::runtime_error("Member '" + key.GetString() + "' not found");
  }

  //
  // Try to set a member
  //
  virtual bool TrySet(Value const& key, Value const& value)
  {
    return false;
  }

  //
  // Evaluate a method
  //
  Value Eval(Value const& key, Evaluator* evaluator, Arguments& arguments)
  {
    Value result;
    if(TryEval(key, evaluator, arguments, result))
    {
      return result;
    }
    throw std::runtime_error("Method '" + key.GetString() + "' not found");
  }

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

};

#endif // CSCRIPT_OBJECT_H
