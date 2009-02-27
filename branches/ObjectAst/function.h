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
#ifndef CSCRIPT_FUNCTION_H
#define CSCRIPT_FUNCTION_H

#include "types.h"
#include "value.h"
#include "args.h"
#include "native.h"
#include "object.h"
#include "variable.h"
#include "ast.h"

class Evaluator;

//////////////////////////////////////////////////////////////////////////
//
// Base class for functions
//

class Function : public Object
{
public:

  //
  // Construction
  //
  Function(String name) :
  m_name (name)
  {
  }

  //
  // Destruction
  //
  virtual ~Function()
  {
  }

  //
  // Object type
  //
  virtual String GetTypeName() const
  {
    return Object::GetTypeName() + "::" + m_name;
  }

  //
  // Function name
  //
  virtual String GetName() const
  {
    return m_name;
  }

  //
  // Parameter list
  //
  virtual Object* GetParameters() const = 0;

  //
  // Execute function 
  //
  virtual Value Execute(Evaluator* evaluator, Arguments& args) = 0;

protected:

  //
  // MemberMap
  //
  String m_name;

};

//////////////////////////////////////////////////////////////////////////
//
// Script function
//

class ScriptFunction : public Function
{
public:

  //
  // Construction
  // 
  ScriptFunction(String name, Object* node) :
  Function (name)
  {
    (*this)["__ast"] = node;
  }

  //
  // Clone
  //
  virtual Object* Clone(Object* into = 0) const
  {
    // Create copy of function
    ScriptFunction* s = new ScriptFunction(m_name, GetNode());

    // Clone object into copy
    Function::Clone(s);

    // Done
    return s;
  }

  //
  // Retrieve node
  //
  Object* GetNode() const
  {
    return const_cast<ScriptFunction&>(*this)["__ast"].GetObject();
  }

  //
  // Parameter list
  //
  virtual Object* GetParameters() const
  {
    return A2(GetNode());
  }

  //
  // Execution
  //
  virtual Value Execute(Evaluator* evaluator, Arguments& args);

};

//////////////////////////////////////////////////////////////////////////
//
// Native function
//

class NativeFunction : public Function
{
public:

  //
  // Construction
  //
  NativeFunction(String decl, NativeCall call);

  //
  // Clone
  //
  virtual Object* Clone(Object* into = 0) const
  {
    throw std::runtime_error("Cannot clone native function");
  }

  //
  // Retrieve node
  //
  Object* GetNode() const
  {
    return const_cast<NativeFunction&>(*this)["__ast"].GetObject();
  }

  //
  // Parameter list
  //
  virtual Object* GetParameters() const
  {
    return A2(GetNode());
  }

  //
  // Execution
  //
  virtual Value Execute(Evaluator* evaluator, Arguments& args);

protected:

  //
  // MemberMap
  //
  NativeCall  m_call;

};

//////////////////////////////////////////////////////////////////////////
//
// Native function
//

class ExternFunction : public Function
{
public:

  //
  // Construction
  //
  ExternFunction(String name, Object* node) :
  Function  (name)
  {
    (*this)["__ast"] = node;
  }

  //
  // Clone
  //
  virtual Object* Clone(Object* into = 0) const
  {
    throw std::runtime_error("Cannot clone extern function");
  }

  //
  // Retrieve node
  //
  Object* GetNode() const
  {
    return const_cast<ExternFunction&>(*this)["__ast"].GetObject();
  }

  //
  // Parameter list
  //
  virtual Object* GetParameters() const;

  //
  // Execution
  //
  virtual Value Execute(Evaluator* evaluator, Arguments& args);

};

#endif // CSCRIPT_FUNCTION_H