//////////////////////////////////////////////////////////////////////////
//
// This file is © 2007 JJ Fahner <jan-jaap@jan-jaap.net>
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
  virtual AstList const* GetParameters() const = 0;

  //
  // Execute function 
  //
  virtual Value Execute(Evaluator* evaluator, Arguments& args) = 0;

protected:

  //
  // Members
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
  ScriptFunction(String name, Ast* node) :
  Function (name),
  m_node   (node)
  {
  }

  //
  // Retrieve node
  //
  Ast* GetNode() const
  {
    return m_node;
  }

  //
  // Parameter list
  //
  virtual AstList const* GetParameters() const;

  //
  // Execution
  //
  virtual Value Execute(Evaluator* evaluator, Arguments& args);

protected:

  //
  // Members
  //
  Ast* m_node;

};

//////////////////////////////////////////////////////////////////////////
//
// Script function
//

class MemberFunction : public ScriptFunction
{
public:

  //
  // Construction
  // 
  MemberFunction(String name, Class const* cl, Ast* node) :
  ScriptFunction(name, node),
  m_class(cl)
  {
  }

  //
  // Object type
  //
  virtual String GetTypeName() const
  {
    return Object::GetTypeName() + "::" + m_class->GetName() + "::" + Function::GetName();
  }

  //
  // Retrieve class
  //
  Class const* GetClass() const
  {
    return m_class;
  }

protected:

  //
  // Members
  //
  Class const* m_class;

};

//////////////////////////////////////////////////////////////////////////
//
// Constructor
//

class Constructor : public MemberFunction
{
public:

  //
  // Construction
  //
  Constructor(Class const* cl, Ast* node) :
  MemberFunction("Constructor", cl, node)
  {
  }

};

//////////////////////////////////////////////////////////////////////////
//
// Constructor
//

class Destructor : public MemberFunction
{
public:

  //
  // Construction
  //
  Destructor(Class const* cl, Ast* node) :
  MemberFunction("Destructor", cl, node)
  {
  }

  //
  // Parameter list
  //
  virtual AstList const* GetParameters() const;

};

//////////////////////////////////////////////////////////////////////////
//
// Conversion operator
//

class ConversionOperator : public MemberFunction
{
public:

  //
  // Construction
  // 
  ConversionOperator(Class const* cl, TypeInfo const& ti, Ast* node) :
  MemberFunction(ti.GetName(), cl, node),
  m_ti (ti)
  {
  }

  //
  // Retrieve type info
  //
  TypeInfo const& GetTypeInfo() const
  {
    return m_ti;
  }

  //
  // Override parameters call; conversion operators have no parameters
  //
  virtual AstList const* GetParameters() const;

protected:

  //
  // Members
  //
  TypeInfo m_ti;

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
  // Parameter list
  //
  virtual AstList const* GetParameters() const
  {
    return m_pars;
  }

  //
  // Execution
  //
  virtual Value Execute(Evaluator* evaluator, Arguments& args);

protected:

  //
  // Members
  //
  NativeCall  m_call;
  AstList*    m_pars;

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
  ExternFunction(String name, Ast* node) :
  Function  (name),
  m_node    (node)
  {
  }

  //
  // Parameter list
  //
  virtual AstList const* GetParameters() const;

  //
  // Execution
  //
  virtual Value Execute(Evaluator* evaluator, Arguments& args);

protected:

  //
  // Members
  //
  Ast* m_node;

};

#endif // CSCRIPT_FUNCTION_H
