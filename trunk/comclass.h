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
#ifndef CSCRIPT_COMCLASS_H
#define CSCRIPT_COMCLASS_H

#include "class.h"
#include "function.h"
#include "instance.h"

#include <windows.h>
#include <objbase.h>

class ComTypeInfo;
class ComMemberFunction;

class ComClass : public Class
{
public:

  //
  // Factory
  //
  static ComClass* FromProgID(String progID);

  //
  // Com objects have no constructors
  //
  virtual Constructor* GetConstructor() const
  {
    return 0;
  }
  virtual void SetConstructor(Constructor*)
  {
    throw std::runtime_error("Cannot add constructor to COM object");
  }

  //
  // Com objects have no destructors
  //
  virtual Destructor* GetDestructor() const
  {
    return 0;
  }
  virtual void SetDestructor(Destructor*)
  {
    throw std::runtime_error("Cannot add destructor to COM object");
  }

  //
  // Com objects cannot have variables added
  //
  virtual void AddVariable(String const&, Ast*)
  {
    throw std::runtime_error("Cannot add variable to COM object");
  }

  //
  // Com objects cannot have methods added
  //
  virtual void AddMethod(String const& name, MemberFunction* node)
  {
    throw std::runtime_error("Cannot add method to COM object");
  }

  //
  // Find a method
  //
  virtual bool FindMethod(String const& name, MemberFunction*& fun) const;

  //
  // Com objects cannot have conversions added
  //
  virtual void AddConversion(ConversionOperator* node)
  {
    throw std::runtime_error("Cannot add conversion to COM object");
  }

  //
  // Find a conversion operator
  //
  virtual bool FindConversion(TypeInfo const& type, ConversionOperator*& node) const;

protected:

  //
  // Constructor
  //
  ComClass(String progID);

  //
  // Destruction
  //
  ~ComClass();

  //
  // Instance construction
  //
  friend class ComInstance;
  virtual void ConstructInstance(ComInstance* inst) const;
  virtual void DestructInstance(ComInstance* inst) const;  

  //
  // Collection of methods
  //
  typedef std::map<DISPID, ComMemberFunction*> Methods;

  //
  // Members
  //
  String        m_progID;
  CLSID         m_clsid;
  mutable ComTypeInfo*  m_info;
  mutable Methods       m_methods;
  
};

//////////////////////////////////////////////////////////////////////////

class ComInstance : public Instance
{
public:

  //
  // Class factory
  //
  static Instance* Create(Evaluator* eval, ComClass const* c);

  //
  // Class type name
  //
  virtual String GetTypeName() const
  {
    return "ComClass " + m_class->GetName();
  }

  //
  // Requires finalization
  //
  virtual bool FinalizeRequired() const
  {
    return true;
  }

  //
  // Finalization
  //
  virtual void Finalize()
  {
    m_class->DestructInstance(this);
  }

  //
  // Retrieve class instance
  //
  virtual ComClass const* GetClass() const
  {
    return m_class;
  }

  //
  // Number of instance variables
  //
  virtual size_t GetVarCount() const
  {
    // TODO
    return 0;
  }

  //
  // Retrieve a variable
  //
  virtual bool FindVar(String const& name, Value& ref) const;

  //
  // Retrieve a function
  //
  virtual bool FindMethod(String const& name, MemberFunction*& fun) const 
  {
    return m_class->FindMethod(name, fun);
  }

  //
  // Invoke a method or property
  //
  Value Invoke(Evaluator* evaluator, DISPID dispid, Arguments& args);

protected:

  //
  // Construction
  //
  ComInstance(Evaluator* eval, ComClass const* c);

  //
  // Members
  //
  friend class ComClass;
  friend class ComMemberFunction;
  ComClass const* m_class;
  IDispatch*      m_dispatch;
};

//////////////////////////////////////////////////////////////////////////

class ComMemberFunction : public MemberFunction
{
public:

  //
  // Construction
  //
  ComMemberFunction(String name, ComClass const* cl, DISPID dispid);

  //
  // Retrieve parameter list
  //
  AstList const* GetParameters() const;

  //
  // Execution
  //
  virtual Value Execute(Evaluator* evaluator, Arguments& args);

protected:

  //
  // Members
  //
  ComClass const* m_class;
  DISPID          m_dispid;

};

#endif // CSCRIPT_COMCLASS_H
