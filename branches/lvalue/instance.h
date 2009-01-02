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
#ifndef CSCRIPT_INSTANCE_H
#define CSCRIPT_INSTANCE_H

#include "class.h"

class Instance : public Object
{
public:

  //
  // Class factory
  //
  static Instance* Create(Evaluator* eval, Class const* c);

  //
  // Destruction
  //
  virtual ~Instance()
  {
  }

  //
  // Class type name
  //
  virtual String GetTypeName() const
  {
    return "ScriptClass " + m_class->GetName();
  }

  //
  // Finalization
  //
  virtual bool FinalizeRequired() const
  {
    return m_class->GetDestructor() != 0;
  }
  virtual void Finalize()
  {
    m_class->DestructInstance(this);
  }

  //
  // Class
  //
  Class const* GetClass() const
  {
    return m_class;
  }

  //
  // Retrieve member variables
  //
  virtual MemberVariables& GetMemberVariables()
  {
    return m_variables;
  }

  //
  // Number of instance variables
  //
  virtual size_t GetVarCount() const
  {
    return m_variables.size();
  }

  //
  // Retrieve a variable
  //
  virtual bool FindVar(String const& name, RValue& ref) const
  {
    MemberVariables::const_iterator it = m_variables.find(name);
    if(it == m_variables.end())
    {
      return false;
    }
    ref = *it->second;
    return true;
  }

  //
  // Retrieve a function
  //
  virtual bool FindMethod(String const& name, MemberFunction*& fun) const 
  {
    return m_class->FindMethod(name, fun);
  }

protected:

  //
  // Construction
  //
  Instance(Evaluator* eval, Class const* c) : 
  Object      (eval),
  m_eval      (eval), 
  m_class     (c),
  m_variables (eval)
  {
    // Delegate to class
    m_class->ConstructInstance(this);
  }

  //
  // Members
  //
  Evaluator*      m_eval;
  Class const*    m_class;
  MemberVariables m_variables;

  //
  // Class required access for instantiation
  //
  friend class Class;

};

#endif // CSCRIPT_INSTANCE_H
