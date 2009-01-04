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
    // TODO
    // DestructInstance(this);
  }

  //
  // Class
  //
  Class const* GetClass() const
  {
    return m_class;
  }

  //
  // Number of instance variables
  //
  virtual size_t GetVarCount() const
  {
    return GetVariables().size();
  }

  //
  // Retrieve a variable
  //
  virtual bool FindVar(String const& name, RValue*& ptr) const
  {
    Variables::const_iterator it = GetVariables().find(name);
    if(it == GetVariables().end())
    {
      return false;
    }
    ptr = it->second;
    return true;
  }

  //
  // Retrieve a function
  //
  virtual bool FindMethod(String const& name, Function*& fun) const
  {
    return m_class->FindMethod(name, fun);
  }

protected:

  //
  // Construction
  //
  Instance(Evaluator* eval, Class const* c);

  //
  // Members
  //
  Class const* m_class;

  //
  // Class required access for instantiation
  //
  friend class Class;

};

#endif // CSCRIPT_INSTANCE_H
