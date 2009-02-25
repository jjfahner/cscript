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
#ifndef CSCRIPT_COMCLASS_H
#define CSCRIPT_COMCLASS_H

#include <windows.h>
#include <objbase.h>
#include <comdef.h>
#include <atlconv.h>

// #$%@ Windows defines!!!
#undef GetObject

#include "comtype.h"
#include "args.h"
#include "object.h"
#include "function.h"
#include "variable.h"
#include "enumerator.h"

class ComTypeInfo;
class ComMemberFunction;
class ComEnumerator;

//////////////////////////////////////////////////////////////////////////

class ComObject : public Object
{
public:

  //
  // Class factory
  //
  static Object* Create(String progID);

  //
  // Class factory
  //
  static Object* Create(IDispatch* p);

  //
  // Class type name
  //
  virtual String GetTypeName() const
  {
    return "ComObject " + m_typeInfo->GetTypeName();
  }

  //
  // Is a certain key present
  //
  virtual bool ContainsKey(Value const& key, bool checkProto = true);

  //
  // Find a member
  //
  virtual bool Find(Value const& key, RValue*& pValue, bool checkProto = true);

  //
  // Raw method invocation
  //
  void Invoke(DISPID dispid, INVOKEKIND invokeKind, Arguments& args, VARIANT& vResult) const;

  //
  // Invoke a method or property
  //
  Value Invoke(DISPID dispid, INVOKEKIND invokeKind, Arguments& args) const;

  //
  // Retrieve an enumerator for a property
  //
  Enumerator* GetEnumerator(Value const& value) const;

protected:

  //
  // Construction
  //
  ComObject(IDispatch* pdisp = 0);

  //
  // Destruction
  //
  ~ComObject();

  //
  // MemberMap
  //
  friend class ComMemberFunction;
  friend class ComMemberVariable;
  IDispatch*    m_dispatch;
  ComTypeInfo*  m_typeInfo;
};

//////////////////////////////////////////////////////////////////////////

class ComEnumerator : public Enumerator
{
  Evaluator*      m_eval;
  IEnumVARIANTPtr m_pEnum;

public:

  //
  // Construction
  //
  ComEnumerator(IEnumVARIANTPtr const& pEnum);

  //
  // Reset enumerator to first entry
  //
  virtual void Reset();

  //
  // Retrieve next value
  //
  virtual bool GetNext(Value& value);

};

//////////////////////////////////////////////////////////////////////////

class ROComVariable : public RValue
{
  ComObject const* m_inst;
  mutable Value m_value;
  String m_name;
  DISPID m_dispid;

public:

  //
  // Construction
  //
  ROComVariable(String name, DISPID dispid, ComObject const* inst);

  //
  // Retrieve value
  //
  Value const& GetValue() const;

  //
  // Create an enumerator
  //
  virtual Enumerator* GetEnumerator() const;

};

//////////////////////////////////////////////////////////////////////////

class RWComVariable : public LValue
{
  ComObject const* m_inst;
  mutable Value m_value;
  String m_name;
  DISPID m_dispid;

public:

  //
  // Construction
  //
  RWComVariable(String name, DISPID dispid, ComObject const* inst);

  //
  // Retrieve value
  //
  Value const& GetValue() const;

  //
  // Set value
  //
  void SetValue(Value const& rhs);

  //
  // Create an enumerator
  //
  virtual Enumerator* GetEnumerator() const;

};

//////////////////////////////////////////////////////////////////////////

class ComMemberFunction : public Function
{
public:

  //
  // Construction
  //
  ComMemberFunction(String name, DISPID dispid, ComObject const* instance);

  //
  // Retrieve parameter list
  //
  Object* GetParameters() const;

  //
  // Execution
  //
  virtual Value Execute(Evaluator* eval, Arguments& args);

protected:

  //
  // MemberMap
  //
  ComObject const* m_inst;
  DISPID       m_dispid;

};

#endif // CSCRIPT_COMCLASS_H
