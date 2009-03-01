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
#ifndef CSCRIPT_NATIVE_CALLS_H
#define CSCRIPT_NATIVE_CALLS_H

#include <cscript.h>
#include <function.h>

class Evaluator;
class Arguments;

//////////////////////////////////////////////////////////////////////////
//
// Native function
//

class NativeFunction : public Function
{
public:
  
  //
  // Native call signature
  //
  typedef Value (*FunPtr)(Evaluator* evaluator, Arguments const& args);

  //
  // Construction
  //
  NativeFunction(String decl, FunPtr funPtr);

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
    return Ast_A2(GetNode());
  }

  //
  // Execution
  //
  virtual Value Execute(Evaluator* evaluator, Arguments& args);

protected:

  //
  // MemberMap
  //
  FunPtr m_funPtr;

};

//
// Registrar for native calls
//
class NativeCallRegistrar
{
public:

  //
  // Execute prior to creating evaluators
  //
  static void RegisterCalls();

  //
  // Invoked automatically by NATIVE_CALL macro
  //
  NativeCallRegistrar(String const& decl, NativeFunction::FunPtr funPtr);
};

//////////////////////////////////////////////////////////////////////////

//
// Create unique identifier from __LINE__
//
#define CONCATENATE_DIRECT(s1, s2)  s1##s2
#define CONCATENATE(s1, s2)         CONCATENATE_DIRECT(s1, s2)
#define UNIQUE_PREFIXED(str)        CONCATENATE(str, __LINE__)
#define UNIQUE_IDENTIFIER           UNIQUE_PREFIXED(unique) 

//
// Native call handler
//
#define GETLINE __LINE__
#define NATIVE_CALL(decl)                                                                           \
  static Value               UNIQUE_PREFIXED(native_)   (Evaluator* evaluator, Arguments const& args);   \
  static NativeCallRegistrar UNIQUE_PREFIXED(registrar_)(decl, UNIQUE_PREFIXED(native_));           \
  static Value               UNIQUE_PREFIXED(native_)   (Evaluator* evaluator, Arguments const& args)

//////////////////////////////////////////////////////////////////////////
//
// Native class method
//

template <typename T>
class NativeMethod : public Function
{
public:

  typedef Value (T::*FunPtr) 
    (Evaluator* evaluator, Arguments const& args);

  //
  // Construction
  //
  NativeMethod(String name, FunPtr funPtr, Object* ast) :
  Function  (name),
  m_funPtr  (funPtr)
  {
    Add("__ast", ast);
  }

  //
  // Retrieve node
  //
  Object* GetNode() const
  {
    RValue* pValue;
    if(!Find("__ast", pValue) || pValue->Type() != Value::tObject)
    {
      throw std::runtime_error("Missing declaration for native method");
    }
    return pValue->GetObject();
  }

  //
  // Parameter list
  //
  virtual Object* GetParameters() const
  {
    return Ast_A2(GetNode());
  }

  //
  // Execution
  //
  virtual Value Execute(Evaluator* evaluator, Arguments& args)
  {
    // Retrieve instance
    T* inst = dynamic_cast<T*>(args.GetObject());
    if(inst == 0)
    {
      throw std::runtime_error("Invalid object type");
    }

    // Invoke method
    return (inst->*m_funPtr)(evaluator, args);
  }

protected:

  //
  // MemberMap
  //
  String m_decl;
  FunPtr m_funPtr;

};

#define NATIVE_METHOD(class, method, decl) \
  (*this)[#method] = new NativeMethod<class>(#method, & class :: method, evaluator->ParseNativeCall(decl))

//////////////////////////////////////////////////////////////////////////

//
// Forced linkage when included
//
#ifdef CSLIB
#define DECLARE_NATIVE_LINKAGE(name) \
struct CSCRIPT_##name##_LINKAGE \
{ \
   CSCRIPT_##name##_LINKAGE(); \
};
#else
#define DECLARE_NATIVE_LINKAGE(name) \
struct CSCRIPT_##name##_LINKAGE \
{ \
  CSCRIPT_##name##_LINKAGE(); \
}; \
static CSCRIPT_##name##_LINKAGE CSCRIPT_##name##_LINKAGE_INST;
#endif

#define DEFINE_NATIVE_LINKAGE(name) \
CSCRIPT_##name##_LINKAGE::CSCRIPT_##name##_LINKAGE() {}

#endif // #ifndef CSCRIPT_NATIVE_CALLS_H
