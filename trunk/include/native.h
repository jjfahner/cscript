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
    if(!ContainsKey("__ast"))
    {
      return 0;
    }
    return const_cast<NativeFunction&>(*this)["__ast"].GetObject();
  }

  //
  // Parameter list
  //
  virtual List* GetParameters() const
  {
    Object* ast = GetNode();
    return ast ? AstList_A2(ast) : 0;
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

template <typename T, typename R>
class NativeMethod : public Function
{
public:

  typedef R (T::*FunPtr)();
  typedef R (T::*FunPtrA)(Arguments const& args);
  typedef R (T::*FunPtrE)(Evaluator* evaluator);
  typedef R (T::*FunPtrEA)(Evaluator* evaluator, Arguments const& args);

  //
  // Construction
  //
  NativeMethod(String name, FunPtr funPtr, Object* ast) :
  Function  (name),
  m_funPtr  (funPtr),
  m_funPtrA (0),
  m_funPtrE (0),
  m_funPtrEA(0)
  {
    Add("__ast", ast);
  }
  NativeMethod(String name, FunPtrA funPtr, Object* ast) :
  Function  (name),
  m_funPtr  (0),
  m_funPtrA (funPtr),
  m_funPtrE (0),
  m_funPtrEA(0)
  {
    Add("__ast", ast);
  }
  NativeMethod(String name, FunPtrE funPtr, Object* ast) :
  Function  (name),
  m_funPtr  (0),
  m_funPtrA (0),
  m_funPtrE (funPtr),
  m_funPtrEA(0)
  {
    Add("__ast", ast);
  }
  NativeMethod(String name, FunPtrEA funPtr, Object* ast) :
  Function  (name),
  m_funPtr  (0),
  m_funPtrA (0),
  m_funPtrE (0),
  m_funPtrEA(funPtr)
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
  virtual List* GetParameters() const
  {
    return AstList_A2(GetNode());
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
    if(m_funPtr)   return (inst->*m_funPtr)  ();
    if(m_funPtrA)  return (inst->*m_funPtrA) (args);
    if(m_funPtrE)  return (inst->*m_funPtrE) (evaluator);
    if(m_funPtrEA) return (inst->*m_funPtrEA)(evaluator, args);

    // ???
    throw std::runtime_error("Invalid native method");
  }

protected:

  //
  // MemberMap
  //
  String m_decl;
  FunPtr m_funPtr;
  FunPtrA m_funPtrA;
  FunPtrE m_funPtrE;
  FunPtrEA m_funPtrEA;

};

#define NATIVE_METHOD(class, method, decl)                    \
{                                                             \
  Object* node = Evaluator::ParseNativeCall(decl);            \
  this->Add(Ast_A1(node).GetString(),                         \
    new NativeMethod<class, Value>(Ast_A1(node).GetString(),  \
      & class :: method, node));                              \
}

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
