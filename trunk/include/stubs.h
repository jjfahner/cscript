//////////////////////////////////////////////////////////////////////////
//
// This file is © 2009 JJ Fahner <jan-jaap@jan-jaap.net>
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
#ifndef CSCRIPT_STUBS_H
#define CSCRIPT_STUBS_H

#include <cscript.h>
#include <value.h>

//
// Forward declare some required types
//
class Object;
class Evaluator;
class Arguments;

//
// Forward declare single-word types that have a wrapper function
//
typedef Object* ObjectPtr;
typedef Value const& ValueCRef;
typedef String const& StringCRef;
typedef Arguments const& ArgsCRef;

//
// Forward declare generic native call handlers
//
bool NativeCallTryGet(struct NativeCall*, Object* instance, Value const& key, Value& value);
bool NativeCallTrySet(struct NativeCall*, Object* instance, Value const& key, Value const& value);
bool NativeCallTryEval(struct NativeCall*, Object* instance, Value const& key, Evaluator* evaluator, Arguments& arguments, Value& result);


//////////////////////////////////////////////////////////////////////////
//
// Native get implementation
//

#define IMPL_NATIVE_GET(class, base)              \
bool                                              \
NativeTryGet(Value const& key, Value& value)      \
{                                                 \
  extern struct NativeCall __stublist_##class[];  \
  if(NativeCallTryGet(__stublist_##class,         \
    (Object*)this, key, value))                   \
  {                                               \
    return true;                                  \
  }                                               \
  return base::TryGet(key, value);                \
}                                                 \
Value                                             \
NativeGet(Value const& key)                       \
{                                                 \
  Value value;                                    \
  if(TryGet(key, value))                          \
  {                                               \
    return value;                                 \
  }                                               \
  throw std::runtime_error("Member '" +           \
      key.GetString() + "' not found");           \
}

#define DEF_NATIVE_GET(class, base)               \
IMPL_NATIVE_GET(class, base)                      \
virtual bool                                      \
TryGet(Value const& key, Value& value)            \
{                                                 \
  return NativeTryGet(key, value);                \
}                                                 \
virtual Value                                     \
Get(Value const& key)                             \
{                                                 \
  return NativeGet(key);                          \
}

//////////////////////////////////////////////////////////////////////////
//
// Native set implementation
//

#define IMPL_NATIVE_SET(class, base)              \
bool                                              \
NativeTrySet(Value const& key, Value const& value)\
{                                                 \
  extern struct NativeCall __stublist_##class[];  \
  if(NativeCallTrySet(__stublist_##class,         \
    (Object*)this, key, value))                   \
  {                                               \
    return true;                                  \
  }                                               \
  return base::TrySet(key, value);                \
}                                                 \
Value const&                                      \
NativeSet(Value const& key, Value const& value)   \
{                                                 \
  if(TrySet(key, value))                          \
  {                                               \
    return value;                                 \
  }                                               \
  throw std::runtime_error("Member '" +           \
      key.GetString() + "' not found");           \
}

#define DEF_NATIVE_SET(class, base)               \
IMPL_NATIVE_SET(class, base)                      \
virtual bool                                      \
TrySet(Value const& key, Value const& value)      \
{                                                 \
  return NativeTrySet(key, value);                \
}                                                 \
virtual Value const&                              \
Set(Value const& key, Value const& value)         \
{                                                 \
  return NativeSet(key, value);                   \
}

//////////////////////////////////////////////////////////////////////////
//
// Nstive Evaluate implementation
//

#define IMPL_NATIVE_EVAL(class, base)             \
bool                                              \
NativeTryEval(Value const& key,                   \
              Evaluator* evaluator,               \
              Arguments& arguments,               \
              Value& result)                      \
{                                                 \
  extern struct NativeCall __stublist_##class[];  \
  if(NativeCallTryEval(__stublist_##class,        \
    this, key, evaluator, arguments, result))     \
  {                                               \
    return true;                                  \
  }                                               \
  return base::TryEval(key, evaluator,            \
                    arguments, result);           \
}                                                 \
Value                                             \
NativeEval(Value const& key,                      \
           Evaluator* evaluator,                  \
           Arguments& arguments)                  \
{                                                 \
  Value result;                                   \
  if(TryEval(key, evaluator, arguments, result))  \
  {                                               \
    return result;                                \
  }                                               \
  throw std::runtime_error("Method '" +           \
      key.GetString() + "' not found");           \
}

#define DEF_NATIVE_EVAL(class, base)              \
IMPL_NATIVE_EVAL(class, base)                     \
virtual bool                                      \
TryEval(Value const& key,                         \
        Evaluator* evaluator,                     \
        Arguments& arguments,                     \
        Value& result)                            \
{                                                 \
  return NativeTryEval(key, evaluator,            \
                   arguments, result);            \
}                                                 \
virtual Value                                     \
Eval(Value const& key,                            \
     Evaluator* evaluator,                        \
     Arguments& arguments,                        \
     Value& result)                               \
{                                                 \
  return NativeEval(key, evaluator, arguments);   \
}

//////////////////////////////////////////////////////////////////////////
//
// Some easy macros
//

#define IMPL_NATIVE_CALLS(class, base)  \
  IMPL_NATIVE_GET (class, base)         \
  IMPL_NATIVE_SET (class, base)         \
  IMPL_NATIVE_EVAL(class, base)

#define DEF_NATIVE_CALLS(class, base)   \
  DEF_NATIVE_GET (class, base)          \
  DEF_NATIVE_SET (class, base)          \
  DEF_NATIVE_EVAL(class, base)

#endif // CSCRIPT_STUBS_H
