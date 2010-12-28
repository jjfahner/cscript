//////////////////////////////////////////////////////////////////////////
//
// This file is © 2009 - 2011 JJ Fahner <jan-jaap@jan-jaap.net>
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
#ifndef CSCRIPT_NATIVE_H
#define CSCRIPT_NATIVE_H

#include <cscript.h>
#include <object.h>

//
// Markers for native calls
//
#define __native_method
#define __native_roprop
#define __native_rwprop
#define __native_construct

//
// Forward declare some required types
//
class Evaluator;
class Arguments;

//
// Forward declare single-word types that have a wrapper function
//
typedef Object*             ObjectPtr;
typedef Value const&        ValueCRef;
typedef String const&       StringCRef;
typedef Arguments const&    ArgsCRef;
typedef Evaluator&          EvalRef;

//
// Create instance of native class
//
Value NativeCreate(Value const& className);

//
// Forward declare generic native call handlers
//
bool NativeCallTryGet (struct NativeCall*, Object*, Value const&, Value&);
bool NativeCallTrySet (struct NativeCall*, Object*, Value const&, Value const&);
bool NativeCallTryEval(struct NativeCall*, Object*, Value const&, Arguments&, Value&);

//////////////////////////////////////////////////////////////////////////
//
// Native get implementation
//

#define IMPL_NATIVE_GET(class, base)              \
bool                                              \
NativeTryGet(Value const& key, Value& value)      \
{                                                 \
  extern struct NativeCall                        \
                 cscript_native_table_##class[];  \
  if(NativeCallTryGet(                            \
            cscript_native_table_##class,         \
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
  extern struct NativeCall                        \
                 cscript_native_table_##class[];  \
  if(NativeCallTrySet(                            \
            cscript_native_table_##class,         \
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
              Arguments& arguments,               \
              Value& result)                      \
{                                                 \
  extern struct NativeCall                        \
                 cscript_native_table_##class[];  \
  if(NativeCallTryEval(                           \
             cscript_native_table_##class,        \
            this, key, arguments, result))        \
  {                                               \
    return true;                                  \
  }                                               \
  return base::TryEval(key, arguments, result);   \
}                                                 \
Value                                             \
NativeEval(Value const& key,                      \
           Arguments& arguments)                  \
{                                                 \
  Value result;                                   \
  if(TryEval(key, arguments, result))             \
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
        Arguments& arguments,                     \
        Value& result)                            \
{                                                 \
  return NativeTryEval(key, arguments, result);   \
}                                                 \
virtual Value                                     \
Eval(Value const& key,                            \
     Arguments& arguments,                        \
     Value& result)                               \
{                                                 \
  return NativeEval(key, arguments);              \
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

#endif // CSCRIPT_NATIVE_H
