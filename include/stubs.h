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
typedef Value& ValueRef;
typedef Value const& ValueCRef;
typedef String& StringRef;
typedef String const& StringCRef;

//
// Forward declare generic native call handlers
//
bool NativeCallTryGet(struct NativeCall*, Object* instance, Value const& key, Value& value);
bool NativeCallTrySet(struct NativeCall*, Object* instance, Value const& key, Value const& value);
bool NativeCallEvaluate(struct NativeCall*, Object* instance, Value const& key, Evaluator* evaluator, Arguments& arguments, Value& result);

//
// Implement native calls for a class by using this macro
//
#define IMPL_NATIVECALLS(class, base)             \
virtual bool                                      \
TryGet(Value const& key, Value& value)            \
{                                                 \
  extern struct NativeCall __stublist_##class[];  \
  if(NativeCallTryGet(__stublist_##class,         \
    (Object*)this, key, value))                   \
  {                                               \
    return true;                                  \
  }                                               \
  return base::TryGet(key, value);                \
}                                                 \
virtual Value Get(Value const& key)               \
{                                                 \
  Value value;                                    \
  if(TryGet(key, value))                          \
  {                                               \
    return value;                                 \
  }                                               \
  throw std::runtime_error("Cannot get value");   \
}                                                 \
virtual bool                                      \
TrySet(Value const& key, Value const& value)      \
{                                                 \
  extern struct NativeCall __stublist_##class[];  \
  if(NativeCallTrySet(__stublist_##class,         \
    (Object*)this, key, value))                   \
  {                                               \
    return true;                                  \
  }                                               \
  return base::TrySet(key, value);                \
}                                                 \
virtual Value const&                              \
Set(Value const& key, Value const& value)         \
{                                                 \
  if(TrySet(key, value))                          \
  {                                               \
    return value;                                 \
  }                                               \
  throw std::runtime_error("Cannot set value");   \
}                                                 \
virtual bool Evaluate(Value const& key,           \
      Evaluator* evaluator,                       \
      Arguments& arguments,                       \
      Value& result)                              \
{                                                 \
  extern struct NativeCall __stublist_##class[];  \
  if(NativeCallEvaluate(__stublist_##class,       \
    this, key, evaluator, arguments, result))     \
  {                                               \
    return true;                                  \
  }                                               \
  return base::Evaluate(key, evaluator,           \
                        arguments, result);       \
}

#endif // CSCRIPT_STUBS_H
