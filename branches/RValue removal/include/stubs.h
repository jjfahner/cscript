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

//
// Forward declare some required types
//
class RValue;
class Value;
class Object;

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
bool NativeCallContainsKey(struct NativeCall*, Object* instance, String const& key, bool checkProto);
bool NativeCallFind(struct NativeCall*, Object* instance, String const& key, RValue*& pValue, bool checkProto);

//
// Implement native calls for a class by using this macro
//
#define IMPL_NATIVECALLS(class, base) \
virtual bool \
ContainsKey(String const& key, bool checkProto = true) const \
{ \
  extern struct NativeCall __stublist_##class[]; \
  if(NativeCallContainsKey(__stublist_##class, (Object*)this, key, checkProto)) \
  { \
    return true;  \
  } \
  return base::ContainsKey(key, checkProto);  \
} \
virtual bool \
Find(String const& key, RValue*& pValue, bool checkProto = true) const \
{ \
  extern struct NativeCall __stublist_##class[]; \
  if(NativeCallFind(__stublist_##class, (Object*)this, key, pValue, checkProto))  \
  { \
    return true;  \
  } \
  return base::Find(key, pValue, checkProto); \
}

#endif // CSCRIPT_STUBS_H
