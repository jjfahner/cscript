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
#ifndef CSCRIPT_NATIVE_CALLS_H
#define CSCRIPT_NATIVE_CALLS_H

#include "types.h"
#include "var.h"
#include "eval.h"

class Evaluator;

//
// Native call pointer
//
typedef VariantRef (*NativeCall)(Evaluator& evaluator, Arguments const& args);

//
// Registrar for native calls
//
struct NativeCallRegistrar
{
  NativeCallRegistrar(String const& decl, NativeCall call);
};

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
  static VariantRef          UNIQUE_PREFIXED(native_)   (Evaluator& evaluator, Arguments const& args);   \
  static NativeCallRegistrar UNIQUE_PREFIXED(registrar_)(decl, UNIQUE_PREFIXED(native_));           \
  static VariantRef          UNIQUE_PREFIXED(native_)   (Evaluator& evaluator, Arguments const& args)

//
// Check the argument type for a native call argument
//
void AssertType(Arguments const& args, size_t index, Variant::SubTypes type, char const* function);
#define ASSERT_TYPE(idx,type) \
  AssertType(args, idx, Variant::type, __FUNCTION__)

//
// Resolve a native call by name
//
NativeCallInfo* FindNative(String const& name);

//
// Execute a native call
//
void ExecNative(int32 function, Evaluator& evaluator, Arguments const& args);


#endif // #ifndef CSCRIPT_NATIVE_CALLS_H
