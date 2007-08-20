//////////////////////////////////////////////////////////////////////////
//
// This file is � 2007 JJ Fahner <jan-jaap@jan-jaap.net>
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

typedef std::vector<VariantRef> RefStack;

//
// Native call pointer
//
typedef VariantRef (*NativeCall)(RefStack& stack, Quad numArgs);

//
// Function information
//
struct NativeCallInfo
{
  String        m_name;
  bool          m_native;
  Quad          m_offset;
  Quad          m_minPar;
  Quad          m_maxPar;
  StringList    m_params;
  NativeCall    m_funPtr;
};

//
// Registrar for native calls
//
struct NativeCallRegistrar
{
  NativeCallRegistrar(
    String const& name, 
    NativeCall call, 
    Quad minPar, 
    Quad maxPar);
};

//
// Native call handler
//
#define NATIVE_CALL(name,minPar,maxPar)                     \
  VariantRef Native_##name(RefStack&, Quad);                \
  NativeCallRegistrar register_##name(#name,                \
    Native_##name, minPar, maxPar);                         \
  VariantRef Native_##name(RefStack& args, Quad numArgs)

//
// Check the argument type for a native call argument
//
inline void 
AssertType(RefStack& args, Quad index, Variant::SubTypes type, char const* function);
#define ASSERT_TYPE(idx,type) \
  AssertType(args, idx, Variant::type, __FUNCTION__)


//
// Resolve a native call by name
//
NativeCallInfo* FindNative(String const& name);

//
// Execute a native call
//
void ExecNative(Quad index, Quad numArgs, RefStack& stack, Quad SP);


#endif // #ifndef CSCRIPT_NATIVE_CALLS_H