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

#include "cscript.h"
#include "value.h"

class Evaluator;
class Arguments;

//
// Native call pointer
//
typedef Value (*NativeCall)(Evaluator* evaluator, Arguments const& args);

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
  static Value               UNIQUE_PREFIXED(native_)   (Evaluator* evaluator, Arguments const& args);   \
  static NativeCallRegistrar UNIQUE_PREFIXED(registrar_)(decl, UNIQUE_PREFIXED(native_));           \
  static Value               UNIQUE_PREFIXED(native_)   (Evaluator* evaluator, Arguments const& args)

#endif // #ifndef CSCRIPT_NATIVE_CALLS_H
