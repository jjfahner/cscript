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
#ifndef CSCRIPT_H
#define CSCRIPT_H

#include <stdexcept>
#include <string>

//
// Library auto-linking
//
#ifdef _MSC_VER
#ifndef CSLIB
#ifndef CSLIB_NO_AUTOLINK
#ifdef _DEBUG
#pragma comment(lib, "cslibd.lib")
#else
#pragma comment(lib, "cslib.lib")
#endif
#endif
#endif
#endif

//
// Compiler-dependent stuff
//
#ifdef _MSC_VER

typedef          __int8   int8;
typedef          __int16  int16;
typedef          __int32  int32;
typedef          __int64  int64;
typedef unsigned __int8   uint8;
typedef unsigned __int16  uint16;
typedef unsigned __int32  uint32;
typedef unsigned __int64  uint64;

typedef unsigned __int8   Byte;

#define strcmp_nocase _stricmp

#elif defined(__GNUC__)

#include <stdint.h>

typedef int8_t            int8;
typedef int16_t           int16;
typedef int32_t           int32;
typedef int64_t           int64;
typedef uint8_t           uint8;
typedef uint16_t          uint16;
typedef uint32_t          uint32;
typedef uint64_t          uint64;

typedef uint8_t           Byte;

#define strcmp_nocase strcasecmp

#else

#error Unknown compiler, cannot continue

#endif

//
// Char type
//
typedef char Char;

//
// String type
//
typedef std::string String;

//
// Marker for native methods
//
#define __native_method
#define __native_roprop
#define __native_rwprop

#endif // #ifndef CSCRIPT_H
