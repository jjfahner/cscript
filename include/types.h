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
#ifndef CSCRIPT_TYPES_H
#define CSCRIPT_TYPES_H

#include <stdexcept>
#include <algorithm>
#include <string>
#include <vector>
#include <stack>
#include <list>
#include <map>

//
// Base types
//
#ifdef _MSC_VER

#ifndef _LIB
#ifdef _DEBUG
#pragma comment(lib, "cslibd.lib")
#else
#pragma comment(lib, "cslib.lib")
#endif
#endif

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
// Convert a number to a string
//
#define TOSTRING2(a) #a
#define TOSTRING(a) TOSTRING2(a)


#define MAKEVER(major,minor,revision,patch) ((major<<24)|(minor<<16)|(revision<<8)|patch)

//
// Compiler version
//
#define VERSION (MAKEVER(0,6,0,0))

//
// Char type
//
typedef char Char;

//
// String type
//
typedef std::string String;

//
// String maps
//
typedef std::list<String> StringList;
typedef std::map<String, String> StringMap;

//
// File position
//
struct FilePos
{
  String  m_file;
  int32   m_line;
};
 

struct NCC {
  bool operator () (String const& l, String const& r) const {
    return strcmp_nocase(l.c_str(), r.c_str()) < 0;
  }
};

#endif // #ifndef CSCRIPT_TYPES_H
