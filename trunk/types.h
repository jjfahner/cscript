#ifndef CSCRIPT_TYPES_H
#define CSCRIPT_TYPES_H

#define _CRT_SECURE_NO_WARNINGS

#include <iostream>
#include <fstream>
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

typedef unsigned __int8   Byte;
typedef unsigned __int16  Word;
typedef unsigned __int32  Quad;

typedef __int64           int64;

#elif defined(__GNUC__)

#include <stdint.h>

typedef uint8_t           Byte;
typedef uint16_t          Word;
typedef uint32_t          Quad;

typedef int64_t           int64;

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
// Stream types
//
static std::istream& cin  = std::cin;
static std::ostream& cout = std::cout;
typedef std::ifstream   ifstream;
typedef std::ofstream   ofstream;

//
// String maps
//
typedef std::list<String> StringList;
typedef std::map<String, String> StringMap;

//
// Native call pointer
//
typedef void (*NativeCall)(class StackMachine&, Word);

//
// Function information
//
struct Function 
{
  String        m_name;
  bool          m_native;
  Quad          m_offset;
  Quad          m_minPar;
  Quad          m_maxPar;
  StringList    m_params;
  NativeCall    m_funPtr;
};

#endif // #ifndef CSCRIPT_TYPES_H
