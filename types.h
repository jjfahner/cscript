#ifndef CSCRIPT_TYPES_H
#define CSCRIPT_TYPES_H

#include <string>
#include <list>

//
// Base types
//
typedef unsigned __int8   Byte;
typedef unsigned __int16  Word;
typedef unsigned __int32  Quad;

//
// String type
//
typedef std::wstring String;

//
// Stringlist
//
typedef std::list<String> StringList;

//
// Native call pointer
//
typedef void (*NativeCall)(class StackMachine&);

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
