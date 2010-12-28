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
#ifndef CSCRIPT_NATIVE_WINAPI_H
#define CSCRIPT_NATIVE_WINAPI_H

#include <cscript.h>
#include <object.h>
#include <native.h>
#include <function.h>
#include <native/wintypes.h>

#ifdef _WIN32
# define WIN32_STDCALL __stdcall
#else
# define WIN32_STDCALL
#endif

class Winapi : public Object
{
public:

  IMPL_NATIVE_GET(Winapi, Object);
  DEF_NATIVE_SET (Winapi, Object);
  DEF_NATIVE_EVAL(Winapi, Object);

  //
  // Retrieves a module
  //
  bool TryGet(Value const& key, Value& value);

  //
  // Type creators
  //
  __native_method ObjectPtr StringBuf(int64 size) { return new WinapiStringBuf(size); }
  __native_method ObjectPtr Uint8(int64 value)    { return new WinapiUint8(value); }
  __native_method ObjectPtr Int8(int64 value)     { return new WinapiInt8(value); }
  __native_method ObjectPtr Uint16(int64 value)   { return new WinapiUint16(value); }
  __native_method ObjectPtr Int16(int64 value)    { return new WinapiInt16(value); }
  __native_method ObjectPtr Uint32(int64 value)   { return new WinapiUint32(value); }
  __native_method ObjectPtr Int32(int64 value)    { return new WinapiInt32(value); }
  __native_method ObjectPtr Uint64(int64 value)   { return new WinapiUint64(value); }
  __native_method ObjectPtr Int64(int64 value)    { return new WinapiInt64(value); }

};

//////////////////////////////////////////////////////////////////////////

class WinapiModule : public Object
{
public:

  //
  // Construction
  //
  WinapiModule(String const& name, void* hModule);

  //
  // Destruction
  //
  ~WinapiModule();

  //
  // Retrieve a function
  //
  virtual bool TryGet(Value const& key, Value& value);

private:

  //
  // Members
  //
  String m_name;
  void* m_hModule;

};

//////////////////////////////////////////////////////////////////////////

class WinapiFunction : public Function
{
public:
  
  //
  // Construction
  //
  WinapiFunction(WinapiModule* module, String const& name, void* hProc);

  //
  // Retrieve parameter list
  //
  virtual List* GetParameters() const;

  //
  // Execute function 
  //
  virtual Value Execute(Arguments& args);

private:

  //
  // Garbage collection
  //
  virtual void MarkObjects(GCObjectVec& grey);

  //
  // Members
  //
  WinapiModule* m_module;
  String        m_name;
  void*         m_hProc;

};

//////////////////////////////////////////////////////////////////////////

class WinapiStub : public Object
{
public:

  //
  // Retrieve (cached) stub code for function
  //
  static void* GetCodePtr(Function* fun);

  //
  // Construction
  //
  WinapiStub(Function* fun);

  //
  // Retrieve code
  //
  void* GetCodePtr() const;

private:

  //
  // Calculate the number of bytes for the ret statement
  //
  size_t CalcRetSize() const;

  //
  // Invoke stub
  //
  static unsigned int WIN32_STDCALL Invoke(WinapiStub* stub, unsigned int* stack);

  //
  // Members
  //
  Function*     m_pfun;
  unsigned char m_code[32];

};

#endif // CSCRIPT_NATIVE_WINAPI_H
