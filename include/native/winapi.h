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

class Winapi : public Object
{
public:

  DEF_NATIVE_SET (Winapi, Object);
  DEF_NATIVE_EVAL(Winapi, Object);

  //
  // Retrieves a module
  //
  bool TryGet(Value const& key, Value& value);

  //
  // Create a type
  //
  __native_method ObjectPtr StringBuf(int64 size);

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
  // Destruction
  //
  ~WinapiStub();

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
  static unsigned int __stdcall Invoke(WinapiStub* stub, unsigned int* stack);

  //
  // Members
  //
  Function* m_pfun;
  void*     m_code;
  size_t    m_size;

};

//////////////////////////////////////////////////////////////////////////

class WinapiType : public Object 
{
public:

  //
  // Retrieve argument size
  //
  virtual size_t GetArgSize() = 0;

  //
  // Retrieve argument
  //
  virtual void CopyArgData(void* dest) = 0;

};

//////////////////////////////////////////////////////////////////////////

class WinapiStringBuf : public WinapiType
{
public:

  DEF_NATIVE_CALLS(WinapiStringBuf, WinapiType);

  //
  // Construction
  //
  WinapiStringBuf(int64 size);

  //
  // Destruction
  //
  ~WinapiStringBuf();

  //
  // Retrieve data as string
  //
  __native_method String ToString();

  //
  // Retrieve argument size
  //
  virtual size_t GetArgSize();

  //
  // Retrieve argument
  //
  virtual void CopyArgData(void* dest);

private:

  //
  // Members
  //
  char* m_data;
  int64 m_size;

};

#endif // CSCRIPT_NATIVE_WINAPI_H
