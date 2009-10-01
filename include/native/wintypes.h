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
#ifndef CSCRIPT_NATIVE_WINTYPES_H
#define CSCRIPT_NATIVE_WINTYPES_H

//
// Base type for Windows api types
//
class WinapiType : public Object 
{
public:

  DEF_NATIVE_CALLS(WinapiType, Object);

  //
  // Retrieve argument size
  //
  virtual size_t GetArgSize() = 0;

  //
  // Retrieve argument pointer
  //
  virtual void* GetArgPtr() = 0;

  //
  // Convert to integer
  //
  __native_method virtual int64 ToInt() = 0;

  //
  // Convert to string
  //
  __native_method virtual String ToString() = 0;

};

//////////////////////////////////////////////////////////////////////////

template <typename T>
class WinapiIntT : public WinapiType
{
public:

  //
  // Construction
  //
  WinapiIntT(int64 value) 
    : m_data ((T)value)
  {
  }

  //
  // Retrieve argument size
  //
  size_t GetArgSize()
  {
    return sizeof(T);
  }

  //
  // Retrieve argument pointer
  //
  void* GetArgPtr()
  {
    return &m_data;
  }

  //
  // Convert to number
  //
  int64 ToInt()
  {
    return m_data;
  }

  //
  // Convert to string
  //
  String ToString()
  {
    return ValString((int64)m_data);
  }

private:

  //
  // Value
  //
  T m_data;

};

typedef WinapiIntT<uint8>   WinapiUint8;
typedef WinapiIntT<int8>    WinapiInt8;
typedef WinapiIntT<uint16>  WinapiUint16;
typedef WinapiIntT<int16>   WinapiInt16;
typedef WinapiIntT<uint32>  WinapiUint32;
typedef WinapiIntT<int32>   WinapiInt32;
typedef WinapiIntT<uint64>  WinapiUint64;
typedef WinapiIntT<int64>   WinapiInt64;

//////////////////////////////////////////////////////////////////////////

class WinapiStringBuf : public WinapiType
{
public:

  //
  // Construction
  //
  WinapiStringBuf(int64 size)
  {
    m_data = new char[(size_t)size + 1];
    m_size = (size_t)size;
  }

  //
  // Destruction
  //
  ~WinapiStringBuf()
  {
    delete m_data;
  }

  //
  // Retrieve data as integer
  //
  int64 ToInt()
  {
    return ValInt(Value(m_data));
  }

  //
  // Retrieve data as string
  //
  String ToString()
  {
    return m_data;
  }

  //
  // Retrieve argument size
  //
  virtual size_t GetArgSize()
  {
    return 4;
  }

  //
  // Retrieve argument
  //
  virtual void* GetArgPtr()
  {
    return &m_data;
  }

private:

  //
  // Members
  //
  char* m_data;
  int64 m_size;

};

#endif // CSCRIPT_NATIVE_WINTYPES_H
