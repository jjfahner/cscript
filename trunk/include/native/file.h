//////////////////////////////////////////////////////////////////////////
//
// This file is © 2009 - 2011 JJ Fahner <jan-jaap@jan-jaap.net>
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
#ifndef CSCRIPT_NATIVE_FILE_H
#define CSCRIPT_NATIVE_FILE_H

#include <native/stream.h>

#include <fstream>

__native_construct class File : public Object, public IOStream
{
public:

  DEF_NATIVE_CALLS(File, Object)

  //
  // Open file
  //
  __native_method void Open(StringCRef s_name, StringCRef s_mode = "r", bool b_binary = false, bool b_atend = false, bool b_truncate = false);

  //
  // Close file
  //
  __native_method void Close();

  //
  // At end of line
  //
  __native_roprop bool Eof();

  //
  // Read character
  //
  __native_method Value ReadChar();

  //
  // Read string
  //
  __native_method Value ReadString();

  //
  // Read line
  //
  __native_method Value ReadLn();

  //
  // Read complete file
  //
  __native_method Value ReadFile();

  //
  // Write character
  //
  __native_method void WriteChar(ValueCRef value);

  //
  // Write supplied arguments to output
  //
  __native_method void Write(ArgsCRef args);

  //
  // Write supplied arguments to output with automatic line feed
  //
  __native_method void WriteLn(ArgsCRef args);

  //
  // Line iterator
  //
  __native_roprop ObjectPtr Lines();

private:

  //
  // Assert readability. Throws if stream is not readable.
  //
  void AssertReadable();

  //
  // Assert writability. Throws if stream is not writable.
  //
  void AssertWritable();

  //
  // Members
  //
  friend class LineEnumerator;
  std::fstream m_stream;

};

#endif // CSCRIPT_NATIVE_FILE_H
