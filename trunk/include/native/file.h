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
#ifndef CSCRIPT_NATIVE_FILE_H
#define CSCRIPT_NATIVE_FILE_H

#include <native.h>
#include <fstream>

DECLARE_NATIVE_LINKAGE(File)

class File : public Object
{
public:

  DEF_NATIVE_CALLS(File, Object)

  //
  // Open file
  //
  __native_method Value Open(String s_name, String s_mode, bool b_binary, bool b_atend, bool b_truncate);

  //
  // Close file
  //
  __native_method Value Close();

  //
  // Read string
  //
  __native_method Value Read();

  //
  // Write string
  //
  __native_method Value Write(String data, int length);

private:

  //
  // Members
  //
  std::fstream m_stream;

};

#endif // CSCRIPT_NATIVE_FILE_H
