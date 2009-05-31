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
#include <native/file.h>
#include <eval.h>

void 
File::Open(StringCRef s_name, StringCRef s_mode, bool b_binary, bool b_atend, bool b_truncate)
{
  // Close current file
  Close();

  // Mode flag
  size_t o_mode = 0;

  // Direction
  if(s_mode == "r")  o_mode = std::ios::in;
  else if(s_mode == "w")  o_mode = std::ios::out;
  else if(s_mode == "rw") o_mode = std::ios::in|std::ios::out;

  // Modifiers
  if(b_binary)   o_mode |= std::ios::binary;
  if(b_atend)    o_mode |= std::ios::ate;
  if(b_truncate) o_mode |= std::ios::trunc;

  // Open the file
  m_stream.open(s_name.c_str(), (std::ios::openmode)o_mode);

  // Check file
  if(!m_stream.is_open() || m_stream.bad())
  {
    throw CatchableException("Failed to open file");
  }
}

void 
File::Close()
{
  // Close current file
  if(m_stream.is_open())
  {
    m_stream.close();
  }
}

Value 
File::Read()
{
  return Value();
}

void
File::Write(StringCRef data, int64 length)
{
  // Check file
  if(!m_stream.is_open())
  {
    throw CatchableException("Write to closed file");
  }

  // Determine length
  if(length == 0)
  {
    length = data.length();
  }

  // Check for no output
  if(length == 0)
  {
    return;
  }

  // Check for buffer underflow
  if(length > (int)data.length())
  {
    throw CatchableException("Buffer underflow");
  }

  // Write the string
  m_stream.write(data.c_str(), (int)length);

  // Check stream
  if(m_stream.bad())
  {
    throw CatchableException("Failed to write to file");
  }
}

//////////////////////////////////////////////////////////////////////////

// TODO
// NATIVE_CALL("CreateFile()")
// {
//   return new File();
// };
