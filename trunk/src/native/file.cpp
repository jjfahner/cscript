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

#include <fstream>

DEFINE_NATIVE_LINKAGE(File)

//////////////////////////////////////////////////////////////////////////

class File : public Object
{
public:

  //
  // Construction
  //
  File(Evaluator* evaluator)
  {
    NATIVE_METHOD(File, Open,  "Open(string path, string mode, bool binary = false, bool atend = false, bool truncate = false)");
    NATIVE_METHOD(File, Close, "Close()");
    NATIVE_METHOD(File, Read,  "Read()");
    NATIVE_METHOD(File, Write, "Write(string data, int length = 0)");
  }

  //
  // Open file
  //
  Value Open(Evaluator*, Arguments const& args)
  {
    // Close current file
    Close(0, Arguments());

    // Mode flag
    size_t o_mode = 0;

    // Direction
    String s_mode = args[1].GetString();
         if(s_mode == "r")  o_mode = std::ios::in;
    else if(s_mode == "w")  o_mode = std::ios::out;
    else if(s_mode == "rw") o_mode = std::ios::in|std::ios::out;

    // Modifiers
    if(args[2].GetBool()) o_mode |= std::ios::binary;
    if(args[3].GetBool()) o_mode |= std::ios::ate;
    if(args[4].GetBool()) o_mode |= std::ios::trunc;

    // Open the file
    m_stream.open(args[0].GetString().c_str(), o_mode);

    // Check file
    if(!m_stream.is_open() || m_stream.bad())
    {
      throw CatchableException(args.GetNode(), "Failed to open file");
    }

    // Return nothing
    return Value();
  }

  //
  // Close file
  //
  Value Close(Evaluator*, Arguments const& args)
  {
    // Close current file
    if(m_stream.is_open())
    {
      m_stream.close();
    }

    // No return value
    return Value();
  }
  
  //
  // Read string
  //
  Value Read(Evaluator*, Arguments const& args)
  {
    return Value();
  }

  //
  // Write string
  //
  Value Write(Evaluator*, Arguments const& args)
  {
    // Check file
    if(!m_stream.is_open())
    {
      throw CatchableException(args.GetNode(), "Write to closed file");
    }

    // Retrieve data
    String const& data = args[0].GetString();

    // Determine length
    size_t length = (size_t) args[1].GetInt();
    if(length == 0)
    {
      length = data.length();
    }

    // Check for no output
    if(length == 0)
    {
      return true;
    }

    // Check for buffer underflow
    if(length > data.length())
    {
      throw CatchableException(args.GetNode(), "Buffer underflow");
    }

    // Write the string
    m_stream.write(data.c_str(), length);

    // Check stream
    if(m_stream.bad())
    {
      throw CatchableException(args.GetNode(), "Failed to write to file");
    }

    // Return nothing
    return Value();    
  }

private:

  //
  // Members
  //
  std::fstream m_stream;

};

//////////////////////////////////////////////////////////////////////////

NATIVE_CALL("CreateFile()")
{
  return new File(evaluator);
};
