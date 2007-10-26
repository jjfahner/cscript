//////////////////////////////////////////////////////////////////////////
//
// This file is © 2007 JJ Fahner <jan-jaap@jan-jaap.net>
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
#include "types.h"
#include "native.h"
#include "args.h"

class NativeFile : public Object
{
public:

  NativeFile() :
  m_file (0)
  {
  }

  virtual ~NativeFile()
  {
    Close();
  }

  Value Close()
  {
    if(m_file)
    {
      fclose(m_file);
      m_file = 0;
    }
    return true;
  }

  bool Open(Value const& name, Value const& mode)
  {
    // Close current file
    if(m_file)
    {
      Close();
    }

    // Open new file
    m_file = fopen(name.GetString().c_str(), 
                   mode.GetString().c_str());

    // Check result
    return m_file != 0;
  }

  Value Read(Value const& length)
  {
    // Check input
    if(m_file == 0)
    {
      return false;
    }

    // Allocate buffer
    size_t len = (int)length.GetInt();
    char*  buf = new char[len];

    // Read from file
    size_t res = fread(buf, len, 1, m_file);
    if(res != 1)
    {
      delete [] buf;
      return false;
    }

    // Copy to string
    String str(buf, len);

    // Free buffer
    delete [] buf;

    // Return string
    return Value(str);
  }

  Value Write(Value const& buf, Value const& bytes = 0)
  {
    // Check file
    if(m_file == 0)
    {
      return false;
    }

    // Determine write length
    size_t len = (int) bytes.GetInt();
    if(len == 0)
    {
      len = buf.GetString().length();
    }

    // Write buffer
    size_t res = fwrite(buf.GetString().c_str(), len, 1, m_file);
    if(res != 1)
    {
      return false;
    }

    // Succeeded
    return true;
  }

private:

  FILE* m_file;

};

//////////////////////////////////////////////////////////////////////////
//
// Native calls
//

NATIVE_CALL("__native fopen(string file, string mode)")
{
  // Create file
  NativeFile* f = new NativeFile;
  if(!f->Open(args[0], args[1]))
  {
    delete f;
    return false;
  }

  // Return new file
  return Value(f);
}

NATIVE_CALL("__native fread(file, int length)")
{
  // Extract file
  NativeFile* f = dynamic_cast<NativeFile*>(&args[0].GetObject());
  if(f == 0)
  {
    return false;
  }

  // Read from file
  return f->Read(args[1]);
}

NATIVE_CALL("__native fwrite(file, string buffer, int length = 0)")
{
  // Extract file
  NativeFile* f = dynamic_cast<NativeFile*>(&args[0].GetObject());
  if(f == 0)
  {
    return false;
  }

  // Write to file
  return f->Write(args[1], args[2]);
}

NATIVE_CALL("__native fclose(file)")
{
  // Extract file
  NativeFile* f = dynamic_cast<NativeFile*>(&args[0].GetObject());
  if(f == 0)
  {
    return false;
  }

  // Close the file
  f->Close();

  // Done
  return true;
}
