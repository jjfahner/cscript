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

class NativeFile : public Variant::Resource
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

  Variant Close()
  {
    if(m_file)
    {
      fclose(m_file);
      m_file = 0;
    }
    return Variant::True;
  }

  bool Open(Variant const& name, Variant const& mode)
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

  Variant Read(Variant const& length)
  {
    // Check input
    if(m_file == 0)
    {
      return Variant::False;
    }

    // Allocate buffer
    size_t len = (int)length.GetInt();
    char*  buf = new char[len];

    // Read from file
    size_t res = fread(buf, len, 1, m_file);
    if(res != 1)
    {
      delete [] buf;
      return Variant::False;
    }

    // Copy to string
    Variant::StringType str(buf, len);

    // Free buffer
    delete [] buf;

    // Return string
    return Variant(str);
  }

  Variant Write(Variant const& buf, Variant const& bytes = 0)
  {
    // Check file
    if(m_file == 0)
    {
      return Variant::False;
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
      return Variant::False;
    }

    // Succeeded
    return Variant::True;
  }

private:

  FILE* m_file;

};

//////////////////////////////////////////////////////////////////////////
//
// Native calls
//

NATIVE_CALL("function fopen(string file, string mode)")
{
  ASSERT_TYPE(0, stString);
  ASSERT_TYPE(1, stString);

  // Create file
  NativeFile* f = new NativeFile;
  if(!f->Open(*args[0], *args[1]))
  {
    delete f;
    return Variant::False;
  }

  // Return new file
  return Variant(f);
}

NATIVE_CALL("function fread(file, int length)")
{
  ASSERT_TYPE(1, stInt);

  // Extract file
  NativeFile* f = dynamic_cast<NativeFile*>(args[0]->GetResource());
  if(f == 0)
  {
    return Variant::False;
  }

  // Read from file
  return f->Read(*args[1]);
}

NATIVE_CALL("function fwrite(file, string buffer, int length = 0)")
{
  // Extract file
  NativeFile* f = dynamic_cast<NativeFile*>(args[0]->GetResource());
  if(f == 0)
  {
    return Variant::False;
  }

  // Write to file
  if(args.size() == 2)
  {
    return f->Write(*args[1]);
  }
  else
  {
    return f->Write(*args[1], *args[2]);
  }
}

NATIVE_CALL("function fclose(file)")
{
  // Extract file
  NativeFile* f = dynamic_cast<NativeFile*>(args[0]->GetResource());
  if(f == 0)
  {
    return Variant::False;
  }

  // Close the file
  f->Close();

  // Done
  return Variant::True;
}
