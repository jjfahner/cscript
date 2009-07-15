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
#include <enumerator.h>

//////////////////////////////////////////////////////////////////////////

class LineEnumerator : public Enumerator
{
public:

  LineEnumerator(File* file) : 
  m_file (file),
  m_line (0)
  {
  }

  virtual void Reset()
  {
    m_file->m_stream.seekg(std::ios::beg);
  }

  virtual bool GetNext(Value& value)
  {
    value = m_file->ReadLn();
    if(m_file->m_stream.good())
    {
      ++m_line;
      return true;
    }
    return false;
  }

  virtual bool GetNext(Value& key, Value& value)
  {
    if(GetNext(value))
    {
      key = m_line;
      return true;
    }
    return false;
  }

private:

  //
  // Mark referred objects
  //
  virtual void MarkObjects(GCObjectVec& grey)
  {
    Enumerator::MarkObjects(grey);
    GC::Mark(grey, m_file);
  }

  //
  // Members
  //
  File* m_file;
  int64 m_line;

};

//////////////////////////////////////////////////////////////////////////

class Lines : public Object
{
public:

  Lines(File* file) :
  m_file (file)
  {
  }

  virtual Enumerator* GetEnumerator()
  {
    return new LineEnumerator(m_file);
  }

private:

  virtual void MarkObjects(GCObjectVec& grey)
  {
    GC::Mark(grey, m_file);
  }

  File* m_file;

};

//////////////////////////////////////////////////////////////////////////

void 
File::Open(StringCRef s_name, StringCRef s_mode, bool b_binary, bool b_atend, bool b_truncate)
{
  // Close current file
  Close();

  // Mode flag
  size_t o_mode = 0;

  // Direction
  if(s_mode == "r")       o_mode = std::ios::in;
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
  m_stream.clear();
}

bool 
File::Eof()
{
  return m_stream.eof();
}

Value 
File::Read()
{
  return Value();
}

Value 
File::ReadLn()
{
  char buf[4098];

  // Test stream before read
  if(m_stream.bad())
  {
    return Value();
  }
  
  // Read line
  m_stream.getline(buf, 4096);

  // Test stream
  if(m_stream.bad())
  {
    return Value();
  }
  
  // Null-terminate buffer
  size_t len = m_stream.gcount();
  buf[len] = 0;

  // Return string
  return GCString::Create(buf, len ? len - 1 : 0);
}

Value 
File::ReadFile()
{
  // Remember file position
  std::istream::pos_type p = m_stream.tellg();

  // Seek to end and store length
  m_stream.seekg(0, std::ios::end);
  std::istream::pos_type e = m_stream.tellg();

  // Seek to start of file
  m_stream.seekg(0, std::ios::beg);
  std::istream::pos_type s = m_stream.tellg();

  // Calculate length
  size_t len = e - s;

  // Allocate buffer
  char* buf = new char[len + 1];

  // Read file
  m_stream.read(buf, len);

  // Terminate buffer
  buf[len] = 0;

  // Construct return value
  Value result = GCString::Create(buf, len);

  // Restore file position
  m_stream.seekg(p);

  // Done
  return result;
}

ObjectPtr 
File::Lines()
{
  // Create a new line iterator
  return new ::Lines(this);
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
