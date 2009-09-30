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
#include <gc.h>
#include <args.h>
#include <enumerator.h>

void PrintValue(std::ostream& stream, Value const& val, bool recurse = true);

//////////////////////////////////////////////////////////////////////////

class LineEnumerator : public Enumerator
{
public:

  LineEnumerator(File* file) : 
  m_file (file),
  m_line (0)
  {
  }

  virtual Object* GetSource() const
  {
    return m_file;
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
File::AssertReadable()
{
  // Check file is open
  if(!m_stream.is_open())
  {
    throw StreamNotOpen();
  }

  // Check for end of file
  if(m_stream.eof())
  {
    throw StreamAtEof();
  }

  // Check for other errors
  if(m_stream.bad())
  {
    throw StreamBadState();
  }

  // TODO Check file is in read mode
}

void 
File::AssertWritable()
{
  // Check file is open
  if(!m_stream.is_open())
  {
    throw StreamNotOpen();
  }

  // Check for other errors
  if(m_stream.bad())
  {
    throw StreamBadState();
  }

  // TODO Check file is in write mode
}

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
  // Close the current file
  if(m_stream.is_open())
  {
    m_stream.close();
  }
  
  // Clear all state flags
  m_stream.clear();
}

bool 
File::Eof()
{
  // Check for eof
  return m_stream.eof();
}

Value 
File::ReadChar()
{
  // Check readability
  AssertReadable();

  // Read character
  char ch;
  m_stream.get(ch);

  // Check read result
  if(m_stream.bad())
  {
    throw StreamReadFail();
  }

  // Return char
  return (int64) ch;
}

Value 
File::ReadString()
{
  // Check readability
  AssertReadable();

  // Read string
  char buf[4098];
  m_stream.get(buf, 4098);

  // Check read result
  if(m_stream.bad())
  {
    throw StreamReadFail();
  }

  // Return string
  return buf;
}

Value 
File::ReadLn()
{
  // Check readability
  AssertReadable();

  // Read line
  char buf[4098];
  m_stream.getline(buf, 4096);

  // Check read result
  if(m_stream.bad())
  {
    throw StreamReadFail();
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

void 
File::WriteChar(ValueCRef value)
{
  // Check writability
  AssertWritable();

  // Write character
  m_stream.put((char)value.GetInt());

  // Check write result
  if(m_stream.bad())
  {
    throw StreamWriteFail();
  }
}

void
File::Write(ArgsCRef args)
{
  // Check writability
  AssertWritable();

  // Enumerate arguments
  Arguments::const_iterator it;
  for(it = args.begin(); it != args.end(); ++it)
  {
    // Write argument
    PrintValue(m_stream, *it);

    // Check write result
    if(m_stream.bad())
    {
      throw StreamWriteFail();
    }
  }
}

void
File::WriteLn(ArgsCRef args)
{
  // Check writability
  AssertWritable();

  // Delegate to Write
  Write(args);

  // Send newline
  m_stream << std::endl;

  // Check write result
  if(m_stream.bad())
  {
    throw StreamWriteFail();
  }
}

ObjectPtr 
File::Lines()
{
  // Create a new line iterator
  return new ::Lines(this);
}
