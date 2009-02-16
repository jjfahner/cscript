//////////////////////////////////////////////////////////////////////////
//
// This file is © 2007 - 2009 JJ Fahner <jan-jaap@jan-jaap.net>
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

#include "srcfile.h"
#include <fstream>

//////////////////////////////////////////////////////////////////////////

#ifdef WIN32
#define PATH_SEPARATOR "\\"
#else
#define PATH_SEPARATOR "/"
#endif
#define PATH_SEPARATORS "\\/"

/*static*/ bool 
Path::Exists(String const& filename)
{
  return true; //_access(filename.c_str(), 0) == 0;
}

/*static*/ bool 
Path::IsAbsolute(String const& filename)
{
#ifdef WIN32
  return filename[0] == '\\' || filename[1] == ':';
#else
  return filename[0] == '/';
#endif
}

String 
Path::Combine(String const& lhs, String const& rhs)
{
  size_t rpos = lhs.find_last_not_of(PATH_SEPARATORS);
  size_t lpos = rhs.find_first_not_of(PATH_SEPARATORS);
  if(rpos == String::npos) rpos = lhs.length();
  if(lpos == String::npos) lpos = 0;
  return lhs.substr(0, rpos + 1) + PATH_SEPARATOR + rhs.substr(lpos);
}

String 
Path::DirectoryPart(String const& path)
{
  size_t rpos = path.find_last_of(PATH_SEPARATORS);
  if(rpos == String::npos) return "";
  return path.substr(0, rpos);
}

//////////////////////////////////////////////////////////////////////////

SourceFile::SourceFile() :
m_type (empty),
m_buff (0),
m_data (0),
m_size (0)
{
}

SourceFile::~SourceFile()
{
  Close();
}

void 
SourceFile::Close()
{
  if(m_buff)
  {
    free(m_buff);
    m_buff = 0;
    m_data = 0;
    m_size = 0;
  }
}

bool
SourceFile::Open(String const& path)
{
  // Open the input file
  std::ifstream ifs(path.c_str(), std::ios::binary);
  if(!ifs.good())
  {
    return false;
  }

  // Store path
  m_path = path;

  // Determine file size
  ifs.seekg(0, std::ios_base::beg);
  std::ios::pos_type sp = ifs.tellg();
  ifs.seekg(0, std::ios_base::end);
  std::ios::pos_type ep = ifs.tellg();
  ifs.seekg(0, std::ios_base::beg);
  std::streamsize len = ep - sp;
  
  // Allocate buffer
  Byte* buf = (Byte*)malloc(len + 4);

  // Read file and null-terminate
  ifs.read((char*)buf, len);
  *(int32*)(buf + len) = 0;

  // Close file
  ifs.close();

  // Store info
  m_buff = buf;
  m_data = buf;
  m_size = len;

  // Set source type
  m_type = source;

  // Determine encoding
  EncTypes enctype;
  if(m_size >= 4 && strncmp((char*)m_data, "\x00\x00\xfe\xff", 4) == 0)
  {
    enctype = UTF32BE;
    m_data += 4;
    m_size -= 4;
  }
  else if(len >= 4 && strncmp((char*)m_data, "\xff\xfe\x00\x00", 4) == 0)
  {
    enctype = UTF32LE;
    m_data += 4;
    m_size -= 4;
  }
  else if(len >= 3 && strncmp((char*)m_data, "\xef\xbb\xbf", 3) == 0)
  {
    enctype = UTF8;
    m_data += 3;
    m_size -= 3;
  }
  else if(len >= 2 && strncmp((char*)m_data, "\xfe\xff", 2) == 0)
  {
    enctype = UTF16BE;
    m_data += 2;
    m_size -= 2;
  }
  else if(len >= 2 && strncmp((char*)m_data, "\xff\xfe", 2) == 0)
  {
    enctype = UTF16BE;
    m_data += 2;
    m_size -= 2;
  }
  else
  {
    enctype = UTF8;
  }

  // Check supported types
  if(enctype != UTF8)
  {
    return false;
  }

  // Done
  return true;
}

String 
SourceFile::GetPath() const
{
  return m_path;
}

SourceFile::FileTypes 
SourceFile::GetType() const
{
  return m_type;
}

int32 
SourceFile::GetSize() const
{
  return m_size;
}

Byte* 
SourceFile::GetData() const
{
  return m_data;
}
