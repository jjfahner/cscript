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

#include <native/path.h>
#include <list.h>
#include <eval.h>

//////////////////////////////////////////////////////////////////////////

#if defined(_MSC_VER)
#include <windows.h>
#include <io.h>
#include <errno.h>
#elif defined(__GNUC__)
#include <unistd.h>
#include <dirent.h>
#else
#error Unknown compiler system
#endif

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
#if defined(_MSC_VER)
  return GetFileAttributes(filename.c_str()) != INVALID_FILE_ATTRIBUTES;
#elif defined(__GNUC__)
  return access(filename.c_str(), 0) == 0;
#endif
}

/*static*/ bool 
Path::IsAbsolute(String const& filename)
{
#ifdef WIN32
  return filename[0] == '\\' || filename[1] == ':';
#elif defined(__GNUC__)
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

String 
Path::WorkingDirectory()
{
  char buf[1024];
#ifdef WIN32
  GetCurrentDirectory(1024, buf);
#else
  getcwd(buf, 1024);
#endif
  return buf;
}

Value 
Path::GetFiles(StringCRef path)
{
  List* list = new List;

#ifdef WIN32
  
  String spec = Combine(path, "*");

  _finddata_t wfd;

  intptr_t hFind = _findfirst(spec.c_str(), &wfd);
  if(hFind == -1)
  {
    if(errno == ENOENT)
    {
      return new List();
    }
    throw CatchableException("Invalid path");
  }

  do
  {
    if(!(wfd.attrib & _A_SUBDIR))
    {
      list->Append(wfd.name);
    }
  } while(_findnext(hFind, &wfd) == 0);

  _findclose(hFind);

#else

  DIR *dp;
  struct dirent *ep;     
  
  dp = opendir(path.c_str());

  if (dp == 0)
  {
    throw CatchableException("Invalid path");
  }
  
  while (ep = readdir(dp))
  {
    if(ep->d_type == DT_REG)
    {
      list->Append(ep->d_name);
    }
  }
  
  closedir(dp);

#endif
  
  return list;
}

Value 
Path::GetDirectories(StringCRef path)
{
  List* list = new List();

#ifdef WIN32

  String spec = Combine(path, "*");

  _finddata_t wfd;

  intptr_t hFind = _findfirst(spec.c_str(), &wfd);
  if(hFind == -1)
  {
    if(errno == ENOENT)
    {
      return new List();
    }
    throw CatchableException("Invalid path");
  }

  do
  {
    if(wfd.attrib & _A_SUBDIR)
    {
      list->Append(wfd.name);
    }
  } while(_findnext(hFind, &wfd) == 0);

  _findclose(hFind);

#else

  DIR *dp;
  struct dirent *ep;

  dp = opendir(path.c_str());

  if (dp == 0)
  {
    throw CatchableException("Invalid path");
  }

  while(ep = readdir(dp))
  {
    if(ep->d_type == DT_DIR)
    {
      list->Append(ep->d_name);
    }
  }

  closedir(dp);

#endif
  
  return list;
}

