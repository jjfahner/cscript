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
#ifndef CSCRIPT_FILE_H
#define CSCRIPT_FILE_H

#include "types.h"

//
// Header for compiled file
//
struct BinHeader
{
  // General
  Quad    m_magic;
  Quad    m_compver;
  Quad    m_machver;
  Quad    m_filelen;

  // Global code segment
  Quad    m_codeseg;
  Quad    m_codelen;

  // Procedure segment
  Quad    m_procseg;
  Quad    m_proclen;

  // Data segment
  Quad    m_dataseg;
  Quad    m_datalen;

  // Class segment
  Quad    m_vtabseg;
  Quad    m_vtablen;
};

//
// File wrapper
//
class File 
{
public:

  //
  // File types
  //
  enum FileTypes
  {
    empty,
    source,
    binary
  };

  //
  // Encoding types
  //
  enum EncTypes
  {
    UTF32BE,
    UTF32LE,
    UTF16BE,
    UTF16LE,
    UTF8
  };

  //
  // Construction
  //
  File();

  //
  // Destruction
  //
  ~File();

  //
  // Open a file
  //
  void Open(String const& path);

  //
  // Close current file
  //
  void Close();

  //
  // File type
  //
  FileTypes GetType() const;

  //
  // Binary header
  //
  BinHeader* GetHeader() const;

  //
  // Get path
  //
  String GetPath() const;

  //
  // Get length
  //
  Quad GetSize() const;

  //
  // Get pointer to file data
  //
  Byte* GetData() const;

private:

  //
  // Members
  //
  String      m_path;
  FileTypes   m_type;
  Byte*       m_buff;
  Byte*       m_data;
  Quad        m_size;

};

#endif // #ifndef CSCRIPT_FILE_H
