#ifndef CSCRIPT_FILE_H
#define CSCRIPT_FILE_H

#include "types.h"

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
  void Open(String const& filename);

  //
  // Close current file
  //
  void Close();

  //
  // File type
  //
  FileTypes GetType() const;

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
  FileTypes   m_type;
  Byte*       m_buff;
  Byte*       m_data;
  Quad        m_size;

};

#endif // #ifndef CSCRIPT_FILE_H
