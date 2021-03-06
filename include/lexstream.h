//////////////////////////////////////////////////////////////////////////
//
// This file is � 2009 - 2011 JJ Fahner <jan-jaap@jan-jaap.net>
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
#ifndef CSCRIPT_LEXSTREAM_H
#define CSCRIPT_LEXSTREAM_H

#include <cscript.h>
#include <iosfwd>

class LexStream
{
public:

  struct Cursor
  {
    char const* m_ptr;
    LexStream&  m_stream;
    Cursor(char const* ptr, LexStream& stream) : 
      m_ptr (ptr), m_stream (stream) {
    }
    char const* operator ++ () {
      if(*++m_ptr == '\n') {
        ++m_stream.m_line;
        m_stream.m_char = 0;
      }
      else {
        ++m_stream.m_char;
      }
      return m_ptr;
    }
    char operator * () {
      return *m_ptr;
    }
    operator char const*& () {
      return m_ptr;
    }
    char const* operator = (char const* ptr) {
      m_ptr = ptr;
      return *this;
    }
  };

  //
  // Construction from istream
  //
  LexStream(std::istream& stream);

  //
  // At end of input
  //
  bool Eof();

  //
  // Must be invoked each time before invoking re2c
  //
  void Start(std::string* lexeme = 0);

  //
  // Flush any parsed data to the current lexeme
  //
  void Flush();

  //
  // Fill buffer and adjust pointers, called by re2c
  //
  int FillBuffer(int minSize = 0);

  //
  // Pointers used by re2c
  //
  Cursor      m_cursor;
  char const* m_marker;
  char const* m_bufend;
  int         m_line;
  int         m_char;

private:

  //
  // Size of buffer
  //
  enum { bufsize = 1024 };

  //
  // Internal buffer
  //
  char m_buffer[bufsize];

  //
  // Offset of lexeme
  //
  char const* m_token;

  //
  // String to write into
  //
  std::string* m_lexeme;

  //
  // Source stream
  //
  std::istream& m_stream;

};

#endif
