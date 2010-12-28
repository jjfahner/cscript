//////////////////////////////////////////////////////////////////////////
//
// This file is © 2009 - 2011 JJ Fahner <jan-jaap@jan-jaap.net>
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
#include "lexstream.h"

#include <istream>
#include <cstring>

#pragma warning(disable:4355)

LexStream::LexStream(std::istream& stream) :
m_cursor (m_buffer, *this),
m_bufend (m_buffer),
m_marker (m_buffer),
m_token  (m_buffer),
m_lexeme (0),
m_stream (stream),
m_line   (1),
m_char   (1)
{
  // Initialize buffer - valgrind says we should :)
  memset(m_buffer, 0, bufsize);
}

bool 
LexStream::Eof()
{
  return m_cursor == m_bufend && m_stream.eof();
}

void 
LexStream::Start(std::string* lexeme)
{
  m_lexeme = lexeme;
  m_token  = m_cursor;
  m_marker = m_cursor;
}

void 
LexStream::Flush()
{
  if(m_cursor != m_token)
  {
    if(m_lexeme)
    {
      m_lexeme->append(m_token, (char const*)m_cursor);
    }
    m_token = m_cursor;
  }
}

int
LexStream::FillBuffer(int minRead)
{
  // If there's enough available, ignore
  if(m_bufend - m_cursor > 0 &&
     m_bufend - m_cursor >= (int)minRead)
  {
    return m_bufend - m_cursor;
  }

  // Append currently parsed data to token
  if(m_cursor > m_token)
  {
    Flush();
  }

  // Check for end of file
  if(m_stream.eof())
  {
    return m_bufend - m_cursor;
  }

  // Move remainder to start of buffer
  if(m_cursor < m_bufend)
  {
    // Determine current offsets
    int pos_cursor = m_cursor - m_buffer;
    int pos_marker = m_marker - m_buffer;
    int pos_bufend = m_bufend - m_buffer;

    // Check for buffer space
    if(bufsize - pos_marker < minRead)
    {
      throw std::runtime_error("Lex buffer too small");
    }

    // Move remaining bytes to start of buffer
    int move = pos_bufend - pos_marker;
    memmove(m_buffer, m_buffer + pos_marker, move);

    // Adjust pointers
    m_marker = m_buffer;
    m_cursor = m_buffer + pos_cursor - pos_marker;
    m_bufend = m_buffer + pos_bufend - pos_marker;
  }
  else
  {
    // Reset pointers
    m_cursor = m_buffer;
    m_marker = m_buffer;
    m_bufend = m_buffer;
  }

  // Reset token start
  m_token = m_buffer; 

  // Read from stream
  size_t offset = m_bufend - m_buffer;
  size_t length = (bufsize / 2) - offset;
  m_stream.read(m_buffer + offset, length);

  // Check fill count
  int gcount = m_stream.gcount();
  if(gcount == 0)
  {
    // This check ought to read 'gcount < minRead', but that simply doesn't work...
    throw std::runtime_error("Unexpected end of file");
  }

  // Determine new buffer end
  m_bufend += gcount;

  // Done
  return m_bufend - m_cursor;
}
