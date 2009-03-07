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
#include "lexstream.h"
#include <istream>

LexStream::LexStream(std::istream& stream) :
m_cursor (m_buffer),
m_bufend (m_buffer),
m_marker (m_buffer),
m_token  (m_buffer),
m_lexeme (0),
m_stream (stream)
{
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
  m_token = m_cursor;
}

void 
LexStream::Flush()
{
  if(m_cursor != m_token)
  {
    m_lexeme->append(m_token, m_cursor);
    m_token = m_cursor;
  }
}

size_t 
LexStream::FillBuffer(size_t minRead)
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

  // Move remainder to start of buffer
  if(m_cursor < m_bufend)
  {
    size_t avail = m_bufend - m_cursor;
    memmove(m_buffer, m_cursor, avail);
    m_cursor = m_buffer;
    m_bufend = m_cursor + avail;
  }
  else
  {
    m_cursor = m_buffer;
    m_bufend = m_buffer;
  }

  // Reset token start
  m_token = m_buffer; 

  // Check for end of file
  if(m_stream.eof())
  {
    return m_bufend - m_cursor;
  }

  // Read from stream
  size_t offset = m_bufend - m_buffer;
  size_t length = bufsize - offset;
  m_stream.read(m_buffer + offset, length);

  // Check fill count
  size_t gcount = m_stream.gcount();
  if(gcount < minRead)
  {
    throw std::runtime_error("Unexpected end of file");
  }

  // Determine new buffer end
  m_bufend += gcount;

  // Done
  return m_bufend - m_cursor;
}
