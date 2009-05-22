//////////////////////////////////////////////////////////////////////////
//
// This file is © 2008 - 2009 JJ Fahner <jan-jaap@jan-jaap.net>
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
#ifndef CSCRIPT_ENUMERATOR_H
#define CSCRIPT_ENUMERATOR_H

#include <cscript.h>
#include <object.h>
#include <list.h>

class Enumerator : public Object
{
public:

  //
  // Setup virtual destruction
  //
  virtual ~Enumerator() {}

  //
  // Reset enumerator to first entry
  //
  virtual void Reset() = 0;

  //
  // Retrieve next value
  //
  virtual bool GetNext(Value& value) = 0;

};

//////////////////////////////////////////////////////////////////////////

class ListEnumerator : public Enumerator
{
  typedef List::Iterator Iterator;

  Iterator m_beg;
  Iterator m_cur;
  Iterator m_end;

public:

  ListEnumerator(List* list) :
  m_beg (list->Begin()),
  m_cur (m_beg),
  m_end (list->End())
  {
  }

  virtual void Reset()
  {
    m_cur = m_beg;
  }

  virtual bool GetNext(Value& value)
  {
    // Check current position
    if(m_cur == m_end)
    {
      return false;
    }

    // Retrieve value from iterator
    value = *m_cur;

    // Advance to next position
    ++m_cur;

    // Succeeded
    return true;
  }

};

#endif // CSCRIPT_ENUMERATOR_H
