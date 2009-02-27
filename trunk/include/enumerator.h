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

class ObjectEnumerator : public Enumerator
{
  typedef Object::ValueIterator Iterator;

  Object*  m_obj;
  Iterator m_cur;

public:

  ObjectEnumerator(Object* object) :
  m_obj (object)
  {
    // Initialize iterator
    Reset();
  }

  virtual void Reset()
  {
    // Set iterator to start
    m_cur = m_obj->ValueBegin();
  }

  virtual bool GetNext(Value& value)
  {
    // Check current position
    if(m_cur == m_obj->ValueEnd())
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

//////////////////////////////////////////////////////////////////////////
//
// TODO move this somewhere
//

inline Enumerator* 
RValue::GetEnumerator() const
{
  return new ObjectEnumerator(GetValue());
}

#endif // CSCRIPT_ENUMERATOR_H
