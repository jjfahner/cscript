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
#ifndef CSCRIPT_STACK_H
#define CSCRIPT_STACK_H

#include <cscript.h>
#include <value.h>
#include <cassert>

class Stack
{
public:

  //
  // Construction
  //
  Stack(int64 stacksize)
  {
    m_size  = stacksize;
    m_stack = (Value*)new char[(size_t)(stacksize * sizeof(Value))];
    m_top = m_stack + stacksize;
  }

  //
  // Destruction
  //
  ~Stack()
  {
    delete [] (char*)m_stack;
  }

  //
  // Push entry onto stack
  //
  void Push(Value const& value)
  {
    assert(m_top > m_stack);
    memcpy(--m_top, &value, sizeof(Value));
  }

  //
  // Pop and return top entry from stack
  //
  Value const& Pop()
  {
    assert(m_top - m_stack < m_size);
    return *(m_top++);
  }

  //
  // Retrieve top stack entry
  //
  Value const& Top()
  {
    assert(m_top - m_stack < m_size);
    return *m_top;
  }
  
  //
  // Retrieve value at offset
  //
  Value const& Get(int64 offset)
  {
    return *(m_top + offset);
  }

  //
  // Swap two entries
  //
  void Swap(int64 off1, int64 off2)
  {
    Value t = *(m_top + off1);
    *(m_top + off1) = *(m_top + off2);
    *(m_top + off2) = t;
  }

private:

  friend class StackFrame;

  //
  // Members
  //
  Value* m_stack;
  Value* m_top;
  int64  m_size;
};

//////////////////////////////////////////////////////////////////////////

class StackFrame
{
public:

  //
  // Construction
  //
  StackFrame(Stack* stack)
  {
    m_stack = stack;
    m_base  = stack->m_top;
  }

  //
  // Add variable
  //
  void Add(StringCRef name)
  {
    //if(m_)
  }


private:

  typedef std::map<String, int64> Offsets;

  //
  // Members
  //
  Stack*  m_stack;
  Value*  m_base;
  Offsets m_offset;

};

#endif // CSCRIPT_STACK_H
