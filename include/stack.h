//////////////////////////////////////////////////////////////////////////
//
// This file is © 2007 - 2011 JJ Fahner <jan-jaap@jan-jaap.net>
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
#include <cstdlib>
#include <cstring>

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
  // Stack usage
  //
  int64 Used() const
  {
    return m_size - (m_top - m_stack);
  }

  //
  // Iterators
  //
  Value const* Begin() const {
    return m_top;
  }
  Value const* End() const {
    return m_stack + m_size;
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
  StackFrame(Stack& stack) :
  m_stack (stack),
  m_base  (stack.m_top)
  {
  }

  //
  // Destruction
  //
  ~StackFrame()
  {
    Return();
  }

  //
  // Return without pushing a value
  //
  void Return()
  {
    if(m_base)
    {
      m_stack.m_top = m_base;
      m_base = 0;
    }    
  }

  //
  // Return and push value
  //
  void Return(Value const& value)
  {
    Return();
    m_stack.Push(value);
  }

private:

  //
  // Members
  //
  Stack&  m_stack;
  Value*  m_base;

};

#endif // CSCRIPT_STACK_H
