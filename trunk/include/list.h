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
#ifndef CSCRIPT_LISTOBJECT_H
#define CSCRIPT_LISTOBJECT_H

#include <cscript.h>
#include <gc.h>

#include <list>

class List : public GC::ComplexObject
{
public:

  //
  // The implementation list type
  //
  typedef std::list<MemberVariable> ListType;

  //
  // Iterator type
  //
  typedef ListType::iterator Iterator;

  //
  // Number of items in list
  //
  int Count() const
  {
    return (int)m_list.size();
  }

  //
  // Iterators
  //
  Iterator Begin() 
  {
    return m_list.begin();
  }
  Iterator End() 
  {
    return m_list.end();
  }

  //
  // Add to head of list
  //
  RValue& AddHead(Value const& v = Value())
  {
    m_list.push_front(v);
    return m_list.front();
  }

  //
  // Add to tail of list
  //
  RValue& AddTail(Value const& v = Value())
  {
    m_list.push_back(v);
    return m_list.back();
  }

  //
  // Retrieve item at front
  //
  RValue& Head()
  {
    if(m_list.size())
    {
      return m_list.front();
    }
    throw std::runtime_error("List is empty");
  }

  //
  // Retrieve item at back
  //
  RValue& Tail()
  {
    if(m_list.size())
    {
      return m_list.back();
    }
    throw std::runtime_error("List is empty");
  }

  //
  // Remove from front
  //
  void PopHead()
  {
    m_list.pop_front();
  }

  //
  // Remove from back
  //
  void PopTail()
  {
    m_list.pop_back();
  }

  //
  // Retrieve at specified index
  //
  RValue& GetAt(int index)
  {
    // Check bounds
    if(index < 0 || index >= Count())
    {
      throw std::runtime_error("Index out of bounds");
    }

    // Make iterator
    ListType::iterator it = m_list.begin();
    if(index)
    {
      std::advance(it, index);
    }
    
    // Done
    return *it;
  }

  //
  // Implement garbage collector hook
  //
  virtual void MarkObjects(GC::ObjectVec& grey) 
  {
    ListType::const_iterator it = m_list.begin();
    ListType::const_iterator ie = m_list.end();
    for(; it != ie; ++it)
    {
      if(GC::Object* o = it->GetGCObject())
      {
        grey.push_back(o);
      }
    }
  }

private:

  //
  // The list instance
  // 
  ListType m_list;

};

#endif // CSCRIPT_LISTOBJECT_H
