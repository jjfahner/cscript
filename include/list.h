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
#include <variable.h>
#include <gc.h>
#include <enumerator.h>
#include <stubs.h>

#include <vector>

class List : public Object
{
public:

  IMPL_NATIVECALLS(List, Object);
  __native_method Value Append(Value v);
  __native_method Value At(int64 index);
  __native_method Value Clear();
  __native_roprop Value Head();
  __native_roprop Value Tail();
  __native_roprop int64 Length();

  //
  // The implementation list type
  //
  typedef std::vector<MemberVariable> ImplType;

  //
  // Iterator type
  //
  typedef ImplType::iterator Iterator;

  //
  // Construction
  //
  List();

  //
  // Retrieve enumerator
  //
  virtual Enumerator* GetEnumerator();

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
  // Append new item
  //
  RValue& FastAppend(Value const& v = Value())
  {
    m_list.push_back(v);
    return m_list.back();
  }

  //
  // Append list contents
  //
  void Append(List* list)
  {
    m_list.insert(
      m_list.end(), 
      list->m_list.begin(), 
      list->m_list.end());
  }

  //
  // Retrieve at specified index
  //
  RValue& GetAt(int64 index)
  {
    if(index >= m_list.size())
    {
      m_list.resize((size_t)index + 1);
    }
    return m_list[(size_t)index];
  }

  //
  // Retrieve a member by index
  //
  virtual RValue& GetAt(Value const& index)
  {
    if(index.Type() != Value::tInt)
    {
      throw std::runtime_error("Invalid key type for list");
    }
    return GetAt(index.GetInt());
  }

  //
  // Implement garbage collector hook
  //
  virtual void MarkObjects(GC::ObjectVec& grey) 
  {
    // Mark object members
    Object::MarkObjects(grey);

    // Mark list entries
    Iterator it = m_list.begin();
    Iterator ie = m_list.end();
    for(; it != ie; ++it)
    {
      if(GC::Object* o = it->GetGCObject())
      {
        GC::Mark(grey, o);
      }
    }
  }

private:

  //
  // The list instance
  // 
  ImplType m_list;

};

#endif // CSCRIPT_LISTOBJECT_H
