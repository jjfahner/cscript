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
#include <list.h>

//////////////////////////////////////////////////////////////////////////

class ListType : public ObjectType
{
public:

  static ListType* Instance()
  {
    static ListType m_type;
    GC::Pin(&m_type);
    return &m_type;
  }

  virtual String TypeName() 
  {
    return "list";
  }

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
    if(m_cur == m_end)
    {
      return false;
    }

    value = *m_cur++;

    return true;
  }

};

//////////////////////////////////////////////////////////////////////////

List::List() : 
Object(ListType::Instance())
{
}

Enumerator* 
List::GetEnumerator()
{
  return new ListEnumerator(this);
}

int64 
List::Length()
{
  return m_list.size();
}

Value 
List::Clear()
{
  m_list.clear();
  return Value();
}

Value 
List::Append(Value v)
{
  m_list.push_back(v);
  return Value();
}

Value 
List::Head()
{
  if(m_list.size())
  {
    return m_list.front();
  }
  throw std::runtime_error("List is empty");
}

Value 
List::Tail()
{
  if(m_list.size())
  {
    return m_list.back();
  }
  throw std::runtime_error("List is empty");
}

Value 
List::At(int64 index)
{
  return GetAt(index);
}

