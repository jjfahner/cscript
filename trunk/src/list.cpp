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
#include <datatype.h>

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

  List*     m_list;
  int64     m_key;
  Iterator  m_cur;

public:

  ListEnumerator(List* list) :
  m_list  (list),
  m_key   (0),
  m_cur   (list->Begin())
  {
  }

  virtual Object* GetSource() const
  {
    return m_list;
  }

  virtual void Reset()
  {
    m_key = 0;
    m_cur = m_list->Begin();
  }

  virtual bool GetNext(Value& value)
  {
    if(m_cur == m_list->End())
    {
      return false;
    }

    value = *m_cur;

    ++m_key;
    ++m_cur;

    return true;
  }

  virtual bool GetNext(Value& key, Value& val)
  {
    if(m_cur == m_list->End())
    {
      return false;
    }

    key = m_key;
    val = *m_cur;

    ++m_key;
    ++m_cur;

    return true;
  }

};

//////////////////////////////////////////////////////////////////////////

DataType* 
List::GetType()
{
  return ListType::Instance();
}

Enumerator* 
List::GetEnumerator()
{
  return new ListEnumerator(this);
}
