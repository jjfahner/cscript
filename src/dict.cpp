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
#include "dict.h"
#include "datatype.h"
#include "gc.h"

class DictionaryType : public ObjectType
{
public:

  static DictionaryType* Instance()
  {
    static DictionaryType m_type;
    GC::Pin(&m_type);
    return &m_type;
  }

  virtual String TypeName() 
  {
    return "dictionary";
  }

};

//////////////////////////////////////////////////////////////////////////

class DictEnumerator : public Enumerator
{
  Dictionary* m_dict;
  Dictionary::Iter m_it;
  Dictionary::Iter m_ie;

public:

  DictEnumerator(Dictionary* dict) : 
  m_dict (dict)
  {
    Reset();
  }

  virtual Object* GetSource() const
  {
    return m_dict;
  }

  virtual void Reset() 
  {
    m_it = m_dict->m_map.begin();
    m_ie = m_dict->m_map.end();
  }

  virtual bool GetNext(Value& value) 
  {
    if(m_it == m_ie) 
    {
      return false;
    }

    value = m_it->second;

    ++m_it;

    return true;
  }

  virtual bool GetNext(Value& key, Value& value)
  {
    if(m_it == m_dict->m_map.end())
    {
      return false;
    }

    key   = m_it->first;
    value = m_it->second;

    ++m_it;

    return true;
  }

  virtual void MarkObjects(GCObjectVec& grey)
  {
    GC::Mark(grey, m_dict);
  }

};

//////////////////////////////////////////////////////////////////////////

DataType* 
Dictionary::GetType()
{
  return DictionaryType::Instance();
}

Enumerator* 
Dictionary::GetEnumerator()
{
  return new DictEnumerator(this);
}

void 
Dictionary::MarkObjects(GCObjectVec& grey)
{
  // Mark object members
  Object::MarkObjects(grey);

  // Mark map contents
  for(Iter it = m_map.begin(); it != m_map.end(); ++it)
  {
    GC::Mark(grey, it->first);
    GC::Mark(grey, it->second);
  }
}
