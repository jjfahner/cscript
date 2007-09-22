//////////////////////////////////////////////////////////////////////////
//
// This file is © 2007 JJ Fahner <jan-jaap@jan-jaap.net>
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
#ifndef CSCRIPT_PROPS_H
#define CSCRIPT_PROPS_H

#include "types.h"
#include "var.h"

//////////////////////////////////////////////////////////////////////////
//
// Property abstraction. Validates whether property exists when reading,
// but allows writing to existing and non-existing properties.
//

template <typename T>
class PropertyT 
{
public:

  //
  // Types
  //
  typedef T ValueType;
  typedef std::map<String, ValueType> MapType;

  //
  // Construction
  //
  PropertyT(MapType& map, String const& idx) :
  m_map  (map),
  m_idx (idx)
  {
  }

  //
  // Explicit retrieval
  //
  ValueType& Get()
  {
    typename MapType::iterator it;
    it = m_map.find(m_idx);
    if(it == m_map.end())
    {
      throw std::logic_error("Attempt to read undefined property '" + m_idx + "'");
    }
    return it->second;
  }
  ValueType const& Get() const
  {
    typename MapType::const_iterator it;
    it = m_map.find(m_idx);
    if(it == m_map.end())
    {
      throw std::logic_error("Attempt to read undefined property '" + m_idx + "'");
    }
    return it->second;
  }

  //
  // Implicit conversions
  //
  operator ValueType& ()
  {
    return Get();
  }
  operator ValueType const& () const
  {
    return Get();
  }

  //
  // Templatized conversion
  //
  template <typename U>
  operator U () const
  {
    return any_cast<U>(Get());
  }

  //
  // Explicit conversion
  //
  template <typename U>
  U const& As() const
  {
    return any_cast<U const>(Get());
  }
  template <typename U>
  U& As()
  {
    return any_cast<U>(Get());
  }

  //
  // Some typed conversions
  //
//   operator int32() const
//   {
//     return any_cast<int32>(Get());
//   }
//   operator int() const
//   {
//     return any_cast<int>(Get());
//   }
//   operator bool() const
//   {
//     return any_cast<bool>(Get());
//   }
//   operator String() const
//   {
//     return any_cast<String>(Get());
//   }

  //
  // Dereference
  //
  ValueType operator * () const
  {
    return Get();
  }

  //
  // Assignment
  //
  ValueType operator = (ValueType value)
  {
    m_map[m_idx] = value;
    return value;
  }

private:

  //
  // Members
  //
  MapType& m_map;
  String   m_idx;

};

template <typename T>
inline bool operator == (PropertyT<T> const& lhs, PropertyT<T> const& rhs)
{
  return *lhs == *rhs;
}

template <typename T>
inline bool operator != (PropertyT<T> const& lhs, PropertyT<T> const& rhs)
{
  return *lhs != *rhs;
}

//////////////////////////////////////////////////////////////////////////
//
// PropertyT container
//

template <typename T>
class PropertiesT
{
public:

  //
  // Types
  //
  typedef T ValueType;
  typedef PropertyT<T> PropertyType;
  typedef std::map<String, T> MapType;

  //
  // Construction
  //
  PropertiesT() : 
  m_properties (0)
  {
  }

  //
  // Destruction
  //
  ~PropertiesT()
  {
    delete m_properties;
  }

  //
  // Property exists
  //
  bool contains(String const& key) const
  {
    return m_properties && m_properties->count(key) == 1;
  }

  //
  // Indexing - uses delayed creation through PropertyT
  //
  PropertyType operator [] (String const& name)
  {
    if(m_properties == 0)
    {
      m_properties = new MapType;
    }
    return PropertyType(*m_properties, name);
  }

private:

  //
  // Members
  //
  MapType* m_properties;

};



#endif // CSCRIPT_PROPS_H
