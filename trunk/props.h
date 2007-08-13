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
  ValueType Get() const
  {
    MapType::const_iterator it;
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
  operator ValueType () const
  {
    return Get();
  }
  operator bool () const
  {
    return Get() != 0;
  }
  bool operator ! () const
  {
    return Get() == 0;
  }

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
  ValueType operator += (ValueType value)
  {
    value = Get() + value;
    m_map[m_idx] = value;
    return value;
  }

  //
  // Increment
  //
  ValueType operator ++ ()
  {
    ValueType value = Get();
    m_map[m_idx] = ++value;
    return value;
  }
  ValueType operator ++ (int)
  {
    ValueType value = Get();
    m_map[m_idx]++;
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

template <typename T>
inline std::ostream& operator << (std::ostream& os, PropertyT<T> const& prop)
{
  return os << prop.Get();
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
