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
  operator ValueType () const
  {
    return Get();
  }

  //
  // Some typed conversions
  //
  operator Quad() const
  {
    return any_cast<Quad>(Get());
  }
  operator int() const
  {
    return any_cast<int>(Get());
  }
  operator bool() const
  {
    return any_cast<bool>(Get());
  }
  operator String() const
  {
    return any_cast<String>(Get());
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
