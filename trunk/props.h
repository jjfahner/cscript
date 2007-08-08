#ifndef CSCRIPT_PROPS_H
#define CSCRIPT_PROPS_H

#include "types.h"

typedef std::map<String, Quad> PropertyMap;

//////////////////////////////////////////////////////////////////////////
//
// Property abstraction. Validates whether property exists when reading,
// but allows writing to existing and non-existing properties.
//

class Property 
{
public:

  //
  // Construction
  //
  Property(PropertyMap& map, String const& idx) :
  m_map  (map),
  m_idx (idx)
  {
  }

  //
  // Explicit retrieval
  //
  Quad Get() const
  {
    PropertyMap::const_iterator it;
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
  operator Quad () const
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
  Quad operator * () const
  {
    return Get();
  }

  //
  // Assignment
  //
  Quad operator = (Quad value)
  {
    m_map[m_idx] = value;
    return value;
  }
  Quad operator += (Quad value)
  {
    value = Get() + value;
    m_map[m_idx] = value;
    return value;
  }

  //
  // Increment
  //
  Quad operator ++ ()
  {
    Quad value = Get();
    m_map[m_idx] = ++value;
    return value;
  }
  Quad operator ++ (int)
  {
    Quad value = Get();
    m_map[m_idx] = value + 1;
    return value;
  }


private:

  //
  // Members
  //
  PropertyMap& m_map;
  String       m_idx;

};

inline bool operator == (Property const& lhs, Property const& rhs)
{
  return *lhs == *rhs;
}

inline bool operator != (Property const& lhs, Property const& rhs)
{
  return *lhs != *rhs;
}

inline std::ostream& operator << (std::ostream& os, Property const& prop)
{
  return os << prop.Get();
}

//////////////////////////////////////////////////////////////////////////
//
// Property container
//

class Properties
{
public:

  //
  // Construction
  //
  Properties() : 
  m_properties (0)
  {
  }

  //
  // Destruction
  //
  ~Properties()
  {
    delete m_properties;
  }

  //
  // Indexing - uses delayed creation through Property
  //
  Property operator [] (String const& name)
  {
    if(m_properties == 0)
    {
      m_properties = new PropertyMap;
    }
    return Property(*m_properties, name);
  }

private:

  //
  // Members
  //
  PropertyMap* m_properties;

};



#endif // CSCRIPT_PROPS_H
