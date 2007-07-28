#ifndef CSCRIPT_VAR_H
#define CSCRIPT_VAR_H

#define _CRT_SECURE_NO_WARNINGS

#include <string>
#include <map>
#include "ref.h"

class Variant;

typedef Ref<Variant> VariantRef;

class Variant : public Ref<Variant>::Counted
{
public:

  //
  // Subtypes
  //
  enum SubTypes
  {
    stNull,
    stBool,
    stInt,
    stString,
    stMap
  };

  //
  // Implementation types
  //
  typedef bool          BoolType;
  typedef __int64       IntType;
  typedef std::wstring  StringType;
  typedef std::map<Variant, VariantRef> MapType;

  //
  // Predicate for exact matching
  //
  struct LessExact {
    bool operator () (Variant const& lhs, Variant const& rhs) const {
      return lhs.Compare(rhs, true) < 0;
    }
  };

  //
  // Default construction
  //
  Variant() :
  m_int   (0),
  m_type  (stNull)
  {
  }
  
  //
  // Copy construction
  //
  Variant(Variant const& value) :
  m_int  (0),
  m_type (stNull)
  {
    *this = value;
  }

  //
  // Copy construction with conversion
  //
  Variant(Variant const& value, SubTypes type) :
  m_int  (0),
  m_type (stNull)
  {
    *this = value;
    SetType(type);
  }

  //
  // Boolean construction
  //
  Variant(BoolType value) :
  m_bool (value),
  m_type (stBool)
  {
  }
  
  //
  // Integer construction
  //
  Variant(IntType const& value) :
  m_int  (value),
  m_type (stInt)
  {
  }

  //
  // String construction
  //
  Variant(StringType const& value) :
  m_string (new StringType(value)),
  m_type   (stString)
  {
  }

  //
  // Map construction
  //
  Variant(MapType const& value) :
  m_map  (new MapType(value)),
  m_type (stMap)
  {
  }

  //
  // Value assignment
  //
  Variant const& operator = (Variant const& rhs);

  //
  // Clear contents
  //
  void Clear();

  //
  // Subtype
  //
  SubTypes GetType() const
  {
    return m_type;
  }

  //
  // Coerce type
  //
  void SetType(SubTypes type);

  //
  // Retrieve subtypes
  //
  BoolType GetBool() const
  {
    if(m_type != stBool)
    {
      throw std::runtime_error("Invalid subtype");
    }
    return m_bool;
  }
  IntType const& GetInt() const
  {
    if(m_type != stInt)
    {
      throw std::runtime_error("Invalid subtype");
    }
    return m_int;
  }
  StringType const& GetString() const
  {
    if(m_type != stString)
    {
      throw std::runtime_error("Invalid subtype");
    }
    return *m_string;
  }
  MapType const& GetMap() const
  {
    if(m_type != stMap)
    {
      throw std::runtime_error("Invalid subtype");
    }
    return *m_map;
  }

  //
  // Retrieve as other type
  //
  BoolType AsBool() const
  {
    if(m_type == stBool)
    {
      return m_bool;
    }
    return Variant(*this, stBool).GetBool();
  }
  IntType AsInt() const
  {
    if(m_type == stInt)
    {
      return m_int;
    }
    return Variant(*this, stInt).GetInt();
  }
  StringType AsString() const
  {
    if(m_type == stString)
    {
      return *m_string;
    }
    return Variant(*this, stString).GetString();
  }
  MapType AsMap() const
  {
    if(m_type == stMap)
    {
      return *m_map;
    }
    return Variant(*this, stMap).AsMap();
  }

  //
  // Comparison
  //
  int Compare(Variant const& rhs, bool exact = false) const;

  //
  // Operators
  //
  Variant const& operator += (Variant const& value);
  Variant const& operator -= (Variant const& value);
  Variant const& operator *= (Variant const& value);
  Variant const& operator /= (Variant const& value);
  Variant const& operator %= (Variant const& value);

  //
  // Map index
  //
  VariantRef const& operator [] (Variant const& index)
  {
    SetType(stMap);
    VariantRef& ref = (*m_map)[index];
    if(!ref) ref = VariantRef(new Variant);
    return ref;
  }

  //
  // Read/write memory location
  //
  void Read(unsigned char* address);
  void Write(unsigned char* address) const;
  size_t WriteLength() const;

private:

  //
  // Type conversion implementation
  //
  void MakeBool();
  void MakeInt();
  void MakeString();
  void MakeMap();

  //
  // Current type
  //
  SubTypes m_type;

  //
  // Subtypes
  //
  union {
    BoolType    m_bool;
    IntType     m_int;
    MapType*    m_map;
    StringType* m_string;
  };  

};

//////////////////////////////////////////////////////////////////////////
//
// Mathematical operations
//

inline Variant 
operator + (Variant const& lhs, Variant const& rhs)
{
  Variant tmp(lhs);
  tmp += rhs;
  return tmp;
}

inline Variant 
operator - (Variant const& lhs, Variant const& rhs)
{
  Variant tmp(lhs);
  tmp -= rhs;
  return tmp;
}

inline Variant 
operator * (Variant const& lhs, Variant const& rhs)
{
  Variant tmp(lhs);
  tmp *= rhs;
  return tmp;
}

inline Variant 
operator / (Variant const& lhs, Variant const& rhs)
{
  Variant tmp(lhs);
  tmp /= rhs;
  return tmp;
}

inline Variant 
operator % (Variant const& lhs, Variant const& rhs)
{
  Variant tmp(lhs);
  tmp %= rhs;
  return tmp;
}

//////////////////////////////////////////////////////////////////////////
//
// Comparison
//

inline bool 
operator == (Variant const& lhs, Variant const& rhs)
{
  return lhs.Compare(rhs) == 0;
}

inline bool 
operator != (Variant const& lhs, Variant const& rhs)
{
  return lhs.Compare(rhs) != 0;
}

inline bool 
operator < (Variant const& lhs, Variant const& rhs)
{
  return lhs.Compare(rhs) < 0;
}

inline bool 
operator <= (Variant const& lhs, Variant const& rhs)
{
  return lhs.Compare(rhs) <= 0;
}

inline bool 
operator > (Variant const& lhs, Variant const& rhs)
{
  return lhs.Compare(rhs) > 0;
}

inline bool 
operator >= (Variant const& lhs, Variant const& rhs)
{
  return lhs.Compare(rhs) >= 0;
}

//////////////////////////////////////////////////////////////////////////
//
// Boolean operations
//

inline bool 
operator || (Variant const& lhs, Variant const& rhs)
{
  return lhs.AsBool() || rhs.AsBool();
}

inline bool 
operator && (Variant const& lhs, Variant const& rhs)
{
  return lhs.AsBool() && rhs.AsBool();
}

#endif // #ifndef CSCRIPT_VAR_H
