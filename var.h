#ifndef CSCRIPT_VAR_H
#define CSCRIPT_VAR_H

#define _CRT_SECURE_NO_WARNINGS

#include "types.h"

#include <map>
#include "ref.h"

class Variant;

typedef Ref<Variant> VariantRef;

//
// Variant implementation
//
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
    stAssoc,
    stResource,
  };

  //
  // Map from variant to variant ref
  //
  typedef std::map<Variant, VariantRef> AssocMap;

  //
  // Base class for resources
  //
  struct Resource
  {
    virtual ~Resource() {}
    virtual Resource* Clone() const = 0;
  };

  //
  // Implementation types
  //
  typedef bool        BoolType;
  typedef __int64     IntType;
  typedef String      StringType;
  typedef AssocMap    AssocType;
  typedef Resource    ResType;

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
  m_bln   (value),
  m_type  (stBool)
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
  // Other integers
  //
  Variant(int value) :
  m_int   (value),
  m_type  (stInt)
  {
  }
  Variant(size_t value) :
  m_int   (value),
  m_type  (stInt)
  {
  }

  //
  // String construction
  //
  Variant(StringType const& value) :
  m_str   (new StringType(value)),
  m_type  (stString)
  {
  }

  //
  // Assoc construction
  //
  Variant(AssocType const& value) :
  m_map  (new AssocType(value)),
  m_type (stAssoc)
  {
  }

  //
  // Resource construction
  //
  Variant(ResType* value) :
  m_res   (value),
  m_type  (stResource)
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
  BoolType& GetBool()
  {
    if(m_type != stBool)
    {
      throw std::runtime_error("Invalid subtype");
    }
    return m_bln;
  }
  IntType& GetInt()
  {
    if(m_type != stInt)
    {
      throw std::runtime_error("Invalid subtype");
    }
    return m_int;
  }
  StringType& GetString()
  {
    if(m_type != stString)
    {
      throw std::runtime_error("Invalid subtype");
    }
    return *m_str;
  }
  AssocType& GetMap()
  {
    if(m_type != stAssoc)
    {
      throw std::runtime_error("Invalid subtype");
    }
    return *m_map;
  }
  ResType* GetResource()
  {
    if(m_type != stResource)
    {
      throw std::runtime_error("Invalid subtype");
    }
    return m_res;
  }
  template <typename T>
  T* GetTypedRes()
  {
    T* ptr;
    if(m_type != stResource || (ptr = dynamic_cast<T*>(m_res)) == 0)
    {
      throw std::runtime_error("Invalid subtype");
    }
    return ptr;
  }

  //
  // Retrieve as other type
  //
  BoolType AsBool() const
  {
    if(m_type == stBool)
    {
      return m_bln;
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
      return *m_str;
    }
    return Variant(*this, stString).GetString();
  }
  AssocType AsMap() const
  {
    if(m_type == stAssoc)
    {
      return *m_map;
    }
    return Variant(*this, stAssoc).AsMap();
  }
  
  //
  // Append item to map
  //
  void Append(VariantRef const& ref);

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
  VariantRef& operator [] (Variant const& index)
  {
    SetType(stAssoc);
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
    BoolType    m_bln;
    IntType     m_int;
    AssocType*  m_map;
    StringType* m_str;
    ResType*    m_res;
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


//////////////////////////////////////////////////////////////////////////
//
// Resource types
//

//
// Iterator resource for foreach implementation
//
struct IteratorRes : public Variant::Resource
{
  //
  // Iterator type
  //
  typedef Variant::AssocType::iterator Iterator;

  //
  // Construction
  //
  IteratorRes() {}
  IteratorRes(Iterator const& cur, Iterator const& end) : 
  m_cur (cur),
  m_end (end)
  {
  }
  IteratorRes(Variant::AssocType& map) :
  m_cur (map.begin()),
  m_end (map.end())
  {
  }

  //
  // Cloning
  //
  virtual IteratorRes* Clone() const {
    return new IteratorRes(m_cur, m_end);
  }

  //
  // Status
  //
  bool AtEnd() const
  {
    return m_cur == m_end;
  }

  //
  // Move forward
  //
  bool Next()
  {
    if(m_cur != m_end)
    {
      ++m_cur;
    }
    return m_cur != m_end;
  }


  //
  // Dereference
  //
  Iterator const& operator -> () const
  {
    return m_cur;
  }

  //
  // Iterator
  //
  Iterator m_cur;
  Iterator m_end;

};

#endif // #ifndef CSCRIPT_VAR_H
