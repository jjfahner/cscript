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
#ifndef CSCRIPT_VAR_H
#define CSCRIPT_VAR_H

#include "types.h"
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
    stReal,
    stString,
    stAssoc,
    stResource,
  };

  //
  // Constants
  //
  static const Variant Null;
  static const Variant True;
  static const Variant False;

  //
  // Map from variant to variant ref
  //
  typedef std::map<Variant, VariantRef> AssocMap;

  //
  // Base class for resources
  //
  class Resource
  {
  public:
    virtual ~Resource() {}
  protected:
    Resource() : m_refs (0) {}
  private:
    friend class Variant;
    int m_refs;
  };

  //
  // Implementation types
  //
  typedef bool        BoolType;
  typedef int64       IntType;
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
  // Const string construction
  //
  Variant(char const* value) :
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
    ++m_res->m_refs;
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
  // Empty variant
  //
  bool Empty() const
  {
    return m_type == stNull;
  }

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
  // Implicit boolean conversions
  //
  operator bool () const
  {
    return AsBool();
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
  // Unary operators
  //
  Variant operator - () const;
  Variant operator ++ (int);
  Variant operator -- (int);
  Variant const& operator ++ ();
  Variant const& operator -- ();
  Variant const& operator ! () const;

  //
  // Binary operators
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
Variant::operator - () const
{
  if(m_type != stInt)
  {
    throw std::runtime_error("Invalid subtype for unary minus");
  }
  Variant tmp(*this);
  tmp.m_int = -tmp.m_int;
  return tmp;
}

inline Variant 
Variant::operator ++ (int)
{
  Variant tmp(*this);
  ++(*this);
  return tmp;
}

inline Variant 
Variant::operator -- (int)
{
  Variant tmp(*this);
  --(*this);
  return tmp;
}

inline Variant const& 
Variant::operator ++ ()
{
  *this += 1;
  return *this;
}

inline Variant const& 
Variant::operator -- ()
{
  *this -= 1;
  return *this;
}

inline Variant const& 
Variant::operator ! () const
{
  return AsBool() ? Variant::False : Variant::True;
}

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

inline Variant 
operator == (Variant const& lhs, Variant const& rhs)
{
  return lhs.Compare(rhs) == 0;
}

inline Variant 
operator != (Variant const& lhs, Variant const& rhs)
{
  return lhs.Compare(rhs) != 0;
}

inline Variant 
operator < (Variant const& lhs, Variant const& rhs)
{
  return lhs.Compare(rhs) < 0;
}

inline Variant 
operator <= (Variant const& lhs, Variant const& rhs)
{
  return lhs.Compare(rhs) <= 0;
}

inline Variant 
operator > (Variant const& lhs, Variant const& rhs)
{
  return lhs.Compare(rhs) > 0;
}

inline Variant 
operator >= (Variant const& lhs, Variant const& rhs)
{
  return lhs.Compare(rhs) >= 0;
}

//////////////////////////////////////////////////////////////////////////
//
// Boolean operations
//

inline Variant 
operator || (Variant const& lhs, Variant const& rhs)
{
  return lhs.AsBool() || rhs.AsBool();
}

inline Variant 
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
