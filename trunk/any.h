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
#ifndef CSCRIPT_ANY_H
#define CSCRIPT_ANY_H

#include <typeinfo>

class any 
{
public:

  //
  //  Default construction
  //
  any() : m_holder (0)
  {
  }

  //
  //  Copy construction
  //
  any(any const& rhs) : m_holder (0)
  {
    *this = rhs;
  }

  //
  //  Construction from any type
  //
  template <typename T>
  any(T const& t) : m_holder (new holder<T>(t))
  {
  }

  //
  //  Destruction
  //
  ~any()
  {
    clear();
  }

  //
  //  Variant state
  //
  bool empty() const
  {
    return m_holder == 0;
  }

  //
  //  Make empty
  //
  void clear()
  {
    if(m_holder)
    {
      holder_base* h = m_holder;
      m_holder = 0;
      delete h;
    }
  }

  //
  //  Swap values
  //
  void swap(any& rhs)
  {
    holder_base* h = m_holder;
    m_holder = rhs.m_holder;
    rhs.m_holder = h;
  }

  //
  //  Type info for current type
  //
  std::type_info const& typeinfo() const
  {
    if(m_holder)
    {
      return m_holder->typeinfo();
    }
    return typeid(void);
  }

  //
  //  Assignment
  //
  any const& operator = (any const& rhs)
  {
    if(&rhs != this)
    {
      clear();
      if(rhs.m_holder)
      {
        m_holder = rhs.m_holder->clone();
      }
    }
    return *this;
  }

  //
  //  Comparison
  //
  bool equals(any const& rhs) const
  {
    return m_holder ? m_holder->equals(rhs.m_holder) : 0;
  }

  //
  // Determine type
  //
  template <typename T>
  bool istype() const
  {
    return m_holder && dynamic_cast<holder<T>*>(m_holder) != 0;
  }

  //
  //  Implicit conversion
  //
  template <typename T>
  operator T const& () const
  {
    if(holder<T>* h = dynamic_cast<holder<T>*>(m_holder))
    {
      return h->m_value;
    }
    throw std::bad_cast();
  }

private:

  //
  //  holder_base allows any to be unaware of its content
  //
  class holder_base
  {
  public:

    //  Destruct virtually
    virtual ~holder_base() {}

    //  Duplicate this instance
    virtual holder_base* clone() const = 0;

    //  Retrieve type id for contained type
    virtual std::type_info const& typeinfo() const = 0;

    //  Compare to one another
    virtual bool equals(holder_base const*) const = 0;
  };

  //  
  //  holder implements the type-aware value holder
  //
  template <typename T>
  class holder : public holder_base
  {
  public:

    //  Contained type
    typedef T value_type;
    value_type m_value;

    //  Construction
    holder(T const& t) : m_value(t)
    {
    }

    //  Clone ourselves
    virtual holder_base* clone() const
    {
      return new holder<T>(*this);
    }

    //  Type info
    virtual std::type_info const& typeinfo() const
    {
      return typeid(m_value);
    }

    //  Comparison
    virtual bool equals(holder_base const* rhs) const
    {
      if(holder<T> const* t = dynamic_cast<holder<T> const*>(rhs))
      {
        return m_value == t->m_value;
      }
      return false;
    }

  };

  //
  //  Make any_cast friend
  //
  template <typename T>
  friend T& any_cast(any& v);
  template <typename T>
  friend T const& any_cast(any const& v);

  //
  //  Instance
  //
  holder_base* m_holder;

};

//
//  Explicit conversion
//
template <typename T>
inline T& any_cast(any& v)
{
  if(any::holder<T>* h = dynamic_cast<any::holder<T>*>(v.m_holder))
  {
    return h->m_value;
  }
  throw std::bad_cast();
}

template <typename T>
inline T const& any_cast(any const& v)
{
  if(any::holder<T> const* h = dynamic_cast<any::holder<T> const*>(v.m_holder))
  {
    return h->m_value;
  }
  throw std::bad_cast();
}

//
//  Comparison operators
//
inline bool 
operator == (any const& lhs, any const& rhs)
{
  return lhs.equals(rhs);
}

inline bool 
operator != (any const& lhs, any const& rhs)
{
  return !lhs.equals(rhs);
}

#endif  //  #ifndef VARIANT_H
