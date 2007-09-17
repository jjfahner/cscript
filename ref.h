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
#ifndef CSCRIPT_REF_H
#define CSCRIPT_REF_H

template <typename T>
class Ref 
{
public:

  //
  // Refcount - base for refcounted classes
  //
  struct Counted
  {
    virtual ~Counted() {}
  protected:
    Counted() : m_refs (0) {}
    Counted(Counted const&) : m_refs (0) {}
    Counted& operator = (Counted const&) { return *this; }
  private:
    friend class Ref<T>;
    size_t m_refs;
  };

  //
  // Empty construction
  //
  Ref() :
  m_counted (0)
  {
  }

  //
  // Construct from Counted
  //
  explicit Ref(T* counted) :
  m_counted (counted)
  {
    ++m_counted->m_refs;
  }

  //
  // Construct from reference to Counted
  //
  Ref(T const& counted) :
  m_counted (new T(counted))
  {
    ++m_counted->m_refs;
  }

  //
  // Construction from valid type for T
  //
  template <typename U>
  Ref(U const& rhs) :
  m_counted (new T(rhs))
  {
    ++m_counted->m_refs;
  }

  //
  // Copy construction
  //
  Ref(Ref const& rhs) :
  m_counted (0)
  {
    if(m_counted = rhs.m_counted)
    {
      ++m_counted->m_refs;
    }
  }

  //
  // Destruction
  //
  ~Ref()
  {
    Clear();
  }

  //
  // Release current ref
  //
  void Clear()
  {
    if(m_counted && --m_counted->m_refs == 0)
    {
      delete m_counted;
    }
    m_counted = 0;
  }

  //
  // State
  //
  bool Empty() const
  {
    return m_counted == 0;
  }

  //
  // Assignment
  //
  Ref const& operator = (Ref const& rhs)
  {
    if(m_counted != rhs.m_counted)
    {
      Clear();
      if(m_counted = rhs.m_counted)
      {
        ++m_counted->m_refs;
      }
    }
    return *this;
  }

  //
  // Dereferencing
  //
  T* operator -> () const
  {
    return m_counted;
  }
  T& operator * () const
  {
    return *m_counted;
  }

  //
  // Boolean conversion
  //
  operator bool () const
  {
    return m_counted != 0;
  }
  bool operator ! () const
  {
    return m_counted == 0;
  }

private:

  //
  // Members
  //
  T* m_counted;

};

#endif // #ifndef CSCRIPT_REF_H
