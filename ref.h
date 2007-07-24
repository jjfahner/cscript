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
  protected:
    Counted() : m_refs (0) {}
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
  // Assignment
  //
  Ref const& operator = (Ref const& rhs)
  {
    if(this != &rhs && m_counted != rhs.m_counted)
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
