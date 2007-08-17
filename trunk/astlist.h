#ifndef CSCRIPT_ASTLIST_H
#define CSCRIPT_ASTLIST_H

#include "ast.h"

class AstList
{
  typedef std::list<Ast*> ListImpl;

public:

  typedef ListImpl::iterator iterator;
  typedef ListImpl::const_iterator const_iterator;

  AstList() :
  m_refs (0)
  {
  }

  AstList(AstList const& rhs)
  {
    *this = rhs;
  }

  ~AstList()
  {
    clear();
  }

  void push_back(Ast* node)
  {
    ++node->m_refs;
    m_list.push_back(node);
  }

  void clear()
  {
    while(m_list.size())
    {
      if(--m_list.front()->m_refs == 0)
      {
        delete m_list.front();
      }
      m_list.pop_front();
    }
    m_list.clear();
  }

  size_t size() const
  {
    return m_list.size();
  }

  iterator begin()
  {
    return m_list.begin();
  }

  iterator end()
  {
    return m_list.end();
  }

  const_iterator begin() const
  {
    return m_list.begin();
  }

  const_iterator end() const
  {
    return m_list.end();
  }

  AstList const& operator = (AstList const& rhs)
  {
    if(this != &rhs)
    {
      clear();
      const_iterator it = rhs.begin();
      const_iterator ie = rhs.end();
      for(; it != ie; ++it)
      {
        push_back(*it);
      }
    }
    return *this;
  }

private:


  std::list<Ast*> m_list;
  
  friend class AstData;
  int m_refs;

};

#endif // CSCRIPT_ASTLIST_H
