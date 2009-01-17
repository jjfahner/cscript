//////////////////////////////////////////////////////////////////////////
//
// This file is © 2007 - 2009 JJ Fahner <jan-jaap@jan-jaap.net>
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
#ifndef CSCRIPT_ASTLIST_H
#define CSCRIPT_ASTLIST_H

#include "ast.h"

class AstList
{
  typedef std::list<Ast*> ListImpl;

public:

  typedef ListImpl::iterator iterator;
  typedef ListImpl::const_iterator const_iterator;
  typedef ListImpl::reverse_iterator reverse_iterator;
  typedef ListImpl::const_reverse_iterator const_reverse_iterator;

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

  void adopt(AstList& rhs)
  {
    m_list.insert(m_list.end(),
            rhs.m_list.begin(), 
            rhs.m_list.end());

    rhs.m_list.clear();
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

  reverse_iterator rbegin()
  {
    return m_list.rbegin();
  }

  reverse_iterator rend()
  {
    return m_list.rend();
  }

  const_reverse_iterator rbegin() const
  {
    return m_list.rbegin();
  }

  const_reverse_iterator rend() const
  {
    return m_list.rend();
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
