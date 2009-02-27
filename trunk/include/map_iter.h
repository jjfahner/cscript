//////////////////////////////////////////////////////////////////////////
//
// This file is © 2009 JJ Fahner <jan-jaap@jan-jaap.net>
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
#ifndef CSCRIPT_MAPITER_H
#define CSCRIPT_MAPITER_H

//
// Accessor class used to access first element in pair
//
template <typename map_type>
struct key_accessor
{
  typedef typename map_type::key_type value_type;
  typedef typename map_type::iterator iterator;

  value_type& operator () (iterator const& iter)
  {
    return iter->first;
  }

  value_type const& operator () (iterator const& iter) const
  {
    return iter->first;
  }

};

//
// Accessor class used to access second element in pair
//
template <typename map_type>
struct value_accessor
{
  typedef typename map_type::mapped_type value_type;
  typedef typename map_type::iterator iterator;

  value_type& operator () (iterator const& iter)
  {
    return iter->second;
  }

  value_type const& operator () (iterator const& iter) const
  {
    return iter->second;
  }

};

template <typename T>
struct pointer_type {
  typedef T value_type;
};
template <typename T>
struct pointer_type<T*> {
  typedef T value_type;
};

template <typename map_type>
struct pointer_accessor
{
  typedef typename map_type::mapped_type mapped_type;
  typedef typename pointer_type<mapped_type>::value_type value_type;
  typedef typename map_type::iterator iterator;

  value_type& operator () (iterator const& iter)
  {
    return *iter->second;
  }

  value_type const& operator () (iterator const& iter) const
  {
    return *iter->second;
  }
};

//
// Iterator wrapper for std::map and std::multimap iterators
// that dereferences only the key or the value in the pair,
// serving as an adaptor to list, set and vector iterators.
//
template <typename map_type, template <typename> class accessor>
class map_iterator_t
{
  typedef accessor<map_type> accessor_type;
  typedef typename accessor_type::value_type value_type;
  typedef typename accessor_type::iterator iterator_type;
  typedef map_iterator_t<map_type, accessor> this_type;

public:

  map_iterator_t()
  {
  }


  map_iterator_t(iterator_type iter) :
  m_iter (iter)
  {
  }

  this_type& operator ++ ()
  {
    ++m_iter;
    return *this;
  }

  this_type operator ++ (int)
  {
    iterator_type iter(m_iter);
    ++m_iter;
    return iter;
  }

  value_type* operator -> ()
  {
    return &m_accessor(m_iter);
  }

  value_type const * operator -> () const
  {
    return &m_accessor(m_iter);
  }

  value_type& operator * ()
  {
    return m_accessor(m_iter);
  }

  value_type const& operator * () const
  {
    return m_accessor(m_iter);
  }

  bool operator == (this_type const& rhs) const
  {
    return m_iter == rhs.m_iter;
  }

  bool operator != (this_type const& rhs) const
  {
    return m_iter != rhs.m_iter;
  }

  this_type const& operator = (this_type const& rhs)
  {
    m_iter = rhs.m_iter;
    return *this;
  }

  this_type const& operator = (iterator_type const& rhs)
  {
    m_iter = rhs;
    return *this;
  }

private:

  iterator_type m_iter;
  accessor_type m_accessor;

};

#endif // CSCRIPT_MAPITER_H
