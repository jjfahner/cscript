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
#include "gc.h"
#include "hash.h"

#include <map>

inline 
GCString::GCString() 
{
}

inline 
GCString::GCString(char const* str, size_t len, bool pin) : String(str, len) 
{ 
  if(pin) 
  {
    GC::Pin(this);
  }
}

//////////////////////////////////////////////////////////////////////////

class StringTable
{
public:

  StringTable()
  {
    m_maxLoad   = 0;
    m_numHits   = 0;
    m_numMisses = 0;
    m_numColls  = 0;
  }

  GCString* Lookup(char const* str, size_t len)
  {
    // Don't try to cache large strings
    if(len > 256)
    {
      return new GCString(str, len, false);
    }

    // Calculate hash for string
    HashType hash = CalcHash(str, len);

    // Lookup string in table
    Tuple& tuple = m_table[hash];
    if(tuple.second == 0)
    {
      // Cache miss
      ++m_numMisses;

      // Create new string
      tuple.first  = 1;
      tuple.second = new GCString(str, len, true);

      // Done
      return tuple.second;
    }
    else if(tuple.second->length() != len || *tuple.second != str)
    {
      // Collision
      ++m_numColls;

      // Collisions cannot not be cached
      return new GCString(str, len, false);
    }
    else
    {
      // Cache hit
      ++m_numHits;

      // Number of refs
      ++tuple.first;
      
      // Return existing string
      return tuple.second;
    }
  }

  void Collect()
  {
    Table table;

    // Iterate over strings in table
    for(Iter it = m_table.begin(), ie = m_table.end(); it != ie; ++it)
    {
      // Check use count
      if(it->second.first < 2)
      {
        // Zero or one references, forget
        GC::Unpin(it->second.second);
      }
      else
      {
        // More than one reference, keep
        table.insert(*it);
      }
    }
    
    // Swap tables
    m_table.swap(table);
  }

private:

  //
  // Table type
  //
  typedef std::pair<size_t, GCString*> Tuple;
  typedef std::map<HashType, Tuple> Table;
  typedef Table::iterator Iter;

  //
  // Members
  //
  Table   m_table;
  size_t  m_maxLoad;
  size_t  m_numHits;
  size_t  m_numMisses;
  size_t  m_numColls;
};

static StringTable g_table;

//////////////////////////////////////////////////////////////////////////

/*static*/ void 
GCString::Collect()
{
  g_table.Collect();
}

/*static*/ GCString* 
GCString::Create()
{
  return new GCString();
}

/*static*/ GCString*
GCString::Create(char const* str, size_t len, bool pin)
{
  len = len ? len : strlen(str);
  return pin ? 
    new GCString(str, len, true)  :
    g_table.Lookup(str, len) ;
}

/*static*/ GCString* 
GCString::Create(String const& str, bool pin)
{
  return Create(str.c_str(), str.length(), pin);
}
