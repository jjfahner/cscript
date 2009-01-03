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
#include "object.h"
#include "variable.h"

#include <typeinfo>
#include <set>
#include <deque>
#include <algorithm>

// Global object list
static Objects g_objects;
static Objects g_finalized;

//////////////////////////////////////////////////////////////////////////

/*static*/ Object*
Object::Create(Evaluator* evaluator)
{
  return new Object(evaluator);
}

/*static*/ Objects const& 
Object::GetObjects()
{
  return g_objects;
}

//////////////////////////////////////////////////////////////////////////

Object::Object(Evaluator* eval) :
m_evaluator (eval),
m_variables (eval)
{
  g_objects.insert(this);
}

Object::~Object()
{
  Variables::iterator it = m_variables.begin();
  for(; it != m_variables.end(); ++it)
  {
    delete it->second;
  }
}

String 
Object::GetTypeName() const
{
  return String("Native") + typeid(*this).name();
}

//////////////////////////////////////////////////////////////////////////
//
// Helpers for garbage collector
//

struct safe_deleter {
  void operator () (Object* ptr) const
  {
    try {
      delete ptr;
    }
    catch(...) {
      // TODO
    }
  }
};

struct safe_finalizer {
  void operator () (Object* ptr) const {
    try {
      ptr->Finalize();
    }
    catch(...) {
      // TODO
    }
  }
};

template <typename T, typename U>
void erase_from(T& cont, U from, U const& to) {
  for(; from != to; ++from) {
    cont.erase(*from);
  }
}

//////////////////////////////////////////////////////////////////////////
/*

The collection algorithm follows the classic tricolor method, with one
important modification: the traditional algorithm specifies that every
object is part of one set, whereas in this implementation, objects that
are reachable are temporarily in both the white and the grey set.

Invariants:

- White objects may be candidates for collection.
- Grey objects are reachable and may point to white objects.
- Black objects are reachable and may not point to white objects.
- Any object is either grey/white, or it is black.

Algorithm:

1.  Consider all objects white.
2.  Consider root objects grey.
3.  Consider the black set empty.
3.  For each grey object:
4.    Remove the grey object from the white set.
5.    Insert the grey object into the black set.
6.    Add objects referred to by the now black object to the grey set.
7.  Move black objects to the global list.
8.  Finalize white objects.
9.  Delete white objects.

*/
//////////////////////////////////////////////////////////////////////////

void 
MarkObjects(Objects& white, Objects& grey, Objects& black)
{
  // Walk stack until empty
  while(grey.size())
  {
    // Remove entry from grey list
    Object* obj = *grey.begin();
    grey.erase(grey.begin());

    // Avoid repeating cycles
    if(black.count(obj))
    {
      continue;
    }

    // Move from white to black
    white.erase(obj);
    black.insert(obj);

    // Walk object members
    Variables::const_iterator it, ie;
    it = obj->GetVariables().begin();
    ie = obj->GetVariables().end();
    for(; it != ie; ++it)
    {
      if(it->first.Type() == Value::tObject)
      {
        grey.insert(&it->first.GetObject());
      }
      if(it->second->GetValue().Type() == Value::tObject)
      {
        grey.insert(&it->second->GetValue().GetObject());
      }
    }
  }
}

/*static*/ void 
Object::Collect(Objects grey)
{
  Objects black;
  Objects white;
  Objects final;
  
  // Move globals into white set
  white.swap(g_objects);

  // Move reachable objects into black set
  MarkObjects(white, grey, black);

  // Copy finalizable objects into separate set
  std::remove_copy_if(white.begin(), white.end(), 
               std::inserter(final, final.end()), 
                      std::not1(std::mem_fun(
                        &Object::FinalizeRequired)));

  // Remove objects that were already finalized before
  erase_from(final, g_finalized.begin(), g_finalized.end());
  
  // Copy final to grey to keep final set intact
  // when calling MarkObjects on finalizable set
  grey = final;

  // Resurrect finalizable objects recursively
  MarkObjects(white, grey, black);

  // Move black list to global list
  black.swap(g_objects);

  // Delete white objects
  std::for_each(white.begin(), white.end(), safe_deleter());
  white.clear();

  // Handle finalized objects
  if(final.size())
  {
    // Finalize objects that have not been finalized
    std::for_each(final.begin(), final.end(), safe_finalizer());

    // Store list of objects not to finalize again
    g_finalized.swap(final);
  }
}
