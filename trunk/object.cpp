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
#include <set>
#include <deque>
#include <algorithm>

// Global object list
static Objects g_objects;

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
    }
  }
};

struct safe_finalizer
{
  void operator () (Object* ptr) const
  {
    try {
      ptr->Finalize();
    }
    catch(...) {
    }
  }
};

//////////////////////////////////////////////////////////////////////////

/*static*/ void 
Object::Collect(Objects valid)
{
  std::deque<Object*> stack;

  // Copy valid objects onto stack
  std::copy(valid.begin(), valid.end(),
    std::inserter(stack, stack.end()));

  // Clear list of valid objects
  valid.clear();

  // Walk stack until empty
  while(stack.size())
  {
    // Remove entry from stack
    Object* obj = stack.front();
    stack.pop_front();

    // Avoid repeating cycles
    if(valid.count(obj))
    {
      continue;
    }

    // Move from invalid to valid list
    g_objects.erase(obj);
    valid.insert(obj);

    // Walk object members
    ValueMap::const_iterator it, ie;
    it = obj->GetMembers().begin();
    ie = obj->GetMembers().end();
    for(; it != ie; ++it)
    {
      if(it->first.Type() == Value::tObject)
      {
        stack.push_back(&it->first.GetObject());
      }
      if(it->second.Type() == Value::tObject)
      {
        stack.push_back(&it->second.GetObject());
      }
    }
  }

  // Swap valid and invalid lists
  g_objects.swap(valid);

  // Finalize all objects to be deleted
  std::for_each(valid.begin(), valid.end(), 
                        safe_finalizer());

  // Delete invalid objects
  std::for_each(valid.begin(), valid.end(), 
                          safe_deleter());
}

//////////////////////////////////////////////////////////////////////////

Object::Object(Evaluator* eval) :
m_evaluator (eval),
m_members   (eval)
{
  g_objects.insert(this);
}

String 
Object::GetTypeName() const
{
  return String("Native") + typeid(*this).name();
}
