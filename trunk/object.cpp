#include "object.h"
#include <set>
#include <deque>
#include <algorithm>

// Global object list
static Objects g_objects;

//////////////////////////////////////////////////////////////////////////

/*static*/ Object*
Object::Create()
{
  return new Object;
}

//////////////////////////////////////////////////////////////////////////
//
// Functor that deletes argument while protecting against exceptions.
//

template <typename T>
struct safe_deleter
{
  void operator () (T* ptr) const
  {
    try {
      delete ptr;
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
    it = obj->m_members.begin();
    ie = obj->m_members.end();
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

  // Delete invalid objects
  std::for_each(valid.begin(), valid.end(), 
                   safe_deleter<Object>());
}

//////////////////////////////////////////////////////////////////////////

Object::Object()
{
  g_objects.insert(this);
}
