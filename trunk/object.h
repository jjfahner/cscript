#ifndef CSCRIPT_OBJECT_H
#define CSCRIPT_OBJECT_H

#include "value.h"
#include <map>
#include <set>

typedef std::map<Value, Value> ValueMap;

class Object 
{
public:

  //
  // List of root objects
  //
  typedef std::set<Object*> Objects;

  //
  // Object factory. Forces heap
  // creation of Object instances.
  //
  static Object* Create();

  //
  // Invoke garbage collection.
  //
  static void Collect(Objects valid);

  //
  // Virtual destruction
  //
  virtual ~Object()
  {
  }

protected:

  //
  // Protected construction
  //
  Object();

  //
  // Object members
  //
  std::map<Value, Value> m_members;

};

#endif // CSCRIPT_OBJECT_H
