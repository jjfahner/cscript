#ifndef CSCRIPT_OBJECT_H
#define CSCRIPT_OBJECT_H

#include <map>
#include <set>

#include "value.h"
#include "valuemap.h"

//
// List of root objects
//
typedef std::set<Object*> Objects;

//
// Object class
//
class Object 
{
public:

  //
  // Object factory. Forces heap
  // creation of Object instances.
  //
  static Object* Create(Evaluator* eval);

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

  //
  // Finalize instance
  //
  virtual void Finalize()
  {
  }

  //
  // Members
  //
  ValueMap& GetMembers()
  {
    return m_members;
  }
  ValueMap const& GetMembers() const
  {
    return m_members;
  }

protected:

  //
  // Protected construction
  //
  Object(Evaluator* eval);

  //
  // Object members
  //
  Evaluator*  m_evaluator;
  ValueMap    m_members;

};

#endif // CSCRIPT_OBJECT_H
