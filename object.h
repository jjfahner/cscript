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

  //////////////////////////////////////////////////////////////////////////
  //
  // Static members
  //

  //
  // Object factory
  //
  static Object* Create(Evaluator* eval);

  //
  // Invoke garbage collection.
  //
  static void Collect(Objects valid);

  //
  // Retrieve list of all objects
  //
  static Objects const& GetObjects();

  //////////////////////////////////////////////////////////////////////////
  //
  // Instance members
  //

  //
  // Virtual destruction
  //
  virtual ~Object()
  {
  }

  //
  // Object type
  //
  virtual String GetTypeName() const;

  //
  // Instance finalization
  //
  virtual void Finalize()
  {
  }

  //
  // Member values
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
