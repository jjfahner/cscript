#ifndef CSCRIPT_SCOPE_H
#define CSCRIPT_SCOPE_H

#include "types.h"

class Frame;
class Scope;

//////////////////////////////////////////////////////////////////////////
//
// Naming scope
//

class Scope
{
public:

  //
  // Construction
  //
  Scope(Frame* frame, Scope* parent);

  //
  // Add a variable to this scope
  //
  Quad AddVar(String const& name);

  //
  // Find a previously declared variable
  //
  Quad FindVar(String const& name);

  //
  // Push a deeper scope
  //
  Scope* PushScope();

  //
  // Pop this scope
  //
  Scope* PopScope();

  //
  // Push a stack frame
  //
  Scope* PushFrame();

  //
  // Pop a stack frame
  //
  Scope* PopFrame();

protected:

  //
  // Find the offset of a variable.
  //
  virtual Quad FindVarImpl(String const& name);

  //
  // Types
  //
  typedef std::map<String, Quad> Variables;

  //
  // Member data
  //

  Frame*    m_frame;
  Scope*    m_parent;
  Variables m_vars;

};

//////////////////////////////////////////////////////////////////////////
//
// Stack frame
//

class Frame : public Scope
{
public:

  //
  // Construction
  //
  Frame(Frame* globals = 0);

  //
  // Return new id
  //
  Quad MakeVarId();

  //
  // Push a stack frame
  //
  Scope* PushFrame();

  //
  // Pop a stack frame
  //
  Scope* PopFrame();

protected:

  //
  // Find the offset of a variable.
  //
  virtual Quad FindVarImpl(String const& name);

  //
  // Members
  //
  Frame*  m_globals;
  Quad    m_varIds;

};



#endif // CSCRIPT_SCOPE_H
