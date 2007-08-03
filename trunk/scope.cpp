#include "scope.h"

//////////////////////////////////////////////////////////////////////////
//
// Implementation of class Scope
//

Scope::Scope(Frame* frame, Scope* parent) :
m_frame   (frame),
m_parent  (parent)
{
}

Quad 
Scope::GetVarCount() const
{
  return (Quad)m_vars.size();
}

Quad 
Scope::AddVar(String const& name)
{
  // Check local scope
  if(m_vars.count(name))
  {
    std::cout << "Error: variable '" << name << "' already declared in this scope\n";
    return 0;
  }

  // Check for shadowing
  if(FindVarImpl(name).m_offset)
  {
    std::cout << "Warning: variable '" << name << "' hides a variable in a parent scope\n";
  }

  // Request id from frame
  Quad id = m_frame->MakeVarId();

  // Insert into variables
  m_vars[name] = id;

  // Done
  return id;
}

VarInfo 
Scope::FindVar(String const& name)
{
  // Find the variable offset
  VarInfo var = FindVarImpl(name);
  if(var.m_offset == 0)
  {
    std::cout << "Error: variable '" << name << "' is not declared in the current scope\n";
  }

  // Done
  return var;
}

VarInfo
Scope::FindVarImpl(String const& name)
{
  // Find variable in local scope
  Variables::iterator it;
  it = m_vars.find(name);
  if(it != m_vars.end())
  {
    return VarInfo(this, it->second);
  }

  // Find variable in parent scope(s)
  if(m_parent)
  {
    return m_parent->FindVarImpl(name);
  }

  // Unknown variable
  return VarInfo();
}

Scope* 
Scope::PushScope()
{
  return new Scope(m_frame, this);
}

Scope* 
Scope::PopScope()
{
  Scope* oldScope = m_parent;
  delete this;
  return oldScope;
}

Scope* 
Scope::PushFrame()
{
  return m_frame->PushFrame();
}

Scope* 
Scope::PopFrame()
{
  return m_frame->PopFrame();
}

//////////////////////////////////////////////////////////////////////////
//
// Implementation of class Frame
//

Frame::Frame(Frame* globals) :
Scope     (this, globals),
m_globals (globals),
m_varIds  (0)
{
}

Quad 
Frame::MakeVarId()
{
  return ++m_varIds;
}

VarInfo
Frame::FindVarImpl(String const& name)
{
  // Find in regular scope
  VarInfo var = Scope::FindVarImpl(name);
  if(var.m_offset)
  {
    return var;
  }

  // Find in global scope
  if(m_globals)
  {
    return m_globals->FindVarImpl(name);
  }

  // Failed
  return VarInfo();
}

Scope* 
Frame::PushFrame()
{
  return new Frame(m_globals ? m_globals : this);
}

Scope* 
Frame::PopFrame()
{
  Scope* scope = m_globals ? m_globals : 0;
  delete this;
  return scope;
}
