#ifndef CSCRIPT_SCOPE_H
#define CSCRIPT_SCOPE_H

#include "types.h"
#include "ast.h"

class Scope 
{
public:

  //
  // Construction
  //
  Scope(Ast* node, Scope* parent) :
  m_node    (node),
  m_parent  (parent)
  {
  }

  //
  // Register a name
  //
  int DeclareParameter(String const& name)
  {
    int id = MakeParameterId();
    m_names[name] = id;
    return id;
  }

  //
  // Declare a name
  //
  int DeclareVariable(String const& name)
  {
    int id = MakeVariableId();
    m_names[name] = id;
    return id;
  }

  //
  // Find a name
  //
  int Lookup(String const& name) const
  {
    Names::const_iterator it;
    if((it = m_names.find(name)) != m_names.end())
    {
      return it->second;
    }
    if(m_node->m_type != function_declaration && m_parent)
    {
      return m_parent->Lookup(name);
    }
    std::cout << "Error: variable or parameter '" << name << "' not found\n";
    return 0x7F000000; // TODO
  }

private:

  int MakeParameterId()
  {
    if(m_node->m_type == function_declaration)
    {
      return -int(++m_node->m_parcount);
    }
    throw std::logic_error("Invalid node for parameter declaration");
  }

  int MakeVariableId()
  {
    if(m_node->m_type == function_declaration)
    {
      return m_node->m_varcount++;
    }
    if(m_parent == 0)
    {
      return m_node->m_varcount++;
    }
    return m_parent->MakeVariableId();
  }


  typedef std::map<String, int> Names;

  //
  // Members
  //
  Scope*  m_parent;
  Ast*    m_node;
  Names   m_names;

};

#endif // CSCRIPT_SCOPE_H
