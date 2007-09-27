#ifndef CSCRIPT_ARGS_H
#define CSCRIPT_ARGS_H

#include "types.h"
#include "var.h"
#include "class.h"

class Arguments : public std::vector<VariantRef>
{
public:

  //
  // Instance
  //
  Instance* GetInstance() const
  {
    return m_instance;
  }
  void SetInstance(Instance* instance)
  {
    m_instance = instance;
  }
  void SetInstance(VariantRef const& instance)
  {
    m_instance = instance->GetTypedRes<Instance>();
  }

  //
  // Parameters
  //
  AstList const* GetParameters() const
  {
    return m_parameters;
  }
  void SetParameters(AstList const* parameters)
  {
    m_parameters = parameters;
  }

  //
  // Retrieve argument by name
  //
  using std::vector<VariantRef>::operator[];
  VariantRef const& operator [] (String const& name) const
  {
    return at(name);
  }

  //
  // Overload at using name
  //
  using std::vector<VariantRef>::at;
  VariantRef const& at(String const& name) const;

protected:

  //
  // Members
  //
  Instance*       m_instance;
  AstList const*  m_parameters;

};

#endif // CSCRIPT_ARGS_H
