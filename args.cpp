#include "args.h"
#include "astlist.h"

VariantRef const& 
Arguments::at(String const& name) const
{
  AstList::const_iterator it, ie;
  it = m_parameters->begin();
  ie = m_parameters->end();
  for(size_t index = 0; it != ie; ++it, ++index)
  {
    if((*it)->m_a1.GetString() == name)
    {
      return at(index);
    }
  }
  throw std::runtime_error("Function has no parameter '" + name + "'");
}
