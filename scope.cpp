#include "scope.h"
#include "eval.h"
#include "ast.h"

Instance* 
Class::CreateInstance(Evaluator& eval) const
{
  // Create new instance
  Instance* inst = new Instance(this);

  // Enumerate variable definitions
  try
  {
    NamedNodeMap::const_iterator it, ie;
    it = m_vars.begin();
    ie = m_vars.end();
    for(; it != ie; ++it)
    {
      // Evaluate initial value
      VariantRef value;
      if(it->second->m_a2.Empty())
      {
        value = Variant::Null;
      }
      else
      {
        value = eval.EvalExpression(it->second->m_a2);
      }

      // Instantiate member variable
      inst->m_vars[it->first] = value;
    }

    // Return new instance
    return inst;
  }
  catch(...)
  {
    // Do cleanup
    delete inst;
    throw;
  }
}
