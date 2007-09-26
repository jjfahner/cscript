#include "function.h"
#include "eval.h"
#include "parser.h"
#include "astlist.h"

AstList const* 
ScriptFunction::GetParameters() const
{
  return m_node->m_a2;
}

VariantRef 
ScriptFunction::Execute(Evaluator& evaluator, Arguments const& args)
{
  return evaluator.EvalScriptCall(this, args);
}

NativeFunction::NativeFunction(String decl, NativeCall call) :
Function  (""),
m_call    (call)
{
  // Create parser
  Reporter rep;
  Parser p(rep);

  // Parse declaration
  p.ParseText((decl + ";").c_str());
  if(rep.GetErrorCount())
  {
    throw std::logic_error("Invalid native function declaration");
  }

  // Extract name and parameter list
  m_name = p.GetRoot()->m_a1->m_a1;
  m_pars = p.GetRoot()->m_a1->m_a2;

  // Make sure the tree isn't deleted
  p.SetRoot(0);
}

VariantRef
NativeFunction::Execute(Evaluator& evaluator, Arguments const& args)
{
  return m_call(evaluator, args);
}

AstList const* 
ExternFunction::GetParameters() const
{
  return m_node->m_a2;
}

#ifdef WIN32

#include <windows.h>

VariantRef
ExternFunction::Execute(Evaluator& evaluator, Arguments const& args)
{
  // Load library
  HMODULE hModule = LoadLibrary(m_node->m_a3.GetString().c_str());
  if(hModule == 0)
  {
    throw std::runtime_error("Failed to load library");
  }

  // Find function address
  FARPROC proc = GetProcAddress(hModule, m_node->m_a1.GetString().c_str());
  if(proc == 0)
  {
    throw std::runtime_error("Failed to retrieve function pointer");
  }

  // Allocate memory for arguments
  int argbytes = args.size() * sizeof(int);
  int* stack = new int[args.size()];

  // Copy arguments into buffer stack
  size_t index = 0;
  AstList::const_reverse_iterator pi, pe;
  pi = m_node->m_a2.GetList()->rbegin();
  pe = m_node->m_a2.GetList()->rend();
  for(; pi != pe; ++pi, ++index)
  {
    Ast* par = (*pi);
    Ast* typ = par->m_a2;
    if(typ->m_type != builtin_type)
    {
      throw std::runtime_error("Invalid argument type");
    }
    switch(typ->m_a1.GetNumber())
    {
    case Variant::stInt:   // int
      stack[index] = (int)args[index]->GetInt();
      break;

    case Variant::stString:   // string
      stack[index] = (int)args[index]->GetString().c_str();
      break;

    default:
      delete [] stack;
      throw std::runtime_error("Invalid argument type");
    }
  }

  int dst;
  int res;

  // Make space on stack and copy address
   __asm sub esp, argbytes;
   __asm mov dst, esp

  // Copy and delete arguments
  memmove((void*)dst, stack, argbytes);
  delete [] stack;

	// Invoke native function
  __asm call proc;

  // Copy return value
  __asm mov res, eax;

  // Done
  return VariantRef(res);
}

#else

VariantRef
ExternFunction::Execute(Evaluator& evaluator, Arguments const& args)
{
  throw std::runtime_error("Extern calls not implemented on this platform");
}

#endif
