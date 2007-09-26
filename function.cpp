#include "function.h"
#include "eval.h"
#include "parser.h"

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

#ifdef WIN32

#include <windows.h>

VariantRef
ExternFunction::Execute(Evaluator& evaluator, Arguments const& args)
{/*
  // Load library
  HMODULE hModule = LoadLibrary(fun.m_code->m_a2.GetString().c_str());
  if(hModule == 0)
  {
    throw std::runtime_error("Failed to load library");
  }

  // Find function address
  FARPROC proc = GetProcAddress(hModule, fun.m_code->m_a1.GetString().c_str());
  if(proc == 0)
  {
    throw std::runtime_error("Failed to retrieve function pointer");
  }

  // Evaluate arguments
  std::vector<VariantRef> args;
  if(call->m_a2)
  {
    AstList::const_iterator ai, ae;
    ai = call->m_a2.GetList()->begin();
    ae = call->m_a2.GetList()->end();
    for(; ai != ae; ++ai)
    {
      args.push_back(EvalExpression(*ai));
    }
  }

  // Check argument count
  if(args.size() != fun.m_code->m_a4.GetList()->size())
  {
    throw std::runtime_error("Invalid number of arguments");
  }
  
  // Allocate memory for arguments
  int argbytes = args.size() * sizeof(int);
  int* stack = new int[args.size()];

  // Copy arguments into buffer stack in reverse order
  size_t argIndex = args.size() - 1;
  size_t parIndex = 0;
  AstList::const_reverse_iterator pi, pe;
  pi = fun.m_code->m_a4.GetList()->rbegin();
  pe = fun.m_code->m_a4.GetList()->rend();
  for(; pi != pe; ++pi, --argIndex, ++parIndex)
  {
    Ast* par = (*pi);
    switch(par->m_a2.GetNumber())
    {
    case 1:   // int
      stack[parIndex] = (int)args[argIndex]->GetInt();
      break;

    case 2:   // string
      stack[parIndex] = (int)args[argIndex]->GetString().c_str();
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
  */
  return VariantRef();
}

#else

VariantRef
ExternFunction::Execute(Evaluator& evaluator, Arguments const& args)
{
  throw std::runtime_error("Extern calls not implemented on this platform");
}

#endif
