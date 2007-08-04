#include "codegen.h"

void 
CodeGenerator::Generate(Ast* node, bool release)
{
  // Validate tree
  Validate(node);

  // Release optimizations
  if(release)
  {
    // Optimize tree
    Optimize(node);

    // Re-validate optimized tree
    Validate(node);
  }

  // Gather information
  m_scopeStack.push(Scope(node, 0));
  Annotate(node);

  // Print tree
  Print("cscript-tree.txt", node);

  // Generate initial stack frame
  PushByte(op_stackg);
  PushQuad(node->m_framesize);

  // Generate top-level code
  GenerateCode(node);

  // Generate halt instruction
  PushByte(op_halt);

  // Resolve calls to functions
  CallList::iterator ci, ce;
  ci = m_calls.begin();
  ce = m_calls.end();
  for(; ci != ce; ++ci)
  {
    // Find ast node for function
    Functions::iterator fi = m_funs.find(ci->first);
    if(fi == m_funs.end())
    {
      std::cout << "Error: function " << ci->first << " doesn't exist\n";
      continue;
    }

    // Generate function
    Quad offset = GenerateFunction(fi->second);

    // Fix offsets
    QuadList::iterator oi, oe;
    oi = ci->second.begin();
    oe = ci->second.end();
    for(; oi != oe; ++oi)
    {
      *(Quad*)(m_code + *oi) = offset;
    }
  }
  
  // Remember last code position
  Quad used = m_used;

  // Generate literals
  Literals::iterator li, le;
  li = m_literals.begin();
  le = m_literals.end();
  for(; li != le; ++li)
  {
    // Store current offset
    Quad offset = m_used;

    // Write literal
    size_t len = li->first.WriteLength();
    Reserve(m_used + len);
    li->first.Write(m_code + m_used);
    m_used += (Quad)len;

    // Patch with current offset
    QuadList::iterator qi, qe;
    qi = li->second.begin();
    qe = li->second.end();
    for(; qi != qe; ++qi)
    {
      *(Quad*)(m_code + *qi) = offset;
    }
    
  }

  // Decompile code
  std::ofstream ofs("cscript-dec.txt");
  Decompile(m_code, used, ofs);
}

Quad 
CodeGenerator::GenerateFunction(Ast* node)
{
  // Store function offset
  Quad fnOffset = m_used;

  // Push stack growth instruction
  PushByte(op_stackg);
  PushQuad(node->m_framesize);

  // Generate function content
  GenerateCode(node->m_a3);

  // Push return value
  PushByte(op_pushl);
  PushLiteral(Variant::Null);

  // Patch return statements
  while(m_returns.size())
  {
    FixPatch(m_returns.front());
    m_returns.pop_front();
  }

  // Write function epilog
  PushByte(op_store);
  PushQuad((Quad)-1);
  PushByte(op_stacks);
  PushQuad(node->m_framesize);

  // Generate return
  PushByte(op_ret);

  // Done
  return fnOffset;
}

void 
CodeGenerator::GenerateCode(Ast* node)
{
  // Offset storage
  Quad offset0;
  Quad offset1;

  // Generate type-specific code
  switch(node->m_type)
  {

    // TODO
  case foreach_statement:
    break;

  case include_statement:
    // Already handled by parser
    break;

  case statement_sequence:
  case parameter_list:
    {
      AstList* list = any_cast<AstList*>(node->m_a1);
      AstList::iterator si, se;
      si = list->begin();
      se = list->end();
      for(; si != se; ++si)
      {
        GenerateCode(*si);
      }
    }
    break;

  case return_statement:
    if(node->m_a1.empty())
    {
      GenerateCode(node->m_a1);
    }
    else
    {
      PushByte(op_pushl);
      PushLiteral(Variant::Null);
    }
    PushByte(op_jmp);
    m_returns.push_back(PushPatch());
    break;

  case compound_statement:
    if(any_cast<Ast*>(node->m_a1) != 0)
    {
      GenerateCode(node->m_a1);
    }
    break;

  case for_statement:
    GenerateCode(node->m_a1);
    offset0 = m_used;
    GenerateCode(node->m_a2);
    PushByte(op_jz);
    offset1 = PushPatch();
    GenerateCode(node->m_a4);
    GenerateCode(node->m_a3);
    PushByte(op_jmp);
    PushQuad(offset0);
    FixPatch(offset1);
    break;

  case if_statement:
    GenerateCode(node->m_a1);
    PushByte(op_jz);
    offset0 = PushPatch();
    GenerateCode(node->m_a2);
    if(node->m_a3.empty())
    {
      FixPatch(offset0);
    }
    else
    {
      PushByte(op_jmp);
      offset1 = PushPatch();
      FixPatch(offset0);
      GenerateCode(node->m_a3);
      FixPatch(offset1);
    }
    break;

  case while_statement:
    offset0 = m_used;
    GenerateCode(node->m_a1);
    PushByte(op_jz);
    offset1 = PushPatch();
    GenerateCode(node->m_a2);
    PushByte(op_jmp);
    PushQuad(offset0);
    FixPatch(offset1);
    break;

  case expression_statement:
    GenerateCode(node->m_a1);
    PushByte(op_pop);
    break;

  case assignment_expression:
    GenerateCode(node->m_a2);
    GenerateCode(node->m_a3);
    PushByte((opcodes)node->m_a1);
    break;

  case binary_expression:
    GenerateCode(node->m_a2);
    GenerateCode(node->m_a3);
    PushByte((opcodes)node->m_a1);
    break;

  case ternary_expression:
    GenerateCode(node->m_a1);
    PushByte(op_jz);
    offset0 = PushPatch();
    GenerateCode(node->m_a2);
    PushByte(op_jmp);
    offset1 = PushPatch();
    FixPatch(offset0);
    GenerateCode(node->m_a3);
    FixPatch(offset1);
    break;

  case literal:
    PushByte(op_pushl);
    PushLiteral(node->m_a1);
    break;

  case list_literal:
    break;

  case prefix_expression:
    GenerateCode(node->m_a2);
    PushByte((opcodes)node->m_a1);
    break;

  case postfix_expression:
    GenerateCode(node->m_a2);
    PushByte((opcodes)node->m_a1);
    break;

  case member_expression:
    break;

  case index_expression:
    GenerateCode(node->m_a1);
    GenerateCode(node->m_a2);
    PushByte(op_pushi);
    break;

  case function_call:
    GenerateFunctionCall(node);
    break;

  case argument_list:
    // Generate argument list in reverse order. This is done
    // to allow variadic arg lists, where named argument can
    // always be found at fixed offsets from ST.
    GenerateCode(node->m_a2);
    GenerateCode(node->m_a1);
    break;

  case argument:
    GenerateCode(node->m_a1);
    break;

  case lvalue:
    PushByte(op_pushv);
    PushQuad(node->m_stackpos);
    break;

  case variable_declaration:
    if(node->m_a2.empty())
    {
      PushByte(op_pushl);
      PushLiteral(Variant::Null);
      PushByte(op_store);
      PushQuad(node->m_stackpos);
    }
    else
    {
      GenerateCode(node->m_a2);
      PushByte(op_store);
      PushQuad(node->m_stackpos);
    }
    break;

  case declaration_sequence:
    GenerateCode(node->m_a1);
    GenerateCode(node->m_a2);
    break;

  case function_declaration:
    if(m_funs.count(node->m_a1))
    {
      std::cout << "Error: redeclaration of function " 
                << any_cast<String>(node->m_a1) << "\n";
    }
    else
    {
      m_funs[node->m_a1] = node;
    }
    break;

  case parameter:
    GenerateCode(node->m_a1);
    break;

  case empty_statement:
    break;

  default:
    std::cout << " " << node->m_type << " ";
    throw std::runtime_error("Unknown node type");
  }
}

void 
CodeGenerator::GenerateFunctionCall(Ast* node)
{
  // Generate argument list
  if(!node->m_a2.empty())
  {
    GenerateCode(node->m_a2);
  }

  // Push space for return value
  PushByte(op_pushl);
  PushLiteral(Variant::Null);

  // Determine call type


  // Push call to function
  PushByte(op_call);
  m_calls[node->m_a1].push_back(m_used);
  PushQuad(0);

  // Write cleanup code for argument list
  if(node->m_argcount)
  {
    PushByte(op_stackt);
    PushQuad(node->m_argcount);
  }
}
