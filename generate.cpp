#include "codegen.h"
#include "native.h"
#include "file.h"

inline Variant LiteralAsVariant(Ast* node)
{
  return any_cast<Variant>(node->m_a1);
}

inline bool IsType(Ast* node, AstTypes type)
{
  return node->m_type == type;
}

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
  Annotate(node);

  // Reserve header space
  Reserve(sizeof(BinHeader));
  m_used += sizeof(BinHeader);
  memset(m_code, 0, sizeof(BinHeader));

  // Store start of global code segment
  ((BinHeader*)m_code)->m_codeseg = m_used;

  // Generate initial stack frame
  PushByte(op_stackg);
  PushQuad(node->m_framesize);

  // Generate top-level code
  GenerateCode(node);

  // Generate halt instruction
  PushByte(op_halt);

  // Store length of code segment
  ((BinHeader*)m_code)->m_codelen = m_used - ((BinHeader*)m_code)->m_codeseg;

  // Store start of function segment
  ((BinHeader*)m_code)->m_procseg = m_used;

  // Generate functions
  Functions::iterator fi, fe;
  fi = m_funs.begin();
  fe = m_funs.end();
  for(; fi != fe; ++fi)
  {
    // Generate function
    Quad offset = GenerateFunction(fi->second.first);

    // Store offset
    fi->second.second = offset;
  }

  // Resolve calls to functions
  CallList::iterator ci, ce;
  ci = m_calls.begin();
  ce = m_calls.end();
  for(; ci != ce; ++ci)
  {
    Byte opcode = 0;
    Quad offset = 0;

    // Find ast node for function
    Functions::iterator fi = m_funs.find(ci->first);
    if(fi == m_funs.end())
    {
      // Find native function
      NativeCallInfo* f = FindNative(ci->first);
      if(f)
      {
        opcode = op_calln;
        offset = f->m_offset; 
      }
      else
      {
        std::cout << "Error: function " << ci->first << " doesn't exist\n";
        continue;
      }
    }
    else
    {
      // Generate function
      opcode = op_call;
      offset = fi->second.second;
    }

    // Fix calls
    Calls::iterator oi, oe;
    oi = ci->second.begin();
    oe = ci->second.end();
    for(; oi != oe; ++oi)
    {
      Quad pos = oi->first;
      if(opcode == op_call)
      {
        *(Byte*)(m_code + pos)     = opcode;
        *(Quad*)(m_code + pos + 1) = offset;
      }
      else
      {
        *(Byte*)(m_code + pos)     = opcode;
        *(Word*)(m_code + pos + 1) = offset;
        *(Word*)(m_code + pos + 3) = oi->second;
      }
      pos = 0;
    }
  }

  // Store length of proc segment
  ((BinHeader*)m_code)->m_proclen = m_used - ((BinHeader*)m_code)->m_procseg;

  // Start of data segment
  ((BinHeader*)m_code)->m_dataseg = m_used;

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

  // Store length of data segment
  ((BinHeader*)m_code)->m_datalen = m_used - ((BinHeader*)m_code)->m_dataseg;

  // Store total length
  ((BinHeader*)m_code)->m_filelen = m_used;

  // Finalize header
  ((BinHeader*)m_code)->m_magic   = FILE_MAGIC;
  ((BinHeader*)m_code)->m_compver = COMPVER;
  ((BinHeader*)m_code)->m_machver = MACHVER;
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
    if(!node->m_a1.empty())
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
    GenerateBinaryExpression(node);
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
    PushByte(op_pushl);
    PushLiteral(Variant::Null);
    GenerateCode(node->m_a1);
    break;

  case list_content:
    GenerateCode(node->m_a1);
    if(!node->m_a2.empty())
    {
      GenerateCode(node->m_a2);
    }
    break;

  case list_entry:
    GenerateCode(node->m_a1);
    PushByte(op_pusha);
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
    PushByte(node->m_globalvar ? op_pushg : op_pushv);
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
      m_funs[node->m_a1] = Function(node, 0);
    }
    break;

  case parameter:
    GenerateCode(node->m_a1);
    break;

  case empty_statement:
    break;

  case switch_statement:
    GenerateSwitchExpression(node);
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

  // Push call to function
  m_calls[node->m_a1].push_back(Call(m_used, node->m_argcount));
  PushByte(op_call);
  PushQuad(0);

  // Write cleanup code for argument list
  if(node->m_argcount)
  {
    PushByte(op_stackt);
    PushQuad(node->m_argcount);
  }
}

void 
CodeGenerator::GenerateBinaryExpression(Ast* node)
{
  opcodes op = any_cast<opcodes>(node->m_a1);

  // Short-circuited logical or
  if(op == op_logor)
  {
    GenerateCode(node->m_a2);
    PushByte(op_jnz);
    Quad off1 = PushPatch();
    GenerateCode(node->m_a3);
    PushByte(op_jnz);
    Quad off2 = PushPatch();
    PushByte(op_pushl);
    PushLiteral(Variant::False);
    PushByte(op_jmp);
    Quad off3 = PushPatch();
    FixPatch(off1);
    FixPatch(off2);
    PushByte(op_pushl);
    PushLiteral(Variant::True);
    FixPatch(off3);
    return;
  }

  // Short-circuited logical and
  if(op == op_logand)
  {
    GenerateCode(node->m_a2);
    PushByte(op_jz);
    Quad off1 = PushPatch();
    GenerateCode(node->m_a3);
    PushByte(op_jz);
    Quad off2 = PushPatch();
    PushByte(op_pushl);
    PushLiteral(Variant::True);
    PushByte(op_jmp);
    Quad off3 = PushPatch();
    FixPatch(off1);
    FixPatch(off2);
    PushByte(op_pushl);
    PushLiteral(Variant::False);
    FixPatch(off3);
    return;
  }

  // Normal binary operators
  GenerateCode(node->m_a2);
  GenerateCode(node->m_a3);
  PushByte((opcodes)node->m_a1);
}

void 
CodeGenerator::GenerateSwitchExpression(Ast* node)
{
  Ast* defaultcase = 0;

  // Store for cases
  typedef std::pair<Quad, Quad> QuadPair;
  typedef std::map<Variant, QuadPair, Variant::LessExact> CaseMap;
  CaseMap cases;

  // Generate jump to conditional code
  PushByte(op_jmp);
  Quad offset = PushPatch();

  // Generate code for all cases
  AstList* list = any_cast<AstList*>(node->m_a2);
  AstList::iterator it = list->begin();
  AstList::iterator ie = list->end();
  for(; it != ie; ++it)
  {
    Ast* casenode = *it;

    // Handle default case
    if(IsType(casenode, default_case))
    {
      // TODO this should be detected by the annotation code
      if(defaultcase)
      {
        std::cout << "Error: switch statement may not contain multiple default cases";
      }
      defaultcase = casenode;
    }
    else
    {
      // Extract value
      Variant value = LiteralAsVariant(casenode->m_a1);

      // TODO Check for duplicates, should be detected by the annotation code
      if(cases.count(value))
      {
        std::cout << "Error: duplicate value in switch";
      }

      // Store current offset with value in map
      cases[value].first = m_used;

      // Pop the switch value from the stack
      PushByte(op_pop);

      // Generate code for the case
      GenerateCode(casenode->m_a2);

      // Generate jump to end
      PushByte(op_jmp);
      cases[value].second = PushPatch();
    }
  }

  // Fix jump to switch code
  FixPatch(offset);

  // Generate expression
  GenerateCode(node->m_a1);

  // Generate comparisons
  CaseMap::iterator ci = cases.begin();
  CaseMap::iterator ce = cases.end();
  for(; ci != ce; ++ci)
  {
    PushByte(op_pushl);
    PushLiteral(ci->first);
    PushByte(op_je);
    PushQuad(ci->second.first);
  }

  // Generate default case
  if(defaultcase)
  {
    GenerateCode(defaultcase->m_a1);
  }

  // Generate jumps to end of switch
  ci = cases.begin();
  for(; ci != ce; ++ci)
  {
    FixPatch(ci->second.second);
  }
}
