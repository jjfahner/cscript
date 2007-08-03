#include "codegen.h"
#include "opcodes.h"

CodeGenerator::CodeGenerator() :
m_code    (0),
m_size    (0),
m_used    (0),
m_globals (0),
m_scope   (0)
{
  m_globals = new Frame();
  m_scope = m_globals;
}

CodeGenerator::~CodeGenerator()
{
  if(m_code)
  {
    free(m_code);
  }
  delete m_globals;
}

void 
CodeGenerator::Write()
{
  std::ofstream ofs("out.csb", std::ios::binary);
  ofs.write((char*)m_code, m_used);
  ofs.close();
}

void 
CodeGenerator::Reserve(Quad size)
{
  if(m_size - m_used < size)
  {
    m_code = (Byte*)realloc(m_code, size * 2);
    m_size = size * 2;
  }
}

void 
CodeGenerator::PushData(Byte* data, Quad size)
{
  Reserve(m_used + size);
  memmove(m_code + m_used, data, size);
  m_used += size;
}

void 
CodeGenerator::PushByte(Byte data)
{
  Reserve(m_used + sizeof(data));
  *(Byte*)(m_code + m_used) = data;
  m_used += sizeof(Byte);
}

void 
CodeGenerator::PushWord(Word data)
{
  Reserve(m_used + sizeof(data));
  *(Word*)(m_code + m_used) = data;
  m_used += sizeof(Word);
}

void 
CodeGenerator::PushQuad(Quad data)
{
  Reserve(m_used + sizeof(data));
  *(Quad*)(m_code + m_used) = data;
  m_used += sizeof(Quad);
}

void 
CodeGenerator::FillQuad(Quad offset, Quad data)
{
  *(Quad*)(m_code + offset) = data;
}

Quad 
CodeGenerator::PushPatch()
{
  Quad pos = m_used;
  PushQuad(0);
  return pos;
}

void
CodeGenerator::FixPatch(Quad pos)
{
  *(Quad*)(m_code + pos) = m_used;
}

void 
CodeGenerator::PushLiteral(Variant const& value)
{
  m_literals.push_back(Literal(value, m_used));
  PushQuad(0);
}

void 
CodeGenerator::PushFrame()
{
}

void 
CodeGenerator::PopFrame()
{
}

void 
CodeGenerator::PushScope()
{
}

void 
CodeGenerator::PopScope()
{

}

void 
CodeGenerator::Generate(Ast* node)
{
  // Optimize the input tree
  Optimize(node);

  // Generate top-level code
  GenerateCode(node);

  // Generate functions
  Functions::iterator it, ie;
  it = m_funs.begin();
  ie = m_funs.end();
  for(; it != ie; ++it)
  {
    // Store function offset with function
    it->second.second = m_used;

    // Push stack grow instruction and reserve
    // space to store the stack grow size
    PushByte(op_stg);
    Quad offset = m_used;
    PushQuad(0);

    // Push a new stack frame for the function
    m_scope = m_scope->PushFrame();

    // TODO Generate parameter code

    // Generate function content
    GenerateCode(it->second.first->m_a3);

    // Update the stack size
    FillQuad(offset, m_scope->GetVarCount());

    // Write stack cleanup
    PushByte(op_sts);
    PushQuad(m_scope->GetVarCount());

    // Pop the stack frame
    m_scope = m_scope->PopFrame();

    // Generate return statement
    // TODO only required if not present
    PushByte(op_pushl);
    PushQuad(0);
    PushByte(op_ret);
  }

  // Fix calls to functions
  CallList::iterator ci, ce;
  ci = m_calls.begin();
  ce = m_calls.end();
  for(; ci != ce; ++ci)
  {
    // Find corresponding function
    Functions::iterator fi = m_funs.find(ci->first);
    if(fi == m_funs.end())
    {
      std::cout << "Error: function " << ci->first << " doesn't exist\n";
      continue;
    }

    // Fix offsets
    QuadList::iterator oi, oe;
    oi = ci->second.begin();
    oe = ci->second.end();
    for(; oi != oe; ++oi)
    {
      *(Quad*)(m_code + *oi) = fi->second.second;
    }
  }

  // Generate literals
  Literals::iterator li, le;
  li = m_literals.begin();
  le = m_literals.end();
  for(; li != le; ++li)
  {
    // Patch source with current offset
    *(Quad*)(m_code + li->second) = m_used;
    
    // Write literal at current offset
    size_t len = li->first.WriteLength();
    Reserve(m_used + len);
    li->first.Write(m_code + m_used);
    m_used += (Quad)len;
  }
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
      PushQuad(0);
    }
    PushByte(op_ret);
    break;

  case compound_statement:
    if(any_cast<Ast*>(node->m_a1) != 0)
    {
      m_scope = m_scope->PushScope();
      GenerateCode(node->m_a1);
      m_scope = m_scope->PopScope();
    }
    break;

  case for_statement:
    m_scope = m_scope->PushScope();
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
    m_scope = m_scope->PopScope();
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
    GenerateCode(node->m_a2);
    PushByte(op_call);
    m_calls[node->m_a1].push_back(m_used);
    PushQuad(0);
    break;

  case argument_list:
    GenerateCode(node->m_a2);
    GenerateCode(node->m_a1);
    break;

  case lvalue:
    {
      VarInfo var = m_scope->FindVar(node->m_a1);
      if(var.m_scope == m_globals)
      {
        PushByte(op_pushg);
        PushQuad(var.m_offset);
      }
      else
      {
        PushByte(op_pushv);
        PushQuad(var.m_offset);
      }
    }
    break;

  case variable_declaration:
    m_scope->AddVar(node->m_a1);
    if(!node->m_a2.empty())
    {
      VarInfo var = m_scope->FindVar(node->m_a1);
      if(var.m_scope == m_globals)
      {
        PushByte(op_pushg);
        PushQuad(var.m_offset);
      }
      else
      {
        PushByte(op_pushl);
        PushQuad(var.m_offset);
      }
      GenerateCode(node->m_a2);
      PushByte(op_assign);
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

  default:
    std::cout << " " << node->m_type << " ";
    throw std::runtime_error("Unknown node type");
  }
}

inline Byte NextByte(Byte*& code)
{
  Byte b = *code;
  code += 1;
  return b;
}

inline Word NextWord(Byte*& code)
{
  Word b = *(Word*)code;
  code += 2;
  return b;
}

inline Quad NextQuad(Byte*& code)
{
  Quad b = *(Quad*)code;
  code += 4;
  return b;
}

inline Variant ReadLiteral(Byte* code)
{
  Variant v;
  v.Read(code);
  return v;
}

void 
CodeGenerator::Execute()
{
  // Instruction pointers
  #define ipb (NextByte(code))
  #define ipw (NextWord(code))
  #define ipq (NextQuad(code))

  // Set code pointers
  Byte* base = m_code;
  Byte* code = m_code;

  // Create stacks
  std::stack<VariantRef>  tstack; // Temporaries
  std::stack<Quad>        rstack; // Return stack
  std::vector<VariantRef> vstack; // Variable stack
  std::vector<VariantRef> gstack; // Global stack

  // Resize stacks
  vstack.resize(10000);
  gstack.resize(10000);

  // Stack manipulation
  #define PUSH(arg) tstack.push(arg)
  #define POP(arg)  arg = tstack.top(); tstack.pop();

  // Temporaries
  VariantRef P0, P1;
  #define R0 (*P0)
  #define R1 (*P1)
  Quad q0;

  // Start of instruction
begin:

  // Execute single instruction
  switch(ipb)
  {
  case op_halt:
    return;

  case op_pushg:
    PUSH(gstack[ipq]);
    break;

  case op_pushl:
    PUSH(ReadLiteral(base + ipq));
    break;

  case op_pushv:
    PUSH(vstack[ipq]);
    break;

  case op_pushi:
    POP(P1);
    POP(P0);
    PUSH((R0)[R1]);
    break;

  case op_pop:
    POP(P0);
    break;

  case op_jmp:
    code = base + ipq;
    break;

  case op_jz:
    q0 = ipq;
    POP(P0);
    if(!R0) code = base + q0;
    break;

  case op_jnz:
    q0 = ipq;
    POP(P0);
    if(R0) code = base + q0;
    break;

  case op_call:
    code = base + ipq;
    rstack.push((Quad)(code - base));
    break;

  case op_ret:
    code = base + rstack.top();
    rstack.pop();
    break;

  case op_inc:
  case op_dec:
    break;

  case op_add:
    POP(P1);
    POP(P0);
    PUSH(R0 + R1);
    break;

  case op_sub:
    POP(P1);
    POP(P0);
    PUSH(R0 - R1);
    break;

  case op_mul:
    POP(P1);
    POP(P0);
    PUSH(R0 * R1);
    break;

  case op_div:
    POP(P1);
    POP(P0);
    PUSH(R0 / R1);
    break;

  case op_mod:  
    POP(P1);
    POP(P0);
    PUSH(R0 % R1);
    break;

  case op_logor:
    POP(P1);
    POP(P0);
    PUSH(R0 || R1);
    break;

  case op_logand:
    POP(P1);
    POP(P0);
    PUSH(R0 && R1);
    break;

  case op_eq:
    POP(P1);
    POP(P0);
    PUSH(R0 == R1);
    break;

  case op_ne:
    POP(P1);
    POP(P0);
    PUSH(R0 != R1);
    break;

  case op_lt:
    POP(P1);
    POP(P0);
    PUSH(R0 < R1);
    break;

  case op_le:
    POP(P1);
    POP(P0);
    PUSH(R0 <= R1);
    break;

  case op_gt:
    POP(P1);
    POP(P0);
    PUSH(R0 > R1);
    break;

  case op_ge:
    POP(P1);
    POP(P0);
    PUSH(R0 >= R1);
    break;

  case op_assign:
    POP(P1);
    POP(P0);
    R0 = R1;
    PUSH(P0);
    break;

  case op_assadd:
    POP(P1);
    POP(P0);
    R0 += R1;
    PUSH(P0);
    break;

  case op_asssub:
    POP(P1);
    POP(P0);
    R0 -= R1;
    PUSH(P0);
    break;

  case op_assmul:
    POP(P1);
    POP(P0);
    R0 *= R1;
    PUSH(P0);
    break;

  case op_assdiv:
    POP(P1);
    POP(P0);
    R0 /= R1;
    PUSH(P0);
    break;

  case op_assmod:
    POP(P1);
    POP(P0);
    R0 %= R1;
    PUSH(P0);
    break;

//   case op_bitor:
//   case op_bitxor:
//   case op_bitand:  
//     break;

  default:
    goto invalid;
  }

  // Next instruction
  goto begin;

// Invalid instruction
invalid:

  std::cout << "Invalid instruction\n";
}

