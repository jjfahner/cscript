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

Quad 
CodeGenerator::PushPatch()
{
  Quad pos = m_used;
  PushQuad(0);
  return pos;
}

void 
CodeGenerator::PushCall(String const& name)
{
  m_calls[name].push_back(m_used);
  PushQuad(0);
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
CodeGenerator::Generate(Ast* node)
{
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

    // TODO Generate parameter code

    // Push function stack frame
    m_scope = m_scope->PushFrame();

    // Generate function content
    GenerateCode(it->second.first->m_a3);

    // Generate return statement
    // TODO only required if not present
    PushByte(op_pushl);
    PushQuad(0);
    PushByte(op_ret);

    // Pop the stack frame
    m_scope = m_scope->PopFrame();
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

  case integer:
    PushByte(op_pushl);
    PushLiteral(Variant(any_cast<String>(node->m_a1), Variant::stInt));
    break;

  case real:
    PushByte(op_pushl);
    PushLiteral(Variant(any_cast<String>(node->m_a1), Variant::stReal));
    break;

  case string:
    PushByte(op_pushl);
    PushLiteral(Variant(any_cast<String>(node->m_a1), Variant::stString));
    break;

  case boolean:
    PushByte(op_pushl);
    PushLiteral(Variant(any_cast<String>(node->m_a1), Variant::stBool));
    break;

  case null:
    PushByte(op_pushl);
    PushLiteral(Variant());
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
    PushByte(op_idx);
    break;

  case function_call:
    GenerateCode(node->m_a2);
    PushByte(op_call);
    PushCall(node->m_a1);
    break;

  case argument_list:
    GenerateCode(node->m_a2); // Reverse order
    GenerateCode(node->m_a1);
    break;

  case identifier:
    PushByte(op_pushv);
    PushQuad(m_scope->FindVar(node->m_a1));
    break;

  case variable_declaration:
    m_scope->AddVar(node->m_a1);
    if(!node->m_a2.empty())
    {
      PushByte(op_pushl);
      PushQuad(m_scope->FindVar(node->m_a1));
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
  case parameter_list:
    break;

  default:
    std::cout << " " << node->m_type << " ";
    throw std::runtime_error("Unknown node type");
  }
}
