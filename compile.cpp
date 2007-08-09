#include "codegen.h"
#include "native.h"
#include "file.h"
#include "annotate.h"

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
  Annotator annotator(m_reporter);
  annotator.Annotate(node);
//  Print("out.ann.txt", node);

  // Reserve header space
  Reserve(sizeof(BinHeader));
  m_used += sizeof(BinHeader);
  memset(m_code, 0, sizeof(BinHeader));

  // Store start of global code segment
  ((BinHeader*)m_code)->m_codeseg = m_used;

  // Generate initial stack frame
  PushByte(op_stackg);
  PushQuad((Quad)node->m_props["framesize"]);

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
    GenerateFunction(fi->second.first);
  }

  // Resolve calls to functions
  AstList::iterator ci, ce;
  ci = m_calls.begin();
  ce = m_calls.end();
  for(; ci != ce; ++ci)
  {
    Ast* call = *ci;
    Ast* proc = call->m_a3;

    Quad patch  = call->m_props["patch"];
    Quad offset = proc->m_props["offset"];

    *(Quad*)(m_code + patch) = offset;
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

//   std::ofstream ofs("out.dec.txt");
//   Decompile(m_code, sizeof(BinHeader), 
//     ((BinHeader*)m_code)->m_codelen +
//     ((BinHeader*)m_code)->m_proclen,
//     ofs);

  // Check for errors
  if(m_reporter.GetErrorCount())
  {
    free(m_code);
    m_code = 0;
    m_used = 0;
    std::cout << "\nCompilation failed with ";
  }
  else
  {
    std::cout << "\nCompilation succeeded with ";
  }
  std::cout << m_reporter.GetErrorCount()   << " error(s), " 
            << m_reporter.GetWarningCount() << " warning(s)\n";
}

void
CodeGenerator::GenerateFunction(Ast* node)
{
  // Store function offset
  node->m_props["offset"] = m_used;

  // Push stack growth instruction
  PushByte(op_stackg);
  PushQuad(node->m_props["framesize"]);

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
  PushQuad(node->m_props["framesize"]);

  // Generate return
  PushByte(op_ret);
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
  case translation_unit:
    GenerateCode(node->m_a1);
    break;

    // TODO
  case foreach_statement:
    break;

  case include_statement:
    // Already handled by parser
    break;

  case statement_sequence:
  case parameter_list:
    {
      AstList::iterator si, se;
      si = node->m_a1.GetList()->begin();
      se = node->m_a1.GetList()->end();
      for(; si != se; ++si)
      {
        GenerateCode(*si);
      }
    }
    break;

  case return_statement:
    if(node->m_a1)
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
    if(node->m_a1)
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
    if(!node->m_a3)
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
    PushByte(node->m_a1.GetNumber());
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
    if(node->m_a2)
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
    PushByte(node->m_a1.GetNumber());
    break;

  case postfix_expression:
    GenerateCode(node->m_a2);
    PushByte(node->m_a1.GetNumber());
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
    PushByte(node->m_props["isglobal"] ? op_pushg : op_pushv);
    PushQuad(node->m_props["stackpos"]);
    break;

  case variable_declaration:
    if(!node->m_a2)
    {
      PushByte(op_pushl);
      PushLiteral(Variant::Null);
      PushByte(op_store);
      PushQuad(node->m_props["stackpos"]);
    }
    else
    {
      GenerateCode(node->m_a2);
      PushByte(op_store);
      PushQuad(node->m_props["stackpos"]);
    }
    break;

  case declaration_sequence:
    GenerateCode(node->m_a1);
    GenerateCode(node->m_a2);
    break;

  case function_declaration:
    if(m_funs.count(node->m_a1))
    {
      m_reporter.ReportError(node->m_pos, "redeclaration of function " + node->m_a1.GetString());
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

  case struct_declaration:
    break;

  default:
    m_reporter.ReportError(node->m_pos, "internal compiler error: attempt to generate an unknown node type");
  }
}

void 
CodeGenerator::GenerateFunctionCall(Ast* node)
{
  // Generate argument list
  if(node->m_a2)
  {
    GenerateCode(node->m_a2);
  }

  // Push space for return value
  PushByte(op_pushl);
  PushLiteral(Variant::Null);

  // Generate call
  if(node->m_a3)
  {
    // User code call
    PushByte(op_call);
    node->m_props["patch"] = m_used;
    m_calls.push_back(node);
    PushQuad(0);
  }
  else
  {
    // Native call
    PushByte(op_calln);
    PushWord((Quad)node->m_props["offset"]);
    PushWord((Quad)node->m_props["argcount"]);
  }

  // Write cleanup code for argument list
  if(node->m_props["argcount"])
  {
    PushByte(op_stackt);
    PushQuad(node->m_props["argcount"]);
  }
}

void 
CodeGenerator::GenerateBinaryExpression(Ast* node)
{
  opcodes op = (opcodes)node->m_a1.GetNumber();

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
  PushByte(node->m_a1.GetNumber());
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
  AstList* list = node->m_a2;
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
        m_reporter.ReportError(casenode->m_pos, "switch statement may not contain multiple default cases");
      }
      defaultcase = casenode;
    }
    else
    {
      // Extract value
      Variant value = casenode->m_a1.GetNode()->m_a1;

      // TODO Check for duplicates, should be detected by the annotation code
      if(cases.count(value))
      {
        m_reporter.ReportError(casenode->m_pos, "duplicate case label in switch statement");
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
