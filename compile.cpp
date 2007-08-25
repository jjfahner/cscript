//////////////////////////////////////////////////////////////////////////
//
// This file is © 2007 JJ Fahner <jan-jaap@jan-jaap.net>
// This file is part of the cscript interpreter.
// CScript can be found at http://svn.jan-jaap.net/
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//
//////////////////////////////////////////////////////////////////////////
#include "codegen.h"
#include "native.h"
#include "file.h"
#include "annotate.h"
#include "optimize.h"
#include "class.h"
#include "report.h"
#include "scope.h"

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
    Optimizer optimizer;
    optimizer.Optimize(node);

    // Re-validate optimized tree
    Validate(node);
  }

  // Gather information
  Annotator annotator(m_reporter);
  annotator.Annotate(node);
#ifdef _DEBUG
  Print("out.ann.txt", node);
#endif

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

  // Generate classes
  Classes::iterator ai, ae;
  ai = m_classes.begin();
  ae = m_classes.end();
  for(; ai != ae; ++ai)
  {
    GenerateClassDeclaration(ai->second);
  }

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

  // Store offset of vtables
  ((BinHeader*)m_code)->m_vtabseg = m_used;

  // Generate vtables
  ai = m_classes.begin();
  ae = m_classes.end();
  for(; ai != ae; ++ai)
  {
    Class* classDef = ai->second->m_props["classdef"];
    classDef->Write(*this);
  }

  // Write vtable terminator
  PushByte(0);

  // Store length of vtable segment
  ((BinHeader*)m_code)->m_vtablen = m_used - ((BinHeader*)m_code)->m_vtabseg;


  // Store total length
  ((BinHeader*)m_code)->m_filelen = m_used;

  // Finalize header
  ((BinHeader*)m_code)->m_magic   = FILE_MAGIC;
  ((BinHeader*)m_code)->m_compver = COMPVER;
  ((BinHeader*)m_code)->m_machver = MACHVER;

#if 0
  std::ofstream ofs("out.dec.txt");
  Decompile(m_code, sizeof(BinHeader), 
    ((BinHeader*)m_code)->m_codelen +
    ((BinHeader*)m_code)->m_proclen,
    ofs);
#endif

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
  PushByte(op_popr);
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
  case pause_statement:
    PushByte(op_nop);
    break;

  case translation_unit:
    GenerateCode(node->m_a1);
    break;

  case foreach_statement:
    GenerateForeachStatement(node);
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
    GenerateForStatement(node);
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
    GenerateWhileStatement(node);
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
    GenerateCode(node->m_a1);
    GenerateCode(node->m_a2);
    PushByte(op_pushmn);
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
    GenerateLValue(node);
    break;

  case variable_declaration:
    GenerateVariableDeclaration(node);
    break;

  case declaration_sequence:
    GenerateCode(node->m_a1);
    GenerateCode(node->m_a2);
    break;

  case function_declaration:
    m_funs[node->m_a1] = Function(node, 0);
    break;

  case parameter:
    GenerateCode(node->m_a1);
    break;

  case empty_statement:
    break;

  case switch_statement:
    GenerateSwitchExpression(node);
    break;

  case new_expression:
    PushByte(op_pushl);
    PushLiteral(node->m_a1.GetString());
    PushByte(op_new);
    break;

  case break_statement:
    GenerateBreakStatement(node);
    break;

  case continue_statement:
    GenerateContinueStatement(node);
    break;

  case class_declaration:
    m_classes[node->m_a1] = node;
    break;

  case member_call:
    GenerateMemberCall(node);
    break;

  case this_expression:
    PushByte(op_pushv);
    PushQuad(-1);
    break;

  default:
    INTERNAL_ERROR(m_reporter, node->m_pos);
  }
}

void
CodeGenerator::GenerateLValue(Ast* node)
{
  VarInfo const& vi = node->m_props["varinfo"];

  // Generate type-specific instruction
  switch(vi.m_type)
  {
  case varGlobal: 
    PushByte(op_pushg); 
    break;
  case varParam:  
    PushByte(op_pushv); 
    break;
  case varLocal:  
    PushByte(op_pushv); 
    break;
  case varMember: 
    PushByte(op_pushv);
    PushQuad(-1);
    PushByte(op_pushmi); 
    break;
  default:        
    INTERNAL_ERROR(m_reporter, node->m_pos);
  }

  // Generate offset
  PushQuad(vi.m_offset);
}

void
CodeGenerator::GenerateVariableDeclaration(Ast* node)
{
  VarInfo const& vi = node->m_props["varinfo"];

  if(node->m_a2.Type() == AstData::Null)
  {
    PushByte(op_pushl);
    PushLiteral(Variant::Null);
    PushByte(op_store);
    PushQuad(vi.m_offset);
  }
  else if(node->m_a2.Type() == AstData::Node)
  {
    GenerateCode(node->m_a2);
    PushByte(op_store);
    PushQuad(vi.m_offset);
  }
  else if(node->m_a2.Type() == AstData::Text)
  {
    
  }
  else
  {
    INTERNAL_ERROR(m_reporter, node->m_pos);
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
  typedef std::map<Variant, Quad, Variant::LessExact> CaseMap;
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
        m_reporter.ReportError(E0010, &casenode->m_pos);
      }

      // Remember as default case
      defaultcase = casenode;
    }
    else
    {
      // Extract value
      Variant value = casenode->m_a1->m_a1;

      // TODO Check for duplicates, should be detected by the annotation code
      if(cases.count(value))
      {
        m_reporter.ReportError(E0011, &casenode->m_pos);
      }

      // Store offset with value in map
      cases[value] = m_used;

      // Pop the switch value from the stack
      PushByte(op_pop);

      // Generate code for the case
      GenerateCode(casenode->m_a2);
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
    PushQuad(ci->second);
  }

  // Generate default case
  if(defaultcase)
  {
    GenerateCode(defaultcase->m_a1);
  }

  // Fix break statements
  AstList const& breaks = node->m_props["break"];
  AstList::const_iterator bi = breaks.begin();
  AstList::const_iterator be = breaks.end();
  for(; bi != be; ++bi)
  {
    FixPatch((*bi)->m_props["offset"]);
  }
}

void
CodeGenerator::GenerateBreakStatement(Ast* node)
{
  // Push jump instruction
  PushByte(op_jmp);
  Quad offset = PushPatch();

  // Store offset in node
  node->m_props["offset"] = offset;
}

void 
CodeGenerator::GenerateContinueStatement(Ast* node)
{
  // Push jump instruction
  PushByte(op_jmp);
  Quad offset = PushPatch();

  // Store offset in node
  node->m_props["offset"] = offset;
}

void 
CodeGenerator::GenerateWhileStatement(Ast* node)
{
  // Store start of while
  Quad offset0 = m_used;

  // Generate expression and comparison
  GenerateCode(node->m_a1);
  PushByte(op_jz);
  Quad offset1 = PushPatch();

  // Generate while contents
  GenerateCode(node->m_a2);

  // Generate jump to start
  PushByte(op_jmp);
  PushQuad(offset0);

  // Fix jump to end
  FixPatch(offset1);

  // Fix break statements
  AstList const& breaks = node->m_props["break"];
  AstList::const_iterator bi = breaks.begin();
  AstList::const_iterator be = breaks.end();
  for(; bi != be; ++bi)
  {
    FixPatch((*bi)->m_props["offset"]);
  }

  // Fix continue statements
  AstList const& continues = node->m_props["continue"];
  AstList::const_iterator ci = continues.begin();
  AstList::const_iterator ce = continues.end();
  for(; ci != ce; ++ci)
  {
    FixPatch((*ci)->m_props["offset"], offset0);
  }
}

void 
CodeGenerator::GenerateForStatement(Ast* node)
{
  // Generate init-decl
  GenerateCode(node->m_a1);

  // Generate condition
  Quad offset0 = m_used;
  GenerateCode(node->m_a2);
  PushByte(op_jz);
  Quad offset1 = PushPatch();

  // Generate contents
  GenerateCode(node->m_a4);

  // Generate post expression
  Quad offset2 = m_used;
  GenerateCode(node->m_a3);
  PushByte(op_pop);

  // Jump to condition
  PushByte(op_jmp);
  PushQuad(offset0);

  // Fix jump out
  FixPatch(offset1);

  // Fix break statements
  AstList const& breaks = node->m_props["break"];
  AstList::const_iterator bi = breaks.begin();
  AstList::const_iterator be = breaks.end();
  for(; bi != be; ++bi)
  {
    FixPatch((*bi)->m_props["offset"]);
  }

  // Fix continue statements
  AstList const& continues = node->m_props["continue"];
  AstList::const_iterator ci = continues.begin();
  AstList::const_iterator ce = continues.end();
  for(; ci != ce; ++ci)
  {
    FixPatch((*ci)->m_props["offset"], offset2);
  }
}

void
CodeGenerator::GenerateForeachStatement(Ast* node)
{
  // Generate expression
  GenerateCode(node->m_a2);

  // Convert expression to iterator
  PushByte(op_iters);
  
  // Test for end of iterator
  Quad offset0 = m_used;
  PushByte(op_iterv);
  Quad offset1 = PushPatch();
  
  // Assign iterator to target
  VarInfo const& vi = node->m_a1->m_props["varinfo"];
  PushByte(op_itern);
  PushQuad(vi.m_offset);

  // Generate loop body
  GenerateCode(node->m_a3);

  // Generate jump to top
  PushByte(op_jmp);
  PushQuad(offset0);

  // Fix jump to end
  FixPatch(offset1);

  // Fix break statements
  AstList const& breaks = node->m_props["break"];
  AstList::const_iterator bi = breaks.begin();
  AstList::const_iterator be = breaks.end();
  for(; bi != be; ++bi)
  {
    FixPatch((*bi)->m_props["offset"]);
  }

  // Fix continue statements
  AstList const& continues = node->m_props["continue"];
  AstList::const_iterator ci = continues.begin();
  AstList::const_iterator ce = continues.end();
  for(; ci != ce; ++ci)
  {
    FixPatch((*ci)->m_props["offset"], offset0);
  }

  // Pop array off stack
  PushByte(op_pop);
}

void 
CodeGenerator::GenerateClassDeclaration(Ast* node)
{
  // Retrieve member list
  AstList* list = node->m_a2;
  AstList::iterator it, ie;

  // Store class definition with declaration
  Class* classDef = new Class(node->m_a1);
  node->m_props["classdef"] = classDef;

  // Enumerate functions
  it = list->begin();
  ie = list->end();
  for(; it != ie; ++it)
  {
    Ast* mem = *it;
    if(mem->m_type == function_declaration)
    {
      // Generate function
      GenerateFunction(mem);
      
      // Store in vtable
      classDef->AddFun(mem->m_a1, 
        mem->m_props["parcount"], 
        mem->m_props["offset"]);
    }
    else
    {
      classDef->AddVar(mem->m_a1);
    }
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

    // In case of broken call, there's no need to 
    // generate correct code, so ignore lack of offset
    if(node->m_props.contains("offset"))
    {
      PushWord((Quad)node->m_props["offset"]);
      PushWord((Quad)node->m_props["argcount"]);
    }
  }

  // Cleanup code
  if((Quad)node->m_props["argcount"] != Quad(0))
  {
    PushByte(op_stackt);
    PushQuad(node->m_props["argcount"]);
  }

  // Push return value onto stack
  PushByte(op_pushr);
}

void
CodeGenerator::GenerateMemberCall(Ast* node)
{
  // Generate function call arguments
  if(!node->m_a2->m_a2.Empty())
  {
    GenerateCode(node->m_a2->m_a2);
  }

  // Generate object
  GenerateCode(node->m_a1);

  // Generate name of function
  PushByte(op_pushl);
  PushLiteral(node->m_a2->m_a1.GetString());

  // Generata call instruction
  PushByte(op_callm);

  // Generate number of arguments
  PushQuad(node->m_props["argcount"]);

  // Cleanup code
  PushByte(op_stackt);
  PushQuad((Quad)node->m_props["argcount"]);

  // Push return value onto stack
  PushByte(op_pushr);
}
