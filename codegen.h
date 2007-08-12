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
#ifndef CSCRIPT_CODEGEN_H
#define CSCRIPT_CODEGEN_H

#include "ast.h"
#include "var.h"
#include "scope.h"
#include "report.h"

class CodeGenerator
{
public:

  //
  // Construction
  //
  CodeGenerator(Reporter& reporter);

  //
  // Destruction
  //
  ~CodeGenerator();

  //
  // Generate code in parse tree
  //
  void Generate(Ast* root, bool release);

  //
  // Release code pointe
  //
  Byte* ReleaseCode();

  //
  // Write to output
  //
  void Write();

  //
  // Print code
  //
  void Print(String filename, Ast* node);

  //
  // Decompile code
  //
  void Decompile(Byte* code, Quad offset, Quad len, std::ostream& ofs);

  //
  // Retrieve code pointer
  //
  Byte* GetCode() const;

  //
  // Retrieve used buffer size
  //
  Quad GetSize() const;
  
private:

  //
  // Validation
  //
  void Validate(Ast*);

  //
  // Printing
  //
  void PrintImpl(Ast* node, int level, std::ostream& s);

  //
  // High-level code generation
  //
  void GenerateCode(Ast*);
  void GenerateFunction(Ast*);
  void GenerateFunctionCall(Ast*);
  void GenerateBinaryExpression(Ast*);
  void GenerateSwitchExpression(Ast*);
  void GenerateBreakStatement(Ast*);

  //
  // Low-level code generation
  //
  void PushData(Byte* data, Quad size);
  void PushByte(Byte);
  void PushWord(Word);
  void PushQuad(Quad);
  void FillQuad(Quad offset, Quad data);

  //
  // Push reference to literal
  //
  void PushLiteral(Variant const&);

  //
  // Push empty quad, return offset
  //
  Quad PushPatch();
  void FixPatch(Quad);

  //
  // Reserve space for code
  //
  void Reserve(Quad);

  //
  // Containers
  //
  typedef std::list<Quad> QuadList;
  typedef std::pair<Ast*, Quad> Function;
  typedef std::map<String, Function> Functions;
  typedef std::map<Variant, QuadList, Variant::LessExact> Literals;

  //
  // Member data
  //
  Reporter& m_reporter;
  Byte*     m_code;
  Quad      m_size;
  Quad      m_used;
  Functions m_funs;
  AstList   m_calls;
  Literals  m_literals;
  QuadList  m_returns;

};

inline Byte* 
CodeGenerator::GetCode() const
{
  return m_code;
}

inline Quad 
CodeGenerator::GetSize() const
{
  return m_used;
}

#endif // CSCRIPT_CODEGEN_H
