#ifndef CSCRIPT_CODEGEN_H
#define CSCRIPT_CODEGEN_H

#include "ast.h"

class CodeGenerator
{
public:

  //
  // Construction
  //
  CodeGenerator();

  //
  // Destruction
  //
  ~CodeGenerator();

  //
  // Generate code in parse tree
  //
  void Generate(Ast*);

};


#endif // CSCRIPT_CODEGEN_H
