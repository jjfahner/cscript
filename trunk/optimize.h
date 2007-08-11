#ifndef CSCRIPT_OPTIMIZE_H
#define CSCRIPT_OPTIMIZE_H

#include "types.h"

class Optimizer 
{
public:

  //
  // Optimize recursively. Returns an optimized
  // representation of the input tree. This is
  // a destructive process for the input tree.
  //
  Ast* Optimize(Ast* root);

private:

  //
  // Implementation methods
  //
  Ast* OptimizeIfStatement(Ast*);
  Ast* OptimizeForStatement(Ast*);
  Ast* OptimizeBinaryExpression(Ast*);
  Ast* OptimizeTernaryExpression(Ast*);
  Ast* OptimizeStatementSequence(Ast*);
  Ast* OptimizeExpressionStatement(Ast*);
  Ast* OptimizeCompoundStatement(Ast*);
  Ast* OptimizeAssignmentExpression(Ast*);
  Ast* OptimizePrefixExpression(Ast*);
  Ast* OptimizeSwitchStatement(Ast*);
  Ast* OptimizeStructDeclaration(Ast*);
  Ast* OptimizeVariableDeclaration(Ast*);

};

#endif // CSCRIPT_OPTIMIZE_H
