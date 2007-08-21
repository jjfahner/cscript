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
  Ast* OptimizeForeachStatement(Ast*);
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
