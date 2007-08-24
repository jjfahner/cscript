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
#include "annotate.h"
#include "report.h"
#include "native.h"
#include "error.h"
#include "scope.h"

//////////////////////////////////////////////////////////////////////////
//
// Helpers
//

inline Quad VarCount(Ast* ast)
{
  return ast->m_props["varcount"];
}

inline Quad ParCount(Ast* ast)
{
  return ast->m_props["parcount"];
}

inline Quad ArgCount(Ast* ast)
{
  return ast->m_props["argcount"];
}

//////////////////////////////////////////////////////////////////////////
//
// Implementation
//

Annotator::Annotator(Reporter& reporter) :
m_reporter  (reporter),
m_scope     (0)
{
}

//////////////////////////////////////////////////////////////////////////
//
// Scope handling
//

void
Annotator::PushScope(Ast* node)
{
  // Check current scope
  if(m_scopeStack.size() && m_scopeStack.top() != m_scope)
  {
    throw std::runtime_error("Invalid scope on stack");
  }

  // Fetch or create scope
  Scope* scope = 0;
  if(node->m_props.contains("scope"))
  {
    scope = node->m_props["scope"];
  }
  else
  {
    scope = new Scope(node, m_scope);
  }

  // Push it on the stack
  m_scopeStack.push(scope);

  // Associate it with the current node
  node->m_props["scope"] = scope;

  // Set as current scope
  m_scope = scope;
}

void
Annotator::PopScope()
{
  // Retrieve scope from stack
  Scope* scope = m_scopeStack.top();
  if(scope != m_scope)
  {
    throw std::runtime_error("Invalid scope on stack");
  }

  // Remove from stack
  m_scopeStack.pop();
  m_scope = m_scopeStack.size() ? m_scopeStack.top() : 0;
}

//////////////////////////////////////////////////////////////////////////
//
// Top-level annotation
//

void
Annotator::Annotate(Ast* node)
{
  // Annotate structure
  AnnotateStructure(node);

  // Annotate code
  AnnotateImpl(node);

  // Annotate functions
  AstMap::iterator it = m_functions.begin();
  AstMap::iterator ie = m_functions.end();
  for(; it != ie; ++it)
  {
    PushScope(it->second->m_props["inscope"]);
    AnnotateFunction(it->first, it->second);
    PopScope();
  }

  // Resolve and check function calls
  ResolveCalls();  
}

//////////////////////////////////////////////////////////////////////////
//
// Annotate global structure; registers global functions and classes.
//

void 
Annotator::AnnotateStructure(Ast* node)
{  
  switch(node->m_type)
  {
    // Handle translation unit
  case translation_unit:
    node->m_props["framesize"] = Quad(0);
    PushScope(node);
    AnnotateStructure(node->m_a1);
    PopScope();
    break;

    // Recurse into statement sequence
  case statement_sequence:
    {
      AstList* list = node->m_a1;
      AstList::iterator si = list->begin();
      AstList::iterator se = list->end();
      for(; si != se; ++si)
      {
        AnnotateStructure(*si);
      }
    }
    break;

    // Register function declaration
  case function_declaration:
    {
      // Declare in local scope
      String mangled = m_scope->DeclareFunction(node->m_a1, node);

      // Register global name
      m_functions[mangled] = node;
    }
    break;

    // Register variable
  case variable_declaration:
    AnnotateVariableDeclaration(node);
    break;

    // Parse class member functions
  case class_declaration:
    node->m_props["framesize"] = Quad(0);
    PushScope(node);
    {
      AstList* list = node->m_a2;
      AstList::iterator si = list->begin();
      AstList::iterator se = list->end();
      for(; si != se; ++si)
      {
        AnnotateStructure(*si);
      }
    }
    PopScope();
    m_classes[node->m_a1] = node;
    break;
  }
}

//////////////////////////////////////////////////////////////////////////
//
// Annotate code. Annotates global code and (member-)function contents.
//

void
Annotator::AnnotateImpl(Ast* node)
{
  switch(node->m_type)
  {
  case pause_statement:
    node->m_props["varcount"] = Quad(0);
    break;

  case translation_unit:
    AnnotateTranslationUnit(node);
    break;

  case statement_sequence:
    AnnotateStatementSequence(node);
    break;

  case expression_statement:
    node->m_props["varcount"] = Quad(0);
    AnnotateImpl(node->m_a1);
    break;

  case assignment_expression:
    AnnotateImpl(node->m_a2);
    AnnotateImpl(node->m_a3);
    break;

  case binary_expression:
    AnnotateImpl(node->m_a2);
    AnnotateImpl(node->m_a3);
    break;

  case ternary_expression:
    AnnotateImpl(node->m_a1);
    AnnotateImpl(node->m_a2);
    AnnotateImpl(node->m_a3);
    break;

  case prefix_expression:
    AnnotateImpl(node->m_a2);
    break;

  case postfix_expression:
    AnnotateImpl(node->m_a2);
    break;

  case member_expression:
    AnnotateMemberExpression(node);
    break;

  case index_expression:
    AnnotateImpl(node->m_a1);
    AnnotateImpl(node->m_a2);
    break;

  case literal:
    break;

  case lvalue:
    AnnotateLValue(node);
    break;

  case list_literal:
    AnnotateImpl(node->m_a1);
    break;
    
  case list_content:
    AnnotateImpl(node->m_a1);
    if(node->m_a2)
    {
      AnnotateImpl(node->m_a2);
    }
    break;
    
  case list_entry:
    AnnotateImpl(node->m_a1);
    break;

  case function_call:
    AnnotateFunctionCall(node);
    break;

  case argument_list:
    AnnotateImpl(node->m_a1);
    AnnotateImpl(node->m_a2);
    node->m_props["argcount"] = ArgCount(node->m_a1) + ArgCount(node->m_a2);
    break;

  case argument:
    AnnotateImpl(node->m_a1);
    node->m_props["argcount"] = Quad(1);
    break;

  case parameter:
    node->m_props["varinfo"] = m_scope->DeclareParameter(node->m_a1);
    break;

  case parameter_list:
    AnnotateImpl(node->m_a1);
    AnnotateImpl(node->m_a2);
    break;

  case variable_declaration:
    AnnotateVariableDeclaration(node);
    break;

  case declaration_sequence:
    node->m_a1->m_a3 = node->m_a3;
    node->m_a2->m_a3 = node->m_a3;
    AnnotateImpl(node->m_a1);
    AnnotateImpl(node->m_a2);
    node->m_props["varcount"] = 
         VarCount(node->m_a1) + 
         VarCount(node->m_a2) ;
    break;

  case empty_statement:
    node->m_props["varcount"] = Quad(0);
    break;

  case include_statement:
    node->m_props["varcount"] = Quad(0);
    break;

  case for_statement:
    node->m_props["varcount"] = 0;
    node->m_props["break"]    = AstList();
    node->m_props["continue"] = AstList();
    PushScope(node);
    AnnotateImpl(node->m_a1);
    AnnotateImpl(node->m_a2);
    AnnotateImpl(node->m_a3);
    PushScope(node);
    AnnotateImpl(node->m_a4);
    PopScope();
    PopScope();
    node->m_props["varcount"] = 
         VarCount(node->m_a1) + 
         VarCount(node->m_a4) ;
    break;

  case foreach_statement:
    node->m_props["varcount"] = 0;
    node->m_props["break"]    = AstList();
    node->m_props["continue"] = AstList();
    PushScope(node);
    AnnotateImpl(node->m_a1);
    AnnotateImpl(node->m_a2);
    PushScope(node);
    AnnotateImpl(node->m_a3);
    PopScope();
    PopScope();
    node->m_props["varcount"] = 
         VarCount(node->m_a1) + 
         VarCount(node->m_a3) ;
    break;

  case if_statement:
    node->m_props["varcount"] = 0;
    AnnotateImpl(node->m_a1);
    PushScope(node);
    AnnotateImpl(node->m_a2);
    if(node->m_a3)
    {
      AnnotateImpl(node->m_a3);
    }
    PopScope();
    node->m_props["varcount"] = VarCount(node->m_a2);
    break;

  case while_statement:
    node->m_props["varcount"] = 0;
    node->m_props["break"]    = AstList();
    node->m_props["continue"] = AstList();
    AnnotateImpl(node->m_a1);
    PushScope(node);
    AnnotateImpl(node->m_a2);
    PopScope();
    node->m_props["varcount"] = VarCount(node->m_a2);
    break;

  case return_statement:
    node->m_props["varcount"] = Quad(0);
    if(node->m_a1)
    {
      AnnotateImpl(node->m_a1);
    }
    break;

  case compound_statement:
    PushScope(node);
    AnnotateImpl(node->m_a1);
    PopScope();
    node->m_props["varcount"] = VarCount(node->m_a1);
    break;

  case switch_statement:
    {
      node->m_props["varcount"] = Quad(0);
      node->m_props["break"]    = AstList();
      AnnotateImpl(node->m_a1);
      AstList* list = node->m_a2;
      AstList::iterator it, ie;
      it = list->begin();
      ie = list->end();
      PushScope(node);
      for(; it != ie; ++it)
      {
        AnnotateImpl(*it);
      }
    }
    break;

  case switch_case:
    AnnotateSwitchCase(node);
    break;

  case default_case:
    AnnotateImpl(node->m_a1);
    break;

  case break_statement:
    AnnotateBreakStatement(node);
    break;

  case continue_statement:
    AnnotateContinueStatement(node);
    break;

  case struct_declaration:
    AnnotateStructDeclaration(node);
    break;

  case struct_members:
    AnnotateImpl(node->m_a1);
    AnnotateImpl(node->m_a2);
    break;

  case new_expression:
    AnnotateNewExpression(node);
    break;

  case class_declaration:
    //m_reporter.ReportError(E0016, &node->m_pos);
    break;

  case function_declaration:
    //m_reporter.ReportError(E0017, &node->m_pos);
    break;

  case member_call:
    AnnotateMemberCall(node);
    break;

  case this_expression:
    break;

  }
}

void 
Annotator::AnnotateTranslationUnit(Ast* node)
{
  // Push initial frame
  PushScope(node);

  // Initialize properties
  node->m_props["framesize"] = Quad(0);
  node->m_props["varcount"]  = Quad(0);

  // Annotate contents
  AnnotateImpl(node->m_a1);
}

void 
Annotator::AnnotateFunction(String fullName, Ast* node)
{
  // Function name
  String name = node->m_a1;

  // Member function
  bool isMember = fullName.find('@') != String::npos;

  // Initialize annotations
  node->m_props["framesize"] = Quad(0);
  node->m_props["varcount"]  = Quad(0);
  node->m_props["parcount"]  = Quad(0);

  // Create new scope for function
  PushScope(node);

  // Annotate parameter list
  if(node->m_a2)
  {
    AnnotateImpl(node->m_a2);
  }

  // Annotate function content
  AnnotateImpl(node->m_a3);

  // Clean up the stack
  PopScope();

  // Store and check number of variables
  node->m_props["varcount"] = VarCount(node->m_a3);
  if(node->m_props["varcount"] != node->m_props["framesize"])
  {
    throw std::logic_error("Invalid frame size");
  }
  
  // Store the function call
  if(!isMember)
  {
    m_functions[name] = node;
  }
}

void
Annotator::AnnotateStatementSequence(Ast* node)
{
  // Determine list
  AstList* list = node->m_a1;

  // Initialize varcount
  Quad varcount = 0;

  // Enumerate statements
  AstList::iterator si, se;
  si = list->begin();
  se = list->end();
  for(; si != se; ++si)
  {
    Ast* s = *si;

    // AnnotateImpl statement
    AnnotateImpl(s);

    // Add to count
    if(s->m_props.contains("varcount"))
    {
      varcount += VarCount(s);
    }
  }    

  // Store varcount
  node->m_props["varcount"] = varcount;
}

void 
Annotator::AnnotateLValue(Ast* node)
{
  // Find lvalue on stack
  VarInfo vi;
  if(!m_scope->LookupVar(node->m_a1, vi))
  {
    m_reporter.ReportError(E0003, &node->m_pos, node->m_a1.GetString().c_str());
  }

  // Store offset
  node->m_props["varcount"] = (Quad)0;
  node->m_props["varinfo"]  = vi;
}

void 
Annotator::ResolveCalls()
{
  // Enumerate calls
  AstList::iterator it = m_calls.begin();
  AstList::iterator ie = m_calls.end();
  for(; it != ie; ++it)
  {
    Ast* call = *it;
    FunInfo const& fi = call->m_props["function"];
    if(fi.m_type == funNative)
    {
      call->m_props["offset"] = fi.m_offs;
    }
    else
    {
      call->m_a3 = fi.m_node;
    }
  }
}

void 
Annotator::AnnotateVariableDeclaration(Ast* node)
{
  // Init expression
  if(node->m_a2)
  {
    // Annotate init expresion *before* declaring the variable,
    // to make sure that the init expresion uses the previously
    // declared variable when initializing a shadowing variable.
    AnnotateImpl(node->m_a2);
  }

  // TODO Check for locally declared variable with same name

  // Allocate slot
  node->m_props["varcount"] = Quad(1);
  node->m_props["varinfo"] = m_scope->DeclareVariable(node->m_a1);
}

void 
Annotator::AnnotateStructDeclaration(Ast* node)
{
  // Check whether name is in use
  if(m_structs.count(node->m_a1))
  {
    m_reporter.ReportError(E0008, &node->m_pos, 
              node->m_a1.GetString().c_str());
  }

  // Store struct node
  m_structs[node->m_a1] = node;

  // Create new scope
  PushScope(node);

  // Handle member declarations
  if(node->m_a2)
  {
    AnnotateImpl(node->m_a2);
  }

  // Pop scope
  PopScope();
}

void 
Annotator::AnnotateNewExpression(Ast* node)
{
  // Check whether the type exists
  if(m_classes.count(node->m_a1) == 0)
  {
    m_reporter.ReportError(E0006, &node->m_pos, 
              node->m_a1.GetString().c_str());    
  }
}

void 
Annotator::AnnotateMemberExpression(Ast* node)
{
  // Annotate left side
  AnnotateImpl(node->m_a1);

  // Create literal for right-hand side
  Ast* lit = new Ast(literal, Variant(
    node->m_a2.GetString(), Variant::stString));

  // Replace right-hand side with literal
  node->m_a2 = lit;
}

void 
Annotator::AnnotateBreakStatement(Ast* node)
{
  Scope* scope = m_scope;

  // Find valid target scope for break
  while(scope)
  {
    // Function declaration is scope boundary
    if(scope->GetNode()->m_type == function_declaration)
    {
      scope = 0;
      break;
    }

    // Breakable statement
    if(scope->GetNode()->m_props.contains("break"))
    {
      break;
    }

    // Next scope
    scope = scope->GetParent();
  }

  // No valid scope found
  if(scope == 0)
  {
    m_reporter.ReportError(E0014, &node->m_pos);
  }

  // Link break statement to node
  node->m_props["scope"] = scope->GetNode();

  // Add break statement to scope
  any_cast<AstList>(scope->GetNode()->m_props["break"]).push_back(node);

  // Store varcount
  node->m_props["varcount"] = Quad(0);
}

void 
Annotator::AnnotateContinueStatement(Ast* node)
{
  Scope* scope = m_scope;

  // Find valid target scope for continue
  while(scope)
  {
    // Function declaration is scope boundary
    if(scope->GetNode()->m_type == function_declaration)
    {
      scope = 0;
      break;
    }

    // Continuable statement
    if(scope->GetNode()->m_props.contains("continue"))
    {
      break;
    }

    // Next scope
    scope = scope->GetParent();
  }

  // No valid scope found
  if(scope == 0)
  {
    m_reporter.ReportError(E0015, &node->m_pos);
  }

  // Link break statement to node
  node->m_props["scope"] = scope->GetNode();

  // Add break statement to scope
  any_cast<AstList>(scope->GetNode()->m_props["continue"]).push_back(node);

  // Store varcount
  node->m_props["varcount"] = Quad(0);
}

void 
Annotator::AnnotateSwitchCase(Ast* node)
{
  // Append a break statement
  AstList* list = new AstList;
  list->push_back(node->m_a2);
  list->push_back(new Ast(break_statement));
  node->m_a2 = new Ast(statement_sequence, list);

  // Annotate contents
  AnnotateImpl(node->m_a2);
}

void
Annotator::AnnotateClassDeclaration(Ast* node)
{
  AstList::iterator it, ie;

  // Init properties
  node->m_props["framesize"] = (Quad)0;
  node->m_props["varcount"]  = (Quad)0;
  
  // TODO Check class name
  m_classes[node->m_a1] = node;

  // Retrieve member list
  AstList* list = node->m_a2;

  // Create class scope
  PushScope(node);

  // Enumerate members first time
  it = list->begin();
  ie = list->end();
  for(; it != ie; ++it)
  {
    Ast* node = *it;
    if(node->m_type == variable_declaration)
    {
      // Add variable to class
      AnnotateVariableDeclaration(node);
    }
    else
    {
      // Register member function name
      m_scope->DeclareFunction(node->m_a1, node);
    }
  }

  // Pop class scope
  PopScope();
}

void 
Annotator::AnnotateFunctionCall(Ast* node)
{
  // Init number of arguments
  node->m_props["argcount"] = Quad(0);

  // Find function
  FunInfo fi;
  String name = node->m_a1;
  if(!m_scope->LookupFun(name, fi))
  {
    m_reporter.ReportError(E0005, &node->m_pos, name.c_str());
    return;
  }

  // Rebuild member call
  if(fi.m_type == funMember)
  {
    Ast* obj = new Ast(this_expression);
    Ast* rep = new Ast(function_declaration, node->m_a1, node->m_a2);
    node->m_type = member_call;
    node->m_a1 = obj;
    node->m_a2 = rep;
    AnnotateMemberCall(node);
    return;
  }

  // Count arguments
  if(node->m_a2)
  {
    AnnotateImpl(node->m_a2);
    node->m_props["argcount"] = ArgCount(node->m_a2);
  }

  // Connect function to call
  node->m_props["function"] = fi;

  // Register call
  m_calls.push_back(node);
}

void 
Annotator::AnnotateMemberCall(Ast* node)
{
  // Function name
  String name = node->m_a2->m_a1;

  // Annotate object expression
  AnnotateImpl(node->m_a1);

  // Init number of arguments
  node->m_props["argcount"] = Quad(1);

  // Count arguments
  if(node->m_a2->m_a2)
  {
    // Add argcount, add 1 for 'this'
    AnnotateImpl(node->m_a2->m_a2);
    node->m_props["argcount"] = ArgCount(node->m_a2->m_a2) + 1;
  }
}
