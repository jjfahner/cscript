//////////////////////////////////////////////////////////////////////////
//
// This file is © 2009 JJ Fahner <jan-jaap@jan-jaap.net>
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
#ifndef CSCRIPT_CONTEXT_H
#define CSCRIPT_CONTEXT_H

#include <cscript.h>

class Evaluator;

class Context
{
public:

  //
  // Retrieve current instance
  //
  static Context* Get();

  //
  // Construction
  //
  Context(Evaluator*);

  //
  // Destruction
  //
  ~Context();

  //
  // Retrieve current evaluator instance
  //
  Evaluator* GetEvaluator() const;

private:

  //
  // Members
  //
  Context*    m_prevCtx;
  Evaluator*  m_evaluator;

  //
  // Pointer to thread-local instances
  //
  static TLS_SLOT Context* g_context;

};

//////////////////////////////////////////////////////////////////////////

/*static*/ inline 
Context* Context::Get()
{
  return g_context;
}

inline 
Context::Context(Evaluator* evaluator) :
m_evaluator (evaluator),
m_prevCtx   (g_context)
{
  m_prevCtx = g_context;
  g_context = this;
}

inline
Context::~Context()
{
  g_context = m_prevCtx;
}

inline
Evaluator* 
Context::GetEvaluator() const
{
  return m_evaluator;
}

// Easy access to current context and members
#define CurCtx    (*(Context::Get()))
#define CurEval   (*(CurCtx.GetEvaluator()))

#endif // CSCRIPT_CONTEXT_H
