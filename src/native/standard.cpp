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
#include <native/standard.h>
#include <eval.h>
#include <scope.h>
#include <timer.h>
#include <lexstream.h>
#include <scriptobj.h>

//////////////////////////////////////////////////////////////////////////

Value 
CScriptMethods::Eval(StringCRef code, bool isFile, EvalRef evaluator)
{
  return evaluator.Eval(code, isFile);
}

void 
CScriptMethods::Reset()
{
  throw ResetException();
}

Value
CScriptMethods::Collect(EvalRef evaluator)
{
  // Perform collection
  GC::CollectInfo const& ci = evaluator.Collect();

  // Create result object
  Object* obj = new ScriptObject();
  evaluator.MakeTemp(obj);

  // Copy fields
  obj->Set("markPhase",    (Value::Int) ci.m_markPhase);
  obj->Set("deletePhase",  (Value::Int) ci.m_deletePhase);
  obj->Set("numCycles",    (Value::Int) ci.m_numCycles);
  obj->Set("numRemaining", (Value::Int) ci.m_numRemaining);
  obj->Set("numCollected", (Value::Int) ci.m_numCollected);

  // Done
  return obj;
}

int64 
CScriptMethods::ObjectCount()
{
  return GC::ObjectCount();
};

void 
CScriptMethods::Exit(int64 exitcode)
{
  // Exit with specified exit code
  exit((int)exitcode);

  // Never executed
  throw std::runtime_error("Exit() failed");
}

void 
CScriptMethods::Quit(int64 exitcode)
{
  // Exit with specified exit code
  exit((int)exitcode);

  // Never executed
  throw std::runtime_error("Quit() failed");
}

Value 
CScriptMethods::Lookup(StringCRef name, EvalRef evaluator)
{
  return evaluator.GetScope()->Get(name);
}

int64 
CScriptMethods::Ticks()
{
  return (int64)Timer::Ticks();
}

void 
CScriptMethods::DebugParser(bool value, EvalRef evaluator)
{
#ifdef _DEBUG
  evaluator.DebugParser(value);
#endif
}


//////////////////////////////////////////////////////////////////////////
/*
#include <xmlparser.h>
#include <fstream>

NATIVE_CALL("parseXml(string file)")
{
  // Open file
  std::ifstream is(args[0].GetString().c_str());
  if(!is.is_open())
  {
    throw std::runtime_error("Failed to open file");
  }

  // Create lexer stream
  LexStream ls(is);

  // Run parser
  XmlParser parser;
  return parser.Parse(ls);
}
*/
