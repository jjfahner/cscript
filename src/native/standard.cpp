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

DEFINE_NATIVE_LINKAGE(Standard)

//////////////////////////////////////////////////////////////////////////

NATIVE_CALL("eval(string code, bool isFile = false)")
{
  return evaluator->Eval(args[0].GetString(), args[1].GetBool());
}

NATIVE_CALL("reset()")
{
  throw ResetException();
}

NATIVE_CALL("collect()")
{
  // Perform collection
  GC::CollectInfo const& ci = evaluator->Collect();

  // Create result object
  Object* obj = new ScriptObject();
  evaluator->MakeTemp(obj);

  // Copy fields
  obj->Set("markPhase",    (Value::Int) ci.m_markPhase);
  obj->Set("deletePhase",  (Value::Int) ci.m_deletePhase);
  obj->Set("numCycles",    (Value::Int) ci.m_numCycles);
  obj->Set("numRemaining", (Value::Int) ci.m_numRemaining);
  obj->Set("numCollected", (Value::Int) ci.m_numCollected);

  // Done
  return obj;
}

NATIVE_CALL("object_count()")
{
  return GC::ObjectCount();
};

NATIVE_CALL("exit(int exitcode = 0)")
{
  // Exit with specified exit code
  exit((int) args[0].GetInt());

  // Never executed
  throw std::runtime_error("exit() failed");
}

NATIVE_CALL("quit(int exitcode = 0)")
{
  // Exit with specified exit code
  exit((int) args[0].GetInt());

  // Never executed
  throw std::runtime_error("exit() failed");
}

//////////////////////////////////////////////////////////////////////////

NATIVE_CALL("lookup(name)")
{
  return evaluator->GetScope()->Get(args[0]);
}

//////////////////////////////////////////////////////////////////////////

NATIVE_CALL("ticks()")
{
  return (Value::Int) Timer::Ticks();
}

//////////////////////////////////////////////////////////////////////////

#ifdef _DEBUG
NATIVE_CALL("dbgparser(bool enable)")
{
  evaluator->DebugParser(args[0].GetBool());
  return Value();
}
#endif

//////////////////////////////////////////////////////////////////////////

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
