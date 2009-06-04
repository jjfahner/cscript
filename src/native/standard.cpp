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
CScriptMethods::Eval(StringCRef code, bool isFile)
{
  return CurEval.Eval(code, isFile);
}

void 
CScriptMethods::Reset()
{
  throw ResetException();
}

Value
CScriptMethods::Collect()
{
  // Perform collection
  GC::CollectInfo const& ci = CurEval.Collect();

  // Create result object
  Object* obj = new ScriptObject();
  CurEval.MakeTemp(obj);

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
CScriptMethods::Lookup(StringCRef name)
{
  return CurEval.GetScope()->Get(name);
}

int64 
CScriptMethods::Ticks()
{
  return (int64)Timer::Ticks();
}

void 
CScriptMethods::DebugParser(bool value)
{
#ifdef _DEBUG
  CurEval.DebugParser(value);
#endif
}

Value
CScriptMethods::Exec(StringCRef command)
{
  // Run the command
  FILE* file = _popen(command.c_str(), "rt");
  if(file == 0)
  {
    return Value();
  }

  // Wait for completion
  return _pclose(file);

  //   // Read from handle until done
  //   for(;;)
  //   {
  //     char buf[1024];
  //     size_t read = fread(buf, 1, sizeof(buf), file);
  //     buf[read] = 0;
  //     std::cout << buf;
  //     if(read < sizeof(buf))
  //     {
  //       break;
  //     }
  //   }
  // 
}