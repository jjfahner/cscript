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

DEFINE_NATIVE_LINKAGE(Standard)

//////////////////////////////////////////////////////////////////////////

NATIVE_CALL("eval(string code)")
{
  return evaluator->Eval(args[0].GetString());
}

NATIVE_CALL("reset()")
{
  throw ResetException();
}

NATIVE_CALL("collect()")
{
  evaluator->Collect();
  return Value();
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
  RValue* pValue;
  if(evaluator->GetScope()->Lookup(args[0], pValue))
  {
    return *pValue;
  }
  return Value();
}

//////////////////////////////////////////////////////////////////////////

NATIVE_CALL("ticks()")
{
  return (Value::Int) Timer::Ticks();
}
