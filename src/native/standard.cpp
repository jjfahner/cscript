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

DEFINE_NATIVE_LINKAGE(Standard)

//////////////////////////////////////////////////////////////////////////

NATIVE_CALL("__native eval(string code)")
{
  return evaluator->Eval(args[0].GetString());
}

NATIVE_CALL("__native reset()")
{
  throw reset_exception();
}

NATIVE_CALL("__native collect()")
{
  evaluator->Collect();
  return Value();
}

NATIVE_CALL("__native object_count()")
{
  return Object::GetObjects().size();
};

NATIVE_CALL("__native exit(int exitcode = 0)")
{
  // Exit with specified exit code
  exit((int) args[0].GetInt());

  // Never executed
  throw std::runtime_error("exit() failed");
}

NATIVE_CALL("__native quit(int exitcode = 0)")
{
  // Exit with specified exit code
  exit((int) args[0].GetInt());

  // Never executed
  throw std::runtime_error("exit() failed");
}

//////////////////////////////////////////////////////////////////////////

NATIVE_CALL("__native dump(fn)")
{
  // Retrieve function
  Function* fun = dynamic_cast<Function*>(args[0].GetObject());

  // Prepare arguments
  Arguments invargs;
  invargs.push_back(Value());

  // Pass all objects to function
  Objects::const_iterator it = Object::GetObjects().begin();
  Objects::const_iterator ie = Object::GetObjects().end();
  for(; it != ie; ++it)
  {
    invargs[0] = *it;
    fun->Execute(evaluator, invargs);
  }

  // Done
  return Value();
}
