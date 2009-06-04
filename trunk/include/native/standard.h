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
#ifndef CSCRIPT_NATIVE_STANDARD_H
#define CSCRIPT_NATIVE_STANDARD_H

#include <native.h>

class CScriptMethods : public Object
{
public:

  DEF_NATIVE_CALLS(CScriptMethods, Object);

  //
  // Evaluate code
  // TODO add default value = false
  __native_method Value Eval(StringCRef code, bool isFile);

  //
  // Reset the evaluator
  //
  __native_method void Reset();

  //
  // Perform garbage collection cycle
  //
  __native_method Value Collect();

  //
  // Retrieve total number of objects
  //
  __native_method int64 ObjectCount();

  //
  // Exit process
  // TODO add default value = 0
  __native_method void Exit(int64 exitcode);

  //
  // Exit process
  // TODO add default value = 0
  __native_method void Quit(int64 exitcode);

  //
  // Lookup a symbol value
  //
  __native_method Value Lookup(StringCRef name);

  //
  // Current number of ticks
  // TODO this doesn't really belong here
  __native_roprop int64 Ticks();

  //
  // Enable parser debugging. Only works in debug versions
  //
  __native_method void DebugParser(bool value);

  //
  // Execute a command
  //
  __native_method Value Exec(StringCRef command);

  //
  // Run code in a different thread
  //
  __native_method void StartThread(ValueCRef code);

};

#endif // CSCRIPT_NATIVE_STANDARD_H
