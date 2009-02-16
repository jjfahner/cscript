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
#include <native/file.h>
#include <eval.h>

#include <iostream>

DEFINE_NATIVE_LINKAGE(File)

//////////////////////////////////////////////////////////////////////////

class File : public Object
{
public:

  //
  // Construction
  //
  File(Evaluator* evaluator)
  {
    NATIVE_METHOD(File, Open, "Open(string path, string mode)");
    NATIVE_METHOD(File, Close, "Close()");
  }

  //
  // Open file
  //
  Value Open(Evaluator*, Arguments const& args)
  {
    return Value();
  }

  //
  // Close file
  //
  Value Close(Evaluator*, Arguments const& args)
  {
    return Value();
  }

};

//////////////////////////////////////////////////////////////////////////

NATIVE_CALL("CreateFile()")
{
  return new File(evaluator);
};
