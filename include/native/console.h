//////////////////////////////////////////////////////////////////////////
//
// This file is © 2009 - 2011 JJ Fahner <jan-jaap@jan-jaap.net>
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
#ifndef CSCRIPT_NATIVE_CONSIO_H
#define CSCRIPT_NATIVE_CONSIO_H

#include <native.h>
#include <object.h>
#include <native/stream.h>

class Console : public Object, public IOStream
{
public:

  DEF_NATIVE_CALLS(Console, Object);

  //
  // Read character
  //
  __native_method Value ReadChar();

  //
  // Read string
  //
  __native_method Value ReadString();

  //
  // Read line
  //
  __native_method Value ReadLn();

  //
  // Write character
  //
  __native_method void WriteChar(ValueCRef value);

  //
  // Print supplied arguments
  //
  __native_method void Write(ArgsCRef args);

  //
  // Print supplied arguments, append newline
  //
  __native_method void WriteLn(ArgsCRef args);

};

#endif // CSCRIPT_NATIVE_CONSIO_H
