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
#ifndef CSCRIPT_MACHINE_H
#define CSCRIPT_MACHINE_H

#include "types.h"
#include "var.h"

class Class;

class Machine
{
public:

  //
  // Execute code
  //
  void Execute(Byte* code, Quad offset = 0);

  //
  // Read classes
  //
  void ReadClasses(Byte* code);

private:

  //
  // Types
  //
  typedef std::map<String, Class*> ClassMap;

  //
  // Members
  //
  ClassMap m_classes;

};

#endif // #ifndef CSCRIPT_MACHINE_H
