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
#ifndef CSCRIPT_THREAD_H
#define CSCRIPT_THREAD_H

#include <cscript.h>

class Object;
class Scope;

class Thread
{
public:

  //
  // Evaluate code in thread
  //
  static void Eval(Object* ast);

private:

  //
  // Thread proc
  //
  static void ThreadProc(void*);

};

#endif // CSCRIPT_THREAD_H
