//////////////////////////////////////////////////////////////////////////
//
// This file is © 2007 - 2009 JJ Fahner <jan-jaap@jan-jaap.net>
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
#ifndef CSCRIPT_CONSIO_H
#define CSCRIPT_CONSIO_H

#include "io.h"

//////////////////////////////////////////////////////////////////////////
//
// Input from console
//
class ConsoleInput : public Input
{
public:

  //
  // Construct from std::cin
  //
  ConsoleInput();

  //
  // Construct from input stream
  //
  ConsoleInput(std::istream&);

  //
  // Read a string
  //
  virtual std::string Read();

protected:

  //
  // MemberMap
  //
  std::istream& m_is;

};

//////////////////////////////////////////////////////////////////////////
//
// Base class for output methods
//
class ConsoleOutput : public Output
{
public:
  
  //
  // Construct from std::cout
  //
  ConsoleOutput();
  
  //
  // Construct from output stream
  //
  ConsoleOutput(std::ostream&);

  //
  // Write a string
  //
  virtual void Write(char const*);

protected:

  //
  // MemberMap
  //
  std::ostream& m_os;

};

#endif // CSCRIPT_CONSIO_H
