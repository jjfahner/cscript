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
#include "consio.h"
#include <iostream>

//
// Global streams
//
Input&   csin  = *new ConsoleInput;
Output&  csout = *new ConsoleOutput;
Output&  cserr = *new ConsoleOutput;

//////////////////////////////////////////////////////////////////////////
//
// ConsoleInput implementation
//

ConsoleInput::ConsoleInput() :
m_is (std::cin)
{
}

ConsoleInput::ConsoleInput(std::istream& is) :
m_is (is)
{
}

std::string 
ConsoleInput::Read()
{
  std::string s;
  m_is >> s;
  return s;
}

//////////////////////////////////////////////////////////////////////////
//
// ConsoleOutput implementation
//

ConsoleOutput::ConsoleOutput() :
m_os (std::cout)
{
}

ConsoleOutput::ConsoleOutput(std::ostream& os) :
m_os (os)
{
}

void 
ConsoleOutput::Write(char const* data)
{
  m_os << data;
}
