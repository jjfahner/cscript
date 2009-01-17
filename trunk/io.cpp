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
#include "io.h"

void 
Output::Write(int64 n)
{
  // Setup buffer
  char buf[100];
  char* p = buf + 50;
  
  // Null-terminate
  *p-- = 0;

  // Handle negative
  bool neg = false;
  if(n < 0)
  {
    neg = true;
    n = -n;
  }

  // Write number
  while(n)
  {
    *p-- = (char)(n % 10) + '0';
    n /= 10;
  }

  // Prepend negative
  if(neg)
  {
    *p-- = '-';
  }

  // Write number
  Write(++p);
}
