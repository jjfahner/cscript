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
#include "convert.h"

Quad 
hex2dec(char const* src)
{
  Quad value = 0;

  // Skip type marker
  if(*src == '0' && *(src+1) == 'x')
  {
    src += 2;
  }

  // Calculate value
  while(*src)
  {
    value *= 16;
    if(*src >= '0' && *src <= '9')
    {
      value += *src++ - '0';
    }
    else if(*src >= 'a' && *src <= 'f')
    {
      value += *src++ - 'a' + 10;
    }
    else if(*src >= 'A' && *src <= 'F')
    {
      value += *src++ - 'A' + 10;
    }
    else
    {
      return 0;
    }
  }

  // Done
  return value;
}

Quad 
bin2dec(char const* src)
{
  Quad value = 0;

  // Skip type marker
  if(*src == '0' && *(src+1) == 'b')
  {
    src += 2;
  }

  // Calculate value
  while(*src)
  {
    value *= 2;
    if(*src == '0')
    {
      ++src;
    }
    else if(*src == '1')
    {
      value += 1;
      ++src;
    }
    else
    {
      return 0;
    }
  }

  // Done
  return value;
}


inline int 
romdigit(char ch)
{
  switch(ch)
  {
  case 'M': return 1000;
  case 'D': return  500;
  case 'C': return  100;
  case 'L': return   50;
  case 'X': return   10;
  case 'V': return    5;
  case 'I': return    1;
  case 'O': return    0;
  default : return    0;
  }
}

Quad 
rom2dec(char const* src)
{
  Quad value = 0;

  // Skip type marker
  if(*src == '0' && *(src+1) == 'r')
  {
    src += 2;
  }


  while(*src)
  {
    int val = romdigit(*src++);
    if(*src != 0)
    {
      int val2 = romdigit(*src);
      if(val2 > val)
      {
        ++src;
        val = val2 - val;
      }
    }
    value += val;
  }

  // Done
  return value;
}
