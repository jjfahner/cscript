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
#ifndef CSCRIPT_CONVERT_H
#define CSCRIPT_CONVERT_H

#include <cscript.h>

//
// Convert decimal string to decimal
//
int32 str2dec(char const* src);

//
// Convert string to floating point
//
// TODO: this is a dummy function
//
int str2flt(char const* src);

//
// Convert hexadecimal string to integer value
//
int32 hex2dec(char const* src);

//
// Convert binary string to integer value
//
int32 bin2dec(char const* src);

//
// Convert roman string to integer value
//
int32 rom2dec(char const* src);

#endif // CSCRIPT_CONVERT_H
