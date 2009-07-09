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
#ifndef CSCRIPT_TOKENS_H
#define CSCRIPT_TOKENS_H

//
// Include main header
//
#include <cscript.h>
#include <gcstring.h>

//
// Define token structure
//
struct Token
{
  int       m_type;
  GCString* m_text;

  operator String const& () const
  {
    return *m_text;
  }

  operator char () const
  {
    return m_text->at(0);
  }
};

#endif // #ifndef CSCRIPT_TOKENS_H
