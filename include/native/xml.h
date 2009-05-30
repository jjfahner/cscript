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
#ifndef CSCRIPT_XML_H
#define CSCRIPT_XML_H

#include <cscript.h>
#include <stubs.h>
#include <object.h>

class XmlNode : public Object
{
public:

  DEF_NATIVE_CALLS(XmlNode, Object)

  //
  // Retrieve parent node
  //
  __native_roprop ObjectPtr parentNode();
  
  //
  // Get/set inner text
  //
  __native_rwprop StringCRef innerText();
  void SetinnerText(StringCRef text);

  //
  // Get/set xml
  //
  __native_rwprop StringCRef xml();
  void Setxml(StringCRef xml);

protected:

  //
  // Members
  //
  String m_text;
  String m_xml;

};

#endif // CSCRIPT_XML_H
