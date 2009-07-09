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
#include <native/xml.h>
#include <native.h>

Object* 
XmlNode::parentNode()
{
  return 0;
}

StringCRef 
XmlNode::innerText()
{
  return m_text;
}

void 
XmlNode::SetinnerText(StringCRef text)
{
  m_text = text;
}

StringCRef
XmlNode::xml()
{
  return m_xml;
}

void 
XmlNode::Setxml(StringCRef xml)
{
  m_xml = xml;
}

//////////////////////////////////////////////////////////////////////////

/*
TODO
NATIVE_CALL("CreateXmlNode()")
{
  return new XmlNode();
}
*/
