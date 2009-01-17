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
#include "astdata.h"
#include "ast.h"
#include "astlist.h"

AstData::AstData(Ast* node) : 
m_type    (Node),
m_node    (node)
{
  if(m_node)
  {
    ++m_node->m_refs;
  }
  else
  {
    m_type = Null;
  }
}

AstData::AstData(AstList* list) : 
m_type    (List),
m_list    (list)
{
  ++m_list->m_refs;
}

void 
AstData::Clear()
{
  switch(m_type)
  {
  case Node:  
    if(--m_node->m_refs == 0)
    {
      delete m_node;
    }
    break;
  
  case Text:  
    delete m_string;  
    break;
  
  case List:  
    if(--m_list->m_refs == 0)
    {
      delete m_list;    
    }
    break;

  case Val: 
    delete m_value;   
    break;
  }

  m_type = Null;
  m_number = 0;
}

AstData const& 
AstData::operator = (AstData const& rhs)
{
  if(this != &rhs)
  {
    Clear();
    switch(m_type = rhs.m_type)
    {
    case Node: 
      m_node = rhs.m_node;
      ++m_node->m_refs;
      break;
    case Text: 
      m_string = new String(*rhs.m_string); 
      break;
    case List:
      m_list = rhs.m_list;
      ++m_list->m_refs;
      break;
    case Val:
      m_value = new Value(*rhs.m_value);
      break;
    case Number:
      m_number = rhs.m_number;
      break;
    }
  }
  return *this;
}
