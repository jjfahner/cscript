#include "astdata.h"
#include "ast.h"
#include "astlist.h"

AstData::AstData(Ast* node) : 
m_type    (Node),
m_node    (node)
{
  ++m_node->m_refs;
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

  case Value: 
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
    case Value:
      m_value = new Variant(*rhs.m_value);
      break;
    case Number:
      m_number = rhs.m_number;
      break;
    }
  }
  return *this;
}
