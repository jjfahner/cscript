#ifndef CSCRIPT_ASTDATA_H
#define CSCRIPT_ASTDATA_H

#include "types.h"
#include "var.h"

class AstData
{
public:

  enum Types {
    Null,
    Node,
    Text,
    List,
    Value,
    Number
  };

  AstData() : 
  m_type    (Null),
  m_number  (0)
  {
  }

  AstData(AstData const& rhs) : 
  m_type    (Null),
  m_number  (0)
  {
    *this = rhs;
  }

  AstData(Ast* node);
  AstData(AstList* list);

  AstData(String const& text) : 
  m_type    (Text),
  m_string  (new String(text))
  {
  }

  AstData(Variant const& value) :
  m_type    (Value),
  m_value   (new Variant(value))
  {
  }

  AstData(Quad number) : 
  m_type    (Number),
  m_number  (number)
  {
  }

  ~AstData()
  {
    Clear();
  }

  bool Empty() const
  {
    return m_type == Null;
  }

  void Clear();

  void Release()
  {
    m_type   = Null;
    m_number = 0;
  }

  operator bool () const
  {
    return !Empty();
  }

  bool operator ! () const
  {
    return Empty();
  }

  Types Type() const
  {
    return m_type;
  }

  void AssertType(Types type) const
  {
    if(m_type != type)
    {
      throw std::runtime_error("Invalid type");
    }
  }

  Ast* GetNode() const
  {
    AssertType(Node);
    return m_node;
  }

  String GetString() const
  {
    AssertType(Text);
    return *m_string;
  }

  AstList* GetList() const
  {
    AssertType(List);
    return m_list;
  }

  Variant GetValue() const
  {
    AssertType(Value);
    return *m_value;
  }

  Quad GetNumber() const
  {
    AssertType(Number);
    return m_number;
  }

  operator Ast* () const
  {
    return GetNode();
  }

  operator AstList* () const
  {
    return GetList();
  }

  operator String () const
  {
    return GetString();
  }

  operator Variant () const
  {
    return GetValue();
  }

  operator Quad () const
  {
    return GetNumber();
  }

  Ast* operator -> () const
  {
    return GetNode();
  }

  AstData const& operator = (AstData const& rhs);

private:

  Types m_type;

  union {
    Ast*      m_node;
    String*   m_string;
    AstList*  m_list;
    Variant*  m_value;
    Quad      m_number;
  };

};

#endif // CSCRIPT_ASTDATA_H
