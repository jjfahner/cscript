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

  AstData(int32 number) : 
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

  String const& GetString() const
  {
    AssertType(Text);
    return *m_string;
  }

  AstList* GetList() const
  {
    AssertType(List);
    return m_list;
  }

  Variant const& GetValue() const
  {
    AssertType(Value);
    return *m_value;
  }

  int32 GetNumber() const
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

  operator String const& () const
  {
    return GetString();
  }

  operator Variant const& () const
  {
    return GetValue();
  }

  operator int32 () const
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
    int32      m_number;
  };

};

#endif // CSCRIPT_ASTDATA_H
