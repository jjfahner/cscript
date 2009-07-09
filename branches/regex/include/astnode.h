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
#ifndef CSCRIPT_ASTOBJ_H
#define CSCRIPT_ASTOBJ_H

#include <cscript.h>
#include <object.h>
#include <value.h>
#include <ast.h>
#include <list.h>
#include <datatype.h>
#include <native.h>

#include <map>

class DataType;

//
// Ast attributes
//
enum AstAttrs
{
  aaResultType,
  aaSideEffects,
  aaDeclaration,
};


class AstNode : public Object 
{
public:

  DEF_NATIVE_CALLS(AstNode, Object);

  //
  // Attributes map
  //
  typedef std::map<AstAttrs, Value> AttrMap;
  
  //
  // Constructors
  //
  AstNode() :
    m_type (invalid) {}
  AstNode(AstTypes type) : 
    m_type (type) {}
  AstNode(AstTypes type, Value const& a1) : 
    m_type (type), m_a1 (a1) {}
  AstNode(AstTypes type, Value const& a1, Value const& a2) : 
    m_type (type), m_a1 (a1), m_a2 (a2) {}
  AstNode(AstTypes type, Value const& a1, Value const& a2, Value const& a3) : 
    m_type (type), m_a1 (a1), m_a2 (a2), m_a3 (a3) {}
  AstNode(AstTypes type, Value const& a1, Value const& a2, Value const& a3, Value const& a4) : 
    m_type (type), m_a1 (a1), m_a2 (a2), m_a3 (a3), m_a4 (a4) {}

  //
  // Members
  //
  AstTypes  m_type;
  Value     m_a1;
  Value     m_a2;
  Value     m_a3;
  Value     m_a4;
  AttrMap   m_attrs;

  //
  // Native accessors
  //
  __native_roprop int64 Type() { return m_type; }
  __native_roprop Value a1() { return m_a1; }
  __native_roprop Value a2() { return m_a2; }
  __native_roprop Value a3() { return m_a3; }
  __native_roprop Value a4() { return m_a4; }

  //
  // Easy attribute access
  //
  Value& operator [] (AstAttrs attr)
  {
    return m_attrs[attr];
  }

  //
  // Mark reachable objects
  //
  virtual void MarkObjects(GCObjectVec& grey)
  {
    // Mark object members
    Object::MarkObjects(grey);

    // Mark ast parts
    GC::Mark(grey, m_a1);
    GC::Mark(grey, m_a2);
    GC::Mark(grey, m_a3);
    GC::Mark(grey, m_a4);

    // Mark attributes
    if(!m_attrs.empty())
    {
      AttrMap::iterator it, ie;
      for(it = m_attrs.begin(), ie = m_attrs.end(); it != ie; ++it)
      {
        GC::Mark(grey, it->second);
      }
    }
  }
};

//////////////////////////////////////////////////////////////////////////
//
// Inline ast member acccessors
//

inline AstTypes Ast_Type(Object* obj) {
  return static_cast<AstNode*>(obj)->m_type;
}
inline Value const& Ast_A1(Object* obj) {
  return static_cast<AstNode*>(obj)->m_a1;
}
inline Value const& Ast_A2(Object* obj) {
  return static_cast<AstNode*>(obj)->m_a2;
}
inline Value const& Ast_A3(Object* obj) {
  return static_cast<AstNode*>(obj)->m_a3;
}
inline Value const& Ast_A4(Object* obj) {
  return static_cast<AstNode*>(obj)->m_a4;
}
inline AstNode* AstNode_A1(Object* obj) {
  return static_cast<AstNode*>(Ast_A1(obj).GetObject());
}
inline AstNode* AstNode_A2(Object* obj) {
  return static_cast<AstNode*>(Ast_A2(obj).GetObject());
}
inline AstNode* AstNode_A3(Object* obj) {
  return static_cast<AstNode*>(Ast_A3(obj).GetObject());
}
inline AstNode* AstNode_A4(Object* obj) {
  return static_cast<AstNode*>(Ast_A4(obj).GetObject());
}
inline List* AstList_A1(Object* obj) {
  List* l = dynamic_cast<List*>(Ast_A1(obj).GetObject());
  return l ? l : throw std::runtime_error("Object is not a list");
}
inline List* AstList_A2(Object* obj) {
  List* l = dynamic_cast<List*>(Ast_A2(obj).GetObject());
  return l ? l : throw std::runtime_error("Object is not a list");
}


inline AstTypes Ast_Type(Object& obj) {
  return static_cast<AstNode&>(obj).m_type;
}
inline Value const& Ast_A1(Object& obj) {
  return static_cast<AstNode&>(obj).m_a1;
}
inline Value const& Ast_A2(Object& obj) {
  return static_cast<AstNode&>(obj).m_a2;
}
inline Value const& Ast_A3(Object& obj) {
  return static_cast<AstNode&>(obj).m_a3;
}
inline Value const& Ast_A4(Object& obj) {
  return static_cast<AstNode&>(obj).m_a4;
}
inline AstNode& AstNode_A1(Object& obj) {
  return static_cast<AstNode&>(*Ast_A1(obj).GetObject());
}
inline AstNode& AstNode_A2(Object& obj) {
  return static_cast<AstNode&>(*Ast_A2(obj).GetObject());
}
inline AstNode& AstNode_A3(Object& obj) {
  return static_cast<AstNode&>(*Ast_A3(obj).GetObject());
}
inline AstNode& AstNode_A4(Object& obj) {
  return static_cast<AstNode&>(*Ast_A4(obj).GetObject());
}

//////////////////////////////////////////////////////////////////////////
//
// Ast recursor. Recurse through right-recursive ast structure.
//

class AstIterator
{
  Object*   m_root;
  Object*   m_iter;
  AstTypes  m_type;

public:

  AstIterator() :
  m_root (0),
  m_iter (0),
  m_type (invalid)
  {
  }

  AstIterator(AstIterator const& rhs) :
  m_root (rhs.m_root),
  m_iter (rhs.m_iter),
  m_type (rhs.m_type)
  {
  }

  AstIterator(Object* root, AstTypes recursionType) :
  m_root (root),
  m_iter (root),
  m_type (recursionType)
  {
  }

  friend bool operator == (AstIterator const& lhs, AstIterator const& rhs)
  {
    if(lhs.m_root && rhs.m_root && lhs.m_root != rhs.m_root)
    {
      throw std::runtime_error("Comparing iterators from different subtrees");
    }
    return lhs.m_iter == rhs.m_iter;
  }

  friend bool operator != (AstIterator const& lhs, AstIterator const& rhs)
  {
    return !(lhs == rhs);
  }

  bool AtEnd() const
  {
    return m_iter == 0;
  }

  operator bool () const
  {
    return !AtEnd();
  }

  bool operator ! () const
  {
    return AtEnd();
  }

  Object* GetValue() const
  {
    if(m_iter == 0)
    {
      throw std::runtime_error("Dereferencing invalid ast iterator");
    }

    if(Ast_Type(m_iter) == m_type)
    {
      return Ast_A1(m_iter);
    }

    return m_iter;
  }

  Object* operator * () const
  {
    return GetValue();
  }

  Object* operator -> () const
  {
    return GetValue();
  }

  void Next()
  {
    if(m_iter == 0)
    {
      throw std::runtime_error("Dereferencing invalid ast iterator");
    }

    if(Ast_Type(m_iter) == m_type)
    {
      m_iter = Ast_A2(m_iter);
    }
    else
    {
      m_iter = 0;
    }
  }

  AstIterator const& operator ++ () 
  {
    Next();
    return *this;
  }

  AstIterator operator ++ (int)
  {
    AstIterator temp(*this);
    Next();
    return temp;
  }

};

#endif // CSCRIPT_ASTOBJ_H
