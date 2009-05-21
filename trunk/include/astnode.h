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
#include <ast.h>

class DataType;

class AstNode : public Object 
{
public:
  
  //
  // Constructors
  //
  AstNode(AstTypes type) : 
    m_type (type), m_updateDone(false) {}
  AstNode(AstTypes type, Value const& a1) : 
    m_type (type), m_a1 (a1), m_updateDone(false) {}
  AstNode(AstTypes type, Value const& a1, Value const& a2) : 
    m_type (type), m_a1 (a1), m_a2 (a2), m_updateDone(false) {}
  AstNode(AstTypes type, Value const& a1, Value const& a2, Value const& a3) : 
    m_type (type), m_a1 (a1), m_a2 (a2), m_a3 (a3), m_updateDone(false) {}
  AstNode(AstTypes type, Value const& a1, Value const& a2, Value const& a3, Value const& a4) : 
    m_type (type), m_a1 (a1), m_a2 (a2), m_a3 (a3), m_a4 (a4), m_updateDone(false) {}

  //
  // Members
  //
  AstTypes  m_type;
  DataType* m_dataType;
  Value     m_a1;
  Value     m_a2;
  Value     m_a3;
  Value     m_a4;
  bool      m_updateDone;

  //
  // Update list
  //
  virtual void UpdateMembers()
  {
    // Avoid recursion or multiple adds
    if(m_updateDone) 
    {
      return;
    }
    m_updateDone = true;
    
    // Add members
    Add(0, m_type);
    if(!m_a1.Empty()) Add(1, m_a1);
    if(!m_a2.Empty()) Add(2, m_a2);
    if(!m_a3.Empty()) Add(3, m_a3);
    if(!m_a4.Empty()) Add(4, m_a4);
  }

  //
  // Mark reachable objects
  //
  virtual void MarkObjects(GC::ObjectVec& grey)
  {
    Object::MarkObjects(grey);
    if(!m_updateDone)
    {
      if(GC::Object* obj = m_a1.GetGCObject()) grey.push_back(obj);
      if(GC::Object* obj = m_a2.GetGCObject()) grey.push_back(obj);
      if(GC::Object* obj = m_a3.GetGCObject()) grey.push_back(obj);
      if(GC::Object* obj = m_a4.GetGCObject()) grey.push_back(obj);
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
