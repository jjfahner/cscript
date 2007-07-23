#ifndef CSCRIPT_CONTEXT_H
#define CSCRIPT_CONTEXT_H

#include "var.h"

typedef unsigned __int8   Byte;
typedef unsigned __int16  Word;
typedef unsigned __int32  Quad;

class ParseContext
{
public:

  ParseContext() : 
  m_code (0),
  m_size (0),
  m_used (0)
  {
  }

  Byte* GetCode() const
  {
    return m_code;
  }

  Quad AddLiteral(Variant const& value)
  {
    unsigned long index = (Quad) m_literals.size();
    m_literals[index] = value;
    return index;
  }

  Variant& GetLiteral(Quad id) const
  {
    Literals::const_iterator it;
    if((it = m_literals.find(id)) == m_literals.end())
    {
      throw std::runtime_error("Unknown literal");
    }
    return const_cast<Variant&>(it->second);
  }

  Quad AddVar(std::wstring const& name)
  {
    identifiers::iterator it;
    if((it = m_variables.find(name)) != m_variables.end())
    {
      throw std::runtime_error("Duplicate variable name");
    }
    Quad index = (Quad)m_variables.size();
    m_variables[name] = index;
    return index;
  }

  Quad GetVar(std::wstring const& name)
  {
    identifiers::iterator it;
    if((it = m_variables.find(name)) == m_variables.end())
    {
      throw std::runtime_error("Invalid variable name ");
    }
    return it->second;
  }

  void PushByte(Byte ch)
  {
    Reserve(m_used + 1);
    *(m_code+m_used) = ch;
    m_used += 1;
  }

  void PushWord(Word sh)
  {
    Reserve(m_used + 2);
    *(Word*)(m_code+m_used) = sh;
    m_used += 2;
  }

  void PushQuad(Quad ln)
  {
    Reserve(m_used + 4);
    *(Quad*)(m_code+m_used) = ln;
    m_used += 4;
  }

  void Reserve(size_t size)
  {
    if(m_size < size)
    {
      m_code = (Byte*) realloc(m_code, size * 2);
      size *= 2;
    }
  }

private:

  //
  // Machine code
  //
  Byte*  m_code;
  size_t m_size;
  size_t m_used;

  //
  // Constants
  //
  typedef std::map<Quad, Variant> Literals;
  Literals m_literals;

  // Identifiers
  typedef std::map<std::wstring, Quad> identifiers;
  identifiers m_variables; 

};

#endif // #ifndef CSCRIPT_CONTEXT_H
