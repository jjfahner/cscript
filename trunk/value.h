#ifndef CSCRIPT_VALUE_H
#define CSCRIPT_VALUE_H

#include <string>

class Object;

class Value
{
public:

  enum Types
  {
    tNull,
    tBool,
    tInt,
    tReal,
    tString,
    tObject
  };

  typedef bool          Bool;
  typedef int           Int;
  typedef std::string   String;

  Value() : m_pdata(&m_ldata)
  {
    m_pdata->m_type = tNull;
    m_pdata->m_int = 0;
  }

  Value(Value const& rhs) : m_pdata(&m_ldata)
  {
    if(rhs.IsReference())
    {
      m_pdata = rhs.m_pdata;
    }
    else
    {
      *this = rhs;
    }
  }

  Value(Value* v) : m_pdata(v->m_pdata)
  {
  }

  Value(Bool val) : m_pdata(&m_ldata)
  {
    m_pdata->m_type = tBool;
    m_pdata->m_bool = val;
  }

  Value(Int val) : m_pdata(&m_ldata)
  {
    m_pdata->m_type = tInt;
    m_pdata->m_int = val;
  }

  Value(String val) : m_pdata(&m_ldata)
  {
    m_pdata->m_type = tString;
    m_pdata->m_string = new String(val);
  }

  Value(char const* val) : m_pdata(&m_ldata)
  {
    m_pdata->m_type = tString;
    m_pdata->m_string = new String(val);
  }

  Value(Object* obj) : m_pdata(&m_ldata)
  {
    m_pdata->m_type = tObject;
    m_pdata->m_object = obj;
  }

  ~Value()
  {
    Clear();
  }

  void Clear(bool deep = false)
  {
    if(!deep && IsReference())
    {
      m_pdata = &m_ldata;
    }
    else
    {
      switch(m_pdata->m_type)
      {
      case tString: delete m_pdata->m_string; break;
      }
    }
    m_pdata->m_type = tNull;
    m_pdata->m_int  = 0;
  }

  Types Type() const
  {
    return m_pdata->m_type;
  }

  bool Empty() const
  {
    return m_pdata->m_type == tNull;
  }

  bool IsReference() const
  {
    return m_pdata != &m_ldata;
  }

  void* GetIdentity() const
  {
    return m_pdata;
  }

  Bool GetBool() const
  {
    AssertType(tBool);
    return m_pdata->m_bool;
  }

  Int GetInt() const
  {
    AssertType(tInt);
    return m_pdata->m_int;
  }

  String const& GetString() const
  {
    AssertType(tString);
    return *m_pdata->m_string;
  }

  Object& GetObject() const
  {
    AssertType(tObject);
    return *m_pdata->m_object;
  }

  //
  // Set value
  //
  void SetValue(Value const& rhs)
  {
    if(m_pdata != rhs.m_pdata)
    {
      Clear(true);
      switch(rhs.m_pdata->m_type)
      {
      case tBool:   m_pdata->m_bool   = rhs.m_pdata->m_bool; break;
      case tInt:    m_pdata->m_int    = rhs.m_pdata->m_int; break;
      case tString: m_pdata->m_string = new String(*rhs.m_pdata->m_string); break;
      case tObject: m_pdata->m_object = rhs.m_pdata->m_object; break;
      }
      m_pdata->m_type = rhs.m_pdata->m_type;
    }
  }

  //
  // Assignment
  //
  Value const& operator = (Value const& rhs)
  {
    SetValue(rhs);
    return *this;
  }

  //
  // Set reference
  //
  void SetRef(Value const& rhs)
  {
    m_pdata = rhs.m_pdata;
  }

private:

  //
  // Not allowed
  //
  bool operator == (Value const& rhs);

  //
  // Type check
  //
  void AssertType(Types type) const
  {
    if(m_pdata->m_type != type)
    {
      throw std::runtime_error("Value is not of expected type");
    }
  }

  // Member data
  struct Data
  {
    Types     m_type;
    union 
    {
      Bool    m_bool;
      Int     m_int;
      String* m_string;
      Object* m_object;
      Value*  m_value;
    };
  };

  //
  // Members
  //
  Data  m_ldata;
  Data* m_pdata;

};

#endif // CSCRIPT_VALUE_H
