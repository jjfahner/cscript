#include "cmdargs.h"

CmdArgs::CmdArgs(int argc, Char** argv)
{
  Char opt[3] = { '-', 0, 0 };

  String* val = 0;
  for(int i = 0; i < argc; ++i)
  {
    Char* ptr = argv[i];
    if(*ptr == '-')
    {
      if(*++ptr == '-')
      {
        if(char* eq = strchr(ptr, '='))
        {
          m_args[String(ptr, eq - ptr)] = eq + 1;
        }
        else
        {
          val = &m_args[ptr];
        }
      }
      else
      {
        while(*ptr)
        {
          opt[1] = *ptr++;
          val = &m_args[opt];
        }
      }
    }
    else
    {
      if(val) 
      {
        *val = ptr;
         val = 0;
      }
      else
      {
        m_args[ptr];
      }
    }
  }
}

struct IsOpt {
  typedef std::pair<String const, String> argument_type;
  bool operator () (std::pair<String const, String> const& pair) const {
    return pair.first[0] == '-';
  }
};

template <typename Container>
struct key_insert_iterator : public std::back_insert_iterator<Container> {
  key_insert_iterator(Container& cont) : std::back_insert_iterator<Container>(cont) {
  }
  key_insert_iterator<Container>& operator=(std::pair<String const, String> const& val) {
      std::back_insert_iterator<Container>::operator = (val.first);
      return *this;
  }
  key_insert_iterator<Container>& operator*() {
    return *this;
  }
  key_insert_iterator operator ++ (int) {
    return *this;
  }
};

template <typename Container>
key_insert_iterator<Container> key_inserter(Container& container) {
  return key_insert_iterator<Container>(container);
}

StringList 
CmdArgs::GetValues() const
{
  StringList result;
  std::remove_copy_if(m_args.begin(), m_args.end(), key_inserter(result), IsOpt());
  return result;
}

StringList 
CmdArgs::GetOpts() const
{
  StringList result;
  std::remove_copy_if(m_args.begin(), m_args.end(), key_inserter(result), std::not1(IsOpt()));
  return result;
}
