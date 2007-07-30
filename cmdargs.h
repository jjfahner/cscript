#ifndef CSCRIPT_CMDARGS_H
#define CSCRIPT_CMDARGS_H

#include "types.h"

class CmdArgs
{
public:

  //
  // Construction
  //
  CmdArgs(int argc, Char** argv);

  //
  // Is option set
  //
  bool IsSet(String const& name) const;

  //
  // Option value
  //
  String const& GetValue(String const& name) const;

  //
  // Shortcurt for GetValue
  //
  String const& operator [] (String const& name) const;

  //
  // List of regular args
  //
  StringList GetValues() const;

  //
  // List of option names
  //
  StringList GetOpts() const;
  
private:

  //
  // Members
  //
  StringMap m_args;

};

inline bool 
CmdArgs::IsSet(String const& name) const
{
  return m_args.count(name) > 0;
}

inline String const&
CmdArgs::GetValue(String const& name) const
{
  static const String dummy;
  StringMap::const_iterator it;
  if((it = m_args.find(name)) == m_args.end())
  {
    return dummy;
  }
  return it->second;
}

inline String const& 
CmdArgs::operator [] (String const& name) const
{
  return GetValue(name);
}

#endif // #ifndef CSCRIPT_CMDARGS_H
