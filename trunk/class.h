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
#ifndef CSCRIPT_CLASS_H
#define CSCRIPT_CLASS_H

#include "var.h"

class Class;
class Instance;
class CodeGenerator;

struct MemFunInfo
{
  Quad  m_params;
  Quad  m_offset;
};

//////////////////////////////////////////////////////////////////////////
//
// Runtime class info
//

class Class 
{
public:

  //
  // Construction
  //
  Class(String const& name = String());

  //
  // Destruction
  //
  ~Class();

  //
  // Class name
  //
  String const& GetName() const
  {
    return m_name;
  }

  //
  // Varcount
  //
  Quad GetVars() const
  {
    return m_vars.size();
  }

  //
  // Read from disk
  //
  Quad Read(Byte const* code);

  //
  // Write to disk
  //
  void Write(CodeGenerator& cg);

  //
  // Add a member function
  //
  void AddFun(String const& name, Quad params, Quad offset);

  //
  // Add a variable
  //
  void AddVar(String const& name);

  //
  // Create a new instance
  //
  Instance* CreateInstance();

  //
  // Lookup offset of function
  //
  Quad LookupFun(String const& name);

  //
  // Lookup offset of variable
  //
  Quad LookupVar(String const& name);

private:

  //
  // Types
  //
  typedef std::map<String, MemFunInfo> MemberFuns;
  typedef std::map<String, Quad>       MemberVars;

  //
  // Members
  //
  String      m_name;
  MemberFuns  m_funs;
  MemberVars  m_vars;

};

//////////////////////////////////////////////////////////////////////////
//
// Runtime class instance
//

class Instance : public Variant::Resource
{
public:

  //
  // Construction
  //
  Instance(Class* c);

  //
  // Destruction
  //
  virtual ~Instance();

  //
  // Lookup offset of function
  //
  Quad LookupFun(String const& name);

  //
  // Lookup offset of variable
  //
  Quad LookupVar(String const& name);

  //
  // Retrieve pointer to variable by index
  //
  VariantRef const& GetVar(Quad index);

  //
  // Retrieve pointer to variable by name
  //
  VariantRef const& GetVar(String const& name);

private:

  //
  // Types
  //
  typedef std::vector<VariantRef>      IndexVars;
  typedef std::map<String, VariantRef> NamedVars;

  //
  // Member data
  //
  Class*    m_class;
  IndexVars m_indexVars;
  NamedVars m_namedVars;

};

#endif // CSCRIPT_CLASS_H
