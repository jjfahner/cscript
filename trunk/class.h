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
    return m_vars;
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
  void AddVar();

  //
  // Create a new instance
  //
  Instance* CreateInstance();

  //
  // Lookup offset of function
  //
  Quad Lookup(String const& name);

private:

  //
  // Types
  //
  typedef std::map<String, MemFunInfo> MemberFuns;

  //
  // Members
  //
  String      m_name;
  MemberFuns  m_funs;
  Quad        m_vars;

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
  Quad Lookup(String const& name);

  //
  // Retrieve pointer to variable
  //
  VariantRef const& GetVar(Quad index);

private:

  //
  // Types
  //
  typedef std::vector<VariantRef> MemberVars;

  //
  // Member data
  //
  Class*      m_class;
  MemberVars  m_vars;

};

#endif // CSCRIPT_CLASS_H
