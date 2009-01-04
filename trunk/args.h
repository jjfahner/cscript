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
#ifndef CSCRIPT_ARGS_H
#define CSCRIPT_ARGS_H

#include "types.h"
#include "value.h"
#include "class.h"

class Arguments : public std::vector<Value>
{
public:

  //
  // Construction
  //
  Arguments() :
  m_object     (0),
  m_parameters (0)
  {
  }

  //
  // Instance
  //
  Object* GetObject() const
  {
    return m_object;
  }
  void SetObject(Object* instance)
  {
    m_object = instance;
  }

  //
  // Parameters
  //
  AstList const* GetParameters() const
  {
    return m_parameters;
  }
  void SetParameters(AstList const* parameters)
  {
    m_parameters = parameters;
  }

  //
  // Retrieve argument by name
  //
  using std::vector<Value>::operator[];
  Value const& operator [] (String const& name) const
  {
    return at(name);
  }

  //
  // Overload at using name
  //
  using std::vector<Value>::at;
  Value const& at(String const& name) const;

protected:

  //
  // Members
  //
  Object*         m_object;
  AstList const*  m_parameters;

};

#endif // CSCRIPT_ARGS_H
