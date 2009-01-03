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
#include "class.h"
#include "ast.h"
#include "eval.h"
#include "function.h"
#include "instance.h"

void 
Class::AddConversion(ConversionOperator* node)
{
  if(m_conv.count(node->GetTypeInfo()))
  {
    throw std::runtime_error("Conversion already declared");
  }
  m_conv[node->GetTypeInfo()] = node;
}

bool 
Class::FindConversion(TypeInfo const& type, ConversionOperator*& node) const
{
  ConversionMap::const_iterator it = m_conv.find(type);
  if(it == m_conv.end())
  {
    return false;
  }
  node = it->second;
  return true;
}
