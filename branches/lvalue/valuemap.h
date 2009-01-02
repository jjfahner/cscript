//////////////////////////////////////////////////////////////////////////
//
// This file is � 2007 JJ Fahner <jan-jaap@jan-jaap.net>
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
#ifndef CSCRIPT_VALUEMAP_H
#define CSCRIPT_VALUEMAP_H

#include <map>

class Value;
class Evaluator;

//
// Less-than comparator
//
class ValueComparatorLess
{
public:

  //
  // Construction
  //
  ValueComparatorLess(Evaluator* evaluator);

  //
  // Less-than operator
  //
  bool operator () (Value const& lhs, Value const& rhs) const;

private:

  Evaluator* m_evaluator;

};

//
// Value map
//
typedef std::map<Value, Value, ValueComparatorLess> ValueMap;

#endif // CSCRIPT_VALUEMAP_H
