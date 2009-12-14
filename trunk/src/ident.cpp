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
#include "ident.h"

#include <map>

//
// Map from name to id
//
typedef std::map<String, IdentId> IdentIdMap;
static IdentIdMap g_idMap;

//
// Map from id to name
//
typedef std::map<IdentId, String> IdentNameMap;
static IdentNameMap g_nameMap;

//
// Id sequence
//
static const String&  g_invalidName = "<<invalid>>";
static const IdentId  g_invalidId = 0;
static IdentId        g_nextId = g_invalidId + 1;

/*static*/ String const& Identifier::Lookup(IdentId id)
{
  IdentNameMap::const_iterator it = g_nameMap.find(id);
  
  if (it == g_nameMap.end())
  {
    return g_invalidName;
  }

  return it->second;
}

/*static*/ IdentId Identifier::Lookup(String const& name)
{
  IdentId& id = g_idMap[name];

  if (id == 0)
  {
    id = g_nextId++;
    g_nameMap[id] = name;
  }

  return id;
}
