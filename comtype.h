//////////////////////////////////////////////////////////////////////////
//
// This file is © 2008 - 2009 JJ Fahner <jan-jaap@jan-jaap.net>
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
#ifndef CSCRIPT_COMTYPE_H
#define CSCRIPT_COMTYPE_H

#include <oaidl.h>
#include <comdef.h>

#include <map>
#include <vector>

#include "cscript.h"
#include "types.h"

class ComTypeInfo
{
public:

  //
  // Find the default event source for an interface
  //
  static bool FindDefaultEventSource(IDispatch* pDispatch, IID* piid);

  //
  // Construction from dispatch
  //
  ComTypeInfo(IDispatch* pDispatch);

  //
  // Construction from ITypeInfo
  //
  ComTypeInfo(ITypeInfo* pTypeInfo);

  //
  // Retrieve type info
  //
  ITypeInfoPtr const& GetTypeInfo() const;

  //
  // Retrieve type name
  //
  String GetTypeName() const;

  //
  // Get CLSID for interface
  //
  CLSID GetCLSID() const;

  //
  // Get ProgID for interface
  //
  String GetProgID() const;

  //
  // Retrieve names of DISPID
  //
  bool GetParameterNames(DISPID dispid, std::vector<String>& names);

  //
  // Retrieve related type info
  //
  bool GetTypeInfoForIID(REFIID iid, ITypeInfo** pTypeInfo);

  //
  // Retrieve DISPID of member
  //
  bool GetDispIdOfName(String name, DISPID& dispid) const;
  bool GetNameOfDispId(DISPID disp, String& name) const;
  bool GetFuncDescOfDispId(DISPID disp, FUNCDESC** ppFuncDesc);

private:

  //
  // Read members into cache
  // 
  void InitCache() const;

  // Cache types
  typedef std::map<String, long, NCC> dispid_map;
  typedef std::map<long,   String>    dispmm_map;
  typedef std::map<long, FUNCDESC>    dispif_map;

  //
  // MemberMap
  //
  ITypeInfoPtr        m_typeinfo;
  CLSID               m_clsid;
  String              m_progID;
  String              m_typename;
  mutable bool        m_initDone;
  mutable dispid_map  m_dispids;      // Cached dispids
  mutable dispmm_map  m_dispnms;      // Cached dispnames
  mutable dispif_map  m_dispinf;      // Cached funcdesc
};

#endif // CSCRIPT_COMTYPE_H
