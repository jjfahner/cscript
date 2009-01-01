//////////////////////////////////////////////////////////////////////////
//
// This file is © 2008 JJ Fahner <jan-jaap@jan-jaap.net>
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
#include "comtype.h"
#include <atlconv.h>

ComTypeInfo::ComTypeInfo(IDispatch* pDispatch) :
  m_initDone (false)
{
  USES_CONVERSION;

  // Retrieve type info
  if(FAILED(pDispatch->GetTypeInfo(0, 0, &m_typeinfo)))
  {
    throw std::runtime_error("Cannot retrieve type information");
  }

  // Retrieve type name
  BSTR bstrName;
  if(SUCCEEDED(m_typeinfo->GetDocumentation(MEMBERID_NIL, &bstrName, 0, 0, 0)))
  {
    m_typename = W2T(bstrName);
    SysFreeString(bstrName);    
  }
}

ComTypeInfo::ComTypeInfo(ITypeInfo* pTypeInfo) :
  m_initDone (false)
{
  USES_CONVERSION;

  // Store the interface pointer
  m_typeinfo = pTypeInfo;

  // Retrieve type name
  BSTR bstrName;
  if(SUCCEEDED(m_typeinfo->GetDocumentation(MEMBERID_NIL, &bstrName, 0, 0, 0)))
  {
    m_typename = W2T(bstrName);
    SysFreeString(bstrName);    
  }
}

ITypeInfoPtr const&
ComTypeInfo::GetTypeInfo() const
{
  return m_typeinfo;
}

String
ComTypeInfo::GetTypeName() const
{
  return m_typename;
}

bool 
ComTypeInfo::GetParameterNames(DISPID dispid, std::vector<String>& names)
{
  USES_CONVERSION;

  // Retrieve function info
  FUNCDESC* pFuncDesc = 0;
  if(!GetFuncDescOfDispId(dispid, &pFuncDesc))
  {
    return false;
  }

  // Retrieve names
  UINT count;
  UINT nparms = pFuncDesc->cParams + 1;
  BSTR* bstrNames = new BSTR[nparms];
  if(FAILED(m_typeinfo->GetNames(dispid, bstrNames, nparms, &count)))
  {
    delete [] bstrNames;
    return false;
  }

  // Add names to array
  for(SHORT i = 1; i < pFuncDesc->cParams + 1; ++i)
  {
    names.push_back(W2T(bstrNames[i]));
    SysFreeString(bstrNames[i]);
  }

  // Free names array
  delete [] bstrNames;

  // Success
  return true;
}

bool
ComTypeInfo::GetTypeInfoForIID(REFIID iid, ITypeInfo** pTypeInfo)
{
  UINT idx;
  ITypeLibPtr pTl;

  // Retrieve typelib for typeinfo
  if(FAILED(m_typeinfo->GetContainingTypeLib(&pTl, &idx)))
  {
    return false;
  }

  // Bepaal typeinfo van event interface
  if(FAILED(pTl->GetTypeInfoOfGuid(iid, pTypeInfo)))
  {
    return false;
  }

  // Success
  return true;
}

bool
ComTypeInfo::GetDispIdOfName(String name, DISPID& dispid) const
{
  // Init result
  dispid = 0;

  // Make sure cache is read
  InitCache();

  // Find in cache
  dispid_map::iterator iter = m_dispids.find(name);
  if(iter == m_dispids.end())
  {
    return false;
  }

  // Success
  dispid = iter->second;
  return true;
}

bool
ComTypeInfo::GetNameOfDispId(DISPID dispid, String& name) const
{
  // Init result
  name.clear();

  // Make sure cache is read
  InitCache();

  // Find in cache
  dispmm_map::iterator iter = m_dispnms.find(dispid);
  if(iter == m_dispnms.end())
  {
    return false;
  }

  // Success
  name = iter->second;
  return true;
}

bool 
ComTypeInfo::GetFuncDescOfDispId(DISPID dispid, FUNCDESC** ppFuncDesc)
{
  // Init result
  *ppFuncDesc = 0;

  // Make sure cache is read
  InitCache();

  // Find in cache
  dispif_map::iterator iter = m_dispinf.find(dispid);
  if(iter == m_dispinf.end())
  {
    return false;
  }

  // Success
  *ppFuncDesc = &iter->second;
  return true;
}

void 
ComTypeInfo::InitCache() const
{
  USES_CONVERSION;

  // Check init flag
  if(m_initDone)
  {
    return;
  }
  m_initDone = true;

  // Retrieve type info for type
  TYPEATTR typeAttr;
  TYPEATTR* pTypeAttr;
  if(SUCCEEDED(m_typeinfo->GetTypeAttr(&pTypeAttr)))
  {
    typeAttr = *pTypeAttr;
    m_typeinfo->ReleaseTypeAttr(pTypeAttr);
  }
  else
  {
    return;
  }

  // Determine interface to enumerate
  ITypeInfoPtr pTypeInfo;
  if(false && typeAttr.typekind == TKIND_DISPATCH && typeAttr.wTypeFlags & TYPEFLAG_FDUAL)
  {
    // Retrieve ref type
    HREFTYPE href;
    if(FAILED(m_typeinfo->GetRefTypeOfImplType(unsigned(-1), &href)) || 
       FAILED(m_typeinfo->GetRefTypeInfo(href, &pTypeInfo))          )
    {
      return;
    }

    // Retrieve the type attributes for this interface
    if(SUCCEEDED(pTypeInfo->GetTypeAttr(&pTypeAttr)))
    {
      typeAttr = *pTypeAttr;
      pTypeInfo->ReleaseTypeAttr(pTypeAttr);
    }
    else
    {
      return;
    }
  }
  else
  {
    pTypeInfo = m_typeinfo;
  }
  
  // Enumerate and cache members
  for(UINT idx = 0; ; ++idx)
  {
    // Retrieve function description
    FUNCDESC* pfd;
    HRESULT hr = pTypeInfo->GetFuncDesc(idx, &pfd);
    if(FAILED(hr))
    {
      break;
    }

    // Retrieve function name
    UINT count = 0;
    BSTR bstrName;
    if(FAILED(pTypeInfo->GetNames(pfd->memid, &bstrName, 1, &count)))
    {
      return;
    }

    // Convert name
    String name(W2T(bstrName));
    SysFreeString(bstrName);

    // Cache function info and DISPID
    m_dispinf[pfd->memid] = *pfd;
    m_dispnms[pfd->memid] = name;

    // Convert name to lower and cache it
    m_dispids[name] = pfd->memid;

    // Release the function description
    pTypeInfo->ReleaseFuncDesc(pfd);
  }
}

#define IMPLTYPE_MASK \
  (IMPLTYPEFLAG_FDEFAULT | IMPLTYPEFLAG_FSOURCE | IMPLTYPEFLAG_FRESTRICTED)

#define IMPLTYPE_DEFAULTSOURCE \
  (IMPLTYPEFLAG_FDEFAULT | IMPLTYPEFLAG_FSOURCE)

/*static*/ bool 
ComTypeInfo::FindDefaultEventSource(IDispatch* pDisp, IID* piid)
{
  *piid = GUID_NULL;

  // Retrieve IProvideClassInfo
  IProvideClassInfoPtr pPCI;
  HRESULT hr = pDisp->QueryInterface(IID_IProvideClassInfo, (void**)&pPCI);
  if(FAILED(hr))
  {
    return false;
  }

  // Find class info
  ITypeInfoPtr pClassInfo;
  if(FAILED(pPCI->GetClassInfo(&pClassInfo)))
  {
    return false;
  }

  // Get type attributes
  LPTYPEATTR pClassAttr;
  if(FAILED(pClassInfo->GetTypeAttr(&pClassAttr)))
  {
    return false;
  }

  // We'll need a coclass
  if(pClassAttr->typekind != TKIND_COCLASS)
  {
    pClassInfo->ReleaseTypeAttr(pClassAttr);
    return false;
  }

  // Enumerate interfaces to find default source
  for(unsigned int i = 0; i < pClassAttr->cImplTypes; i++)
  {
    int nFlags;
    HREFTYPE hRefType;

    // Check for default source flag
    if (SUCCEEDED(pClassInfo->GetImplTypeFlags(i, &nFlags)) && 
        ((nFlags & IMPLTYPE_MASK) == IMPLTYPE_DEFAULTSOURCE))
    {
      // Retrieve the typeinfo for the interface
      ITypeInfoPtr pEventInfo;
      if(SUCCEEDED(pClassInfo->GetRefTypeOfImplType(i, &hRefType)) &&	
         SUCCEEDED(pClassInfo->GetRefTypeInfo(hRefType, &pEventInfo)))
      {
        // Retrieve the type attributes
        LPTYPEATTR pTypeAttr;
        if(SUCCEEDED(pEventInfo->GetTypeAttr(&pTypeAttr)))
        {
          // Make sure this is an IDispatch-based interface
          // We crash on IUnknown-based interfaces, since they
          // expect a vtable based on the actual event interface.
          if(pTypeAttr->typekind == TKIND_DISPATCH)
          {
            *piid = pTypeAttr->guid;
            pEventInfo->ReleaseTypeAttr(pTypeAttr);
            break;
          }
          else
          {
            pEventInfo->ReleaseTypeAttr(pTypeAttr);
          }
        }
      }
    }
  }

  // Release the typeinfo
  pClassInfo->ReleaseTypeAttr(pClassAttr);

  // Failed, no default event interface
  return !InlineIsEqualGUID(*piid, GUID_NULL);
}
