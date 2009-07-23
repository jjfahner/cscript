//////////////////////////////////////////////////////////////////////////
//
// This file is © 2007 - 2009 JJ Fahner <jan-jaap@jan-jaap.net>
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
#include <native/comobject.h>
#include <native/comobject_i.h>

#include <map>

DEFINE_NATIVE_LINKAGE(Com)

//////////////////////////////////////////////////////////////////////////

struct CoInit
{
  CoInit() {
    CoInitialize(0);
  }
  ~CoInit() {
    //CoUninitialize();
  }
};

//////////////////////////////////////////////////////////////////////////

NATIVE_CALL("cocreate(string classname)")
{
  // Initialize COM
  static CoInit coInit;

  // Create instance
  return ComObject::Create(evaluator, args[0].GetString());
}

//////////////////////////////////////////////////////////////////////////

NATIVE_CALL("cosleep(int timeout)")
{
  MSG msg;
  
  DWORD timeout = (DWORD)args[0].GetInt();  
  DWORD ticks = GetTickCount();

  while(true)
  {
    int remain = timeout - (GetTickCount() - ticks);
    if(remain < 0)
    {
      break;
    }

    if(MsgWaitForMultipleObjects(0, 0, false, remain, QS_ALLINPUT) == WAIT_TIMEOUT)
    {
      break;
    }
    
    while(PeekMessage(&msg, 0, 0, 0, PM_REMOVE))
    {
      TranslateMessage(&msg);
      DispatchMessage(&msg);
    }
  }

  return Value();
}

//////////////////////////////////////////////////////////////////////////

void 
ValueToVariant(Value const& value, VARIANT& variant)
{
  USES_CONVERSION;

  VariantClear(&variant);
  switch(value.Type())
  {
  case Value::tNull:
    variant.vt    = VT_ERROR;
    variant.scode = DISP_E_PARAMNOTFOUND;
    break;
  case Value::tBool:
    variant.vt      = VT_BOOL;
    variant.boolVal = value.GetBool() ? VARIANT_TRUE : VARIANT_FALSE;
    break;
  case Value::tInt:
    variant.vt      = VT_INT;
    variant.intVal  = (int)value.GetInt();
    break;
  case Value::tString:
    variant.vt      = VT_BSTR;
    variant.bstrVal = SysAllocString(T2W(value.GetString().c_str()));
    break;
  default:
    throw std::runtime_error("Invalid argument type for COM method invocation");
  }
}

//////////////////////////////////////////////////////////////////////////

void
VariantToValue(Evaluator* evaluator, VARIANT const& variant, Value& value)
{
  USES_CONVERSION;

  VARTYPE vtSrc = variant.vt;
  VARTYPE vtDst = VT_EMPTY;

  // Determine target type
  switch(vtSrc & ~VT_BYREF)
  {
  case VT_EMPTY:    vtDst = VT_EMPTY;     break;
  case VT_NULL:     vtDst = VT_EMPTY;     break;
  case VT_I2:       vtDst = VT_I8;        break;
  case VT_I4:       vtDst = VT_I8;        break;
  case VT_R4:       vtDst = VT_R8;        break;
  case VT_R8:       vtDst = VT_R8;        break;
  case VT_DATE:     vtDst = VT_BSTR;      break;
  case VT_BSTR:     vtDst = VT_BSTR;      break;
  case VT_DISPATCH: vtDst = VT_DISPATCH;  break;
  case VT_ERROR:    vtDst = VT_BSTR;      break;
  case VT_BOOL:     vtDst = VT_BOOL;      break;
  case VT_VARIANT:  vtDst = VT_BSTR;      break;
  case VT_UNKNOWN:  vtDst = VT_DISPATCH;  break;
  case VT_DECIMAL:  vtDst = VT_R8;        break;
  case VT_I1:       vtDst = VT_I8;        break;
  case VT_UI1:      vtDst = VT_I8;        break;
  case VT_UI2:      vtDst = VT_I8;        break;
  case VT_UI4:      vtDst = VT_I8;        break;
  case VT_I8:       vtDst = VT_I8;        break;
  case VT_UI8:      vtDst = VT_I8;        break;
  case VT_INT:      vtDst = VT_I8;        break;
  case VT_UINT:     vtDst = VT_I8;        break;
  case VT_HRESULT:  vtDst = VT_I8;        break;
  case VT_LPSTR:    vtDst = VT_BSTR;      break;
  case VT_LPWSTR:   vtDst = VT_BSTR;      break;
  case VT_FILETIME: vtDst = VT_BSTR;      break;
  case VT_CLSID:    vtDst = VT_BSTR;      break;
  default: throw std::runtime_error("Invalid variant type");
  }

  // Convert to target type
  if(vtSrc != vtDst)
  {
    // Initialize variant
    VARIANT vt;
    VariantInit(&vt);

    // Change to destination type
    if(SUCCEEDED(VariantChangeType(&vt, &variant, 0, vtDst)))
    {
      return VariantToValue(evaluator, vt, value);
    }

    // Failed conversion
    throw std::runtime_error("Failed to convert OLE VARIANT to native type");
  }

  // Create value
  switch(vtSrc)
  {
  case VT_EMPTY:    
    value.Clear(); 
    break;

  case VT_BOOL:     
    value = variant.boolVal != 0; 
    break;
  
  case VT_I8:       
    value = variant.llVal; 
    break;
  
  case VT_BSTR:     
    if(variant.bstrVal)
    {
      value = W2T(variant.bstrVal);
    }
    else
    {
      value = "";
    }
    break;

  case VT_DISPATCH: 
    if(variant.pdispVal)
    {
      value = ComObject::Create(evaluator, variant.pdispVal);
    }
    break;
        
  default: 
    throw std::runtime_error("Failed to convert OLE VARIANT to native type");
  }
}

//////////////////////////////////////////////////////////////////////////

/*static*/ Object* 
ComObject::Create(Evaluator* evaluator, String progID)
{
  USES_CONVERSION;

  // Find classid from program id
  CLSID clsid;
  if(FAILED(CLSIDFromProgID(T2W(progID.c_str()), &clsid)))
  {
    throw std::runtime_error("Invalid COM classname");
  }

  IDispatchPtr pDispatch;

  // First attempt: use CLSCTX_ALL
  if(SUCCEEDED(CoCreateInstance(clsid, NULL, CLSCTX_ALL, 
    IID_IDispatch, (void**)&pDispatch)))
  {
    return ComObject::Create(evaluator, pDispatch);
  }

  // Second attempt: use CLSCTX_LOCAL_SERVER
  if(SUCCEEDED(CoCreateInstance(clsid, NULL, CLSCTX_LOCAL_SERVER, 
    IID_IDispatch,(void**)&pDispatch)))
  {
    return ComObject::Create(evaluator, pDispatch);
  }

  // Failed
  throw std::runtime_error("Failed to instantiate COM object");
}

/*static*/ Object* 
ComObject::Create(Evaluator* evaluator, IDispatch* pdisp)
{
  return new ComObject(evaluator, pdisp);
}

ComObject::ComObject(Evaluator* evaluator, IDispatch* pdisp) : \
m_evaluator (evaluator),
m_dispatch  (pdisp),
m_typeInfo  (0)
{
  // Take ref on object
  m_dispatch->AddRef();

  // Load the type library
  m_typeInfo = new ComTypeInfo(m_dispatch);
}

ComObject::~ComObject()
{
  // Remove pointer from instance
  IDispatch* pDispatch = m_dispatch;
  m_dispatch = 0;

  // Release the object
  if(pDispatch)
  {
    pDispatch->Release();
  }

  // Free the type info
  delete m_typeInfo;
}

bool 
ComObject::ContainsKey(Value const& key, bool checkProto) const
{
  // Lazy as I am: use the implementation of Find for ContainsKey
  RValue* pDummy;
  return Find(key, pDummy, checkProto);
}

bool 
ComObject::Find(Value const& name, RValue*& ptr, bool checkProto) const
{
  // Find on primary interface
  if(FindOnInterface(*m_typeInfo, name, ptr, false))
  {
    return true;
  }

  // Retrieve connection point container
  IConnectionPointContainerPtr pConnPtC(m_dispatch);
  if(pConnPtC == 0)
  {
    return false;
  }

  // Retrieve connection point enumerator
  IEnumConnectionPointsPtr pEnum;
  if(FAILED(pConnPtC->EnumConnectionPoints(&pEnum)))
  {
    return false;
  }

  // Enumerate connection points
  for(ULONG num = 0;;)
  {
    // Retrieve connection point
    IConnectionPointPtr pConnPt;
    if(FAILED(pEnum->Next(1, &pConnPt, &num)) || num != 1)
    {
      break;
    }

    // Retrieve interface iid
    IID iid;
    if(FAILED(pConnPt->GetConnectionInterface(&iid)))
    {
      continue;
    }

    // Retrieve type info
    ITypeInfoPtr pTypeInfo;
    if(!m_typeInfo->GetTypeInfoForIID(iid, &pTypeInfo))
    {
      continue;
    }

    // Find event on this interface
    ComTypeInfo cti(pTypeInfo);
    if(FindOnInterface(cti, name, ptr, pConnPt))
    {
      return true;
    }
  }

  // Done
  return 0;
}

bool 
ComObject::FindOnInterface(ComTypeInfo& cti, Value const& name, RValue*& ptr, IConnectionPointPtr const& pConnPt) const
{
  // Find method info
  DISPID dispid;
  if(!cti.GetDispIdOfName(name.GetString(), dispid))
  {
    return false;
  }

  // Retrieve proper name
  String properName;
  if(!cti.GetNameOfDispId(dispid, properName))
  {
    return false;
  }

  // Find in current member list
  ComMemberMap::const_iterator it = m_members.find(properName);
  if(it != m_members.end())
  {
    ptr = it->second;
    return true;
  }

  // Retrieve function info
  FUNCDESC* pfd;
  if(!cti.GetFuncDescOfDispId(dispid, &pfd))
  {
    return false;
  }

  // Create right sort of object
  if(pfd->invkind == INVOKE_FUNC)
  {
    if(pConnPt)
    {
      ptr = new ComEventHandler(properName, dispid, this, pConnPt);
    }
    else
    {
      ptr = new ROVariable(new ComMemberFunction(properName, dispid, this));
    }
  }
  else
  {
    if(pConnPt)
    {
      throw std::runtime_error("Unexpected: property on source interface");
    }

    if(!(pfd->invkind & INVOKE_PROPERTYGET))
    {
      throw std::runtime_error("Write-only COM properties not supported");
    }
    
    if((pfd->invkind & INVOKE_PROPERTYPUT)    ||
      (pfd->invkind & INVOKE_PROPERTYPUTREF) )
    {
      ptr = new RWComVariable(properName, dispid, this);
    }
    else
    {
      ptr = new ROComVariable(properName, dispid, this);
    }
  }

  // Add to member variables
  m_members[properName] = ptr;

  // Done
  return true;
}

void
ComObject::Invoke(DISPID dispid, INVOKEKIND invokeKind, Arguments& args, VARIANT& vResult) const
{
  USES_CONVERSION;

  // Check dispatch pointer
  if(!m_dispatch)
  {
    //throw std::runtime_error("Invoke on released COM object");
    VariantInit(&vResult);
    return;
  }

  // Retrieve function info for the dispid
  FUNCDESC* pfd;
  if(FAILED(m_typeInfo->GetFuncDescOfDispId(dispid, &pfd)))
  {
    throw std::runtime_error("Object doesn't support this property or method");
  }

  // Check parameter count
  USHORT minPars = pfd->cParams - pfd->cParamsOpt;
  USHORT maxPars = pfd->cParams;
//   if(/*args.size() < minPars ||*/ args.size() > maxPars)
//   {
//     throw std::runtime_error("Invalid number of arguments supplied to COM method");
//   }

  // Create arguments list
  DISPPARAMS dispparams;
  memset(&dispparams, 0, sizeof dispparams);
  if(args.size())
  {
    // Create variant entries
    dispparams.cArgs = args.size();
    dispparams.rgvarg = new VARIANT[dispparams.cArgs];

    // Reverse copy arguments into enties
    for(int src = args.size() - 1, dst = 0; src >= 0; --src, ++dst)
    {
      VariantInit(&dispparams.rgvarg[dst]);
      ValueToVariant(args[src], dispparams.rgvarg[dst]);
    }
  }

  // Property put invocations require an additional flag
  DISPID disp = DISPID_PROPERTYPUT;
  if(invokeKind == DISPATCH_PROPERTYPUT)
  {
    dispparams.cNamedArgs = 1;
    dispparams.rgdispidNamedArgs = &disp;
  }

  // Initialize error info
  EXCEPINFO excepInfo;
  memset(&excepInfo, 0, sizeof excepInfo);

  // Initialize to invalid value
  UINT nArgErr = (UINT)-1;

  // Invoke the method
  HRESULT hr;
  hr = m_dispatch->Invoke(pfd->memid, IID_NULL, LOCALE_USER_DEFAULT, 
           invokeKind, &dispparams, &vResult, &excepInfo, &nArgErr);

  // Cleanup argument list
  for(unsigned int i = 0; i < dispparams.cArgs; ++i)
  {
    VariantClear(&dispparams.rgvarg[i]);
  }
  delete [] dispparams.rgvarg;

  // Check result
  if(FAILED(hr))
  {
    String name;
    m_typeInfo->GetNameOfDispId(pfd->memid, name);
    String err = W2T(excepInfo.bstrDescription);
    err = "COM method '" + name + "' failed: " + err;
    throw std::runtime_error(err);
  }
}

Value
ComObject::Invoke(DISPID dispid, INVOKEKIND invokeKind, Arguments& args) const
{
  // Initialize result
  VARIANT vResult;
  VariantInit(&vResult);

  // Invoke the method
  Invoke(dispid, invokeKind, args, vResult);
  
  // Convert result value
  Value result;
  VariantToValue(m_evaluator, vResult, result);
  VariantClear(&vResult);

  // Done
  return result;
}

Enumerator* 
ComObject::GetEnumerator(Value const& value) const
{
  // Check that it's a dispatch pointer
  if(value.Type() != Value::tObject)
  {
    throw std::runtime_error("Member cannot be enumerated");
  }

  // Convert the object to a ComObject
  ComObject* ci = dynamic_cast<ComObject*>(value.GetObject());
  if(ci == 0)
  {
    throw std::runtime_error("Member cannot be enumerated");
  }

  // Retrieve the DISPID_NEWENUM object
  IUnknownPtr pUnk;
  try
  {
    // Prepare result variant
    VARIANT vResult;
    VariantInit(&vResult);

    // Invoke NEWENUM
    ci->Invoke(DISPID_NEWENUM, INVOKE_PROPERTYGET, Arguments(), vResult);

    // Copy pEnum
    pUnk = vResult.punkVal;

    // Cleanup
    VariantClear(&vResult);
  }
  catch(std::runtime_error const&)
  {
    throw std::runtime_error("Member cannot be enumerated");
  }

  // Accept empty enumerator, but when filled, check conversion
  IEnumVARIANTPtr pEnum(pUnk);
  if(pUnk && !pEnum)
  {
    throw std::runtime_error("Member cannot be enumerated");
  }

  // Construct enumerator
  return new ComEnumerator(m_evaluator, pEnum);
}

void 
ComObject::MarkObjects(GC::ObjectVec& grey)
{
  // Mark object members
  Object::MarkObjects(grey);

  // Iterate over members
  ComMemberMap::iterator mi, me;
  mi = m_members.begin();
  me = m_members.end();
  for(; mi != me; ++mi)
  {
    // Check key content
    if(GC::Object* o = mi->first.GetGCObject()) {
      grey.push_back(o);
    }

    // Check value content
    if(GC::Object* o = mi->second->GetGCObject()) {
      grey.push_back(o);
    }
  }
}

//////////////////////////////////////////////////////////////////////////

ComEnumerator::ComEnumerator(Evaluator* evaluator, IEnumVARIANTPtr const& pEnum) :
m_evaluator (evaluator),
m_pEnum (pEnum)
{
}

void ComEnumerator::Reset()
{
  if(m_pEnum)
  {
    m_pEnum->Reset();
  }
}

bool 
ComEnumerator::GetNext(Value& value)
{
  // Check enumerator
  if(!m_pEnum)
  {
    return false;
  }

  // Prepare variant
  VARIANT vElem;
  VariantInit(&vElem);

  // Move to next item
  ULONG fetched = 0;
  if(FAILED(m_pEnum->Next(1, &vElem, &fetched)) || fetched != 1)
  {
    return false;
  }

  // Convert value
  VariantToValue(m_evaluator, vElem, value);
  VariantClear(&vElem);

  // Done
  return true;
}

//////////////////////////////////////////////////////////////////////////

ROComVariable::ROComVariable(String name, DISPID dispid, ComObject const* inst) :
m_name    (name),
m_dispid  (dispid),
m_inst    (inst)
{
}

Value const& 
ROComVariable::GetValue() const
{
  // Setup empty argument list
  Arguments args;

  // Invoke property getter
  m_value = m_inst->Invoke(m_dispid, INVOKE_PROPERTYGET, args);

  // Return stored value
  return m_value;
}

Enumerator* 
ROComVariable::GetEnumerator() const
{
  return m_inst->GetEnumerator(GetValue());
}

//////////////////////////////////////////////////////////////////////////

RWComVariable::RWComVariable(String name, DISPID dispid, ComObject const* inst) :
m_name    (name),
m_dispid  (dispid),
m_inst    (inst)
{
}

Value const& 
RWComVariable::GetValue() const
{
  // Setup empty argument list
  Arguments args;

  // Invoke property getter
  m_value = m_inst->Invoke(m_dispid, INVOKE_PROPERTYGET, args);

  // Return stored value
  return m_value;
}

void 
RWComVariable::SetValue(Value const& rhs)
{
  // Setup argument list
  Arguments args;
  args.push_back(rhs);

  // Invoke property setter
  m_inst->Invoke(m_dispid, INVOKE_PROPERTYPUT, args);
}

Enumerator* 
RWComVariable::GetEnumerator() const
{
  return m_inst->GetEnumerator(GetValue());
}

//////////////////////////////////////////////////////////////////////////

ComMemberFunction::ComMemberFunction(String name, DISPID dispid, ComObject const* instance) :
Function  (name),
m_inst    (instance),
m_dispid  (dispid)
{
}

List* 
ComMemberFunction::GetParameters() const
{
  return 0;
}

Value 
ComMemberFunction::Execute(Evaluator*, Arguments& args)
{
  return m_inst->Invoke(m_dispid, INVOKE_FUNC, args);
}

//////////////////////////////////////////////////////////////////////////

ComEventHandler::ComEventHandler(String name, DISPID dispid, ComObject const* inst, IConnectionPointPtr const& pConnPt) :
m_name    (name),
m_dispid  (dispid),
m_inst    (inst),
m_refs    (1)
{
  // Retrieve iid for event interface
  pConnPt->GetConnectionInterface(&m_iid);

  // Retrieve type info
  ITypeInfoPtr pTypeInfo;
  m_inst->m_typeInfo->GetTypeInfoForIID(m_iid, &pTypeInfo);
  m_typeInfo = new ComTypeInfo(pTypeInfo);

  // Connect to interface
  pConnPt->Advise(this, &m_cookie);
}

void 
ComEventHandler::Delete()
{
  Release();
}

Value const& 
ComEventHandler::GetValue() const
{
  return m_handler;
}

void 
ComEventHandler::SetValue(Value const& rhs)
{
  // TODO check type of rhs, must be function
  m_handler = rhs;
}

HRESULT WINAPI
ComEventHandler::QueryInterface(REFIID riid, void **ppvObject)
{
  *ppvObject = 0;
  if(IsEqualGUID(riid, IID_IUnknown)  || 
     IsEqualGUID(riid, IID_IDispatch) ||
     IsEqualGUID(riid, m_iid)         )
  {
    *ppvObject = this;
  }
  else
  {
    return E_NOINTERFACE;
  }

  static_cast<IUnknown*>(*ppvObject)->AddRef();
  
  return S_OK;
}

ULONG WINAPI
ComEventHandler::AddRef()
{
  return InterlockedIncrement(&m_refs);
}

ULONG WINAPI 
ComEventHandler::Release()
{
  if(InterlockedDecrement(&m_refs) == 0)
  {
    delete this;
  }
  return 0;
}

HRESULT WINAPI
ComEventHandler::GetTypeInfoCount(UINT *pctinfo)
{
  *pctinfo = 0;
  return E_NOTIMPL;
}

HRESULT WINAPI
ComEventHandler::GetTypeInfo(UINT iTInfo, 
                             LCID lcid, 
                             ITypeInfo **ppTInfo)
{
  *ppTInfo = 0;
  return E_NOTIMPL;
}

HRESULT WINAPI
ComEventHandler::GetIDsOfNames(REFIID riid, 
                               LPOLESTR *rgszNames, 
                               UINT cNames, 
                               LCID lcid, 
                               DISPID *rgDispId)
{
  *rgDispId = 0;
  return E_NOTIMPL;
}

HRESULT WINAPI
ComEventHandler::Invoke(DISPID dispIdMember, 
                        REFIID riid, 
                        LCID lcid, 
                        WORD wFlags, 
                        DISPPARAMS *pDispParams, 
                        VARIANT *pVarResult, 
                        EXCEPINFO *pExcepInfo, 
                        UINT *puArgErr)
{
  // Retrieve name of function
  String name;
  m_typeInfo->GetNameOfDispId(dispIdMember, name);  

  // Compare with subscribed name
  if(name != m_name)
  {
    return E_NOTIMPL;
  }

  // Retrieve the function information
  FUNCDESC* pfd;
  if(!m_typeInfo->GetFuncDescOfDispId(dispIdMember, &pfd))
  {
    return E_UNEXPECTED;
  }

  // Extract handler
  Function* handler = dynamic_cast<Function*>(m_handler.GetObject());
  if(handler == 0)
  {
    return E_UNEXPECTED;
  }
  
  // Check argument counts
  //if()

  // Build argument list
  Arguments args;
  for(SHORT i = pfd->cParams - 1; i >= 0; --i)
  {
    VARIANT& src = pDispParams->rgvarg[i];
    Value    dst;

    VariantToValue(m_inst->m_evaluator, src, dst);
    
    args.push_back(dst);
  }

  // Build call stack
  handler->Execute(m_inst->m_evaluator, args);

  // Done
  return S_OK;
}
