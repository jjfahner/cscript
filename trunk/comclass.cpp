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
#include "comclass.h"
#include "native.h"
#include "args.h"
#include "comtype.h"

#include <map>
#include <atlconv.h>

static std::map<String, ComClass*> g_comClasses;

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

NATIVE_CALL("__native cocreate(string classname)")
{
  // Initialize COM
  static CoInit coInit;

  // Find class
  ComClass* cc = ComClass::FromProgID(args[0].GetString());

  // Create instance
  return ComInstance::Create(cc);
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
VariantToValue(VARIANT const& variant, Value& value)
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
      return VariantToValue(vt, value);
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
      value = ComInstance::Create(variant.pdispVal);
    }
    break;
        
  default: 
    throw std::runtime_error("Failed to convert OLE VARIANT to native type");
  }
}

//////////////////////////////////////////////////////////////////////////

/*static*/ ComClass* 
ComClass::FromProgID(String progID)
{
  // Find in static map
  if(g_comClasses.count(progID))
  {
    return g_comClasses[progID];
  }

  // Create instance
  ComClass* nc = new ComClass(progID);
  g_comClasses[progID] = nc;

  // Done
  return nc;
}

/*static*/ ComClass* 
ComClass::FromDispatch(IDispatch* pdisp)
{
  ComTypeInfo* pTypeInfo = new ComTypeInfo(pdisp);
  return new ComClass(pTypeInfo);
}

ComClass::ComClass(String progID) :

Class     (progID),
m_progID  (progID),
m_clsid   (CLSID_NULL),
m_typeInfo    (0)
{
  USES_CONVERSION;

  // Find classid from program id
  if(FAILED(CLSIDFromProgID(T2W(progID.c_str()), &m_clsid)))
  {
    throw std::runtime_error("Invalid COM classname");
  }
}

ComClass::ComClass(ComTypeInfo* pTypeInfo) :
Class     (pTypeInfo->GetProgID()),
m_progID  (pTypeInfo->GetProgID()),
m_clsid   (pTypeInfo->GetCLSID()),
m_typeInfo    (pTypeInfo)
{

}

ComClass::~ComClass()
{
  delete m_typeInfo;
}

IDispatch*
ComClass::CreateInstance() const
{
  IDispatch* pDispatch;

  // Create the instance
  if(FAILED(CoCreateInstance(m_clsid, NULL, CLSCTX_ALL, 
                  IID_IDispatch, (void**)&pDispatch)))
  {
    if(FAILED(CoCreateInstance(m_clsid, NULL, CLSCTX_LOCAL_SERVER, 
                               IID_IDispatch,(void**)&pDispatch)))
    {
      throw std::runtime_error("Failed to instantiate COM object");
    }
  }

  // Load the type library
  if(m_typeInfo == 0)
  {
    m_typeInfo = new ComTypeInfo(pDispatch);
  }

  // Done
  return pDispatch;
}

ComTypeInfo* 
ComClass::GetTypeInfo() const
{
  return m_typeInfo;
}

bool 
ComClass::FindConversion(TypeInfo const& type, ConversionOperator*& node) const
{
  return false;
}

//////////////////////////////////////////////////////////////////////////

/*static*/ Instance* 
ComInstance::Create(ComClass const* c)
{
  return new ComInstance(c);
}

/*static*/ Instance* 
ComInstance::Create(IDispatch* pdisp)
{
  ComClass* c = ComClass::FromDispatch(pdisp); 
  return new ComInstance(c, pdisp);
}

ComInstance::ComInstance(ComClass const* c, IDispatch* pdisp) : 
Instance    (0, c),
m_class     (c),
m_dispatch  (pdisp)
{
  // Delegate to class for construction
  if(m_dispatch == 0)
  {
    m_dispatch = m_class->CreateInstance();
  }
  else
  {
    m_dispatch->AddRef();
  }
}

ComInstance::~ComInstance()
{
  // Remove pointer from instance
  IDispatch* pDispatch = m_dispatch;
  m_dispatch = 0;

  // Release the object
  if(pDispatch)
  {
    pDispatch->Release();
  }
}

bool 
ComInstance::Find(Value const& name, RValue*& ptr) const
{
  // No typelib loaded yet
  if(m_class->GetTypeInfo() == 0)
  {
    return false;
  }

  // Find method info
  DISPID dispid;
  if(!m_class->GetTypeInfo()->GetDispIdOfName(name.GetString(), dispid))
  {
    return false;
  }

  // Retrieve proper name
  String properName;
  if(!m_class->GetTypeInfo()->GetNameOfDispId(dispid, properName))
  {
    return false;
  }

  // We'll need a non-const version of the members array
  Members& members = const_cast<Members&>(m_members);

  // Lookup the variable
  if(members.count(properName))
  {
    ptr = members[properName];
    return true;
  }

  // Retrieve function info
  FUNCDESC* pfd;
  if(!m_class->GetTypeInfo()->GetFuncDescOfDispId(dispid, &pfd))
  {
    return false;
  }

  // Create right sort of object
  if(pfd->invkind == INVOKE_FUNC)
  {
    ptr = new ROVariable(new ComMemberFunction(properName, dispid, this));
  }
  else
  {
    ptr = new ComMemberVariable(properName, dispid, this);
  }

  // Add to member variables
  members[properName] = ptr;

  // Done
  return true;
}

void
ComInstance::Invoke(DISPID dispid, INVOKEKIND invokeKind, Arguments& args, VARIANT& vResult) const
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
  if(FAILED(m_class->GetTypeInfo()->GetFuncDescOfDispId(dispid, &pfd)))
  {
    throw std::runtime_error("Object doesn't support this property or method");
  }

  // Check parameter count
  USHORT minPars = pfd->cParams - pfd->cParamsOpt;
  USHORT maxPars = pfd->cParams;
  if(/*args.size() < minPars ||*/ args.size() > maxPars)
  {
    throw std::runtime_error("Invalid number of arguments supplied to COM method");
  }

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
    m_class->GetTypeInfo()->GetNameOfDispId(pfd->memid, name);
    String err = W2T(excepInfo.bstrDescription);
    err = "COM method '" + name + "' failed: " + err;
    throw std::runtime_error(err);
  }
}

Value
ComInstance::Invoke(DISPID dispid, INVOKEKIND invokeKind, Arguments& args) const
{
  // Initialize result
  VARIANT vResult;
  VariantInit(&vResult);

  // Invoke the method
  Invoke(dispid, invokeKind, args, vResult);
  
  // Convert result value
  Value result;
  VariantToValue(vResult, result);
  VariantClear(&vResult);

  // Done
  return result;
}

//////////////////////////////////////////////////////////////////////////

ComEnumerator::ComEnumerator(IEnumVARIANTPtr const& pEnum) :
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
  VariantToValue(vElem, value);
  VariantClear(&vElem);

  // Done
  return true;
}

//////////////////////////////////////////////////////////////////////////

ComMemberVariable::ComMemberVariable(String name, DISPID dispid, ComInstance const* inst) :
m_name    (name),
m_dispid  (dispid),
m_inst    (inst)
{
}

Value const& 
ComMemberVariable::GetValue() const
{
  // Setup empty argument list
  Arguments args;

  // Invoke property getter
  m_value = m_inst->Invoke(m_dispid, INVOKE_PROPERTYGET, args);

  // Return stored value
  return m_value;
}

void 
ComMemberVariable::SetValue(Value const& rhs)
{
  // Setup argument list
  Arguments args;
  args.push_back(rhs);

  // Invoke property setter
  m_inst->Invoke(m_dispid, INVOKE_PROPERTYPUT, args);
}

ComEnumerator* 
ComMemberVariable::GetEnumerator() const
{
  // Retrieve property value
  Value const& value = GetValue();

  // Check that it's a dispatch pointer
  if(value.Type() != Value::tObject)
  {
    throw std::runtime_error("Member cannot be enumerated");
  }

  // Convert the object to a ComInstance
  ComInstance* ci = dynamic_cast<ComInstance*>(&value.GetObject());
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
  return new ComEnumerator(pEnum);
}

//////////////////////////////////////////////////////////////////////////

ComMemberFunction::ComMemberFunction(String name, DISPID dispid, ComInstance const* instance) :
Function  (name),
m_inst    (instance),
m_dispid  (dispid)
{
}

AstList const* 
ComMemberFunction::GetParameters() const
{
  return 0;
}

Value 
ComMemberFunction::Execute(Evaluator*, Arguments& args)
{
  return m_inst->Invoke(m_dispid, INVOKE_FUNC, args);
}
