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
    CoUninitialize();
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
  return ComInstance::Create(evaluator, cc);
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
  case VT_EMPTY:  value.Clear(); break;
  case VT_BOOL:   value = variant.boolVal != 0; break;
  case VT_I8:     value = variant.llVal; break;
  case VT_BSTR:   value = W2T(variant.bstrVal); break;
  default: throw std::runtime_error("Failed to convert OLE VARIANT to native type");
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

ComClass::ComClass(String progID) :
Class     (progID),
m_progID  (progID),
m_clsid   (CLSID_NULL),
m_info    (0)
{
  USES_CONVERSION;

  // Find classid from program id
  if(FAILED(CLSIDFromProgID(T2W(progID.c_str()), &m_clsid)))
  {
    throw std::runtime_error("Invalid COM classname");
  }
}

ComClass::~ComClass()
{
  delete m_info;
}

bool 
ComClass::FindMethod(String const& name, MemberFunction*& fun) const
{
  // No typelib loaded yet
  if(m_info == 0)
  {
    return false;
  }

  // Find method info
  DISPID dispid;
  if(!m_info->GetDispIdOfName(name, dispid))
  {
    return false;
  }

  // Lookup in method list
  if(m_methods.count(dispid))
  {
    fun = m_methods[dispid];
    return true;
  }

  // Retrieve proper name
  String properName;
  if(!m_info->GetNameOfDispId(dispid, properName))
  {
    return false;
  }

  // Retrieve function info
  FUNCDESC* pfd;
  if(!m_info->GetFuncDescOfDispId(dispid, &pfd))
  {
    return false;
  }

  // Property accessors are not returned as functions
  if(pfd->invkind == INVOKE_PROPERTYGET    ||
     pfd->invkind == INVOKE_PROPERTYPUT    ||
     pfd->invkind == INVOKE_PROPERTYPUTREF )
  {
    return false;
  }

  // Create and cache method
  ComMemberFunction* method = new ComMemberFunction(properName, this, dispid);
  m_methods[dispid] = method;

  // Succeeded
  fun = method;
  return true;
}

bool 
ComClass::FindConversion(TypeInfo const& type, ConversionOperator*& node) const
{
  return false;
}

void 
ComClass::ConstructInstance(ComInstance* inst) const
{
  IDispatch* pDispatch;

  // Create the instance
  if(SUCCEEDED(CoCreateInstance(m_clsid, NULL,
    CLSCTX_ALL, IID_IDispatch, (void**)&pDispatch)))
  {
    inst->m_dispatch = pDispatch;
  }
  else if(SUCCEEDED(CoCreateInstance(m_clsid, NULL,
    CLSCTX_LOCAL_SERVER, IID_IDispatch,(void**)&pDispatch)))
  {
    inst->m_dispatch = pDispatch;
  }
  else
  {
    throw std::runtime_error("Failed to instantiate COM object");
  }

  // Load the type library (if required)
  if(m_info == 0)
  {
    m_info = new ComTypeInfo(inst->m_dispatch);
  }
}

void 
ComClass::DestructInstance(ComInstance* inst) const
{
  // Remove pointer from instance
  IDispatch* pDispatch = inst->m_dispatch;
  inst->m_dispatch = 0;

  // Release the object
  if(pDispatch)
  {
    pDispatch->Release();
  }
}

//////////////////////////////////////////////////////////////////////////

/*static*/ Instance* 
ComInstance::Create(Evaluator* eval, ComClass const* c)
{
  return new ComInstance(eval, c);
}

ComInstance::ComInstance(Evaluator* eval, ComClass const* c) : 
Instance    (eval, c),
m_class     (c),
m_dispatch  (0)
{
  // Delegate to class
  m_class->ConstructInstance(this);
}

bool 
ComInstance::FindVar(String const& name, Value& ref) const
{
  // No typelib loaded yet
  if(m_class->m_info == 0)
  {
    return false;
  }

  // Find method info
  DISPID dispid;
  if(!m_class->m_info->GetDispIdOfName(name, dispid))
  {
    return false;
  }

  // Retrieve proper name
  String properName;
  if(!m_class->m_info->GetNameOfDispId(dispid, properName))
  {
    return false;
  }

  // Retrieve function info
  FUNCDESC* pfd;
  if(!m_class->m_info->GetFuncDescOfDispId(dispid, &pfd))
  {
    return false;
  }

  // Property accessors are not returned as functions
  if(pfd->invkind == INVOKE_PROPERTYGET    ||
     pfd->invkind == INVOKE_PROPERTYPUT    ||
     pfd->invkind == INVOKE_PROPERTYPUTREF )
  {
    return false;
  }

  // TODO

  return false;
}

Value 
ComInstance::Invoke(Evaluator* evaluator, DISPID dispid, Arguments& args)
{
  USES_CONVERSION;

  // Retrieve function info for the dispid
  FUNCDESC* pfd;
  if(FAILED(m_class->m_info->GetFuncDescOfDispId(dispid, &pfd)))
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
  if(pfd->invkind == DISPATCH_PROPERTYPUT)
  {
    dispparams.cNamedArgs = 1;
    dispparams.rgdispidNamedArgs = &disp;
  }

  // Initialize result value
  VARIANT vtResult;
  VariantInit(&vtResult);

  // Initialize error info
  EXCEPINFO excepInfo;
  memset(&excepInfo, 0, sizeof excepInfo);

  // Initialize to invalid value
  UINT nArgErr = (UINT)-1;

  // Invoke the method
  HRESULT hr;
  hr = m_dispatch->Invoke(pfd->memid, IID_NULL, LOCALE_USER_DEFAULT, 
        pfd->invkind, &dispparams, &vtResult, &excepInfo, &nArgErr);

  // Cleanup argument list
  for(unsigned int i = 0; i < dispparams.cArgs; ++i)
  {
    VariantClear(&dispparams.rgvarg[i]);
  }
  delete [] dispparams.rgvarg;

  // Throw error
  if(FAILED(hr)) 
  {
    String name;
    m_class->m_info->GetNameOfDispId(pfd->memid, name);
    String err = W2T(excepInfo.bstrDescription);
    err = "COM method '" + name + "' failed: " + err;
    throw std::runtime_error(err);
  }

  // Determine result
  Value result;
  VariantToValue(vtResult, result);
  VariantClear(&vtResult);

  // Klaar
  return result;
}

//////////////////////////////////////////////////////////////////////////

ComMemberFunction::ComMemberFunction(String name, ComClass const* cl, DISPID dispid) :
MemberFunction  (name, cl, 0),
m_class         (cl),
m_dispid        (dispid)
{
}

AstList const* 
ComMemberFunction::GetParameters() const
{
  return 0;
}

Value 
ComMemberFunction::Execute(Evaluator* evaluator, Arguments& args)
{
  USES_CONVERSION;

  // Fetch instance
  ComInstance* inst = dynamic_cast<ComInstance*>(args.GetInstance());
  if(inst == 0)
  {
    throw std::runtime_error("COM method invoked on invalid object type");
  }

  // Invoke on instance
  return inst->Invoke(evaluator, m_dispid, args);
}
