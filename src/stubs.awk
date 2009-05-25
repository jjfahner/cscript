# Script to generate native call stubs for cscript
#
# The script will store a classname when it is encountered, 
# and use it for stub generation whenever a __native method
# is encountered.
#
# Supported syntax:
#
# '__native' ['virtual'] MethodName(<'type'> <'name'>, ...) 
#
# TODO Match invalid __native_xxx statements

# Setup separators
BEGIN {
  FS = "[ \t(),]+";
};

# Remember most current classname
/^class [a-zA-Z0-9_]+$/ { 
  className = $2;
  classIds[className] = 0;
}
/^class [a-zA-Z0-9_]+ +:/ { 
  className = $2;  
  classIds[className] = 0;
}

# Handle native declarations
$2 ~ /__native_method/ {
  
  # Check number of words
  if(NF < 3) {
    next;
  }
  
  # Find index of first word
  for(i = 1; i <= NF; i++) {
    if($i == "__native_method") {
      i++;
      if($i == "virtual") {
        i++;
      }
      break;
    }
  }
  
  # Store return type
  returns = $i;
  i++;
  
  # Store member name
  memberName = $i;
  i++;
  
  # Enumerate parameters
  p = 0;
  for(; i <= NF; i++) {
    if($i == "" || $i == "=" || $i == ";" || $i == "{" || $i == "\r") {
      break;
    }
    else {
      # Store parameter type and name
      p++;
      ptype[p] = $i;
      i++;
      pname[p] = $i;
    }
  }
  
  # Generate call stub
  stub = sprintf("__stub_%s_%s", className, memberName);
  printf("static Value %s(Evaluator*, Arguments const& args) {\n", stub);
  if(returns == "void")
  {
    printf("  dynamic_cast<%s*>(args.GetObject())->%s(", className, memberName);
  }
  else
  {
    printf("  return dynamic_cast<%s*>(args.GetObject())->%s(", className, memberName);
  }
  for(i = 1; i <= p; ++i)
  {
    comma = "";
    if(i < p) comma = ",";
    printf("\n    __stub_arg_to_%s(args[%d])%s", ptype[i], (i-1), comma);
  }
  printf(");\n");
  if(returns == "void")
  {
    printf("  return Value();\n");
  }
  printf("}\n");
  
  # Store name and stub in arrays
  i = classIds[className] + 1;
  stubName[className i]  = memberName;
  stubProcR[className i] = stub;
  stubProcW[className i] = "0";
  stubType[className i]  = "stMethod";
  classIds[className]    = i;
}

# Handle native declarations
$2 ~ /__native_roprop/ || $2 ~ /__native_rwprop/ {
  
  # Check number of words
  if(NF < 3) {
    next;
  }
  
  # Find index of first word
  for(i = 1; i <= NF; i++) {
    if($i == "__native_roprop") {
      type = "stRoProp";
      i++;
      break;
    }
    if($i == "__native_rwprop") {
      type = "stRwProp";
      i++;
      break;
    }
  }
  
  # Strip virtual
  if($i == "virtual") {
    i++;
  }
  
  # Store return type
  returns = $i;
  i++;
  
  # Store member name
  memberName = $i;
  
  # Generate reader call stub
  stubR = sprintf("__stub_%s_%s", className, memberName);
  printf("static Value %s(Object* instance) {\n", stubR);
  printf("  return dynamic_cast<%s*>(instance)->%s();\n", className, memberName);
  printf("}\n");
  
  # Generate writer call stub
  stubW = "0";
  if(type == "stRwProp")
  {
    stubW = sprintf("__stub_set_%s_%s", className, memberName);
    printf("static void %s(Object* instance, Value const& value) {\n", stubW);
    printf("  dynamic_cast<%s*>(instance)->Set%s(value);\n", className, memberName);
    printf("}\n");
  }

  # Store name and stub in arrays
  i = classIds[className] + 1;
  stubName[className i] = memberName;
  stubProcR[className i] = stubR;
  stubProcW[className i] = stubW;
  stubType[className i] = type;
  classIds[className] = i;
}

END {
  
  # Generate tables
  for(className in classIds)
  {
    numStubs = classIds[className]; 
    if(numStubs > 0)
    {
      printf("\nNativeCall __stublist_%s[] = {\n", className);
      for(i = 1; i <= numStubs; ++i)
      {
        printf("  { %s, \"%s\", (void*)%s, (void*)%s, 0 },\n", stubType[className i], stubName[className i], stubProcR[className i], stubProcW[className i]);
      }
      printf("  { stMethod, 0, 0, 0, 0 }\n");
      printf("};\n");
    }
  }  
}
