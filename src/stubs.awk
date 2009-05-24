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
  printf("  return dynamic_cast<%s*>(args.GetObject())->%s(\n", className, memberName);
  for(i = 1; i <= p; ++i)
  {
    comma = "";
    if(i < p) comma = ",";
    printf("    __stub_arg_to_%s(args[%d])%s\n", ptype[i], (i-1), comma);
  }
  printf("    );\n");
  printf("}\n");
  
  # Store name and stub in arrays
  i = classIds[className] + 1;
  stubName[className i] = memberName;
  stubProc[className i] = stub;
  stubType[className i] = "stMethod";
  classIds[className] = i;
}

# Handle native declarations
$2 ~ /__native_roprop/ {
  
  # Check number of words
  if(NF < 3) {
    next;
  }
  
  # Find index of first word
  for(i = 1; i <= NF; i++) {
    if($i == "__native_roprop") {
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
  
  # Generate call stub
  stub = sprintf("__stub_%s_%s", className, memberName);
  printf("static Value %s(Object* instance) {\n", stub);
  printf("  return dynamic_cast<%s*>(instance)->%s();\n", className, memberName);
  printf("}\n");

  # Store name and stub in arrays
  i = classIds[className] + 1;
  stubName[className i] = memberName;
  stubProc[className i] = stub;
  stubType[className i] = "stRoProp";
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
        printf("  { \"%s\", %s, %s, 0 },\n", stubName[className i], stubProc[className i], stubType[className i]);
      }
      printf("  { 0, 0, stMethod, 0 }\n");
      printf("};\n");
    }
  }  
}
