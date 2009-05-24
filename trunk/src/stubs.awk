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
# TODO Match invalid __native statements


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
$2 ~ /__native/ {
  
  # Check number of words
  if(NF < 3) {
    next;
  }
  
  # Find index of first word
  for(i = 1; i <= NF; i++) {
    if($i == "__native") {
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
  
  # Print name
  print name;
  
  # Enumerate parameters
  p = 0;
  for(; i <= NF; i++) {
    if($i == "=" || $i == ";" || $i == "{") {
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
  printf("inline Value %s(Evaluator*, Arguments const& args)\n{\n", stub);
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
        printf("  { \"%s\", %s, 0 },\n", stubName[className i], stubProc[className i])
      }
      printf("  { 0, 0, 0 }\n");
      printf("};\n");
    }
  }  
}
