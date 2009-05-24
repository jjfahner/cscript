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
/^class [a-zA-Z0-9_]+$/   { className = $2 }
/^class [a-zA-Z0-9_]+ +:/ { className = $2 }

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
  printf("inline %s __stub_%s_%s(Evaluator*, Arguments const& args)\n{\n", returns, className, memberName);
  printf("  return dynamic_cast<%s*>(args.GetObject())->%s(\n", className, memberName);
  for(i = 1; i <= p; ++i)
  {
    comma = "";
    if(i < p) comma = ",";
    printf("    __stub_arg_to_%s(args[%d])%s\n", ptype[i], (i-1), comma);
  }
  printf("    );\n");
  printf("}\n");
  
  print stub;
}
