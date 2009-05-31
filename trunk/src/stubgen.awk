#
# Script to generate native call stubs for cscript
#

# Setup separators
BEGIN {
  #
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

# Match native functions
$1 ~ /__native_method/ || 
$1 ~ /__native_roprop/ || 
$1 ~ /__native_rwprop/ {

  # Detect when the script moves to a new file
  if(filename != FILENAME)
  {
    # Store current name for matching
    filename = FILENAME;
    
    # Generate an include statement
    file = gensub("\\\\", "/", "g", filename);
    file = gensub("include/",  "", "", file);
    printf("\n#include <%s>\n", file);
  }

  # Generate index for arrays
  idx = className classIds[className];
  
  # Store unmodified declaration
  stubDecls[idx] = $0;
  stubFiles[idx] = filename;

  # Separate punctuation from words
  gsub("\\).*$", " ) ");
  gsub("\\(", " ( ");
  gsub(",", " , ");
  gsub(";", " ; ");
  gsub("=", " = ");
  
  # Store stub type and discard
  stubTypes[idx] = $1;
  $1 = "";
  $0 = $0;
  
  # Discard any function specifiers
  while($1 == "virtual"  ||
        $1 == "static"   ||
        $1 == "inline"   ||
        $1 == "explicit" ||
        $1 == "static"   )
  {
    $1 = "";
    $0 = $0;
  }
  
  # Start parsing from first argument
  i = 1;
  
  # Parse type and name
  type = "";
  name = "";
  for(i = 1; i <= NF && $i != "("; ++i)
  {
    type = type name;
    name = $i;
  }
  
  # Check both
  if(type == "" || name == "")
  {
    print("Missing return type or method name\n");
    exit 1;
  }
  
  # Generate stub prolog
  printf("static Value cscript_native_stub_%s_%s\n", className, name);
  printf("  (Evaluator* evaluator, Object* instance, Arguments const& arguments)\n");
  printf("{\n");
  
  # Generate function call
  returns = type == "void" ? "" : "return ";
  printf("  %sstatic_cast<%s*>(instance)->%s(", returns, className, name);
  
  # Close function call
  printf(");\n");
  
  # Generate return statement
  if(type == "void")
  {
    printf("  return Value();\n");
  }
  
  # Generate stub epilog
  printf("}\n");
}
