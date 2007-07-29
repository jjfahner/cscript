#ifndef CSCRIPT_NATIVE_CALLS_H
#define CSCRIPT_NATIVE_CALLS_H

#include "types.h"
#include "var.h"

class StackMachine;

//
// Resolve a native call by name
//
Function* FindNative(String const& name);

//
// Execute a native call
//
void ExecNative(Quad index, StackMachine& machine, Word numArgs);


#endif // #ifndef CSCRIPT_NATIVE_CALLS_H
