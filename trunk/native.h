#ifndef CSCRIPT_NATIVE_CALLS_H
#define CSCRIPT_NATIVE_CALLS_H

#include "types.h"
#include "var.h"

class StackMachine;

//
// Resolve a native call by name
//
Quad FindNative(std::wstring const& name);

//
// Execute a native call
//
void ExecNative(Quad index, StackMachine& machine);


#endif // #ifndef CSCRIPT_NATIVE_CALLS_H
