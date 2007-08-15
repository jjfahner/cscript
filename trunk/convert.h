#ifndef CSCRIPT_CONVERT_H
#define CSCRIPT_CONVERT_H

#include "types.h"

//
// Convert hexadecimal string to integer value
//
Quad hex2dec(char const* src);

//
// Convert binary string to integer value
//
Quad bin2dec(char const* src);

//
// Convert roman string to integer value
//
Quad rom2dec(char const* src);

#endif // CSCRIPT_CONVERT_H
