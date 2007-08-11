#include "error.h"

//
// Expand errors to definitions
//
#define ERROR_EXPANSION(name,number,notice) \
  Notice name = { number, notice }
ERROR_EXPANSION_LIST
#undef ERROR_EXPANSION
