#ifndef CSCRIPT_ERROR_H
#define CSCRIPT_ERROR_H

//
// Notice information
//
struct Notice
{
  int         m_number;
  char const* m_notice;
};

//
// Errors
//
#define ERROR_EXPANSION_LIST  \
ERROR_EXPANSION(E0001,     1, "internal compiler error in file %s on line %d."); \
ERROR_EXPANSION(E0002,     2, "function '%s' already defined."); \
ERROR_EXPANSION(E0003,     3, "undefined variable '%s'."); \
ERROR_EXPANSION(E0004,     4, "invalid number of arguments in call to function '%s'."); \
ERROR_EXPANSION(E0005,     5, "function '%s' not found."); \
ERROR_EXPANSION(E0006,     6, "undeclared type '%s'."); \
ERROR_EXPANSION(E0007,     7, "invalid initializer for variable '%s'."); \
ERROR_EXPANSION(E0008,     8, "struct '%s' already declared."); \
ERROR_EXPANSION(E0010,    10, "switch statement contains more than one default case."); \
ERROR_EXPANSION(E0011,    11, "switch statement contains duplicate case value."); \
ERROR_EXPANSION(E0012,    12, "unrecoverable syntax error."); \
ERROR_EXPANSION(E0013,    13, "syntax error.");

//
// Expand errors to extern declarations
//
#define ERROR_EXPANSION(name,number,notice) \
  extern Notice name
ERROR_EXPANSION_LIST
#undef ERROR_EXPANSION

#endif // CSCRIPT_ERROR_H
