#ifndef CSCRIPT_TOKENS_H
#define CSCRIPT_TOKENS_H

#pragma once

//
// Include lemon-generated symbols
//
#include "cscript.h"

//
// Define additional instruction values
//
#define TOK_LVALUE        100
#define TOK_RVALUE        101
#define TOK_PREINC        102
#define TOK_PRESUB        103
#define TOK_POSTINC       104
#define TOK_POSTSUB       105
#define TOK_HALT          106
#define TOK_VARINIT       107
#define TOK_POP           108
#define TOK_UNDEF         109
#define TOK_JMP           110
#define TOK_JZ            111
#define TOK_JNZ           112

#endif // #ifndef CSCRIPT_TOKENS_H
