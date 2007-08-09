/* Driver template for the LEMON parser generator.
** The author disclaims copyright to this source code.
*/
/* First off, code is include which follows the "include" declaration
** in the input file. */
#include <stdio.h>
#line 30 "astgen.in"


#include "tokens.h"
#include "parser.h"
#include "ast.h"

#pragma warning(disable:4065)

#line 18 "astgen.c"
/* Next is all token values, in a form suitable for use by makeheaders.
** This section will be null unless lemon is run with the -m switch.
*/
/* 
** These constants (all generated automatically by the parser generator)
** specify the various kinds of tokens (terminals) that the parser
** understands. 
**
** Each symbol here is a terminal symbol in the grammar.
*/
/* Make sure the INTERFACE macro is defined.
*/
#ifndef INTERFACE
# define INTERFACE 1
#endif
/* The next thing included is series of defines which control
** various aspects of the generated parser.
**    YYCODETYPE         is the data type used for storing terminal
**                       and nonterminal numbers.  "unsigned char" is
**                       used if there are fewer than 250 terminals
**                       and nonterminals.  "int" is used otherwise.
**    YYNOCODE           is a number of type YYCODETYPE which corresponds
**                       to no legal terminal or nonterminal number.  This
**                       number is used to fill in empty slots of the hash 
**                       table.
**    YYFALLBACK         If defined, this indicates that one or more tokens
**                       have fall-back values which should be used if the
**                       original value of the token will not parse.
**    YYACTIONTYPE       is the data type used for storing terminal
**                       and nonterminal numbers.  "unsigned char" is
**                       used if there are fewer than 250 rules and
**                       states combined.  "int" is used otherwise.
**    CScriptParseTOKENTYPE     is the data type used for minor tokens given 
**                       directly to the parser from the tokenizer.
**    YYMINORTYPE        is the data type used for all minor tokens.
**                       This is typically a union of many types, one of
**                       which is CScriptParseTOKENTYPE.  The entry in the union
**                       for base tokens is called "yy0".
**    YYSTACKDEPTH       is the maximum depth of the parser's stack.  If
**                       zero the stack is dynamically sized using realloc()
**    CScriptParseARG_SDECL     A static variable declaration for the %extra_argument
**    CScriptParseARG_PDECL     A parameter declaration for the %extra_argument
**    CScriptParseARG_STORE     Code to store %extra_argument into yypParser
**    CScriptParseARG_FETCH     Code to extract %extra_argument from yypParser
**    YYNSTATE           the combined number of states.
**    YYNRULE            the number of rules in the grammar
**    YYERRORSYMBOL      is the code number of the error symbol.  If not
**                       defined, then do no error processing.
*/
#define YYCODETYPE unsigned char
#define YYNOCODE 109
#define YYACTIONTYPE unsigned short int
#define CScriptParseTOKENTYPE  Token 
typedef union {
  CScriptParseTOKENTYPE yy0;
  Ast* yy31;
  AstList* yy127;
  opcodes yy198;
  int yy217;
} YYMINORTYPE;
#ifndef YYSTACKDEPTH
#define YYSTACKDEPTH 100
#endif
#define CScriptParseARG_SDECL  Parser* p ;
#define CScriptParseARG_PDECL , Parser* p 
#define CScriptParseARG_FETCH  Parser* p  = yypParser->p 
#define CScriptParseARG_STORE yypParser->p  = p 
#define YYNSTATE 209
#define YYNRULE 121
#define YYERRORSYMBOL 58
#define YYERRSYMDT yy217
#define YY_NO_ACTION      (YYNSTATE+YYNRULE+2)
#define YY_ACCEPT_ACTION  (YYNSTATE+YYNRULE+1)
#define YY_ERROR_ACTION   (YYNSTATE+YYNRULE)

/* Next are that tables used to determine what action to take based on the
** current state and lookahead token.  These tables are used to implement
** functions that take a state number and lookahead value and return an
** action integer.  
**
** Suppose the action integer is N.  Then the action is determined as
** follows
**
**   0 <= N < YYNSTATE                  Shift N.  That is, push the lookahead
**                                      token onto the stack and goto state N.
**
**   YYNSTATE <= N < YYNSTATE+YYNRULE   Reduce by rule N-YYNSTATE.
**
**   N == YYNSTATE+YYNRULE              A syntax error has occurred.
**
**   N == YYNSTATE+YYNRULE+1            The parser accepts its input.
**
**   N == YYNSTATE+YYNRULE+2            No such action.  Denotes unused
**                                      slots in the yy_action[] table.
**
** The action table is constructed as a single large table named yy_action[].
** Given state S and lookahead X, the action is computed as
**
**      yy_action[ yy_shift_ofst[S] + X ]
**
** If the index value yy_shift_ofst[S]+X is out of range or if the value
** yy_lookahead[yy_shift_ofst[S]+X] is not equal to X or if yy_shift_ofst[S]
** is equal to YY_SHIFT_USE_DFLT, it means that the action is not in the table
** and that yy_default[S] should be used instead.  
**
** The formula above is for computing the action when the lookahead is
** a terminal symbol.  If the lookahead is a non-terminal (as occurs after
** a reduce action) then the yy_reduce_ofst[] array is used in place of
** the yy_shift_ofst[] array and YY_REDUCE_USE_DFLT is used in place of
** YY_SHIFT_USE_DFLT.
**
** The following are the tables generated in this section:
**
**  yy_action[]        A single table containing all actions.
**  yy_lookahead[]     A table containing the lookahead for each entry in
**                     yy_action.  Used to detect hash collisions.
**  yy_shift_ofst[]    For each state, the offset into yy_action for
**                     shifting terminals.
**  yy_reduce_ofst[]   For each state, the offset into yy_action for
**                     shifting non-terminals after a reduce.
**  yy_default[]       Default action for each state.
*/
static const YYACTIONTYPE yy_action[] = {
 /*     0 */   162,  331,  128,  204,    9,  200,  193,  190,  187,  185,
 /*    10 */   181,  178,  177,  175,  171,  170,  167,  164,  112,  159,
 /*    20 */    15,  157,   57,  110,  152,   73,  120,  140,   31,  134,
 /*    30 */   142,   80,  162,    5,   77,  137,   10,  200,  193,  190,
 /*    40 */   187,  185,  181,  178,  177,  175,  171,  170,  167,  164,
 /*    50 */   112,  159,  119,  157,   57,   56,  152,   73,  120,  140,
 /*    60 */    29,  134,  142,  139,  132,  133,   77,  137,  188,  159,
 /*    70 */    81,  157,   57,  122,  152,   73,  120,  140,  162,  134,
 /*    80 */   142,  182,   10,  200,  193,  190,  187,  185,  181,  178,
 /*    90 */   177,  175,  171,  170,  167,  164,  112,  159,  125,  157,
 /*   100 */    57,   28,  152,   73,  120,  140,  127,  134,  142,  131,
 /*   110 */   132,  133,   77,  137,   38,   40,   41,   44,   39,   47,
 /*   120 */    48,   46,   45,   43,   42,  162,  101,  179,  206,    6,
 /*   130 */   200,  193,  190,  187,  185,  181,  178,  177,  175,  171,
 /*   140 */   170,  167,  164,  112,  159,  141,  157,   57,  105,  152,
 /*   150 */    73,  120,  140,   52,  134,  142,  162,  173,  174,   77,
 /*   160 */   137,  146,  193,  190,  187,  185,  181,  178,  177,  175,
 /*   170 */   171,  170,  167,  164,  112,  159,  144,  157,   57,  172,
 /*   180 */   152,   73,  120,  140,   60,  134,  142,   45,   43,   42,
 /*   190 */    77,  137,  162,   58,   84,  207,  194,  146,  193,  190,
 /*   200 */   187,  185,  181,  178,  177,  175,  171,  170,  167,  164,
 /*   210 */   112,  159,    4,  157,   57,  158,  152,   73,  120,  140,
 /*   220 */    86,  134,  142,  124,   14,   17,   77,  137,  162,   32,
 /*   230 */    18,   25,  145,  160,  193,  190,  187,  185,  181,  178,
 /*   240 */   177,  175,  171,  170,  167,  164,  112,  159,  104,  157,
 /*   250 */    57,    7,  152,   73,  120,  140,   22,  134,  142,  162,
 /*   260 */    80,   11,   77,  137,  191,  193,  190,  187,  185,  181,
 /*   270 */   178,  177,  175,  171,  170,  167,  164,  112,  159,    8,
 /*   280 */   157,   57,   19,  152,   73,  120,  140,   12,  134,  142,
 /*   290 */    26,  162,   53,   77,  137,   13,  196,  193,  190,  187,
 /*   300 */   185,  181,  178,  177,  175,  171,  170,  167,  164,  112,
 /*   310 */   159,    1,  157,   57,   23,  152,   73,  120,  140,  180,
 /*   320 */   134,  142,  162,   55,  129,   77,  137,   92,  193,  190,
 /*   330 */   187,  185,  181,  178,  177,  175,  171,  170,  167,  164,
 /*   340 */   112,  159,   87,  157,   57,   61,  152,   73,  120,  140,
 /*   350 */   332,  134,  142,  162,    2,  115,   77,  137,  153,  193,
 /*   360 */   190,  187,  185,  181,  178,  177,  175,  171,  170,  167,
 /*   370 */   164,  112,  159,  332,  157,   57,  332,  152,   73,  120,
 /*   380 */   140,  332,  134,  142,  162,  332,  332,   77,  137,  165,
 /*   390 */   193,  190,  187,  185,  181,  178,  177,  175,  171,  170,
 /*   400 */   167,  164,  112,  159,  332,  157,   57,  332,  152,   73,
 /*   410 */   120,  140,   49,  134,  142,   55,  184,  332,   77,  137,
 /*   420 */   201,  202,  332,   93,   51,   50,  332,   16,  332,  113,
 /*   430 */    27,  332,  143,  147,  149,  151,  154,  156,  332,  117,
 /*   440 */     3,  208,  111,   20,  107,  106,   54,  103,   98,   49,
 /*   450 */   332,  332,   95,   91,   89,   48,   46,   45,   43,   42,
 /*   460 */   332,   51,   50,  332,   16,  332,  113,   27,  332,  143,
 /*   470 */   147,  149,  151,  154,  156,  332,  117,    3,  116,  111,
 /*   480 */    20,  107,  106,   54,  103,   98,   49,  332,  332,   95,
 /*   490 */    91,   89,   99,  332,  198,  202,   83,  100,   51,   50,
 /*   500 */   332,   16,  332,  113,   27,  332,  143,  147,  149,  151,
 /*   510 */   154,  156,  332,  117,    3,  332,  111,   20,  107,  106,
 /*   520 */    54,  103,   98,  332,  332,  332,   95,   91,   89,   33,
 /*   530 */    34,   35,   36,   37,   38,   40,   41,   44,   39,   47,
 /*   540 */    48,   46,   45,   43,   42,  183,  186,  189,  195,  199,
 /*   550 */   205,   24,   34,   35,   36,   37,   38,   40,   41,   44,
 /*   560 */    39,   47,   48,   46,   45,   43,   42,   35,   36,   37,
 /*   570 */    38,   40,   41,   44,   39,   47,   48,   46,   45,   43,
 /*   580 */    42,   49,   41,   44,   39,   47,   48,   46,   45,   43,
 /*   590 */    42,  332,  332,   51,   50,  332,   16,  332,  113,   27,
 /*   600 */   332,  143,  147,  149,  151,  154,  156,   49,  117,   75,
 /*   610 */   332,  152,   73,  120,  140,   54,  134,  142,  332,   51,
 /*   620 */    50,   49,   16,  332,  113,   27,  114,  143,  147,  149,
 /*   630 */   151,  154,  156,   51,   50,  332,   16,  332,  113,   27,
 /*   640 */   192,  143,  147,  149,  151,  154,  156,  332,  123,  332,
 /*   650 */   332,  112,  159,  332,  157,   57,  332,  152,   73,  120,
 /*   660 */    97,  332,  134,  142,  332,  332,  155,   79,  157,   57,
 /*   670 */   332,  152,   73,  120,  140,  332,  134,  142,   21,  332,
 /*   680 */   332,  332,  332,   36,   37,   38,   40,   41,   44,   39,
 /*   690 */    47,   48,   46,   45,   43,   42,  168,  159,  332,  157,
 /*   700 */    57,   49,  152,   73,  120,  140,   78,  134,  142,  332,
 /*   710 */   332,  176,  332,   51,   50,  332,   16,  332,  113,   27,
 /*   720 */   332,  143,  147,  149,  151,  154,  156,  332,  163,  159,
 /*   730 */   332,  157,   57,  332,  152,   73,  120,  140,  332,  134,
 /*   740 */   142,   76,  166,   37,   38,   40,   41,   44,   39,   47,
 /*   750 */    48,   46,   45,   43,   42,  332,  332,  332,  332,  332,
 /*   760 */   163,  159,  332,  157,   57,  332,  152,   73,  120,  140,
 /*   770 */   332,  134,  142,  332,  161,  332,  168,  159,  332,  157,
 /*   780 */    57,  332,  152,   73,  120,  140,  332,  134,  142,  332,
 /*   790 */   332,  169,  332,  102,  159,  332,  157,   57,  332,  152,
 /*   800 */    73,  120,  140,  332,  134,  142,   96,  159,  332,  157,
 /*   810 */    57,  332,  152,   73,  120,  140,  332,  134,  142,   82,
 /*   820 */   159,  332,  157,   57,  332,  152,   73,  120,  140,  332,
 /*   830 */   134,  142,  150,  148,   59,   30,  332,  108,  159,  332,
 /*   840 */   157,   57,  332,  152,   73,  120,  140,  332,  134,  142,
 /*   850 */   332,  109,  159,  332,  157,   57,  332,  152,   73,  120,
 /*   860 */   140,  332,  134,  142,   90,  159,  332,  157,   57,  332,
 /*   870 */   152,   73,  120,  140,  332,  134,  142,   85,  159,  332,
 /*   880 */   157,   57,  332,  152,   73,  120,  140,  332,  134,  142,
 /*   890 */    94,  159,  332,  157,   57,  332,  152,   73,  120,  140,
 /*   900 */   332,  134,  142,  197,  159,  332,  157,   57,  332,  152,
 /*   910 */    73,  120,  140,  332,  134,  142,  332,   88,  159,  332,
 /*   920 */   157,   57,  332,  152,   73,  120,  140,  332,  134,  142,
 /*   930 */   332,  136,  159,  332,  157,   57,  332,  152,   73,  120,
 /*   940 */   140,  332,  134,  142,  332,  203,  332,  157,   57,  332,
 /*   950 */   152,   73,  120,  140,  332,  134,  142,  126,  332,  152,
 /*   960 */    73,  120,  140,  332,  134,  142,   71,  332,  152,   73,
 /*   970 */   120,  140,  332,  134,  142,   70,  332,  152,   73,  120,
 /*   980 */   140,  332,  134,  142,   74,  332,  152,   73,  120,  140,
 /*   990 */   332,  134,  142,  121,  332,  152,   73,  120,  140,  332,
 /*  1000 */   134,  142,   66,  332,  152,   73,  120,  140,  332,  134,
 /*  1010 */   142,  332,  118,  332,  152,   73,  120,  140,  332,  134,
 /*  1020 */   142,   64,  332,  152,   73,  120,  140,  332,  134,  142,
 /*  1030 */   332,   69,  332,  152,   73,  120,  140,  332,  134,  142,
 /*  1040 */    68,  332,  152,   73,  120,  140,  332,  134,  142,   63,
 /*  1050 */   332,  152,   73,  120,  140,  332,  134,  142,   72,  332,
 /*  1060 */   152,   73,  120,  140,  332,  134,  142,   62,  332,  152,
 /*  1070 */    73,  120,  140,  332,  134,  142,   67,  332,  152,   73,
 /*  1080 */   120,  140,  332,  134,  142,   65,  332,  152,   73,  120,
 /*  1090 */   140,  332,  134,  142,  332,  138,   73,  120,  140,  332,
 /*  1100 */   134,  142,  135,   73,  120,  140,  332,  134,  142,  130,
 /*  1110 */    73,  120,  140,  332,  134,  142,  332,  143,  147,  149,
 /*  1120 */   151,  154,  156,
};
static const YYCODETYPE yy_lookahead[] = {
 /*     0 */    58,   59,   60,   61,   62,   63,   64,   65,   66,   67,
 /*    10 */    68,   69,   70,   71,   72,   73,   74,   75,   76,   77,
 /*    20 */    31,   79,   80,   35,   82,   83,   84,   85,   24,   87,
 /*    30 */    88,   30,   58,   32,   92,   93,   62,   63,   64,   65,
 /*    40 */    66,   67,   68,   69,   70,   71,   72,   73,   74,   75,
 /*    50 */    76,   77,   40,   79,   80,  104,   82,   83,   84,   85,
 /*    60 */    17,   87,   88,   94,   95,   96,   92,   93,   76,   77,
 /*    70 */    78,   79,   80,   40,   82,   83,   84,   85,   58,   87,
 /*    80 */    88,  107,   62,   63,   64,   65,   66,   67,   68,   69,
 /*    90 */    70,   71,   72,   73,   74,   75,   76,   77,   40,   79,
 /*   100 */    80,   17,   82,   83,   84,   85,   40,   87,   88,   94,
 /*   110 */    95,   96,   92,   93,    6,    7,    8,    9,   10,   11,
 /*   120 */    12,   13,   14,   15,   16,   58,   30,  107,  100,   62,
 /*   130 */    63,   64,   65,   66,   67,   68,   69,   70,   71,   72,
 /*   140 */    73,   74,   75,   76,   77,   32,   79,   80,   30,   82,
 /*   150 */    83,   84,   85,   31,   87,   88,   58,  105,  106,   92,
 /*   160 */    93,   63,   64,   65,   66,   67,   68,   69,   70,   71,
 /*   170 */    72,   73,   74,   75,   76,   77,   85,   79,   80,   42,
 /*   180 */    82,   83,   84,   85,   39,   87,   88,   14,   15,   16,
 /*   190 */    92,   93,   58,   56,   57,   30,   98,   63,   64,   65,
 /*   200 */    66,   67,   68,   69,   70,   71,   72,   73,   74,   75,
 /*   210 */    76,   77,   32,   79,   80,   29,   82,   83,   84,   85,
 /*   220 */    87,   87,   88,   32,   31,   39,   92,   93,   58,   81,
 /*   230 */    39,   50,   98,   63,   64,   65,   66,   67,   68,   69,
 /*   240 */    70,   71,   72,   73,   74,   75,   76,   77,   30,   79,
 /*   250 */    80,   32,   82,   83,   84,   85,   31,   87,   88,   58,
 /*   260 */    30,   32,   92,   93,   63,   64,   65,   66,   67,   68,
 /*   270 */    69,   70,   71,   72,   73,   74,   75,   76,   77,   32,
 /*   280 */    79,   80,   40,   82,   83,   84,   85,   52,   87,   88,
 /*   290 */    31,   58,   39,   92,   93,   32,   63,   64,   65,   66,
 /*   300 */    67,   68,   69,   70,   71,   72,   73,   74,   75,   76,
 /*   310 */    77,   24,   79,   80,   31,   82,   83,   84,   85,   29,
 /*   320 */    87,   88,   58,   39,   40,   92,   93,   63,   64,   65,
 /*   330 */    66,   67,   68,   69,   70,   71,   72,   73,   74,   75,
 /*   340 */    76,   77,   32,   79,   80,   41,   82,   83,   84,   85,
 /*   350 */   108,   87,   88,   58,   24,   40,   92,   93,   63,   64,
 /*   360 */    65,   66,   67,   68,   69,   70,   71,   72,   73,   74,
 /*   370 */    75,   76,   77,  108,   79,   80,  108,   82,   83,   84,
 /*   380 */    85,  108,   87,   88,   58,  108,  108,   92,   93,   63,
 /*   390 */    64,   65,   66,   67,   68,   69,   70,   71,   72,   73,
 /*   400 */    74,   75,   76,   77,  108,   79,   80,  108,   82,   83,
 /*   410 */    84,   85,   13,   87,   88,   39,   40,  108,   92,   93,
 /*   420 */    99,  100,  108,  102,   25,   26,  108,   28,  108,   30,
 /*   430 */    31,  108,   33,   34,   35,   36,   37,   38,  108,   40,
 /*   440 */    41,   42,   43,   44,   45,   46,   47,   48,   49,   13,
 /*   450 */   108,  108,   53,   54,   55,   12,   13,   14,   15,   16,
 /*   460 */   108,   25,   26,  108,   28,  108,   30,   31,  108,   33,
 /*   470 */    34,   35,   36,   37,   38,  108,   40,   41,   42,   43,
 /*   480 */    44,   45,   46,   47,   48,   49,   13,  108,  108,   53,
 /*   490 */    54,   55,   97,  108,   99,  100,  101,  102,   25,   26,
 /*   500 */   108,   28,  108,   30,   31,  108,   33,   34,   35,   36,
 /*   510 */    37,   38,  108,   40,   41,  108,   43,   44,   45,   46,
 /*   520 */    47,   48,   49,  108,  108,  108,   53,   54,   55,    1,
 /*   530 */     2,    3,    4,    5,    6,    7,    8,    9,   10,   11,
 /*   540 */    12,   13,   14,   15,   16,   17,   18,   19,   20,   21,
 /*   550 */    22,   23,    2,    3,    4,    5,    6,    7,    8,    9,
 /*   560 */    10,   11,   12,   13,   14,   15,   16,    3,    4,    5,
 /*   570 */     6,    7,    8,    9,   10,   11,   12,   13,   14,   15,
 /*   580 */    16,   13,    8,    9,   10,   11,   12,   13,   14,   15,
 /*   590 */    16,  108,  108,   25,   26,  108,   28,  108,   30,   31,
 /*   600 */   108,   33,   34,   35,   36,   37,   38,   13,   40,   80,
 /*   610 */   108,   82,   83,   84,   85,   47,   87,   88,  108,   25,
 /*   620 */    26,   13,   28,  108,   30,   31,   32,   33,   34,   35,
 /*   630 */    36,   37,   38,   25,   26,  108,   28,  108,   30,   31,
 /*   640 */    65,   33,   34,   35,   36,   37,   38,  108,   40,  108,
 /*   650 */   108,   76,   77,  108,   79,   80,  108,   82,   83,   84,
 /*   660 */    85,  108,   87,   88,  108,  108,   77,   92,   79,   80,
 /*   670 */   108,   82,   83,   84,   85,  108,   87,   88,  103,  108,
 /*   680 */   108,  108,  108,    4,    5,    6,    7,    8,    9,   10,
 /*   690 */    11,   12,   13,   14,   15,   16,   76,   77,  108,   79,
 /*   700 */    80,   13,   82,   83,   84,   85,   86,   87,   88,  108,
 /*   710 */   108,   91,  108,   25,   26,  108,   28,  108,   30,   31,
 /*   720 */   108,   33,   34,   35,   36,   37,   38,  108,   76,   77,
 /*   730 */   108,   79,   80,  108,   82,   83,   84,   85,  108,   87,
 /*   740 */    88,   89,   90,    5,    6,    7,    8,    9,   10,   11,
 /*   750 */    12,   13,   14,   15,   16,  108,  108,  108,  108,  108,
 /*   760 */    76,   77,  108,   79,   80,  108,   82,   83,   84,   85,
 /*   770 */   108,   87,   88,  108,   90,  108,   76,   77,  108,   79,
 /*   780 */    80,  108,   82,   83,   84,   85,  108,   87,   88,  108,
 /*   790 */   108,   91,  108,   76,   77,  108,   79,   80,  108,   82,
 /*   800 */    83,   84,   85,  108,   87,   88,   76,   77,  108,   79,
 /*   810 */    80,  108,   82,   83,   84,   85,  108,   87,   88,   76,
 /*   820 */    77,  108,   79,   80,  108,   82,   83,   84,   85,  108,
 /*   830 */    87,   88,   25,   26,   27,   28,  108,   76,   77,  108,
 /*   840 */    79,   80,  108,   82,   83,   84,   85,  108,   87,   88,
 /*   850 */   108,   76,   77,  108,   79,   80,  108,   82,   83,   84,
 /*   860 */    85,  108,   87,   88,   76,   77,  108,   79,   80,  108,
 /*   870 */    82,   83,   84,   85,  108,   87,   88,   76,   77,  108,
 /*   880 */    79,   80,  108,   82,   83,   84,   85,  108,   87,   88,
 /*   890 */    76,   77,  108,   79,   80,  108,   82,   83,   84,   85,
 /*   900 */   108,   87,   88,   76,   77,  108,   79,   80,  108,   82,
 /*   910 */    83,   84,   85,  108,   87,   88,  108,   76,   77,  108,
 /*   920 */    79,   80,  108,   82,   83,   84,   85,  108,   87,   88,
 /*   930 */   108,   76,   77,  108,   79,   80,  108,   82,   83,   84,
 /*   940 */    85,  108,   87,   88,  108,   77,  108,   79,   80,  108,
 /*   950 */    82,   83,   84,   85,  108,   87,   88,   80,  108,   82,
 /*   960 */    83,   84,   85,  108,   87,   88,   80,  108,   82,   83,
 /*   970 */    84,   85,  108,   87,   88,   80,  108,   82,   83,   84,
 /*   980 */    85,  108,   87,   88,   80,  108,   82,   83,   84,   85,
 /*   990 */   108,   87,   88,   80,  108,   82,   83,   84,   85,  108,
 /*  1000 */    87,   88,   80,  108,   82,   83,   84,   85,  108,   87,
 /*  1010 */    88,  108,   80,  108,   82,   83,   84,   85,  108,   87,
 /*  1020 */    88,   80,  108,   82,   83,   84,   85,  108,   87,   88,
 /*  1030 */   108,   80,  108,   82,   83,   84,   85,  108,   87,   88,
 /*  1040 */    80,  108,   82,   83,   84,   85,  108,   87,   88,   80,
 /*  1050 */   108,   82,   83,   84,   85,  108,   87,   88,   80,  108,
 /*  1060 */    82,   83,   84,   85,  108,   87,   88,   80,  108,   82,
 /*  1070 */    83,   84,   85,  108,   87,   88,   80,  108,   82,   83,
 /*  1080 */    84,   85,  108,   87,   88,   80,  108,   82,   83,   84,
 /*  1090 */    85,  108,   87,   88,  108,   82,   83,   84,   85,  108,
 /*  1100 */    87,   88,   82,   83,   84,   85,  108,   87,   88,   82,
 /*  1110 */    83,   84,   85,  108,   87,   88,  108,   33,   34,   35,
 /*  1120 */    36,   37,   38,
};
#define YY_SHIFT_USE_DFLT (-13)
#define YY_SHIFT_MAX 113
static const short yy_shift_ofst[] = {
 /*     0 */   473,  473,  473,  399,  473,  473,  436,  473,  473,  473,
 /*    10 */   473,  473,  473,  473,  568,  594,  688,  688,  688,  688,
 /*    20 */   608,  688,  688,  688,  688,  688,  688,  688,  688,  688,
 /*    30 */   688,  688,  688,  688,  688,  688,  688,  688,  688,  688,
 /*    40 */   688,  688,  688,  688,  688,  688,  688,  688,  688,  688,
 /*    50 */   688,  688,    1,  230,  218,  218,  137,  528, 1084,  165,
 /*    60 */   118,  -13,  550,  564,  679,  738,  108,  574,  574,  443,
 /*    70 */   443,  443,  443,  807,  173,  173,  186,  284,  191,  376,
 /*    80 */    43,  247,  242,  253,  287,  290,  330,  304,  310,  283,
 /*    90 */   263,  259,  235,  145,  229,  225,  219,  181,  193,  180,
 /*   100 */   145,  122,  113,   96,   84,   43,   66,   58,   33,    4,
 /*   110 */    12,  -12,  315,  -11,
};
#define YY_REDUCE_USE_DFLT (-59)
#define YY_REDUCE_MAX 61
static const short yy_reduce_ofst[] = {
 /*     0 */   -58,  -26,   20,   67,  134,   98,  233,  295,  201,  233,
 /*    10 */   233,  264,  170,  326,  575,  620,  652,  684,  700,   -8,
 /*    20 */   761,  743,  814,  841,  775,  730,  788,  717,  855,  827,
 /*    30 */   801,  868,  589,  987,  969,  941, 1005,  922,  960,  978,
 /*    40 */   996,  951,  932,  913,  895,  877,  529,  886,  904, 1013,
 /*    50 */  1027, 1020,  395,  321,  -31,   15,   52,  148,  133,   91,
 /*    60 */    28,  -49,
};
static const YYACTIONTYPE yy_default[] = {
 /*     0 */   213,  330,  330,  330,  330,  330,  330,  330,  330,  214,
 /*    10 */   329,  330,  330,  330,  330,  330,  330,  330,  330,  229,
 /*    20 */   330,  330,  330,  330,  330,  330,  330,  330,  330,  330,
 /*    30 */   330,  330,  330,  330,  330,  330,  330,  330,  330,  330,
 /*    40 */   330,  330,  330,  330,  330,  330,  330,  330,  330,  330,
 /*    50 */   330,  330,  330,  330,  330,  330,  330,  239,  330,  330,
 /*    60 */   330,  324,  242,  243,  244,  245,  246,  248,  247,  249,
 /*    70 */   250,  252,  251,  258,  253,  254,  330,  330,  330,  330,
 /*    80 */   306,  330,  330,  312,  330,  330,  330,  330,  330,  330,
 /*    90 */   330,  330,  320,  314,  330,  330,  330,  270,  330,  330,
 /*   100 */   313,  330,  330,  330,  302,  330,  330,  330,  330,  330,
 /*   110 */   330,  330,  330,  279,  267,  288,  290,  287,  257,  291,
 /*   120 */   262,  256,  292,  293,  268,  294,  255,  295,  209,  296,
 /*   130 */   261,  299,  300,  301,  269,  260,  303,  297,  259,  298,
 /*   140 */   270,  271,  272,  273,  265,  304,  315,  274,  264,  275,
 /*   150 */   263,  276,  241,  319,  277,  232,  278,  231,  280,  228,
 /*   160 */   321,  282,  227,  283,  226,  322,  281,  225,  284,  286,
 /*   170 */   224,  223,  323,  325,  326,  222,  285,  221,  220,  327,
 /*   180 */   266,  219,  328,  233,  318,  218,  234,  217,  230,  235,
 /*   190 */   216,  316,  317,  215,  305,  236,  212,  307,  308,  237,
 /*   200 */   211,  309,  310,  240,  210,  238,  311,  279,  289,
};
#define YY_SZ_ACTTAB (int)(sizeof(yy_action)/sizeof(yy_action[0]))

/* The next table maps tokens into fallback tokens.  If a construct
** like the following:
** 
**      %fallback ID X Y Z.
**
** appears in the grammer, then ID becomes a fallback token for X, Y,
** and Z.  Whenever one of the tokens X, Y, or Z is input to the parser
** but it does not parse, the type of the token is changed to ID and
** the parse is retried before an error is thrown.
*/
#ifdef YYFALLBACK
static const YYCODETYPE yyFallback[] = {
};
#endif /* YYFALLBACK */

/* The following structure represents a single element of the
** parser's stack.  Information stored includes:
**
**   +  The state number for the parser at this level of the stack.
**
**   +  The value of the token stored at this level of the stack.
**      (In other words, the "major" token.)
**
**   +  The semantic value stored at this level of the stack.  This is
**      the information used by the action routines in the grammar.
**      It is sometimes called the "minor" token.
*/
struct yyStackEntry {
  int stateno;       /* The state-number */
  int major;         /* The major token value.  This is the code
                     ** number for the token at this stack level */
  YYMINORTYPE minor; /* The user-supplied minor token value.  This
                     ** is the value of the token  */
};
typedef struct yyStackEntry yyStackEntry;

/* The state of the parser is completely contained in an instance of
** the following structure */
struct yyParser {
  int yyidx;                    /* Index of top element in stack */
  int yyerrcnt;                 /* Shifts left before out of the error */
  CScriptParseARG_SDECL                /* A place to hold %extra_argument */
#if YYSTACKDEPTH<=0
  int yystksz;                  /* Current side of the stack */
  yyStackEntry *yystack;        /* The parser's stack */
#else
  yyStackEntry yystack[YYSTACKDEPTH];  /* The parser's stack */
#endif
};
typedef struct yyParser yyParser;

#ifndef NDEBUG
#include <stdio.h>
static FILE *yyTraceFILE = 0;
static char *yyTracePrompt = 0;
#endif /* NDEBUG */

#ifndef NDEBUG
/* 
** Turn parser tracing on by giving a stream to which to write the trace
** and a prompt to preface each trace message.  Tracing is turned off
** by making either argument NULL 
**
** Inputs:
** <ul>
** <li> A FILE* to which trace output should be written.
**      If NULL, then tracing is turned off.
** <li> A prefix string written at the beginning of every
**      line of trace output.  If NULL, then tracing is
**      turned off.
** </ul>
**
** Outputs:
** None.
*/
void CScriptParseTrace(FILE *TraceFILE, char *zTracePrompt){
  yyTraceFILE = TraceFILE;
  yyTracePrompt = zTracePrompt;
  if( yyTraceFILE==0 ) yyTracePrompt = 0;
  else if( yyTracePrompt==0 ) yyTraceFILE = 0;
}
#endif /* NDEBUG */

#ifndef NDEBUG
/* For tracing shifts, the names of all terminals and nonterminals
** are required.  The following table supplies these names */
static const char *const yyTokenName[] = { 
  "$",             "LOGOR",         "LOGAND",        "BITOR",       
  "BITXOR",        "BITAND",        "EQUALS",        "NEQUALS",     
  "ST",            "SE",            "GT",            "GE",          
  "ADDOP",         "SUBOP",         "MULOP",         "DIVOP",       
  "MODOP",         "ASSIGN",        "ASSADD",        "ASSSUB",      
  "ASSMUL",        "ASSDIV",        "ASSMOD",        "QUESTION",    
  "COLON",         "ADDADD",        "SUBSUB",        "DOT",         
  "LBRACKET",      "RBRACKET",      "IDENTIFIER",    "LPAREN",      
  "RPAREN",        "INTEGER",       "REAL",          "STRING",      
  "TRUE",          "FALSE",         "NULL",          "COMMA",       
  "SEMICOLON",     "LBRACE",        "RBRACE",        "INCLUDE",     
  "RETURN",        "BREAK",         "CONTINUE",      "VAR",         
  "FUNCTION",      "FOR",           "IN",            "LOWER_THAN_ELSE",
  "ELSE",          "IF",            "WHILE",         "SWITCH",      
  "CASE",          "DEFAULT",       "error",         "main",        
  "translation_unit",  "statement_sequence_opt",  "statement_sequence",  "statement",   
  "include_statement",  "expression_statement",  "declaration_statement",  "for_statement",
  "compound_statement",  "if_statement",  "while_statement",  "foreach_statement",
  "return_statement",  "switch_statement",  "break_statement",  "continue_statement",
  "expression",    "assignment_expression",  "expression_opt",  "conditional_expression",
  "binary_expression",  "assignment_operator",  "unary_expression",  "postfix_expression",
  "primary_expression",  "id_expression",  "argument_list",  "literal",     
  "list_literal",  "list_content",  "list_entry",    "argument",    
  "declaration_sequence",  "function_declaration",  "variable_declaration",  "simple_declaration",
  "init_declaration",  "parameter_list",  "function_body",  "parameter",   
  "opt_parameter",  "parameters",    "opt_parameters",  "for_init_statement",
  "switch_body",   "switch_case",   "default_case",  "case_statements",
};
#endif /* NDEBUG */

#ifndef NDEBUG
/* For tracing reduce actions, the names of all rules are required.
*/
static const char *const yyRuleName[] = {
 /*   0 */ "main ::= translation_unit",
 /*   1 */ "translation_unit ::= statement_sequence_opt",
 /*   2 */ "statement_sequence ::= statement",
 /*   3 */ "statement_sequence ::= statement_sequence statement",
 /*   4 */ "statement_sequence_opt ::=",
 /*   5 */ "statement_sequence_opt ::= statement_sequence",
 /*   6 */ "statement ::= include_statement",
 /*   7 */ "statement ::= expression_statement",
 /*   8 */ "statement ::= declaration_statement",
 /*   9 */ "statement ::= for_statement",
 /*  10 */ "statement ::= compound_statement",
 /*  11 */ "statement ::= if_statement",
 /*  12 */ "statement ::= while_statement",
 /*  13 */ "statement ::= foreach_statement",
 /*  14 */ "statement ::= return_statement",
 /*  15 */ "statement ::= switch_statement",
 /*  16 */ "statement ::= break_statement",
 /*  17 */ "statement ::= continue_statement",
 /*  18 */ "statement ::= error",
 /*  19 */ "expression ::= assignment_expression",
 /*  20 */ "expression_opt ::=",
 /*  21 */ "expression_opt ::= expression",
 /*  22 */ "assignment_expression ::= conditional_expression",
 /*  23 */ "assignment_expression ::= binary_expression assignment_operator assignment_expression",
 /*  24 */ "assignment_operator ::= ASSIGN",
 /*  25 */ "assignment_operator ::= ASSADD",
 /*  26 */ "assignment_operator ::= ASSSUB",
 /*  27 */ "assignment_operator ::= ASSMUL",
 /*  28 */ "assignment_operator ::= ASSDIV",
 /*  29 */ "assignment_operator ::= ASSMOD",
 /*  30 */ "conditional_expression ::= binary_expression",
 /*  31 */ "conditional_expression ::= binary_expression QUESTION expression COLON assignment_expression",
 /*  32 */ "binary_expression ::= unary_expression",
 /*  33 */ "binary_expression ::= binary_expression LOGOR binary_expression",
 /*  34 */ "binary_expression ::= binary_expression LOGAND binary_expression",
 /*  35 */ "binary_expression ::= binary_expression BITOR binary_expression",
 /*  36 */ "binary_expression ::= binary_expression BITXOR binary_expression",
 /*  37 */ "binary_expression ::= binary_expression BITAND binary_expression",
 /*  38 */ "binary_expression ::= binary_expression EQUALS binary_expression",
 /*  39 */ "binary_expression ::= binary_expression NEQUALS binary_expression",
 /*  40 */ "binary_expression ::= binary_expression ST binary_expression",
 /*  41 */ "binary_expression ::= binary_expression SE binary_expression",
 /*  42 */ "binary_expression ::= binary_expression GT binary_expression",
 /*  43 */ "binary_expression ::= binary_expression GE binary_expression",
 /*  44 */ "binary_expression ::= binary_expression ADDOP binary_expression",
 /*  45 */ "binary_expression ::= binary_expression SUBOP binary_expression",
 /*  46 */ "binary_expression ::= binary_expression MULOP binary_expression",
 /*  47 */ "binary_expression ::= binary_expression DIVOP binary_expression",
 /*  48 */ "binary_expression ::= binary_expression MODOP binary_expression",
 /*  49 */ "unary_expression ::= postfix_expression",
 /*  50 */ "unary_expression ::= SUBOP unary_expression",
 /*  51 */ "unary_expression ::= ADDADD unary_expression",
 /*  52 */ "unary_expression ::= SUBSUB unary_expression",
 /*  53 */ "postfix_expression ::= primary_expression",
 /*  54 */ "postfix_expression ::= postfix_expression ADDADD",
 /*  55 */ "postfix_expression ::= postfix_expression SUBSUB",
 /*  56 */ "postfix_expression ::= postfix_expression DOT id_expression",
 /*  57 */ "postfix_expression ::= postfix_expression LBRACKET expression RBRACKET",
 /*  58 */ "postfix_expression ::= IDENTIFIER LPAREN RPAREN",
 /*  59 */ "postfix_expression ::= IDENTIFIER LPAREN argument_list RPAREN",
 /*  60 */ "primary_expression ::= literal",
 /*  61 */ "primary_expression ::= id_expression",
 /*  62 */ "primary_expression ::= LPAREN expression RPAREN",
 /*  63 */ "primary_expression ::= list_literal",
 /*  64 */ "literal ::= INTEGER",
 /*  65 */ "literal ::= REAL",
 /*  66 */ "literal ::= STRING",
 /*  67 */ "literal ::= TRUE",
 /*  68 */ "literal ::= FALSE",
 /*  69 */ "literal ::= NULL",
 /*  70 */ "id_expression ::= IDENTIFIER",
 /*  71 */ "list_literal ::= LBRACKET list_content RBRACKET",
 /*  72 */ "list_content ::= list_entry",
 /*  73 */ "list_content ::= list_content COMMA list_entry",
 /*  74 */ "list_entry ::= expression",
 /*  75 */ "argument ::= expression",
 /*  76 */ "argument_list ::= argument",
 /*  77 */ "argument_list ::= argument_list COMMA argument",
 /*  78 */ "expression_statement ::= SEMICOLON",
 /*  79 */ "expression_statement ::= expression SEMICOLON",
 /*  80 */ "compound_statement ::= LBRACE RBRACE",
 /*  81 */ "compound_statement ::= LBRACE statement_sequence RBRACE",
 /*  82 */ "include_statement ::= INCLUDE STRING SEMICOLON",
 /*  83 */ "return_statement ::= RETURN expression SEMICOLON",
 /*  84 */ "return_statement ::= RETURN SEMICOLON",
 /*  85 */ "break_statement ::= BREAK SEMICOLON",
 /*  86 */ "continue_statement ::= CONTINUE SEMICOLON",
 /*  87 */ "declaration_statement ::= declaration_sequence SEMICOLON",
 /*  88 */ "declaration_statement ::= function_declaration",
 /*  89 */ "declaration_sequence ::= VAR variable_declaration",
 /*  90 */ "declaration_sequence ::= declaration_sequence COMMA variable_declaration",
 /*  91 */ "variable_declaration ::= simple_declaration",
 /*  92 */ "variable_declaration ::= init_declaration",
 /*  93 */ "simple_declaration ::= IDENTIFIER",
 /*  94 */ "init_declaration ::= IDENTIFIER ASSIGN expression",
 /*  95 */ "function_declaration ::= FUNCTION IDENTIFIER LPAREN parameter_list RPAREN function_body",
 /*  96 */ "function_declaration ::= FUNCTION IDENTIFIER LPAREN RPAREN function_body",
 /*  97 */ "parameter ::= IDENTIFIER",
 /*  98 */ "opt_parameter ::= IDENTIFIER ASSIGN expression",
 /*  99 */ "parameters ::= parameter",
 /* 100 */ "parameters ::= parameters COMMA parameter",
 /* 101 */ "opt_parameters ::= opt_parameter",
 /* 102 */ "opt_parameters ::= opt_parameters COMMA opt_parameter",
 /* 103 */ "parameter_list ::= parameters",
 /* 104 */ "parameter_list ::= opt_parameters",
 /* 105 */ "parameter_list ::= parameters COMMA opt_parameters",
 /* 106 */ "function_body ::= statement",
 /* 107 */ "for_statement ::= FOR LPAREN for_init_statement expression SEMICOLON expression_opt RPAREN statement",
 /* 108 */ "for_init_statement ::= expression_statement",
 /* 109 */ "for_init_statement ::= declaration_sequence SEMICOLON",
 /* 110 */ "foreach_statement ::= FOR LPAREN id_expression IN expression RPAREN statement",
 /* 111 */ "if_statement ::= IF LPAREN expression RPAREN statement",
 /* 112 */ "if_statement ::= IF LPAREN expression RPAREN statement ELSE statement",
 /* 113 */ "while_statement ::= WHILE LPAREN expression RPAREN statement",
 /* 114 */ "switch_statement ::= SWITCH LPAREN expression RPAREN LBRACE switch_body RBRACE",
 /* 115 */ "switch_body ::=",
 /* 116 */ "switch_body ::= switch_body switch_case",
 /* 117 */ "switch_body ::= switch_body default_case",
 /* 118 */ "switch_case ::= CASE literal COLON case_statements",
 /* 119 */ "default_case ::= DEFAULT COLON case_statements",
 /* 120 */ "case_statements ::= statement_sequence",
};
#endif /* NDEBUG */


#if YYSTACKDEPTH<=0
/*
** Try to increase the size of the parser stack.
*/
static void yyGrowStack(yyParser *p){
  int newSize;
  yyStackEntry *pNew;

  newSize = p->yystksz*2 + 100;
  pNew = realloc(p->yystack, newSize*sizeof(pNew[0]));
  if( pNew ){
    p->yystack = pNew;
    p->yystksz = newSize;
#ifndef NDEBUG
    if( yyTraceFILE ){
      fprintf(yyTraceFILE,"%sStack grows to %d entries!\n",
              yyTracePrompt, p->yystksz);
    }
#endif
  }
}
#endif

/* 
** This function allocates a new parser.
** The only argument is a pointer to a function which works like
** malloc.
**
** Inputs:
** A pointer to the function used to allocate memory.
**
** Outputs:
** A pointer to a parser.  This pointer is used in subsequent calls
** to CScriptParse and CScriptParseFree.
*/
void *CScriptParseAlloc(void *(*mallocProc)(size_t)){
  yyParser *pParser;
  pParser = (yyParser*)(*mallocProc)( (size_t)sizeof(yyParser) );
  if( pParser ){
    pParser->yyidx = -1;
#if YYSTACKDEPTH<=0
    yyGrowStack(pParser);
#endif
  }
  return pParser;
}

/* The following function deletes the value associated with a
** symbol.  The symbol can be either a terminal or nonterminal.
** "yymajor" is the symbol code, and "yypminor" is a pointer to
** the value.
*/
static void yy_destructor(YYCODETYPE yymajor, YYMINORTYPE *yypminor){
  switch( yymajor ){
    /* Here is inserted the actions which take place when a
    ** terminal or non-terminal is destroyed.  This can happen
    ** when the symbol is popped from the stack during a
    ** reduce or during error processing or when a parser is 
    ** being destroyed before it is finished parsing.
    **
    ** Note: during a reduce, the only symbols destroyed are those
    ** which appear on the RHS of the rule, but which are not used
    ** inside the C code.
    */
    default:  break;   /* If no destructor action specified: do nothing */
  }
}

/*
** Pop the parser's stack once.
**
** If there is a destructor routine associated with the token which
** is popped from the stack, then call it.
**
** Return the major token number for the symbol popped.
*/
static int yy_pop_parser_stack(yyParser *pParser){
  YYCODETYPE yymajor;
  yyStackEntry *yytos = &pParser->yystack[pParser->yyidx];

  if( pParser->yyidx<0 ) return 0;
#ifndef NDEBUG
  if( yyTraceFILE && pParser->yyidx>=0 ){
    fprintf(yyTraceFILE,"%sPopping %s\n",
      yyTracePrompt,
      yyTokenName[yytos->major]);
  }
#endif
  yymajor = yytos->major;
  yy_destructor( yymajor, &yytos->minor);
  pParser->yyidx--;
  return yymajor;
}

/* 
** Deallocate and destroy a parser.  Destructors are all called for
** all stack elements before shutting the parser down.
**
** Inputs:
** <ul>
** <li>  A pointer to the parser.  This should be a pointer
**       obtained from CScriptParseAlloc.
** <li>  A pointer to a function used to reclaim memory obtained
**       from malloc.
** </ul>
*/
void CScriptParseFree(
  void *p,                    /* The parser to be deleted */
  void (*freeProc)(void*)     /* Function used to reclaim memory */
){
  yyParser *pParser = (yyParser*)p;
  if( pParser==0 ) return;
  while( pParser->yyidx>=0 ) yy_pop_parser_stack(pParser);
#if YYSTACKDEPTH<=0
  free(pParser->yystack);
#endif
  (*freeProc)((void*)pParser);
}

/*
** Find the appropriate action for a parser given the terminal
** look-ahead token iLookAhead.
**
** If the look-ahead token is YYNOCODE, then check to see if the action is
** independent of the look-ahead.  If it is, return the action, otherwise
** return YY_NO_ACTION.
*/
static int yy_find_shift_action(
  yyParser *pParser,        /* The parser */
  YYCODETYPE iLookAhead     /* The look-ahead token */
){
  int i;
  int stateno = pParser->yystack[pParser->yyidx].stateno;
 
  if( stateno>YY_SHIFT_MAX || (i = yy_shift_ofst[stateno])==YY_SHIFT_USE_DFLT ){
    return yy_default[stateno];
  }
  if( iLookAhead==YYNOCODE ){
    return YY_NO_ACTION;
  }
  i += iLookAhead;
  if( i<0 || i>=YY_SZ_ACTTAB || yy_lookahead[i]!=iLookAhead ){
    if( iLookAhead>0 ){
#ifdef YYFALLBACK
      int iFallback;            /* Fallback token */
      if( iLookAhead<sizeof(yyFallback)/sizeof(yyFallback[0])
             && (iFallback = yyFallback[iLookAhead])!=0 ){
#ifndef NDEBUG
        if( yyTraceFILE ){
          fprintf(yyTraceFILE, "%sFALLBACK %s => %s\n",
             yyTracePrompt, yyTokenName[iLookAhead], yyTokenName[iFallback]);
        }
#endif
        return yy_find_shift_action(pParser, iFallback);
      }
#endif
#ifdef YYWILDCARD
      {
        int j = i - iLookAhead + YYWILDCARD;
        if( j>=0 && j<YY_SZ_ACTTAB && yy_lookahead[j]==YYWILDCARD ){
#ifndef NDEBUG
          if( yyTraceFILE ){
            fprintf(yyTraceFILE, "%sWILDCARD %s => %s\n",
               yyTracePrompt, yyTokenName[iLookAhead], yyTokenName[YYWILDCARD]);
          }
#endif /* NDEBUG */
          return yy_action[j];
        }
      }
#endif /* YYWILDCARD */
    }
    return yy_default[stateno];
  }else{
    return yy_action[i];
  }
}

/*
** Find the appropriate action for a parser given the non-terminal
** look-ahead token iLookAhead.
**
** If the look-ahead token is YYNOCODE, then check to see if the action is
** independent of the look-ahead.  If it is, return the action, otherwise
** return YY_NO_ACTION.
*/
static int yy_find_reduce_action(
  int stateno,              /* Current state number */
  YYCODETYPE iLookAhead     /* The look-ahead token */
){
  int i;
  /* int stateno = pParser->yystack[pParser->yyidx].stateno; */
 
  if( stateno>YY_REDUCE_MAX ||
      (i = yy_reduce_ofst[stateno])==YY_REDUCE_USE_DFLT ){
    return yy_default[stateno];
  }
  if( iLookAhead==YYNOCODE ){
    return YY_NO_ACTION;
  }
  i += iLookAhead;
  if( i<0 || i>=YY_SZ_ACTTAB || yy_lookahead[i]!=iLookAhead ){
    return yy_default[stateno];
  }else{
    return yy_action[i];
  }
}

/*
** The following routine is called if the stack overflows.
*/
static void yyStackOverflow(yyParser *yypParser, YYMINORTYPE *yypMinor){
   CScriptParseARG_FETCH;
   yypParser->yyidx--;
#ifndef NDEBUG
   if( yyTraceFILE ){
     fprintf(yyTraceFILE,"%sStack Overflow!\n",yyTracePrompt);
   }
#endif
   while( yypParser->yyidx>=0 ) yy_pop_parser_stack(yypParser);
   /* Here code is inserted which will execute if the parser
   ** stack every overflows */
   CScriptParseARG_STORE; /* Suppress warning about unused %extra_argument var */
}

/*
** Perform a shift action.
*/
static void yy_shift(
  yyParser *yypParser,          /* The parser to be shifted */
  int yyNewState,               /* The new state to shift in */
  int yyMajor,                  /* The major token to shift in */
  YYMINORTYPE *yypMinor         /* Pointer ot the minor token to shift in */
){
  yyStackEntry *yytos;
  yypParser->yyidx++;
#if YYSTACKDEPTH>0 
  if( yypParser->yyidx>=YYSTACKDEPTH ){
    yyStackOverflow(yypParser, yypMinor);
    return;
  }
#else
  if( yypParser->yyidx>=yypParser->yystksz ){
    yyGrowStack(yypParser);
    if( yypParser->yyidx>=yypParser->yystksz ){
      yyStackOverflow(yypParser, yypMinor);
      return;
    }
  }
#endif
  yytos = &yypParser->yystack[yypParser->yyidx];
  yytos->stateno = yyNewState;
  yytos->major = yyMajor;
  yytos->minor = *yypMinor;
#ifndef NDEBUG
  if( yyTraceFILE && yypParser->yyidx>0 ){
    int i;
    fprintf(yyTraceFILE,"%sShift %d\n",yyTracePrompt,yyNewState);
    fprintf(yyTraceFILE,"%sStack:",yyTracePrompt);
    for(i=1; i<=yypParser->yyidx; i++)
      fprintf(yyTraceFILE," %s",yyTokenName[yypParser->yystack[i].major]);
    fprintf(yyTraceFILE,"\n");
  }
#endif
}

/* The following table contains information about every rule that
** is used during the reduce.
*/
static const struct {
  YYCODETYPE lhs;         /* Symbol on the left-hand side of the rule */
  unsigned char nrhs;     /* Number of right-hand side symbols in the rule */
} yyRuleInfo[] = {
  { 59, 1 },
  { 60, 1 },
  { 62, 1 },
  { 62, 2 },
  { 61, 0 },
  { 61, 1 },
  { 63, 1 },
  { 63, 1 },
  { 63, 1 },
  { 63, 1 },
  { 63, 1 },
  { 63, 1 },
  { 63, 1 },
  { 63, 1 },
  { 63, 1 },
  { 63, 1 },
  { 63, 1 },
  { 63, 1 },
  { 63, 1 },
  { 76, 1 },
  { 78, 0 },
  { 78, 1 },
  { 77, 1 },
  { 77, 3 },
  { 81, 1 },
  { 81, 1 },
  { 81, 1 },
  { 81, 1 },
  { 81, 1 },
  { 81, 1 },
  { 79, 1 },
  { 79, 5 },
  { 80, 1 },
  { 80, 3 },
  { 80, 3 },
  { 80, 3 },
  { 80, 3 },
  { 80, 3 },
  { 80, 3 },
  { 80, 3 },
  { 80, 3 },
  { 80, 3 },
  { 80, 3 },
  { 80, 3 },
  { 80, 3 },
  { 80, 3 },
  { 80, 3 },
  { 80, 3 },
  { 80, 3 },
  { 82, 1 },
  { 82, 2 },
  { 82, 2 },
  { 82, 2 },
  { 83, 1 },
  { 83, 2 },
  { 83, 2 },
  { 83, 3 },
  { 83, 4 },
  { 83, 3 },
  { 83, 4 },
  { 84, 1 },
  { 84, 1 },
  { 84, 3 },
  { 84, 1 },
  { 87, 1 },
  { 87, 1 },
  { 87, 1 },
  { 87, 1 },
  { 87, 1 },
  { 87, 1 },
  { 85, 1 },
  { 88, 3 },
  { 89, 1 },
  { 89, 3 },
  { 90, 1 },
  { 91, 1 },
  { 86, 1 },
  { 86, 3 },
  { 65, 1 },
  { 65, 2 },
  { 68, 2 },
  { 68, 3 },
  { 64, 3 },
  { 72, 3 },
  { 72, 2 },
  { 74, 2 },
  { 75, 2 },
  { 66, 2 },
  { 66, 1 },
  { 92, 2 },
  { 92, 3 },
  { 94, 1 },
  { 94, 1 },
  { 95, 1 },
  { 96, 3 },
  { 93, 6 },
  { 93, 5 },
  { 99, 1 },
  { 100, 3 },
  { 101, 1 },
  { 101, 3 },
  { 102, 1 },
  { 102, 3 },
  { 97, 1 },
  { 97, 1 },
  { 97, 3 },
  { 98, 1 },
  { 67, 8 },
  { 103, 1 },
  { 103, 2 },
  { 71, 7 },
  { 69, 5 },
  { 69, 7 },
  { 70, 5 },
  { 73, 7 },
  { 104, 0 },
  { 104, 2 },
  { 104, 2 },
  { 105, 4 },
  { 106, 3 },
  { 107, 1 },
};

static void yy_accept(yyParser*);  /* Forward Declaration */

/*
** Perform a reduce action and the shift that must immediately
** follow the reduce.
*/
static void yy_reduce(
  yyParser *yypParser,         /* The parser */
  int yyruleno                 /* Number of the rule by which to reduce */
){
  int yygoto;                     /* The next state */
  int yyact;                      /* The next action */
  YYMINORTYPE yygotominor;        /* The LHS of the rule reduced */
  yyStackEntry *yymsp;            /* The top of the parser's stack */
  int yysize;                     /* Amount to pop the stack */
  CScriptParseARG_FETCH;
  yymsp = &yypParser->yystack[yypParser->yyidx];
#ifndef NDEBUG
  if( yyTraceFILE && yyruleno>=0 
        && yyruleno<(int)(sizeof(yyRuleName)/sizeof(yyRuleName[0])) ){
    fprintf(yyTraceFILE, "%sReduce [%s].\n", yyTracePrompt,
      yyRuleName[yyruleno]);
  }
#endif /* NDEBUG */

  /* Silence complaints from purify about yygotominor being uninitialized
  ** in some cases when it is copied into the stack after the following
  ** switch.  yygotominor is uninitialized when a rule reduces that does
  ** not set the value of its left-hand side nonterminal.  Leaving the
  ** value of the nonterminal uninitialized is utterly harmless as long
  ** as the value is never used.  So really the only thing this code
  ** accomplishes is to quieten purify.  
  **
  ** 2007-01-16:  The wireshark project (www.wireshark.org) reports that
  ** without this code, their parser segfaults.  I'm not sure what there
  ** parser is doing to make this happen.  This is the second bug report
  ** from wireshark this week.  Clearly they are stressing Lemon in ways
  ** that it has not been previously stressed...  (SQLite ticket #2172)
  */
  memset(&yygotominor, 0, sizeof(yygotominor));


  switch( yyruleno ){
  /* Beginning here are the reduction cases.  A typical example
  ** follows:
  **   case 0:
  **  #line <lineno> <grammarfile>
  **     { ... }           // User supplied code
  **  #line <lineno> <thisfile>
  **     break;
  */
      case 0:
#line 72 "astgen.in"
{ p->SetRoot(yymsp[0].minor.yy31); }
#line 1118 "astgen.c"
        break;
      case 1:
#line 75 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(translation_unit, yymsp[0].minor.yy31); }
#line 1123 "astgen.c"
        break;
      case 2:
      case 5:
      case 6:
      case 7:
      case 8:
      case 9:
      case 10:
      case 11:
      case 12:
      case 13:
      case 14:
      case 15:
      case 16:
      case 17:
      case 19:
      case 21:
      case 22:
      case 30:
      case 32:
      case 49:
      case 53:
      case 60:
      case 61:
      case 63:
      case 76:
      case 88:
      case 89:
      case 91:
      case 92:
      case 99:
      case 101:
      case 103:
      case 104:
      case 106:
      case 108:
      case 120:
#line 78 "astgen.in"
{ yygotominor.yy31 = yymsp[0].minor.yy31; }
#line 1163 "astgen.c"
        break;
      case 3:
#line 79 "astgen.in"
{ 
  if(yymsp[-1].minor.yy31->m_type == statement_sequence) {
    yygotominor.yy31 = yymsp[-1].minor.yy31;
  }
  else {
    yygotominor.yy31 = p->AllocAst(statement_sequence, new AstList);
    yygotominor.yy31->m_a1.GetList()->push_back(yymsp[-1].minor.yy31);
  }
  yygotominor.yy31->m_a1.GetList()->push_back(yymsp[0].minor.yy31);
}
#line 1177 "astgen.c"
        break;
      case 4:
      case 20:
#line 92 "astgen.in"
{ yygotominor.yy31 = 0; }
#line 1183 "astgen.c"
        break;
      case 18:
      case 78:
#line 110 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(empty_statement); }
#line 1189 "astgen.c"
        break;
      case 23:
#line 126 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(assignment_expression, yymsp[-1].minor.yy198, yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1194 "astgen.c"
        break;
      case 24:
#line 130 "astgen.in"
{ yygotominor.yy198 = op_assign; }
#line 1199 "astgen.c"
        break;
      case 25:
#line 131 "astgen.in"
{ yygotominor.yy198 = op_assadd; }
#line 1204 "astgen.c"
        break;
      case 26:
#line 132 "astgen.in"
{ yygotominor.yy198 = op_asssub; }
#line 1209 "astgen.c"
        break;
      case 27:
#line 133 "astgen.in"
{ yygotominor.yy198 = op_assmul; }
#line 1214 "astgen.c"
        break;
      case 28:
#line 134 "astgen.in"
{ yygotominor.yy198 = op_assdiv; }
#line 1219 "astgen.c"
        break;
      case 29:
#line 135 "astgen.in"
{ yygotominor.yy198 = op_assmod; }
#line 1224 "astgen.c"
        break;
      case 31:
#line 139 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(ternary_expression, yymsp[-4].minor.yy31, yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1229 "astgen.c"
        break;
      case 33:
#line 143 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(binary_expression, op_logor,   yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1234 "astgen.c"
        break;
      case 34:
#line 144 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(binary_expression, op_logand,  yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1239 "astgen.c"
        break;
      case 35:
#line 145 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(binary_expression, op_bitor,   yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1244 "astgen.c"
        break;
      case 36:
#line 146 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(binary_expression, op_bitxor,  yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1249 "astgen.c"
        break;
      case 37:
#line 147 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(binary_expression, op_bitand,  yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1254 "astgen.c"
        break;
      case 38:
#line 148 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(binary_expression, op_eq,   yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1259 "astgen.c"
        break;
      case 39:
#line 149 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(binary_expression, op_ne,   yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1264 "astgen.c"
        break;
      case 40:
#line 150 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(binary_expression, op_lt,   yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1269 "astgen.c"
        break;
      case 41:
#line 151 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(binary_expression, op_le,   yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1274 "astgen.c"
        break;
      case 42:
#line 152 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(binary_expression, op_gt,   yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1279 "astgen.c"
        break;
      case 43:
#line 153 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(binary_expression, op_ge,   yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1284 "astgen.c"
        break;
      case 44:
#line 154 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(binary_expression, op_add,  yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1289 "astgen.c"
        break;
      case 45:
#line 155 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(binary_expression, op_sub,  yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1294 "astgen.c"
        break;
      case 46:
#line 156 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(binary_expression, op_mul,  yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1299 "astgen.c"
        break;
      case 47:
#line 157 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(binary_expression, op_div,  yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1304 "astgen.c"
        break;
      case 48:
#line 158 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(binary_expression, op_mod,  yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1309 "astgen.c"
        break;
      case 50:
#line 162 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(prefix_expression, op_negate, yymsp[0].minor.yy31); }
#line 1314 "astgen.c"
        break;
      case 51:
#line 163 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(prefix_expression, op_preinc, yymsp[0].minor.yy31); }
#line 1319 "astgen.c"
        break;
      case 52:
#line 164 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(prefix_expression, op_predec, yymsp[0].minor.yy31); }
#line 1324 "astgen.c"
        break;
      case 54:
#line 168 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(postfix_expression, op_postinc, yymsp[-1].minor.yy31); }
#line 1329 "astgen.c"
        break;
      case 55:
#line 169 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(postfix_expression, op_postdec, yymsp[-1].minor.yy31); }
#line 1334 "astgen.c"
        break;
      case 56:
#line 170 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(member_expression, yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1339 "astgen.c"
        break;
      case 57:
#line 171 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(index_expression, yymsp[-3].minor.yy31, yymsp[-1].minor.yy31); }
#line 1344 "astgen.c"
        break;
      case 58:
#line 172 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(function_call, String(yymsp[-2].minor.yy0)); }
#line 1349 "astgen.c"
        break;
      case 59:
#line 173 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(function_call, String(yymsp[-3].minor.yy0), yymsp[-1].minor.yy31); }
#line 1354 "astgen.c"
        break;
      case 62:
      case 87:
      case 109:
#line 178 "astgen.in"
{ yygotominor.yy31 = yymsp[-1].minor.yy31; }
#line 1361 "astgen.c"
        break;
      case 64:
#line 182 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(literal, Variant(String(yymsp[0].minor.yy0), Variant::stInt)); }
#line 1366 "astgen.c"
        break;
      case 65:
#line 183 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(literal, Variant(String(yymsp[0].minor.yy0), Variant::stReal)); }
#line 1371 "astgen.c"
        break;
      case 66:
#line 184 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(literal, Variant(String(yymsp[0].minor.yy0), Variant::stString)); }
#line 1376 "astgen.c"
        break;
      case 67:
#line 185 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(literal, Variant(true));    }
#line 1381 "astgen.c"
        break;
      case 68:
#line 186 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(literal, Variant(false));   }
#line 1386 "astgen.c"
        break;
      case 69:
#line 187 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(literal, Variant());        }
#line 1391 "astgen.c"
        break;
      case 70:
#line 190 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(lvalue, String(yymsp[0].minor.yy0)); }
#line 1396 "astgen.c"
        break;
      case 71:
#line 193 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(list_literal, yymsp[-1].minor.yy31); }
#line 1401 "astgen.c"
        break;
      case 72:
#line 195 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(list_content, yymsp[0].minor.yy31); }
#line 1406 "astgen.c"
        break;
      case 73:
#line 196 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(list_content, yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1411 "astgen.c"
        break;
      case 74:
#line 198 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(list_entry, yymsp[0].minor.yy31); }
#line 1416 "astgen.c"
        break;
      case 75:
#line 207 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(argument, yymsp[0].minor.yy31); }
#line 1421 "astgen.c"
        break;
      case 77:
#line 211 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(argument_list, yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1426 "astgen.c"
        break;
      case 79:
#line 220 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(expression_statement, yymsp[-1].minor.yy31); }
#line 1431 "astgen.c"
        break;
      case 80:
#line 223 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(compound_statement); }
#line 1436 "astgen.c"
        break;
      case 81:
#line 224 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(compound_statement, yymsp[-1].minor.yy31); }
#line 1441 "astgen.c"
        break;
      case 82:
#line 227 "astgen.in"
{ p->Parse(yymsp[-1].minor.yy0); yygotominor.yy31 = p->GetRoot(); }
#line 1446 "astgen.c"
        break;
      case 83:
#line 230 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(return_statement, yymsp[-1].minor.yy31); }
#line 1451 "astgen.c"
        break;
      case 84:
#line 231 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(return_statement);    }
#line 1456 "astgen.c"
        break;
      case 85:
#line 234 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(break_statement); }
#line 1461 "astgen.c"
        break;
      case 86:
#line 235 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(continue_statement); }
#line 1466 "astgen.c"
        break;
      case 90:
#line 248 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(declaration_sequence, yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1471 "astgen.c"
        break;
      case 93:
#line 255 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(variable_declaration, String(yymsp[0].minor.yy0)); }
#line 1476 "astgen.c"
        break;
      case 94:
#line 258 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(variable_declaration, String(yymsp[-2].minor.yy0), yymsp[0].minor.yy31); }
#line 1481 "astgen.c"
        break;
      case 95:
#line 266 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(function_declaration, String(yymsp[-4].minor.yy0), yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1486 "astgen.c"
        break;
      case 96:
#line 267 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(function_declaration, String(yymsp[-3].minor.yy0), AstData(), yymsp[0].minor.yy31); }
#line 1491 "astgen.c"
        break;
      case 97:
#line 270 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(parameter, String(yymsp[0].minor.yy0)); }
#line 1496 "astgen.c"
        break;
      case 98:
#line 273 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(parameter, String(yymsp[-2].minor.yy0), yymsp[0].minor.yy31); }
#line 1501 "astgen.c"
        break;
      case 100:
      case 102:
      case 105:
#line 277 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(parameter_list, yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1508 "astgen.c"
        break;
      case 107:
#line 298 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(for_statement, yymsp[-5].minor.yy31, yymsp[-4].minor.yy31, yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1513 "astgen.c"
        break;
      case 110:
#line 309 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(foreach_statement, yymsp[-4].minor.yy31, yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1518 "astgen.c"
        break;
      case 111:
#line 320 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(if_statement, yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1523 "astgen.c"
        break;
      case 112:
#line 321 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(if_statement, yymsp[-4].minor.yy31, yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1528 "astgen.c"
        break;
      case 113:
#line 329 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(while_statement, yymsp[-2].minor.yy31,  yymsp[0].minor.yy31); }
#line 1533 "astgen.c"
        break;
      case 114:
#line 337 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(switch_statement, yymsp[-4].minor.yy31, yymsp[-1].minor.yy127); }
#line 1538 "astgen.c"
        break;
      case 115:
#line 341 "astgen.in"
{ yygotominor.yy127 = new AstList; }
#line 1543 "astgen.c"
        break;
      case 116:
      case 117:
#line 342 "astgen.in"
{ yygotominor.yy127 = yymsp[-1].minor.yy127; yygotominor.yy127->push_back(yymsp[0].minor.yy31); }
#line 1549 "astgen.c"
        break;
      case 118:
#line 346 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(switch_case, yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1554 "astgen.c"
        break;
      case 119:
#line 349 "astgen.in"
{ yygotominor.yy31 = p->AllocAst(default_case, yymsp[0].minor.yy31); }
#line 1559 "astgen.c"
        break;
  };
  yygoto = yyRuleInfo[yyruleno].lhs;
  yysize = yyRuleInfo[yyruleno].nrhs;
  yypParser->yyidx -= yysize;
  yyact = yy_find_reduce_action(yymsp[-yysize].stateno,yygoto);
  if( yyact < YYNSTATE ){
#ifdef NDEBUG
    /* If we are not debugging and the reduce action popped at least
    ** one element off the stack, then we can push the new element back
    ** onto the stack here, and skip the stack overflow test in yy_shift().
    ** That gives a significant speed improvement. */
    if( yysize ){
      yypParser->yyidx++;
      yymsp -= yysize-1;
      yymsp->stateno = yyact;
      yymsp->major = yygoto;
      yymsp->minor = yygotominor;
    }else
#endif
    {
      yy_shift(yypParser,yyact,yygoto,&yygotominor);
    }
  }else if( yyact == YYNSTATE + YYNRULE + 1 ){
    yy_accept(yypParser);
  }
}

/*
** The following code executes when the parse fails
*/
static void yy_parse_failed(
  yyParser *yypParser           /* The parser */
){
  CScriptParseARG_FETCH;
#ifndef NDEBUG
  if( yyTraceFILE ){
    fprintf(yyTraceFILE,"%sFail!\n",yyTracePrompt);
  }
#endif
  while( yypParser->yyidx>=0 ) yy_pop_parser_stack(yypParser);
  /* Here code is inserted which will be executed whenever the
  ** parser fails */
#line 52 "astgen.in"

  p->OnParseFailure();
#line 1607 "astgen.c"
  CScriptParseARG_STORE; /* Suppress warning about unused %extra_argument variable */
}

/*
** The following code executes when a syntax error first occurs.
*/
static void yy_syntax_error(
  yyParser *yypParser,           /* The parser */
  int yymajor,                   /* The major type of the error token */
  YYMINORTYPE yyminor            /* The minor type of the error token */
){
  CScriptParseARG_FETCH;
#define TOKEN (yyminor.yy0)
#line 55 "astgen.in"

  p->OnSyntaxError();
#line 1625 "astgen.c"
  CScriptParseARG_STORE; /* Suppress warning about unused %extra_argument variable */
}

/*
** The following is executed when the parser accepts
*/
static void yy_accept(
  yyParser *yypParser           /* The parser */
){
  CScriptParseARG_FETCH;
#ifndef NDEBUG
  if( yyTraceFILE ){
    fprintf(yyTraceFILE,"%sAccept!\n",yyTracePrompt);
  }
#endif
  while( yypParser->yyidx>=0 ) yy_pop_parser_stack(yypParser);
  /* Here code is inserted which will be executed whenever the
  ** parser accepts */
  CScriptParseARG_STORE; /* Suppress warning about unused %extra_argument variable */
}

/* The main parser program.
** The first argument is a pointer to a structure obtained from
** "CScriptParseAlloc" which describes the current state of the parser.
** The second argument is the major token number.  The third is
** the minor token.  The fourth optional argument is whatever the
** user wants (and specified in the grammar) and is available for
** use by the action routines.
**
** Inputs:
** <ul>
** <li> A pointer to the parser (an opaque structure.)
** <li> The major token number.
** <li> The minor token number.
** <li> An option argument of a grammar-specified type.
** </ul>
**
** Outputs:
** None.
*/
void CScriptParse(
  void *yyp,                   /* The parser */
  int yymajor,                 /* The major token code number */
  CScriptParseTOKENTYPE yyminor       /* The value for the token */
  CScriptParseARG_PDECL               /* Optional %extra_argument parameter */
){
  YYMINORTYPE yyminorunion;
  int yyact;            /* The parser action. */
  int yyendofinput;     /* True if we are at the end of input */
  int yyerrorhit = 0;   /* True if yymajor has invoked an error */
  yyParser *yypParser;  /* The parser */

  /* (re)initialize the parser, if necessary */
  yypParser = (yyParser*)yyp;
  if( yypParser->yyidx<0 ){
#if YYSTACKDEPTH<=0
    if( yypParser->yystksz <=0 ){
      memset(&yyminorunion, 0, sizeof(yyminorunion));
      yyStackOverflow(yypParser, &yyminorunion);
      return;
    }
#endif
    yypParser->yyidx = 0;
    yypParser->yyerrcnt = -1;
    yypParser->yystack[0].stateno = 0;
    yypParser->yystack[0].major = 0;
  }
  yyminorunion.yy0 = yyminor;
  yyendofinput = (yymajor==0);
  CScriptParseARG_STORE;

#ifndef NDEBUG
  if( yyTraceFILE ){
    fprintf(yyTraceFILE,"%sInput %s\n",yyTracePrompt,yyTokenName[yymajor]);
  }
#endif

  do{
    yyact = yy_find_shift_action(yypParser,yymajor);
    if( yyact<YYNSTATE ){
      yy_shift(yypParser,yyact,yymajor,&yyminorunion);
      yypParser->yyerrcnt--;
      if( yyendofinput && yypParser->yyidx>=0 ){
        yymajor = 0;
      }else{
        yymajor = YYNOCODE;
      }
    }else if( yyact < YYNSTATE + YYNRULE ){
      yy_reduce(yypParser,yyact-YYNSTATE);
    }else if( yyact == YY_ERROR_ACTION ){
      int yymx;
#ifndef NDEBUG
      if( yyTraceFILE ){
        fprintf(yyTraceFILE,"%sSyntax Error!\n",yyTracePrompt);
      }
#endif
#ifdef YYERRORSYMBOL
      /* A syntax error has occurred.
      ** The response to an error depends upon whether or not the
      ** grammar defines an error token "ERROR".  
      **
      ** This is what we do if the grammar does define ERROR:
      **
      **  * Call the %syntax_error function.
      **
      **  * Begin popping the stack until we enter a state where
      **    it is legal to shift the error symbol, then shift
      **    the error symbol.
      **
      **  * Set the error count to three.
      **
      **  * Begin accepting and shifting new tokens.  No new error
      **    processing will occur until three tokens have been
      **    shifted successfully.
      **
      */
      if( yypParser->yyerrcnt<0 ){
        yy_syntax_error(yypParser,yymajor,yyminorunion);
      }
      yymx = yypParser->yystack[yypParser->yyidx].major;
      if( yymx==YYERRORSYMBOL || yyerrorhit ){
#ifndef NDEBUG
        if( yyTraceFILE ){
          fprintf(yyTraceFILE,"%sDiscard input token %s\n",
             yyTracePrompt,yyTokenName[yymajor]);
        }
#endif
        yy_destructor(yymajor,&yyminorunion);
        yymajor = YYNOCODE;
      }else{
         while(
          yypParser->yyidx >= 0 &&
          yymx != YYERRORSYMBOL &&
          (yyact = yy_find_reduce_action(
                        yypParser->yystack[yypParser->yyidx].stateno,
                        YYERRORSYMBOL)) >= YYNSTATE
        ){
          yy_pop_parser_stack(yypParser);
        }
        if( yypParser->yyidx < 0 || yymajor==0 ){
          yy_destructor(yymajor,&yyminorunion);
          yy_parse_failed(yypParser);
          yymajor = YYNOCODE;
        }else if( yymx!=YYERRORSYMBOL ){
          YYMINORTYPE u2;
          u2.YYERRSYMDT = 0;
          yy_shift(yypParser,yyact,YYERRORSYMBOL,&u2);
        }
      }
      yypParser->yyerrcnt = 3;
      yyerrorhit = 1;
#else  /* YYERRORSYMBOL is not defined */
      /* This is what we do if the grammar does not define ERROR:
      **
      **  * Report an error message, and throw away the input token.
      **
      **  * If the input token is $, then fail the parse.
      **
      ** As before, subsequent error messages are suppressed until
      ** three input tokens have been successfully shifted.
      */
      if( yypParser->yyerrcnt<=0 ){
        yy_syntax_error(yypParser,yymajor,yyminorunion);
      }
      yypParser->yyerrcnt = 3;
      yy_destructor(yymajor,&yyminorunion);
      if( yyendofinput ){
        yy_parse_failed(yypParser);
      }
      yymajor = YYNOCODE;
#endif
    }else{
      yy_accept(yypParser);
      yymajor = YYNOCODE;
    }
  }while( yymajor!=YYNOCODE && yypParser->yyidx>=0 );
  return;
}

