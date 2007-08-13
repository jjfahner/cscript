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
#include "astlist.h"

#pragma warning(disable:4065)

#line 19 "astgen.c"
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
#define YYNOCODE 114
#define YYACTIONTYPE unsigned short int
#define CScriptParseTOKENTYPE  Token 
typedef union {
  CScriptParseTOKENTYPE yy0;
  opcodes yy38;
  Ast* yy81;
  AstList* yy223;
  int yy227;
} YYMINORTYPE;
#ifndef YYSTACKDEPTH
#define YYSTACKDEPTH 100
#endif
#define CScriptParseARG_SDECL  Parser* p ;
#define CScriptParseARG_PDECL , Parser* p 
#define CScriptParseARG_FETCH  Parser* p  = yypParser->p 
#define CScriptParseARG_STORE yypParser->p  = p 
#define YYNSTATE 222
#define YYNRULE 128
#define YYERRORSYMBOL 60
#define YYERRSYMDT yy227
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
 /*     0 */   193,  351,  220,  218,    7,  217,  213,  212,  209,  208,
 /*    10 */   206,  204,  201,  200,  198,  197,  196,  194,  114,  191,
 /*    20 */   120,  190,   58,  122,  188,   74,  177,  176,  100,  172,
 /*    30 */   171,  169,  151,   25,  193,  130,  112,  110,    9,  217,
 /*    40 */   213,  212,  209,  208,  206,  204,  201,  200,  198,  197,
 /*    50 */   196,  194,  114,  191,   55,  190,   58,  148,  188,   74,
 /*    60 */   177,  176,   57,  172,  171,  169,   44,   45,   35,  130,
 /*    70 */   112,  110,   42,   47,   40,   36,   37,   38,   46,   43,
 /*    80 */    44,   45,   35,  193,   80,  142,  195,    9,  217,  213,
 /*    90 */   212,  209,  208,  206,  204,  201,  200,  198,  197,  196,
 /*   100 */   194,  114,  191,  127,  190,   58,   21,  188,   74,  177,
 /*   110 */   176,  103,  172,  171,  169,    5,  186,  187,  130,  112,
 /*   120 */   110,   40,   36,   37,   38,   46,   43,   44,   45,   35,
 /*   130 */   214,  215,  193,  109,   14,  192,    6,  217,  213,  212,
 /*   140 */   209,  208,  206,  204,  201,  200,  198,  197,  196,  194,
 /*   150 */   114,  191,  126,  190,   58,  146,  188,   74,  177,  176,
 /*   160 */    57,  172,  171,  169,  141,  193,  129,  130,  112,  110,
 /*   170 */   159,  213,  212,  209,  208,  206,  204,  201,  200,  198,
 /*   180 */   197,  196,  194,  114,  191,   96,  190,   58,  161,  188,
 /*   190 */    74,  177,  176,  132,  172,  171,  169,   17,  174,  219,
 /*   200 */   130,  112,  110,  100,  193,   18,   56,  152,  207,  159,
 /*   210 */   213,  212,  209,  208,  206,  204,  201,  200,  198,  197,
 /*   220 */   196,  194,  114,  191,   60,  190,   58,  117,  188,   74,
 /*   230 */   177,  176,  134,  172,  171,  169,   91,   32,   11,  130,
 /*   240 */   112,  110,   10,  193,   13,  103,  124,  158,  202,  213,
 /*   250 */   212,  209,  208,  206,  204,  201,  200,  198,  197,  196,
 /*   260 */   194,  114,  191,   54,  190,   58,   52,  188,   74,  177,
 /*   270 */   176,   62,  172,  171,  169,   61,  193,    4,  130,  112,
 /*   280 */   110,  166,  213,  212,  209,  208,  206,  204,  201,  200,
 /*   290 */   198,  197,  196,  194,  114,  191,  182,  190,   58,  104,
 /*   300 */   188,   74,  177,  176,  205,  172,  171,  169,    8,  193,
 /*   310 */   150,  130,  112,  110,  216,  213,  212,  209,  208,  206,
 /*   320 */   204,  201,  200,  198,  197,  196,  194,  114,  191,  156,
 /*   330 */   190,   58,   12,  188,   74,  177,  176,  170,  172,  171,
 /*   340 */   169,  147,  193,   23,  130,  112,  110,  107,  213,  212,
 /*   350 */   209,  208,  206,  204,  201,  200,  198,  197,  196,  194,
 /*   360 */   114,  191,   19,  190,   58,   31,  188,   74,  177,  176,
 /*   370 */    28,  172,  171,  169,   79,  193,    1,  130,  112,  110,
 /*   380 */   173,  213,  212,  209,  208,  206,  204,  201,  200,  198,
 /*   390 */   197,  196,  194,  114,  191,   24,  190,   58,   15,  188,
 /*   400 */    74,  177,  176,   92,  172,  171,  169,   22,  193,    2,
 /*   410 */   130,  112,  110,  178,  213,  212,  209,  208,  206,  204,
 /*   420 */   201,  200,  198,  197,  196,  194,  114,  191,   53,  190,
 /*   430 */    58,  185,  188,   74,  177,  176,   49,  172,  171,  169,
 /*   440 */   352,  352,  352,  130,  112,  110,   59,   95,   50,   51,
 /*   450 */   352,   94,   16,  352,   26,  352,  168,  167,  165,  164,
 /*   460 */   163,  162,  352,   84,  128,    3,  221,  119,   20,  115,
 /*   470 */   113,   57,   90,  102,  121,   49,  352,  352,   93,   89,
 /*   480 */   116,   82,  352,  211,  215,  105,  111,   50,   51,  352,
 /*   490 */    94,   16,  352,   26,  352,  168,  167,  165,  164,  163,
 /*   500 */   162,  352,   84,  128,    3,  123,  119,   20,  115,  113,
 /*   510 */    57,   90,  102,  121,   49,  352,  352,   93,   89,  116,
 /*   520 */    46,   43,   44,   45,   35,  352,   50,   51,  352,   94,
 /*   530 */    16,  352,   26,  352,  168,  167,  165,  164,  163,  162,
 /*   540 */   352,   84,  128,    3,  352,  119,   20,  115,  113,   57,
 /*   550 */    90,  102,  121,  352,  352,  352,   93,   89,  116,   39,
 /*   560 */    33,   34,   48,   41,   42,   47,   40,   36,   37,   38,
 /*   570 */    46,   43,   44,   45,   35,  145,  144,  143,  139,  138,
 /*   580 */   137,   29,   33,   34,   48,   41,   42,   47,   40,   36,
 /*   590 */    37,   38,   46,   43,   44,   45,   35,   49,  181,   74,
 /*   600 */   177,  176,  352,  172,  171,  169,  352,  352,  352,   50,
 /*   610 */    51,  352,   94,   16,  352,   26,  352,  168,  167,  165,
 /*   620 */   164,  163,  162,   49,   84,  128,  168,  167,  165,  164,
 /*   630 */   163,  162,   57,  352,  352,   50,   51,  352,   94,   16,
 /*   640 */   352,   26,  352,  168,  167,  165,  164,  163,  162,  352,
 /*   650 */    84,  125,   34,   48,   41,   42,   47,   40,   36,   37,
 /*   660 */    38,   46,   43,   44,   45,   35,   49,   75,  352,  188,
 /*   670 */    74,  177,  176,  352,  172,  171,  169,  352,   50,   51,
 /*   680 */   352,   94,   16,  203,   26,  175,  168,  167,  165,  164,
 /*   690 */   163,  162,  352,   84,  114,  191,   49,  190,   58,  352,
 /*   700 */   188,   74,  177,  176,  352,  172,   86,  169,   50,   51,
 /*   710 */   352,   94,   16,  101,   26,  352,  168,  167,  165,  164,
 /*   720 */   163,  162,  352,   84,   27,  157,  191,  352,  190,   58,
 /*   730 */   352,  188,   74,  177,  176,  352,  172,  171,  169,   78,
 /*   740 */   155,  154,  191,  352,  190,   58,  352,  188,   74,  177,
 /*   750 */   176,   77,  172,  171,  169,  352,  352,  149,   48,   41,
 /*   760 */    42,   47,   40,   36,   37,   38,   46,   43,   44,   45,
 /*   770 */    35,   41,   42,   47,   40,   36,   37,   38,   46,   43,
 /*   780 */    44,   45,   35,  352,  154,  191,  352,  190,   58,  352,
 /*   790 */   188,   74,  177,  176,  352,  172,  171,  169,  352,  352,
 /*   800 */   153,  352,  199,  191,   99,  190,   58,  352,  188,   74,
 /*   810 */   177,  176,  352,  172,  171,  169,  157,  191,  352,  190,
 /*   820 */    58,  352,  188,   74,  177,  176,  352,  172,  171,  169,
 /*   830 */   352,  160,  352,  118,  191,  352,  190,   58,  352,  188,
 /*   840 */    74,  177,  176,  352,  172,  171,  169,   88,  191,  352,
 /*   850 */   190,   58,  352,  188,   74,  177,  176,  352,  172,  171,
 /*   860 */   169,  108,  191,  352,  190,   58,  352,  188,   74,  177,
 /*   870 */   176,  352,  172,  171,  169,   83,  191,  352,  190,   58,
 /*   880 */   352,  188,   74,  177,  176,  352,  172,  171,  169,  106,
 /*   890 */   191,  352,  190,   58,  352,  188,   74,  177,  176,  352,
 /*   900 */   172,  171,  169,  210,  191,  352,  190,   58,  352,  188,
 /*   910 */    74,  177,  176,  352,  172,  171,  169,  352,   98,  191,
 /*   920 */   352,  190,   58,  352,  188,   74,  177,  176,  352,  172,
 /*   930 */   171,  169,   85,  191,  352,  190,   58,  352,  188,   74,
 /*   940 */   177,  176,  352,  172,  171,  169,   87,  191,  352,  190,
 /*   950 */    58,  352,  188,   74,  177,  176,  352,  172,  171,  169,
 /*   960 */    97,  191,  352,  190,   58,  352,  188,   74,  177,  176,
 /*   970 */   352,  172,  171,  169,  140,  191,  352,  190,   58,  352,
 /*   980 */   188,   74,  177,  176,  352,  172,  171,  169,  352,  189,
 /*   990 */   352,  190,   58,  352,  188,   74,  177,  176,  352,  172,
 /*  1000 */   171,  169,  136,  352,  190,   58,  352,  188,   74,  177,
 /*  1010 */   176,  352,  172,  171,  169,   67,  352,  188,   74,  177,
 /*  1020 */   176,  352,  172,  171,  169,  352,   68,  352,  188,   74,
 /*  1030 */   177,  176,  352,  172,  171,  169,   69,  352,  188,   74,
 /*  1040 */   177,  176,  352,  172,  171,  169,  133,  352,  188,   74,
 /*  1050 */   177,  176,  352,  172,  171,  169,   70,  352,  188,   74,
 /*  1060 */   177,  176,  352,  172,  171,  169,  131,  352,  188,   74,
 /*  1070 */   177,  176,  352,  172,  171,  169,  352,   76,  352,  188,
 /*  1080 */    74,  177,  176,  352,  172,  171,  169,   73,  352,  188,
 /*  1090 */    74,  177,  176,  352,  172,  171,  169,   63,  352,  188,
 /*  1100 */    74,  177,  176,  352,  172,  171,  169,   65,  352,  188,
 /*  1110 */    74,  177,  176,  352,  172,  171,  169,   71,  352,  188,
 /*  1120 */    74,  177,  176,  352,  172,  171,  169,   66,  352,  188,
 /*  1130 */    74,  177,  176,  352,  172,  171,  169,  135,  352,  188,
 /*  1140 */    74,  177,  176,  352,  172,  171,  169,   64,  352,  188,
 /*  1150 */    74,  177,  176,  352,  172,  171,  169,   72,  352,  188,
 /*  1160 */    74,  177,  176,  352,  172,  171,  169,  180,   74,  177,
 /*  1170 */   176,  352,  172,  171,  169,  179,   74,  177,  176,  352,
 /*  1180 */   172,  171,  169,  184,  183,   81,  352,   30,
};
static const YYCODETYPE yy_lookahead[] = {
 /*     0 */    60,   61,   62,   63,   64,   65,   66,   67,   68,   69,
 /*    10 */    70,   71,   72,   73,   74,   75,   76,   77,   78,   79,
 /*    20 */    35,   81,   82,   41,   84,   85,   86,   87,   97,   89,
 /*    30 */    90,   91,  101,   17,   60,   95,   96,   97,   64,   65,
 /*    40 */    66,   67,   68,   69,   70,   71,   72,   73,   74,   75,
 /*    50 */    76,   77,   78,   79,  109,   81,   82,   43,   84,   85,
 /*    60 */    86,   87,   48,   89,   90,   91,   14,   15,   16,   95,
 /*    70 */    96,   97,    6,    7,    8,    9,   10,   11,   12,   13,
 /*    80 */    14,   15,   16,   60,   98,   99,  112,   64,   65,   66,
 /*    90 */    67,   68,   69,   70,   71,   72,   73,   74,   75,   76,
 /*   100 */    77,   78,   79,   41,   81,   82,   31,   84,   85,   86,
 /*   110 */    87,   28,   89,   90,   91,   32,  110,  111,   95,   96,
 /*   120 */    97,    8,    9,   10,   11,   12,   13,   14,   15,   16,
 /*   130 */   104,  105,   60,  107,   31,  112,   64,   65,   66,   67,
 /*   140 */    68,   69,   70,   71,   72,   73,   74,   75,   76,   77,
 /*   150 */    78,   79,   41,   81,   82,   43,   84,   85,   86,   87,
 /*   160 */    48,   89,   90,   91,   99,   60,   41,   95,   96,   97,
 /*   170 */    65,   66,   67,   68,   69,   70,   71,   72,   73,   74,
 /*   180 */    75,   76,   77,   78,   79,   28,   81,   82,   30,   84,
 /*   190 */    85,   86,   87,   41,   89,   90,   91,   39,   32,  105,
 /*   200 */    95,   96,   97,   97,   60,   39,  100,  101,  103,   65,
 /*   210 */    66,   67,   68,   69,   70,   71,   72,   73,   74,   75,
 /*   220 */    76,   77,   78,   79,   39,   81,   82,   28,   84,   85,
 /*   230 */    86,   87,   41,   89,   90,   91,   89,   83,   32,   95,
 /*   240 */    96,   97,   54,   60,   32,   28,   41,  103,   65,   66,
 /*   250 */    67,   68,   69,   70,   71,   72,   73,   74,   75,   76,
 /*   260 */    77,   78,   79,   39,   81,   82,   31,   84,   85,   86,
 /*   270 */    87,   42,   89,   90,   91,   39,   60,   32,   95,   96,
 /*   280 */    97,   65,   66,   67,   68,   69,   70,   71,   72,   73,
 /*   290 */    74,   75,   76,   77,   78,   79,   28,   81,   82,   28,
 /*   300 */    84,   85,   86,   87,   41,   89,   90,   91,   32,   60,
 /*   310 */    41,   95,   96,   97,   65,   66,   67,   68,   69,   70,
 /*   320 */    71,   72,   73,   74,   75,   76,   77,   78,   79,   28,
 /*   330 */    81,   82,   32,   84,   85,   86,   87,   32,   89,   90,
 /*   340 */    91,   30,   60,   52,   95,   96,   97,   65,   66,   67,
 /*   350 */    68,   69,   70,   71,   72,   73,   74,   75,   76,   77,
 /*   360 */    78,   79,   41,   81,   82,   24,   84,   85,   86,   87,
 /*   370 */    17,   89,   90,   91,   32,   60,   24,   95,   96,   97,
 /*   380 */    65,   66,   67,   68,   69,   70,   71,   72,   73,   74,
 /*   390 */    75,   76,   77,   78,   79,   31,   81,   82,   31,   84,
 /*   400 */    85,   86,   87,   28,   89,   90,   91,   31,   60,   24,
 /*   410 */    95,   96,   97,   65,   66,   67,   68,   69,   70,   71,
 /*   420 */    72,   73,   74,   75,   76,   77,   78,   79,   42,   81,
 /*   430 */    82,   43,   84,   85,   86,   87,   13,   89,   90,   91,
 /*   440 */   113,  113,  113,   95,   96,   97,   58,   59,   25,   26,
 /*   450 */   113,   28,   29,  113,   31,  113,   33,   34,   35,   36,
 /*   460 */    37,   38,  113,   40,   41,   42,   43,   44,   45,   46,
 /*   470 */    47,   48,   49,   50,   51,   13,  113,  113,   55,   56,
 /*   480 */    57,  102,  113,  104,  105,  106,  107,   25,   26,  113,
 /*   490 */    28,   29,  113,   31,  113,   33,   34,   35,   36,   37,
 /*   500 */    38,  113,   40,   41,   42,   43,   44,   45,   46,   47,
 /*   510 */    48,   49,   50,   51,   13,  113,  113,   55,   56,   57,
 /*   520 */    12,   13,   14,   15,   16,  113,   25,   26,  113,   28,
 /*   530 */    29,  113,   31,  113,   33,   34,   35,   36,   37,   38,
 /*   540 */   113,   40,   41,   42,  113,   44,   45,   46,   47,   48,
 /*   550 */    49,   50,   51,  113,  113,  113,   55,   56,   57,    1,
 /*   560 */     2,    3,    4,    5,    6,    7,    8,    9,   10,   11,
 /*   570 */    12,   13,   14,   15,   16,   17,   18,   19,   20,   21,
 /*   580 */    22,   23,    2,    3,    4,    5,    6,    7,    8,    9,
 /*   590 */    10,   11,   12,   13,   14,   15,   16,   13,   84,   85,
 /*   600 */    86,   87,  113,   89,   90,   91,  113,  113,  113,   25,
 /*   610 */    26,  113,   28,   29,  113,   31,  113,   33,   34,   35,
 /*   620 */    36,   37,   38,   13,   40,   41,   33,   34,   35,   36,
 /*   630 */    37,   38,   48,  113,  113,   25,   26,  113,   28,   29,
 /*   640 */   113,   31,  113,   33,   34,   35,   36,   37,   38,  113,
 /*   650 */    40,   41,    3,    4,    5,    6,    7,    8,    9,   10,
 /*   660 */    11,   12,   13,   14,   15,   16,   13,   82,  113,   84,
 /*   670 */    85,   86,   87,  113,   89,   90,   91,  113,   25,   26,
 /*   680 */   113,   28,   29,   67,   31,   32,   33,   34,   35,   36,
 /*   690 */    37,   38,  113,   40,   78,   79,   13,   81,   82,  113,
 /*   700 */    84,   85,   86,   87,  113,   89,   90,   91,   25,   26,
 /*   710 */   113,   28,   29,   97,   31,  113,   33,   34,   35,   36,
 /*   720 */    37,   38,  113,   40,  108,   78,   79,  113,   81,   82,
 /*   730 */   113,   84,   85,   86,   87,  113,   89,   90,   91,   92,
 /*   740 */    93,   78,   79,  113,   81,   82,  113,   84,   85,   86,
 /*   750 */    87,   88,   89,   90,   91,  113,  113,   94,    4,    5,
 /*   760 */     6,    7,    8,    9,   10,   11,   12,   13,   14,   15,
 /*   770 */    16,    5,    6,    7,    8,    9,   10,   11,   12,   13,
 /*   780 */    14,   15,   16,  113,   78,   79,  113,   81,   82,  113,
 /*   790 */    84,   85,   86,   87,  113,   89,   90,   91,  113,  113,
 /*   800 */    94,  113,   78,   79,   80,   81,   82,  113,   84,   85,
 /*   810 */    86,   87,  113,   89,   90,   91,   78,   79,  113,   81,
 /*   820 */    82,  113,   84,   85,   86,   87,  113,   89,   90,   91,
 /*   830 */   113,   93,  113,   78,   79,  113,   81,   82,  113,   84,
 /*   840 */    85,   86,   87,  113,   89,   90,   91,   78,   79,  113,
 /*   850 */    81,   82,  113,   84,   85,   86,   87,  113,   89,   90,
 /*   860 */    91,   78,   79,  113,   81,   82,  113,   84,   85,   86,
 /*   870 */    87,  113,   89,   90,   91,   78,   79,  113,   81,   82,
 /*   880 */   113,   84,   85,   86,   87,  113,   89,   90,   91,   78,
 /*   890 */    79,  113,   81,   82,  113,   84,   85,   86,   87,  113,
 /*   900 */    89,   90,   91,   78,   79,  113,   81,   82,  113,   84,
 /*   910 */    85,   86,   87,  113,   89,   90,   91,  113,   78,   79,
 /*   920 */   113,   81,   82,  113,   84,   85,   86,   87,  113,   89,
 /*   930 */    90,   91,   78,   79,  113,   81,   82,  113,   84,   85,
 /*   940 */    86,   87,  113,   89,   90,   91,   78,   79,  113,   81,
 /*   950 */    82,  113,   84,   85,   86,   87,  113,   89,   90,   91,
 /*   960 */    78,   79,  113,   81,   82,  113,   84,   85,   86,   87,
 /*   970 */   113,   89,   90,   91,   78,   79,  113,   81,   82,  113,
 /*   980 */    84,   85,   86,   87,  113,   89,   90,   91,  113,   79,
 /*   990 */   113,   81,   82,  113,   84,   85,   86,   87,  113,   89,
 /*  1000 */    90,   91,   79,  113,   81,   82,  113,   84,   85,   86,
 /*  1010 */    87,  113,   89,   90,   91,   82,  113,   84,   85,   86,
 /*  1020 */    87,  113,   89,   90,   91,  113,   82,  113,   84,   85,
 /*  1030 */    86,   87,  113,   89,   90,   91,   82,  113,   84,   85,
 /*  1040 */    86,   87,  113,   89,   90,   91,   82,  113,   84,   85,
 /*  1050 */    86,   87,  113,   89,   90,   91,   82,  113,   84,   85,
 /*  1060 */    86,   87,  113,   89,   90,   91,   82,  113,   84,   85,
 /*  1070 */    86,   87,  113,   89,   90,   91,  113,   82,  113,   84,
 /*  1080 */    85,   86,   87,  113,   89,   90,   91,   82,  113,   84,
 /*  1090 */    85,   86,   87,  113,   89,   90,   91,   82,  113,   84,
 /*  1100 */    85,   86,   87,  113,   89,   90,   91,   82,  113,   84,
 /*  1110 */    85,   86,   87,  113,   89,   90,   91,   82,  113,   84,
 /*  1120 */    85,   86,   87,  113,   89,   90,   91,   82,  113,   84,
 /*  1130 */    85,   86,   87,  113,   89,   90,   91,   82,  113,   84,
 /*  1140 */    85,   86,   87,  113,   89,   90,   91,   82,  113,   84,
 /*  1150 */    85,   86,   87,  113,   89,   90,   91,   82,  113,   84,
 /*  1160 */    85,   86,   87,  113,   89,   90,   91,   84,   85,   86,
 /*  1170 */    87,  113,   89,   90,   91,   84,   85,   86,   87,  113,
 /*  1180 */    89,   90,   91,   25,   26,   27,  113,   29,
};
#define YY_SHIFT_USE_DFLT (-19)
#define YY_SHIFT_MAX 121
static const short yy_shift_ofst[] = {
 /*     0 */   501,  501,  501,  423,  501,  501,  462,  501,  501,  501,
 /*    10 */   501,  501,  501,  501,  584,  653,  683,  683,  683,  683,
 /*    20 */   610,  683,  683,  683,  683,  683,  683,  683,  683,  683,
 /*    30 */   683,  683,  683,  683,  683,  683,  683,  683,  683,  683,
 /*    40 */   683,  683,  683,  683,  683,  683,  683,  683,  683,  683,
 /*    50 */   683,  683,   83,  112,  217,  388,   14,  157,  558,  593,
 /*    60 */   199,  157,  -19,  580,  649,  754,  766,   66,  113,  113,
 /*    70 */   508,  508,  508,  508, 1158,   52,   52,  166,  158,  229,
 /*    80 */   236,  268,  245,  276,  301,  305,  291,  341,  342,  364,
 /*    90 */   375,  385,  386,  376,  367,  352,  353,  321,  311,  300,
 /*   100 */   269,  263,  271,   16,  235,  224,  212,  188,  206,  185,
 /*   110 */   191,  185,  152,  125,  111,   62,   75,   16,  205,  -15,
 /*   120 */   -18,  103,
};
#define YY_REDUCE_USE_DFLT (-70)
#define YY_REDUCE_MAX 62
static const short yy_reduce_ofst[] = {
 /*     0 */   -60,  -26,   23,   72,  144,  105,  249,  249,  216,  249,
 /*    10 */   315,  282,  183,  348,  616,  663,  647,  738,  706,  724,
 /*    20 */   755,  769,  783,  797,  811,  825,  854,  882,  896,  868,
 /*    30 */   840,  923,  910, 1065, 1025,  984, 1075, 1005, 1035, 1015,
 /*    40 */   974,  933,  954,  995, 1055,  964,  585,  944, 1045,  514,
 /*    50 */  1083, 1091,  379,  106,   26,    6,  -69,  -14,  154,  147,
 /*    60 */    94,   65,  -55,
};
static const YYACTIONTYPE yy_default[] = {
 /*     0 */   226,  350,  350,  350,  350,  350,  350,  227,  350,  349,
 /*    10 */   350,  350,  350,  350,  350,  350,  350,  350,  350,  242,
 /*    20 */   350,  350,  350,  350,  350,  350,  350,  350,  350,  350,
 /*    30 */   350,  350,  350,  350,  350,  350,  350,  350,  350,  350,
 /*    40 */   350,  350,  350,  350,  350,  350,  350,  350,  350,  350,
 /*    50 */   350,  350,  350,  350,  350,  350,  350,  350,  252,  350,
 /*    60 */   350,  350,  344,  255,  256,  257,  258,  259,  261,  260,
 /*    70 */   262,  265,  263,  264,  271,  266,  267,  350,  350,  350,
 /*    80 */   314,  350,  350,  350,  350,  350,  284,  350,  350,  350,
 /*    90 */   350,  350,  350,  350,  293,  350,  315,  350,  350,  350,
 /*   100 */   350,  350,  350,  326,  350,  332,  350,  340,  350,  334,
 /*   110 */   350,  333,  350,  350,  350,  350,  350,  350,  350,  350,
 /*   120 */   350,  350,  306,  305,  307,  308,  303,  309,  302,  310,
 /*   130 */   311,  270,  312,  269,  313,  268,  253,  251,  250,  249,
 /*   140 */   316,  318,  317,  248,  247,  246,  319,  280,  320,  300,
 /*   150 */   321,  323,  322,  301,  299,  295,  298,  297,  324,  335,
 /*   160 */   296,  294,  292,  291,  290,  289,  339,  288,  287,  286,
 /*   170 */   285,  284,  283,  341,  282,  281,  276,  275,  342,  274,
 /*   180 */   273,  272,  279,  278,  277,  343,  345,  346,  254,  245,
 /*   190 */   244,  241,  347,  240,  239,  348,  238,  237,  236,  243,
 /*   200 */   235,  234,  336,  337,  233,  338,  232,  325,  231,  230,
 /*   210 */   327,  328,  229,  228,  329,  330,  225,  224,  223,  331,
 /*   220 */   222,  304,
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
  "IDENTIFIER",    "LBRACKET",      "RBRACKET",      "LPAREN",      
  "RPAREN",        "INTEGER",       "REAL",          "STRING",      
  "TRUE",          "FALSE",         "NULL",          "COMMA",       
  "NEW",           "SEMICOLON",     "LBRACE",        "RBRACE",      
  "INCLUDE",       "RETURN",        "BREAK",         "CONTINUE",    
  "VAR",           "STRUCT",        "FUNCTION",      "FOR",         
  "IN",            "LOWER_THAN_ELSE",  "ELSE",          "IF",          
  "WHILE",         "SWITCH",        "CASE",          "DEFAULT",     
  "error",         "main",          "translation_unit",  "statement_sequence_opt",
  "statement_sequence",  "statement",     "include_statement",  "expression_statement",
  "declaration_statement",  "for_statement",  "compound_statement",  "if_statement",
  "while_statement",  "foreach_statement",  "return_statement",  "switch_statement",
  "break_statement",  "continue_statement",  "expression",    "assignment_expression",
  "expression_opt",  "conditional_expression",  "binary_expression",  "assignment_operator",
  "unary_expression",  "postfix_expression",  "new_expression",  "primary_expression",
  "argument_list",  "literal",       "id_expression",  "list_literal",
  "list_content",  "list_entry",    "argument",      "function_declaration",
  "struct_declaration",  "variable_declaration",  "declarator_sequence",  "declarator",  
  "struct_members",  "struct_member",  "parameter_list",  "function_body",
  "parameter",     "opt_parameter",  "parameters",    "opt_parameters",
  "for_init_statement",  "switch_body",   "switch_case",   "default_case",
  "case_statements",
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
 /*  53 */ "unary_expression ::= new_expression",
 /*  54 */ "postfix_expression ::= primary_expression",
 /*  55 */ "postfix_expression ::= postfix_expression ADDADD",
 /*  56 */ "postfix_expression ::= postfix_expression SUBSUB",
 /*  57 */ "postfix_expression ::= postfix_expression DOT IDENTIFIER",
 /*  58 */ "postfix_expression ::= postfix_expression LBRACKET expression RBRACKET",
 /*  59 */ "postfix_expression ::= IDENTIFIER LPAREN RPAREN",
 /*  60 */ "postfix_expression ::= IDENTIFIER LPAREN argument_list RPAREN",
 /*  61 */ "primary_expression ::= literal",
 /*  62 */ "primary_expression ::= id_expression",
 /*  63 */ "primary_expression ::= LPAREN expression RPAREN",
 /*  64 */ "primary_expression ::= list_literal",
 /*  65 */ "literal ::= INTEGER",
 /*  66 */ "literal ::= REAL",
 /*  67 */ "literal ::= STRING",
 /*  68 */ "literal ::= TRUE",
 /*  69 */ "literal ::= FALSE",
 /*  70 */ "literal ::= NULL",
 /*  71 */ "id_expression ::= IDENTIFIER",
 /*  72 */ "list_literal ::= LBRACKET list_content RBRACKET",
 /*  73 */ "list_content ::= list_entry",
 /*  74 */ "list_content ::= list_content COMMA list_entry",
 /*  75 */ "list_entry ::= expression",
 /*  76 */ "new_expression ::= NEW IDENTIFIER",
 /*  77 */ "argument ::= expression",
 /*  78 */ "argument_list ::= argument",
 /*  79 */ "argument_list ::= argument_list COMMA argument",
 /*  80 */ "expression_statement ::= SEMICOLON",
 /*  81 */ "expression_statement ::= expression SEMICOLON",
 /*  82 */ "compound_statement ::= LBRACE RBRACE",
 /*  83 */ "compound_statement ::= LBRACE statement_sequence RBRACE",
 /*  84 */ "include_statement ::= INCLUDE STRING SEMICOLON",
 /*  85 */ "return_statement ::= RETURN expression SEMICOLON",
 /*  86 */ "return_statement ::= RETURN SEMICOLON",
 /*  87 */ "break_statement ::= BREAK SEMICOLON",
 /*  88 */ "continue_statement ::= CONTINUE SEMICOLON",
 /*  89 */ "declaration_statement ::= function_declaration",
 /*  90 */ "declaration_statement ::= struct_declaration SEMICOLON",
 /*  91 */ "declaration_statement ::= variable_declaration SEMICOLON",
 /*  92 */ "variable_declaration ::= VAR declarator_sequence",
 /*  93 */ "declarator ::= IDENTIFIER",
 /*  94 */ "declarator ::= IDENTIFIER ASSIGN expression",
 /*  95 */ "declarator_sequence ::= declarator",
 /*  96 */ "declarator_sequence ::= declarator_sequence COMMA declarator",
 /*  97 */ "struct_declaration ::= STRUCT IDENTIFIER LBRACE RBRACE",
 /*  98 */ "struct_declaration ::= STRUCT IDENTIFIER LBRACE struct_members RBRACE",
 /*  99 */ "struct_member ::= variable_declaration SEMICOLON",
 /* 100 */ "struct_members ::= struct_member",
 /* 101 */ "struct_members ::= struct_members struct_member",
 /* 102 */ "function_declaration ::= FUNCTION IDENTIFIER LPAREN parameter_list RPAREN function_body",
 /* 103 */ "function_declaration ::= FUNCTION IDENTIFIER LPAREN RPAREN function_body",
 /* 104 */ "parameter ::= IDENTIFIER",
 /* 105 */ "opt_parameter ::= IDENTIFIER ASSIGN expression",
 /* 106 */ "parameters ::= parameter",
 /* 107 */ "parameters ::= parameters COMMA parameter",
 /* 108 */ "opt_parameters ::= opt_parameter",
 /* 109 */ "opt_parameters ::= opt_parameters COMMA opt_parameter",
 /* 110 */ "parameter_list ::= parameters",
 /* 111 */ "parameter_list ::= opt_parameters",
 /* 112 */ "parameter_list ::= parameters COMMA opt_parameters",
 /* 113 */ "function_body ::= statement",
 /* 114 */ "for_statement ::= FOR LPAREN for_init_statement expression SEMICOLON expression_opt RPAREN statement",
 /* 115 */ "for_init_statement ::= expression_statement",
 /* 116 */ "for_init_statement ::= variable_declaration SEMICOLON",
 /* 117 */ "foreach_statement ::= FOR LPAREN id_expression IN expression RPAREN statement",
 /* 118 */ "if_statement ::= IF LPAREN expression RPAREN statement",
 /* 119 */ "if_statement ::= IF LPAREN expression RPAREN statement ELSE statement",
 /* 120 */ "while_statement ::= WHILE LPAREN expression RPAREN statement",
 /* 121 */ "switch_statement ::= SWITCH LPAREN expression RPAREN LBRACE switch_body RBRACE",
 /* 122 */ "switch_body ::=",
 /* 123 */ "switch_body ::= switch_body switch_case",
 /* 124 */ "switch_body ::= switch_body default_case",
 /* 125 */ "switch_case ::= CASE literal COLON case_statements",
 /* 126 */ "default_case ::= DEFAULT COLON case_statements",
 /* 127 */ "case_statements ::= statement_sequence",
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
  { 61, 1 },
  { 62, 1 },
  { 64, 1 },
  { 64, 2 },
  { 63, 0 },
  { 63, 1 },
  { 65, 1 },
  { 65, 1 },
  { 65, 1 },
  { 65, 1 },
  { 65, 1 },
  { 65, 1 },
  { 65, 1 },
  { 65, 1 },
  { 65, 1 },
  { 65, 1 },
  { 65, 1 },
  { 65, 1 },
  { 65, 1 },
  { 78, 1 },
  { 80, 0 },
  { 80, 1 },
  { 79, 1 },
  { 79, 3 },
  { 83, 1 },
  { 83, 1 },
  { 83, 1 },
  { 83, 1 },
  { 83, 1 },
  { 83, 1 },
  { 81, 1 },
  { 81, 5 },
  { 82, 1 },
  { 82, 3 },
  { 82, 3 },
  { 82, 3 },
  { 82, 3 },
  { 82, 3 },
  { 82, 3 },
  { 82, 3 },
  { 82, 3 },
  { 82, 3 },
  { 82, 3 },
  { 82, 3 },
  { 82, 3 },
  { 82, 3 },
  { 82, 3 },
  { 82, 3 },
  { 82, 3 },
  { 84, 1 },
  { 84, 2 },
  { 84, 2 },
  { 84, 2 },
  { 84, 1 },
  { 85, 1 },
  { 85, 2 },
  { 85, 2 },
  { 85, 3 },
  { 85, 4 },
  { 85, 3 },
  { 85, 4 },
  { 87, 1 },
  { 87, 1 },
  { 87, 3 },
  { 87, 1 },
  { 89, 1 },
  { 89, 1 },
  { 89, 1 },
  { 89, 1 },
  { 89, 1 },
  { 89, 1 },
  { 90, 1 },
  { 91, 3 },
  { 92, 1 },
  { 92, 3 },
  { 93, 1 },
  { 86, 2 },
  { 94, 1 },
  { 88, 1 },
  { 88, 3 },
  { 67, 1 },
  { 67, 2 },
  { 70, 2 },
  { 70, 3 },
  { 66, 3 },
  { 74, 3 },
  { 74, 2 },
  { 76, 2 },
  { 77, 2 },
  { 68, 1 },
  { 68, 2 },
  { 68, 2 },
  { 97, 2 },
  { 99, 1 },
  { 99, 3 },
  { 98, 1 },
  { 98, 3 },
  { 96, 4 },
  { 96, 5 },
  { 101, 2 },
  { 100, 1 },
  { 100, 2 },
  { 95, 6 },
  { 95, 5 },
  { 104, 1 },
  { 105, 3 },
  { 106, 1 },
  { 106, 3 },
  { 107, 1 },
  { 107, 3 },
  { 102, 1 },
  { 102, 1 },
  { 102, 3 },
  { 103, 1 },
  { 69, 8 },
  { 108, 1 },
  { 108, 2 },
  { 73, 7 },
  { 71, 5 },
  { 71, 7 },
  { 72, 5 },
  { 75, 7 },
  { 109, 0 },
  { 109, 2 },
  { 109, 2 },
  { 110, 4 },
  { 111, 3 },
  { 112, 1 },
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
#line 73 "astgen.in"
{ p->SetRoot(yymsp[0].minor.yy81); }
#line 1150 "astgen.c"
        break;
      case 1:
#line 76 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(translation_unit, yymsp[0].minor.yy81); }
#line 1155 "astgen.c"
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
      case 54:
      case 61:
      case 62:
      case 64:
      case 78:
      case 89:
      case 92:
      case 95:
      case 100:
      case 106:
      case 108:
      case 110:
      case 111:
      case 113:
      case 115:
      case 127:
#line 79 "astgen.in"
{ yygotominor.yy81 = yymsp[0].minor.yy81; }
#line 1196 "astgen.c"
        break;
      case 3:
#line 80 "astgen.in"
{ 
  if(yymsp[-1].minor.yy81->m_type == statement_sequence) {
    yygotominor.yy81 = yymsp[-1].minor.yy81;
  }
  else {
    yygotominor.yy81 = p->AllocAst(statement_sequence, new AstList);
    yygotominor.yy81->m_a1.GetList()->push_back(yymsp[-1].minor.yy81);
  }
  yygotominor.yy81->m_a1.GetList()->push_back(yymsp[0].minor.yy81);
}
#line 1210 "astgen.c"
        break;
      case 4:
      case 20:
#line 93 "astgen.in"
{ yygotominor.yy81 = 0; }
#line 1216 "astgen.c"
        break;
      case 18:
      case 80:
#line 111 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(empty_statement); }
#line 1222 "astgen.c"
        break;
      case 23:
#line 127 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(assignment_expression, yymsp[-1].minor.yy38, yymsp[-2].minor.yy81, yymsp[0].minor.yy81); }
#line 1227 "astgen.c"
        break;
      case 24:
#line 131 "astgen.in"
{ yygotominor.yy38 = op_assign; }
#line 1232 "astgen.c"
        break;
      case 25:
#line 132 "astgen.in"
{ yygotominor.yy38 = op_assadd; }
#line 1237 "astgen.c"
        break;
      case 26:
#line 133 "astgen.in"
{ yygotominor.yy38 = op_asssub; }
#line 1242 "astgen.c"
        break;
      case 27:
#line 134 "astgen.in"
{ yygotominor.yy38 = op_assmul; }
#line 1247 "astgen.c"
        break;
      case 28:
#line 135 "astgen.in"
{ yygotominor.yy38 = op_assdiv; }
#line 1252 "astgen.c"
        break;
      case 29:
#line 136 "astgen.in"
{ yygotominor.yy38 = op_assmod; }
#line 1257 "astgen.c"
        break;
      case 31:
#line 140 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(ternary_expression, yymsp[-4].minor.yy81, yymsp[-2].minor.yy81, yymsp[0].minor.yy81); }
#line 1262 "astgen.c"
        break;
      case 33:
#line 144 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(binary_expression, op_logor,   yymsp[-2].minor.yy81, yymsp[0].minor.yy81); }
#line 1267 "astgen.c"
        break;
      case 34:
#line 145 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(binary_expression, op_logand,  yymsp[-2].minor.yy81, yymsp[0].minor.yy81); }
#line 1272 "astgen.c"
        break;
      case 35:
#line 146 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(binary_expression, op_bitor,   yymsp[-2].minor.yy81, yymsp[0].minor.yy81); }
#line 1277 "astgen.c"
        break;
      case 36:
#line 147 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(binary_expression, op_bitxor,  yymsp[-2].minor.yy81, yymsp[0].minor.yy81); }
#line 1282 "astgen.c"
        break;
      case 37:
#line 148 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(binary_expression, op_bitand,  yymsp[-2].minor.yy81, yymsp[0].minor.yy81); }
#line 1287 "astgen.c"
        break;
      case 38:
#line 149 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(binary_expression, op_eq,   yymsp[-2].minor.yy81, yymsp[0].minor.yy81); }
#line 1292 "astgen.c"
        break;
      case 39:
#line 150 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(binary_expression, op_ne,   yymsp[-2].minor.yy81, yymsp[0].minor.yy81); }
#line 1297 "astgen.c"
        break;
      case 40:
#line 151 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(binary_expression, op_lt,   yymsp[-2].minor.yy81, yymsp[0].minor.yy81); }
#line 1302 "astgen.c"
        break;
      case 41:
#line 152 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(binary_expression, op_le,   yymsp[-2].minor.yy81, yymsp[0].minor.yy81); }
#line 1307 "astgen.c"
        break;
      case 42:
#line 153 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(binary_expression, op_gt,   yymsp[-2].minor.yy81, yymsp[0].minor.yy81); }
#line 1312 "astgen.c"
        break;
      case 43:
#line 154 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(binary_expression, op_ge,   yymsp[-2].minor.yy81, yymsp[0].minor.yy81); }
#line 1317 "astgen.c"
        break;
      case 44:
#line 155 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(binary_expression, op_add,  yymsp[-2].minor.yy81, yymsp[0].minor.yy81); }
#line 1322 "astgen.c"
        break;
      case 45:
#line 156 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(binary_expression, op_sub,  yymsp[-2].minor.yy81, yymsp[0].minor.yy81); }
#line 1327 "astgen.c"
        break;
      case 46:
#line 157 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(binary_expression, op_mul,  yymsp[-2].minor.yy81, yymsp[0].minor.yy81); }
#line 1332 "astgen.c"
        break;
      case 47:
#line 158 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(binary_expression, op_div,  yymsp[-2].minor.yy81, yymsp[0].minor.yy81); }
#line 1337 "astgen.c"
        break;
      case 48:
#line 159 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(binary_expression, op_mod,  yymsp[-2].minor.yy81, yymsp[0].minor.yy81); }
#line 1342 "astgen.c"
        break;
      case 50:
#line 163 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(prefix_expression, op_negate, yymsp[0].minor.yy81); }
#line 1347 "astgen.c"
        break;
      case 51:
#line 164 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(prefix_expression, op_preinc, yymsp[0].minor.yy81); }
#line 1352 "astgen.c"
        break;
      case 52:
#line 165 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(prefix_expression, op_predec, yymsp[0].minor.yy81); }
#line 1357 "astgen.c"
        break;
      case 55:
#line 170 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(postfix_expression, op_postinc, yymsp[-1].minor.yy81); }
#line 1362 "astgen.c"
        break;
      case 56:
#line 171 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(postfix_expression, op_postdec, yymsp[-1].minor.yy81); }
#line 1367 "astgen.c"
        break;
      case 57:
#line 172 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(member_expression, yymsp[-2].minor.yy81, String(yymsp[0].minor.yy0)); }
#line 1372 "astgen.c"
        break;
      case 58:
#line 173 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(index_expression, yymsp[-3].minor.yy81, yymsp[-1].minor.yy81); }
#line 1377 "astgen.c"
        break;
      case 59:
#line 174 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(function_call, String(yymsp[-2].minor.yy0)); }
#line 1382 "astgen.c"
        break;
      case 60:
#line 175 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(function_call, String(yymsp[-3].minor.yy0), yymsp[-1].minor.yy81); }
#line 1387 "astgen.c"
        break;
      case 63:
      case 90:
      case 91:
      case 99:
      case 116:
#line 180 "astgen.in"
{ yygotominor.yy81 = yymsp[-1].minor.yy81; }
#line 1396 "astgen.c"
        break;
      case 65:
#line 184 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(literal, Variant(String(yymsp[0].minor.yy0), Variant::stInt)); }
#line 1401 "astgen.c"
        break;
      case 66:
#line 185 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(literal, Variant(String(yymsp[0].minor.yy0), Variant::stReal)); }
#line 1406 "astgen.c"
        break;
      case 67:
#line 186 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(literal, Variant(String(yymsp[0].minor.yy0), Variant::stString)); }
#line 1411 "astgen.c"
        break;
      case 68:
#line 187 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(literal, Variant(true));    }
#line 1416 "astgen.c"
        break;
      case 69:
#line 188 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(literal, Variant(false));   }
#line 1421 "astgen.c"
        break;
      case 70:
#line 189 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(literal, Variant());        }
#line 1426 "astgen.c"
        break;
      case 71:
#line 192 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(lvalue, String(yymsp[0].minor.yy0)); }
#line 1431 "astgen.c"
        break;
      case 72:
#line 195 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(list_literal, yymsp[-1].minor.yy81); }
#line 1436 "astgen.c"
        break;
      case 73:
#line 196 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(list_content, yymsp[0].minor.yy81); }
#line 1441 "astgen.c"
        break;
      case 74:
#line 197 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(list_content, yymsp[-2].minor.yy81, yymsp[0].minor.yy81); }
#line 1446 "astgen.c"
        break;
      case 75:
#line 198 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(list_entry, yymsp[0].minor.yy81); }
#line 1451 "astgen.c"
        break;
      case 76:
#line 201 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(new_expression, String(yymsp[0].minor.yy0)); }
#line 1456 "astgen.c"
        break;
      case 77:
#line 210 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(argument, yymsp[0].minor.yy81); }
#line 1461 "astgen.c"
        break;
      case 79:
#line 214 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(argument_list, yymsp[-2].minor.yy81, yymsp[0].minor.yy81); }
#line 1466 "astgen.c"
        break;
      case 81:
#line 223 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(expression_statement, yymsp[-1].minor.yy81); }
#line 1471 "astgen.c"
        break;
      case 82:
#line 226 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(compound_statement); }
#line 1476 "astgen.c"
        break;
      case 83:
#line 227 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(compound_statement, yymsp[-1].minor.yy81); }
#line 1481 "astgen.c"
        break;
      case 84:
#line 230 "astgen.in"
{ p->Parse(yymsp[-1].minor.yy0); yygotominor.yy81 = p->GetRoot(); }
#line 1486 "astgen.c"
        break;
      case 85:
#line 233 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(return_statement, yymsp[-1].minor.yy81); }
#line 1491 "astgen.c"
        break;
      case 86:
#line 234 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(return_statement);    }
#line 1496 "astgen.c"
        break;
      case 87:
#line 237 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(break_statement); }
#line 1501 "astgen.c"
        break;
      case 88:
#line 238 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(continue_statement); }
#line 1506 "astgen.c"
        break;
      case 93:
#line 252 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(variable_declaration, String(yymsp[0].minor.yy0));    }
#line 1511 "astgen.c"
        break;
      case 94:
#line 253 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(variable_declaration, String(yymsp[-2].minor.yy0), yymsp[0].minor.yy81); }
#line 1516 "astgen.c"
        break;
      case 96:
#line 256 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(declaration_sequence, yymsp[-2].minor.yy81, yymsp[0].minor.yy81); }
#line 1521 "astgen.c"
        break;
      case 97:
#line 265 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(struct_declaration, String(yymsp[-2].minor.yy0)); }
#line 1526 "astgen.c"
        break;
      case 98:
#line 266 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(struct_declaration, String(yymsp[-3].minor.yy0), yymsp[-1].minor.yy81); }
#line 1531 "astgen.c"
        break;
      case 101:
#line 273 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(struct_members, yymsp[-1].minor.yy81, yymsp[0].minor.yy81); }
#line 1536 "astgen.c"
        break;
      case 102:
#line 281 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(function_declaration, String(yymsp[-4].minor.yy0), yymsp[-2].minor.yy81, yymsp[0].minor.yy81); }
#line 1541 "astgen.c"
        break;
      case 103:
#line 282 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(function_declaration, String(yymsp[-3].minor.yy0), AstData(), yymsp[0].minor.yy81); }
#line 1546 "astgen.c"
        break;
      case 104:
#line 285 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(parameter, String(yymsp[0].minor.yy0)); }
#line 1551 "astgen.c"
        break;
      case 105:
#line 288 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(parameter, String(yymsp[-2].minor.yy0), yymsp[0].minor.yy81); }
#line 1556 "astgen.c"
        break;
      case 107:
      case 109:
      case 112:
#line 292 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(parameter_list, yymsp[-2].minor.yy81, yymsp[0].minor.yy81); }
#line 1563 "astgen.c"
        break;
      case 114:
#line 313 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(for_statement, yymsp[-5].minor.yy81, yymsp[-4].minor.yy81, yymsp[-2].minor.yy81, yymsp[0].minor.yy81); }
#line 1568 "astgen.c"
        break;
      case 117:
#line 324 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(foreach_statement, yymsp[-4].minor.yy81, yymsp[-2].minor.yy81, yymsp[0].minor.yy81); }
#line 1573 "astgen.c"
        break;
      case 118:
#line 335 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(if_statement, yymsp[-2].minor.yy81, yymsp[0].minor.yy81); }
#line 1578 "astgen.c"
        break;
      case 119:
#line 336 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(if_statement, yymsp[-4].minor.yy81, yymsp[-2].minor.yy81, yymsp[0].minor.yy81); }
#line 1583 "astgen.c"
        break;
      case 120:
#line 344 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(while_statement, yymsp[-2].minor.yy81,  yymsp[0].minor.yy81); }
#line 1588 "astgen.c"
        break;
      case 121:
#line 352 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(switch_statement, yymsp[-4].minor.yy81, yymsp[-1].minor.yy223); }
#line 1593 "astgen.c"
        break;
      case 122:
#line 356 "astgen.in"
{ yygotominor.yy223 = new AstList; }
#line 1598 "astgen.c"
        break;
      case 123:
      case 124:
#line 357 "astgen.in"
{ yygotominor.yy223 = yymsp[-1].minor.yy223; yygotominor.yy223->push_back(yymsp[0].minor.yy81); }
#line 1604 "astgen.c"
        break;
      case 125:
#line 361 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(switch_case, yymsp[-2].minor.yy81, yymsp[0].minor.yy81); }
#line 1609 "astgen.c"
        break;
      case 126:
#line 364 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(default_case, yymsp[0].minor.yy81); }
#line 1614 "astgen.c"
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
#line 53 "astgen.in"

  p->OnParseFailure();
#line 1662 "astgen.c"
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
#line 56 "astgen.in"

  p->OnSyntaxError();
#line 1680 "astgen.c"
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

