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
#include "convert.h"

#pragma warning(disable:4065)

#line 20 "astgen.c"
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
#define YYNOCODE 129
#define YYACTIONTYPE unsigned short int
#define CScriptParseTOKENTYPE  Token 
typedef union {
  CScriptParseTOKENTYPE yy0;
  opcodes yy46;
  AstList* yy87;
  AccessTypes yy156;
  Ast* yy215;
  int yy257;
} YYMINORTYPE;
#ifndef YYSTACKDEPTH
#define YYSTACKDEPTH 100
#endif
#define CScriptParseARG_SDECL  Parser* p ;
#define CScriptParseARG_PDECL , Parser* p 
#define CScriptParseARG_FETCH  Parser* p  = yypParser->p 
#define CScriptParseARG_STORE yypParser->p  = p 
#define YYNSTATE 253
#define YYNRULE 148
#define YYERRORSYMBOL 71
#define YYERRSYMDT yy257
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
 /*     0 */   218,  402,  146,  244,    9,  243,  239,  238,  235,  234,
 /*    10 */   232,  230,  229,  228,  227,  225,  222,  221,  219,  124,
 /*    20 */   217,  139,  216,   65,  175,  213,   84,  202,  201,  200,
 /*    30 */    55,  199,  198,  195,  166,   20,  104,  145,  127,  126,
 /*    40 */   250,   62,   53,   56,   54,  171,   98,   17,  170,   24,
 /*    50 */   111,  194,  193,  191,  190,  189,  188,  187,  186,  184,
 /*    60 */   183,   15,  123,  148,    3,  252,  132,   21,  134,  131,
 /*    70 */   128,   64,   99,   55,  133,   64,  113,  135,  119,  157,
 /*    80 */   113,  102,  121,   90,  116,   53,   56,   54,    4,   98,
 /*    90 */    17,  142,   24,   61,  194,  193,  191,  190,  189,  188,
 /*   100 */   187,  186,  184,  183,  246,  123,  148,    3,  141,  132,
 /*   110 */    21,  134,  131,  128,   64,   99,   42,   47,   48,  113,
 /*   120 */   135,  205,  206,  218,  102,  121,   90,    7,  243,  239,
 /*   130 */   238,  235,  234,  232,  230,  229,  228,  227,  225,  222,
 /*   140 */   221,  219,  124,  217,   25,  216,   65,  245,  213,   84,
 /*   150 */   202,  201,  200,  196,  199,  198,  195,  163,   30,   55,
 /*   160 */   145,  127,  126,   64,  130,  247,  248,  249,  113,  144,
 /*   170 */   168,   53,   56,   54,  147,   98,   17,   92,   24,  211,
 /*   180 */   194,  193,  191,  190,  189,  188,  187,  186,  184,  183,
 /*   190 */   149,  123,  148,    3,  156,  132,   21,  134,  131,  128,
 /*   200 */    64,   99,  390,    5,  143,  113,  135,   89,   97,  218,
 /*   210 */   102,  121,   90,    7,  243,  239,  238,  235,  234,  232,
 /*   220 */   230,  229,  228,  227,  225,  222,  221,  219,  124,  217,
 /*   230 */   179,  216,   65,   69,  213,   84,  202,  201,  200,   32,
 /*   240 */   199,  198,  195,   33,  218,   87,  145,  127,  126,  178,
 /*   250 */   239,  238,  235,  234,  232,  230,  229,  228,  227,  225,
 /*   260 */   222,  221,  219,  124,  217,  214,  216,   65,   11,  213,
 /*   270 */    84,  202,  201,  200,  116,  199,  198,  195,   67,  140,
 /*   280 */    60,  145,  127,  126,  212,  210,  218,   68,   30,   22,
 /*   290 */   233,  178,  239,  238,  235,  234,  232,  230,  229,  228,
 /*   300 */   227,  225,  222,  221,  219,  124,  217,   13,  216,   65,
 /*   310 */    26,  213,   84,  202,  201,  200,   12,  199,  198,  195,
 /*   320 */    58,   16,  182,  145,  127,  126,   10,  115,   14,  218,
 /*   330 */    29,   34,  177,    6,  243,  239,  238,  235,  234,  232,
 /*   340 */   230,  229,  228,  227,  225,  222,  221,  219,  124,  217,
 /*   350 */   169,  216,   65,    2,  213,   84,  202,  201,  200,  226,
 /*   360 */   199,  198,  195,    1,  218,   70,  145,  127,  126,  242,
 /*   370 */   239,  238,  235,  234,  232,  230,  229,  228,  227,  225,
 /*   380 */   222,  221,  219,  124,  217,    8,  216,   65,  204,  213,
 /*   390 */    84,  202,  201,  200,  100,  199,  198,  195,  167,   18,
 /*   400 */    57,  145,  127,  126,  218,   19,   28,   66,   96,   94,
 /*   410 */   239,  238,  235,  234,  232,  230,  229,  228,  227,  225,
 /*   420 */   222,  221,  219,  124,  217,   23,  216,   65,  165,  213,
 /*   430 */    84,  202,  201,  200,  109,  199,  198,  195,  403,  218,
 /*   440 */   403,  145,  127,  126,  185,  239,  238,  235,  234,  232,
 /*   450 */   230,  229,  228,  227,  225,  222,  221,  219,  124,  217,
 /*   460 */   403,  216,   65,  403,  213,   84,  202,  201,  200,  403,
 /*   470 */   199,  198,  195,  403,  218,  403,  145,  127,  126,  231,
 /*   480 */   239,  238,  235,  234,  232,  230,  229,  228,  227,  225,
 /*   490 */   222,  221,  219,  124,  217,  403,  216,   65,  403,  213,
 /*   500 */    84,  202,  201,  200,  403,  199,  198,  195,  403,  218,
 /*   510 */   403,  145,  127,  126,  197,  239,  238,  235,  234,  232,
 /*   520 */   230,  229,  228,  227,  225,  222,  221,  219,  124,  217,
 /*   530 */   403,  216,   65,  403,  213,   84,  202,  201,  200,  403,
 /*   540 */   199,  198,  195,  403,  218,  403,  145,  127,  126,  223,
 /*   550 */   239,  238,  235,  234,  232,  230,  229,  228,  227,  225,
 /*   560 */   222,  221,  219,  124,  217,  403,  216,   65,  403,  213,
 /*   570 */    84,  202,  201,  200,  403,  199,  198,  195,  403,  218,
 /*   580 */   403,  145,  127,  126,  192,  239,  238,  235,  234,  232,
 /*   590 */   230,  229,  228,  227,  225,  222,  221,  219,  124,  217,
 /*   600 */   403,  216,   65,  403,  213,   84,  202,  201,  200,  403,
 /*   610 */   199,  198,  195,  403,  403,  403,  145,  127,  126,   50,
 /*   620 */    36,   39,   52,   44,   51,   40,   46,   37,   45,   43,
 /*   630 */    35,   41,   38,   49,   42,   47,   48,  164,  162,  160,
 /*   640 */   159,  158,  154,   31,   55,   46,   37,   45,   43,   35,
 /*   650 */    41,   38,   49,   42,   47,   48,   53,   56,   54,  403,
 /*   660 */    98,   17,  403,   24,  403,  194,  193,  191,  190,  189,
 /*   670 */   188,  187,  186,  184,  183,  403,  123,  148,   86,   55,
 /*   680 */   213,   84,  202,  201,  200,   63,  199,  198,  195,  403,
 /*   690 */   403,   53,   56,   54,  403,   98,   17,  403,   24,  176,
 /*   700 */   194,  193,  191,  190,  189,  188,  187,  186,  184,  183,
 /*   710 */   403,  123,   55,   71,  403,  213,   84,  202,  201,  200,
 /*   720 */   403,  199,  198,  195,   53,   56,   54,  403,   98,   17,
 /*   730 */   403,   24,  403,  194,  193,  191,  190,  189,  188,  187,
 /*   740 */   186,  184,  183,  403,  123,  138,  151,   55,  213,   84,
 /*   750 */   202,  201,  200,  403,  199,  198,  195,  403,  403,   53,
 /*   760 */    56,   54,  403,   98,   17,  403,   24,  403,  194,  193,
 /*   770 */   191,  190,  189,  188,  187,  186,  184,  183,  403,  123,
 /*   780 */    36,   39,   52,   44,   51,   40,   46,   37,   45,   43,
 /*   790 */    35,   41,   38,   49,   42,   47,   48,   39,   52,   44,
 /*   800 */    51,   40,   46,   37,   45,   43,   35,   41,   38,   49,
 /*   810 */    42,   47,   48,  224,   77,  403,  213,   84,  202,  201,
 /*   820 */   200,  403,  199,  198,  195,  124,  217,  403,  216,   65,
 /*   830 */   403,  213,   84,  202,  201,  200,  403,  199,  105,  195,
 /*   840 */   240,  241,  403,  122,   79,  110,  213,   84,  202,  201,
 /*   850 */   200,  403,  199,  198,  195,  403,  403,   27,  112,  403,
 /*   860 */   403,  403,   52,   44,   51,   40,   46,   37,   45,   43,
 /*   870 */    35,   41,   38,   49,   42,   47,   48,  174,  217,  403,
 /*   880 */   216,   65,  403,  213,   84,  202,  201,  200,   88,  199,
 /*   890 */   198,  195,  180,  217,  172,  216,   65,  403,  213,   84,
 /*   900 */   202,  201,  200,  161,  199,  198,  195,  181,  101,   64,
 /*   910 */   403,  247,  248,  249,  113,  403,  180,  217,  403,  216,
 /*   920 */    65,  403,  213,   84,  202,  201,  200,  403,  199,  198,
 /*   930 */   195,   93,  101,  403,   44,   51,   40,   46,   37,   45,
 /*   940 */    43,   35,   41,   38,   49,   42,   47,   48,  403,  174,
 /*   950 */   217,  403,  216,   65,  403,  213,   84,  202,  201,  200,
 /*   960 */   403,  199,  198,  195,  403,  403,  173,  403,   51,   40,
 /*   970 */    46,   37,   45,   43,   35,   41,   38,   49,   42,   47,
 /*   980 */    48,  220,  217,  108,  216,   65,  403,  213,   84,  202,
 /*   990 */   201,  200,  403,  199,  198,  195,  155,  217,  403,  216,
 /*  1000 */    65,  403,  213,   84,  202,  201,  200,  403,  199,  198,
 /*  1010 */   195,   95,  217,  403,  216,   65,  403,  213,   84,  202,
 /*  1020 */   201,  200,  403,  199,  198,  195,  114,  217,  403,  216,
 /*  1030 */    65,  403,  213,   84,  202,  201,  200,  403,  199,  198,
 /*  1040 */   195,  117,  217,  403,  216,   65,  403,  213,   84,  202,
 /*  1050 */   201,  200,  403,  199,  198,  195,  403,  107,  217,  403,
 /*  1060 */   216,   65,  403,  213,   84,  202,  201,  200,  403,  199,
 /*  1070 */   198,  195,  129,  217,  403,  216,   65,  403,  213,   84,
 /*  1080 */   202,  201,  200,  403,  199,  198,  195,  106,  217,  403,
 /*  1090 */   216,   65,  403,  213,   84,  202,  201,  200,  403,  199,
 /*  1100 */   198,  195,  120,  217,  403,  216,   65,  403,  213,   84,
 /*  1110 */   202,  201,  200,  403,  199,  198,  195,  103,  217,  403,
 /*  1120 */   216,   65,  403,  213,   84,  202,  201,  200,  403,  199,
 /*  1130 */   198,  195,   91,  217,  403,  216,   65,  403,  213,   84,
 /*  1140 */   202,  201,  200,  403,  199,  198,  195,  403,  236,  217,
 /*  1150 */   403,  216,   65,  403,  213,   84,  202,  201,  200,  403,
 /*  1160 */   199,  198,  195,  136,  217,  403,  216,   65,  403,  213,
 /*  1170 */    84,  202,  201,  200,  403,  199,  198,  195,  215,  403,
 /*  1180 */   216,   65,  403,  213,   84,  202,  201,  200,  403,  199,
 /*  1190 */   198,  195,  153,  403,  216,   65,  403,  213,   84,  202,
 /*  1200 */   201,  200,  403,  199,  198,  195,   74,  403,  213,   84,
 /*  1210 */   202,  201,  200,  403,  199,  198,  195,  403,   83,  403,
 /*  1220 */   213,   84,  202,  201,  200,  403,  199,  198,  195,  403,
 /*  1230 */    78,  403,  213,   84,  202,  201,  200,  403,  199,  198,
 /*  1240 */   195,   75,  403,  213,   84,  202,  201,  200,  403,  199,
 /*  1250 */   198,  195,   72,  403,  213,   84,  202,  201,  200,  403,
 /*  1260 */   199,  198,  195,  403,   81,  403,  213,   84,  202,  201,
 /*  1270 */   200,  403,  199,  198,  195,  152,  403,  213,   84,  202,
 /*  1280 */   201,  200,  403,  199,  198,  195,   80,  403,  213,   84,
 /*  1290 */   202,  201,  200,  403,  199,  198,  195,  403,   82,  403,
 /*  1300 */   213,   84,  202,  201,  200,  403,  199,  198,  195,   76,
 /*  1310 */   403,  213,   84,  202,  201,  200,  403,  199,  198,  195,
 /*  1320 */   403,   73,  403,  213,   84,  202,  201,  200,  403,  199,
 /*  1330 */   198,  195,  403,   85,  403,  213,   84,  202,  201,  200,
 /*  1340 */   403,  199,  198,  195,   45,   43,   35,   41,   38,   49,
 /*  1350 */    42,   47,   48,  193,  191,  190,  189,  188,  187,  186,
 /*  1360 */   184,  183,  150,  403,  213,   84,  202,  201,  200,  403,
 /*  1370 */   199,  198,  195,  403,  208,   84,  202,  201,  200,  403,
 /*  1380 */   199,  198,  195,  209,   84,  202,  201,  200,  403,  199,
 /*  1390 */   198,  195,  207,   84,  202,  201,  200,  403,  199,  198,
 /*  1400 */   195,  203,   84,  202,  201,  200,  403,  199,  198,  195,
 /*  1410 */   403,  125,  403,  237,  241,  118,  137,  166,  403,  104,
 /*  1420 */   403,  403,   59,  251,   62,  403,   38,   49,   42,   47,
 /*  1430 */    48,
};
static const YYCODETYPE yy_lookahead[] = {
 /*     0 */    71,   72,   73,   74,   75,   76,   77,   78,   79,   80,
 /*    10 */    81,   82,   83,   84,   85,   86,   87,   88,   89,   90,
 /*    20 */    91,   48,   93,   94,   35,   96,   97,   98,   99,  100,
 /*    30 */    15,  102,  103,  104,  108,   46,  110,  108,  109,  110,
 /*    40 */   114,  115,   27,   28,   29,   26,   31,   32,  108,   34,
 /*    50 */   110,   36,   37,   38,   39,   40,   41,   42,   43,   44,
 /*    60 */    45,   34,   47,   48,   49,   50,   51,   52,   53,   54,
 /*    70 */    55,   56,   57,   15,   42,   56,   61,   62,  111,  112,
 /*    80 */    61,   66,   67,   68,   31,   27,   28,   29,   35,   31,
 /*    90 */    32,   48,   34,  124,   36,   37,   38,   39,   40,   41,
 /*   100 */    42,   43,   44,   45,   48,   47,   48,   49,   50,   51,
 /*   110 */    52,   53,   54,   55,   56,   57,   16,   17,   18,   61,
 /*   120 */    62,  125,  126,   71,   66,   67,   68,   75,   76,   77,
 /*   130 */    78,   79,   80,   81,   82,   83,   84,   85,   86,   87,
 /*   140 */    88,   89,   90,   91,   19,   93,   94,  119,   96,   97,
 /*   150 */    98,   99,  100,   35,  102,  103,  104,   50,   19,   15,
 /*   160 */   108,  109,  110,   56,   31,   58,   59,   60,   61,   48,
 /*   170 */   100,   27,   28,   29,   48,   31,   32,   31,   34,  127,
 /*   180 */    36,   37,   38,   39,   40,   41,   42,   43,   44,   45,
 /*   190 */    48,   47,   48,   49,  112,   51,   52,   53,   54,   55,
 /*   200 */    56,   57,   63,   35,   48,   61,   62,   31,  102,   71,
 /*   210 */    66,   67,   68,   75,   76,   77,   78,   79,   80,   81,
 /*   220 */    82,   83,   84,   85,   86,   87,   88,   89,   90,   91,
 /*   230 */    31,   93,   94,   46,   96,   97,   98,   99,  100,   34,
 /*   240 */   102,  103,  104,   95,   71,   31,  108,  109,  110,   76,
 /*   250 */    77,   78,   79,   80,   81,   82,   83,   84,   85,   86,
 /*   260 */    87,   88,   89,   90,   91,  127,   93,   94,   35,   96,
 /*   270 */    97,   98,   99,  100,   31,  102,  103,  104,   46,   48,
 /*   280 */    46,  108,  109,  110,   27,   28,   71,   30,   19,   32,
 /*   290 */   117,   76,   77,   78,   79,   80,   81,   82,   83,   84,
 /*   300 */    85,   86,   87,   88,   89,   90,   91,   35,   93,   94,
 /*   310 */    34,   96,   97,   98,   99,  100,   35,  102,  103,  104,
 /*   320 */    34,   34,   33,  108,  109,  110,   35,   31,   65,   71,
 /*   330 */    63,   26,  117,   75,   76,   77,   78,   79,   80,   81,
 /*   340 */    82,   83,   84,   85,   86,   87,   88,   89,   90,   91,
 /*   350 */    48,   93,   94,   26,   96,   97,   98,   99,  100,   48,
 /*   360 */   102,  103,  104,   26,   71,   49,  108,  109,  110,   76,
 /*   370 */    77,   78,   79,   80,   81,   82,   83,   84,   85,   86,
 /*   380 */    87,   88,   89,   90,   91,   35,   93,   94,   50,   96,
 /*   390 */    97,   98,   99,  100,   31,  102,  103,  104,   33,   46,
 /*   400 */    49,  108,  109,  110,   71,   48,   63,   69,   70,   76,
 /*   410 */    77,   78,   79,   80,   81,   82,   83,   84,   85,   86,
 /*   420 */    87,   88,   89,   90,   91,   34,   93,   94,   48,   96,
 /*   430 */    97,   98,   99,  100,   35,  102,  103,  104,  128,   71,
 /*   440 */   128,  108,  109,  110,   76,   77,   78,   79,   80,   81,
 /*   450 */    82,   83,   84,   85,   86,   87,   88,   89,   90,   91,
 /*   460 */   128,   93,   94,  128,   96,   97,   98,   99,  100,  128,
 /*   470 */   102,  103,  104,  128,   71,  128,  108,  109,  110,   76,
 /*   480 */    77,   78,   79,   80,   81,   82,   83,   84,   85,   86,
 /*   490 */    87,   88,   89,   90,   91,  128,   93,   94,  128,   96,
 /*   500 */    97,   98,   99,  100,  128,  102,  103,  104,  128,   71,
 /*   510 */   128,  108,  109,  110,   76,   77,   78,   79,   80,   81,
 /*   520 */    82,   83,   84,   85,   86,   87,   88,   89,   90,   91,
 /*   530 */   128,   93,   94,  128,   96,   97,   98,   99,  100,  128,
 /*   540 */   102,  103,  104,  128,   71,  128,  108,  109,  110,   76,
 /*   550 */    77,   78,   79,   80,   81,   82,   83,   84,   85,   86,
 /*   560 */    87,   88,   89,   90,   91,  128,   93,   94,  128,   96,
 /*   570 */    97,   98,   99,  100,  128,  102,  103,  104,  128,   71,
 /*   580 */   128,  108,  109,  110,   76,   77,   78,   79,   80,   81,
 /*   590 */    82,   83,   84,   85,   86,   87,   88,   89,   90,   91,
 /*   600 */   128,   93,   94,  128,   96,   97,   98,   99,  100,  128,
 /*   610 */   102,  103,  104,  128,  128,  128,  108,  109,  110,    1,
 /*   620 */     2,    3,    4,    5,    6,    7,    8,    9,   10,   11,
 /*   630 */    12,   13,   14,   15,   16,   17,   18,   19,   20,   21,
 /*   640 */    22,   23,   24,   25,   15,    8,    9,   10,   11,   12,
 /*   650 */    13,   14,   15,   16,   17,   18,   27,   28,   29,  128,
 /*   660 */    31,   32,  128,   34,  128,   36,   37,   38,   39,   40,
 /*   670 */    41,   42,   43,   44,   45,  128,   47,   48,   94,   15,
 /*   680 */    96,   97,   98,   99,  100,   56,  102,  103,  104,  128,
 /*   690 */   128,   27,   28,   29,  128,   31,   32,  128,   34,   35,
 /*   700 */    36,   37,   38,   39,   40,   41,   42,   43,   44,   45,
 /*   710 */   128,   47,   15,   94,  128,   96,   97,   98,   99,  100,
 /*   720 */   128,  102,  103,  104,   27,   28,   29,  128,   31,   32,
 /*   730 */   128,   34,  128,   36,   37,   38,   39,   40,   41,   42,
 /*   740 */    43,   44,   45,  128,   47,   48,   94,   15,   96,   97,
 /*   750 */    98,   99,  100,  128,  102,  103,  104,  128,  128,   27,
 /*   760 */    28,   29,  128,   31,   32,  128,   34,  128,   36,   37,
 /*   770 */    38,   39,   40,   41,   42,   43,   44,   45,  128,   47,
 /*   780 */     2,    3,    4,    5,    6,    7,    8,    9,   10,   11,
 /*   790 */    12,   13,   14,   15,   16,   17,   18,    3,    4,    5,
 /*   800 */     6,    7,    8,    9,   10,   11,   12,   13,   14,   15,
 /*   810 */    16,   17,   18,   78,   94,  128,   96,   97,   98,   99,
 /*   820 */   100,  128,  102,  103,  104,   90,   91,  128,   93,   94,
 /*   830 */   128,   96,   97,   98,   99,  100,  128,  102,  103,  104,
 /*   840 */   118,  119,  128,  121,   94,  110,   96,   97,   98,   99,
 /*   850 */   100,  128,  102,  103,  104,  128,  128,  122,  123,  128,
 /*   860 */   128,  128,    4,    5,    6,    7,    8,    9,   10,   11,
 /*   870 */    12,   13,   14,   15,   16,   17,   18,   90,   91,  128,
 /*   880 */    93,   94,  128,   96,   97,   98,   99,  100,  101,  102,
 /*   890 */   103,  104,   90,   91,  107,   93,   94,  128,   96,   97,
 /*   900 */    98,   99,  100,   50,  102,  103,  104,  105,  106,   56,
 /*   910 */   128,   58,   59,   60,   61,  128,   90,   91,  128,   93,
 /*   920 */    94,  128,   96,   97,   98,   99,  100,  128,  102,  103,
 /*   930 */   104,  105,  106,  128,    5,    6,    7,    8,    9,   10,
 /*   940 */    11,   12,   13,   14,   15,   16,   17,   18,  128,   90,
 /*   950 */    91,  128,   93,   94,  128,   96,   97,   98,   99,  100,
 /*   960 */   128,  102,  103,  104,  128,  128,  107,  128,    6,    7,
 /*   970 */     8,    9,   10,   11,   12,   13,   14,   15,   16,   17,
 /*   980 */    18,   90,   91,   92,   93,   94,  128,   96,   97,   98,
 /*   990 */    99,  100,  128,  102,  103,  104,   90,   91,  128,   93,
 /*  1000 */    94,  128,   96,   97,   98,   99,  100,  128,  102,  103,
 /*  1010 */   104,   90,   91,  128,   93,   94,  128,   96,   97,   98,
 /*  1020 */    99,  100,  128,  102,  103,  104,   90,   91,  128,   93,
 /*  1030 */    94,  128,   96,   97,   98,   99,  100,  128,  102,  103,
 /*  1040 */   104,   90,   91,  128,   93,   94,  128,   96,   97,   98,
 /*  1050 */    99,  100,  128,  102,  103,  104,  128,   90,   91,  128,
 /*  1060 */    93,   94,  128,   96,   97,   98,   99,  100,  128,  102,
 /*  1070 */   103,  104,   90,   91,  128,   93,   94,  128,   96,   97,
 /*  1080 */    98,   99,  100,  128,  102,  103,  104,   90,   91,  128,
 /*  1090 */    93,   94,  128,   96,   97,   98,   99,  100,  128,  102,
 /*  1100 */   103,  104,   90,   91,  128,   93,   94,  128,   96,   97,
 /*  1110 */    98,   99,  100,  128,  102,  103,  104,   90,   91,  128,
 /*  1120 */    93,   94,  128,   96,   97,   98,   99,  100,  128,  102,
 /*  1130 */   103,  104,   90,   91,  128,   93,   94,  128,   96,   97,
 /*  1140 */    98,   99,  100,  128,  102,  103,  104,  128,   90,   91,
 /*  1150 */   128,   93,   94,  128,   96,   97,   98,   99,  100,  128,
 /*  1160 */   102,  103,  104,   90,   91,  128,   93,   94,  128,   96,
 /*  1170 */    97,   98,   99,  100,  128,  102,  103,  104,   91,  128,
 /*  1180 */    93,   94,  128,   96,   97,   98,   99,  100,  128,  102,
 /*  1190 */   103,  104,   91,  128,   93,   94,  128,   96,   97,   98,
 /*  1200 */    99,  100,  128,  102,  103,  104,   94,  128,   96,   97,
 /*  1210 */    98,   99,  100,  128,  102,  103,  104,  128,   94,  128,
 /*  1220 */    96,   97,   98,   99,  100,  128,  102,  103,  104,  128,
 /*  1230 */    94,  128,   96,   97,   98,   99,  100,  128,  102,  103,
 /*  1240 */   104,   94,  128,   96,   97,   98,   99,  100,  128,  102,
 /*  1250 */   103,  104,   94,  128,   96,   97,   98,   99,  100,  128,
 /*  1260 */   102,  103,  104,  128,   94,  128,   96,   97,   98,   99,
 /*  1270 */   100,  128,  102,  103,  104,   94,  128,   96,   97,   98,
 /*  1280 */    99,  100,  128,  102,  103,  104,   94,  128,   96,   97,
 /*  1290 */    98,   99,  100,  128,  102,  103,  104,  128,   94,  128,
 /*  1300 */    96,   97,   98,   99,  100,  128,  102,  103,  104,   94,
 /*  1310 */   128,   96,   97,   98,   99,  100,  128,  102,  103,  104,
 /*  1320 */   128,   94,  128,   96,   97,   98,   99,  100,  128,  102,
 /*  1330 */   103,  104,  128,   94,  128,   96,   97,   98,   99,  100,
 /*  1340 */   128,  102,  103,  104,   10,   11,   12,   13,   14,   15,
 /*  1350 */    16,   17,   18,   37,   38,   39,   40,   41,   42,   43,
 /*  1360 */    44,   45,   94,  128,   96,   97,   98,   99,  100,  128,
 /*  1370 */   102,  103,  104,  128,   96,   97,   98,   99,  100,  128,
 /*  1380 */   102,  103,  104,   96,   97,   98,   99,  100,  128,  102,
 /*  1390 */   103,  104,   96,   97,   98,   99,  100,  128,  102,  103,
 /*  1400 */   104,   96,   97,   98,   99,  100,  128,  102,  103,  104,
 /*  1410 */   128,  116,  128,  118,  119,  120,  121,  108,  128,  110,
 /*  1420 */   128,  128,  113,  114,  115,  128,   14,   15,   16,   17,
 /*  1430 */    18,
};
#define YY_SHIFT_USE_DFLT (-28)
#define YY_SHIFT_MAX 137
static const short yy_shift_ofst[] = {
 /*     0 */   144,  144,  144,   15,  144,  144,   58,  144,  144,  144,
 /*    10 */   144,  144,  144,  144,  144,  629,  664,  732,  732,  732,
 /*    20 */   732,  697,  732,  732,  732,  732,  732,  732,  732,  732,
 /*    30 */   732,  732,  732,  732,  732,  732,  732,  732,  732,  732,
 /*    40 */   732,  732,  732,  732,  732,  732,  732,  732,  732,  732,
 /*    50 */   732,  732,  732,  732,  732,  732,  732,  853,   53,  107,
 /*    60 */   243,  338,   19,  214,  176,  618, 1316,  176,  146,  133,
 /*    70 */   -28,  778,  794,  858,  929,  962,  637,  637, 1334, 1334,
 /*    80 */  1412, 1412, 1412, 1412,  257,  100,  100,  139,  -11,  269,
 /*    90 */   276,  281,  287,  289,  263,  305,  327,  337,  287,  363,
 /*   100 */   351,  353,  391,  399,  380,  343,  357,  365,  350,  316,
 /*   110 */   311,  302,  267,  296,  291,  286,  125,  272,  234,  232,
 /*   120 */   233,  205,  187,  199,  156,  168,  142,  126,  121,  118,
 /*   130 */   125,   43,   32,  -27,  231,   27,   56,  187,
};
#define YY_REDUCE_USE_DFLT (-75)
#define YY_REDUCE_MAX 70
static const short yy_reduce_ofst[] = {
 /*     0 */   -71,   52,  138,  258,  173,  215,  293,  293,  473,  293,
 /*    10 */   403,  333,  368,  438,  508,  735,  787,  826,  802,  891,
 /*    20 */   859, 1073,  967, 1012,  982, 1058, 1027,  997, 1042,  936,
 /*    30 */   906,  921,  951, 1087, 1101, 1204, 1158, 1136, 1239, 1227,
 /*    40 */  1215, 1192, 1181, 1170, 1147, 1124,  750,  652, 1268,  584,
 /*    50 */   619,  720, 1112, 1278, 1305, 1287, 1296, 1309, 1295,  -74,
 /*    60 */   722,   -4,  -60,  -33,  -33,  148,  106,   82,   70,   28,
 /*    70 */   -31,
};
static const YYACTIONTYPE yy_default[] = {
 /*     0 */   257,  401,  401,  401,  401,  401,  401,  400,  401,  258,
 /*    10 */   401,  401,  401,  401,  401,  401,  401,  401,  401,  274,
 /*    20 */   401,  401,  401,  401,  401,  401,  401,  401,  401,  401,
 /*    30 */   401,  401,  401,  401,  401,  401,  401,  401,  401,  401,
 /*    40 */   401,  401,  401,  401,  401,  401,  401,  401,  401,  401,
 /*    50 */   401,  401,  401,  401,  401,  401,  401,  401,  401,  401,
 /*    60 */   401,  401,  401,  401,  401,  284,  401,  401,  401,  401,
 /*    70 */   395,  287,  288,  289,  290,  291,  304,  303,  293,  292,
 /*    80 */   297,  295,  296,  294,  305,  298,  299,  357,  401,  357,
 /*    90 */   401,  401,  315,  401,  391,  401,  401,  401,  334,  401,
 /*   100 */   401,  336,  401,  401,  401,  321,  401,  401,  401,  401,
 /*   110 */   401,  401,  401,  401,  401,  401,  375,  401,  381,  356,
 /*   120 */   401,  401,  383,  401,  401,  401,  401,  401,  401,  401,
 /*   130 */   401,  401,  401,  401,  401,  401,  401,  382,  349,  347,
 /*   140 */   350,  346,  351,  344,  352,  353,  253,  354,  343,  355,
 /*   150 */   302,  301,  300,  285,  283,  358,  360,  359,  282,  281,
 /*   160 */   280,  361,  279,  362,  278,  363,  364,  317,  316,  365,
 /*   170 */   366,  367,  341,  342,  340,  319,  318,  373,  384,  339,
 /*   180 */   338,  337,  335,  333,  332,  388,  331,  330,  329,  328,
 /*   190 */   327,  326,  392,  325,  324,  323,  322,  393,  321,  320,
 /*   200 */   314,  311,  310,  309,  394,  396,  397,  308,  307,  306,
 /*   210 */   313,  398,  312,  286,  399,  277,  276,  273,  272,  271,
 /*   220 */   275,  270,  269,  385,  386,  268,  387,  267,  266,  265,
 /*   230 */   264,  389,  263,  374,  262,  261,  376,  377,  260,  259,
 /*   240 */   378,  379,  256,  255,  254,  380,  348,  368,  369,  370,
 /*   250 */   372,  371,  345,
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
  "BITXOR",        "BITAND",        "SEQ",           "SNE",         
  "EQUALS",        "NEQUALS",       "ST",            "SE",          
  "GT",            "GE",            "ADDOP",         "SUBOP",       
  "MULOP",         "DIVOP",         "MODOP",         "ASSIGN",      
  "ASSADD",        "ASSSUB",        "ASSMUL",        "ASSDIV",      
  "ASSMOD",        "QUESTION",      "COLON",         "ADDADD",      
  "SUBSUB",        "NOT",           "DOT",           "IDENTIFIER",  
  "LBRACKET",      "RBRACKET",      "LPAREN",        "RPAREN",      
  "THIS",          "INTEGER",       "HEX",           "BIN",         
  "ROM",           "REAL",          "STRING",        "TRUE",        
  "FALSE",         "NULL",          "COMMA",         "NEW",         
  "SEMICOLON",     "LBRACE",        "RBRACE",        "INCLUDE",     
  "RETURN",        "BREAK",         "CONTINUE",      "PAUSE",       
  "VAR",           "CLASS",         "PRIVATE",       "PROTECTED",   
  "PUBLIC",        "FUNCTION",      "FOR",           "IN",          
  "LOWER_THAN_ELSE",  "ELSE",          "IF",            "WHILE",       
  "SWITCH",        "CASE",          "DEFAULT",       "error",       
  "main",          "translation_unit",  "statement_sequence_opt",  "statement_sequence",
  "statement",     "include_statement",  "expression_statement",  "declaration_statement",
  "for_statement",  "compound_statement",  "if_statement",  "while_statement",
  "foreach_statement",  "return_statement",  "switch_statement",  "break_statement",
  "continue_statement",  "pause_statement",  "expression",    "assignment_expression",
  "expression_opt",  "conditional_expression",  "binary_expression",  "assignment_operator",
  "unary_expression",  "postfix_expression",  "new_expression",  "primary_expression",
  "function_call",  "argument_list",  "literal",       "id_expression",
  "list_literal",  "list_content",  "list_entry",    "argument",    
  "function_declaration",  "class_declaration",  "variable_declaration",  "declarator_sequence",
  "declarator",    "class_members",  "class_member",  "access_specifier",
  "parameter_list",  "function_body",  "parameter",     "opt_parameter",
  "parameters",    "opt_parameters",  "for_init_statement",  "foreach_decl",
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
 /*  18 */ "statement ::= pause_statement",
 /*  19 */ "statement ::= error",
 /*  20 */ "expression ::= assignment_expression",
 /*  21 */ "expression_opt ::=",
 /*  22 */ "expression_opt ::= expression",
 /*  23 */ "assignment_expression ::= conditional_expression",
 /*  24 */ "assignment_expression ::= binary_expression assignment_operator assignment_expression",
 /*  25 */ "assignment_operator ::= ASSIGN",
 /*  26 */ "assignment_operator ::= ASSADD",
 /*  27 */ "assignment_operator ::= ASSSUB",
 /*  28 */ "assignment_operator ::= ASSMUL",
 /*  29 */ "assignment_operator ::= ASSDIV",
 /*  30 */ "assignment_operator ::= ASSMOD",
 /*  31 */ "conditional_expression ::= binary_expression",
 /*  32 */ "conditional_expression ::= binary_expression QUESTION expression COLON assignment_expression",
 /*  33 */ "binary_expression ::= unary_expression",
 /*  34 */ "binary_expression ::= binary_expression LOGOR binary_expression",
 /*  35 */ "binary_expression ::= binary_expression LOGAND binary_expression",
 /*  36 */ "binary_expression ::= binary_expression BITOR binary_expression",
 /*  37 */ "binary_expression ::= binary_expression BITXOR binary_expression",
 /*  38 */ "binary_expression ::= binary_expression BITAND binary_expression",
 /*  39 */ "binary_expression ::= binary_expression EQUALS binary_expression",
 /*  40 */ "binary_expression ::= binary_expression NEQUALS binary_expression",
 /*  41 */ "binary_expression ::= binary_expression ST binary_expression",
 /*  42 */ "binary_expression ::= binary_expression SE binary_expression",
 /*  43 */ "binary_expression ::= binary_expression GT binary_expression",
 /*  44 */ "binary_expression ::= binary_expression GE binary_expression",
 /*  45 */ "binary_expression ::= binary_expression ADDOP binary_expression",
 /*  46 */ "binary_expression ::= binary_expression SUBOP binary_expression",
 /*  47 */ "binary_expression ::= binary_expression MULOP binary_expression",
 /*  48 */ "binary_expression ::= binary_expression DIVOP binary_expression",
 /*  49 */ "binary_expression ::= binary_expression MODOP binary_expression",
 /*  50 */ "binary_expression ::= binary_expression SEQ binary_expression",
 /*  51 */ "binary_expression ::= binary_expression SNE binary_expression",
 /*  52 */ "unary_expression ::= postfix_expression",
 /*  53 */ "unary_expression ::= SUBOP unary_expression",
 /*  54 */ "unary_expression ::= ADDADD unary_expression",
 /*  55 */ "unary_expression ::= SUBSUB unary_expression",
 /*  56 */ "unary_expression ::= NOT unary_expression",
 /*  57 */ "unary_expression ::= new_expression",
 /*  58 */ "postfix_expression ::= primary_expression",
 /*  59 */ "postfix_expression ::= postfix_expression ADDADD",
 /*  60 */ "postfix_expression ::= postfix_expression SUBSUB",
 /*  61 */ "postfix_expression ::= function_call",
 /*  62 */ "postfix_expression ::= postfix_expression DOT IDENTIFIER",
 /*  63 */ "postfix_expression ::= postfix_expression DOT function_call",
 /*  64 */ "postfix_expression ::= postfix_expression LBRACKET expression RBRACKET",
 /*  65 */ "function_call ::= IDENTIFIER LPAREN RPAREN",
 /*  66 */ "function_call ::= IDENTIFIER LPAREN argument_list RPAREN",
 /*  67 */ "primary_expression ::= literal",
 /*  68 */ "primary_expression ::= id_expression",
 /*  69 */ "primary_expression ::= LPAREN expression RPAREN",
 /*  70 */ "primary_expression ::= list_literal",
 /*  71 */ "primary_expression ::= THIS",
 /*  72 */ "literal ::= INTEGER",
 /*  73 */ "literal ::= HEX",
 /*  74 */ "literal ::= BIN",
 /*  75 */ "literal ::= ROM",
 /*  76 */ "literal ::= REAL",
 /*  77 */ "literal ::= STRING",
 /*  78 */ "literal ::= TRUE",
 /*  79 */ "literal ::= FALSE",
 /*  80 */ "literal ::= NULL",
 /*  81 */ "id_expression ::= IDENTIFIER",
 /*  82 */ "list_literal ::= LBRACKET list_content RBRACKET",
 /*  83 */ "list_content ::= list_entry",
 /*  84 */ "list_content ::= list_entry COMMA list_content",
 /*  85 */ "list_entry ::= expression",
 /*  86 */ "new_expression ::= NEW IDENTIFIER",
 /*  87 */ "argument ::= expression",
 /*  88 */ "argument_list ::= argument",
 /*  89 */ "argument_list ::= argument_list COMMA argument",
 /*  90 */ "expression_statement ::= SEMICOLON",
 /*  91 */ "expression_statement ::= expression SEMICOLON",
 /*  92 */ "compound_statement ::= LBRACE RBRACE",
 /*  93 */ "compound_statement ::= LBRACE statement_sequence RBRACE",
 /*  94 */ "include_statement ::= INCLUDE STRING SEMICOLON",
 /*  95 */ "return_statement ::= RETURN expression SEMICOLON",
 /*  96 */ "return_statement ::= RETURN SEMICOLON",
 /*  97 */ "break_statement ::= BREAK SEMICOLON",
 /*  98 */ "continue_statement ::= CONTINUE SEMICOLON",
 /*  99 */ "pause_statement ::= PAUSE SEMICOLON",
 /* 100 */ "declaration_statement ::= function_declaration",
 /* 101 */ "declaration_statement ::= class_declaration SEMICOLON",
 /* 102 */ "declaration_statement ::= variable_declaration SEMICOLON",
 /* 103 */ "variable_declaration ::= VAR declarator_sequence",
 /* 104 */ "declarator ::= IDENTIFIER",
 /* 105 */ "declarator ::= IDENTIFIER ASSIGN expression",
 /* 106 */ "declarator_sequence ::= declarator",
 /* 107 */ "declarator_sequence ::= declarator_sequence COMMA declarator",
 /* 108 */ "class_declaration ::= CLASS IDENTIFIER LBRACE RBRACE",
 /* 109 */ "class_declaration ::= CLASS IDENTIFIER LBRACE class_members RBRACE",
 /* 110 */ "class_member ::= variable_declaration SEMICOLON",
 /* 111 */ "class_member ::= function_declaration",
 /* 112 */ "class_member ::= access_specifier variable_declaration SEMICOLON",
 /* 113 */ "class_member ::= access_specifier function_declaration",
 /* 114 */ "class_member ::= access_specifier COLON",
 /* 115 */ "access_specifier ::= PRIVATE",
 /* 116 */ "access_specifier ::= PROTECTED",
 /* 117 */ "access_specifier ::= PUBLIC",
 /* 118 */ "class_members ::= class_member",
 /* 119 */ "class_members ::= class_members class_member",
 /* 120 */ "function_declaration ::= FUNCTION IDENTIFIER LPAREN parameter_list RPAREN function_body",
 /* 121 */ "function_declaration ::= FUNCTION IDENTIFIER LPAREN RPAREN function_body",
 /* 122 */ "parameter ::= IDENTIFIER",
 /* 123 */ "opt_parameter ::= IDENTIFIER ASSIGN expression",
 /* 124 */ "parameters ::= parameter",
 /* 125 */ "parameters ::= parameters COMMA parameter",
 /* 126 */ "opt_parameters ::= opt_parameter",
 /* 127 */ "opt_parameters ::= opt_parameters COMMA opt_parameter",
 /* 128 */ "parameter_list ::= parameters",
 /* 129 */ "parameter_list ::= opt_parameters",
 /* 130 */ "parameter_list ::= parameters COMMA opt_parameters",
 /* 131 */ "function_body ::= statement",
 /* 132 */ "for_statement ::= FOR LPAREN for_init_statement expression SEMICOLON expression_opt RPAREN statement",
 /* 133 */ "for_init_statement ::= expression_statement",
 /* 134 */ "for_init_statement ::= variable_declaration SEMICOLON",
 /* 135 */ "foreach_statement ::= FOR LPAREN id_expression IN expression RPAREN statement",
 /* 136 */ "foreach_statement ::= FOR LPAREN foreach_decl IN expression RPAREN statement",
 /* 137 */ "foreach_decl ::= VAR IDENTIFIER",
 /* 138 */ "if_statement ::= IF LPAREN expression RPAREN statement",
 /* 139 */ "if_statement ::= IF LPAREN expression RPAREN statement ELSE statement",
 /* 140 */ "while_statement ::= WHILE LPAREN expression RPAREN statement",
 /* 141 */ "switch_statement ::= SWITCH LPAREN expression RPAREN LBRACE switch_body RBRACE",
 /* 142 */ "switch_body ::=",
 /* 143 */ "switch_body ::= switch_body switch_case",
 /* 144 */ "switch_body ::= switch_body default_case",
 /* 145 */ "switch_case ::= CASE literal COLON case_statements",
 /* 146 */ "default_case ::= DEFAULT COLON case_statements",
 /* 147 */ "case_statements ::= statement_sequence",
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
  { 72, 1 },
  { 73, 1 },
  { 75, 1 },
  { 75, 2 },
  { 74, 0 },
  { 74, 1 },
  { 76, 1 },
  { 76, 1 },
  { 76, 1 },
  { 76, 1 },
  { 76, 1 },
  { 76, 1 },
  { 76, 1 },
  { 76, 1 },
  { 76, 1 },
  { 76, 1 },
  { 76, 1 },
  { 76, 1 },
  { 76, 1 },
  { 76, 1 },
  { 90, 1 },
  { 92, 0 },
  { 92, 1 },
  { 91, 1 },
  { 91, 3 },
  { 95, 1 },
  { 95, 1 },
  { 95, 1 },
  { 95, 1 },
  { 95, 1 },
  { 95, 1 },
  { 93, 1 },
  { 93, 5 },
  { 94, 1 },
  { 94, 3 },
  { 94, 3 },
  { 94, 3 },
  { 94, 3 },
  { 94, 3 },
  { 94, 3 },
  { 94, 3 },
  { 94, 3 },
  { 94, 3 },
  { 94, 3 },
  { 94, 3 },
  { 94, 3 },
  { 94, 3 },
  { 94, 3 },
  { 94, 3 },
  { 94, 3 },
  { 94, 3 },
  { 94, 3 },
  { 96, 1 },
  { 96, 2 },
  { 96, 2 },
  { 96, 2 },
  { 96, 2 },
  { 96, 1 },
  { 97, 1 },
  { 97, 2 },
  { 97, 2 },
  { 97, 1 },
  { 97, 3 },
  { 97, 3 },
  { 97, 4 },
  { 100, 3 },
  { 100, 4 },
  { 99, 1 },
  { 99, 1 },
  { 99, 3 },
  { 99, 1 },
  { 99, 1 },
  { 102, 1 },
  { 102, 1 },
  { 102, 1 },
  { 102, 1 },
  { 102, 1 },
  { 102, 1 },
  { 102, 1 },
  { 102, 1 },
  { 102, 1 },
  { 103, 1 },
  { 104, 3 },
  { 105, 1 },
  { 105, 3 },
  { 106, 1 },
  { 98, 2 },
  { 107, 1 },
  { 101, 1 },
  { 101, 3 },
  { 78, 1 },
  { 78, 2 },
  { 81, 2 },
  { 81, 3 },
  { 77, 3 },
  { 85, 3 },
  { 85, 2 },
  { 87, 2 },
  { 88, 2 },
  { 89, 2 },
  { 79, 1 },
  { 79, 2 },
  { 79, 2 },
  { 110, 2 },
  { 112, 1 },
  { 112, 3 },
  { 111, 1 },
  { 111, 3 },
  { 109, 4 },
  { 109, 5 },
  { 114, 2 },
  { 114, 1 },
  { 114, 3 },
  { 114, 2 },
  { 114, 2 },
  { 115, 1 },
  { 115, 1 },
  { 115, 1 },
  { 113, 1 },
  { 113, 2 },
  { 108, 6 },
  { 108, 5 },
  { 118, 1 },
  { 119, 3 },
  { 120, 1 },
  { 120, 3 },
  { 121, 1 },
  { 121, 3 },
  { 116, 1 },
  { 116, 1 },
  { 116, 3 },
  { 117, 1 },
  { 80, 8 },
  { 122, 1 },
  { 122, 2 },
  { 84, 7 },
  { 84, 7 },
  { 123, 2 },
  { 82, 5 },
  { 82, 7 },
  { 83, 5 },
  { 86, 7 },
  { 124, 0 },
  { 124, 2 },
  { 124, 2 },
  { 125, 4 },
  { 126, 3 },
  { 127, 1 },
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
#line 75 "astgen.in"
{ p->SetRoot(yymsp[0].minor.yy215); }
#line 1250 "astgen.c"
        break;
      case 1:
#line 78 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(translation_unit, yymsp[0].minor.yy215); }
#line 1255 "astgen.c"
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
      case 18:
      case 20:
      case 22:
      case 23:
      case 31:
      case 33:
      case 52:
      case 57:
      case 58:
      case 61:
      case 67:
      case 68:
      case 70:
      case 88:
      case 100:
      case 103:
      case 106:
      case 124:
      case 126:
      case 128:
      case 129:
      case 131:
      case 133:
      case 147:
#line 81 "astgen.in"
{ yygotominor.yy215 = yymsp[0].minor.yy215; }
#line 1297 "astgen.c"
        break;
      case 3:
#line 82 "astgen.in"
{ 
  if(yymsp[-1].minor.yy215->m_type == statement_sequence) {
    yygotominor.yy215 = yymsp[-1].minor.yy215;
  }
  else {
    yygotominor.yy215 = p->AllocAst(statement_sequence, new AstList);
    yygotominor.yy215->m_a1.GetList()->push_back(yymsp[-1].minor.yy215);
  }
  yygotominor.yy215->m_a1.GetList()->push_back(yymsp[0].minor.yy215);
}
#line 1311 "astgen.c"
        break;
      case 4:
      case 21:
#line 95 "astgen.in"
{ yygotominor.yy215 = 0; }
#line 1317 "astgen.c"
        break;
      case 19:
      case 90:
#line 114 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(empty_statement); }
#line 1323 "astgen.c"
        break;
      case 24:
#line 130 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(assignment_expression, yymsp[-1].minor.yy46, yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1328 "astgen.c"
        break;
      case 25:
#line 134 "astgen.in"
{ yygotominor.yy46 = op_assign; }
#line 1333 "astgen.c"
        break;
      case 26:
#line 135 "astgen.in"
{ yygotominor.yy46 = op_assadd; }
#line 1338 "astgen.c"
        break;
      case 27:
#line 136 "astgen.in"
{ yygotominor.yy46 = op_asssub; }
#line 1343 "astgen.c"
        break;
      case 28:
#line 137 "astgen.in"
{ yygotominor.yy46 = op_assmul; }
#line 1348 "astgen.c"
        break;
      case 29:
#line 138 "astgen.in"
{ yygotominor.yy46 = op_assdiv; }
#line 1353 "astgen.c"
        break;
      case 30:
#line 139 "astgen.in"
{ yygotominor.yy46 = op_assmod; }
#line 1358 "astgen.c"
        break;
      case 32:
#line 143 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(ternary_expression, yymsp[-4].minor.yy215, yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1363 "astgen.c"
        break;
      case 34:
#line 147 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(binary_expression, op_logor,   yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1368 "astgen.c"
        break;
      case 35:
#line 148 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(binary_expression, op_logand,  yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1373 "astgen.c"
        break;
      case 36:
#line 149 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(binary_expression, op_bitor,   yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1378 "astgen.c"
        break;
      case 37:
#line 150 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(binary_expression, op_bitxor,  yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1383 "astgen.c"
        break;
      case 38:
#line 151 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(binary_expression, op_bitand,  yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1388 "astgen.c"
        break;
      case 39:
#line 152 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(binary_expression, op_eq,   yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1393 "astgen.c"
        break;
      case 40:
#line 153 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(binary_expression, op_ne,   yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1398 "astgen.c"
        break;
      case 41:
#line 154 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(binary_expression, op_lt,   yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1403 "astgen.c"
        break;
      case 42:
#line 155 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(binary_expression, op_le,   yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1408 "astgen.c"
        break;
      case 43:
#line 156 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(binary_expression, op_gt,   yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1413 "astgen.c"
        break;
      case 44:
#line 157 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(binary_expression, op_ge,   yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1418 "astgen.c"
        break;
      case 45:
#line 158 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(binary_expression, op_add,  yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1423 "astgen.c"
        break;
      case 46:
#line 159 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(binary_expression, op_sub,  yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1428 "astgen.c"
        break;
      case 47:
#line 160 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(binary_expression, op_mul,  yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1433 "astgen.c"
        break;
      case 48:
#line 161 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(binary_expression, op_div,  yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1438 "astgen.c"
        break;
      case 49:
#line 162 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(binary_expression, op_mod,  yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1443 "astgen.c"
        break;
      case 50:
#line 163 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(binary_expression, op_seq,  yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1448 "astgen.c"
        break;
      case 51:
#line 164 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(binary_expression, op_sne,  yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1453 "astgen.c"
        break;
      case 53:
#line 168 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(prefix_expression, op_negate, yymsp[0].minor.yy215); }
#line 1458 "astgen.c"
        break;
      case 54:
#line 169 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(prefix_expression, op_preinc, yymsp[0].minor.yy215); }
#line 1463 "astgen.c"
        break;
      case 55:
#line 170 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(prefix_expression, op_predec, yymsp[0].minor.yy215); }
#line 1468 "astgen.c"
        break;
      case 56:
#line 171 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(prefix_expression, op_not,    yymsp[0].minor.yy215); }
#line 1473 "astgen.c"
        break;
      case 59:
#line 176 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(postfix_expression, op_postinc, yymsp[-1].minor.yy215); }
#line 1478 "astgen.c"
        break;
      case 60:
#line 177 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(postfix_expression, op_postdec, yymsp[-1].minor.yy215); }
#line 1483 "astgen.c"
        break;
      case 62:
#line 179 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(member_expression, yymsp[-2].minor.yy215, String(yymsp[0].minor.yy0)); }
#line 1488 "astgen.c"
        break;
      case 63:
#line 180 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(member_call, yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1493 "astgen.c"
        break;
      case 64:
#line 181 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(index_expression, yymsp[-3].minor.yy215, yymsp[-1].minor.yy215); }
#line 1498 "astgen.c"
        break;
      case 65:
#line 184 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(function_call, String(yymsp[-2].minor.yy0)); }
#line 1503 "astgen.c"
        break;
      case 66:
#line 185 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(function_call, String(yymsp[-3].minor.yy0), yymsp[-1].minor.yy215); }
#line 1508 "astgen.c"
        break;
      case 69:
      case 101:
      case 102:
      case 134:
#line 191 "astgen.in"
{ yygotominor.yy215 = yymsp[-1].minor.yy215; }
#line 1516 "astgen.c"
        break;
      case 71:
#line 193 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(this_expression); }
#line 1521 "astgen.c"
        break;
      case 72:
#line 196 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(literal, Variant(String(yymsp[0].minor.yy0), Variant::stInt)); }
#line 1526 "astgen.c"
        break;
      case 73:
#line 197 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(literal, Variant(hex2dec(String(yymsp[0].minor.yy0).c_str()))); }
#line 1531 "astgen.c"
        break;
      case 74:
#line 198 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(literal, Variant(bin2dec(String(yymsp[0].minor.yy0).c_str()))); }
#line 1536 "astgen.c"
        break;
      case 75:
#line 199 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(literal, Variant(rom2dec(String(yymsp[0].minor.yy0).c_str()))); }
#line 1541 "astgen.c"
        break;
      case 76:
#line 200 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(literal, Variant(String(yymsp[0].minor.yy0), Variant::stReal)); }
#line 1546 "astgen.c"
        break;
      case 77:
#line 201 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(literal, Variant(String(yymsp[0].minor.yy0), Variant::stString)); }
#line 1551 "astgen.c"
        break;
      case 78:
#line 202 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(literal, Variant(true));    }
#line 1556 "astgen.c"
        break;
      case 79:
#line 203 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(literal, Variant(false));   }
#line 1561 "astgen.c"
        break;
      case 80:
#line 204 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(literal, Variant());        }
#line 1566 "astgen.c"
        break;
      case 81:
#line 207 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(lvalue, String(yymsp[0].minor.yy0)); }
#line 1571 "astgen.c"
        break;
      case 82:
#line 210 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(list_literal, yymsp[-1].minor.yy215); }
#line 1576 "astgen.c"
        break;
      case 83:
#line 211 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(list_content, yymsp[0].minor.yy215); }
#line 1581 "astgen.c"
        break;
      case 84:
#line 212 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(list_content, yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1586 "astgen.c"
        break;
      case 85:
#line 214 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(list_entry, yymsp[0].minor.yy215); }
#line 1591 "astgen.c"
        break;
      case 86:
#line 217 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(new_expression, String(yymsp[0].minor.yy0)); }
#line 1596 "astgen.c"
        break;
      case 87:
#line 226 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(argument, yymsp[0].minor.yy215); }
#line 1601 "astgen.c"
        break;
      case 89:
#line 230 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(argument_list, yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1606 "astgen.c"
        break;
      case 91:
#line 239 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(expression_statement, yymsp[-1].minor.yy215); }
#line 1611 "astgen.c"
        break;
      case 92:
#line 242 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(compound_statement); }
#line 1616 "astgen.c"
        break;
      case 93:
#line 243 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(compound_statement, yymsp[-1].minor.yy215); }
#line 1621 "astgen.c"
        break;
      case 94:
#line 246 "astgen.in"
{ p->Parse(yymsp[-1].minor.yy0); yygotominor.yy215 = p->GetRoot(); }
#line 1626 "astgen.c"
        break;
      case 95:
#line 249 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(return_statement, yymsp[-1].minor.yy215); }
#line 1631 "astgen.c"
        break;
      case 96:
#line 250 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(return_statement);    }
#line 1636 "astgen.c"
        break;
      case 97:
#line 253 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(break_statement); }
#line 1641 "astgen.c"
        break;
      case 98:
#line 254 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(continue_statement); }
#line 1646 "astgen.c"
        break;
      case 99:
#line 257 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(pause_statement); }
#line 1651 "astgen.c"
        break;
      case 104:
#line 271 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(variable_declaration, String(yymsp[0].minor.yy0));    }
#line 1656 "astgen.c"
        break;
      case 105:
#line 272 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(variable_declaration, String(yymsp[-2].minor.yy0), yymsp[0].minor.yy215); }
#line 1661 "astgen.c"
        break;
      case 107:
#line 275 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(declaration_sequence, yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1666 "astgen.c"
        break;
      case 108:
#line 282 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(class_declaration, String(yymsp[-2].minor.yy0)); }
#line 1671 "astgen.c"
        break;
      case 109:
#line 283 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(class_declaration, String(yymsp[-3].minor.yy0), yymsp[-1].minor.yy87); }
#line 1676 "astgen.c"
        break;
      case 110:
#line 286 "astgen.in"
{ yygotominor.yy215 = yymsp[-1].minor.yy215; yymsp[-1].minor.yy215->m_props["access"] = accessDefault; }
#line 1681 "astgen.c"
        break;
      case 111:
#line 287 "astgen.in"
{ yygotominor.yy215 = yymsp[0].minor.yy215; yymsp[0].minor.yy215->m_props["access"] = accessDefault; }
#line 1686 "astgen.c"
        break;
      case 112:
#line 288 "astgen.in"
{ yygotominor.yy215 = yymsp[-1].minor.yy215; yymsp[-1].minor.yy215->m_props["access"] = yymsp[-2].minor.yy156; }
#line 1691 "astgen.c"
        break;
      case 113:
#line 289 "astgen.in"
{ yygotominor.yy215 = yymsp[0].minor.yy215; yymsp[0].minor.yy215->m_props["access"] = yymsp[-1].minor.yy156; }
#line 1696 "astgen.c"
        break;
      case 114:
#line 290 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(access_specifier, yymsp[-1].minor.yy156); }
#line 1701 "astgen.c"
        break;
      case 115:
#line 294 "astgen.in"
{ yygotominor.yy156 = accessPrivate;   }
#line 1706 "astgen.c"
        break;
      case 116:
#line 295 "astgen.in"
{ yygotominor.yy156 = accessProtected; }
#line 1711 "astgen.c"
        break;
      case 117:
#line 296 "astgen.in"
{ yygotominor.yy156 = accessPublic;    }
#line 1716 "astgen.c"
        break;
      case 118:
#line 300 "astgen.in"
{ 
  yygotominor.yy87 = new AstList;
  yygotominor.yy87->push_back(yymsp[0].minor.yy215);
}
#line 1724 "astgen.c"
        break;
      case 119:
#line 304 "astgen.in"
{ 
  yygotominor.yy87 = yymsp[-1].minor.yy87;
  yygotominor.yy87->push_back(yymsp[0].minor.yy215);
}
#line 1732 "astgen.c"
        break;
      case 120:
#line 315 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(function_declaration, String(yymsp[-4].minor.yy0), yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1737 "astgen.c"
        break;
      case 121:
#line 316 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(function_declaration, String(yymsp[-3].minor.yy0), AstData(), yymsp[0].minor.yy215); }
#line 1742 "astgen.c"
        break;
      case 122:
#line 319 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(parameter, String(yymsp[0].minor.yy0)); }
#line 1747 "astgen.c"
        break;
      case 123:
#line 322 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(parameter, String(yymsp[-2].minor.yy0), yymsp[0].minor.yy215); }
#line 1752 "astgen.c"
        break;
      case 125:
      case 127:
      case 130:
#line 326 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(parameter_list, yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1759 "astgen.c"
        break;
      case 132:
#line 347 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(for_statement, yymsp[-5].minor.yy215, yymsp[-4].minor.yy215, yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1764 "astgen.c"
        break;
      case 135:
      case 136:
#line 358 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(foreach_statement, yymsp[-4].minor.yy215, yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1770 "astgen.c"
        break;
      case 137:
#line 360 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(variable_declaration, String(yymsp[0].minor.yy0)); }
#line 1775 "astgen.c"
        break;
      case 138:
#line 371 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(if_statement, yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1780 "astgen.c"
        break;
      case 139:
#line 372 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(if_statement, yymsp[-4].minor.yy215, yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1785 "astgen.c"
        break;
      case 140:
#line 380 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(while_statement, yymsp[-2].minor.yy215,  yymsp[0].minor.yy215); }
#line 1790 "astgen.c"
        break;
      case 141:
#line 388 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(switch_statement, yymsp[-4].minor.yy215, yymsp[-1].minor.yy87); }
#line 1795 "astgen.c"
        break;
      case 142:
#line 392 "astgen.in"
{ yygotominor.yy87 = new AstList; }
#line 1800 "astgen.c"
        break;
      case 143:
      case 144:
#line 393 "astgen.in"
{ yygotominor.yy87 = yymsp[-1].minor.yy87; yygotominor.yy87->push_back(yymsp[0].minor.yy215); }
#line 1806 "astgen.c"
        break;
      case 145:
#line 397 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(switch_case, yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1811 "astgen.c"
        break;
      case 146:
#line 400 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(default_case, yymsp[0].minor.yy215); }
#line 1816 "astgen.c"
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
#line 54 "astgen.in"

  p->OnParseFailure();
#line 1864 "astgen.c"
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
#line 57 "astgen.in"

  p->OnSyntaxError();
#line 1882 "astgen.c"
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

