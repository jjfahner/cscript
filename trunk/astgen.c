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
 /*     0 */   218,  402,  143,  244,   11,  243,  239,  238,  235,  234,
 /*    10 */   232,  230,  229,  228,  227,  225,  222,  221,  219,  124,
 /*    20 */   217,  245,  216,   65,  140,  213,   84,  202,  201,  200,
 /*    30 */    56,  199,  198,  195,  166,  182,  135,  145,  126,  121,
 /*    40 */   250,   61,   55,   54,   53,  171,  100,   17,   19,   26,
 /*    50 */    24,  194,  193,  191,  190,  189,  188,  187,  186,  184,
 /*    60 */   183,  130,  122,  146,    3,  252,  132,   21,  131,  128,
 /*    70 */   127,   63,  113,   56,  156,   63,   98,  125,  205,  206,
 /*    80 */    98,  120,   90,  129,   93,   55,   54,   53,    5,  100,
 /*    90 */    17,  170,   26,   97,  194,  193,  191,  190,  189,  188,
 /*   100 */   187,  186,  184,  183,   22,  122,  146,    3,  139,  132,
 /*   110 */    21,  131,  128,  127,   63,  113,   38,   39,   46,   98,
 /*   120 */   125,  117,  157,  218,  120,   90,  129,    7,  243,  239,
 /*   130 */   238,  235,  234,  232,  230,  229,  228,  227,  225,  222,
 /*   140 */   221,  219,  124,  217,  109,  216,   65,  174,  213,   84,
 /*   150 */   202,  201,  200,  142,  199,  198,  195,  161,   20,   56,
 /*   160 */   145,  126,  121,   63,  110,  247,  248,  249,   98,  144,
 /*   170 */   133,   55,   54,   53,  147,  100,   17,   93,   26,  211,
 /*   180 */   194,  193,  191,  190,  189,  188,  187,  186,  184,  183,
 /*   190 */    15,  122,  146,    3,   88,  132,   21,  131,  128,  127,
 /*   200 */    63,  113,  246,  141,   60,   98,  125,  116,  179,  218,
 /*   210 */   120,   90,  129,    7,  243,  239,  238,  235,  234,  232,
 /*   220 */   230,  229,  228,  227,  225,  222,  221,  219,  124,  217,
 /*   230 */    23,  216,   65,  149,  213,   84,  202,  201,  200,  137,
 /*   240 */   199,  198,  195,   32,  218,  167,  145,  126,  121,  178,
 /*   250 */   239,  238,  235,  234,  232,  230,  229,  228,  227,  225,
 /*   260 */   222,  221,  219,  124,  217,  214,  216,   65,    2,  213,
 /*   270 */    84,  202,  201,  200,  390,  199,  198,  195,    4,   33,
 /*   280 */    67,  145,  126,  121,  212,  210,  218,   69,   62,   31,
 /*   290 */   177,  178,  239,  238,  235,  234,  232,  230,  229,  228,
 /*   300 */   227,  225,  222,  221,  219,  124,  217,  165,  216,   65,
 /*   310 */    16,  213,   84,  202,  201,  200,   25,  199,  198,  195,
 /*   320 */    57,  164,   18,  145,  126,  121,   10,  115,  226,  218,
 /*   330 */     9,    1,  233,    6,  243,  239,  238,  235,  234,  232,
 /*   340 */   230,  229,  228,  227,  225,  222,  221,  219,  124,  217,
 /*   350 */    13,  216,   65,   12,  213,   84,  202,  201,  200,  196,
 /*   360 */   199,  198,  195,   23,  218,  169,  145,  126,  121,  223,
 /*   370 */   239,  238,  235,  234,  232,  230,  229,  228,  227,  225,
 /*   380 */   222,  221,  219,  124,  217,    8,  216,   65,   99,  213,
 /*   390 */    84,  202,  201,  200,   58,  199,  198,  195,   34,   27,
 /*   400 */    14,  145,  126,  121,  218,  240,  241,   70,  103,  242,
 /*   410 */   239,  238,  235,  234,  232,  230,  229,  228,  227,  225,
 /*   420 */   222,  221,  219,  124,  217,   30,  216,   65,  104,  213,
 /*   430 */    84,  202,  201,  200,   68,  199,  198,  195,  403,  218,
 /*   440 */   403,  145,  126,  121,  185,  239,  238,  235,  234,  232,
 /*   450 */   230,  229,  228,  227,  225,  222,  221,  219,  124,  217,
 /*   460 */   403,  216,   65,  403,  213,   84,  202,  201,  200,  403,
 /*   470 */   199,  198,  195,  403,  218,  403,  145,  126,  121,  197,
 /*   480 */   239,  238,  235,  234,  232,  230,  229,  228,  227,  225,
 /*   490 */   222,  221,  219,  124,  217,  403,  216,   65,  403,  213,
 /*   500 */    84,  202,  201,  200,  403,  199,  198,  195,  403,  218,
 /*   510 */   403,  145,  126,  121,  108,  239,  238,  235,  234,  232,
 /*   520 */   230,  229,  228,  227,  225,  222,  221,  219,  124,  217,
 /*   530 */   403,  216,   65,  403,  213,   84,  202,  201,  200,  403,
 /*   540 */   199,  198,  195,  403,  218,  403,  145,  126,  121,  192,
 /*   550 */   239,  238,  235,  234,  232,  230,  229,  228,  227,  225,
 /*   560 */   222,  221,  219,  124,  217,  403,  216,   65,  403,  213,
 /*   570 */    84,  202,  201,  200,  403,  199,  198,  195,  403,  218,
 /*   580 */   403,  145,  126,  121,  231,  239,  238,  235,  234,  232,
 /*   590 */   230,  229,  228,  227,  225,  222,  221,  219,  124,  217,
 /*   600 */   403,  216,   65,  403,  213,   84,  202,  201,  200,  403,
 /*   610 */   199,  198,  195,  403,  403,  403,  145,  126,  121,   51,
 /*   620 */    49,   48,   47,   45,   50,   44,   43,   42,   41,   40,
 /*   630 */    52,   35,   36,   37,   38,   39,   46,  162,  160,  159,
 /*   640 */   158,  154,  153,   29,   56,   43,   42,   41,   40,   52,
 /*   650 */    35,   36,   37,   38,   39,   46,   55,   54,   53,  403,
 /*   660 */   100,   17,  403,   26,  403,  194,  193,  191,  190,  189,
 /*   670 */   188,  187,  186,  184,  183,  403,  122,  146,   72,   56,
 /*   680 */   213,   84,  202,  201,  200,   64,  199,  198,  195,  403,
 /*   690 */   403,   55,   54,   53,  403,  100,   17,  403,   26,  175,
 /*   700 */   194,  193,  191,  190,  189,  188,  187,  186,  184,  183,
 /*   710 */   403,  122,   56,   77,  403,  213,   84,  202,  201,  200,
 /*   720 */   403,  199,  198,  195,   55,   54,   53,  403,  100,   17,
 /*   730 */   403,   26,  403,  194,  193,  191,  190,  189,  188,  187,
 /*   740 */   186,  184,  183,  403,  122,  138,   86,   56,  213,   84,
 /*   750 */   202,  201,  200,  403,  199,  198,  195,  403,  403,   55,
 /*   760 */    54,   53,  403,  100,   17,  403,   26,  403,  194,  193,
 /*   770 */   191,  190,  189,  188,  187,  186,  184,  183,  403,  122,
 /*   780 */    49,   48,   47,   45,   50,   44,   43,   42,   41,   40,
 /*   790 */    52,   35,   36,   37,   38,   39,   46,   48,   47,   45,
 /*   800 */    50,   44,   43,   42,   41,   40,   52,   35,   36,   37,
 /*   810 */    38,   39,   46,  224,   71,  403,  213,   84,  202,  201,
 /*   820 */   200,  204,  199,  198,  195,  124,  217,  403,  216,   65,
 /*   830 */   403,  213,   84,  202,  201,  200,  403,  199,  106,  195,
 /*   840 */    66,  119,  403,  403,   74,   94,  213,   84,  202,  201,
 /*   850 */   200,  403,  199,  198,  195,  403,  403,   28,  101,  403,
 /*   860 */   403,  403,   47,   45,   50,   44,   43,   42,   41,   40,
 /*   870 */    52,   35,   36,   37,   38,   39,   46,  180,  217,  403,
 /*   880 */   216,   65,  403,  213,   84,  202,  201,  200,  403,  199,
 /*   890 */   198,  195,   89,  176,  403,   45,   50,   44,   43,   42,
 /*   900 */    41,   40,   52,   35,   36,   37,   38,   39,   46,  403,
 /*   910 */    36,   37,   38,   39,   46,  403,  173,  217,  403,  216,
 /*   920 */    65,  403,  213,   84,  202,  201,  200,   87,  199,  198,
 /*   930 */   195,  403,  403,  168,  403,  403,  220,  217,  112,  216,
 /*   940 */    65,  403,  213,   84,  202,  201,  200,  403,  199,  198,
 /*   950 */   195,   50,   44,   43,   42,   41,   40,   52,   35,   36,
 /*   960 */    37,   38,   39,   46,  173,  217,  403,  216,   65,  403,
 /*   970 */   213,   84,  202,  201,  200,  403,  199,  198,  195,  403,
 /*   980 */   403,  172,  180,  217,  403,  216,   65,  403,  213,   84,
 /*   990 */   202,  201,  200,  403,  199,  198,  195,  403,  181,  105,
 /*  1000 */   217,  403,  216,   65,  403,  213,   84,  202,  201,  200,
 /*  1010 */   403,  199,  198,  195,  134,  217,  403,  216,   65,  403,
 /*  1020 */   213,   84,  202,  201,  200,  403,  199,  198,  195,  107,
 /*  1030 */   217,  403,  216,   65,  403,  213,   84,  202,  201,  200,
 /*  1040 */   403,  199,  198,  195,   91,  217,  403,  216,   65,  403,
 /*  1050 */   213,   84,  202,  201,  200,  403,  199,  198,  195,  114,
 /*  1060 */   217,  403,  216,   65,  403,  213,   84,  202,  201,  200,
 /*  1070 */   403,  199,  198,  195,  111,  217,  403,  216,   65,  403,
 /*  1080 */   213,   84,  202,  201,  200,  403,  199,  198,  195,   95,
 /*  1090 */   217,  403,  216,   65,  403,  213,   84,  202,  201,  200,
 /*  1100 */   403,  199,  198,  195,  155,  217,  403,  216,   65,  403,
 /*  1110 */   213,   84,  202,  201,  200,  403,  199,  198,  195,   96,
 /*  1120 */   217,  403,  216,   65,  403,  213,   84,  202,  201,  200,
 /*  1130 */   403,  199,  198,  195,  236,  217,  403,  216,   65,  403,
 /*  1140 */   213,   84,  202,  201,  200,  403,  199,  198,  195,   92,
 /*  1150 */   217,  403,  216,   65,  403,  213,   84,  202,  201,  200,
 /*  1160 */   403,  199,  198,  195,  102,  217,  403,  216,   65,  403,
 /*  1170 */   213,   84,  202,  201,  200,  403,  199,  198,  195,  152,
 /*  1180 */   403,  216,   65,  403,  213,   84,  202,  201,  200,  403,
 /*  1190 */   199,  198,  195,  215,  403,  216,   65,  403,  213,   84,
 /*  1200 */   202,  201,  200,  403,  199,  198,  195,   82,  403,  213,
 /*  1210 */    84,  202,  201,  200,  403,  199,  198,  195,  148,  403,
 /*  1220 */   213,   84,  202,  201,  200,  403,  199,  198,  195,   41,
 /*  1230 */    40,   52,   35,   36,   37,   38,   39,   46,   75,  403,
 /*  1240 */   213,   84,  202,  201,  200,  403,  199,  198,  195,  403,
 /*  1250 */    76,  403,  213,   84,  202,  201,  200,  403,  199,  198,
 /*  1260 */   195,  403,   79,  403,  213,   84,  202,  201,  200,  403,
 /*  1270 */   199,  198,  195,   78,  403,  213,   84,  202,  201,  200,
 /*  1280 */   403,  199,  198,  195,  403,   83,  403,  213,   84,  202,
 /*  1290 */   201,  200,  403,  199,  198,  195,   81,  403,  213,   84,
 /*  1300 */   202,  201,  200,  403,  199,  198,  195,  150,  403,  213,
 /*  1310 */    84,  202,  201,  200,  403,  199,  198,  195,  151,  403,
 /*  1320 */   213,   84,  202,  201,  200,  403,  199,  198,  195,   80,
 /*  1330 */   403,  213,   84,  202,  201,  200,  403,  199,  198,  195,
 /*  1340 */   403,   73,  403,  213,   84,  202,  201,  200,  403,  199,
 /*  1350 */   198,  195,  403,  193,  191,  190,  189,  188,  187,  186,
 /*  1360 */   184,  183,   85,  403,  213,   84,  202,  201,  200,  403,
 /*  1370 */   199,  198,  195,  209,   84,  202,  201,  200,  403,  199,
 /*  1380 */   198,  195,  208,   84,  202,  201,  200,  403,  199,  198,
 /*  1390 */   195,  207,   84,  202,  201,  200,  403,  199,  198,  195,
 /*  1400 */   403,  203,   84,  202,  201,  200,  163,  199,  198,  195,
 /*  1410 */   403,  403,   63,  403,  247,  248,  249,   98,  118,  403,
 /*  1420 */   237,  241,  123,  136,  403,  166,  403,  135,  403,  403,
 /*  1430 */    59,  251,   61,
};
static const YYCODETYPE yy_lookahead[] = {
 /*     0 */    71,   72,   73,   74,   75,   76,   77,   78,   79,   80,
 /*    10 */    81,   82,   83,   84,   85,   86,   87,   88,   89,   90,
 /*    20 */    91,  119,   93,   94,   48,   96,   97,   98,   99,  100,
 /*    30 */    15,  102,  103,  104,  108,   33,  110,  108,  109,  110,
 /*    40 */   114,  115,   27,   28,   29,   26,   31,   32,   46,   34,
 /*    50 */    19,   36,   37,   38,   39,   40,   41,   42,   43,   44,
 /*    60 */    45,   31,   47,   48,   49,   50,   51,   52,   53,   54,
 /*    70 */    55,   56,   57,   15,  112,   56,   61,   62,  125,  126,
 /*    80 */    61,   66,   67,   68,   31,   27,   28,   29,   35,   31,
 /*    90 */    32,  108,   34,  110,   36,   37,   38,   39,   40,   41,
 /*   100 */    42,   43,   44,   45,   34,   47,   48,   49,   50,   51,
 /*   110 */    52,   53,   54,   55,   56,   57,   16,   17,   18,   61,
 /*   120 */    62,  111,  112,   71,   66,   67,   68,   75,   76,   77,
 /*   130 */    78,   79,   80,   81,   82,   83,   84,   85,   86,   87,
 /*   140 */    88,   89,   90,   91,   31,   93,   94,   35,   96,   97,
 /*   150 */    98,   99,  100,   48,  102,  103,  104,   50,   46,   15,
 /*   160 */   108,  109,  110,   56,  102,   58,   59,   60,   61,   48,
 /*   170 */    42,   27,   28,   29,   48,   31,   32,   31,   34,  127,
 /*   180 */    36,   37,   38,   39,   40,   41,   42,   43,   44,   45,
 /*   190 */    34,   47,   48,   49,   31,   51,   52,   53,   54,   55,
 /*   200 */    56,   57,   48,   48,   46,   61,   62,   31,   31,   71,
 /*   210 */    66,   67,   68,   75,   76,   77,   78,   79,   80,   81,
 /*   220 */    82,   83,   84,   85,   86,   87,   88,   89,   90,   91,
 /*   230 */    19,   93,   94,   48,   96,   97,   98,   99,  100,   48,
 /*   240 */   102,  103,  104,   34,   71,  100,  108,  109,  110,   76,
 /*   250 */    77,   78,   79,   80,   81,   82,   83,   84,   85,   86,
 /*   260 */    87,   88,   89,   90,   91,  127,   93,   94,   26,   96,
 /*   270 */    97,   98,   99,  100,   63,  102,  103,  104,   35,   95,
 /*   280 */    46,  108,  109,  110,   27,   28,   71,   30,  124,   32,
 /*   290 */   117,   76,   77,   78,   79,   80,   81,   82,   83,   84,
 /*   300 */    85,   86,   87,   88,   89,   90,   91,   48,   93,   94,
 /*   310 */    34,   96,   97,   98,   99,  100,   34,  102,  103,  104,
 /*   320 */    49,   33,   48,  108,  109,  110,   35,   31,   48,   71,
 /*   330 */    35,   26,  117,   75,   76,   77,   78,   79,   80,   81,
 /*   340 */    82,   83,   84,   85,   86,   87,   88,   89,   90,   91,
 /*   350 */    35,   93,   94,   35,   96,   97,   98,   99,  100,   35,
 /*   360 */   102,  103,  104,   19,   71,   48,  108,  109,  110,   76,
 /*   370 */    77,   78,   79,   80,   81,   82,   83,   84,   85,   86,
 /*   380 */    87,   88,   89,   90,   91,   65,   93,   94,   31,   96,
 /*   390 */    97,   98,   99,  100,   34,  102,  103,  104,   26,   63,
 /*   400 */    35,  108,  109,  110,   71,  118,  119,   49,  121,   76,
 /*   410 */    77,   78,   79,   80,   81,   82,   83,   84,   85,   86,
 /*   420 */    87,   88,   89,   90,   91,   63,   93,   94,   35,   96,
 /*   430 */    97,   98,   99,  100,   46,  102,  103,  104,  128,   71,
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
 /*   820 */   100,   50,  102,  103,  104,   90,   91,  128,   93,   94,
 /*   830 */   128,   96,   97,   98,   99,  100,  128,  102,  103,  104,
 /*   840 */    69,   70,  128,  128,   94,  110,   96,   97,   98,   99,
 /*   850 */   100,  128,  102,  103,  104,  128,  128,  122,  123,  128,
 /*   860 */   128,  128,    4,    5,    6,    7,    8,    9,   10,   11,
 /*   870 */    12,   13,   14,   15,   16,   17,   18,   90,   91,  128,
 /*   880 */    93,   94,  128,   96,   97,   98,   99,  100,  128,  102,
 /*   890 */   103,  104,  105,  106,  128,    5,    6,    7,    8,    9,
 /*   900 */    10,   11,   12,   13,   14,   15,   16,   17,   18,  128,
 /*   910 */    14,   15,   16,   17,   18,  128,   90,   91,  128,   93,
 /*   920 */    94,  128,   96,   97,   98,   99,  100,  101,  102,  103,
 /*   930 */   104,  128,  128,  107,  128,  128,   90,   91,   92,   93,
 /*   940 */    94,  128,   96,   97,   98,   99,  100,  128,  102,  103,
 /*   950 */   104,    6,    7,    8,    9,   10,   11,   12,   13,   14,
 /*   960 */    15,   16,   17,   18,   90,   91,  128,   93,   94,  128,
 /*   970 */    96,   97,   98,   99,  100,  128,  102,  103,  104,  128,
 /*   980 */   128,  107,   90,   91,  128,   93,   94,  128,   96,   97,
 /*   990 */    98,   99,  100,  128,  102,  103,  104,  128,  106,   90,
 /*  1000 */    91,  128,   93,   94,  128,   96,   97,   98,   99,  100,
 /*  1010 */   128,  102,  103,  104,   90,   91,  128,   93,   94,  128,
 /*  1020 */    96,   97,   98,   99,  100,  128,  102,  103,  104,   90,
 /*  1030 */    91,  128,   93,   94,  128,   96,   97,   98,   99,  100,
 /*  1040 */   128,  102,  103,  104,   90,   91,  128,   93,   94,  128,
 /*  1050 */    96,   97,   98,   99,  100,  128,  102,  103,  104,   90,
 /*  1060 */    91,  128,   93,   94,  128,   96,   97,   98,   99,  100,
 /*  1070 */   128,  102,  103,  104,   90,   91,  128,   93,   94,  128,
 /*  1080 */    96,   97,   98,   99,  100,  128,  102,  103,  104,   90,
 /*  1090 */    91,  128,   93,   94,  128,   96,   97,   98,   99,  100,
 /*  1100 */   128,  102,  103,  104,   90,   91,  128,   93,   94,  128,
 /*  1110 */    96,   97,   98,   99,  100,  128,  102,  103,  104,   90,
 /*  1120 */    91,  128,   93,   94,  128,   96,   97,   98,   99,  100,
 /*  1130 */   128,  102,  103,  104,   90,   91,  128,   93,   94,  128,
 /*  1140 */    96,   97,   98,   99,  100,  128,  102,  103,  104,   90,
 /*  1150 */    91,  128,   93,   94,  128,   96,   97,   98,   99,  100,
 /*  1160 */   128,  102,  103,  104,   90,   91,  128,   93,   94,  128,
 /*  1170 */    96,   97,   98,   99,  100,  128,  102,  103,  104,   91,
 /*  1180 */   128,   93,   94,  128,   96,   97,   98,   99,  100,  128,
 /*  1190 */   102,  103,  104,   91,  128,   93,   94,  128,   96,   97,
 /*  1200 */    98,   99,  100,  128,  102,  103,  104,   94,  128,   96,
 /*  1210 */    97,   98,   99,  100,  128,  102,  103,  104,   94,  128,
 /*  1220 */    96,   97,   98,   99,  100,  128,  102,  103,  104,   10,
 /*  1230 */    11,   12,   13,   14,   15,   16,   17,   18,   94,  128,
 /*  1240 */    96,   97,   98,   99,  100,  128,  102,  103,  104,  128,
 /*  1250 */    94,  128,   96,   97,   98,   99,  100,  128,  102,  103,
 /*  1260 */   104,  128,   94,  128,   96,   97,   98,   99,  100,  128,
 /*  1270 */   102,  103,  104,   94,  128,   96,   97,   98,   99,  100,
 /*  1280 */   128,  102,  103,  104,  128,   94,  128,   96,   97,   98,
 /*  1290 */    99,  100,  128,  102,  103,  104,   94,  128,   96,   97,
 /*  1300 */    98,   99,  100,  128,  102,  103,  104,   94,  128,   96,
 /*  1310 */    97,   98,   99,  100,  128,  102,  103,  104,   94,  128,
 /*  1320 */    96,   97,   98,   99,  100,  128,  102,  103,  104,   94,
 /*  1330 */   128,   96,   97,   98,   99,  100,  128,  102,  103,  104,
 /*  1340 */   128,   94,  128,   96,   97,   98,   99,  100,  128,  102,
 /*  1350 */   103,  104,  128,   37,   38,   39,   40,   41,   42,   43,
 /*  1360 */    44,   45,   94,  128,   96,   97,   98,   99,  100,  128,
 /*  1370 */   102,  103,  104,   96,   97,   98,   99,  100,  128,  102,
 /*  1380 */   103,  104,   96,   97,   98,   99,  100,  128,  102,  103,
 /*  1390 */   104,   96,   97,   98,   99,  100,  128,  102,  103,  104,
 /*  1400 */   128,   96,   97,   98,   99,  100,   50,  102,  103,  104,
 /*  1410 */   128,  128,   56,  128,   58,   59,   60,   61,  116,  128,
 /*  1420 */   118,  119,  120,  121,  128,  108,  128,  110,  128,  128,
 /*  1430 */   113,  114,  115,
};
#define YY_SHIFT_USE_DFLT (-25)
#define YY_SHIFT_MAX 136
static const short yy_shift_ofst[] = {
 /*     0 */   144,  144,  144,   15,  144,  144,   58,  144,  144,  144,
 /*    10 */   144,  144,  144,  144,  144,  629,  664,  732,  732,  732,
 /*    20 */   732,  697,  732,  732,  732,  732,  732,  732,  732,  732,
 /*    30 */   732,  732,  732,  732,  732,  732,  732,  732,  732,  732,
 /*    40 */   732,  732,  732,  732,  732,  732,  732,  732,  732,  732,
 /*    50 */   732,  732,  732,  732,  732,  732,  732,  107,   53, 1356,
 /*    60 */   146,   19,  771,  113,  163,  618, 1316,  113,   30,  176,
 /*    70 */   -25,  778,  794,  858,  890,  945,  637,  637, 1219, 1219,
 /*    80 */   896,  896,  896,  896,  257,  100,  100,  112,  211,    2,
 /*    90 */   282,  288,  291,   31,  280,  318,  324,  317,  357,  360,
 /*   100 */   276,  362,  393,  388,  358,  365,  336,  372,  320,  344,
 /*   110 */   305,  315,  295,  296,  274,  271,  276,  234,  243,  242,
 /*   120 */   209,  185,  177,  158,  155,  156,  126,  121,  105,   70,
 /*   130 */    31,  -24,  128,  154,  191,  259,  388,
};
#define YY_REDUCE_USE_DFLT (-99)
#define YY_REDUCE_MAX 70
static const short yy_reduce_ofst[] = {
 /*     0 */   -71,   52,  138,  258,  173,  215,  333,  333,  473,  293,
 /*    10 */   403,  333,  368,  438,  508,  735,  826,  787,  846,  892,
 /*    20 */   874,  924, 1074, 1014, 1044, 1059, 1029,  999,  969,  939,
 /*    30 */   909,  954,  984, 1102, 1088, 1235, 1268,  652, 1224, 1213,
 /*    40 */  1202, 1191, 1179, 1168, 1156, 1144, 1124,  750, 1247,  584,
 /*    50 */   619,  720, 1113, 1305, 1295, 1286, 1277, 1317, 1302,  -74,
 /*    60 */   287,  -17,  -47,   10,   10,  184,   62,  -38,  -98,  145,
 /*    70 */   164,
};
static const YYACTIONTYPE yy_default[] = {
 /*     0 */   257,  401,  401,  401,  401,  401,  401,  400,  401,  401,
 /*    10 */   401,  258,  401,  401,  401,  401,  401,  401,  274,  401,
 /*    20 */   401,  401,  401,  401,  401,  401,  401,  401,  401,  401,
 /*    30 */   401,  401,  401,  401,  401,  401,  401,  401,  401,  401,
 /*    40 */   401,  401,  401,  401,  401,  401,  401,  401,  401,  401,
 /*    50 */   401,  401,  401,  401,  401,  401,  401,  401,  401,  401,
 /*    60 */   401,  401,  401,  401,  401,  284,  401,  401,  401,  401,
 /*    70 */   395,  287,  288,  289,  290,  291,  304,  303,  293,  292,
 /*    80 */   297,  295,  296,  294,  305,  298,  299,  401,  357,  401,
 /*    90 */   401,  401,  401,  375,  401,  401,  401,  401,  401,  401,
 /*   100 */   334,  401,  401,  383,  401,  401,  321,  401,  391,  357,
 /*   110 */   401,  401,  401,  401,  401,  401,  315,  356,  401,  401,
 /*   120 */   401,  401,  401,  381,  401,  401,  401,  401,  401,  401,
 /*   130 */   401,  401,  401,  401,  401,  401,  382,  348,  349,  346,
 /*   140 */   350,  344,  351,  253,  352,  353,  343,  354,  302,  355,
 /*   150 */   301,  300,  285,  283,  282,  358,  360,  359,  281,  280,
 /*   160 */   279,  361,  278,  362,  317,  363,  364,  316,  341,  365,
 /*   170 */   366,  367,  342,  340,  319,  318,  336,  373,  384,  339,
 /*   180 */   338,  337,  335,  333,  332,  388,  331,  330,  329,  328,
 /*   190 */   327,  326,  392,  325,  324,  323,  322,  393,  321,  320,
 /*   200 */   314,  311,  310,  309,  394,  396,  397,  308,  307,  306,
 /*   210 */   313,  398,  312,  286,  399,  277,  276,  273,  272,  271,
 /*   220 */   275,  270,  269,  385,  386,  268,  387,  267,  266,  265,
 /*   230 */   264,  389,  263,  374,  262,  261,  376,  377,  260,  259,
 /*   240 */   378,  379,  256,  255,  254,  380,  347,  368,  369,  370,
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
 /*  84 */ "list_content ::= list_content COMMA list_entry",
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
#line 213 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(list_entry, yymsp[0].minor.yy215); }
#line 1591 "astgen.c"
        break;
      case 86:
#line 216 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(new_expression, String(yymsp[0].minor.yy0)); }
#line 1596 "astgen.c"
        break;
      case 87:
#line 225 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(argument, yymsp[0].minor.yy215); }
#line 1601 "astgen.c"
        break;
      case 89:
#line 229 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(argument_list, yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1606 "astgen.c"
        break;
      case 91:
#line 238 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(expression_statement, yymsp[-1].minor.yy215); }
#line 1611 "astgen.c"
        break;
      case 92:
#line 241 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(compound_statement); }
#line 1616 "astgen.c"
        break;
      case 93:
#line 242 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(compound_statement, yymsp[-1].minor.yy215); }
#line 1621 "astgen.c"
        break;
      case 94:
#line 245 "astgen.in"
{ p->Parse(yymsp[-1].minor.yy0); yygotominor.yy215 = p->GetRoot(); }
#line 1626 "astgen.c"
        break;
      case 95:
#line 248 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(return_statement, yymsp[-1].minor.yy215); }
#line 1631 "astgen.c"
        break;
      case 96:
#line 249 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(return_statement);    }
#line 1636 "astgen.c"
        break;
      case 97:
#line 252 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(break_statement); }
#line 1641 "astgen.c"
        break;
      case 98:
#line 253 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(continue_statement); }
#line 1646 "astgen.c"
        break;
      case 99:
#line 256 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(pause_statement); }
#line 1651 "astgen.c"
        break;
      case 104:
#line 270 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(variable_declaration, String(yymsp[0].minor.yy0));    }
#line 1656 "astgen.c"
        break;
      case 105:
#line 271 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(variable_declaration, String(yymsp[-2].minor.yy0), yymsp[0].minor.yy215); }
#line 1661 "astgen.c"
        break;
      case 107:
#line 274 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(declaration_sequence, yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1666 "astgen.c"
        break;
      case 108:
#line 281 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(class_declaration, String(yymsp[-2].minor.yy0)); }
#line 1671 "astgen.c"
        break;
      case 109:
#line 282 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(class_declaration, String(yymsp[-3].minor.yy0), yymsp[-1].minor.yy87); }
#line 1676 "astgen.c"
        break;
      case 110:
#line 285 "astgen.in"
{ yygotominor.yy215 = yymsp[-1].minor.yy215; yymsp[-1].minor.yy215->m_props["access"] = accessDefault; }
#line 1681 "astgen.c"
        break;
      case 111:
#line 286 "astgen.in"
{ yygotominor.yy215 = yymsp[0].minor.yy215; yymsp[0].minor.yy215->m_props["access"] = accessDefault; }
#line 1686 "astgen.c"
        break;
      case 112:
#line 287 "astgen.in"
{ yygotominor.yy215 = yymsp[-1].minor.yy215; yymsp[-1].minor.yy215->m_props["access"] = yymsp[-2].minor.yy156; }
#line 1691 "astgen.c"
        break;
      case 113:
#line 288 "astgen.in"
{ yygotominor.yy215 = yymsp[0].minor.yy215; yymsp[0].minor.yy215->m_props["access"] = yymsp[-1].minor.yy156; }
#line 1696 "astgen.c"
        break;
      case 114:
#line 289 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(access_specifier, yymsp[-1].minor.yy156); }
#line 1701 "astgen.c"
        break;
      case 115:
#line 293 "astgen.in"
{ yygotominor.yy156 = accessPrivate;   }
#line 1706 "astgen.c"
        break;
      case 116:
#line 294 "astgen.in"
{ yygotominor.yy156 = accessProtected; }
#line 1711 "astgen.c"
        break;
      case 117:
#line 295 "astgen.in"
{ yygotominor.yy156 = accessPublic;    }
#line 1716 "astgen.c"
        break;
      case 118:
#line 299 "astgen.in"
{ 
  yygotominor.yy87 = new AstList;
  yygotominor.yy87->push_back(yymsp[0].minor.yy215);
}
#line 1724 "astgen.c"
        break;
      case 119:
#line 303 "astgen.in"
{ 
  yygotominor.yy87 = yymsp[-1].minor.yy87;
  yygotominor.yy87->push_back(yymsp[0].minor.yy215);
}
#line 1732 "astgen.c"
        break;
      case 120:
#line 314 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(function_declaration, String(yymsp[-4].minor.yy0), yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1737 "astgen.c"
        break;
      case 121:
#line 315 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(function_declaration, String(yymsp[-3].minor.yy0), AstData(), yymsp[0].minor.yy215); }
#line 1742 "astgen.c"
        break;
      case 122:
#line 318 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(parameter, String(yymsp[0].minor.yy0)); }
#line 1747 "astgen.c"
        break;
      case 123:
#line 321 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(parameter, String(yymsp[-2].minor.yy0), yymsp[0].minor.yy215); }
#line 1752 "astgen.c"
        break;
      case 125:
      case 127:
      case 130:
#line 325 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(parameter_list, yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1759 "astgen.c"
        break;
      case 132:
#line 346 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(for_statement, yymsp[-5].minor.yy215, yymsp[-4].minor.yy215, yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1764 "astgen.c"
        break;
      case 135:
      case 136:
#line 357 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(foreach_statement, yymsp[-4].minor.yy215, yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1770 "astgen.c"
        break;
      case 137:
#line 359 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(variable_declaration, String(yymsp[0].minor.yy0)); }
#line 1775 "astgen.c"
        break;
      case 138:
#line 370 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(if_statement, yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1780 "astgen.c"
        break;
      case 139:
#line 371 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(if_statement, yymsp[-4].minor.yy215, yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1785 "astgen.c"
        break;
      case 140:
#line 379 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(while_statement, yymsp[-2].minor.yy215,  yymsp[0].minor.yy215); }
#line 1790 "astgen.c"
        break;
      case 141:
#line 387 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(switch_statement, yymsp[-4].minor.yy215, yymsp[-1].minor.yy87); }
#line 1795 "astgen.c"
        break;
      case 142:
#line 391 "astgen.in"
{ yygotominor.yy87 = new AstList; }
#line 1800 "astgen.c"
        break;
      case 143:
      case 144:
#line 392 "astgen.in"
{ yygotominor.yy87 = yymsp[-1].minor.yy87; yygotominor.yy87->push_back(yymsp[0].minor.yy215); }
#line 1806 "astgen.c"
        break;
      case 145:
#line 396 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(switch_case, yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1811 "astgen.c"
        break;
      case 146:
#line 399 "astgen.in"
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

