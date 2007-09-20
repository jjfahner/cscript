/* Driver template for the LEMON parser generator.
** The author disclaims copyright to this source code.
*/
/* First off, code is include which follows the "include" declaration
** in the input file. */
#include <stdio.h>
#line 30 "cscript.in"


#include "tokens.h"
#include "parser.h"
#include "ast.h"
#include "astlist.h"
#include "convert.h"

#pragma warning(disable:4065)

#line 20 "cscript.c"
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
#define YYNOCODE 134
#define YYACTIONTYPE unsigned short int
#define CScriptParseTOKENTYPE  Token 
typedef union {
  CScriptParseTOKENTYPE yy0;
  AstList* yy43;
  int yy46;
  opcodes yy54;
  Ast* yy109;
  AccessTypes yy222;
  int yy267;
} YYMINORTYPE;
#ifndef YYSTACKDEPTH
#define YYSTACKDEPTH 100
#endif
#define CScriptParseARG_SDECL  Parser* p ;
#define CScriptParseARG_PDECL , Parser* p 
#define CScriptParseARG_FETCH  Parser* p  = yypParser->p 
#define CScriptParseARG_STORE yypParser->p  = p 
#define YYNSTATE 263
#define YYNRULE 154
#define YYERRORSYMBOL 73
#define YYERRSYMDT yy267
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
 /*     0 */   229,  418,  155,  251,   10,  250,  248,  247,  245,  244,
 /*    10 */   242,  241,  240,  239,  237,  234,  233,  231,  230,  126,
 /*    20 */   228,  102,  227,   66,  123,  224,   86,  210,  209,  208,
 /*    30 */    54,  207,  206,  204,  163,   63,  120,  145,  143,  142,
 /*    40 */   260,   61,   55,   53,   52,  146,  115,   15,  133,   26,
 /*    50 */   221,  200,  199,  198,  197,  196,  195,  193,  192,  191,
 /*    60 */   190,  255,  106,  159,    3,  262,  131,   20,  138,  140,
 /*    70 */    64,  132,  139,  154,   54,   96,  108,   43,   42,   39,
 /*    80 */   141,   93,   99,  202,  203,  117,   55,   53,   52,  147,
 /*    90 */   115,   15,   62,   26,  168,  200,  199,  198,  197,  196,
 /*   100 */   195,  193,  192,  191,  190,  148,  106,  159,    3,  156,
 /*   110 */   131,   20,  138,  140,   64,  132,  217,  218,   54,   96,
 /*   120 */   108,   69,  184,   64,  141,   93,   99,   70,   96,  117,
 /*   130 */    55,   53,   52,   19,  115,   15,  167,   26,  116,  200,
 /*   140 */   199,  198,  197,  196,  195,  193,  192,  191,  190,   27,
 /*   150 */   106,  159,    3,  179,  131,   20,  138,  140,   64,  132,
 /*   160 */   163,    2,  120,   96,  108,   58,  261,   61,  141,   93,
 /*   170 */    99,  229,  127,  117,   27,    9,  250,  248,  247,  245,
 /*   180 */   244,  242,  241,  240,  239,  237,  234,  233,  231,  230,
 /*   190 */   126,  228,  399,  227,   66,  252,  224,   86,  210,  209,
 /*   200 */   208,  150,  207,  206,  204,  129,  153,  201,  145,  143,
 /*   210 */   142,   35,   41,   45,   38,   46,   51,   48,   47,   34,
 /*   220 */    44,   43,   42,   39,  229,   67,  137,  226,    9,  250,
 /*   230 */   248,  247,  245,  244,  242,  241,  240,  239,  237,  234,
 /*   240 */   233,  231,  230,  126,  228,   72,  227,   66,  220,  224,
 /*   250 */    86,  210,  209,  208,  136,  207,  206,  204,  130,  229,
 /*   260 */   128,  145,  143,  142,  175,  248,  247,  245,  244,  242,
 /*   270 */   241,  240,  239,  237,  234,  233,  231,  230,  126,  228,
 /*   280 */   223,  227,   66,  107,  224,   86,  210,  209,  208,  118,
 /*   290 */   207,  206,  204,  133,   89,  222,  145,  143,  142,   56,
 /*   300 */   253,  254,  229,  110,   32,  174,    5,  250,  248,  247,
 /*   310 */   245,  244,  242,  241,  240,  239,  237,  234,  233,  231,
 /*   320 */   230,  126,  228,   28,  227,   66,   91,  224,   86,  210,
 /*   330 */   209,  208,  151,  207,  206,  204,   16,  229,  157,  145,
 /*   340 */   143,  142,  235,  248,  247,  245,  244,  242,  241,  240,
 /*   350 */   239,  237,  234,  233,  231,  230,  126,  228,   25,  227,
 /*   360 */    66,   18,  224,   86,  210,  209,  208,   60,  207,  206,
 /*   370 */   204,  188,  229,   22,  145,  143,  142,  249,  248,  247,
 /*   380 */   245,  244,  242,  241,  240,  239,  237,  234,  233,  231,
 /*   390 */   230,  126,  228,  215,  227,   66,   12,  224,   86,  210,
 /*   400 */   209,  208,    8,  207,  206,  204,   13,  229,    7,  145,
 /*   410 */   143,  142,  189,  248,  247,  245,  244,  242,  241,  240,
 /*   420 */   239,  237,  234,  233,  231,  230,  126,  228,  103,  227,
 /*   430 */    66,  162,  224,   86,  210,  209,  208,   31,  207,  206,
 /*   440 */   204,   59,  229,  119,  145,  143,  142,  104,  248,  247,
 /*   450 */   245,  244,  242,  241,  240,  239,  237,  234,  233,  231,
 /*   460 */   230,  126,  228,   17,  227,   66,   29,  224,   86,  210,
 /*   470 */   209,  208,   68,  207,  206,  204,   33,  229,  134,  145,
 /*   480 */   143,  142,  243,  248,  247,  245,  244,  242,  241,  240,
 /*   490 */   239,  237,  234,  233,  231,  230,  126,  228,  178,  227,
 /*   500 */    66,  166,  224,   86,  210,  209,  208,   57,  207,  206,
 /*   510 */   204,    6,  229,   11,  145,  143,  142,  182,  248,  247,
 /*   520 */   245,  244,  242,  241,  240,  239,  237,  234,  233,  231,
 /*   530 */   230,  126,  228,   21,  227,   66,    4,  224,   86,  210,
 /*   540 */   209,  208,  238,  207,  206,  204,  185,  229,    1,  145,
 /*   550 */   143,  142,  194,  248,  247,  245,  244,  242,  241,  240,
 /*   560 */   239,  237,  234,  233,  231,  230,  126,  228,  205,  227,
 /*   570 */    66,   14,  224,   86,  210,  209,  208,  419,  207,  206,
 /*   580 */   204,  419,  419,  419,  145,  143,  142,   49,   50,   37,
 /*   590 */    40,   36,   35,   41,   45,   38,   46,   51,   48,   47,
 /*   600 */    34,   44,   43,   42,   39,  177,  176,  173,  172,  171,
 /*   610 */   170,   30,   54,   45,   38,   46,   51,   48,   47,   34,
 /*   620 */    44,   43,   42,   39,   55,   53,   52,  419,  115,   15,
 /*   630 */   419,   26,  419,  200,  199,  198,  197,  196,  195,  193,
 /*   640 */   192,  191,  190,  419,  106,  159,  158,   54,  419,  419,
 /*   650 */   419,   64,   65,  257,  258,  259,   96,  419,  419,   55,
 /*   660 */    53,   52,  419,  115,   15,  419,   26,  419,  200,  199,
 /*   670 */   198,  197,  196,  195,  193,  192,  191,  190,  419,  106,
 /*   680 */   149,   88,   54,  224,   86,  210,  209,  208,  419,  207,
 /*   690 */   206,  204,  419,  419,   55,   53,   52,  419,  115,   15,
 /*   700 */   419,   26,  419,  200,  199,  198,  197,  196,  195,  193,
 /*   710 */   192,  191,  190,  419,  106,   50,   37,   40,   36,   35,
 /*   720 */    41,   45,   38,   46,   51,   48,   47,   34,   44,   43,
 /*   730 */    42,   39,   37,   40,   36,   35,   41,   45,   38,   46,
 /*   740 */    51,   48,   47,   34,   44,   43,   42,   39,  419,  236,
 /*   750 */    46,   51,   48,   47,   34,   44,   43,   42,   39,  419,
 /*   760 */   419,  126,  228,  419,  227,   66,  419,  224,   86,  210,
 /*   770 */   209,  208,  419,  207,  112,  204,  419,  419,  419,  419,
 /*   780 */    73,  111,  224,   86,  210,  209,  208,  419,  207,  206,
 /*   790 */   204,  419,  419,   23,   97,  419,  419,  419,   40,   36,
 /*   800 */    35,   41,   45,   38,   46,   51,   48,   47,   34,   44,
 /*   810 */    43,   42,   39,  183,  228,  419,  227,   66,  419,  224,
 /*   820 */    86,  210,  209,  208,   90,  207,  206,  204,  186,  228,
 /*   830 */   180,  227,   66,  419,  224,   86,  210,  209,  208,  419,
 /*   840 */   207,  206,  204,   92,   98,  199,  198,  197,  196,  195,
 /*   850 */   193,  192,  191,  190,  186,  228,  419,  227,   66,  419,
 /*   860 */   224,   86,  210,  209,  208,  419,  207,  206,  204,  187,
 /*   870 */    98,  419,   36,   35,   41,   45,   38,   46,   51,   48,
 /*   880 */    47,   34,   44,   43,   42,   39,  419,  183,  228,  419,
 /*   890 */   227,   66,  419,  224,   86,  210,  209,  208,  419,  207,
 /*   900 */   206,  204,  419,  419,  181,  232,  228,  113,  227,   66,
 /*   910 */   419,  224,   86,  210,  209,  208,  419,  207,  206,  204,
 /*   920 */   114,  228,  419,  227,   66,  419,  224,   86,  210,  209,
 /*   930 */   208,  419,  207,  206,  204,  135,  228,  419,  227,   66,
 /*   940 */   419,  224,   86,  210,  209,  208,  419,  207,  206,  204,
 /*   950 */    94,  228,  419,  227,   66,  419,  224,   86,  210,  209,
 /*   960 */   208,  419,  207,  206,  204,  122,  228,  419,  227,   66,
 /*   970 */   419,  224,   86,  210,  209,  208,  419,  207,  206,  204,
 /*   980 */   100,  228,  419,  227,   66,  419,  224,   86,  210,  209,
 /*   990 */   208,  419,  207,  206,  204,  419,  419,   95,  228,  419,
 /*  1000 */   227,   66,  419,  224,   86,  210,  209,  208,  419,  207,
 /*  1010 */   206,  204,  246,  228,  419,  227,   66,  419,  224,   86,
 /*  1020 */   210,  209,  208,  419,  207,  206,  204,  125,  228,  419,
 /*  1030 */   227,   66,  419,  224,   86,  210,  209,  208,  419,  207,
 /*  1040 */   206,  204,  152,  228,  419,  227,   66,  419,  224,   86,
 /*  1050 */   210,  209,  208,  419,  207,  206,  204,  101,  228,  419,
 /*  1060 */   227,   66,  419,  224,   86,  210,  209,  208,  419,  207,
 /*  1070 */   206,  204,  109,  228,  419,  227,   66,  419,  224,   86,
 /*  1080 */   210,  209,  208,  419,  207,  206,  204,  121,  228,  419,
 /*  1090 */   227,   66,  419,  224,   86,  210,  209,  208,  419,  207,
 /*  1100 */   206,  204,  419,  225,  419,  227,   66,  419,  224,   86,
 /*  1110 */   210,  209,  208,  419,  207,  206,  204,  419,  169,  419,
 /*  1120 */   227,   66,  419,  224,   86,  210,  209,  208,  419,  207,
 /*  1130 */   206,  204,  165,  419,  224,   86,  210,  209,  208,  419,
 /*  1140 */   207,  206,  204,   74,  419,  224,   86,  210,  209,  208,
 /*  1150 */   419,  207,  206,  204,  164,  419,  224,   86,  210,  209,
 /*  1160 */   208,  419,  207,  206,  204,  419,   83,  419,  224,   86,
 /*  1170 */   210,  209,  208,  419,  207,  206,  204,   79,  419,  224,
 /*  1180 */    86,  210,  209,  208,  419,  207,  206,  204,   85,  419,
 /*  1190 */   224,   86,  210,  209,  208,  419,  207,  206,  204,  419,
 /*  1200 */    76,  419,  224,   86,  210,  209,  208,  419,  207,  206,
 /*  1210 */   204,   80,  419,  224,   86,  210,  209,  208,  419,  207,
 /*  1220 */   206,  204,   82,  419,  224,   86,  210,  209,  208,  419,
 /*  1230 */   207,  206,  204,   84,  419,  224,   86,  210,  209,  208,
 /*  1240 */   419,  207,  206,  204,  161,  419,  224,   86,  210,  209,
 /*  1250 */   208,  419,  207,  206,  204,   87,  419,  224,   86,  210,
 /*  1260 */   209,  208,  419,  207,  206,  204,   81,  419,  224,   86,
 /*  1270 */   210,  209,  208,  419,  207,  206,  204,   78,  419,  224,
 /*  1280 */    86,  210,  209,  208,  419,  207,  206,  204,   75,  419,
 /*  1290 */   224,   86,  210,  209,  208,  419,  207,  206,  204,   77,
 /*  1300 */   419,  224,   86,  210,  209,  208,  419,  207,  206,  204,
 /*  1310 */   211,   86,  210,  209,  208,  419,  207,  206,  204,  214,
 /*  1320 */    86,  210,  209,  208,  419,  207,  206,  204,  419,  213,
 /*  1330 */    86,  210,  209,  208,  419,  207,  206,  204,  212,   86,
 /*  1340 */   210,  209,  208,  160,  207,  206,  204,  419,   64,  419,
 /*  1350 */   257,  258,  259,   96,  105,  419,  256,  254,  124,  144,
 /*  1360 */    34,   44,   43,   42,   39,  219,  216,  419,   71,  419,
 /*  1370 */    24,
};
static const YYCODETYPE yy_lookahead[] = {
 /*     0 */    73,   74,   75,   76,   77,   78,   79,   80,   81,   82,
 /*    10 */    83,   84,   85,   86,   87,   88,   89,   90,   91,   92,
 /*    20 */    93,   31,   95,   96,   35,   98,   99,  100,  101,  102,
 /*    30 */    15,  104,  105,  106,  110,   46,  112,  110,  111,  112,
 /*    40 */   116,  117,   27,   28,   29,   48,   31,   32,  130,   34,
 /*    50 */   132,   36,   37,   38,   39,   40,   41,   42,   43,   44,
 /*    60 */    45,   48,   47,   48,   49,   50,   51,   52,   53,   54,
 /*    70 */    55,   56,  113,  114,   15,   60,   61,   16,   17,   18,
 /*    80 */    65,   66,   67,  127,  128,   70,   27,   28,   29,   48,
 /*    90 */    31,   32,  126,   34,   26,   36,   37,   38,   39,   40,
 /*   100 */    41,   42,   43,   44,   45,   48,   47,   48,   49,   50,
 /*   110 */    51,   52,   53,   54,   55,   56,   71,   72,   15,   60,
 /*   120 */    61,   46,   35,   55,   65,   66,   67,   46,   60,   70,
 /*   130 */    27,   28,   29,   46,   31,   32,  110,   34,  112,   36,
 /*   140 */    37,   38,   39,   40,   41,   42,   43,   44,   45,   19,
 /*   150 */    47,   48,   49,  102,   51,   52,   53,   54,   55,   56,
 /*   160 */   110,   26,  112,   60,   61,  115,  116,  117,   65,   66,
 /*   170 */    67,   73,   31,   70,   19,   77,   78,   79,   80,   81,
 /*   180 */    82,   83,   84,   85,   86,   87,   88,   89,   90,   91,
 /*   190 */    92,   93,   62,   95,   96,  121,   98,   99,  100,  101,
 /*   200 */   102,   48,  104,  105,  106,   31,  114,   50,  110,  111,
 /*   210 */   112,    6,    7,    8,    9,   10,   11,   12,   13,   14,
 /*   220 */    15,   16,   17,   18,   73,   68,   69,  129,   77,   78,
 /*   230 */    79,   80,   81,   82,   83,   84,   85,   86,   87,   88,
 /*   240 */    89,   90,   91,   92,   93,   49,   95,   96,   31,   98,
 /*   250 */    99,  100,  101,  102,   31,  104,  105,  106,   31,   73,
 /*   260 */    42,  110,  111,  112,   78,   79,   80,   81,   82,   83,
 /*   270 */    84,   85,   86,   87,   88,   89,   90,   91,   92,   93,
 /*   280 */   129,   95,   96,  104,   98,   99,  100,  101,  102,  130,
 /*   290 */   104,  105,  106,  130,  131,  132,  110,  111,  112,   49,
 /*   300 */   120,  121,   73,  123,   97,  119,   77,   78,   79,   80,
 /*   310 */    81,   82,   83,   84,   85,   86,   87,   88,   89,   90,
 /*   320 */    91,   92,   93,   19,   95,   96,   31,   98,   99,  100,
 /*   330 */   101,  102,   48,  104,  105,  106,   34,   73,   48,  110,
 /*   340 */   111,  112,   78,   79,   80,   81,   82,   83,   84,   85,
 /*   350 */    86,   87,   88,   89,   90,   91,   92,   93,   34,   95,
 /*   360 */    96,   48,   98,   99,  100,  101,  102,   46,  104,  105,
 /*   370 */   106,   33,   73,   34,  110,  111,  112,   78,   79,   80,
 /*   380 */    81,   82,   83,   84,   85,   86,   87,   88,   89,   90,
 /*   390 */    91,   92,   93,   48,   95,   96,   35,   98,   99,  100,
 /*   400 */   101,  102,   35,  104,  105,  106,   35,   73,   35,  110,
 /*   410 */   111,  112,   78,   79,   80,   81,   82,   83,   84,   85,
 /*   420 */    86,   87,   88,   89,   90,   91,   92,   93,   31,   95,
 /*   430 */    96,   48,   98,   99,  100,  101,  102,   62,  104,  105,
 /*   440 */   106,   34,   73,   31,  110,  111,  112,   78,   79,   80,
 /*   450 */    81,   82,   83,   84,   85,   86,   87,   88,   89,   90,
 /*   460 */    91,   92,   93,   46,   95,   96,   34,   98,   99,  100,
 /*   470 */   101,  102,   42,  104,  105,  106,   26,   73,   35,  110,
 /*   480 */   111,  112,   78,   79,   80,   81,   82,   83,   84,   85,
 /*   490 */    86,   87,   88,   89,   90,   91,   92,   93,   33,   95,
 /*   500 */    96,   48,   98,   99,  100,  101,  102,   34,  104,  105,
 /*   510 */   106,   35,   73,   64,  110,  111,  112,   78,   79,   80,
 /*   520 */    81,   82,   83,   84,   85,   86,   87,   88,   89,   90,
 /*   530 */    91,   92,   93,   62,   95,   96,   35,   98,   99,  100,
 /*   540 */   101,  102,   48,  104,  105,  106,   31,   73,   26,  110,
 /*   550 */   111,  112,   78,   79,   80,   81,   82,   83,   84,   85,
 /*   560 */    86,   87,   88,   89,   90,   91,   92,   93,   35,   95,
 /*   570 */    96,   34,   98,   99,  100,  101,  102,  133,  104,  105,
 /*   580 */   106,  133,  133,  133,  110,  111,  112,    1,    2,    3,
 /*   590 */     4,    5,    6,    7,    8,    9,   10,   11,   12,   13,
 /*   600 */    14,   15,   16,   17,   18,   19,   20,   21,   22,   23,
 /*   610 */    24,   25,   15,    8,    9,   10,   11,   12,   13,   14,
 /*   620 */    15,   16,   17,   18,   27,   28,   29,  133,   31,   32,
 /*   630 */   133,   34,  133,   36,   37,   38,   39,   40,   41,   42,
 /*   640 */    43,   44,   45,  133,   47,   48,   50,   15,  133,  133,
 /*   650 */   133,   55,   55,   57,   58,   59,   60,  133,  133,   27,
 /*   660 */    28,   29,  133,   31,   32,  133,   34,  133,   36,   37,
 /*   670 */    38,   39,   40,   41,   42,   43,   44,   45,  133,   47,
 /*   680 */    48,   96,   15,   98,   99,  100,  101,  102,  133,  104,
 /*   690 */   105,  106,  133,  133,   27,   28,   29,  133,   31,   32,
 /*   700 */   133,   34,  133,   36,   37,   38,   39,   40,   41,   42,
 /*   710 */    43,   44,   45,  133,   47,    2,    3,    4,    5,    6,
 /*   720 */     7,    8,    9,   10,   11,   12,   13,   14,   15,   16,
 /*   730 */    17,   18,    3,    4,    5,    6,    7,    8,    9,   10,
 /*   740 */    11,   12,   13,   14,   15,   16,   17,   18,  133,   80,
 /*   750 */    10,   11,   12,   13,   14,   15,   16,   17,   18,  133,
 /*   760 */   133,   92,   93,  133,   95,   96,  133,   98,   99,  100,
 /*   770 */   101,  102,  133,  104,  105,  106,  133,  133,  133,  133,
 /*   780 */    96,  112,   98,   99,  100,  101,  102,  133,  104,  105,
 /*   790 */   106,  133,  133,  124,  125,  133,  133,  133,    4,    5,
 /*   800 */     6,    7,    8,    9,   10,   11,   12,   13,   14,   15,
 /*   810 */    16,   17,   18,   92,   93,  133,   95,   96,  133,   98,
 /*   820 */    99,  100,  101,  102,  103,  104,  105,  106,   92,   93,
 /*   830 */   109,   95,   96,  133,   98,   99,  100,  101,  102,  133,
 /*   840 */   104,  105,  106,  107,  108,   37,   38,   39,   40,   41,
 /*   850 */    42,   43,   44,   45,   92,   93,  133,   95,   96,  133,
 /*   860 */    98,   99,  100,  101,  102,  133,  104,  105,  106,  107,
 /*   870 */   108,  133,    5,    6,    7,    8,    9,   10,   11,   12,
 /*   880 */    13,   14,   15,   16,   17,   18,  133,   92,   93,  133,
 /*   890 */    95,   96,  133,   98,   99,  100,  101,  102,  133,  104,
 /*   900 */   105,  106,  133,  133,  109,   92,   93,   94,   95,   96,
 /*   910 */   133,   98,   99,  100,  101,  102,  133,  104,  105,  106,
 /*   920 */    92,   93,  133,   95,   96,  133,   98,   99,  100,  101,
 /*   930 */   102,  133,  104,  105,  106,   92,   93,  133,   95,   96,
 /*   940 */   133,   98,   99,  100,  101,  102,  133,  104,  105,  106,
 /*   950 */    92,   93,  133,   95,   96,  133,   98,   99,  100,  101,
 /*   960 */   102,  133,  104,  105,  106,   92,   93,  133,   95,   96,
 /*   970 */   133,   98,   99,  100,  101,  102,  133,  104,  105,  106,
 /*   980 */    92,   93,  133,   95,   96,  133,   98,   99,  100,  101,
 /*   990 */   102,  133,  104,  105,  106,  133,  133,   92,   93,  133,
 /*  1000 */    95,   96,  133,   98,   99,  100,  101,  102,  133,  104,
 /*  1010 */   105,  106,   92,   93,  133,   95,   96,  133,   98,   99,
 /*  1020 */   100,  101,  102,  133,  104,  105,  106,   92,   93,  133,
 /*  1030 */    95,   96,  133,   98,   99,  100,  101,  102,  133,  104,
 /*  1040 */   105,  106,   92,   93,  133,   95,   96,  133,   98,   99,
 /*  1050 */   100,  101,  102,  133,  104,  105,  106,   92,   93,  133,
 /*  1060 */    95,   96,  133,   98,   99,  100,  101,  102,  133,  104,
 /*  1070 */   105,  106,   92,   93,  133,   95,   96,  133,   98,   99,
 /*  1080 */   100,  101,  102,  133,  104,  105,  106,   92,   93,  133,
 /*  1090 */    95,   96,  133,   98,   99,  100,  101,  102,  133,  104,
 /*  1100 */   105,  106,  133,   93,  133,   95,   96,  133,   98,   99,
 /*  1110 */   100,  101,  102,  133,  104,  105,  106,  133,   93,  133,
 /*  1120 */    95,   96,  133,   98,   99,  100,  101,  102,  133,  104,
 /*  1130 */   105,  106,   96,  133,   98,   99,  100,  101,  102,  133,
 /*  1140 */   104,  105,  106,   96,  133,   98,   99,  100,  101,  102,
 /*  1150 */   133,  104,  105,  106,   96,  133,   98,   99,  100,  101,
 /*  1160 */   102,  133,  104,  105,  106,  133,   96,  133,   98,   99,
 /*  1170 */   100,  101,  102,  133,  104,  105,  106,   96,  133,   98,
 /*  1180 */    99,  100,  101,  102,  133,  104,  105,  106,   96,  133,
 /*  1190 */    98,   99,  100,  101,  102,  133,  104,  105,  106,  133,
 /*  1200 */    96,  133,   98,   99,  100,  101,  102,  133,  104,  105,
 /*  1210 */   106,   96,  133,   98,   99,  100,  101,  102,  133,  104,
 /*  1220 */   105,  106,   96,  133,   98,   99,  100,  101,  102,  133,
 /*  1230 */   104,  105,  106,   96,  133,   98,   99,  100,  101,  102,
 /*  1240 */   133,  104,  105,  106,   96,  133,   98,   99,  100,  101,
 /*  1250 */   102,  133,  104,  105,  106,   96,  133,   98,   99,  100,
 /*  1260 */   101,  102,  133,  104,  105,  106,   96,  133,   98,   99,
 /*  1270 */   100,  101,  102,  133,  104,  105,  106,   96,  133,   98,
 /*  1280 */    99,  100,  101,  102,  133,  104,  105,  106,   96,  133,
 /*  1290 */    98,   99,  100,  101,  102,  133,  104,  105,  106,   96,
 /*  1300 */   133,   98,   99,  100,  101,  102,  133,  104,  105,  106,
 /*  1310 */    98,   99,  100,  101,  102,  133,  104,  105,  106,   98,
 /*  1320 */    99,  100,  101,  102,  133,  104,  105,  106,  133,   98,
 /*  1330 */    99,  100,  101,  102,  133,  104,  105,  106,   98,   99,
 /*  1340 */   100,  101,  102,   50,  104,  105,  106,  133,   55,  133,
 /*  1350 */    57,   58,   59,   60,  118,  133,  120,  121,  122,  123,
 /*  1360 */    14,   15,   16,   17,   18,   27,   28,  133,   30,  133,
 /*  1370 */    32,
};
#define YY_SHIFT_USE_DFLT (-12)
#define YY_SHIFT_MAX 144
static const short yy_shift_ofst[] = {
 /*     0 */   103,  103,  103,   15,  103,   59,  103,  103,  103,  103,
 /*    10 */   103,  103,  103,  103,  597,  667,  667,  667,  667,  667,
 /*    20 */   632,  667,  667,  667,  667,  667,  667,  667,  667,  667,
 /*    30 */   667,  667,  667,  667,  667,  667,  667,  667,  667,  667,
 /*    40 */   667,  667,  667,  667,  667,  667,  667,  667,  667,  667,
 /*    50 */   667,  667,  667,  667,  667,  667,  596,  -10, 1293,   45,
 /*    60 */   -10,   68,  157,   45,  223,  295,  586,  808,   45,  223,
 /*    70 */   174,  141,  -12,  713,  729,  794,  867,  205,  605,  605,
 /*    80 */   740,  740, 1346, 1346, 1346, 1346, 1338,   61,   61,  -11,
 /*    90 */    87,  130,  338,  339,  361,  371,  397,  375,  417,  432,
 /*   100 */   450,  443,  304,  473,  449,  501,  515,  522,  537,  533,
 /*   110 */    81,  494,  471,  476,  465,  302,  453,  430,  412,  407,
 /*   120 */   383,  373,  367,  345,  321,  313,  290,  302,  284,  304,
 /*   130 */   250,  218,  227,  217,  196,  153,  155,  135,   57,   75,
 /*   140 */    41,  324,   -3,   13,   81,
};
#define YY_REDUCE_USE_DFLT (-83)
#define YY_REDUCE_MAX 72
static const short yy_reduce_ofst[] = {
 /*     0 */   -73,  151,   98,  229,  186,  299,  264,  369,  439,  299,
 /*    10 */   299,  334,  404,  474,  669,  736,  721,  762,  813,  795,
 /*    20 */   843,  873,  905,  935,  828,  995,  980,  950,  920,  965,
 /*    30 */   888,  858, 1010, 1025, 1159, 1181, 1203, 1192, 1170, 1148,
 /*    40 */  1104, 1081, 1058, 1036,  585, 1115, 1092, 1126, 1070,  684,
 /*    50 */  1047, 1137, 1212, 1240, 1221, 1231,   50, 1236,  -76,  163,
 /*    60 */   180,   26,  -44,  -82,  -41,  -41,  207,  179,  159,   92,
 /*    70 */    74,   51,  -34,
};
static const YYACTIONTYPE yy_default[] = {
 /*     0 */   267,  417,  417,  417,  417,  417,  417,  417,  417,  409,
 /*    10 */   268,  417,  417,  417,  417,  417,  350,  417,  284,  417,
 /*    20 */   417,  417,  417,  417,  417,  417,  417,  417,  417,  417,
 /*    30 */   417,  417,  417,  417,  417,  417,  417,  417,  417,  417,
 /*    40 */   417,  417,  417,  417,  417,  417,  417,  417,  417,  417,
 /*    50 */   417,  417,  417,  417,  417,  417,  417,  385,  417,  414,
 /*    60 */   417,  417,  417,  417,  417,  417,  294,  417,  417,  417,
 /*    70 */   417,  417,  404,  297,  298,  299,  300,  301,  313,  314,
 /*    80 */   302,  303,  307,  306,  305,  304,  315,  308,  309,  417,
 /*    90 */   417,  366,  417,  417,  417,  417,  417,  417,  345,  417,
 /*   100 */   417,  417,  383,  417,  400,  417,  417,  417,  417,  417,
 /*   110 */   388,  417,  330,  417,  417,  343,  417,  417,  417,  417,
 /*   120 */   417,  417,  417,  417,  386,  417,  417,  325,  417,  417,
 /*   130 */   417,  417,  417,  417,  417,  417,  366,  417,  417,  365,
 /*   140 */   417,  417,  417,  417,  387,  362,  364,  361,  360,  359,
 /*   150 */   358,  357,  367,  369,  368,  263,  356,  354,  370,  353,
 /*   160 */   371,  312,  372,  373,  311,  310,  374,  375,  376,  295,
 /*   170 */   293,  292,  291,  290,  382,  393,  289,  288,  327,  326,
 /*   180 */   351,  352,  397,  349,  328,  348,  347,  346,  344,  401,
 /*   190 */   342,  341,  340,  339,  402,  338,  337,  336,  335,  334,
 /*   200 */   333,  403,  405,  406,  332,  331,  330,  329,  324,  321,
 /*   210 */   320,  319,  318,  317,  316,  410,  323,  411,  412,  322,
 /*   220 */   413,  416,  415,  407,  296,  287,  408,  286,  283,  282,
 /*   230 */   281,  280,  285,  279,  278,  394,  395,  277,  396,  276,
 /*   240 */   275,  274,  273,  398,  272,  271,  384,  270,  269,  266,
 /*   250 */   265,  264,  392,  390,  391,  363,  389,  377,  378,  379,
 /*   260 */   381,  380,  355,
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
  "RETURN",        "BREAK",         "CONTINUE",      "VAR",         
  "CLASS",         "PRIVATE",       "PROTECTED",     "PUBLIC",      
  "FUNCTION",      "FOR",           "IN",            "LOWER_THAN_ELSE",
  "ELSE",          "IF",            "WHILE",         "SWITCH",      
  "CASE",          "DEFAULT",       "EXTERN",        "INT",         
  "STR",           "error",         "main",          "translation_unit",
  "statement_sequence_opt",  "statement_sequence",  "statement",     "include_statement",
  "expression_statement",  "declaration_statement",  "for_statement",  "compound_statement",
  "if_statement",  "while_statement",  "foreach_statement",  "return_statement",
  "switch_statement",  "break_statement",  "continue_statement",  "extern_declaration",
  "expression",    "assignment_expression",  "expression_opt",  "conditional_expression",
  "binary_expression",  "assignment_operator",  "unary_expression",  "postfix_expression",
  "new_expression",  "primary_expression",  "function_call",  "argument_list",
  "literal",       "id_expression",  "list_literal",  "list_content",
  "list_entry",    "argument",      "function_declaration",  "class_declaration",
  "variable_declaration",  "declarator_sequence",  "declarator",    "class_members",
  "class_member",  "access_specifier",  "parameter_list",  "function_body",
  "parameter",     "opt_parameter",  "parameters",    "opt_parameters",
  "for_init_statement",  "foreach_decl",  "switch_body",   "switch_case", 
  "default_case",  "case_statements",  "extern_type",   "extern_parameter_list",
  "extern_parameter",
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
 /*  18 */ "statement ::= extern_declaration",
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
 /*  65 */ "function_call ::= IDENTIFIER LPAREN argument_list RPAREN",
 /*  66 */ "primary_expression ::= literal",
 /*  67 */ "primary_expression ::= id_expression",
 /*  68 */ "primary_expression ::= LPAREN expression RPAREN",
 /*  69 */ "primary_expression ::= list_literal",
 /*  70 */ "primary_expression ::= THIS",
 /*  71 */ "literal ::= INTEGER",
 /*  72 */ "literal ::= HEX",
 /*  73 */ "literal ::= BIN",
 /*  74 */ "literal ::= ROM",
 /*  75 */ "literal ::= REAL",
 /*  76 */ "literal ::= STRING",
 /*  77 */ "literal ::= TRUE",
 /*  78 */ "literal ::= FALSE",
 /*  79 */ "literal ::= NULL",
 /*  80 */ "id_expression ::= IDENTIFIER",
 /*  81 */ "list_literal ::= LBRACKET list_content RBRACKET",
 /*  82 */ "list_content ::= list_entry",
 /*  83 */ "list_content ::= list_entry COMMA list_content",
 /*  84 */ "list_entry ::= expression",
 /*  85 */ "new_expression ::= NEW IDENTIFIER",
 /*  86 */ "argument ::= expression",
 /*  87 */ "argument_list ::=",
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
 /*  99 */ "declaration_statement ::= function_declaration",
 /* 100 */ "declaration_statement ::= class_declaration SEMICOLON",
 /* 101 */ "declaration_statement ::= variable_declaration SEMICOLON",
 /* 102 */ "variable_declaration ::= VAR declarator_sequence",
 /* 103 */ "declarator ::= IDENTIFIER",
 /* 104 */ "declarator ::= IDENTIFIER ASSIGN expression",
 /* 105 */ "declarator_sequence ::= declarator",
 /* 106 */ "declarator_sequence ::= declarator_sequence COMMA declarator",
 /* 107 */ "class_declaration ::= CLASS IDENTIFIER LBRACE RBRACE",
 /* 108 */ "class_declaration ::= CLASS IDENTIFIER LBRACE class_members RBRACE",
 /* 109 */ "class_member ::= variable_declaration SEMICOLON",
 /* 110 */ "class_member ::= function_declaration",
 /* 111 */ "class_member ::= access_specifier variable_declaration SEMICOLON",
 /* 112 */ "class_member ::= access_specifier function_declaration",
 /* 113 */ "class_member ::= access_specifier COLON",
 /* 114 */ "access_specifier ::= PRIVATE",
 /* 115 */ "access_specifier ::= PROTECTED",
 /* 116 */ "access_specifier ::= PUBLIC",
 /* 117 */ "class_members ::= class_member",
 /* 118 */ "class_members ::= class_members class_member",
 /* 119 */ "function_declaration ::= FUNCTION IDENTIFIER LPAREN parameter_list RPAREN function_body",
 /* 120 */ "parameter ::= IDENTIFIER",
 /* 121 */ "opt_parameter ::= IDENTIFIER ASSIGN expression",
 /* 122 */ "parameter_list ::=",
 /* 123 */ "parameter_list ::= parameters",
 /* 124 */ "parameter_list ::= opt_parameters",
 /* 125 */ "parameter_list ::= parameters COMMA opt_parameters",
 /* 126 */ "parameters ::= parameter",
 /* 127 */ "parameters ::= parameters COMMA parameter",
 /* 128 */ "opt_parameters ::= opt_parameter",
 /* 129 */ "opt_parameters ::= opt_parameters COMMA opt_parameter",
 /* 130 */ "function_body ::= statement",
 /* 131 */ "for_statement ::= FOR LPAREN for_init_statement expression SEMICOLON expression_opt RPAREN statement",
 /* 132 */ "for_init_statement ::= expression_statement",
 /* 133 */ "for_init_statement ::= variable_declaration SEMICOLON",
 /* 134 */ "foreach_statement ::= FOR LPAREN id_expression IN expression RPAREN statement",
 /* 135 */ "foreach_statement ::= FOR LPAREN foreach_decl IN expression RPAREN statement",
 /* 136 */ "foreach_decl ::= VAR IDENTIFIER",
 /* 137 */ "if_statement ::= IF LPAREN expression RPAREN statement",
 /* 138 */ "if_statement ::= IF LPAREN expression RPAREN statement ELSE statement",
 /* 139 */ "while_statement ::= WHILE LPAREN expression RPAREN statement",
 /* 140 */ "switch_statement ::= SWITCH LPAREN expression RPAREN LBRACE switch_body RBRACE",
 /* 141 */ "switch_body ::=",
 /* 142 */ "switch_body ::= switch_body switch_case",
 /* 143 */ "switch_body ::= switch_body default_case",
 /* 144 */ "switch_case ::= CASE literal COLON case_statements",
 /* 145 */ "default_case ::= DEFAULT COLON case_statements",
 /* 146 */ "case_statements ::= statement_sequence",
 /* 147 */ "extern_declaration ::= EXTERN STRING extern_type IDENTIFIER LPAREN extern_parameter_list RPAREN SEMICOLON",
 /* 148 */ "extern_type ::= INT",
 /* 149 */ "extern_type ::= STR",
 /* 150 */ "extern_parameter ::= extern_type IDENTIFIER",
 /* 151 */ "extern_parameter_list ::=",
 /* 152 */ "extern_parameter_list ::= extern_parameter",
 /* 153 */ "extern_parameter_list ::= extern_parameter_list COMMA extern_parameter",
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
  { 74, 1 },
  { 75, 1 },
  { 77, 1 },
  { 77, 2 },
  { 76, 0 },
  { 76, 1 },
  { 78, 1 },
  { 78, 1 },
  { 78, 1 },
  { 78, 1 },
  { 78, 1 },
  { 78, 1 },
  { 78, 1 },
  { 78, 1 },
  { 78, 1 },
  { 78, 1 },
  { 78, 1 },
  { 78, 1 },
  { 78, 1 },
  { 78, 1 },
  { 92, 1 },
  { 94, 0 },
  { 94, 1 },
  { 93, 1 },
  { 93, 3 },
  { 97, 1 },
  { 97, 1 },
  { 97, 1 },
  { 97, 1 },
  { 97, 1 },
  { 97, 1 },
  { 95, 1 },
  { 95, 5 },
  { 96, 1 },
  { 96, 3 },
  { 96, 3 },
  { 96, 3 },
  { 96, 3 },
  { 96, 3 },
  { 96, 3 },
  { 96, 3 },
  { 96, 3 },
  { 96, 3 },
  { 96, 3 },
  { 96, 3 },
  { 96, 3 },
  { 96, 3 },
  { 96, 3 },
  { 96, 3 },
  { 96, 3 },
  { 96, 3 },
  { 96, 3 },
  { 98, 1 },
  { 98, 2 },
  { 98, 2 },
  { 98, 2 },
  { 98, 2 },
  { 98, 1 },
  { 99, 1 },
  { 99, 2 },
  { 99, 2 },
  { 99, 1 },
  { 99, 3 },
  { 99, 3 },
  { 99, 4 },
  { 102, 4 },
  { 101, 1 },
  { 101, 1 },
  { 101, 3 },
  { 101, 1 },
  { 101, 1 },
  { 104, 1 },
  { 104, 1 },
  { 104, 1 },
  { 104, 1 },
  { 104, 1 },
  { 104, 1 },
  { 104, 1 },
  { 104, 1 },
  { 104, 1 },
  { 105, 1 },
  { 106, 3 },
  { 107, 1 },
  { 107, 3 },
  { 108, 1 },
  { 100, 2 },
  { 109, 1 },
  { 103, 0 },
  { 103, 1 },
  { 103, 3 },
  { 80, 1 },
  { 80, 2 },
  { 83, 2 },
  { 83, 3 },
  { 79, 3 },
  { 87, 3 },
  { 87, 2 },
  { 89, 2 },
  { 90, 2 },
  { 81, 1 },
  { 81, 2 },
  { 81, 2 },
  { 112, 2 },
  { 114, 1 },
  { 114, 3 },
  { 113, 1 },
  { 113, 3 },
  { 111, 4 },
  { 111, 5 },
  { 116, 2 },
  { 116, 1 },
  { 116, 3 },
  { 116, 2 },
  { 116, 2 },
  { 117, 1 },
  { 117, 1 },
  { 117, 1 },
  { 115, 1 },
  { 115, 2 },
  { 110, 6 },
  { 120, 1 },
  { 121, 3 },
  { 118, 0 },
  { 118, 1 },
  { 118, 1 },
  { 118, 3 },
  { 122, 1 },
  { 122, 3 },
  { 123, 1 },
  { 123, 3 },
  { 119, 1 },
  { 82, 8 },
  { 124, 1 },
  { 124, 2 },
  { 86, 7 },
  { 86, 7 },
  { 125, 2 },
  { 84, 5 },
  { 84, 7 },
  { 85, 5 },
  { 88, 7 },
  { 126, 0 },
  { 126, 2 },
  { 126, 2 },
  { 127, 4 },
  { 128, 3 },
  { 129, 1 },
  { 91, 8 },
  { 130, 1 },
  { 130, 1 },
  { 132, 2 },
  { 131, 0 },
  { 131, 1 },
  { 131, 3 },
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
#line 75 "cscript.in"
{ p->SetRoot(yymsp[0].minor.yy109); }
#line 1255 "cscript.c"
        break;
      case 1:
#line 78 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(translation_unit, yymsp[0].minor.yy109); }
#line 1260 "cscript.c"
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
      case 66:
      case 67:
      case 69:
      case 86:
      case 99:
      case 102:
      case 105:
      case 130:
      case 132:
      case 146:
#line 81 "cscript.in"
{ yygotominor.yy109 = yymsp[0].minor.yy109; }
#line 1298 "cscript.c"
        break;
      case 3:
#line 82 "cscript.in"
{ 
  if(yymsp[-1].minor.yy109->m_type == statement_sequence) {
    yygotominor.yy109 = yymsp[-1].minor.yy109;
  }
  else {
    yygotominor.yy109 = p->AllocAst(statement_sequence, new AstList);
    yygotominor.yy109->m_a1.GetList()->push_back(yymsp[-1].minor.yy109);
  }
  yygotominor.yy109->m_a1.GetList()->push_back(yymsp[0].minor.yy109);
}
#line 1312 "cscript.c"
        break;
      case 4:
      case 21:
#line 95 "cscript.in"
{ yygotominor.yy109 = 0; }
#line 1318 "cscript.c"
        break;
      case 19:
      case 90:
#line 114 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(empty_statement); }
#line 1324 "cscript.c"
        break;
      case 24:
#line 130 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(assignment_expression, yymsp[-1].minor.yy54, yymsp[-2].minor.yy109, yymsp[0].minor.yy109); }
#line 1329 "cscript.c"
        break;
      case 25:
#line 134 "cscript.in"
{ yygotominor.yy54 = op_assign; }
#line 1334 "cscript.c"
        break;
      case 26:
#line 135 "cscript.in"
{ yygotominor.yy54 = op_assadd; }
#line 1339 "cscript.c"
        break;
      case 27:
#line 136 "cscript.in"
{ yygotominor.yy54 = op_asssub; }
#line 1344 "cscript.c"
        break;
      case 28:
#line 137 "cscript.in"
{ yygotominor.yy54 = op_assmul; }
#line 1349 "cscript.c"
        break;
      case 29:
#line 138 "cscript.in"
{ yygotominor.yy54 = op_assdiv; }
#line 1354 "cscript.c"
        break;
      case 30:
#line 139 "cscript.in"
{ yygotominor.yy54 = op_assmod; }
#line 1359 "cscript.c"
        break;
      case 32:
#line 143 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(ternary_expression, yymsp[-4].minor.yy109, yymsp[-2].minor.yy109, yymsp[0].minor.yy109); }
#line 1364 "cscript.c"
        break;
      case 34:
#line 147 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(binary_expression, op_logor,   yymsp[-2].minor.yy109, yymsp[0].minor.yy109); }
#line 1369 "cscript.c"
        break;
      case 35:
#line 148 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(binary_expression, op_logand,  yymsp[-2].minor.yy109, yymsp[0].minor.yy109); }
#line 1374 "cscript.c"
        break;
      case 36:
#line 149 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(binary_expression, op_bitor,   yymsp[-2].minor.yy109, yymsp[0].minor.yy109); }
#line 1379 "cscript.c"
        break;
      case 37:
#line 150 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(binary_expression, op_bitxor,  yymsp[-2].minor.yy109, yymsp[0].minor.yy109); }
#line 1384 "cscript.c"
        break;
      case 38:
#line 151 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(binary_expression, op_bitand,  yymsp[-2].minor.yy109, yymsp[0].minor.yy109); }
#line 1389 "cscript.c"
        break;
      case 39:
#line 152 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(binary_expression, op_eq,   yymsp[-2].minor.yy109, yymsp[0].minor.yy109); }
#line 1394 "cscript.c"
        break;
      case 40:
#line 153 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(binary_expression, op_ne,   yymsp[-2].minor.yy109, yymsp[0].minor.yy109); }
#line 1399 "cscript.c"
        break;
      case 41:
#line 154 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(binary_expression, op_lt,   yymsp[-2].minor.yy109, yymsp[0].minor.yy109); }
#line 1404 "cscript.c"
        break;
      case 42:
#line 155 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(binary_expression, op_le,   yymsp[-2].minor.yy109, yymsp[0].minor.yy109); }
#line 1409 "cscript.c"
        break;
      case 43:
#line 156 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(binary_expression, op_gt,   yymsp[-2].minor.yy109, yymsp[0].minor.yy109); }
#line 1414 "cscript.c"
        break;
      case 44:
#line 157 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(binary_expression, op_ge,   yymsp[-2].minor.yy109, yymsp[0].minor.yy109); }
#line 1419 "cscript.c"
        break;
      case 45:
#line 158 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(binary_expression, op_add,  yymsp[-2].minor.yy109, yymsp[0].minor.yy109); }
#line 1424 "cscript.c"
        break;
      case 46:
#line 159 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(binary_expression, op_sub,  yymsp[-2].minor.yy109, yymsp[0].minor.yy109); }
#line 1429 "cscript.c"
        break;
      case 47:
#line 160 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(binary_expression, op_mul,  yymsp[-2].minor.yy109, yymsp[0].minor.yy109); }
#line 1434 "cscript.c"
        break;
      case 48:
#line 161 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(binary_expression, op_div,  yymsp[-2].minor.yy109, yymsp[0].minor.yy109); }
#line 1439 "cscript.c"
        break;
      case 49:
#line 162 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(binary_expression, op_mod,  yymsp[-2].minor.yy109, yymsp[0].minor.yy109); }
#line 1444 "cscript.c"
        break;
      case 50:
#line 163 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(binary_expression, op_seq,  yymsp[-2].minor.yy109, yymsp[0].minor.yy109); }
#line 1449 "cscript.c"
        break;
      case 51:
#line 164 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(binary_expression, op_sne,  yymsp[-2].minor.yy109, yymsp[0].minor.yy109); }
#line 1454 "cscript.c"
        break;
      case 53:
#line 168 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(prefix_expression, op_negate, yymsp[0].minor.yy109); }
#line 1459 "cscript.c"
        break;
      case 54:
#line 169 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(prefix_expression, op_preinc, yymsp[0].minor.yy109); }
#line 1464 "cscript.c"
        break;
      case 55:
#line 170 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(prefix_expression, op_predec, yymsp[0].minor.yy109); }
#line 1469 "cscript.c"
        break;
      case 56:
#line 171 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(prefix_expression, op_not,    yymsp[0].minor.yy109); }
#line 1474 "cscript.c"
        break;
      case 59:
#line 176 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(postfix_expression, op_postinc, yymsp[-1].minor.yy109); }
#line 1479 "cscript.c"
        break;
      case 60:
#line 177 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(postfix_expression, op_postdec, yymsp[-1].minor.yy109); }
#line 1484 "cscript.c"
        break;
      case 62:
#line 179 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(member_expression, yymsp[-2].minor.yy109, String(yymsp[0].minor.yy0)); }
#line 1489 "cscript.c"
        break;
      case 63:
#line 180 "cscript.in"
{ yygotominor.yy109 = yymsp[0].minor.yy109; yymsp[0].minor.yy109->m_a3 = yymsp[-2].minor.yy109; }
#line 1494 "cscript.c"
        break;
      case 64:
#line 181 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(index_expression, yymsp[-3].minor.yy109, yymsp[-1].minor.yy109); }
#line 1499 "cscript.c"
        break;
      case 65:
#line 184 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(function_call, String(yymsp[-3].minor.yy0), yymsp[-1].minor.yy43); }
#line 1504 "cscript.c"
        break;
      case 68:
      case 100:
      case 101:
      case 133:
#line 189 "cscript.in"
{ yygotominor.yy109 = yymsp[-1].minor.yy109; }
#line 1512 "cscript.c"
        break;
      case 70:
#line 191 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(this_expression); }
#line 1517 "cscript.c"
        break;
      case 71:
#line 194 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(literal_value, Variant(String(yymsp[0].minor.yy0), Variant::stInt)); }
#line 1522 "cscript.c"
        break;
      case 72:
#line 195 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(literal_value, Variant(hex2dec(String(yymsp[0].minor.yy0).c_str()))); }
#line 1527 "cscript.c"
        break;
      case 73:
#line 196 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(literal_value, Variant(bin2dec(String(yymsp[0].minor.yy0).c_str()))); }
#line 1532 "cscript.c"
        break;
      case 74:
#line 197 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(literal_value, Variant(rom2dec(String(yymsp[0].minor.yy0).c_str()))); }
#line 1537 "cscript.c"
        break;
      case 75:
#line 198 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(literal_value, Variant(String(yymsp[0].minor.yy0), Variant::stReal)); }
#line 1542 "cscript.c"
        break;
      case 76:
#line 199 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(literal_value, Variant(String(yymsp[0].minor.yy0), Variant::stString)); }
#line 1547 "cscript.c"
        break;
      case 77:
#line 200 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(literal_value, Variant(true));    }
#line 1552 "cscript.c"
        break;
      case 78:
#line 201 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(literal_value, Variant(false));   }
#line 1557 "cscript.c"
        break;
      case 79:
#line 202 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(literal_value, Variant());        }
#line 1562 "cscript.c"
        break;
      case 80:
#line 205 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(lvalue, String(yymsp[0].minor.yy0)); }
#line 1567 "cscript.c"
        break;
      case 81:
#line 208 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(list_literal, yymsp[-1].minor.yy109); }
#line 1572 "cscript.c"
        break;
      case 82:
#line 209 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(list_content, yymsp[0].minor.yy109); }
#line 1577 "cscript.c"
        break;
      case 83:
#line 210 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(list_content, yymsp[-2].minor.yy109, yymsp[0].minor.yy109); }
#line 1582 "cscript.c"
        break;
      case 84:
#line 212 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(list_entry, yymsp[0].minor.yy109); }
#line 1587 "cscript.c"
        break;
      case 85:
#line 215 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(new_expression, String(yymsp[0].minor.yy0)); }
#line 1592 "cscript.c"
        break;
      case 87:
      case 122:
      case 141:
#line 228 "cscript.in"
{ yygotominor.yy43 = new AstList; }
#line 1599 "cscript.c"
        break;
      case 88:
      case 126:
      case 128:
#line 229 "cscript.in"
{ yygotominor.yy43 = new AstList; yygotominor.yy43->push_back(yymsp[0].minor.yy109); }
#line 1606 "cscript.c"
        break;
      case 89:
      case 127:
      case 129:
#line 230 "cscript.in"
{ yygotominor.yy43 = yymsp[-2].minor.yy43; yymsp[-2].minor.yy43->push_back(yymsp[0].minor.yy109); }
#line 1613 "cscript.c"
        break;
      case 91:
#line 239 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(expression_statement, yymsp[-1].minor.yy109); }
#line 1618 "cscript.c"
        break;
      case 92:
#line 242 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(compound_statement); }
#line 1623 "cscript.c"
        break;
      case 93:
#line 243 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(compound_statement, yymsp[-1].minor.yy109); }
#line 1628 "cscript.c"
        break;
      case 94:
#line 246 "cscript.in"
{ p->Parse(yymsp[-1].minor.yy0); yygotominor.yy109 = p->GetRoot(); }
#line 1633 "cscript.c"
        break;
      case 95:
#line 249 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(return_statement, yymsp[-1].minor.yy109); }
#line 1638 "cscript.c"
        break;
      case 96:
#line 250 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(return_statement);    }
#line 1643 "cscript.c"
        break;
      case 97:
#line 253 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(break_statement); }
#line 1648 "cscript.c"
        break;
      case 98:
#line 254 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(continue_statement); }
#line 1653 "cscript.c"
        break;
      case 103:
#line 268 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(variable_declaration, String(yymsp[0].minor.yy0));    }
#line 1658 "cscript.c"
        break;
      case 104:
#line 269 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(variable_declaration, String(yymsp[-2].minor.yy0), yymsp[0].minor.yy109); }
#line 1663 "cscript.c"
        break;
      case 106:
#line 272 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(declaration_sequence, yymsp[-2].minor.yy109, yymsp[0].minor.yy109); }
#line 1668 "cscript.c"
        break;
      case 107:
#line 279 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(class_declaration, String(yymsp[-2].minor.yy0)); }
#line 1673 "cscript.c"
        break;
      case 108:
#line 280 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(class_declaration, String(yymsp[-3].minor.yy0), yymsp[-1].minor.yy43); }
#line 1678 "cscript.c"
        break;
      case 109:
#line 283 "cscript.in"
{ yygotominor.yy109 = yymsp[-1].minor.yy109; yymsp[-1].minor.yy109->m_props["access"] = accessDefault; }
#line 1683 "cscript.c"
        break;
      case 110:
#line 284 "cscript.in"
{ yygotominor.yy109 = yymsp[0].minor.yy109; yymsp[0].minor.yy109->m_props["access"] = accessDefault; }
#line 1688 "cscript.c"
        break;
      case 111:
#line 285 "cscript.in"
{ yygotominor.yy109 = yymsp[-1].minor.yy109; yymsp[-1].minor.yy109->m_props["access"] = yymsp[-2].minor.yy222; }
#line 1693 "cscript.c"
        break;
      case 112:
#line 286 "cscript.in"
{ yygotominor.yy109 = yymsp[0].minor.yy109; yymsp[0].minor.yy109->m_props["access"] = yymsp[-1].minor.yy222; }
#line 1698 "cscript.c"
        break;
      case 113:
#line 287 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(access_specifier, yymsp[-1].minor.yy222); }
#line 1703 "cscript.c"
        break;
      case 114:
#line 291 "cscript.in"
{ yygotominor.yy222 = accessPrivate;   }
#line 1708 "cscript.c"
        break;
      case 115:
#line 292 "cscript.in"
{ yygotominor.yy222 = accessProtected; }
#line 1713 "cscript.c"
        break;
      case 116:
#line 293 "cscript.in"
{ yygotominor.yy222 = accessPublic;    }
#line 1718 "cscript.c"
        break;
      case 117:
#line 297 "cscript.in"
{ 
  yygotominor.yy43 = new AstList;
  yygotominor.yy43->push_back(yymsp[0].minor.yy109);
}
#line 1726 "cscript.c"
        break;
      case 118:
#line 301 "cscript.in"
{ 
  yygotominor.yy43 = yymsp[-1].minor.yy43;
  yygotominor.yy43->push_back(yymsp[0].minor.yy109);
}
#line 1734 "cscript.c"
        break;
      case 119:
#line 312 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(function_declaration, String(yymsp[-4].minor.yy0), yymsp[-2].minor.yy43, yymsp[0].minor.yy109); }
#line 1739 "cscript.c"
        break;
      case 120:
#line 315 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(parameter, String(yymsp[0].minor.yy0)); }
#line 1744 "cscript.c"
        break;
      case 121:
#line 318 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(parameter, String(yymsp[-2].minor.yy0), yymsp[0].minor.yy109); }
#line 1749 "cscript.c"
        break;
      case 123:
      case 124:
#line 323 "cscript.in"
{ yygotominor.yy43 = yymsp[0].minor.yy43; }
#line 1755 "cscript.c"
        break;
      case 125:
#line 325 "cscript.in"
{ yygotominor.yy43 = yymsp[-2].minor.yy43; yymsp[-2].minor.yy43->adopt(*yymsp[0].minor.yy43); }
#line 1760 "cscript.c"
        break;
      case 131:
#line 347 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(for_statement, yymsp[-5].minor.yy109, yymsp[-4].minor.yy109, yymsp[-2].minor.yy109, yymsp[0].minor.yy109); }
#line 1765 "cscript.c"
        break;
      case 134:
      case 135:
#line 358 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(foreach_statement, yymsp[-4].minor.yy109, yymsp[-2].minor.yy109, yymsp[0].minor.yy109); }
#line 1771 "cscript.c"
        break;
      case 136:
#line 360 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(variable_declaration, String(yymsp[0].minor.yy0)); }
#line 1776 "cscript.c"
        break;
      case 137:
#line 371 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(if_statement, yymsp[-2].minor.yy109, yymsp[0].minor.yy109); }
#line 1781 "cscript.c"
        break;
      case 138:
#line 372 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(if_statement, yymsp[-4].minor.yy109, yymsp[-2].minor.yy109, yymsp[0].minor.yy109); }
#line 1786 "cscript.c"
        break;
      case 139:
#line 380 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(while_statement, yymsp[-2].minor.yy109,  yymsp[0].minor.yy109); }
#line 1791 "cscript.c"
        break;
      case 140:
#line 388 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(switch_statement, yymsp[-4].minor.yy109, yymsp[-1].minor.yy43); }
#line 1796 "cscript.c"
        break;
      case 142:
      case 143:
#line 393 "cscript.in"
{ yygotominor.yy43 = yymsp[-1].minor.yy43; yygotominor.yy43->push_back(yymsp[0].minor.yy109); }
#line 1802 "cscript.c"
        break;
      case 144:
#line 397 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(switch_case, yymsp[-2].minor.yy109, yymsp[0].minor.yy109); }
#line 1807 "cscript.c"
        break;
      case 145:
#line 400 "cscript.in"
{ yygotominor.yy109 = p->AllocAst(default_case, yymsp[0].minor.yy109); }
#line 1812 "cscript.c"
        break;
      case 147:
#line 410 "cscript.in"
{ 
  yygotominor.yy109 = new Ast(extern_declaration, String(yymsp[-4].minor.yy0), String(yymsp[-6].minor.yy0), yymsp[-5].minor.yy46, yymsp[-2].minor.yy43); }
#line 1818 "cscript.c"
        break;
      case 148:
#line 414 "cscript.in"
{ yygotominor.yy46 = 1; }
#line 1823 "cscript.c"
        break;
      case 149:
#line 415 "cscript.in"
{ yygotominor.yy46 = 2; }
#line 1828 "cscript.c"
        break;
      case 150:
#line 417 "cscript.in"
{ yygotominor.yy109 = new Ast(extern_parameter, String(yymsp[0].minor.yy0), yymsp[-1].minor.yy46); }
#line 1833 "cscript.c"
        break;
      case 151:
#line 420 "cscript.in"
{ yygotominor.yy43 = new AstList(); }
#line 1838 "cscript.c"
        break;
      case 152:
#line 421 "cscript.in"
{ yygotominor.yy43 = new AstList(); yygotominor.yy43->push_back(yymsp[0].minor.yy109); }
#line 1843 "cscript.c"
        break;
      case 153:
#line 422 "cscript.in"
{ yygotominor.yy43 = yymsp[-2].minor.yy43; yygotominor.yy43->push_back(yymsp[0].minor.yy109); }
#line 1848 "cscript.c"
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
#line 54 "cscript.in"

  p->OnParseFailure();
#line 1896 "cscript.c"
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
#line 57 "cscript.in"

  p->OnSyntaxError();
#line 1914 "cscript.c"
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

