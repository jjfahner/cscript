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
#define YYNSTATE 257
#define YYNRULE 148
#define YYERRORSYMBOL 69
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
 /*     0 */   226,  406,  152,  252,   14,  251,  247,  246,  243,  242,
 /*    10 */   240,  238,  237,  236,  235,  233,  230,  229,  227,  127,
 /*    20 */   225,  147,  224,   66,   32,  221,   85,  210,  209,  208,
 /*    30 */    56,  207,  206,  203,   41,   48,   51,  145,  137,  134,
 /*    40 */   131,  113,   55,   54,   53,  168,  123,   17,  144,   25,
 /*    50 */   149,  202,  201,  199,  198,  197,  196,  195,  194,  192,
 /*    60 */   191,   62,   95,  153,    3,  256,  132,   21,  139,  140,
 /*    70 */   138,   64,   99,  125,  101,   94,   56,  165,  190,  135,
 /*    80 */   102,  109,   92,   64,  245,  249,  130,  128,   55,   54,
 /*    90 */    53,   19,  123,   17,  148,   25,  141,  202,  201,  199,
 /*   100 */   198,  197,  196,  195,  194,  192,  191,  133,   95,  153,
 /*   110 */     3,  150,  132,   21,  139,  140,  138,   64,   99,  125,
 /*   120 */   101,   94,  174,   26,  226,  135,  102,  109,    8,  251,
 /*   130 */   247,  246,  243,  242,  240,  238,  237,  236,  235,  233,
 /*   140 */   230,  229,  227,  127,  225,  178,  224,   66,  106,  221,
 /*   150 */    85,  210,  209,  208,  179,  207,  206,  203,  213,  214,
 /*   160 */   212,  145,  137,  134,  131,  394,   76,   56,  221,   85,
 /*   170 */   210,  209,  208,  151,  207,  206,  203,   67,  104,   55,
 /*   180 */    54,   53,  222,  123,   17,  100,   25,   61,  202,  201,
 /*   190 */   199,  198,  197,  196,  195,  194,  192,  191,  124,   95,
 /*   200 */   153,    3,    5,  132,   21,  139,  140,  138,   64,   99,
 /*   210 */   125,  101,   94,  126,  159,  163,  135,  102,  109,  226,
 /*   220 */   158,   64,    7,    8,  251,  247,  246,  243,  242,  240,
 /*   230 */   238,  237,  236,  235,  233,  230,  229,  227,  127,  225,
 /*   240 */   119,  224,   66,  182,  221,   85,  210,  209,  208,   68,
 /*   250 */   207,  206,  203,  253,   20,  254,  145,  137,  134,  131,
 /*   260 */    42,   43,   44,   45,   47,   50,   46,   36,   41,   48,
 /*   270 */    51,  248,  249,  226,  122,   93,   69,  219,  186,  247,
 /*   280 */   246,  243,  242,  240,  238,  237,  236,  235,  233,  230,
 /*   290 */   229,  227,  127,  225,   96,  224,   66,  110,  221,   85,
 /*   300 */   210,  209,  208,   34,  207,  206,  203,   88,   22,   16,
 /*   310 */   145,  137,  134,  131,  124,  220,  218,  226,   70,    9,
 /*   320 */    29,  185,  186,  247,  246,  243,  242,  240,  238,  237,
 /*   330 */   236,  235,  233,  230,  229,  227,  127,  225,  146,  224,
 /*   340 */    66,   11,  221,   85,  210,  209,  208,  204,  207,  206,
 /*   350 */   203,   26,  175,    4,  145,  137,  134,  131,   64,   23,
 /*   360 */   113,  101,  226,   63,  169,  241,    6,  251,  247,  246,
 /*   370 */   243,  242,  240,  238,  237,  236,  235,  233,  230,  229,
 /*   380 */   227,  127,  225,  234,  224,   66,   15,  221,   85,  210,
 /*   390 */   209,  208,   12,  207,  206,  203,  187,   24,  226,  145,
 /*   400 */   137,  134,  131,  108,  247,  246,  243,  242,  240,  238,
 /*   410 */   237,  236,  235,  233,  230,  229,  227,  127,  225,    2,
 /*   420 */   224,   66,   18,  221,   85,  210,  209,  208,  167,  207,
 /*   430 */   206,  203,   13,   33,   71,  145,  137,  134,  131,  226,
 /*   440 */   107,   58,   57,  111,  239,  247,  246,  243,  242,  240,
 /*   450 */   238,  237,  236,  235,  233,  230,  229,  227,  127,  225,
 /*   460 */    27,  224,   66,   31,  221,   85,  210,  209,  208,  172,
 /*   470 */   207,  206,  203,   10,   60,  226,  145,  137,  134,  131,
 /*   480 */   193,  247,  246,  243,  242,  240,  238,  237,  236,  235,
 /*   490 */   233,  230,  229,  227,  127,  225,    1,  224,   66,  177,
 /*   500 */   221,   85,  210,  209,  208,  142,  207,  206,  203,  112,
 /*   510 */   407,  407,  145,  137,  134,  131,  226,  407,  407,  407,
 /*   520 */   407,  231,  247,  246,  243,  242,  240,  238,  237,  236,
 /*   530 */   235,  233,  230,  229,  227,  127,  225,  407,  224,   66,
 /*   540 */   407,  221,   85,  210,  209,  208,  407,  207,  206,  203,
 /*   550 */   407,  407,  226,  145,  137,  134,  131,  200,  247,  246,
 /*   560 */   243,  242,  240,  238,  237,  236,  235,  233,  230,  229,
 /*   570 */   227,  127,  225,  407,  224,   66,  407,  221,   85,  210,
 /*   580 */   209,  208,  407,  207,  206,  203,  407,  407,  407,  145,
 /*   590 */   137,  134,  131,  226,  407,  407,  407,  407,  250,  247,
 /*   600 */   246,  243,  242,  240,  238,  237,  236,  235,  233,  230,
 /*   610 */   229,  227,  127,  225,  407,  224,   66,  407,  221,   85,
 /*   620 */   210,  209,  208,  407,  207,  206,  203,  407,  407,  226,
 /*   630 */   145,  137,  134,  131,  205,  247,  246,  243,  242,  240,
 /*   640 */   238,  237,  236,  235,  233,  230,  229,  227,  127,  225,
 /*   650 */   407,  224,   66,  407,  221,   85,  210,  209,  208,  407,
 /*   660 */   207,  206,  203,  407,  407,  407,  145,  137,  134,  131,
 /*   670 */    40,   39,   37,   35,   49,   52,   38,   42,   43,   44,
 /*   680 */    45,   47,   50,   46,   36,   41,   48,   51,  171,  170,
 /*   690 */   166,  164,  162,  161,   30,   56,   82,  407,  221,   85,
 /*   700 */   210,  209,  208,  407,  207,  206,  203,   55,   54,   53,
 /*   710 */   407,  123,   17,  407,   25,  407,  202,  201,  199,  198,
 /*   720 */   197,  196,  195,  194,  192,  191,  407,   95,  153,   83,
 /*   730 */    56,  221,   85,  210,  209,  208,   65,  207,  206,  203,
 /*   740 */   407,  407,   55,   54,   53,  407,  123,   17,  407,   25,
 /*   750 */   183,  202,  201,  199,  198,  197,  196,  195,  194,  192,
 /*   760 */   191,  407,   95,   56,  154,  407,  221,   85,  210,  209,
 /*   770 */   208,  407,  207,  206,  203,   55,   54,   53,  407,  123,
 /*   780 */    17,  407,   25,  407,  202,  201,  199,  198,  197,  196,
 /*   790 */   195,  194,  192,  191,  407,   95,  143,   86,   56,  221,
 /*   800 */    85,  210,  209,  208,  407,  207,  206,  203,  407,  407,
 /*   810 */    55,   54,   53,  407,  123,   17,  407,   25,  407,  202,
 /*   820 */   201,  199,  198,  197,  196,  195,  194,  192,  191,  407,
 /*   830 */    95,   39,   37,   35,   49,   52,   38,   42,   43,   44,
 /*   840 */    45,   47,   50,   46,   36,   41,   48,   51,   37,   35,
 /*   850 */    49,   52,   38,   42,   43,   44,   45,   47,   50,   46,
 /*   860 */    36,   41,   48,   51,  232,   77,  407,  221,   85,  210,
 /*   870 */   209,  208,  173,  207,  206,  203,  127,  225,   64,  224,
 /*   880 */    66,  101,  221,   85,  210,  209,  208,  407,  207,  115,
 /*   890 */   203,  407,  407,  407,  407,  407,   84,  117,  221,   85,
 /*   900 */   210,  209,  208,  407,  207,  206,  203,  407,  407,  407,
 /*   910 */    28,  118,  407,  407,  407,   35,   49,   52,   38,   42,
 /*   920 */    43,   44,   45,   47,   50,   46,   36,   41,   48,   51,
 /*   930 */   181,  225,  407,  224,   66,  407,  221,   85,  210,  209,
 /*   940 */   208,   90,  207,  206,  203,  407,  407,  176,   49,   52,
 /*   950 */    38,   42,   43,   44,   45,   47,   50,   46,   36,   41,
 /*   960 */    48,   51,  407,  407,  407,  188,  225,  407,  224,   66,
 /*   970 */   407,  221,   85,  210,  209,  208,  407,  207,  206,  203,
 /*   980 */    89,  184,   52,   38,   42,   43,   44,   45,   47,   50,
 /*   990 */    46,   36,   41,   48,   51,   46,   36,   41,   48,   51,
 /*  1000 */   181,  225,  407,  224,   66,  407,  221,   85,  210,  209,
 /*  1010 */   208,  407,  207,  206,  203,  407,  407,  180,  228,  225,
 /*  1020 */   116,  224,   66,  407,  221,   85,  210,  209,  208,  407,
 /*  1030 */   207,  206,  203,  188,  225,  407,  224,   66,  407,  221,
 /*  1040 */    85,  210,  209,  208,  407,  207,  206,  203,  407,  189,
 /*  1050 */    98,  225,  407,  224,   66,  407,  221,   85,  210,  209,
 /*  1060 */   208,  407,  207,  206,  203,  407,   97,  225,  407,  224,
 /*  1070 */    66,  407,  221,   85,  210,  209,  208,  407,  207,  206,
 /*  1080 */   203,  103,  225,  407,  224,   66,  407,  221,   85,  210,
 /*  1090 */   209,  208,  407,  207,  206,  203,  121,  225,  407,  224,
 /*  1100 */    66,  407,  221,   85,  210,  209,  208,  407,  207,  206,
 /*  1110 */   203,  244,  225,  407,  224,   66,  407,  221,   85,  210,
 /*  1120 */   209,  208,  407,  207,  206,  203,  129,  225,  407,  224,
 /*  1130 */    66,  407,  221,   85,  210,  209,  208,  407,  207,  206,
 /*  1140 */   203,  407,  105,  225,  407,  224,   66,  407,  221,   85,
 /*  1150 */   210,  209,  208,  407,  207,  206,  203,  407,  120,  225,
 /*  1160 */   407,  224,   66,  407,  221,   85,  210,  209,  208,  407,
 /*  1170 */   207,  206,  203,  157,  225,  407,  224,   66,  407,  221,
 /*  1180 */    85,  210,  209,  208,  407,  207,  206,  203,  114,  225,
 /*  1190 */   407,  224,   66,  407,  221,   85,  210,  209,  208,  407,
 /*  1200 */   207,  206,  203,   91,  225,  407,  224,   66,  407,  221,
 /*  1210 */    85,  210,  209,  208,  407,  207,  206,  203,  136,  225,
 /*  1220 */   407,  224,   66,  407,  221,   85,  210,  209,  208,  407,
 /*  1230 */   207,  206,  203,  160,  407,  224,   66,  407,  221,   85,
 /*  1240 */   210,  209,  208,  407,  207,  206,  203,  407,  223,  407,
 /*  1250 */   224,   66,  407,  221,   85,  210,  209,  208,  407,  207,
 /*  1260 */   206,  203,  407,   81,  407,  221,   85,  210,  209,  208,
 /*  1270 */   407,  207,  206,  203,   80,  407,  221,   85,  210,  209,
 /*  1280 */   208,  407,  207,  206,  203,   74,  407,  221,   85,  210,
 /*  1290 */   209,  208,  407,  207,  206,  203,   79,  407,  221,   85,
 /*  1300 */   210,  209,  208,  407,  207,  206,  203,  407,   87,  407,
 /*  1310 */   221,   85,  210,  209,  208,  407,  207,  206,  203,  156,
 /*  1320 */   407,  221,   85,  210,  209,  208,  407,  207,  206,  203,
 /*  1330 */   407,   72,  407,  221,   85,  210,  209,  208,  407,  207,
 /*  1340 */   206,  203,  407,   75,  407,  221,   85,  210,  209,  208,
 /*  1350 */   407,  207,  206,  203,   73,  407,  221,   85,  210,  209,
 /*  1360 */   208,  407,  207,  206,  203,   78,  407,  221,   85,  210,
 /*  1370 */   209,  208,  407,  207,  206,  203,   44,   45,   47,   50,
 /*  1380 */    46,   36,   41,   48,   51,  201,  199,  198,  197,  196,
 /*  1390 */   195,  194,  192,  191,  155,  407,  221,   85,  210,  209,
 /*  1400 */   208,  407,  207,  206,  203,  211,   85,  210,  209,  208,
 /*  1410 */   407,  207,  206,  203,  215,   85,  210,  209,  208,  407,
 /*  1420 */   207,  206,  203,  217,   85,  210,  209,  208,  407,  207,
 /*  1430 */   206,  203,  216,   85,  210,  209,  208,  407,  207,  206,
 /*  1440 */   203,  178,  407,  407,  106,  407,  407,  407,  407,   59,
 /*  1450 */   255,
};
static const YYCODETYPE yy_lookahead[] = {
 /*     0 */    69,   70,   71,   72,   73,   74,   75,   76,   77,   78,
 /*    10 */    79,   80,   81,   82,   83,   84,   85,   86,   87,   88,
 /*    20 */    89,   48,   91,   92,   34,   94,   95,   96,   97,   98,
 /*    30 */    15,  100,  101,  102,   16,   17,   18,  106,  107,  108,
 /*    40 */   109,  109,   27,   28,   29,  113,   31,   32,   48,   34,
 /*    50 */    48,   36,   37,   38,   39,   40,   41,   42,   43,   44,
 /*    60 */    45,  124,   47,   48,   49,   50,   51,   52,   53,   54,
 /*    70 */    55,   56,   57,   58,   59,   60,   15,   50,   33,   64,
 /*    80 */    65,   66,  116,   56,  118,  119,  120,  121,   27,   28,
 /*    90 */    29,   46,   31,   32,   48,   34,   48,   36,   37,   38,
 /*   100 */    39,   40,   41,   42,   43,   44,   45,   42,   47,   48,
 /*   110 */    49,   50,   51,   52,   53,   54,   55,   56,   57,   58,
 /*   120 */    59,   60,   98,   19,   69,   64,   65,   66,   73,   74,
 /*   130 */    75,   76,   77,   78,   79,   80,   81,   82,   83,   84,
 /*   140 */    85,   86,   87,   88,   89,  106,   91,   92,  109,   94,
 /*   150 */    95,   96,   97,   98,  115,  100,  101,  102,  125,  126,
 /*   160 */    50,  106,  107,  108,  109,   61,   92,   15,   94,   95,
 /*   170 */    96,   97,   98,   48,  100,  101,  102,   67,   68,   27,
 /*   180 */    28,   29,  127,   31,   32,   31,   34,   46,   36,   37,
 /*   190 */    38,   39,   40,   41,   42,   43,   44,   45,   31,   47,
 /*   200 */    48,   49,   35,   51,   52,   53,   54,   55,   56,   57,
 /*   210 */    58,   59,   60,  110,  111,   50,   64,   65,   66,   69,
 /*   220 */   111,   56,   35,   73,   74,   75,   76,   77,   78,   79,
 /*   230 */    80,   81,   82,   83,   84,   85,   86,   87,   88,   89,
 /*   240 */    31,   91,   92,   35,   94,   95,   96,   97,   98,   46,
 /*   250 */   100,  101,  102,  119,   46,   48,  106,  107,  108,  109,
 /*   260 */     8,    9,   10,   11,   12,   13,   14,   15,   16,   17,
 /*   270 */    18,  118,  119,   69,  121,   31,   46,  127,   74,   75,
 /*   280 */    76,   77,   78,   79,   80,   81,   82,   83,   84,   85,
 /*   290 */    86,   87,   88,   89,  100,   91,   92,   31,   94,   95,
 /*   300 */    96,   97,   98,   93,  100,  101,  102,   31,   19,   34,
 /*   310 */   106,  107,  108,  109,   31,   27,   28,   69,   30,   35,
 /*   320 */    32,  117,   74,   75,   76,   77,   78,   79,   80,   81,
 /*   330 */    82,   83,   84,   85,   86,   87,   88,   89,   48,   91,
 /*   340 */    92,   35,   94,   95,   96,   97,   98,   35,  100,  101,
 /*   350 */   102,   19,   50,   35,  106,  107,  108,  109,   56,   61,
 /*   360 */   109,   59,   69,  112,  113,  117,   73,   74,   75,   76,
 /*   370 */    77,   78,   79,   80,   81,   82,   83,   84,   85,   86,
 /*   380 */    87,   88,   89,   48,   91,   92,   34,   94,   95,   96,
 /*   390 */    97,   98,   35,  100,  101,  102,   31,   61,   69,  106,
 /*   400 */   107,  108,  109,   74,   75,   76,   77,   78,   79,   80,
 /*   410 */    81,   82,   83,   84,   85,   86,   87,   88,   89,   26,
 /*   420 */    91,   92,   48,   94,   95,   96,   97,   98,   48,  100,
 /*   430 */   101,  102,   35,   26,   49,  106,  107,  108,  109,   69,
 /*   440 */    31,   49,   34,   31,   74,   75,   76,   77,   78,   79,
 /*   450 */    80,   81,   82,   83,   84,   85,   86,   87,   88,   89,
 /*   460 */    34,   91,   92,   34,   94,   95,   96,   97,   98,   33,
 /*   470 */   100,  101,  102,   63,   49,   69,  106,  107,  108,  109,
 /*   480 */    74,   75,   76,   77,   78,   79,   80,   81,   82,   83,
 /*   490 */    84,   85,   86,   87,   88,   89,   26,   91,   92,   48,
 /*   500 */    94,   95,   96,   97,   98,   48,  100,  101,  102,   35,
 /*   510 */   128,  128,  106,  107,  108,  109,   69,  128,  128,  128,
 /*   520 */   128,   74,   75,   76,   77,   78,   79,   80,   81,   82,
 /*   530 */    83,   84,   85,   86,   87,   88,   89,  128,   91,   92,
 /*   540 */   128,   94,   95,   96,   97,   98,  128,  100,  101,  102,
 /*   550 */   128,  128,   69,  106,  107,  108,  109,   74,   75,   76,
 /*   560 */    77,   78,   79,   80,   81,   82,   83,   84,   85,   86,
 /*   570 */    87,   88,   89,  128,   91,   92,  128,   94,   95,   96,
 /*   580 */    97,   98,  128,  100,  101,  102,  128,  128,  128,  106,
 /*   590 */   107,  108,  109,   69,  128,  128,  128,  128,   74,   75,
 /*   600 */    76,   77,   78,   79,   80,   81,   82,   83,   84,   85,
 /*   610 */    86,   87,   88,   89,  128,   91,   92,  128,   94,   95,
 /*   620 */    96,   97,   98,  128,  100,  101,  102,  128,  128,   69,
 /*   630 */   106,  107,  108,  109,   74,   75,   76,   77,   78,   79,
 /*   640 */    80,   81,   82,   83,   84,   85,   86,   87,   88,   89,
 /*   650 */   128,   91,   92,  128,   94,   95,   96,   97,   98,  128,
 /*   660 */   100,  101,  102,  128,  128,  128,  106,  107,  108,  109,
 /*   670 */     1,    2,    3,    4,    5,    6,    7,    8,    9,   10,
 /*   680 */    11,   12,   13,   14,   15,   16,   17,   18,   19,   20,
 /*   690 */    21,   22,   23,   24,   25,   15,   92,  128,   94,   95,
 /*   700 */    96,   97,   98,  128,  100,  101,  102,   27,   28,   29,
 /*   710 */   128,   31,   32,  128,   34,  128,   36,   37,   38,   39,
 /*   720 */    40,   41,   42,   43,   44,   45,  128,   47,   48,   92,
 /*   730 */    15,   94,   95,   96,   97,   98,   56,  100,  101,  102,
 /*   740 */   128,  128,   27,   28,   29,  128,   31,   32,  128,   34,
 /*   750 */    35,   36,   37,   38,   39,   40,   41,   42,   43,   44,
 /*   760 */    45,  128,   47,   15,   92,  128,   94,   95,   96,   97,
 /*   770 */    98,  128,  100,  101,  102,   27,   28,   29,  128,   31,
 /*   780 */    32,  128,   34,  128,   36,   37,   38,   39,   40,   41,
 /*   790 */    42,   43,   44,   45,  128,   47,   48,   92,   15,   94,
 /*   800 */    95,   96,   97,   98,  128,  100,  101,  102,  128,  128,
 /*   810 */    27,   28,   29,  128,   31,   32,  128,   34,  128,   36,
 /*   820 */    37,   38,   39,   40,   41,   42,   43,   44,   45,  128,
 /*   830 */    47,    2,    3,    4,    5,    6,    7,    8,    9,   10,
 /*   840 */    11,   12,   13,   14,   15,   16,   17,   18,    3,    4,
 /*   850 */     5,    6,    7,    8,    9,   10,   11,   12,   13,   14,
 /*   860 */    15,   16,   17,   18,   76,   92,  128,   94,   95,   96,
 /*   870 */    97,   98,   50,  100,  101,  102,   88,   89,   56,   91,
 /*   880 */    92,   59,   94,   95,   96,   97,   98,  128,  100,  101,
 /*   890 */   102,  128,  128,  128,  128,  128,   92,  109,   94,   95,
 /*   900 */    96,   97,   98,  128,  100,  101,  102,  128,  128,  128,
 /*   910 */   122,  123,  128,  128,  128,    4,    5,    6,    7,    8,
 /*   920 */     9,   10,   11,   12,   13,   14,   15,   16,   17,   18,
 /*   930 */    88,   89,  128,   91,   92,  128,   94,   95,   96,   97,
 /*   940 */    98,   99,  100,  101,  102,  128,  128,  105,    5,    6,
 /*   950 */     7,    8,    9,   10,   11,   12,   13,   14,   15,   16,
 /*   960 */    17,   18,  128,  128,  128,   88,   89,  128,   91,   92,
 /*   970 */   128,   94,   95,   96,   97,   98,  128,  100,  101,  102,
 /*   980 */   103,  104,    6,    7,    8,    9,   10,   11,   12,   13,
 /*   990 */    14,   15,   16,   17,   18,   14,   15,   16,   17,   18,
 /*  1000 */    88,   89,  128,   91,   92,  128,   94,   95,   96,   97,
 /*  1010 */    98,  128,  100,  101,  102,  128,  128,  105,   88,   89,
 /*  1020 */    90,   91,   92,  128,   94,   95,   96,   97,   98,  128,
 /*  1030 */   100,  101,  102,   88,   89,  128,   91,   92,  128,   94,
 /*  1040 */    95,   96,   97,   98,  128,  100,  101,  102,  128,  104,
 /*  1050 */    88,   89,  128,   91,   92,  128,   94,   95,   96,   97,
 /*  1060 */    98,  128,  100,  101,  102,  128,   88,   89,  128,   91,
 /*  1070 */    92,  128,   94,   95,   96,   97,   98,  128,  100,  101,
 /*  1080 */   102,   88,   89,  128,   91,   92,  128,   94,   95,   96,
 /*  1090 */    97,   98,  128,  100,  101,  102,   88,   89,  128,   91,
 /*  1100 */    92,  128,   94,   95,   96,   97,   98,  128,  100,  101,
 /*  1110 */   102,   88,   89,  128,   91,   92,  128,   94,   95,   96,
 /*  1120 */    97,   98,  128,  100,  101,  102,   88,   89,  128,   91,
 /*  1130 */    92,  128,   94,   95,   96,   97,   98,  128,  100,  101,
 /*  1140 */   102,  128,   88,   89,  128,   91,   92,  128,   94,   95,
 /*  1150 */    96,   97,   98,  128,  100,  101,  102,  128,   88,   89,
 /*  1160 */   128,   91,   92,  128,   94,   95,   96,   97,   98,  128,
 /*  1170 */   100,  101,  102,   88,   89,  128,   91,   92,  128,   94,
 /*  1180 */    95,   96,   97,   98,  128,  100,  101,  102,   88,   89,
 /*  1190 */   128,   91,   92,  128,   94,   95,   96,   97,   98,  128,
 /*  1200 */   100,  101,  102,   88,   89,  128,   91,   92,  128,   94,
 /*  1210 */    95,   96,   97,   98,  128,  100,  101,  102,   88,   89,
 /*  1220 */   128,   91,   92,  128,   94,   95,   96,   97,   98,  128,
 /*  1230 */   100,  101,  102,   89,  128,   91,   92,  128,   94,   95,
 /*  1240 */    96,   97,   98,  128,  100,  101,  102,  128,   89,  128,
 /*  1250 */    91,   92,  128,   94,   95,   96,   97,   98,  128,  100,
 /*  1260 */   101,  102,  128,   92,  128,   94,   95,   96,   97,   98,
 /*  1270 */   128,  100,  101,  102,   92,  128,   94,   95,   96,   97,
 /*  1280 */    98,  128,  100,  101,  102,   92,  128,   94,   95,   96,
 /*  1290 */    97,   98,  128,  100,  101,  102,   92,  128,   94,   95,
 /*  1300 */    96,   97,   98,  128,  100,  101,  102,  128,   92,  128,
 /*  1310 */    94,   95,   96,   97,   98,  128,  100,  101,  102,   92,
 /*  1320 */   128,   94,   95,   96,   97,   98,  128,  100,  101,  102,
 /*  1330 */   128,   92,  128,   94,   95,   96,   97,   98,  128,  100,
 /*  1340 */   101,  102,  128,   92,  128,   94,   95,   96,   97,   98,
 /*  1350 */   128,  100,  101,  102,   92,  128,   94,   95,   96,   97,
 /*  1360 */    98,  128,  100,  101,  102,   92,  128,   94,   95,   96,
 /*  1370 */    97,   98,  128,  100,  101,  102,   10,   11,   12,   13,
 /*  1380 */    14,   15,   16,   17,   18,   37,   38,   39,   40,   41,
 /*  1390 */    42,   43,   44,   45,   92,  128,   94,   95,   96,   97,
 /*  1400 */    98,  128,  100,  101,  102,   94,   95,   96,   97,   98,
 /*  1410 */   128,  100,  101,  102,   94,   95,   96,   97,   98,  128,
 /*  1420 */   100,  101,  102,   94,   95,   96,   97,   98,  128,  100,
 /*  1430 */   101,  102,   94,   95,   96,   97,   98,  128,  100,  101,
 /*  1440 */   102,  106,  128,  128,  109,  128,  128,  128,  128,  114,
 /*  1450 */   115,
};
#define YY_SHIFT_USE_DFLT (-28)
#define YY_SHIFT_MAX 140
static const short yy_shift_ofst[] = {
 /*     0 */   152,  152,  152,   15,  152,  152,   61,  152,  152,  152,
 /*    10 */   152,  152,  152,  152,  152,  680,  715,  783,  783,  783,
 /*    20 */   783,  748,  783,  783,  783,  783,  783,  783,  783,  783,
 /*    30 */   783,  783,  783,  783,  783,  783,  783,  783,  783,  783,
 /*    40 */   783,  783,  783,  783,  783,  783,  783,  783,  783,  783,
 /*    50 */   783,  783,  783,  783,  783,  783,  783,  167,  822,  302,
 /*    60 */   165,  283,  110,   27,  209,  276,  669, 1348,  244,  209,
 /*    70 */   154,  -28,  829,  845,  911,  943,  976,  252,  252, 1366,
 /*    80 */  1366,  981,  981,  981,  981,  288,   18,   18,  104,   45,
 /*    90 */   208,  312,  318,  289,  352,  365,  393,  397,  407,  409,
 /*   100 */   275,  412,  429,  436,  470,  474,  451,  425,  410,  426,
 /*   110 */   392,  408,  385,  380,  374,  336,  357,  335,  298,  332,
 /*   120 */   306,  284,  203,  275,  289,  266,  230,  207,  203,  187,
 /*   130 */   141,  125,   65,   46,    2,  -10,  290,  -27,    0,   48,
 /*   140 */   457,
};
#define YY_REDUCE_USE_DFLT (-70)
#define YY_REDUCE_MAX 71
static const short yy_reduce_ofst[] = {
 /*     0 */   -69,   55,  150,  293,  204,  248,  524,  406,  524,  329,
 /*    10 */   483,  370,  447,  560,  524,  788,  842,  877,  930,  945,
 /*    20 */   912, 1130, 1023, 1070, 1038, 1115, 1085, 1054, 1100,  993,
 /*    30 */   962,  978, 1008, 1144, 1159, 1251, 1216, 1193, 1273, 1262,
 /*    40 */  1239, 1227, 1204, 1182, 1171,  804,  705,  637, 1302,   74,
 /*    50 */   604,  672,  773, 1311, 1320, 1338, 1329,  -34, 1335,   39,
 /*    60 */   251,  153,   33,  -68,  103,  103,  210,  194,  134,  109,
 /*    70 */    24,  -63,
};
static const YYACTIONTYPE yy_default[] = {
 /*     0 */   261,  405,  405,  405,  405,  405,  405,  405,  404,  405,
 /*    10 */   405,  405,  405,  405,  262,  405,  405,  405,  278,  405,
 /*    20 */   405,  405,  405,  405,  405,  405,  405,  405,  405,  405,
 /*    30 */   405,  405,  405,  405,  405,  405,  405,  405,  405,  405,
 /*    40 */   405,  405,  405,  405,  405,  405,  405,  405,  405,  405,
 /*    50 */   405,  405,  405,  405,  405,  405,  405,  405,  405,  405,
 /*    60 */   405,  405,  405,  405,  405,  405,  288,  405,  405,  405,
 /*    70 */   405,  399,  291,  292,  293,  294,  295,  307,  308,  296,
 /*    80 */   297,  298,  301,  300,  299,  309,  302,  303,  362,  405,
 /*    90 */   405,  405,  405,  405,  405,  405,  405,  405,  405,  405,
 /*   100 */   319,  405,  405,  405,  405,  405,  405,  405,  395,  405,
 /*   110 */   405,  405,  405,  405,  405,  325,  405,  405,  405,  362,
 /*   120 */   405,  405,  387,  338,  379,  405,  361,  405,  386,  405,
 /*   130 */   385,  405,  405,  405,  405,  405,  405,  405,  405,  405,
 /*   140 */   405,  354,  355,  353,  356,  357,  352,  358,  351,  359,
 /*   150 */   350,  360,  257,  347,  306,  305,  304,  363,  365,  364,
 /*   160 */   289,  287,  286,  366,  285,  367,  284,  368,  370,  369,
 /*   170 */   283,  282,  321,  371,  320,  372,  345,  373,  374,  376,
 /*   180 */   346,  344,  323,  322,  340,  377,  388,  343,  342,  341,
 /*   190 */   339,  337,  336,  392,  335,  334,  333,  332,  331,  330,
 /*   200 */   396,  329,  328,  327,  326,  397,  325,  324,  318,  315,
 /*   210 */   314,  313,  398,  400,  401,  312,  311,  310,  317,  402,
 /*   220 */   316,  290,  403,  281,  280,  277,  276,  275,  279,  274,
 /*   230 */   273,  389,  390,  272,  391,  271,  270,  269,  268,  393,
 /*   240 */   267,  378,  266,  265,  380,  381,  264,  263,  382,  383,
 /*   250 */   260,  259,  258,  384,  348,  375,  349,
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
  "VAR",           "STRUCT",        "CLASS",         "FUNCTION",    
  "FOR",           "IN",            "LOWER_THAN_ELSE",  "ELSE",        
  "IF",            "WHILE",         "SWITCH",        "CASE",        
  "DEFAULT",       "error",         "main",          "translation_unit",
  "statement_sequence_opt",  "statement_sequence",  "statement",     "include_statement",
  "expression_statement",  "declaration_statement",  "for_statement",  "compound_statement",
  "if_statement",  "while_statement",  "foreach_statement",  "return_statement",
  "switch_statement",  "break_statement",  "continue_statement",  "pause_statement",
  "expression",    "assignment_expression",  "expression_opt",  "conditional_expression",
  "binary_expression",  "assignment_operator",  "unary_expression",  "postfix_expression",
  "new_expression",  "primary_expression",  "function_call",  "argument_list",
  "literal",       "id_expression",  "list_literal",  "list_content",
  "list_entry",    "argument",      "function_declaration",  "struct_declaration",
  "class_declaration",  "variable_declaration",  "declarator_sequence",  "declarator",  
  "struct_members",  "struct_member",  "class_members",  "class_member",
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
 /* 101 */ "declaration_statement ::= struct_declaration SEMICOLON",
 /* 102 */ "declaration_statement ::= class_declaration SEMICOLON",
 /* 103 */ "declaration_statement ::= variable_declaration SEMICOLON",
 /* 104 */ "variable_declaration ::= VAR declarator_sequence",
 /* 105 */ "declarator ::= IDENTIFIER",
 /* 106 */ "declarator ::= IDENTIFIER ASSIGN expression",
 /* 107 */ "declarator_sequence ::= declarator",
 /* 108 */ "declarator_sequence ::= declarator_sequence COMMA declarator",
 /* 109 */ "struct_declaration ::= STRUCT IDENTIFIER LBRACE RBRACE",
 /* 110 */ "struct_declaration ::= STRUCT IDENTIFIER LBRACE struct_members RBRACE",
 /* 111 */ "struct_member ::= variable_declaration SEMICOLON",
 /* 112 */ "struct_members ::= struct_member",
 /* 113 */ "struct_members ::= struct_members struct_member",
 /* 114 */ "class_declaration ::= CLASS IDENTIFIER LBRACE RBRACE",
 /* 115 */ "class_declaration ::= CLASS IDENTIFIER LBRACE class_members RBRACE",
 /* 116 */ "class_member ::= variable_declaration SEMICOLON",
 /* 117 */ "class_member ::= function_declaration",
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
  { 70, 1 },
  { 71, 1 },
  { 73, 1 },
  { 73, 2 },
  { 72, 0 },
  { 72, 1 },
  { 74, 1 },
  { 74, 1 },
  { 74, 1 },
  { 74, 1 },
  { 74, 1 },
  { 74, 1 },
  { 74, 1 },
  { 74, 1 },
  { 74, 1 },
  { 74, 1 },
  { 74, 1 },
  { 74, 1 },
  { 74, 1 },
  { 74, 1 },
  { 88, 1 },
  { 90, 0 },
  { 90, 1 },
  { 89, 1 },
  { 89, 3 },
  { 93, 1 },
  { 93, 1 },
  { 93, 1 },
  { 93, 1 },
  { 93, 1 },
  { 93, 1 },
  { 91, 1 },
  { 91, 5 },
  { 92, 1 },
  { 92, 3 },
  { 92, 3 },
  { 92, 3 },
  { 92, 3 },
  { 92, 3 },
  { 92, 3 },
  { 92, 3 },
  { 92, 3 },
  { 92, 3 },
  { 92, 3 },
  { 92, 3 },
  { 92, 3 },
  { 92, 3 },
  { 92, 3 },
  { 92, 3 },
  { 92, 3 },
  { 92, 3 },
  { 92, 3 },
  { 94, 1 },
  { 94, 2 },
  { 94, 2 },
  { 94, 2 },
  { 94, 2 },
  { 94, 1 },
  { 95, 1 },
  { 95, 2 },
  { 95, 2 },
  { 95, 1 },
  { 95, 3 },
  { 95, 3 },
  { 95, 4 },
  { 98, 3 },
  { 98, 4 },
  { 97, 1 },
  { 97, 1 },
  { 97, 3 },
  { 97, 1 },
  { 97, 1 },
  { 100, 1 },
  { 100, 1 },
  { 100, 1 },
  { 100, 1 },
  { 100, 1 },
  { 100, 1 },
  { 100, 1 },
  { 100, 1 },
  { 100, 1 },
  { 101, 1 },
  { 102, 3 },
  { 103, 1 },
  { 103, 3 },
  { 104, 1 },
  { 96, 2 },
  { 105, 1 },
  { 99, 1 },
  { 99, 3 },
  { 76, 1 },
  { 76, 2 },
  { 79, 2 },
  { 79, 3 },
  { 75, 3 },
  { 83, 3 },
  { 83, 2 },
  { 85, 2 },
  { 86, 2 },
  { 87, 2 },
  { 77, 1 },
  { 77, 2 },
  { 77, 2 },
  { 77, 2 },
  { 109, 2 },
  { 111, 1 },
  { 111, 3 },
  { 110, 1 },
  { 110, 3 },
  { 107, 4 },
  { 107, 5 },
  { 113, 2 },
  { 112, 1 },
  { 112, 2 },
  { 108, 4 },
  { 108, 5 },
  { 115, 2 },
  { 115, 1 },
  { 114, 1 },
  { 114, 2 },
  { 106, 6 },
  { 106, 5 },
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
  { 78, 8 },
  { 122, 1 },
  { 122, 2 },
  { 82, 7 },
  { 82, 7 },
  { 123, 2 },
  { 80, 5 },
  { 80, 7 },
  { 81, 5 },
  { 84, 7 },
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
#line 1254 "astgen.c"
        break;
      case 1:
#line 78 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(translation_unit, yymsp[0].minor.yy215); }
#line 1259 "astgen.c"
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
      case 104:
      case 107:
      case 112:
      case 117:
      case 124:
      case 126:
      case 128:
      case 129:
      case 131:
      case 133:
      case 147:
#line 81 "astgen.in"
{ yygotominor.yy215 = yymsp[0].minor.yy215; }
#line 1303 "astgen.c"
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
#line 1317 "astgen.c"
        break;
      case 4:
      case 21:
#line 95 "astgen.in"
{ yygotominor.yy215 = 0; }
#line 1323 "astgen.c"
        break;
      case 19:
      case 90:
#line 114 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(empty_statement); }
#line 1329 "astgen.c"
        break;
      case 24:
#line 130 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(assignment_expression, yymsp[-1].minor.yy46, yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1334 "astgen.c"
        break;
      case 25:
#line 134 "astgen.in"
{ yygotominor.yy46 = op_assign; }
#line 1339 "astgen.c"
        break;
      case 26:
#line 135 "astgen.in"
{ yygotominor.yy46 = op_assadd; }
#line 1344 "astgen.c"
        break;
      case 27:
#line 136 "astgen.in"
{ yygotominor.yy46 = op_asssub; }
#line 1349 "astgen.c"
        break;
      case 28:
#line 137 "astgen.in"
{ yygotominor.yy46 = op_assmul; }
#line 1354 "astgen.c"
        break;
      case 29:
#line 138 "astgen.in"
{ yygotominor.yy46 = op_assdiv; }
#line 1359 "astgen.c"
        break;
      case 30:
#line 139 "astgen.in"
{ yygotominor.yy46 = op_assmod; }
#line 1364 "astgen.c"
        break;
      case 32:
#line 143 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(ternary_expression, yymsp[-4].minor.yy215, yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1369 "astgen.c"
        break;
      case 34:
#line 147 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(binary_expression, op_logor,   yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1374 "astgen.c"
        break;
      case 35:
#line 148 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(binary_expression, op_logand,  yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1379 "astgen.c"
        break;
      case 36:
#line 149 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(binary_expression, op_bitor,   yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1384 "astgen.c"
        break;
      case 37:
#line 150 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(binary_expression, op_bitxor,  yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1389 "astgen.c"
        break;
      case 38:
#line 151 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(binary_expression, op_bitand,  yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1394 "astgen.c"
        break;
      case 39:
#line 152 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(binary_expression, op_eq,   yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1399 "astgen.c"
        break;
      case 40:
#line 153 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(binary_expression, op_ne,   yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1404 "astgen.c"
        break;
      case 41:
#line 154 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(binary_expression, op_lt,   yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1409 "astgen.c"
        break;
      case 42:
#line 155 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(binary_expression, op_le,   yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1414 "astgen.c"
        break;
      case 43:
#line 156 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(binary_expression, op_gt,   yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1419 "astgen.c"
        break;
      case 44:
#line 157 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(binary_expression, op_ge,   yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1424 "astgen.c"
        break;
      case 45:
#line 158 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(binary_expression, op_add,  yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1429 "astgen.c"
        break;
      case 46:
#line 159 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(binary_expression, op_sub,  yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1434 "astgen.c"
        break;
      case 47:
#line 160 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(binary_expression, op_mul,  yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1439 "astgen.c"
        break;
      case 48:
#line 161 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(binary_expression, op_div,  yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1444 "astgen.c"
        break;
      case 49:
#line 162 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(binary_expression, op_mod,  yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1449 "astgen.c"
        break;
      case 50:
#line 163 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(binary_expression, op_seq,  yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1454 "astgen.c"
        break;
      case 51:
#line 164 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(binary_expression, op_sne,  yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1459 "astgen.c"
        break;
      case 53:
#line 168 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(prefix_expression, op_negate, yymsp[0].minor.yy215); }
#line 1464 "astgen.c"
        break;
      case 54:
#line 169 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(prefix_expression, op_preinc, yymsp[0].minor.yy215); }
#line 1469 "astgen.c"
        break;
      case 55:
#line 170 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(prefix_expression, op_predec, yymsp[0].minor.yy215); }
#line 1474 "astgen.c"
        break;
      case 56:
#line 171 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(prefix_expression, op_not,    yymsp[0].minor.yy215); }
#line 1479 "astgen.c"
        break;
      case 59:
#line 176 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(postfix_expression, op_postinc, yymsp[-1].minor.yy215); }
#line 1484 "astgen.c"
        break;
      case 60:
#line 177 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(postfix_expression, op_postdec, yymsp[-1].minor.yy215); }
#line 1489 "astgen.c"
        break;
      case 62:
#line 179 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(member_expression, yymsp[-2].minor.yy215, String(yymsp[0].minor.yy0)); }
#line 1494 "astgen.c"
        break;
      case 63:
#line 180 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(member_call, yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1499 "astgen.c"
        break;
      case 64:
#line 181 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(index_expression, yymsp[-3].minor.yy215, yymsp[-1].minor.yy215); }
#line 1504 "astgen.c"
        break;
      case 65:
#line 184 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(function_call, String(yymsp[-2].minor.yy0)); }
#line 1509 "astgen.c"
        break;
      case 66:
#line 185 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(function_call, String(yymsp[-3].minor.yy0), yymsp[-1].minor.yy215); }
#line 1514 "astgen.c"
        break;
      case 69:
      case 101:
      case 102:
      case 103:
      case 111:
      case 116:
      case 134:
#line 191 "astgen.in"
{ yygotominor.yy215 = yymsp[-1].minor.yy215; }
#line 1525 "astgen.c"
        break;
      case 71:
#line 193 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(this_expression); }
#line 1530 "astgen.c"
        break;
      case 72:
#line 196 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(literal, Variant(String(yymsp[0].minor.yy0), Variant::stInt)); }
#line 1535 "astgen.c"
        break;
      case 73:
#line 197 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(literal, Variant(hex2dec(String(yymsp[0].minor.yy0).c_str()))); }
#line 1540 "astgen.c"
        break;
      case 74:
#line 198 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(literal, Variant(bin2dec(String(yymsp[0].minor.yy0).c_str()))); }
#line 1545 "astgen.c"
        break;
      case 75:
#line 199 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(literal, Variant(rom2dec(String(yymsp[0].minor.yy0).c_str()))); }
#line 1550 "astgen.c"
        break;
      case 76:
#line 200 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(literal, Variant(String(yymsp[0].minor.yy0), Variant::stReal)); }
#line 1555 "astgen.c"
        break;
      case 77:
#line 201 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(literal, Variant(String(yymsp[0].minor.yy0), Variant::stString)); }
#line 1560 "astgen.c"
        break;
      case 78:
#line 202 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(literal, Variant(true));    }
#line 1565 "astgen.c"
        break;
      case 79:
#line 203 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(literal, Variant(false));   }
#line 1570 "astgen.c"
        break;
      case 80:
#line 204 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(literal, Variant());        }
#line 1575 "astgen.c"
        break;
      case 81:
#line 207 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(lvalue, String(yymsp[0].minor.yy0)); }
#line 1580 "astgen.c"
        break;
      case 82:
#line 210 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(list_literal, yymsp[-1].minor.yy215); }
#line 1585 "astgen.c"
        break;
      case 83:
#line 211 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(list_content, yymsp[0].minor.yy215); }
#line 1590 "astgen.c"
        break;
      case 84:
#line 212 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(list_content, yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1595 "astgen.c"
        break;
      case 85:
#line 213 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(list_entry, yymsp[0].minor.yy215); }
#line 1600 "astgen.c"
        break;
      case 86:
#line 216 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(new_expression, String(yymsp[0].minor.yy0)); }
#line 1605 "astgen.c"
        break;
      case 87:
#line 225 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(argument, yymsp[0].minor.yy215); }
#line 1610 "astgen.c"
        break;
      case 89:
#line 229 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(argument_list, yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1615 "astgen.c"
        break;
      case 91:
#line 238 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(expression_statement, yymsp[-1].minor.yy215); }
#line 1620 "astgen.c"
        break;
      case 92:
#line 241 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(compound_statement); }
#line 1625 "astgen.c"
        break;
      case 93:
#line 242 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(compound_statement, yymsp[-1].minor.yy215); }
#line 1630 "astgen.c"
        break;
      case 94:
#line 245 "astgen.in"
{ p->Parse(yymsp[-1].minor.yy0); yygotominor.yy215 = p->GetRoot(); }
#line 1635 "astgen.c"
        break;
      case 95:
#line 248 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(return_statement, yymsp[-1].minor.yy215); }
#line 1640 "astgen.c"
        break;
      case 96:
#line 249 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(return_statement);    }
#line 1645 "astgen.c"
        break;
      case 97:
#line 252 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(break_statement); }
#line 1650 "astgen.c"
        break;
      case 98:
#line 253 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(continue_statement); }
#line 1655 "astgen.c"
        break;
      case 99:
#line 256 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(pause_statement); }
#line 1660 "astgen.c"
        break;
      case 105:
#line 271 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(variable_declaration, String(yymsp[0].minor.yy0));    }
#line 1665 "astgen.c"
        break;
      case 106:
#line 272 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(variable_declaration, String(yymsp[-2].minor.yy0), yymsp[0].minor.yy215); }
#line 1670 "astgen.c"
        break;
      case 108:
#line 275 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(declaration_sequence, yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1675 "astgen.c"
        break;
      case 109:
#line 284 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(struct_declaration, String(yymsp[-2].minor.yy0)); }
#line 1680 "astgen.c"
        break;
      case 110:
#line 285 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(struct_declaration, String(yymsp[-3].minor.yy0), yymsp[-1].minor.yy215); }
#line 1685 "astgen.c"
        break;
      case 113:
#line 292 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(struct_members, yymsp[-1].minor.yy215, yymsp[0].minor.yy215); }
#line 1690 "astgen.c"
        break;
      case 114:
#line 299 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(class_declaration, String(yymsp[-2].minor.yy0)); }
#line 1695 "astgen.c"
        break;
      case 115:
#line 300 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(class_declaration, String(yymsp[-3].minor.yy0), yymsp[-1].minor.yy87); }
#line 1700 "astgen.c"
        break;
      case 118:
#line 306 "astgen.in"
{ 
  yygotominor.yy87 = new AstList;
  yygotominor.yy87->push_back(yymsp[0].minor.yy215);
}
#line 1708 "astgen.c"
        break;
      case 119:
#line 310 "astgen.in"
{ 
  yygotominor.yy87 = yymsp[-1].minor.yy87;
  yygotominor.yy87->push_back(yymsp[0].minor.yy215);
}
#line 1716 "astgen.c"
        break;
      case 120:
#line 321 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(function_declaration, String(yymsp[-4].minor.yy0), yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1721 "astgen.c"
        break;
      case 121:
#line 322 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(function_declaration, String(yymsp[-3].minor.yy0), AstData(), yymsp[0].minor.yy215); }
#line 1726 "astgen.c"
        break;
      case 122:
#line 325 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(parameter, String(yymsp[0].minor.yy0)); }
#line 1731 "astgen.c"
        break;
      case 123:
#line 328 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(parameter, String(yymsp[-2].minor.yy0), yymsp[0].minor.yy215); }
#line 1736 "astgen.c"
        break;
      case 125:
      case 127:
      case 130:
#line 332 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(parameter_list, yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1743 "astgen.c"
        break;
      case 132:
#line 353 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(for_statement, yymsp[-5].minor.yy215, yymsp[-4].minor.yy215, yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1748 "astgen.c"
        break;
      case 135:
      case 136:
#line 364 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(foreach_statement, yymsp[-4].minor.yy215, yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1754 "astgen.c"
        break;
      case 137:
#line 366 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(variable_declaration, String(yymsp[0].minor.yy0)); }
#line 1759 "astgen.c"
        break;
      case 138:
#line 377 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(if_statement, yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1764 "astgen.c"
        break;
      case 139:
#line 378 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(if_statement, yymsp[-4].minor.yy215, yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1769 "astgen.c"
        break;
      case 140:
#line 386 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(while_statement, yymsp[-2].minor.yy215,  yymsp[0].minor.yy215); }
#line 1774 "astgen.c"
        break;
      case 141:
#line 394 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(switch_statement, yymsp[-4].minor.yy215, yymsp[-1].minor.yy87); }
#line 1779 "astgen.c"
        break;
      case 142:
#line 398 "astgen.in"
{ yygotominor.yy87 = new AstList; }
#line 1784 "astgen.c"
        break;
      case 143:
      case 144:
#line 399 "astgen.in"
{ yygotominor.yy87 = yymsp[-1].minor.yy87; yygotominor.yy87->push_back(yymsp[0].minor.yy215); }
#line 1790 "astgen.c"
        break;
      case 145:
#line 403 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(switch_case, yymsp[-2].minor.yy215, yymsp[0].minor.yy215); }
#line 1795 "astgen.c"
        break;
      case 146:
#line 406 "astgen.in"
{ yygotominor.yy215 = p->AllocAst(default_case, yymsp[0].minor.yy215); }
#line 1800 "astgen.c"
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
#line 1848 "astgen.c"
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
#line 1866 "astgen.c"
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

