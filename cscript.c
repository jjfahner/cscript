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
#define YYNOCODE 127
#define YYACTIONTYPE unsigned short int
#define CScriptParseTOKENTYPE  Token 
typedef union {
  CScriptParseTOKENTYPE yy0;
  opcodes yy54;
  Ast* yy67;
  AccessTypes yy96;
  AstList* yy127;
  int yy253;
} YYMINORTYPE;
#ifndef YYSTACKDEPTH
#define YYSTACKDEPTH 100
#endif
#define CScriptParseARG_SDECL  Parser* p ;
#define CScriptParseARG_PDECL , Parser* p 
#define CScriptParseARG_FETCH  Parser* p  = yypParser->p 
#define CScriptParseARG_STORE yypParser->p  = p 
#define YYNSTATE 250
#define YYNRULE 146
#define YYERRORSYMBOL 70
#define YYERRSYMDT yy253
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
 /*     0 */   216,  397,  243,  241,   13,  240,  236,  235,  232,  231,
 /*    10 */   229,  227,  226,  225,  224,  222,  219,  218,  126,  215,
 /*    20 */   141,  214,   65,  173,  212,   84,  200,  199,  198,   56,
 /*    30 */   197,  196,  193,  163,   19,  113,  142,  130,  127,  247,
 /*    40 */    62,   55,   54,   53,  168,  103,   18,  167,   27,  106,
 /*    50 */   192,  191,  190,  188,  187,  186,  185,  184,  183,  181,
 /*    60 */   138,   97,  147,    3,  249,  131,   21,  136,  133,   63,
 /*    70 */   104,   56,  140,   63,  112,  134,  120,  154,  112,   93,
 /*    80 */    96,   98,  124,   55,   54,   53,    5,  103,   18,  132,
 /*    90 */    27,   61,  192,  191,  190,  188,  187,  186,  185,  184,
 /*   100 */   183,  181,  242,   97,  147,    3,  143,  131,   21,  136,
 /*   110 */   133,   63,  104,  163,   25,  113,  112,  134,   59,  248,
 /*   120 */    62,   93,   96,   98,  216,   48,   45,   38,   14,  240,
 /*   130 */   236,  235,  232,  231,  229,  227,  226,  225,  224,  222,
 /*   140 */   219,  218,  126,  215,  139,  214,   65,  144,  212,   84,
 /*   150 */   200,  199,  198,   29,  197,  196,  193,  385,  160,   56,
 /*   160 */   142,  130,  127,   63,  129,  244,  245,  246,  112,  202,
 /*   170 */   203,   55,   54,   53,   69,  103,   18,  153,   27,  211,
 /*   180 */   192,  191,  190,  188,  187,  186,  185,  184,  183,  181,
 /*   190 */   146,   97,  147,    3,  116,  131,   21,  136,  133,   63,
 /*   200 */   104,  145,  169,   60,  112,  134,  109,  107,  216,   93,
 /*   210 */    96,   98,   14,  240,  236,  235,  232,  231,  229,  227,
 /*   220 */   226,  225,  224,  222,  219,  218,  126,  215,   10,  214,
 /*   230 */    65,   88,  212,   84,  200,  199,  198,   68,  197,  196,
 /*   240 */   193,   34,   32,  216,  142,  130,  127,  124,  175,  236,
 /*   250 */   235,  232,  231,  229,  227,  226,  225,  224,  222,  219,
 /*   260 */   218,  126,  215,  208,  214,   65,  223,  212,   84,  200,
 /*   270 */   199,  198,   15,  197,  196,  193,    7,   31,   20,  142,
 /*   280 */   130,  127,  210,  209,  216,   67,   17,   23,  174,  175,
 /*   290 */   236,  235,  232,  231,  229,  227,  226,  225,  224,  222,
 /*   300 */   219,  218,  126,  215,   25,  214,   65,    8,  212,   84,
 /*   310 */   200,  199,  198,    9,  197,  196,  193,  180,  162,  165,
 /*   320 */   142,  130,  127,   30,  110,   12,  216,   11,    1,  230,
 /*   330 */     6,  240,  236,  235,  232,  231,  229,  227,  226,  225,
 /*   340 */   224,  222,  219,  218,  126,  215,   22,  214,   65,   58,
 /*   350 */   212,   84,  200,  199,  198,  177,  197,  196,  193,   16,
 /*   360 */   216,   28,  142,  130,  127,  194,  236,  235,  232,  231,
 /*   370 */   229,  227,  226,  225,  224,  222,  219,  218,  126,  215,
 /*   380 */    57,  214,   65,  195,  212,   84,  200,  199,  198,    2,
 /*   390 */   197,  196,  193,   33,  158,  166,  142,  130,  127,   63,
 /*   400 */   216,  244,  245,  246,  112,   95,  236,  235,  232,  231,
 /*   410 */   229,  227,  226,  225,  224,  222,  219,  218,  126,  215,
 /*   420 */   105,  214,   65,   70,  212,   84,  200,  199,  198,    4,
 /*   430 */   197,  196,  193,  108,  216,  398,  142,  130,  127,  220,
 /*   440 */   236,  235,  232,  231,  229,  227,  226,  225,  224,  222,
 /*   450 */   219,  218,  126,  215,  398,  214,   65,  398,  212,   84,
 /*   460 */   200,  199,  198,  398,  197,  196,  193,  398,  216,  398,
 /*   470 */   142,  130,  127,  189,  236,  235,  232,  231,  229,  227,
 /*   480 */   226,  225,  224,  222,  219,  218,  126,  215,  398,  214,
 /*   490 */    65,  398,  212,   84,  200,  199,  198,  398,  197,  196,
 /*   500 */   193,  398,  216,  398,  142,  130,  127,  228,  236,  235,
 /*   510 */   232,  231,  229,  227,  226,  225,  224,  222,  219,  218,
 /*   520 */   126,  215,  398,  214,   65,  398,  212,   84,  200,  199,
 /*   530 */   198,  398,  197,  196,  193,  398,  216,  398,  142,  130,
 /*   540 */   127,  239,  236,  235,  232,  231,  229,  227,  226,  225,
 /*   550 */   224,  222,  219,  218,  126,  215,  398,  214,   65,  398,
 /*   560 */   212,   84,  200,  199,  198,  398,  197,  196,  193,  398,
 /*   570 */   216,  398,  142,  130,  127,  182,  236,  235,  232,  231,
 /*   580 */   229,  227,  226,  225,  224,  222,  219,  218,  126,  215,
 /*   590 */   398,  214,   65,  398,  212,   84,  200,  199,  198,  398,
 /*   600 */   197,  196,  193,  398,  398,  398,  142,  130,  127,   51,
 /*   610 */    36,   44,   39,   49,   35,   37,   46,   43,   42,   41,
 /*   620 */    50,   40,   52,   47,   48,   45,   38,  164,  161,  159,
 /*   630 */   157,  156,  155,   24,   56,   46,   43,   42,   41,   50,
 /*   640 */    40,   52,   47,   48,   45,   38,   55,   54,   53,  398,
 /*   650 */   103,   18,  398,   27,  398,  192,  191,  190,  188,  187,
 /*   660 */   186,  185,  184,  183,  181,  398,   97,  147,  102,   56,
 /*   670 */   234,  238,  125,  122,   64,   52,   47,   48,   45,   38,
 /*   680 */   398,   55,   54,   53,  398,  103,   18,  398,   27,  176,
 /*   690 */   192,  191,  190,  188,  187,  186,  185,  184,  183,  181,
 /*   700 */   398,   97,   56,   75,  398,  212,   84,  200,  199,  198,
 /*   710 */   398,  197,  196,  193,   55,   54,   53,  398,  103,   18,
 /*   720 */   398,   27,  398,  192,  191,  190,  188,  187,  186,  185,
 /*   730 */   184,  183,  181,  398,   97,  137,   81,   56,  212,   84,
 /*   740 */   200,  199,  198,  398,  197,  196,  193,  398,  398,   55,
 /*   750 */    54,   53,  398,  103,   18,  398,   27,  398,  192,  191,
 /*   760 */   190,  188,  187,  186,  185,  184,  183,  181,  398,   97,
 /*   770 */    36,   44,   39,   49,   35,   37,   46,   43,   42,   41,
 /*   780 */    50,   40,   52,   47,   48,   45,   38,   44,   39,   49,
 /*   790 */    35,   37,   46,   43,   42,   41,   50,   40,   52,   47,
 /*   800 */    48,   45,   38,  221,   42,   41,   50,   40,   52,   47,
 /*   810 */    48,   45,   38,  201,  126,  215,  398,  214,   65,  398,
 /*   820 */   212,   84,  200,  199,  198,  398,  197,   89,  193,  398,
 /*   830 */   398,   66,  111,   85,  119,  212,   84,  200,  199,  198,
 /*   840 */   398,  197,  196,  193,  398,  398,   26,  121,  398,  398,
 /*   850 */   398,   39,   49,   35,   37,   46,   43,   42,   41,   50,
 /*   860 */    40,   52,   47,   48,   45,   38,  172,  215,  398,  214,
 /*   870 */    65,  398,  212,   84,  200,  199,  198,   87,  197,  196,
 /*   880 */   193,  178,  215,  170,  214,   65,  398,  212,   84,  200,
 /*   890 */   199,  198,  398,  197,  196,  193,   92,   90,  237,  238,
 /*   900 */   398,  128,  398,  178,  215,  398,  214,   65,  398,  212,
 /*   910 */    84,  200,  199,  198,  398,  197,  196,  193,  179,   90,
 /*   920 */   398,   49,   35,   37,   46,   43,   42,   41,   50,   40,
 /*   930 */    52,   47,   48,   45,   38,  398,  217,  215,  118,  214,
 /*   940 */    65,  398,  212,   84,  200,  199,  198,  398,  197,  196,
 /*   950 */   193,   35,   37,   46,   43,   42,   41,   50,   40,   52,
 /*   960 */    47,   48,   45,   38,  172,  215,  398,  214,   65,  398,
 /*   970 */   212,   84,  200,  199,  198,  398,  197,  196,  193,   94,
 /*   980 */   215,  171,  214,   65,  398,  212,   84,  200,  199,  198,
 /*   990 */   398,  197,  196,  193,  398,   91,  215,  398,  214,   65,
 /*  1000 */   398,  212,   84,  200,  199,  198,  398,  197,  196,  193,
 /*  1010 */   233,  215,  398,  214,   65,  398,  212,   84,  200,  199,
 /*  1020 */   198,  398,  197,  196,  193,  123,  215,  398,  214,   65,
 /*  1030 */   398,  212,   84,  200,  199,  198,  398,  197,  196,  193,
 /*  1040 */   115,  215,  398,  214,   65,  398,  212,   84,  200,  199,
 /*  1050 */   198,  398,  197,  196,  193,  398,  100,  215,  398,  214,
 /*  1060 */    65,  398,  212,   84,  200,  199,  198,  398,  197,  196,
 /*  1070 */   193,   99,  215,  398,  214,   65,  398,  212,   84,  200,
 /*  1080 */   199,  198,  398,  197,  196,  193,  398,  114,  215,  398,
 /*  1090 */   214,   65,  398,  212,   84,  200,  199,  198,  398,  197,
 /*  1100 */   196,  193,  117,  215,  398,  214,   65,  398,  212,   84,
 /*  1110 */   200,  199,  198,  398,  197,  196,  193,  101,  215,  398,
 /*  1120 */   214,   65,  398,  212,   84,  200,  199,  198,  398,  197,
 /*  1130 */   196,  193,  152,  215,  398,  214,   65,  398,  212,   84,
 /*  1140 */   200,  199,  198,  398,  197,  196,  193,  398,  135,  215,
 /*  1150 */   398,  214,   65,  398,  212,   84,  200,  199,  198,  398,
 /*  1160 */   197,  196,  193,  398,  151,  398,  214,   65,  398,  212,
 /*  1170 */    84,  200,  199,  198,  398,  197,  196,  193,  213,  398,
 /*  1180 */   214,   65,  398,  212,   84,  200,  199,  198,  398,  197,
 /*  1190 */   196,  193,   71,  398,  212,   84,  200,  199,  198,  398,
 /*  1200 */   197,  196,  193,   79,  398,  212,   84,  200,  199,  198,
 /*  1210 */   398,  197,  196,  193,   86,  398,  212,   84,  200,  199,
 /*  1220 */   198,  398,  197,  196,  193,  149,  398,  212,   84,  200,
 /*  1230 */   199,  198,  398,  197,  196,  193,  398,   76,  398,  212,
 /*  1240 */    84,  200,  199,  198,  398,  197,  196,  193,   73,  398,
 /*  1250 */   212,   84,  200,  199,  198,  398,  197,  196,  193,   72,
 /*  1260 */   398,  212,   84,  200,  199,  198,  398,  197,  196,  193,
 /*  1270 */    78,  398,  212,   84,  200,  199,  198,  398,  197,  196,
 /*  1280 */   193,  398,   80,  398,  212,   84,  200,  199,  198,  398,
 /*  1290 */   197,  196,  193,   83,  398,  212,   84,  200,  199,  198,
 /*  1300 */   398,  197,  196,  193,   77,  398,  212,   84,  200,  199,
 /*  1310 */   198,  398,  197,  196,  193,   82,  398,  212,   84,  200,
 /*  1320 */   199,  198,  398,  197,  196,  193,   74,  398,  212,   84,
 /*  1330 */   200,  199,  198,  398,  197,  196,  193,  398,  148,  398,
 /*  1340 */   212,   84,  200,  199,  198,  398,  197,  196,  193,  398,
 /*  1350 */   191,  190,  188,  187,  186,  185,  184,  183,  181,  398,
 /*  1360 */   150,  398,  212,   84,  200,  199,  198,  398,  197,  196,
 /*  1370 */   193,  204,   84,  200,  199,  198,  398,  197,  196,  193,
 /*  1380 */   206,   84,  200,  199,  198,  398,  197,  196,  193,  398,
 /*  1390 */   207,   84,  200,  199,  198,  398,  197,  196,  193,  205,
 /*  1400 */    84,  200,  199,  198,  398,  197,  196,  193,
};
static const YYCODETYPE yy_lookahead[] = {
 /*     0 */    70,   71,   72,   73,   74,   75,   76,   77,   78,   79,
 /*    10 */    80,   81,   82,   83,   84,   85,   86,   87,   88,   89,
 /*    20 */    48,   91,   92,   35,   94,   95,   96,   97,   98,   15,
 /*    30 */   100,  101,  102,  106,   46,  108,  106,  107,  108,  112,
 /*    40 */   113,   27,   28,   29,   26,   31,   32,  106,   34,  108,
 /*    50 */    36,   37,   38,   39,   40,   41,   42,   43,   44,   45,
 /*    60 */    48,   47,   48,   49,   50,   51,   52,   53,   54,   55,
 /*    70 */    56,   15,   48,   55,   60,   61,  109,  110,   60,   65,
 /*    80 */    66,   67,   31,   27,   28,   29,   35,   31,   32,   42,
 /*    90 */    34,  122,   36,   37,   38,   39,   40,   41,   42,   43,
 /*   100 */    44,   45,  117,   47,   48,   49,   50,   51,   52,   53,
 /*   110 */    54,   55,   56,  106,   19,  108,   60,   61,  111,  112,
 /*   120 */   113,   65,   66,   67,   70,   16,   17,   18,   74,   75,
 /*   130 */    76,   77,   78,   79,   80,   81,   82,   83,   84,   85,
 /*   140 */    86,   87,   88,   89,   48,   91,   92,   48,   94,   95,
 /*   150 */    96,   97,   98,   19,  100,  101,  102,   62,   50,   15,
 /*   160 */   106,  107,  108,   55,   31,   57,   58,   59,   60,  123,
 /*   170 */   124,   27,   28,   29,   46,   31,   32,  110,   34,  125,
 /*   180 */    36,   37,   38,   39,   40,   41,   42,   43,   44,   45,
 /*   190 */    48,   47,   48,   49,   31,   51,   52,   53,   54,   55,
 /*   200 */    56,   48,   98,   46,   60,   61,   31,  100,   70,   65,
 /*   210 */    66,   67,   74,   75,   76,   77,   78,   79,   80,   81,
 /*   220 */    82,   83,   84,   85,   86,   87,   88,   89,   35,   91,
 /*   230 */    92,   31,   94,   95,   96,   97,   98,   46,  100,  101,
 /*   240 */   102,   93,   62,   70,  106,  107,  108,   31,   75,   76,
 /*   250 */    77,   78,   79,   80,   81,   82,   83,   84,   85,   86,
 /*   260 */    87,   88,   89,  125,   91,   92,   48,   94,   95,   96,
 /*   270 */    97,   98,   34,  100,  101,  102,   35,   62,   48,  106,
 /*   280 */   107,  108,   27,   28,   70,   30,   46,   32,  115,   75,
 /*   290 */    76,   77,   78,   79,   80,   81,   82,   83,   84,   85,
 /*   300 */    86,   87,   88,   89,   19,   91,   92,   35,   94,   95,
 /*   310 */    96,   97,   98,   35,  100,  101,  102,   33,   48,   33,
 /*   320 */   106,  107,  108,   34,   31,   64,   70,   35,   26,  115,
 /*   330 */    74,   75,   76,   77,   78,   79,   80,   81,   82,   83,
 /*   340 */    84,   85,   86,   87,   88,   89,   34,   91,   92,   34,
 /*   350 */    94,   95,   96,   97,   98,   31,  100,  101,  102,   34,
 /*   360 */    70,   34,  106,  107,  108,   75,   76,   77,   78,   79,
 /*   370 */    80,   81,   82,   83,   84,   85,   86,   87,   88,   89,
 /*   380 */    49,   91,   92,   35,   94,   95,   96,   97,   98,   26,
 /*   390 */   100,  101,  102,   26,   50,   48,  106,  107,  108,   55,
 /*   400 */    70,   57,   58,   59,   60,   75,   76,   77,   78,   79,
 /*   410 */    80,   81,   82,   83,   84,   85,   86,   87,   88,   89,
 /*   420 */    35,   91,   92,   49,   94,   95,   96,   97,   98,   35,
 /*   430 */   100,  101,  102,   31,   70,  126,  106,  107,  108,   75,
 /*   440 */    76,   77,   78,   79,   80,   81,   82,   83,   84,   85,
 /*   450 */    86,   87,   88,   89,  126,   91,   92,  126,   94,   95,
 /*   460 */    96,   97,   98,  126,  100,  101,  102,  126,   70,  126,
 /*   470 */   106,  107,  108,   75,   76,   77,   78,   79,   80,   81,
 /*   480 */    82,   83,   84,   85,   86,   87,   88,   89,  126,   91,
 /*   490 */    92,  126,   94,   95,   96,   97,   98,  126,  100,  101,
 /*   500 */   102,  126,   70,  126,  106,  107,  108,   75,   76,   77,
 /*   510 */    78,   79,   80,   81,   82,   83,   84,   85,   86,   87,
 /*   520 */    88,   89,  126,   91,   92,  126,   94,   95,   96,   97,
 /*   530 */    98,  126,  100,  101,  102,  126,   70,  126,  106,  107,
 /*   540 */   108,   75,   76,   77,   78,   79,   80,   81,   82,   83,
 /*   550 */    84,   85,   86,   87,   88,   89,  126,   91,   92,  126,
 /*   560 */    94,   95,   96,   97,   98,  126,  100,  101,  102,  126,
 /*   570 */    70,  126,  106,  107,  108,   75,   76,   77,   78,   79,
 /*   580 */    80,   81,   82,   83,   84,   85,   86,   87,   88,   89,
 /*   590 */   126,   91,   92,  126,   94,   95,   96,   97,   98,  126,
 /*   600 */   100,  101,  102,  126,  126,  126,  106,  107,  108,    1,
 /*   610 */     2,    3,    4,    5,    6,    7,    8,    9,   10,   11,
 /*   620 */    12,   13,   14,   15,   16,   17,   18,   19,   20,   21,
 /*   630 */    22,   23,   24,   25,   15,    8,    9,   10,   11,   12,
 /*   640 */    13,   14,   15,   16,   17,   18,   27,   28,   29,  126,
 /*   650 */    31,   32,  126,   34,  126,   36,   37,   38,   39,   40,
 /*   660 */    41,   42,   43,   44,   45,  126,   47,   48,  114,   15,
 /*   670 */   116,  117,  118,  119,   55,   14,   15,   16,   17,   18,
 /*   680 */   126,   27,   28,   29,  126,   31,   32,  126,   34,   35,
 /*   690 */    36,   37,   38,   39,   40,   41,   42,   43,   44,   45,
 /*   700 */   126,   47,   15,   92,  126,   94,   95,   96,   97,   98,
 /*   710 */   126,  100,  101,  102,   27,   28,   29,  126,   31,   32,
 /*   720 */   126,   34,  126,   36,   37,   38,   39,   40,   41,   42,
 /*   730 */    43,   44,   45,  126,   47,   48,   92,   15,   94,   95,
 /*   740 */    96,   97,   98,  126,  100,  101,  102,  126,  126,   27,
 /*   750 */    28,   29,  126,   31,   32,  126,   34,  126,   36,   37,
 /*   760 */    38,   39,   40,   41,   42,   43,   44,   45,  126,   47,
 /*   770 */     2,    3,    4,    5,    6,    7,    8,    9,   10,   11,
 /*   780 */    12,   13,   14,   15,   16,   17,   18,    3,    4,    5,
 /*   790 */     6,    7,    8,    9,   10,   11,   12,   13,   14,   15,
 /*   800 */    16,   17,   18,   77,   10,   11,   12,   13,   14,   15,
 /*   810 */    16,   17,   18,   50,   88,   89,  126,   91,   92,  126,
 /*   820 */    94,   95,   96,   97,   98,  126,  100,  101,  102,  126,
 /*   830 */   126,   68,   69,   92,  108,   94,   95,   96,   97,   98,
 /*   840 */   126,  100,  101,  102,  126,  126,  120,  121,  126,  126,
 /*   850 */   126,    4,    5,    6,    7,    8,    9,   10,   11,   12,
 /*   860 */    13,   14,   15,   16,   17,   18,   88,   89,  126,   91,
 /*   870 */    92,  126,   94,   95,   96,   97,   98,   99,  100,  101,
 /*   880 */   102,   88,   89,  105,   91,   92,  126,   94,   95,   96,
 /*   890 */    97,   98,  126,  100,  101,  102,  103,  104,  116,  117,
 /*   900 */   126,  119,  126,   88,   89,  126,   91,   92,  126,   94,
 /*   910 */    95,   96,   97,   98,  126,  100,  101,  102,  103,  104,
 /*   920 */   126,    5,    6,    7,    8,    9,   10,   11,   12,   13,
 /*   930 */    14,   15,   16,   17,   18,  126,   88,   89,   90,   91,
 /*   940 */    92,  126,   94,   95,   96,   97,   98,  126,  100,  101,
 /*   950 */   102,    6,    7,    8,    9,   10,   11,   12,   13,   14,
 /*   960 */    15,   16,   17,   18,   88,   89,  126,   91,   92,  126,
 /*   970 */    94,   95,   96,   97,   98,  126,  100,  101,  102,   88,
 /*   980 */    89,  105,   91,   92,  126,   94,   95,   96,   97,   98,
 /*   990 */   126,  100,  101,  102,  126,   88,   89,  126,   91,   92,
 /*  1000 */   126,   94,   95,   96,   97,   98,  126,  100,  101,  102,
 /*  1010 */    88,   89,  126,   91,   92,  126,   94,   95,   96,   97,
 /*  1020 */    98,  126,  100,  101,  102,   88,   89,  126,   91,   92,
 /*  1030 */   126,   94,   95,   96,   97,   98,  126,  100,  101,  102,
 /*  1040 */    88,   89,  126,   91,   92,  126,   94,   95,   96,   97,
 /*  1050 */    98,  126,  100,  101,  102,  126,   88,   89,  126,   91,
 /*  1060 */    92,  126,   94,   95,   96,   97,   98,  126,  100,  101,
 /*  1070 */   102,   88,   89,  126,   91,   92,  126,   94,   95,   96,
 /*  1080 */    97,   98,  126,  100,  101,  102,  126,   88,   89,  126,
 /*  1090 */    91,   92,  126,   94,   95,   96,   97,   98,  126,  100,
 /*  1100 */   101,  102,   88,   89,  126,   91,   92,  126,   94,   95,
 /*  1110 */    96,   97,   98,  126,  100,  101,  102,   88,   89,  126,
 /*  1120 */    91,   92,  126,   94,   95,   96,   97,   98,  126,  100,
 /*  1130 */   101,  102,   88,   89,  126,   91,   92,  126,   94,   95,
 /*  1140 */    96,   97,   98,  126,  100,  101,  102,  126,   88,   89,
 /*  1150 */   126,   91,   92,  126,   94,   95,   96,   97,   98,  126,
 /*  1160 */   100,  101,  102,  126,   89,  126,   91,   92,  126,   94,
 /*  1170 */    95,   96,   97,   98,  126,  100,  101,  102,   89,  126,
 /*  1180 */    91,   92,  126,   94,   95,   96,   97,   98,  126,  100,
 /*  1190 */   101,  102,   92,  126,   94,   95,   96,   97,   98,  126,
 /*  1200 */   100,  101,  102,   92,  126,   94,   95,   96,   97,   98,
 /*  1210 */   126,  100,  101,  102,   92,  126,   94,   95,   96,   97,
 /*  1220 */    98,  126,  100,  101,  102,   92,  126,   94,   95,   96,
 /*  1230 */    97,   98,  126,  100,  101,  102,  126,   92,  126,   94,
 /*  1240 */    95,   96,   97,   98,  126,  100,  101,  102,   92,  126,
 /*  1250 */    94,   95,   96,   97,   98,  126,  100,  101,  102,   92,
 /*  1260 */   126,   94,   95,   96,   97,   98,  126,  100,  101,  102,
 /*  1270 */    92,  126,   94,   95,   96,   97,   98,  126,  100,  101,
 /*  1280 */   102,  126,   92,  126,   94,   95,   96,   97,   98,  126,
 /*  1290 */   100,  101,  102,   92,  126,   94,   95,   96,   97,   98,
 /*  1300 */   126,  100,  101,  102,   92,  126,   94,   95,   96,   97,
 /*  1310 */    98,  126,  100,  101,  102,   92,  126,   94,   95,   96,
 /*  1320 */    97,   98,  126,  100,  101,  102,   92,  126,   94,   95,
 /*  1330 */    96,   97,   98,  126,  100,  101,  102,  126,   92,  126,
 /*  1340 */    94,   95,   96,   97,   98,  126,  100,  101,  102,  126,
 /*  1350 */    37,   38,   39,   40,   41,   42,   43,   44,   45,  126,
 /*  1360 */    92,  126,   94,   95,   96,   97,   98,  126,  100,  101,
 /*  1370 */   102,   94,   95,   96,   97,   98,  126,  100,  101,  102,
 /*  1380 */    94,   95,   96,   97,   98,  126,  100,  101,  102,  126,
 /*  1390 */    94,   95,   96,   97,   98,  126,  100,  101,  102,   94,
 /*  1400 */    95,   96,   97,   98,  126,  100,  101,  102,
};
#define YY_SHIFT_USE_DFLT (-29)
#define YY_SHIFT_MAX 136
static const short yy_shift_ofst[] = {
 /*     0 */   144,  144,  144,   14,  144,  144,   56,  144,  144,  144,
 /*    10 */   144,  144,  144,  144,  144,  619,  654,  722,  722,  722,
 /*    20 */   722,  687,  722,  722,  722,  722,  722,  722,  722,  722,
 /*    30 */   722,  722,  722,  722,  722,  722,  722,  722,  722,  722,
 /*    40 */   722,  722,  722,  722,  722,  722,  722,  722,  722,  722,
 /*    50 */   722,  722,  722,  722,  722,  722,  722,  344,   51,  108,
 /*    60 */   216,  763,   18,  163,  200,  608, 1313,  175,  163,  133,
 /*    70 */   -29,  768,  784,  847,  916,  945,  627,  627,  794,  794,
 /*    80 */   661,  661,  661,  661,  255,  109,  109,  -12,   95,  215,
 /*    90 */   240,  272,  284,  289,  292,  261,  312,  324,  327,  348,
 /*   100 */   367,  385,  394,  325,  402,  374,  347,  363,  331,  325,
 /*   110 */   315,  302,  293,  270,  286,  278,  285,  230,  241,  218,
 /*   120 */   191,  180,  128,  193,  134,  157,  153,  142,  128,  134,
 /*   130 */    99,   47,   24,  -28,  238,   12,   96,
};
#define YY_REDUCE_USE_DFLT (-74)
#define YY_REDUCE_MAX 70
static const short yy_reduce_ofst[] = {
 /*     0 */   -70,   54,  138,  256,  173,  214,  466,  364,  500,  290,
 /*    10 */   432,  330,  398,  466,  466,  726,  778,  815,  793,  876,
 /*    20 */   848, 1060,  952,  999,  968, 1044, 1014,  983, 1029,  922,
 /*    30 */   891,  907,  937, 1075, 1089, 1212, 1167, 1145, 1246, 1234,
 /*    40 */  1223, 1201, 1190, 1178, 1156, 1133, 1111,  741, 1268,  611,
 /*    50 */   644, 1100, 1122, 1277, 1305, 1286, 1296,    7,  554,  -73,
 /*    60 */   782,   46,  -59,  -33,  -33,  148,  107,  104,   67,  -15,
 /*    70 */   -31,
};
static const YYACTIONTYPE yy_default[] = {
 /*     0 */   254,  396,  396,  396,  396,  396,  396,  396,  396,  396,
 /*    10 */   396,  396,  396,  255,  395,  396,  396,  396,  396,  396,
 /*    20 */   270,  396,  396,  396,  396,  396,  396,  396,  396,  396,
 /*    30 */   396,  396,  396,  396,  396,  396,  396,  396,  396,  396,
 /*    40 */   396,  396,  396,  396,  396,  396,  396,  396,  396,  396,
 /*    50 */   396,  396,  396,  396,  396,  396,  396,  396,  396,  396,
 /*    60 */   396,  396,  396,  396,  396,  280,  396,  396,  396,  396,
 /*    70 */   390,  283,  284,  285,  286,  287,  300,  299,  289,  288,
 /*    80 */   290,  292,  293,  291,  301,  295,  294,  396,  352,  317,
 /*    90 */   332,  396,  396,  396,  396,  386,  396,  396,  396,  396,
 /*   100 */   396,  396,  396,  330,  396,  396,  396,  396,  396,  311,
 /*   110 */   396,  396,  396,  396,  396,  396,  352,  396,  396,  396,
 /*   120 */   351,  396,  377,  396,  370,  376,  396,  396,  378,  396,
 /*   130 */   396,  396,  396,  396,  396,  396,  396,  345,  344,  346,
 /*   140 */   343,  347,  348,  342,  349,  340,  350,  339,  298,  297,
 /*   150 */   296,  281,  353,  355,  354,  279,  278,  277,  356,  276,
 /*   160 */   357,  275,  358,  359,  274,  313,  360,  361,  362,  312,
 /*   170 */   337,  338,  336,  315,  368,  379,  314,  335,  334,  333,
 /*   180 */   331,  329,  383,  328,  327,  326,  325,  324,  323,  387,
 /*   190 */   322,  321,  320,  319,  388,  318,  317,  316,  310,  307,
 /*   200 */   306,  389,  391,  392,  305,  304,  303,  302,  393,  309,
 /*   210 */   308,  394,  282,  273,  272,  269,  268,  271,  267,  266,
 /*   220 */   380,  381,  265,  382,  264,  263,  262,  261,  384,  260,
 /*   230 */   369,  259,  258,  371,  372,  257,  256,  373,  374,  253,
 /*   240 */   252,  251,  375,  250,  363,  364,  365,  367,  366,  341,
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
  "CASE",          "DEFAULT",       "error",         "main",        
  "translation_unit",  "statement_sequence_opt",  "statement_sequence",  "statement",   
  "include_statement",  "expression_statement",  "declaration_statement",  "for_statement",
  "compound_statement",  "if_statement",  "while_statement",  "foreach_statement",
  "return_statement",  "switch_statement",  "break_statement",  "continue_statement",
  "expression",    "assignment_expression",  "expression_opt",  "conditional_expression",
  "binary_expression",  "assignment_operator",  "unary_expression",  "postfix_expression",
  "new_expression",  "primary_expression",  "function_call",  "argument_list",
  "literal",       "id_expression",  "list_literal",  "list_content",
  "list_entry",    "argument",      "function_declaration",  "class_declaration",
  "variable_declaration",  "declarator_sequence",  "declarator",    "class_members",
  "class_member",  "access_specifier",  "parameter_list",  "function_body",
  "parameter",     "opt_parameter",  "parameters",    "opt_parameters",
  "for_init_statement",  "foreach_decl",  "switch_body",   "switch_case", 
  "default_case",  "case_statements",
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
 /*  49 */ "binary_expression ::= binary_expression SEQ binary_expression",
 /*  50 */ "binary_expression ::= binary_expression SNE binary_expression",
 /*  51 */ "unary_expression ::= postfix_expression",
 /*  52 */ "unary_expression ::= SUBOP unary_expression",
 /*  53 */ "unary_expression ::= ADDADD unary_expression",
 /*  54 */ "unary_expression ::= SUBSUB unary_expression",
 /*  55 */ "unary_expression ::= NOT unary_expression",
 /*  56 */ "unary_expression ::= new_expression",
 /*  57 */ "postfix_expression ::= primary_expression",
 /*  58 */ "postfix_expression ::= postfix_expression ADDADD",
 /*  59 */ "postfix_expression ::= postfix_expression SUBSUB",
 /*  60 */ "postfix_expression ::= function_call",
 /*  61 */ "postfix_expression ::= postfix_expression DOT IDENTIFIER",
 /*  62 */ "postfix_expression ::= postfix_expression DOT function_call",
 /*  63 */ "postfix_expression ::= postfix_expression LBRACKET expression RBRACKET",
 /*  64 */ "function_call ::= IDENTIFIER LPAREN RPAREN",
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
 /*  87 */ "argument_list ::= argument",
 /*  88 */ "argument_list ::= argument_list COMMA argument",
 /*  89 */ "expression_statement ::= SEMICOLON",
 /*  90 */ "expression_statement ::= expression SEMICOLON",
 /*  91 */ "compound_statement ::= LBRACE RBRACE",
 /*  92 */ "compound_statement ::= LBRACE statement_sequence RBRACE",
 /*  93 */ "include_statement ::= INCLUDE STRING SEMICOLON",
 /*  94 */ "return_statement ::= RETURN expression SEMICOLON",
 /*  95 */ "return_statement ::= RETURN SEMICOLON",
 /*  96 */ "break_statement ::= BREAK SEMICOLON",
 /*  97 */ "continue_statement ::= CONTINUE SEMICOLON",
 /*  98 */ "declaration_statement ::= function_declaration",
 /*  99 */ "declaration_statement ::= class_declaration SEMICOLON",
 /* 100 */ "declaration_statement ::= variable_declaration SEMICOLON",
 /* 101 */ "variable_declaration ::= VAR declarator_sequence",
 /* 102 */ "declarator ::= IDENTIFIER",
 /* 103 */ "declarator ::= IDENTIFIER ASSIGN expression",
 /* 104 */ "declarator_sequence ::= declarator",
 /* 105 */ "declarator_sequence ::= declarator_sequence COMMA declarator",
 /* 106 */ "class_declaration ::= CLASS IDENTIFIER LBRACE RBRACE",
 /* 107 */ "class_declaration ::= CLASS IDENTIFIER LBRACE class_members RBRACE",
 /* 108 */ "class_member ::= variable_declaration SEMICOLON",
 /* 109 */ "class_member ::= function_declaration",
 /* 110 */ "class_member ::= access_specifier variable_declaration SEMICOLON",
 /* 111 */ "class_member ::= access_specifier function_declaration",
 /* 112 */ "class_member ::= access_specifier COLON",
 /* 113 */ "access_specifier ::= PRIVATE",
 /* 114 */ "access_specifier ::= PROTECTED",
 /* 115 */ "access_specifier ::= PUBLIC",
 /* 116 */ "class_members ::= class_member",
 /* 117 */ "class_members ::= class_members class_member",
 /* 118 */ "function_declaration ::= FUNCTION IDENTIFIER LPAREN parameter_list RPAREN function_body",
 /* 119 */ "function_declaration ::= FUNCTION IDENTIFIER LPAREN RPAREN function_body",
 /* 120 */ "parameter ::= IDENTIFIER",
 /* 121 */ "opt_parameter ::= IDENTIFIER ASSIGN expression",
 /* 122 */ "parameters ::= parameter",
 /* 123 */ "parameters ::= parameters COMMA parameter",
 /* 124 */ "opt_parameters ::= opt_parameter",
 /* 125 */ "opt_parameters ::= opt_parameters COMMA opt_parameter",
 /* 126 */ "parameter_list ::= parameters",
 /* 127 */ "parameter_list ::= opt_parameters",
 /* 128 */ "parameter_list ::= parameters COMMA opt_parameters",
 /* 129 */ "function_body ::= statement",
 /* 130 */ "for_statement ::= FOR LPAREN for_init_statement expression SEMICOLON expression_opt RPAREN statement",
 /* 131 */ "for_init_statement ::= expression_statement",
 /* 132 */ "for_init_statement ::= variable_declaration SEMICOLON",
 /* 133 */ "foreach_statement ::= FOR LPAREN id_expression IN expression RPAREN statement",
 /* 134 */ "foreach_statement ::= FOR LPAREN foreach_decl IN expression RPAREN statement",
 /* 135 */ "foreach_decl ::= VAR IDENTIFIER",
 /* 136 */ "if_statement ::= IF LPAREN expression RPAREN statement",
 /* 137 */ "if_statement ::= IF LPAREN expression RPAREN statement ELSE statement",
 /* 138 */ "while_statement ::= WHILE LPAREN expression RPAREN statement",
 /* 139 */ "switch_statement ::= SWITCH LPAREN expression RPAREN LBRACE switch_body RBRACE",
 /* 140 */ "switch_body ::=",
 /* 141 */ "switch_body ::= switch_body switch_case",
 /* 142 */ "switch_body ::= switch_body default_case",
 /* 143 */ "switch_case ::= CASE literal COLON case_statements",
 /* 144 */ "default_case ::= DEFAULT COLON case_statements",
 /* 145 */ "case_statements ::= statement_sequence",
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
  { 71, 1 },
  { 72, 1 },
  { 74, 1 },
  { 74, 2 },
  { 73, 0 },
  { 73, 1 },
  { 75, 1 },
  { 75, 1 },
  { 75, 1 },
  { 75, 1 },
  { 75, 1 },
  { 75, 1 },
  { 75, 1 },
  { 75, 1 },
  { 75, 1 },
  { 75, 1 },
  { 75, 1 },
  { 75, 1 },
  { 75, 1 },
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
  { 77, 1 },
  { 77, 2 },
  { 80, 2 },
  { 80, 3 },
  { 76, 3 },
  { 84, 3 },
  { 84, 2 },
  { 86, 2 },
  { 87, 2 },
  { 78, 1 },
  { 78, 2 },
  { 78, 2 },
  { 108, 2 },
  { 110, 1 },
  { 110, 3 },
  { 109, 1 },
  { 109, 3 },
  { 107, 4 },
  { 107, 5 },
  { 112, 2 },
  { 112, 1 },
  { 112, 3 },
  { 112, 2 },
  { 112, 2 },
  { 113, 1 },
  { 113, 1 },
  { 113, 1 },
  { 111, 1 },
  { 111, 2 },
  { 106, 6 },
  { 106, 5 },
  { 116, 1 },
  { 117, 3 },
  { 118, 1 },
  { 118, 3 },
  { 119, 1 },
  { 119, 3 },
  { 114, 1 },
  { 114, 1 },
  { 114, 3 },
  { 115, 1 },
  { 79, 8 },
  { 120, 1 },
  { 120, 2 },
  { 83, 7 },
  { 83, 7 },
  { 121, 2 },
  { 81, 5 },
  { 81, 7 },
  { 82, 5 },
  { 85, 7 },
  { 122, 0 },
  { 122, 2 },
  { 122, 2 },
  { 123, 4 },
  { 124, 3 },
  { 125, 1 },
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
{ p->SetRoot(yymsp[0].minor.yy67); }
#line 1239 "cscript.c"
        break;
      case 1:
#line 78 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(translation_unit, yymsp[0].minor.yy67); }
#line 1244 "cscript.c"
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
      case 51:
      case 56:
      case 57:
      case 60:
      case 66:
      case 67:
      case 69:
      case 87:
      case 98:
      case 101:
      case 104:
      case 122:
      case 124:
      case 126:
      case 127:
      case 129:
      case 131:
      case 145:
#line 81 "cscript.in"
{ yygotominor.yy67 = yymsp[0].minor.yy67; }
#line 1285 "cscript.c"
        break;
      case 3:
#line 82 "cscript.in"
{ 
  if(yymsp[-1].minor.yy67->m_type == statement_sequence) {
    yygotominor.yy67 = yymsp[-1].minor.yy67;
  }
  else {
    yygotominor.yy67 = p->AllocAst(statement_sequence, new AstList);
    yygotominor.yy67->m_a1.GetList()->push_back(yymsp[-1].minor.yy67);
  }
  yygotominor.yy67->m_a1.GetList()->push_back(yymsp[0].minor.yy67);
}
#line 1299 "cscript.c"
        break;
      case 4:
      case 20:
#line 95 "cscript.in"
{ yygotominor.yy67 = 0; }
#line 1305 "cscript.c"
        break;
      case 18:
      case 89:
#line 113 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(empty_statement); }
#line 1311 "cscript.c"
        break;
      case 23:
#line 129 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(assignment_expression, yymsp[-1].minor.yy54, yymsp[-2].minor.yy67, yymsp[0].minor.yy67); }
#line 1316 "cscript.c"
        break;
      case 24:
#line 133 "cscript.in"
{ yygotominor.yy54 = op_assign; }
#line 1321 "cscript.c"
        break;
      case 25:
#line 134 "cscript.in"
{ yygotominor.yy54 = op_assadd; }
#line 1326 "cscript.c"
        break;
      case 26:
#line 135 "cscript.in"
{ yygotominor.yy54 = op_asssub; }
#line 1331 "cscript.c"
        break;
      case 27:
#line 136 "cscript.in"
{ yygotominor.yy54 = op_assmul; }
#line 1336 "cscript.c"
        break;
      case 28:
#line 137 "cscript.in"
{ yygotominor.yy54 = op_assdiv; }
#line 1341 "cscript.c"
        break;
      case 29:
#line 138 "cscript.in"
{ yygotominor.yy54 = op_assmod; }
#line 1346 "cscript.c"
        break;
      case 31:
#line 142 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(ternary_expression, yymsp[-4].minor.yy67, yymsp[-2].minor.yy67, yymsp[0].minor.yy67); }
#line 1351 "cscript.c"
        break;
      case 33:
#line 146 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(binary_expression, op_logor,   yymsp[-2].minor.yy67, yymsp[0].minor.yy67); }
#line 1356 "cscript.c"
        break;
      case 34:
#line 147 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(binary_expression, op_logand,  yymsp[-2].minor.yy67, yymsp[0].minor.yy67); }
#line 1361 "cscript.c"
        break;
      case 35:
#line 148 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(binary_expression, op_bitor,   yymsp[-2].minor.yy67, yymsp[0].minor.yy67); }
#line 1366 "cscript.c"
        break;
      case 36:
#line 149 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(binary_expression, op_bitxor,  yymsp[-2].minor.yy67, yymsp[0].minor.yy67); }
#line 1371 "cscript.c"
        break;
      case 37:
#line 150 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(binary_expression, op_bitand,  yymsp[-2].minor.yy67, yymsp[0].minor.yy67); }
#line 1376 "cscript.c"
        break;
      case 38:
#line 151 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(binary_expression, op_eq,   yymsp[-2].minor.yy67, yymsp[0].minor.yy67); }
#line 1381 "cscript.c"
        break;
      case 39:
#line 152 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(binary_expression, op_ne,   yymsp[-2].minor.yy67, yymsp[0].minor.yy67); }
#line 1386 "cscript.c"
        break;
      case 40:
#line 153 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(binary_expression, op_lt,   yymsp[-2].minor.yy67, yymsp[0].minor.yy67); }
#line 1391 "cscript.c"
        break;
      case 41:
#line 154 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(binary_expression, op_le,   yymsp[-2].minor.yy67, yymsp[0].minor.yy67); }
#line 1396 "cscript.c"
        break;
      case 42:
#line 155 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(binary_expression, op_gt,   yymsp[-2].minor.yy67, yymsp[0].minor.yy67); }
#line 1401 "cscript.c"
        break;
      case 43:
#line 156 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(binary_expression, op_ge,   yymsp[-2].minor.yy67, yymsp[0].minor.yy67); }
#line 1406 "cscript.c"
        break;
      case 44:
#line 157 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(binary_expression, op_add,  yymsp[-2].minor.yy67, yymsp[0].minor.yy67); }
#line 1411 "cscript.c"
        break;
      case 45:
#line 158 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(binary_expression, op_sub,  yymsp[-2].minor.yy67, yymsp[0].minor.yy67); }
#line 1416 "cscript.c"
        break;
      case 46:
#line 159 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(binary_expression, op_mul,  yymsp[-2].minor.yy67, yymsp[0].minor.yy67); }
#line 1421 "cscript.c"
        break;
      case 47:
#line 160 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(binary_expression, op_div,  yymsp[-2].minor.yy67, yymsp[0].minor.yy67); }
#line 1426 "cscript.c"
        break;
      case 48:
#line 161 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(binary_expression, op_mod,  yymsp[-2].minor.yy67, yymsp[0].minor.yy67); }
#line 1431 "cscript.c"
        break;
      case 49:
#line 162 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(binary_expression, op_seq,  yymsp[-2].minor.yy67, yymsp[0].minor.yy67); }
#line 1436 "cscript.c"
        break;
      case 50:
#line 163 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(binary_expression, op_sne,  yymsp[-2].minor.yy67, yymsp[0].minor.yy67); }
#line 1441 "cscript.c"
        break;
      case 52:
#line 167 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(prefix_expression, op_negate, yymsp[0].minor.yy67); }
#line 1446 "cscript.c"
        break;
      case 53:
#line 168 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(prefix_expression, op_preinc, yymsp[0].minor.yy67); }
#line 1451 "cscript.c"
        break;
      case 54:
#line 169 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(prefix_expression, op_predec, yymsp[0].minor.yy67); }
#line 1456 "cscript.c"
        break;
      case 55:
#line 170 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(prefix_expression, op_not,    yymsp[0].minor.yy67); }
#line 1461 "cscript.c"
        break;
      case 58:
#line 175 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(postfix_expression, op_postinc, yymsp[-1].minor.yy67); }
#line 1466 "cscript.c"
        break;
      case 59:
#line 176 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(postfix_expression, op_postdec, yymsp[-1].minor.yy67); }
#line 1471 "cscript.c"
        break;
      case 61:
#line 178 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(member_expression, yymsp[-2].minor.yy67, String(yymsp[0].minor.yy0)); }
#line 1476 "cscript.c"
        break;
      case 62:
#line 179 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(member_call, yymsp[-2].minor.yy67, yymsp[0].minor.yy67); }
#line 1481 "cscript.c"
        break;
      case 63:
#line 180 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(index_expression, yymsp[-3].minor.yy67, yymsp[-1].minor.yy67); }
#line 1486 "cscript.c"
        break;
      case 64:
#line 183 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(function_call, String(yymsp[-2].minor.yy0)); }
#line 1491 "cscript.c"
        break;
      case 65:
#line 184 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(function_call, String(yymsp[-3].minor.yy0), yymsp[-1].minor.yy67); }
#line 1496 "cscript.c"
        break;
      case 68:
      case 99:
      case 100:
      case 132:
#line 190 "cscript.in"
{ yygotominor.yy67 = yymsp[-1].minor.yy67; }
#line 1504 "cscript.c"
        break;
      case 70:
#line 192 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(this_expression); }
#line 1509 "cscript.c"
        break;
      case 71:
#line 195 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(literal, Variant(String(yymsp[0].minor.yy0), Variant::stInt)); }
#line 1514 "cscript.c"
        break;
      case 72:
#line 196 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(literal, Variant(hex2dec(String(yymsp[0].minor.yy0).c_str()))); }
#line 1519 "cscript.c"
        break;
      case 73:
#line 197 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(literal, Variant(bin2dec(String(yymsp[0].minor.yy0).c_str()))); }
#line 1524 "cscript.c"
        break;
      case 74:
#line 198 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(literal, Variant(rom2dec(String(yymsp[0].minor.yy0).c_str()))); }
#line 1529 "cscript.c"
        break;
      case 75:
#line 199 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(literal, Variant(String(yymsp[0].minor.yy0), Variant::stReal)); }
#line 1534 "cscript.c"
        break;
      case 76:
#line 200 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(literal, Variant(String(yymsp[0].minor.yy0), Variant::stString)); }
#line 1539 "cscript.c"
        break;
      case 77:
#line 201 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(literal, Variant(true));    }
#line 1544 "cscript.c"
        break;
      case 78:
#line 202 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(literal, Variant(false));   }
#line 1549 "cscript.c"
        break;
      case 79:
#line 203 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(literal, Variant());        }
#line 1554 "cscript.c"
        break;
      case 80:
#line 206 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(lvalue, String(yymsp[0].minor.yy0)); }
#line 1559 "cscript.c"
        break;
      case 81:
#line 209 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(list_literal, yymsp[-1].minor.yy67); }
#line 1564 "cscript.c"
        break;
      case 82:
#line 210 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(list_content, yymsp[0].minor.yy67); }
#line 1569 "cscript.c"
        break;
      case 83:
#line 211 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(list_content, yymsp[-2].minor.yy67, yymsp[0].minor.yy67); }
#line 1574 "cscript.c"
        break;
      case 84:
#line 213 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(list_entry, yymsp[0].minor.yy67); }
#line 1579 "cscript.c"
        break;
      case 85:
#line 216 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(new_expression, String(yymsp[0].minor.yy0)); }
#line 1584 "cscript.c"
        break;
      case 86:
#line 225 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(argument, yymsp[0].minor.yy67); }
#line 1589 "cscript.c"
        break;
      case 88:
#line 229 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(argument_list, yymsp[-2].minor.yy67, yymsp[0].minor.yy67); }
#line 1594 "cscript.c"
        break;
      case 90:
#line 238 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(expression_statement, yymsp[-1].minor.yy67); }
#line 1599 "cscript.c"
        break;
      case 91:
#line 241 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(compound_statement); }
#line 1604 "cscript.c"
        break;
      case 92:
#line 242 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(compound_statement, yymsp[-1].minor.yy67); }
#line 1609 "cscript.c"
        break;
      case 93:
#line 245 "cscript.in"
{ p->Parse(yymsp[-1].minor.yy0); yygotominor.yy67 = p->GetRoot(); }
#line 1614 "cscript.c"
        break;
      case 94:
#line 248 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(return_statement, yymsp[-1].minor.yy67); }
#line 1619 "cscript.c"
        break;
      case 95:
#line 249 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(return_statement);    }
#line 1624 "cscript.c"
        break;
      case 96:
#line 252 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(break_statement); }
#line 1629 "cscript.c"
        break;
      case 97:
#line 253 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(continue_statement); }
#line 1634 "cscript.c"
        break;
      case 102:
#line 267 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(variable_declaration, String(yymsp[0].minor.yy0));    }
#line 1639 "cscript.c"
        break;
      case 103:
#line 268 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(variable_declaration, String(yymsp[-2].minor.yy0), yymsp[0].minor.yy67); }
#line 1644 "cscript.c"
        break;
      case 105:
#line 271 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(declaration_sequence, yymsp[-2].minor.yy67, yymsp[0].minor.yy67); }
#line 1649 "cscript.c"
        break;
      case 106:
#line 278 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(class_declaration, String(yymsp[-2].minor.yy0)); }
#line 1654 "cscript.c"
        break;
      case 107:
#line 279 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(class_declaration, String(yymsp[-3].minor.yy0), yymsp[-1].minor.yy127); }
#line 1659 "cscript.c"
        break;
      case 108:
#line 282 "cscript.in"
{ yygotominor.yy67 = yymsp[-1].minor.yy67; yymsp[-1].minor.yy67->m_props["access"] = accessDefault; }
#line 1664 "cscript.c"
        break;
      case 109:
#line 283 "cscript.in"
{ yygotominor.yy67 = yymsp[0].minor.yy67; yymsp[0].minor.yy67->m_props["access"] = accessDefault; }
#line 1669 "cscript.c"
        break;
      case 110:
#line 284 "cscript.in"
{ yygotominor.yy67 = yymsp[-1].minor.yy67; yymsp[-1].minor.yy67->m_props["access"] = yymsp[-2].minor.yy96; }
#line 1674 "cscript.c"
        break;
      case 111:
#line 285 "cscript.in"
{ yygotominor.yy67 = yymsp[0].minor.yy67; yymsp[0].minor.yy67->m_props["access"] = yymsp[-1].minor.yy96; }
#line 1679 "cscript.c"
        break;
      case 112:
#line 286 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(access_specifier, yymsp[-1].minor.yy96); }
#line 1684 "cscript.c"
        break;
      case 113:
#line 290 "cscript.in"
{ yygotominor.yy96 = accessPrivate;   }
#line 1689 "cscript.c"
        break;
      case 114:
#line 291 "cscript.in"
{ yygotominor.yy96 = accessProtected; }
#line 1694 "cscript.c"
        break;
      case 115:
#line 292 "cscript.in"
{ yygotominor.yy96 = accessPublic;    }
#line 1699 "cscript.c"
        break;
      case 116:
#line 296 "cscript.in"
{ 
  yygotominor.yy127 = new AstList;
  yygotominor.yy127->push_back(yymsp[0].minor.yy67);
}
#line 1707 "cscript.c"
        break;
      case 117:
#line 300 "cscript.in"
{ 
  yygotominor.yy127 = yymsp[-1].minor.yy127;
  yygotominor.yy127->push_back(yymsp[0].minor.yy67);
}
#line 1715 "cscript.c"
        break;
      case 118:
#line 311 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(function_declaration, String(yymsp[-4].minor.yy0), yymsp[-2].minor.yy67, yymsp[0].minor.yy67); }
#line 1720 "cscript.c"
        break;
      case 119:
#line 312 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(function_declaration, String(yymsp[-3].minor.yy0), AstData(), yymsp[0].minor.yy67); }
#line 1725 "cscript.c"
        break;
      case 120:
#line 315 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(parameter, String(yymsp[0].minor.yy0)); }
#line 1730 "cscript.c"
        break;
      case 121:
#line 318 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(parameter, String(yymsp[-2].minor.yy0), yymsp[0].minor.yy67); }
#line 1735 "cscript.c"
        break;
      case 123:
      case 125:
      case 128:
#line 322 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(parameter_list, yymsp[-2].minor.yy67, yymsp[0].minor.yy67); }
#line 1742 "cscript.c"
        break;
      case 130:
#line 343 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(for_statement, yymsp[-5].minor.yy67, yymsp[-4].minor.yy67, yymsp[-2].minor.yy67, yymsp[0].minor.yy67); }
#line 1747 "cscript.c"
        break;
      case 133:
      case 134:
#line 354 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(foreach_statement, yymsp[-4].minor.yy67, yymsp[-2].minor.yy67, yymsp[0].minor.yy67); }
#line 1753 "cscript.c"
        break;
      case 135:
#line 356 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(variable_declaration, String(yymsp[0].minor.yy0)); }
#line 1758 "cscript.c"
        break;
      case 136:
#line 367 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(if_statement, yymsp[-2].minor.yy67, yymsp[0].minor.yy67); }
#line 1763 "cscript.c"
        break;
      case 137:
#line 368 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(if_statement, yymsp[-4].minor.yy67, yymsp[-2].minor.yy67, yymsp[0].minor.yy67); }
#line 1768 "cscript.c"
        break;
      case 138:
#line 376 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(while_statement, yymsp[-2].minor.yy67,  yymsp[0].minor.yy67); }
#line 1773 "cscript.c"
        break;
      case 139:
#line 384 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(switch_statement, yymsp[-4].minor.yy67, yymsp[-1].minor.yy127); }
#line 1778 "cscript.c"
        break;
      case 140:
#line 388 "cscript.in"
{ yygotominor.yy127 = new AstList; }
#line 1783 "cscript.c"
        break;
      case 141:
      case 142:
#line 389 "cscript.in"
{ yygotominor.yy127 = yymsp[-1].minor.yy127; yygotominor.yy127->push_back(yymsp[0].minor.yy67); }
#line 1789 "cscript.c"
        break;
      case 143:
#line 393 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(switch_case, yymsp[-2].minor.yy67, yymsp[0].minor.yy67); }
#line 1794 "cscript.c"
        break;
      case 144:
#line 396 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(default_case, yymsp[0].minor.yy67); }
#line 1799 "cscript.c"
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
#line 1847 "cscript.c"
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
#line 1865 "cscript.c"
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

