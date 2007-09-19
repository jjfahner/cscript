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
#define YYNSTATE 247
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
 /*     0 */   214,  394,  239,  235,    7,  234,  232,  231,  229,  228,
 /*    10 */   226,  225,  224,  223,  221,  218,  217,  215,  125,  213,
 /*    20 */   114,  212,   64,  172,  209,   83,  198,  197,  196,   55,
 /*    30 */   195,  194,  191,  162,   19,  113,  141,  129,  126,  244,
 /*    40 */    60,   53,   54,   52,  139,   89,   16,  166,   23,  101,
 /*    50 */   190,  189,  187,  186,  185,  184,  183,  182,  180,  179,
 /*    60 */   140,  111,  146,    3,  246,  130,   20,  135,  132,   62,
 /*    70 */   105,   55,  119,  153,   97,  133,   41,   46,   47,  100,
 /*    80 */   124,   91,   14,   53,   54,   52,   61,   89,   16,  143,
 /*    90 */    23,  137,  190,  189,  187,  186,  185,  184,  183,  182,
 /*   100 */   180,  179,   24,  111,  146,    3,  142,  130,   20,  135,
 /*   110 */   132,   62,  105,   55,  201,  202,   97,  133,  236,   15,
 /*   120 */   128,  100,  124,   91,   30,   53,   54,   52,  145,   89,
 /*   130 */    16,  152,   23,  115,  190,  189,  187,  186,  185,  184,
 /*   140 */   183,  182,  180,  179,   31,  111,  146,    3,  144,  130,
 /*   150 */    20,  135,  132,   62,  105,  162,  168,  113,   97,  133,
 /*   160 */    58,  245,   60,  100,  124,   91,  214,  382,   57,  127,
 /*   170 */    11,  234,  232,  231,  229,  228,  226,  225,  224,  223,
 /*   180 */   221,  218,  217,  215,  125,  213,   68,  212,   64,  164,
 /*   190 */   209,   83,  198,  197,  196,  102,  195,  194,  191,   32,
 /*   200 */    67,  214,  141,  129,  126,   11,  234,  232,  231,  229,
 /*   210 */   228,  226,  225,  224,  223,  221,  218,  217,  215,  125,
 /*   220 */   213,  207,  212,   64,   86,  209,   83,  198,  197,  196,
 /*   230 */    12,  195,  194,  191,    6,   59,  214,  141,  129,  126,
 /*   240 */   131,  174,  232,  231,  229,  228,  226,  225,  224,  223,
 /*   250 */   221,  218,  217,  215,  125,  213,  210,  212,   64,   17,
 /*   260 */   209,   83,  198,  197,  196,   30,  195,  194,  191,  178,
 /*   270 */   161,   25,  141,  129,  126,    9,  237,  238,  214,  122,
 /*   280 */    13,  173,    5,  234,  232,  231,  229,  228,  226,  225,
 /*   290 */   224,  223,  221,  218,  217,  215,  125,  213,  175,  212,
 /*   300 */    64,  109,  209,   83,  198,  197,  196,    1,  195,  194,
 /*   310 */   191,   28,  214,   69,  141,  129,  126,  193,  232,  231,
 /*   320 */   229,  228,  226,  225,  224,  223,  221,  218,  217,  215,
 /*   330 */   125,  213,    4,  212,   64,  222,  209,   83,  198,  197,
 /*   340 */   196,   33,  195,  194,  191,  123,  157,   56,  141,  129,
 /*   350 */   126,   62,  214,  241,  242,  243,   97,  188,  232,  231,
 /*   360 */   229,  228,  226,  225,  224,  223,  221,  218,  217,  215,
 /*   370 */   125,  213,    8,  212,   64,  192,  209,   83,  198,  197,
 /*   380 */   196,   10,  195,  194,  191,  107,  214,   18,  141,  129,
 /*   390 */   126,  233,  232,  231,  229,  228,  226,  225,  224,  223,
 /*   400 */   221,  218,  217,  215,  125,  213,   22,  212,   64,   27,
 /*   410 */   209,   83,  198,  197,  196,  165,  195,  194,  191,    2,
 /*   420 */   214,  138,  141,  129,  126,  227,  232,  231,  229,  228,
 /*   430 */   226,  225,  224,  223,  221,  218,  217,  215,  125,  213,
 /*   440 */   395,  212,   64,  395,  209,   83,  198,  197,  196,  395,
 /*   450 */   195,  194,  191,  395,  214,  395,  141,  129,  126,   99,
 /*   460 */   232,  231,  229,  228,  226,  225,  224,  223,  221,  218,
 /*   470 */   217,  215,  125,  213,  395,  212,   64,  395,  209,   83,
 /*   480 */   198,  197,  196,  395,  195,  194,  191,  395,  214,  395,
 /*   490 */   141,  129,  126,  219,  232,  231,  229,  228,  226,  225,
 /*   500 */   224,  223,  221,  218,  217,  215,  125,  213,  395,  212,
 /*   510 */    64,  395,  209,   83,  198,  197,  196,  395,  195,  194,
 /*   520 */   191,  395,  214,  395,  141,  129,  126,  181,  232,  231,
 /*   530 */   229,  228,  226,  225,  224,  223,  221,  218,  217,  215,
 /*   540 */   125,  213,  395,  212,   64,  395,  209,   83,  198,  197,
 /*   550 */   196,  395,  195,  194,  191,  395,  395,  395,  141,  129,
 /*   560 */   126,   49,   35,   38,   51,   43,   39,   50,   45,   36,
 /*   570 */    44,   42,   34,   40,   37,   48,   41,   46,   47,  163,
 /*   580 */   160,  158,  156,  155,  154,   29,   55,   45,   36,   44,
 /*   590 */    42,   34,   40,   37,   48,   41,   46,   47,   53,   54,
 /*   600 */    52,  395,   89,   16,  200,   23,  395,  190,  189,  187,
 /*   610 */   186,  185,  184,  183,  182,  180,  179,  395,  111,  146,
 /*   620 */   159,   55,   65,   94,  395,   62,   63,  241,  242,  243,
 /*   630 */    97,  395,  395,   53,   54,   52,  395,   89,   16,  395,
 /*   640 */    23,  395,  190,  189,  187,  186,  185,  184,  183,  182,
 /*   650 */   180,  179,  395,  111,  136,   79,   55,  209,   83,  198,
 /*   660 */   197,  196,  395,  195,  194,  191,  395,  395,   53,   54,
 /*   670 */    52,  395,   89,   16,  395,   23,  395,  190,  189,  187,
 /*   680 */   186,  185,  184,  183,  182,  180,  179,  395,  111,   35,
 /*   690 */    38,   51,   43,   39,   50,   45,   36,   44,   42,   34,
 /*   700 */    40,   37,   48,   41,   46,   47,   38,   51,   43,   39,
 /*   710 */    50,   45,   36,   44,   42,   34,   40,   37,   48,   41,
 /*   720 */    46,   47,  220,   44,   42,   34,   40,   37,   48,   41,
 /*   730 */    46,   47,  395,  125,  213,  167,  212,   64,  395,  209,
 /*   740 */    83,  198,  197,  196,  395,  195,  103,  191,  395,  395,
 /*   750 */   395,  395,   70,  108,  209,   83,  198,  197,  196,  395,
 /*   760 */   195,  194,  191,  395,   62,   26,  110,  395,  395,   97,
 /*   770 */    51,   43,   39,   50,   45,   36,   44,   42,   34,   40,
 /*   780 */    37,   48,   41,   46,   47,  176,  213,  395,  212,   64,
 /*   790 */   395,  209,   83,  198,  197,  196,  395,  195,  194,  191,
 /*   800 */    90,   88,  171,  213,  395,  212,   64,  395,  209,   83,
 /*   810 */   198,  197,  196,   87,  195,  194,  191,  395,  395,  169,
 /*   820 */   395,  395,  176,  213,  395,  212,   64,  395,  209,   83,
 /*   830 */   198,  197,  196,  395,  195,  194,  191,  177,   88,  395,
 /*   840 */    43,   39,   50,   45,   36,   44,   42,   34,   40,   37,
 /*   850 */    48,   41,   46,   47,   39,   50,   45,   36,   44,   42,
 /*   860 */    34,   40,   37,   48,   41,   46,   47,  171,  213,  395,
 /*   870 */   212,   64,  395,  209,   83,  198,  197,  196,  395,  195,
 /*   880 */   194,  191,  395,  395,  170,  395,  216,  213,  106,  212,
 /*   890 */    64,  395,  209,   83,  198,  197,  196,  395,  195,  194,
 /*   900 */   191,  230,  213,  395,  212,   64,  395,  209,   83,  198,
 /*   910 */   197,  196,  395,  195,  194,  191,  395,  134,  213,  395,
 /*   920 */   212,   64,  395,  209,   83,  198,  197,  196,  395,  195,
 /*   930 */   194,  191,  117,  213,  395,  212,   64,  395,  209,   83,
 /*   940 */   198,  197,  196,  395,  195,  194,  191,  121,  213,  395,
 /*   950 */   212,   64,  395,  209,   83,  198,  197,  196,  395,  195,
 /*   960 */   194,  191,  151,  213,  395,  212,   64,  395,  209,   83,
 /*   970 */   198,  197,  196,  395,  195,  194,  191,  118,  213,  395,
 /*   980 */   212,   64,  395,  209,   83,  198,  197,  196,  395,  195,
 /*   990 */   194,  191,  395,  112,  213,  395,  212,   64,  395,  209,
 /*  1000 */    83,  198,  197,  196,  395,  195,  194,  191,  395,   98,
 /*  1010 */   213,  395,  212,   64,  395,  209,   83,  198,  197,  196,
 /*  1020 */   395,  195,  194,  191,   92,  213,  395,  212,   64,  395,
 /*  1030 */   209,   83,  198,  197,  196,  395,  195,  194,  191,   96,
 /*  1040 */   213,  395,  212,   64,  395,  209,   83,  198,  197,  196,
 /*  1050 */   395,  195,  194,  191,  104,  213,  395,  212,   64,  395,
 /*  1060 */   209,   83,  198,  197,  196,  395,  195,  194,  191,   93,
 /*  1070 */   213,  395,  212,   64,  395,  209,   83,  198,  197,  196,
 /*  1080 */   395,  195,  194,  191,  211,  395,  212,   64,  395,  209,
 /*  1090 */    83,  198,  197,  196,  395,  195,  194,  191,  395,  150,
 /*  1100 */   395,  212,   64,  395,  209,   83,  198,  197,  196,  395,
 /*  1110 */   195,  194,  191,  395,   74,  395,  209,   83,  198,  197,
 /*  1120 */   196,  395,  195,  194,  191,   75,  395,  209,   83,  198,
 /*  1130 */   197,  196,  395,  195,  194,  191,   82,  395,  209,   83,
 /*  1140 */   198,  197,  196,  395,  195,  194,  191,   85,  395,  209,
 /*  1150 */    83,  198,  197,  196,  395,  195,  194,  191,  395,  149,
 /*  1160 */   395,  209,   83,  198,  197,  196,  395,  195,  194,  191,
 /*  1170 */   148,  395,  209,   83,  198,  197,  196,  395,  195,  194,
 /*  1180 */   191,  395,   80,  395,  209,   83,  198,  197,  196,  395,
 /*  1190 */   195,  194,  191,  395,   78,  395,  209,   83,  198,  197,
 /*  1200 */   196,  395,  195,  194,  191,  147,  395,  209,   83,  198,
 /*  1210 */   197,  196,  395,  195,  194,  191,   73,  395,  209,   83,
 /*  1220 */   198,  197,  196,  395,  195,  194,  191,   76,  395,  209,
 /*  1230 */    83,  198,  197,  196,  395,  195,  194,  191,   81,  395,
 /*  1240 */   209,   83,  198,  197,  196,  395,  195,  194,  191,   72,
 /*  1250 */   395,  209,   83,  198,  197,  196,  395,  195,  194,  191,
 /*  1260 */    71,  395,  209,   83,  198,  197,  196,  395,  195,  194,
 /*  1270 */   191,   84,  395,  209,   83,  198,  197,  196,  395,  195,
 /*  1280 */   194,  191,  189,  187,  186,  185,  184,  183,  182,  180,
 /*  1290 */   179,   77,  395,  209,   83,  198,  197,  196,  395,  195,
 /*  1300 */   194,  191,  204,   83,  198,  197,  196,  395,  195,  194,
 /*  1310 */   191,  203,   83,  198,  197,  196,  395,  195,  194,  191,
 /*  1320 */   205,   83,  198,  197,  196,  395,  195,  194,  191,  395,
 /*  1330 */   199,   83,  198,  197,  196,  395,  195,  194,  191,   95,
 /*  1340 */   395,  240,  238,  116,  120,   37,   48,   41,   46,   47,
 /*  1350 */   208,  206,  395,   66,  395,   21,
};
static const YYCODETYPE yy_lookahead[] = {
 /*     0 */    70,   71,   72,   73,   74,   75,   76,   77,   78,   79,
 /*    10 */    80,   81,   82,   83,   84,   85,   86,   87,   88,   89,
 /*    20 */    31,   91,   92,   35,   94,   95,   96,   97,   98,   15,
 /*    30 */   100,  101,  102,  106,   46,  108,  106,  107,  108,  112,
 /*    40 */   113,   27,   28,   29,   48,   31,   32,  106,   34,  108,
 /*    50 */    36,   37,   38,   39,   40,   41,   42,   43,   44,   45,
 /*    60 */    48,   47,   48,   49,   50,   51,   52,   53,   54,   55,
 /*    70 */    56,   15,  109,  110,   60,   61,   16,   17,   18,   65,
 /*    80 */    66,   67,   34,   27,   28,   29,  122,   31,   32,   48,
 /*    90 */    34,   48,   36,   37,   38,   39,   40,   41,   42,   43,
 /*   100 */    44,   45,   19,   47,   48,   49,   50,   51,   52,   53,
 /*   110 */    54,   55,   56,   15,  123,  124,   60,   61,  117,   34,
 /*   120 */    31,   65,   66,   67,   19,   27,   28,   29,   48,   31,
 /*   130 */    32,  110,   34,   31,   36,   37,   38,   39,   40,   41,
 /*   140 */    42,   43,   44,   45,   34,   47,   48,   49,   48,   51,
 /*   150 */    52,   53,   54,   55,   56,  106,   98,  108,   60,   61,
 /*   160 */   111,  112,  113,   65,   66,   67,   70,   62,   34,   31,
 /*   170 */    74,   75,   76,   77,   78,   79,   80,   81,   82,   83,
 /*   180 */    84,   85,   86,   87,   88,   89,   46,   91,   92,   33,
 /*   190 */    94,   95,   96,   97,   98,  100,  100,  101,  102,   93,
 /*   200 */    46,   70,  106,  107,  108,   74,   75,   76,   77,   78,
 /*   210 */    79,   80,   81,   82,   83,   84,   85,   86,   87,   88,
 /*   220 */    89,  125,   91,   92,   31,   94,   95,   96,   97,   98,
 /*   230 */    35,  100,  101,  102,   35,   46,   70,  106,  107,  108,
 /*   240 */    42,   75,   76,   77,   78,   79,   80,   81,   82,   83,
 /*   250 */    84,   85,   86,   87,   88,   89,  125,   91,   92,   46,
 /*   260 */    94,   95,   96,   97,   98,   19,  100,  101,  102,   33,
 /*   270 */    48,   34,  106,  107,  108,   35,  116,  117,   70,  119,
 /*   280 */    35,  115,   74,   75,   76,   77,   78,   79,   80,   81,
 /*   290 */    82,   83,   84,   85,   86,   87,   88,   89,   31,   91,
 /*   300 */    92,   35,   94,   95,   96,   97,   98,   26,  100,  101,
 /*   310 */   102,   62,   70,   49,  106,  107,  108,   75,   76,   77,
 /*   320 */    78,   79,   80,   81,   82,   83,   84,   85,   86,   87,
 /*   330 */    88,   89,   35,   91,   92,   48,   94,   95,   96,   97,
 /*   340 */    98,   26,  100,  101,  102,   31,   50,   49,  106,  107,
 /*   350 */   108,   55,   70,   57,   58,   59,   60,   75,   76,   77,
 /*   360 */    78,   79,   80,   81,   82,   83,   84,   85,   86,   87,
 /*   370 */    88,   89,   35,   91,   92,   35,   94,   95,   96,   97,
 /*   380 */    98,   64,  100,  101,  102,   31,   70,   48,  106,  107,
 /*   390 */   108,   75,   76,   77,   78,   79,   80,   81,   82,   83,
 /*   400 */    84,   85,   86,   87,   88,   89,   34,   91,   92,   62,
 /*   410 */    94,   95,   96,   97,   98,   48,  100,  101,  102,   26,
 /*   420 */    70,   48,  106,  107,  108,   75,   76,   77,   78,   79,
 /*   430 */    80,   81,   82,   83,   84,   85,   86,   87,   88,   89,
 /*   440 */   126,   91,   92,  126,   94,   95,   96,   97,   98,  126,
 /*   450 */   100,  101,  102,  126,   70,  126,  106,  107,  108,   75,
 /*   460 */    76,   77,   78,   79,   80,   81,   82,   83,   84,   85,
 /*   470 */    86,   87,   88,   89,  126,   91,   92,  126,   94,   95,
 /*   480 */    96,   97,   98,  126,  100,  101,  102,  126,   70,  126,
 /*   490 */   106,  107,  108,   75,   76,   77,   78,   79,   80,   81,
 /*   500 */    82,   83,   84,   85,   86,   87,   88,   89,  126,   91,
 /*   510 */    92,  126,   94,   95,   96,   97,   98,  126,  100,  101,
 /*   520 */   102,  126,   70,  126,  106,  107,  108,   75,   76,   77,
 /*   530 */    78,   79,   80,   81,   82,   83,   84,   85,   86,   87,
 /*   540 */    88,   89,  126,   91,   92,  126,   94,   95,   96,   97,
 /*   550 */    98,  126,  100,  101,  102,  126,  126,  126,  106,  107,
 /*   560 */   108,    1,    2,    3,    4,    5,    6,    7,    8,    9,
 /*   570 */    10,   11,   12,   13,   14,   15,   16,   17,   18,   19,
 /*   580 */    20,   21,   22,   23,   24,   25,   15,    8,    9,   10,
 /*   590 */    11,   12,   13,   14,   15,   16,   17,   18,   27,   28,
 /*   600 */    29,  126,   31,   32,   50,   34,  126,   36,   37,   38,
 /*   610 */    39,   40,   41,   42,   43,   44,   45,  126,   47,   48,
 /*   620 */    50,   15,   68,   69,  126,   55,   55,   57,   58,   59,
 /*   630 */    60,  126,  126,   27,   28,   29,  126,   31,   32,  126,
 /*   640 */    34,  126,   36,   37,   38,   39,   40,   41,   42,   43,
 /*   650 */    44,   45,  126,   47,   48,   92,   15,   94,   95,   96,
 /*   660 */    97,   98,  126,  100,  101,  102,  126,  126,   27,   28,
 /*   670 */    29,  126,   31,   32,  126,   34,  126,   36,   37,   38,
 /*   680 */    39,   40,   41,   42,   43,   44,   45,  126,   47,    2,
 /*   690 */     3,    4,    5,    6,    7,    8,    9,   10,   11,   12,
 /*   700 */    13,   14,   15,   16,   17,   18,    3,    4,    5,    6,
 /*   710 */     7,    8,    9,   10,   11,   12,   13,   14,   15,   16,
 /*   720 */    17,   18,   77,   10,   11,   12,   13,   14,   15,   16,
 /*   730 */    17,   18,  126,   88,   89,   26,   91,   92,  126,   94,
 /*   740 */    95,   96,   97,   98,  126,  100,  101,  102,  126,  126,
 /*   750 */   126,  126,   92,  108,   94,   95,   96,   97,   98,  126,
 /*   760 */   100,  101,  102,  126,   55,  120,  121,  126,  126,   60,
 /*   770 */     4,    5,    6,    7,    8,    9,   10,   11,   12,   13,
 /*   780 */    14,   15,   16,   17,   18,   88,   89,  126,   91,   92,
 /*   790 */   126,   94,   95,   96,   97,   98,  126,  100,  101,  102,
 /*   800 */   103,  104,   88,   89,  126,   91,   92,  126,   94,   95,
 /*   810 */    96,   97,   98,   99,  100,  101,  102,  126,  126,  105,
 /*   820 */   126,  126,   88,   89,  126,   91,   92,  126,   94,   95,
 /*   830 */    96,   97,   98,  126,  100,  101,  102,  103,  104,  126,
 /*   840 */     5,    6,    7,    8,    9,   10,   11,   12,   13,   14,
 /*   850 */    15,   16,   17,   18,    6,    7,    8,    9,   10,   11,
 /*   860 */    12,   13,   14,   15,   16,   17,   18,   88,   89,  126,
 /*   870 */    91,   92,  126,   94,   95,   96,   97,   98,  126,  100,
 /*   880 */   101,  102,  126,  126,  105,  126,   88,   89,   90,   91,
 /*   890 */    92,  126,   94,   95,   96,   97,   98,  126,  100,  101,
 /*   900 */   102,   88,   89,  126,   91,   92,  126,   94,   95,   96,
 /*   910 */    97,   98,  126,  100,  101,  102,  126,   88,   89,  126,
 /*   920 */    91,   92,  126,   94,   95,   96,   97,   98,  126,  100,
 /*   930 */   101,  102,   88,   89,  126,   91,   92,  126,   94,   95,
 /*   940 */    96,   97,   98,  126,  100,  101,  102,   88,   89,  126,
 /*   950 */    91,   92,  126,   94,   95,   96,   97,   98,  126,  100,
 /*   960 */   101,  102,   88,   89,  126,   91,   92,  126,   94,   95,
 /*   970 */    96,   97,   98,  126,  100,  101,  102,   88,   89,  126,
 /*   980 */    91,   92,  126,   94,   95,   96,   97,   98,  126,  100,
 /*   990 */   101,  102,  126,   88,   89,  126,   91,   92,  126,   94,
 /*  1000 */    95,   96,   97,   98,  126,  100,  101,  102,  126,   88,
 /*  1010 */    89,  126,   91,   92,  126,   94,   95,   96,   97,   98,
 /*  1020 */   126,  100,  101,  102,   88,   89,  126,   91,   92,  126,
 /*  1030 */    94,   95,   96,   97,   98,  126,  100,  101,  102,   88,
 /*  1040 */    89,  126,   91,   92,  126,   94,   95,   96,   97,   98,
 /*  1050 */   126,  100,  101,  102,   88,   89,  126,   91,   92,  126,
 /*  1060 */    94,   95,   96,   97,   98,  126,  100,  101,  102,   88,
 /*  1070 */    89,  126,   91,   92,  126,   94,   95,   96,   97,   98,
 /*  1080 */   126,  100,  101,  102,   89,  126,   91,   92,  126,   94,
 /*  1090 */    95,   96,   97,   98,  126,  100,  101,  102,  126,   89,
 /*  1100 */   126,   91,   92,  126,   94,   95,   96,   97,   98,  126,
 /*  1110 */   100,  101,  102,  126,   92,  126,   94,   95,   96,   97,
 /*  1120 */    98,  126,  100,  101,  102,   92,  126,   94,   95,   96,
 /*  1130 */    97,   98,  126,  100,  101,  102,   92,  126,   94,   95,
 /*  1140 */    96,   97,   98,  126,  100,  101,  102,   92,  126,   94,
 /*  1150 */    95,   96,   97,   98,  126,  100,  101,  102,  126,   92,
 /*  1160 */   126,   94,   95,   96,   97,   98,  126,  100,  101,  102,
 /*  1170 */    92,  126,   94,   95,   96,   97,   98,  126,  100,  101,
 /*  1180 */   102,  126,   92,  126,   94,   95,   96,   97,   98,  126,
 /*  1190 */   100,  101,  102,  126,   92,  126,   94,   95,   96,   97,
 /*  1200 */    98,  126,  100,  101,  102,   92,  126,   94,   95,   96,
 /*  1210 */    97,   98,  126,  100,  101,  102,   92,  126,   94,   95,
 /*  1220 */    96,   97,   98,  126,  100,  101,  102,   92,  126,   94,
 /*  1230 */    95,   96,   97,   98,  126,  100,  101,  102,   92,  126,
 /*  1240 */    94,   95,   96,   97,   98,  126,  100,  101,  102,   92,
 /*  1250 */   126,   94,   95,   96,   97,   98,  126,  100,  101,  102,
 /*  1260 */    92,  126,   94,   95,   96,   97,   98,  126,  100,  101,
 /*  1270 */   102,   92,  126,   94,   95,   96,   97,   98,  126,  100,
 /*  1280 */   101,  102,   37,   38,   39,   40,   41,   42,   43,   44,
 /*  1290 */    45,   92,  126,   94,   95,   96,   97,   98,  126,  100,
 /*  1300 */   101,  102,   94,   95,   96,   97,   98,  126,  100,  101,
 /*  1310 */   102,   94,   95,   96,   97,   98,  126,  100,  101,  102,
 /*  1320 */    94,   95,   96,   97,   98,  126,  100,  101,  102,  126,
 /*  1330 */    94,   95,   96,   97,   98,  126,  100,  101,  102,  114,
 /*  1340 */   126,  116,  117,  118,  119,   14,   15,   16,   17,   18,
 /*  1350 */    27,   28,  126,   30,  126,   32,
};
#define YY_SHIFT_USE_DFLT (-13)
#define YY_SHIFT_MAX 135
static const short yy_shift_ofst[] = {
 /*     0 */    98,   98,   98,   14,   98,   56,   98,   98,   98,   98,
 /*    10 */    98,   98,   98,   98,  571,  641,  641,  641,  641,  641,
 /*    20 */   606,  641,  641,  641,  641,  641,  641,  641,  641,  641,
 /*    30 */   641,  641,  641,  641,  641,  641,  641,  641,  641,  641,
 /*    40 */   641,  641,  641,  641,  641,  641,  641,  641,  641,  641,
 /*    50 */   641,  641,  641,  641,  641,  641,  296,  -11,  570,  -11,
 /*    60 */   709,  554,  102,  193,  560, 1245,  138,  102,   89,  -13,
 /*    70 */   687,  703,  766,  835,  848,  579,  579,  713,  713, 1331,
 /*    80 */  1331, 1331, 1331, 1323,   60,   60,  105,  -12,  213,   85,
 /*    90 */   236,  237,  245,  266,  281,  297,  315,  314,  340,  317,
 /*   100 */   372,  367,  393,  347,  339,  354,  337,  298,  287,  264,
 /*   110 */   249,  267,  240,  222,   83,  246,  189,  199,  195,  154,
 /*   120 */   140,  156,  140,  134,  110,  100,   80,   85,   83,   41,
 /*   130 */   198,   -4,   12,   48,   43,  373,
};
#define YY_REDUCE_USE_DFLT (-74)
#define YY_REDUCE_MAX 69
static const short yy_reduce_ofst[] = {
 /*     0 */   -70,  131,   96,  208,  166,  316,  242,  316,  418,  350,
 /*    10 */   282,  316,  384,  452,  645,  714,  697,  734,  798,  779,
 /*    20 */   829,  859,  889,  921,  813,  981,  966,  936,  905,  951,
 /*    30 */   874,  844,  995, 1010, 1146, 1168, 1199, 1179, 1157, 1135,
 /*    40 */  1090, 1067, 1044, 1022,  563, 1102, 1078, 1113, 1055,  660,
 /*    50 */  1033, 1124, 1236, 1208, 1217, 1226,   49, 1225,  -73,  160,
 /*    60 */   -59,   -9,  -37,  -37,  106,   95,   58,   21,    1,  -36,
};
static const YYACTIONTYPE yy_default[] = {
 /*     0 */   251,  393,  393,  393,  393,  393,  393,  252,  393,  393,
 /*    10 */   393,  392,  393,  393,  393,  333,  393,  393,  267,  393,
 /*    20 */   393,  393,  393,  393,  393,  393,  393,  393,  393,  393,
 /*    30 */   393,  393,  393,  393,  393,  393,  393,  393,  393,  393,
 /*    40 */   393,  393,  393,  393,  393,  393,  393,  393,  393,  393,
 /*    50 */   393,  393,  393,  393,  393,  393,  393,  368,  393,  393,
 /*    60 */   393,  393,  393,  393,  277,  393,  393,  393,  393,  387,
 /*    70 */   280,  281,  282,  283,  284,  297,  296,  286,  285,  287,
 /*    80 */   290,  289,  288,  298,  291,  292,  349,  393,  328,  326,
 /*    90 */   393,  393,  393,  393,  393,  393,  393,  393,  393,  383,
 /*   100 */   393,  393,  393,  313,  393,  393,  393,  393,  393,  393,
 /*   110 */   393,  393,  393,  393,  366,  349,  369,  393,  393,  348,
 /*   120 */   370,  393,  371,  393,  393,  393,  393,  308,  393,  393,
 /*   130 */   393,  393,  393,  393,  393,  393,  342,  341,  343,  340,
 /*   140 */   344,  345,  339,  346,  337,  347,  336,  295,  294,  293,
 /*   150 */   278,  350,  352,  351,  276,  275,  274,  353,  273,  354,
 /*   160 */   272,  355,  356,  271,  310,  357,  358,  359,  309,  334,
 /*   170 */   335,  332,  311,  365,  376,  331,  330,  329,  327,  325,
 /*   180 */   324,  380,  323,  322,  321,  320,  319,  318,  384,  317,
 /*   190 */   316,  315,  314,  385,  313,  312,  307,  304,  303,  302,
 /*   200 */   386,  388,  389,  301,  300,  299,  306,  390,  305,  279,
 /*   210 */   391,  270,  269,  266,  265,  264,  268,  263,  262,  377,
 /*   220 */   378,  261,  379,  260,  259,  258,  257,  381,  256,  255,
 /*   230 */   367,  254,  253,  250,  249,  248,  375,  373,  374,  247,
 /*   240 */   372,  360,  361,  362,  364,  363,  338,
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
 /*  64 */ "function_call ::= IDENTIFIER LPAREN argument_list RPAREN",
 /*  65 */ "primary_expression ::= literal",
 /*  66 */ "primary_expression ::= id_expression",
 /*  67 */ "primary_expression ::= LPAREN expression RPAREN",
 /*  68 */ "primary_expression ::= list_literal",
 /*  69 */ "primary_expression ::= THIS",
 /*  70 */ "literal ::= INTEGER",
 /*  71 */ "literal ::= HEX",
 /*  72 */ "literal ::= BIN",
 /*  73 */ "literal ::= ROM",
 /*  74 */ "literal ::= REAL",
 /*  75 */ "literal ::= STRING",
 /*  76 */ "literal ::= TRUE",
 /*  77 */ "literal ::= FALSE",
 /*  78 */ "literal ::= NULL",
 /*  79 */ "id_expression ::= IDENTIFIER",
 /*  80 */ "list_literal ::= LBRACKET list_content RBRACKET",
 /*  81 */ "list_content ::= list_entry",
 /*  82 */ "list_content ::= list_entry COMMA list_content",
 /*  83 */ "list_entry ::= expression",
 /*  84 */ "new_expression ::= NEW IDENTIFIER",
 /*  85 */ "argument ::= expression",
 /*  86 */ "argument_list ::=",
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
 /* 119 */ "parameter ::= IDENTIFIER",
 /* 120 */ "opt_parameter ::= IDENTIFIER ASSIGN expression",
 /* 121 */ "parameter_list ::=",
 /* 122 */ "parameter_list ::= parameters",
 /* 123 */ "parameter_list ::= opt_parameters",
 /* 124 */ "parameter_list ::= parameters COMMA opt_parameters",
 /* 125 */ "parameters ::= parameter",
 /* 126 */ "parameters ::= parameters COMMA parameter",
 /* 127 */ "opt_parameters ::= opt_parameter",
 /* 128 */ "opt_parameters ::= opt_parameters COMMA opt_parameter",
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
  { 99, 0 },
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
  { 116, 1 },
  { 117, 3 },
  { 114, 0 },
  { 114, 1 },
  { 114, 1 },
  { 114, 3 },
  { 118, 1 },
  { 118, 3 },
  { 119, 1 },
  { 119, 3 },
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
#line 1228 "cscript.c"
        break;
      case 1:
#line 78 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(translation_unit, yymsp[0].minor.yy67); }
#line 1233 "cscript.c"
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
      case 65:
      case 66:
      case 68:
      case 85:
      case 98:
      case 101:
      case 104:
      case 129:
      case 131:
      case 145:
#line 81 "cscript.in"
{ yygotominor.yy67 = yymsp[0].minor.yy67; }
#line 1270 "cscript.c"
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
#line 1284 "cscript.c"
        break;
      case 4:
      case 20:
#line 95 "cscript.in"
{ yygotominor.yy67 = 0; }
#line 1290 "cscript.c"
        break;
      case 18:
      case 89:
#line 113 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(empty_statement); }
#line 1296 "cscript.c"
        break;
      case 23:
#line 129 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(assignment_expression, yymsp[-1].minor.yy54, yymsp[-2].minor.yy67, yymsp[0].minor.yy67); }
#line 1301 "cscript.c"
        break;
      case 24:
#line 133 "cscript.in"
{ yygotominor.yy54 = op_assign; }
#line 1306 "cscript.c"
        break;
      case 25:
#line 134 "cscript.in"
{ yygotominor.yy54 = op_assadd; }
#line 1311 "cscript.c"
        break;
      case 26:
#line 135 "cscript.in"
{ yygotominor.yy54 = op_asssub; }
#line 1316 "cscript.c"
        break;
      case 27:
#line 136 "cscript.in"
{ yygotominor.yy54 = op_assmul; }
#line 1321 "cscript.c"
        break;
      case 28:
#line 137 "cscript.in"
{ yygotominor.yy54 = op_assdiv; }
#line 1326 "cscript.c"
        break;
      case 29:
#line 138 "cscript.in"
{ yygotominor.yy54 = op_assmod; }
#line 1331 "cscript.c"
        break;
      case 31:
#line 142 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(ternary_expression, yymsp[-4].minor.yy67, yymsp[-2].minor.yy67, yymsp[0].minor.yy67); }
#line 1336 "cscript.c"
        break;
      case 33:
#line 146 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(binary_expression, op_logor,   yymsp[-2].minor.yy67, yymsp[0].minor.yy67); }
#line 1341 "cscript.c"
        break;
      case 34:
#line 147 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(binary_expression, op_logand,  yymsp[-2].minor.yy67, yymsp[0].minor.yy67); }
#line 1346 "cscript.c"
        break;
      case 35:
#line 148 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(binary_expression, op_bitor,   yymsp[-2].minor.yy67, yymsp[0].minor.yy67); }
#line 1351 "cscript.c"
        break;
      case 36:
#line 149 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(binary_expression, op_bitxor,  yymsp[-2].minor.yy67, yymsp[0].minor.yy67); }
#line 1356 "cscript.c"
        break;
      case 37:
#line 150 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(binary_expression, op_bitand,  yymsp[-2].minor.yy67, yymsp[0].minor.yy67); }
#line 1361 "cscript.c"
        break;
      case 38:
#line 151 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(binary_expression, op_eq,   yymsp[-2].minor.yy67, yymsp[0].minor.yy67); }
#line 1366 "cscript.c"
        break;
      case 39:
#line 152 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(binary_expression, op_ne,   yymsp[-2].minor.yy67, yymsp[0].minor.yy67); }
#line 1371 "cscript.c"
        break;
      case 40:
#line 153 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(binary_expression, op_lt,   yymsp[-2].minor.yy67, yymsp[0].minor.yy67); }
#line 1376 "cscript.c"
        break;
      case 41:
#line 154 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(binary_expression, op_le,   yymsp[-2].minor.yy67, yymsp[0].minor.yy67); }
#line 1381 "cscript.c"
        break;
      case 42:
#line 155 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(binary_expression, op_gt,   yymsp[-2].minor.yy67, yymsp[0].minor.yy67); }
#line 1386 "cscript.c"
        break;
      case 43:
#line 156 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(binary_expression, op_ge,   yymsp[-2].minor.yy67, yymsp[0].minor.yy67); }
#line 1391 "cscript.c"
        break;
      case 44:
#line 157 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(binary_expression, op_add,  yymsp[-2].minor.yy67, yymsp[0].minor.yy67); }
#line 1396 "cscript.c"
        break;
      case 45:
#line 158 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(binary_expression, op_sub,  yymsp[-2].minor.yy67, yymsp[0].minor.yy67); }
#line 1401 "cscript.c"
        break;
      case 46:
#line 159 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(binary_expression, op_mul,  yymsp[-2].minor.yy67, yymsp[0].minor.yy67); }
#line 1406 "cscript.c"
        break;
      case 47:
#line 160 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(binary_expression, op_div,  yymsp[-2].minor.yy67, yymsp[0].minor.yy67); }
#line 1411 "cscript.c"
        break;
      case 48:
#line 161 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(binary_expression, op_mod,  yymsp[-2].minor.yy67, yymsp[0].minor.yy67); }
#line 1416 "cscript.c"
        break;
      case 49:
#line 162 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(binary_expression, op_seq,  yymsp[-2].minor.yy67, yymsp[0].minor.yy67); }
#line 1421 "cscript.c"
        break;
      case 50:
#line 163 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(binary_expression, op_sne,  yymsp[-2].minor.yy67, yymsp[0].minor.yy67); }
#line 1426 "cscript.c"
        break;
      case 52:
#line 167 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(prefix_expression, op_negate, yymsp[0].minor.yy67); }
#line 1431 "cscript.c"
        break;
      case 53:
#line 168 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(prefix_expression, op_preinc, yymsp[0].minor.yy67); }
#line 1436 "cscript.c"
        break;
      case 54:
#line 169 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(prefix_expression, op_predec, yymsp[0].minor.yy67); }
#line 1441 "cscript.c"
        break;
      case 55:
#line 170 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(prefix_expression, op_not,    yymsp[0].minor.yy67); }
#line 1446 "cscript.c"
        break;
      case 58:
#line 175 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(postfix_expression, op_postinc, yymsp[-1].minor.yy67); }
#line 1451 "cscript.c"
        break;
      case 59:
#line 176 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(postfix_expression, op_postdec, yymsp[-1].minor.yy67); }
#line 1456 "cscript.c"
        break;
      case 61:
#line 178 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(member_expression, yymsp[-2].minor.yy67, String(yymsp[0].minor.yy0)); }
#line 1461 "cscript.c"
        break;
      case 62:
#line 179 "cscript.in"
{ yygotominor.yy67 = yymsp[0].minor.yy67; yymsp[0].minor.yy67->m_a3 = yymsp[-2].minor.yy67; }
#line 1466 "cscript.c"
        break;
      case 63:
#line 180 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(index_expression, yymsp[-3].minor.yy67, yymsp[-1].minor.yy67); }
#line 1471 "cscript.c"
        break;
      case 64:
#line 183 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(function_call, String(yymsp[-3].minor.yy0), yymsp[-1].minor.yy127); }
#line 1476 "cscript.c"
        break;
      case 67:
      case 99:
      case 100:
      case 132:
#line 188 "cscript.in"
{ yygotominor.yy67 = yymsp[-1].minor.yy67; }
#line 1484 "cscript.c"
        break;
      case 69:
#line 190 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(this_expression); }
#line 1489 "cscript.c"
        break;
      case 70:
#line 193 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(literal, Variant(String(yymsp[0].minor.yy0), Variant::stInt)); }
#line 1494 "cscript.c"
        break;
      case 71:
#line 194 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(literal, Variant(hex2dec(String(yymsp[0].minor.yy0).c_str()))); }
#line 1499 "cscript.c"
        break;
      case 72:
#line 195 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(literal, Variant(bin2dec(String(yymsp[0].minor.yy0).c_str()))); }
#line 1504 "cscript.c"
        break;
      case 73:
#line 196 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(literal, Variant(rom2dec(String(yymsp[0].minor.yy0).c_str()))); }
#line 1509 "cscript.c"
        break;
      case 74:
#line 197 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(literal, Variant(String(yymsp[0].minor.yy0), Variant::stReal)); }
#line 1514 "cscript.c"
        break;
      case 75:
#line 198 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(literal, Variant(String(yymsp[0].minor.yy0), Variant::stString)); }
#line 1519 "cscript.c"
        break;
      case 76:
#line 199 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(literal, Variant(true));    }
#line 1524 "cscript.c"
        break;
      case 77:
#line 200 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(literal, Variant(false));   }
#line 1529 "cscript.c"
        break;
      case 78:
#line 201 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(literal, Variant());        }
#line 1534 "cscript.c"
        break;
      case 79:
#line 204 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(lvalue, String(yymsp[0].minor.yy0)); }
#line 1539 "cscript.c"
        break;
      case 80:
#line 207 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(list_literal, yymsp[-1].minor.yy67); }
#line 1544 "cscript.c"
        break;
      case 81:
#line 208 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(list_content, yymsp[0].minor.yy67); }
#line 1549 "cscript.c"
        break;
      case 82:
#line 209 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(list_content, yymsp[-2].minor.yy67, yymsp[0].minor.yy67); }
#line 1554 "cscript.c"
        break;
      case 83:
#line 211 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(list_entry, yymsp[0].minor.yy67); }
#line 1559 "cscript.c"
        break;
      case 84:
#line 214 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(new_expression, String(yymsp[0].minor.yy0)); }
#line 1564 "cscript.c"
        break;
      case 86:
      case 121:
      case 140:
#line 227 "cscript.in"
{ yygotominor.yy127 = new AstList; }
#line 1571 "cscript.c"
        break;
      case 87:
      case 125:
      case 127:
#line 228 "cscript.in"
{ yygotominor.yy127 = new AstList; yygotominor.yy127->push_back(yymsp[0].minor.yy67); }
#line 1578 "cscript.c"
        break;
      case 88:
      case 126:
      case 128:
#line 229 "cscript.in"
{ yygotominor.yy127 = yymsp[-2].minor.yy127; yymsp[-2].minor.yy127->push_back(yymsp[0].minor.yy67); }
#line 1585 "cscript.c"
        break;
      case 90:
#line 238 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(expression_statement, yymsp[-1].minor.yy67); }
#line 1590 "cscript.c"
        break;
      case 91:
#line 241 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(compound_statement); }
#line 1595 "cscript.c"
        break;
      case 92:
#line 242 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(compound_statement, yymsp[-1].minor.yy67); }
#line 1600 "cscript.c"
        break;
      case 93:
#line 245 "cscript.in"
{ p->Parse(yymsp[-1].minor.yy0); yygotominor.yy67 = p->GetRoot(); }
#line 1605 "cscript.c"
        break;
      case 94:
#line 248 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(return_statement, yymsp[-1].minor.yy67); }
#line 1610 "cscript.c"
        break;
      case 95:
#line 249 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(return_statement);    }
#line 1615 "cscript.c"
        break;
      case 96:
#line 252 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(break_statement); }
#line 1620 "cscript.c"
        break;
      case 97:
#line 253 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(continue_statement); }
#line 1625 "cscript.c"
        break;
      case 102:
#line 267 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(variable_declaration, String(yymsp[0].minor.yy0));    }
#line 1630 "cscript.c"
        break;
      case 103:
#line 268 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(variable_declaration, String(yymsp[-2].minor.yy0), yymsp[0].minor.yy67); }
#line 1635 "cscript.c"
        break;
      case 105:
#line 271 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(declaration_sequence, yymsp[-2].minor.yy67, yymsp[0].minor.yy67); }
#line 1640 "cscript.c"
        break;
      case 106:
#line 278 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(class_declaration, String(yymsp[-2].minor.yy0)); }
#line 1645 "cscript.c"
        break;
      case 107:
#line 279 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(class_declaration, String(yymsp[-3].minor.yy0), yymsp[-1].minor.yy127); }
#line 1650 "cscript.c"
        break;
      case 108:
#line 282 "cscript.in"
{ yygotominor.yy67 = yymsp[-1].minor.yy67; yymsp[-1].minor.yy67->m_props["access"] = accessDefault; }
#line 1655 "cscript.c"
        break;
      case 109:
#line 283 "cscript.in"
{ yygotominor.yy67 = yymsp[0].minor.yy67; yymsp[0].minor.yy67->m_props["access"] = accessDefault; }
#line 1660 "cscript.c"
        break;
      case 110:
#line 284 "cscript.in"
{ yygotominor.yy67 = yymsp[-1].minor.yy67; yymsp[-1].minor.yy67->m_props["access"] = yymsp[-2].minor.yy96; }
#line 1665 "cscript.c"
        break;
      case 111:
#line 285 "cscript.in"
{ yygotominor.yy67 = yymsp[0].minor.yy67; yymsp[0].minor.yy67->m_props["access"] = yymsp[-1].minor.yy96; }
#line 1670 "cscript.c"
        break;
      case 112:
#line 286 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(access_specifier, yymsp[-1].minor.yy96); }
#line 1675 "cscript.c"
        break;
      case 113:
#line 290 "cscript.in"
{ yygotominor.yy96 = accessPrivate;   }
#line 1680 "cscript.c"
        break;
      case 114:
#line 291 "cscript.in"
{ yygotominor.yy96 = accessProtected; }
#line 1685 "cscript.c"
        break;
      case 115:
#line 292 "cscript.in"
{ yygotominor.yy96 = accessPublic;    }
#line 1690 "cscript.c"
        break;
      case 116:
#line 296 "cscript.in"
{ 
  yygotominor.yy127 = new AstList;
  yygotominor.yy127->push_back(yymsp[0].minor.yy67);
}
#line 1698 "cscript.c"
        break;
      case 117:
#line 300 "cscript.in"
{ 
  yygotominor.yy127 = yymsp[-1].minor.yy127;
  yygotominor.yy127->push_back(yymsp[0].minor.yy67);
}
#line 1706 "cscript.c"
        break;
      case 118:
#line 311 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(function_declaration, String(yymsp[-4].minor.yy0), yymsp[-2].minor.yy127, yymsp[0].minor.yy67); }
#line 1711 "cscript.c"
        break;
      case 119:
#line 314 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(parameter, String(yymsp[0].minor.yy0)); }
#line 1716 "cscript.c"
        break;
      case 120:
#line 317 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(parameter, String(yymsp[-2].minor.yy0), yymsp[0].minor.yy67); }
#line 1721 "cscript.c"
        break;
      case 122:
      case 123:
#line 322 "cscript.in"
{ yygotominor.yy127 = yymsp[0].minor.yy127; }
#line 1727 "cscript.c"
        break;
      case 124:
#line 324 "cscript.in"
{ yygotominor.yy127 = yymsp[-2].minor.yy127; yymsp[-2].minor.yy127->adopt(*yymsp[0].minor.yy127); }
#line 1732 "cscript.c"
        break;
      case 130:
#line 346 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(for_statement, yymsp[-5].minor.yy67, yymsp[-4].minor.yy67, yymsp[-2].minor.yy67, yymsp[0].minor.yy67); }
#line 1737 "cscript.c"
        break;
      case 133:
      case 134:
#line 357 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(foreach_statement, yymsp[-4].minor.yy67, yymsp[-2].minor.yy67, yymsp[0].minor.yy67); }
#line 1743 "cscript.c"
        break;
      case 135:
#line 359 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(variable_declaration, String(yymsp[0].minor.yy0)); }
#line 1748 "cscript.c"
        break;
      case 136:
#line 370 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(if_statement, yymsp[-2].minor.yy67, yymsp[0].minor.yy67); }
#line 1753 "cscript.c"
        break;
      case 137:
#line 371 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(if_statement, yymsp[-4].minor.yy67, yymsp[-2].minor.yy67, yymsp[0].minor.yy67); }
#line 1758 "cscript.c"
        break;
      case 138:
#line 379 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(while_statement, yymsp[-2].minor.yy67,  yymsp[0].minor.yy67); }
#line 1763 "cscript.c"
        break;
      case 139:
#line 387 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(switch_statement, yymsp[-4].minor.yy67, yymsp[-1].minor.yy127); }
#line 1768 "cscript.c"
        break;
      case 141:
      case 142:
#line 392 "cscript.in"
{ yygotominor.yy127 = yymsp[-1].minor.yy127; yygotominor.yy127->push_back(yymsp[0].minor.yy67); }
#line 1774 "cscript.c"
        break;
      case 143:
#line 396 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(switch_case, yymsp[-2].minor.yy67, yymsp[0].minor.yy67); }
#line 1779 "cscript.c"
        break;
      case 144:
#line 399 "cscript.in"
{ yygotominor.yy67 = p->AllocAst(default_case, yymsp[0].minor.yy67); }
#line 1784 "cscript.c"
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
#line 1832 "cscript.c"
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
#line 1850 "cscript.c"
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

