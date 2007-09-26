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
#define YYNOCODE 136
#define YYACTIONTYPE unsigned short int
#define CScriptParseTOKENTYPE  Token 
typedef union {
  CScriptParseTOKENTYPE yy0;
  Ast* yy31;
  AccessTypes yy78;
  opcodes yy144;
  AstList* yy235;
  int yy271;
} YYMINORTYPE;
#ifndef YYSTACKDEPTH
#define YYSTACKDEPTH 100
#endif
#define CScriptParseARG_SDECL  Parser* p ;
#define CScriptParseARG_PDECL , Parser* p 
#define CScriptParseARG_FETCH  Parser* p  = yypParser->p 
#define CScriptParseARG_STORE yypParser->p  = p 
#define YYNSTATE 276
#define YYNRULE 159
#define YYERRORSYMBOL 74
#define YYERRSYMDT yy271
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
 /*     0 */   257,  436,  169,  260,    6,  251,  222,  209,  204,  202,
 /*    10 */   197,  189,  167,  174,  267,  266,  265,  263,  259,  136,
 /*    20 */   256,  101,  255,   68,   28,  253,   89,  237,  236,  235,
 /*    30 */   234,  232,  230,   54,  114,  122,  364,  245,  243,  106,
 /*    40 */   119,  157,  153,  154,   21,   57,   56,   55,  158,   99,
 /*    50 */    16,  241,   31,  142,  229,  228,  227,  225,  224,  221,
 /*    60 */   220,  216,  215,  214,   24,  135,   43,   52,   51,  268,
 /*    70 */     3,  275,  152,   19,  148,  150,   66,  143,  151,  164,
 /*    80 */   155,  133,  125,  218,  219,   54,  115,  103,  130,  159,
 /*    90 */   423,   96,   37,   41,   43,   52,   51,   57,   56,   55,
 /*   100 */   217,   99,   16,  177,   31,  134,  229,  228,  227,  225,
 /*   110 */   224,  221,  220,  216,  215,  214,   35,  135,   69,  131,
 /*   120 */   178,  268,    3,  166,  152,   19,  148,  150,   66,  143,
 /*   130 */    21,   15,   64,  133,  125,  156,  264,   54,  115,  103,
 /*   140 */   130,  250,  247,   96,   71,  193,   30,  161,  123,   57,
 /*   150 */    56,   55,   66,   99,   16,   10,   31,  133,  229,  228,
 /*   160 */   227,  225,  224,  221,  220,  216,  215,  214,  163,  135,
 /*   170 */    92,  140,  147,  268,    3,  165,  152,   19,  148,  150,
 /*   180 */    66,  143,  192,   13,  126,  133,  125,  182,  183,  184,
 /*   190 */   115,  103,  130,   95,   60,   96,   98,  257,   18,   11,
 /*   200 */    33,   12,  251,  222,  209,  204,  202,  197,  189,  167,
 /*   210 */   174,  267,  266,  265,  263,  259,  136,  256,  101,  255,
 /*   220 */    68,  172,  253,   89,  237,  236,  235,  234,  232,  230,
 /*   230 */    94,  208,  128,  171,  245,  243,  106,  119,  157,  153,
 /*   240 */   154,  203,   72,  253,   89,  237,  236,  235,  234,  232,
 /*   250 */   230,    1,  176,  132,  257,   58,   70,  252,   12,  251,
 /*   260 */   222,  209,  204,  202,  197,  189,  167,  174,  267,  266,
 /*   270 */   265,  263,  259,  136,  256,  101,  255,   68,   74,  253,
 /*   280 */    89,  237,  236,  235,  234,  232,  230,    2,  100,   29,
 /*   290 */    15,  242,  243,  248,  108,  157,  153,  154,  258,  256,
 /*   300 */   144,  255,   68,   59,  253,   89,  237,  236,  235,  234,
 /*   310 */   232,  230,    4,  257,  249,  102,  188,    5,  251,  222,
 /*   320 */   209,  204,  202,  197,  189,  167,  174,  267,  266,  265,
 /*   330 */   263,  259,  136,  256,   23,  255,   68,   20,  253,   89,
 /*   340 */   237,  236,  235,  234,  232,  230,   14,   32,  231,    7,
 /*   350 */    35,   62,  257,  129,  157,  153,  154,  191,  222,  209,
 /*   360 */   204,  202,  197,  189,  167,  174,  267,  266,  265,  263,
 /*   370 */   259,  136,  256,  213,  255,   68,   73,  253,   89,  237,
 /*   380 */   236,  235,  234,  232,  230,  239,   89,  237,  236,  235,
 /*   390 */   234,  232,  230,  157,  153,  154,   63,   26,  257,   97,
 /*   400 */    17,   28,  190,  223,  222,  209,  204,  202,  197,  189,
 /*   410 */   167,  174,  267,  266,  265,  263,  259,  136,  256,   34,
 /*   420 */   255,   68,    8,  253,   89,  237,  236,  235,  234,  232,
 /*   430 */   230,    9,  200,   22,  437,  118,  437,  257,  437,  157,
 /*   440 */   153,  154,  261,  222,  209,  204,  202,  197,  189,  167,
 /*   450 */   174,  267,  266,  265,  263,  259,  136,  256,  437,  255,
 /*   460 */    68,  437,  253,   89,  237,  236,  235,  234,  232,  230,
 /*   470 */   437,  437,  437,  437,  437,  437,  437,  257,  157,  153,
 /*   480 */   154,  437,  105,  222,  209,  204,  202,  197,  189,  167,
 /*   490 */   174,  267,  266,  265,  263,  259,  136,  256,  437,  255,
 /*   500 */    68,  437,  253,   89,  237,  236,  235,  234,  232,  230,
 /*   510 */   437,  437,  437,  437,  437,  437,  257,  437,  157,  153,
 /*   520 */   154,  210,  222,  209,  204,  202,  197,  189,  167,  174,
 /*   530 */   267,  266,  265,  263,  259,  136,  256,  437,  255,   68,
 /*   540 */   437,  253,   89,  237,  236,  235,  234,  232,  230,  437,
 /*   550 */   437,  437,  437,  437,  437,  257,  437,  157,  153,  154,
 /*   560 */   198,  222,  209,  204,  202,  197,  189,  167,  174,  267,
 /*   570 */   266,  265,  263,  259,  136,  256,  437,  255,   68,  437,
 /*   580 */   253,   89,  237,  236,  235,  234,  232,  230,  437,  437,
 /*   590 */   437,  437,  437,  437,  257,  437,  157,  153,  154,  205,
 /*   600 */   222,  209,  204,  202,  197,  189,  167,  174,  267,  266,
 /*   610 */   265,  263,  259,  136,  256,  437,  255,   68,  437,  253,
 /*   620 */    89,  237,  236,  235,  234,  232,  230,  437,  437,  437,
 /*   630 */   437,  437,  437,  257,  437,  157,  153,  154,  269,  222,
 /*   640 */   209,  204,  202,  197,  189,  167,  174,  267,  266,  265,
 /*   650 */   263,  259,  136,  256,  437,  255,   68,  437,  253,   89,
 /*   660 */   237,  236,  235,  234,  232,  230,  238,   89,  237,  236,
 /*   670 */   235,  234,  232,  230,  157,  153,  154,   38,   42,   49,
 /*   680 */    44,   40,   48,   46,   36,   45,   39,   47,   53,   50,
 /*   690 */    37,   41,   43,   52,   51,  186,  185,  181,  180,  179,
 /*   700 */   175,   25,   54,   36,   45,   39,   47,   53,   50,   37,
 /*   710 */    41,   43,   52,   51,   57,   56,   55,  437,   99,   16,
 /*   720 */   437,   31,  437,  229,  228,  227,  225,  224,  221,  220,
 /*   730 */   216,  215,  214,  437,  135,   54,  437,  173,  268,  137,
 /*   740 */   437,  437,   61,  274,   65,   67,  437,   57,   56,   55,
 /*   750 */   437,   99,   16,  437,   31,  437,  229,  228,  227,  225,
 /*   760 */   224,  221,  220,  216,  215,  214,  437,  135,  437,  437,
 /*   770 */   437,  160,  437,   38,   42,   49,   44,   40,   48,   46,
 /*   780 */    36,   45,   39,   47,   53,   50,   37,   41,   43,   52,
 /*   790 */    51,   54,  173,  437,  137,  437,  437,   25,  273,   65,
 /*   800 */   437,  437,  437,   57,   56,   55,  437,   93,   16,  437,
 /*   810 */    31,  437,  229,  228,  227,  225,  224,  221,  220,  216,
 /*   820 */   215,  214,  437,  135,   54,  201,   75,  437,  253,   89,
 /*   830 */   237,  236,  235,  234,  232,  230,   57,   56,   55,  437,
 /*   840 */    99,   16,  437,   31,  437,  229,  228,  227,  225,  224,
 /*   850 */   221,  220,  216,  215,  214,  437,  135,   42,   49,   44,
 /*   860 */    40,   48,   46,   36,   45,   39,   47,   53,   50,   37,
 /*   870 */    41,   43,   52,   51,  211,  256,  437,  255,   68,  262,
 /*   880 */   253,   89,  237,  236,  235,  234,  232,  230,  212,  110,
 /*   890 */   226,  136,  256,  437,  255,   68,  437,  253,   89,  237,
 /*   900 */   236,  235,  234,  120,  230,  437,  437,  182,  183,  184,
 /*   910 */   211,  256,  437,  255,   68,  146,  253,   89,  237,  236,
 /*   920 */   235,  234,  232,  230,  107,  110,  437,   27,  149,  437,
 /*   930 */   437,   49,   44,   40,   48,   46,   36,   45,   39,   47,
 /*   940 */    53,   50,   37,   41,   43,   52,   51,   44,   40,   48,
 /*   950 */    46,   36,   45,   39,   47,   53,   50,   37,   41,   43,
 /*   960 */    52,   51,  199,   75,  437,  253,   89,  237,  236,  235,
 /*   970 */   234,  232,  230,  437,  437,  437,  113,  196,  117,  194,
 /*   980 */   121,  437,   40,   48,   46,   36,   45,   39,   47,   53,
 /*   990 */    50,   37,   41,   43,   52,   51,   48,   46,   36,   45,
 /*  1000 */    39,   47,   53,   50,   37,   41,   43,   52,   51,  162,
 /*  1010 */   256,  437,  255,   68,  437,  253,   89,  237,  236,  235,
 /*  1020 */   234,  232,  230,  437,  437,  127,  256,  437,  255,   68,
 /*  1030 */   437,  253,   89,  237,  236,  235,  234,  232,  230,  141,
 /*  1040 */   256,  437,  255,   68,  437,  253,   89,  237,  236,  235,
 /*  1050 */   234,  232,  230,  109,  256,  437,  255,   68,  437,  253,
 /*  1060 */    89,  237,  236,  235,  234,  232,  230,  437,  437,  437,
 /*  1070 */   437,  233,  256,  437,  255,   68,  437,  253,   89,  237,
 /*  1080 */   236,  235,  234,  232,  230,  112,  256,  437,  255,   68,
 /*  1090 */   437,  253,   89,  237,  236,  235,  234,  232,  230,  187,
 /*  1100 */   256,  437,  255,   68,  437,  253,   89,  237,  236,  235,
 /*  1110 */   234,  232,  230,  104,  256,  437,  255,   68,  437,  253,
 /*  1120 */    89,  237,  236,  235,  234,  232,  230,  139,  256,  437,
 /*  1130 */   255,   68,  437,  253,   89,  237,  236,  235,  234,  232,
 /*  1140 */   230,  145,  256,  437,  255,   68,  437,  253,   89,  237,
 /*  1150 */   236,  235,  234,  232,  230,  116,  256,  437,  255,   68,
 /*  1160 */   437,  253,   89,  237,  236,  235,  234,  232,  230,  138,
 /*  1170 */   256,  437,  255,   68,  437,  253,   89,  237,  236,  235,
 /*  1180 */   234,  232,  230,  124,  256,  437,  255,   68,  437,  253,
 /*  1190 */    89,  237,  236,  235,  234,  232,  230,  254,  437,  255,
 /*  1200 */    68,  437,  253,   89,  237,  236,  235,  234,  232,  230,
 /*  1210 */   199,   75,  437,  253,   89,  237,  236,  235,  234,  232,
 /*  1220 */   230,  246,  437,  255,   68,  195,  253,   89,  237,  236,
 /*  1230 */   235,  234,  232,  230,   82,  437,  253,   89,  237,  236,
 /*  1240 */   235,  234,  232,  230,  437,  206,  437,  253,   89,  237,
 /*  1250 */   236,  235,  234,  232,  230,  437,   86,  437,  253,   89,
 /*  1260 */   237,  236,  235,  234,  232,  230,   87,  437,  253,   89,
 /*  1270 */   237,  236,  235,  234,  232,  230,   81,  437,  253,   89,
 /*  1280 */   237,  236,  235,  234,  232,  230,  437,   76,  437,  253,
 /*  1290 */    89,  237,  236,  235,  234,  232,  230,   84,  437,  253,
 /*  1300 */    89,  237,  236,  235,  234,  232,  230,   79,  437,  253,
 /*  1310 */    89,  237,  236,  235,  234,  232,  230,  207,  437,  253,
 /*  1320 */    89,  237,  236,  235,  234,  232,  230,   88,  437,  253,
 /*  1330 */    89,  237,  236,  235,  234,  232,  230,   77,  437,  253,
 /*  1340 */    89,  237,  236,  235,  234,  232,  230,   83,  437,  253,
 /*  1350 */    89,  237,  236,  235,  234,  232,  230,   91,  437,  253,
 /*  1360 */    89,  237,  236,  235,  234,  232,  230,   90,  437,  253,
 /*  1370 */    89,  237,  236,  235,  234,  232,  230,   80,  437,  253,
 /*  1380 */    89,  237,  236,  235,  234,  232,  230,   85,  437,  253,
 /*  1390 */    89,  237,  236,  235,  234,  232,  230,   39,   47,   53,
 /*  1400 */    50,   37,   41,   43,   52,   51,  437,  228,  227,  225,
 /*  1410 */   224,  221,  220,  216,  215,  214,  437,   78,  437,  253,
 /*  1420 */    89,  237,  236,  235,  234,  232,  230,  244,   89,  237,
 /*  1430 */   236,  235,  234,  232,  230,  240,   89,  237,  236,  235,
 /*  1440 */   234,  232,  230,  168,  111,  437,  437,  437,   66,  170,
 /*  1450 */   270,  271,  272,  133,   66,  437,  270,  271,  272,  133,
 /*  1460 */   437,  182,  183,  184,
};
static const YYCODETYPE yy_lookahead[] = {
 /*     0 */    74,   75,   76,   77,   78,   79,   80,   81,   82,   83,
 /*    10 */    84,   85,   86,   87,   88,   89,   90,   91,   92,   93,
 /*    20 */    94,  109,   96,   97,   19,   99,  100,  101,  102,  103,
 /*    30 */   104,  105,  106,   15,  109,  123,   31,  125,  126,  127,
 /*    40 */   128,  115,  116,  117,   19,   27,   28,   29,   51,   31,
 /*    50 */    32,  126,   34,   42,   36,   37,   38,   39,   40,   41,
 /*    60 */    42,   43,   44,   45,   65,   47,   16,   17,   18,   51,
 /*    70 */    52,   53,   54,   55,   56,   57,   58,   59,  118,  119,
 /*    80 */    51,   63,   64,  132,  133,   15,   68,   69,   70,   51,
 /*    90 */    65,   73,   14,   15,   16,   17,   18,   27,   28,   29,
 /*   100 */    53,   31,   32,  115,   34,  117,   36,   37,   38,   39,
 /*   110 */    40,   41,   42,   43,   44,   45,   19,   47,   71,   72,
 /*   120 */    26,   51,   52,   53,   54,   55,   56,   57,   58,   59,
 /*   130 */    19,   34,  131,   63,   64,   51,   51,   15,   68,   69,
 /*   140 */    70,   27,   28,   73,   30,  113,   32,   51,   31,   27,
 /*   150 */    28,   29,   58,   31,   32,   35,   34,   63,   36,   37,
 /*   160 */    38,   39,   40,   41,   42,   43,   44,   45,  119,   47,
 /*   170 */    31,   31,   31,   51,   52,   51,   54,   55,   56,   57,
 /*   180 */    58,   59,  103,   35,   31,   63,   64,   48,   49,   50,
 /*   190 */    68,   69,   70,  104,   52,   73,  109,   74,   51,   35,
 /*   200 */    98,   78,   79,   80,   81,   82,   83,   84,   85,   86,
 /*   210 */    87,   88,   89,   90,   91,   92,   93,   94,  109,   96,
 /*   220 */    97,   51,   99,  100,  101,  102,  103,  104,  105,  106,
 /*   230 */    31,   31,  123,   51,  125,  126,  127,  128,  115,  116,
 /*   240 */   117,   97,   46,   99,  100,  101,  102,  103,  104,  105,
 /*   250 */   106,   26,   51,   31,   74,   34,   50,  134,   78,   79,
 /*   260 */    80,   81,   82,   83,   84,   85,   86,   87,   88,   89,
 /*   270 */    90,   91,   92,   93,   94,  109,   96,   97,   52,   99,
 /*   280 */   100,  101,  102,  103,  104,  105,  106,   26,   31,   34,
 /*   290 */    34,  125,  126,   51,  128,  115,  116,  117,   93,   94,
 /*   300 */    95,   96,   97,   34,   99,  100,  101,  102,  103,  104,
 /*   310 */   105,  106,   35,   74,  134,   31,   33,   78,   79,   80,
 /*   320 */    81,   82,   83,   84,   85,   86,   87,   88,   89,   90,
 /*   330 */    91,   92,   93,   94,   19,   96,   97,   34,   99,  100,
 /*   340 */   101,  102,  103,  104,  105,  106,   34,   26,   35,   67,
 /*   350 */    19,   46,   74,   35,  115,  116,  117,   79,   80,   81,
 /*   360 */    82,   83,   84,   85,   86,   87,   88,   89,   90,   91,
 /*   370 */    92,   93,   94,   33,   96,   97,   46,   99,  100,  101,
 /*   380 */   102,  103,  104,  105,  106,   99,  100,  101,  102,  103,
 /*   390 */   104,  105,  106,  115,  116,  117,   46,   65,   74,   35,
 /*   400 */    46,   19,  124,   79,   80,   81,   82,   83,   84,   85,
 /*   410 */    86,   87,   88,   89,   90,   91,   92,   93,   94,   46,
 /*   420 */    96,   97,   35,   99,  100,  101,  102,  103,  104,  105,
 /*   430 */   106,   35,   35,   34,  135,   31,  135,   74,  135,  115,
 /*   440 */   116,  117,   79,   80,   81,   82,   83,   84,   85,   86,
 /*   450 */    87,   88,   89,   90,   91,   92,   93,   94,  135,   96,
 /*   460 */    97,  135,   99,  100,  101,  102,  103,  104,  105,  106,
 /*   470 */   135,  135,  135,  135,  135,  135,  135,   74,  115,  116,
 /*   480 */   117,  135,   79,   80,   81,   82,   83,   84,   85,   86,
 /*   490 */    87,   88,   89,   90,   91,   92,   93,   94,  135,   96,
 /*   500 */    97,  135,   99,  100,  101,  102,  103,  104,  105,  106,
 /*   510 */   135,  135,  135,  135,  135,  135,   74,  135,  115,  116,
 /*   520 */   117,   79,   80,   81,   82,   83,   84,   85,   86,   87,
 /*   530 */    88,   89,   90,   91,   92,   93,   94,  135,   96,   97,
 /*   540 */   135,   99,  100,  101,  102,  103,  104,  105,  106,  135,
 /*   550 */   135,  135,  135,  135,  135,   74,  135,  115,  116,  117,
 /*   560 */    79,   80,   81,   82,   83,   84,   85,   86,   87,   88,
 /*   570 */    89,   90,   91,   92,   93,   94,  135,   96,   97,  135,
 /*   580 */    99,  100,  101,  102,  103,  104,  105,  106,  135,  135,
 /*   590 */   135,  135,  135,  135,   74,  135,  115,  116,  117,   79,
 /*   600 */    80,   81,   82,   83,   84,   85,   86,   87,   88,   89,
 /*   610 */    90,   91,   92,   93,   94,  135,   96,   97,  135,   99,
 /*   620 */   100,  101,  102,  103,  104,  105,  106,  135,  135,  135,
 /*   630 */   135,  135,  135,   74,  135,  115,  116,  117,   79,   80,
 /*   640 */    81,   82,   83,   84,   85,   86,   87,   88,   89,   90,
 /*   650 */    91,   92,   93,   94,  135,   96,   97,  135,   99,  100,
 /*   660 */   101,  102,  103,  104,  105,  106,   99,  100,  101,  102,
 /*   670 */   103,  104,  105,  106,  115,  116,  117,    1,    2,    3,
 /*   680 */     4,    5,    6,    7,    8,    9,   10,   11,   12,   13,
 /*   690 */    14,   15,   16,   17,   18,   19,   20,   21,   22,   23,
 /*   700 */    24,   25,   15,    8,    9,   10,   11,   12,   13,   14,
 /*   710 */    15,   16,   17,   18,   27,   28,   29,  135,   31,   32,
 /*   720 */   135,   34,  135,   36,   37,   38,   39,   40,   41,   42,
 /*   730 */    43,   44,   45,  135,   47,   15,  135,  115,   51,  117,
 /*   740 */   135,  135,  120,  121,  122,   58,  135,   27,   28,   29,
 /*   750 */   135,   31,   32,  135,   34,  135,   36,   37,   38,   39,
 /*   760 */    40,   41,   42,   43,   44,   45,  135,   47,  135,  135,
 /*   770 */   135,   51,  135,    1,    2,    3,    4,    5,    6,    7,
 /*   780 */     8,    9,   10,   11,   12,   13,   14,   15,   16,   17,
 /*   790 */    18,   15,  115,  135,  117,  135,  135,   25,  121,  122,
 /*   800 */   135,  135,  135,   27,   28,   29,  135,   31,   32,  135,
 /*   810 */    34,  135,   36,   37,   38,   39,   40,   41,   42,   43,
 /*   820 */    44,   45,  135,   47,   15,   96,   97,  135,   99,  100,
 /*   830 */   101,  102,  103,  104,  105,  106,   27,   28,   29,  135,
 /*   840 */    31,   32,  135,   34,  135,   36,   37,   38,   39,   40,
 /*   850 */    41,   42,   43,   44,   45,  135,   47,    2,    3,    4,
 /*   860 */     5,    6,    7,    8,    9,   10,   11,   12,   13,   14,
 /*   870 */    15,   16,   17,   18,   93,   94,  135,   96,   97,   81,
 /*   880 */    99,  100,  101,  102,  103,  104,  105,  106,  107,  108,
 /*   890 */    31,   93,   94,  135,   96,   97,  135,   99,  100,  101,
 /*   900 */   102,  103,  104,  105,  106,  135,  135,   48,   49,   50,
 /*   910 */    93,   94,  135,   96,   97,  117,   99,  100,  101,  102,
 /*   920 */   103,  104,  105,  106,  107,  108,  135,  129,  130,  135,
 /*   930 */   135,    3,    4,    5,    6,    7,    8,    9,   10,   11,
 /*   940 */    12,   13,   14,   15,   16,   17,   18,    4,    5,    6,
 /*   950 */     7,    8,    9,   10,   11,   12,   13,   14,   15,   16,
 /*   960 */    17,   18,   96,   97,  135,   99,  100,  101,  102,  103,
 /*   970 */   104,  105,  106,  135,  135,  135,  110,  111,  112,  113,
 /*   980 */   114,  135,    5,    6,    7,    8,    9,   10,   11,   12,
 /*   990 */    13,   14,   15,   16,   17,   18,    6,    7,    8,    9,
 /*  1000 */    10,   11,   12,   13,   14,   15,   16,   17,   18,   93,
 /*  1010 */    94,  135,   96,   97,  135,   99,  100,  101,  102,  103,
 /*  1020 */   104,  105,  106,  135,  135,   93,   94,  135,   96,   97,
 /*  1030 */   135,   99,  100,  101,  102,  103,  104,  105,  106,   93,
 /*  1040 */    94,  135,   96,   97,  135,   99,  100,  101,  102,  103,
 /*  1050 */   104,  105,  106,   93,   94,  135,   96,   97,  135,   99,
 /*  1060 */   100,  101,  102,  103,  104,  105,  106,  135,  135,  135,
 /*  1070 */   135,   93,   94,  135,   96,   97,  135,   99,  100,  101,
 /*  1080 */   102,  103,  104,  105,  106,   93,   94,  135,   96,   97,
 /*  1090 */   135,   99,  100,  101,  102,  103,  104,  105,  106,   93,
 /*  1100 */    94,  135,   96,   97,  135,   99,  100,  101,  102,  103,
 /*  1110 */   104,  105,  106,   93,   94,  135,   96,   97,  135,   99,
 /*  1120 */   100,  101,  102,  103,  104,  105,  106,   93,   94,  135,
 /*  1130 */    96,   97,  135,   99,  100,  101,  102,  103,  104,  105,
 /*  1140 */   106,   93,   94,  135,   96,   97,  135,   99,  100,  101,
 /*  1150 */   102,  103,  104,  105,  106,   93,   94,  135,   96,   97,
 /*  1160 */   135,   99,  100,  101,  102,  103,  104,  105,  106,   93,
 /*  1170 */    94,  135,   96,   97,  135,   99,  100,  101,  102,  103,
 /*  1180 */   104,  105,  106,   93,   94,  135,   96,   97,  135,   99,
 /*  1190 */   100,  101,  102,  103,  104,  105,  106,   94,  135,   96,
 /*  1200 */    97,  135,   99,  100,  101,  102,  103,  104,  105,  106,
 /*  1210 */    96,   97,  135,   99,  100,  101,  102,  103,  104,  105,
 /*  1220 */   106,   94,  135,   96,   97,  111,   99,  100,  101,  102,
 /*  1230 */   103,  104,  105,  106,   97,  135,   99,  100,  101,  102,
 /*  1240 */   103,  104,  105,  106,  135,   97,  135,   99,  100,  101,
 /*  1250 */   102,  103,  104,  105,  106,  135,   97,  135,   99,  100,
 /*  1260 */   101,  102,  103,  104,  105,  106,   97,  135,   99,  100,
 /*  1270 */   101,  102,  103,  104,  105,  106,   97,  135,   99,  100,
 /*  1280 */   101,  102,  103,  104,  105,  106,  135,   97,  135,   99,
 /*  1290 */   100,  101,  102,  103,  104,  105,  106,   97,  135,   99,
 /*  1300 */   100,  101,  102,  103,  104,  105,  106,   97,  135,   99,
 /*  1310 */   100,  101,  102,  103,  104,  105,  106,   97,  135,   99,
 /*  1320 */   100,  101,  102,  103,  104,  105,  106,   97,  135,   99,
 /*  1330 */   100,  101,  102,  103,  104,  105,  106,   97,  135,   99,
 /*  1340 */   100,  101,  102,  103,  104,  105,  106,   97,  135,   99,
 /*  1350 */   100,  101,  102,  103,  104,  105,  106,   97,  135,   99,
 /*  1360 */   100,  101,  102,  103,  104,  105,  106,   97,  135,   99,
 /*  1370 */   100,  101,  102,  103,  104,  105,  106,   97,  135,   99,
 /*  1380 */   100,  101,  102,  103,  104,  105,  106,   97,  135,   99,
 /*  1390 */   100,  101,  102,  103,  104,  105,  106,   10,   11,   12,
 /*  1400 */    13,   14,   15,   16,   17,   18,  135,   37,   38,   39,
 /*  1410 */    40,   41,   42,   43,   44,   45,  135,   97,  135,   99,
 /*  1420 */   100,  101,  102,  103,  104,  105,  106,   99,  100,  101,
 /*  1430 */   102,  103,  104,  105,  106,   99,  100,  101,  102,  103,
 /*  1440 */   104,  105,  106,   53,   31,  135,  135,  135,   58,   53,
 /*  1450 */    60,   61,   62,   63,   58,  135,   60,   61,   62,   63,
 /*  1460 */   135,   48,   49,   50,
};
#define YY_SHIFT_USE_DFLT (-4)
#define YY_SHIFT_MAX 154
static const short yy_shift_ofst[] = {
 /*     0 */   122,  122,  122,   18,  122,   70,  122,  122,  122,  122,
 /*    10 */   122,  122,  122,  122,  687,  776,  809,  809,  809,  720,
 /*    20 */   809,  809,  809,  809,  809,  809,  809,  809,  809,  809,
 /*    30 */   809,  809,  809,  809,  809,  809,  809,  809,  809,  809,
 /*    40 */   809,  809,  809,  809,  809,  809,  809,  809,  809,  809,
 /*    50 */   809,  809,  809,  809,  809,  809,  809,  809,  139,  139,
 /*    60 */  1390, 1396,  139, 1413,   47,   94,  141,  199,  676, 1370,
 /*    70 */   859,  153,  141,  117,   -4,  772,  855,  928,  943,  977,
 /*    80 */   990,  695,  695, 1387, 1387,   78,   78,   78,   78,  114,
 /*    90 */    50,   50,    5,   97,   25,  225,  206,  226,  257,  256,
 /*   100 */   269,  284,  315,  303,  313,  282,  305,  340,  350,  364,
 /*   110 */   354,  382,  387,  397,  404,  399,  396,  373,  315,  350,
 /*   120 */   332,  330,  318,  331,  321,  312,  256,  283,  277,  242,
 /*   130 */   255,  261,  221,  222,  201,  200,  182,  170,  164,  147,
 /*   140 */   142,  148,  124,  140,  120,   96,   85,  111,   38,   -1,
 /*   150 */    -3,  196,   11,   29,   84,
};
#define YY_REDUCE_USE_DFLT (-89)
#define YY_REDUCE_MAX 74
static const short yy_reduce_ofst[] = {
 /*     0 */   -74,  180,  123,  239,  278,  324,  324,  520,  403,  481,
 /*    10 */   363,  442,  324,  559,  798,  866,  817,  781,  205, 1048,
 /*    20 */  1076,  916,  992,  978,  946, 1090, 1062, 1034, 1006,  960,
 /*    30 */   932, 1020, 1127, 1103, 1114,  729, 1250, 1270, 1190, 1290,
 /*    40 */  1280, 1260, 1240, 1220, 1210, 1200, 1179, 1159, 1137, 1320,
 /*    50 */  1230,  144, 1148, 1169, 1328,  567,  286, 1336,  109,  -88,
 /*    60 */   622,  677,  166,  -75,  -49,  -12,  -40,  -40,  102,   89,
 /*    70 */    87,   79,   49,   32,    1,
};
static const YYACTIONTYPE yy_default[] = {
 /*     0 */   280,  435,  435,  435,  435,  435,  281,  435,  435,  435,
 /*    10 */   435,  435,  433,  435,  435,  372,  435,  435,  297,  435,
 /*    20 */   435,  435,  435,  435,  435,  435,  435,  435,  435,  435,
 /*    30 */   435,  435,  435,  435,  435,  435,  435,  435,  435,  435,
 /*    40 */   435,  435,  435,  435,  435,  435,  435,  435,  435,  435,
 /*    50 */   435,  435,  435,  435,  435,  435,  435,  435,  409,  409,
 /*    60 */   435,  435,  435,  435,  435,  435,  435,  435,  307,  435,
 /*    70 */   435,  435,  435,  435,  428,  307,  310,  311,  312,  313,
 /*    80 */   314,  327,  326,  315,  316,  317,  318,  319,  320,  328,
 /*    90 */   321,  322,  406,  355,  388,  435,  435,  435,  435,  355,
 /*   100 */   435,  435,  405,  435,  435,  424,  410,  435,  412,  435,
 /*   110 */   357,  364,  435,  435,  435,  435,  435,  373,  435,  411,
 /*   120 */   342,  374,  435,  435,  435,  435,  338,  435,  435,  435,
 /*   130 */   435,  435,  435,  435,  435,  435,  435,  435,  435,  435,
 /*   140 */   435,  435,  435,  435,  435,  435,  435,  388,  435,  435,
 /*   150 */   435,  387,  435,  435,  435,  385,  386,  384,  383,  382,
 /*   160 */   381,  380,  389,  391,  390,  379,  378,  288,  392,  276,
 /*   170 */   393,  376,  394,  395,  289,  306,  396,  397,  398,  305,
 /*   180 */   304,  303,  361,  362,  363,  302,  301,  408,  340,  287,
 /*   190 */   404,  417,  339,  371,  370,  368,  367,  286,  421,  366,
 /*   200 */   365,  369,  285,  325,  284,  425,  324,  323,  360,  283,
 /*   210 */   426,  359,  358,  356,  354,  353,  352,  427,  429,  430,
 /*   220 */   351,  350,  282,  279,  349,  348,  364,  347,  346,  345,
 /*   230 */   344,  343,  342,  407,  341,  337,  334,  333,  332,  331,
 /*   240 */   330,  416,  414,  415,  329,  413,  308,  336,  434,  431,
 /*   250 */   335,  278,  432,  309,  300,  299,  296,  295,  298,  294,
 /*   260 */   277,  418,  419,  293,  420,  292,  291,  290,  375,  422,
 /*   270 */   399,  400,  401,  403,  402,  377,
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
  "THIS",          "LIT_INTEGER",   "LIT_HEX",       "LIT_BIN",     
  "LIT_ROM",       "LIT_REAL",      "LIT_STRING",    "TRUE",        
  "FALSE",         "NULL",          "COMMA",         "NEW",         
  "BOOL",          "INT",           "STRING",        "SEMICOLON",   
  "LBRACE",        "RBRACE",        "INCLUDE",       "RETURN",      
  "BREAK",         "CONTINUE",      "VAR",           "CLASS",       
  "PRIVATE",       "PROTECTED",     "PUBLIC",        "FUNCTION",    
  "FOR",           "IN",            "LOWER_THAN_ELSE",  "ELSE",        
  "IF",            "WHILE",         "SWITCH",        "CASE",        
  "DEFAULT",       "EXTERN",        "error",         "main",        
  "translation_unit",  "statement_sequence_opt",  "statement_sequence",  "statement",   
  "include_statement",  "expression_statement",  "declaration_statement",  "for_statement",
  "compound_statement",  "if_statement",  "while_statement",  "foreach_statement",
  "return_statement",  "switch_statement",  "break_statement",  "continue_statement",
  "extern_declaration",  "expression",    "assignment_expression",  "expression_opt",
  "conditional_expression",  "binary_expression",  "assignment_operator",  "unary_expression",
  "postfix_expression",  "new_expression",  "primary_expression",  "function_call",
  "literal",       "id_expression",  "list_literal",  "list_content",
  "list_entry",    "type",          "argument_list",  "positional_argument",
  "positional_argument_list",  "named_argument",  "named_argument_list",  "function_declaration",
  "class_declaration",  "variable_declaration",  "declarator_sequence",  "declarator",  
  "class_members",  "class_member",  "access_specifier",  "parameter_list",
  "function_body",  "parameter",     "opt_parameter",  "parameters",  
  "opt_parameters",  "for_init_statement",  "foreach_decl",  "switch_body", 
  "switch_case",   "default_case",  "case_statements",
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
 /*  65 */ "primary_expression ::= literal",
 /*  66 */ "primary_expression ::= id_expression",
 /*  67 */ "primary_expression ::= LPAREN expression RPAREN",
 /*  68 */ "primary_expression ::= list_literal",
 /*  69 */ "primary_expression ::= THIS",
 /*  70 */ "literal ::= LIT_INTEGER",
 /*  71 */ "literal ::= LIT_HEX",
 /*  72 */ "literal ::= LIT_BIN",
 /*  73 */ "literal ::= LIT_ROM",
 /*  74 */ "literal ::= LIT_REAL",
 /*  75 */ "literal ::= LIT_STRING",
 /*  76 */ "literal ::= TRUE",
 /*  77 */ "literal ::= FALSE",
 /*  78 */ "literal ::= NULL",
 /*  79 */ "id_expression ::= IDENTIFIER",
 /*  80 */ "list_literal ::= LBRACKET list_content RBRACKET",
 /*  81 */ "list_content ::= list_entry",
 /*  82 */ "list_content ::= list_entry COMMA list_content",
 /*  83 */ "list_entry ::= expression",
 /*  84 */ "new_expression ::= NEW IDENTIFIER",
 /*  85 */ "type ::= BOOL",
 /*  86 */ "type ::= INT",
 /*  87 */ "type ::= STRING",
 /*  88 */ "type ::= IDENTIFIER",
 /*  89 */ "function_call ::= IDENTIFIER LPAREN argument_list RPAREN",
 /*  90 */ "positional_argument ::= conditional_expression",
 /*  91 */ "positional_argument_list ::= positional_argument",
 /*  92 */ "positional_argument_list ::= positional_argument_list COMMA positional_argument",
 /*  93 */ "named_argument ::= IDENTIFIER ASSIGN conditional_expression",
 /*  94 */ "named_argument_list ::= named_argument",
 /*  95 */ "named_argument_list ::= named_argument_list COMMA named_argument",
 /*  96 */ "argument_list ::=",
 /*  97 */ "argument_list ::= positional_argument_list",
 /*  98 */ "argument_list ::= named_argument_list",
 /*  99 */ "expression_statement ::= SEMICOLON",
 /* 100 */ "expression_statement ::= expression SEMICOLON",
 /* 101 */ "compound_statement ::= LBRACE RBRACE",
 /* 102 */ "compound_statement ::= LBRACE statement_sequence RBRACE",
 /* 103 */ "include_statement ::= INCLUDE LIT_STRING SEMICOLON",
 /* 104 */ "return_statement ::= RETURN expression SEMICOLON",
 /* 105 */ "return_statement ::= RETURN SEMICOLON",
 /* 106 */ "break_statement ::= BREAK SEMICOLON",
 /* 107 */ "continue_statement ::= CONTINUE SEMICOLON",
 /* 108 */ "declaration_statement ::= function_declaration",
 /* 109 */ "declaration_statement ::= class_declaration SEMICOLON",
 /* 110 */ "declaration_statement ::= variable_declaration SEMICOLON",
 /* 111 */ "variable_declaration ::= VAR declarator_sequence",
 /* 112 */ "declarator ::= IDENTIFIER",
 /* 113 */ "declarator ::= IDENTIFIER ASSIGN expression",
 /* 114 */ "declarator_sequence ::= declarator",
 /* 115 */ "declarator_sequence ::= declarator_sequence COMMA declarator",
 /* 116 */ "class_declaration ::= CLASS IDENTIFIER LBRACE RBRACE",
 /* 117 */ "class_declaration ::= CLASS IDENTIFIER LBRACE class_members RBRACE",
 /* 118 */ "class_member ::= variable_declaration SEMICOLON",
 /* 119 */ "class_member ::= function_declaration",
 /* 120 */ "class_member ::= access_specifier variable_declaration SEMICOLON",
 /* 121 */ "class_member ::= access_specifier function_declaration",
 /* 122 */ "class_member ::= access_specifier COLON",
 /* 123 */ "access_specifier ::= PRIVATE",
 /* 124 */ "access_specifier ::= PROTECTED",
 /* 125 */ "access_specifier ::= PUBLIC",
 /* 126 */ "class_members ::= class_member",
 /* 127 */ "class_members ::= class_members class_member",
 /* 128 */ "function_declaration ::= FUNCTION IDENTIFIER LPAREN parameter_list RPAREN function_body",
 /* 129 */ "parameter ::= type IDENTIFIER",
 /* 130 */ "parameter ::= IDENTIFIER",
 /* 131 */ "opt_parameter ::= type IDENTIFIER ASSIGN expression",
 /* 132 */ "opt_parameter ::= IDENTIFIER ASSIGN expression",
 /* 133 */ "parameter_list ::=",
 /* 134 */ "parameter_list ::= parameters",
 /* 135 */ "parameter_list ::= opt_parameters",
 /* 136 */ "parameter_list ::= parameters COMMA opt_parameters",
 /* 137 */ "parameters ::= parameter",
 /* 138 */ "parameters ::= parameters COMMA parameter",
 /* 139 */ "opt_parameters ::= opt_parameter",
 /* 140 */ "opt_parameters ::= opt_parameters COMMA opt_parameter",
 /* 141 */ "function_body ::= statement",
 /* 142 */ "for_statement ::= FOR LPAREN for_init_statement expression SEMICOLON expression_opt RPAREN statement",
 /* 143 */ "for_init_statement ::= expression_statement",
 /* 144 */ "for_init_statement ::= variable_declaration SEMICOLON",
 /* 145 */ "foreach_statement ::= FOR LPAREN id_expression IN expression RPAREN statement",
 /* 146 */ "foreach_statement ::= FOR LPAREN foreach_decl IN expression RPAREN statement",
 /* 147 */ "foreach_decl ::= VAR IDENTIFIER",
 /* 148 */ "if_statement ::= IF LPAREN expression RPAREN statement",
 /* 149 */ "if_statement ::= IF LPAREN expression RPAREN statement ELSE statement",
 /* 150 */ "while_statement ::= WHILE LPAREN expression RPAREN statement",
 /* 151 */ "switch_statement ::= SWITCH LPAREN expression RPAREN LBRACE switch_body RBRACE",
 /* 152 */ "switch_body ::=",
 /* 153 */ "switch_body ::= switch_body switch_case",
 /* 154 */ "switch_body ::= switch_body default_case",
 /* 155 */ "switch_case ::= CASE literal COLON case_statements",
 /* 156 */ "default_case ::= DEFAULT COLON case_statements",
 /* 157 */ "case_statements ::= statement_sequence",
 /* 158 */ "extern_declaration ::= EXTERN STRING type IDENTIFIER LPAREN parameter_list RPAREN SEMICOLON",
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
  { 75, 1 },
  { 76, 1 },
  { 78, 1 },
  { 78, 2 },
  { 77, 0 },
  { 77, 1 },
  { 79, 1 },
  { 79, 1 },
  { 79, 1 },
  { 79, 1 },
  { 79, 1 },
  { 79, 1 },
  { 79, 1 },
  { 79, 1 },
  { 79, 1 },
  { 79, 1 },
  { 79, 1 },
  { 79, 1 },
  { 79, 1 },
  { 79, 1 },
  { 93, 1 },
  { 95, 0 },
  { 95, 1 },
  { 94, 1 },
  { 94, 3 },
  { 98, 1 },
  { 98, 1 },
  { 98, 1 },
  { 98, 1 },
  { 98, 1 },
  { 98, 1 },
  { 96, 1 },
  { 96, 5 },
  { 97, 1 },
  { 97, 3 },
  { 97, 3 },
  { 97, 3 },
  { 97, 3 },
  { 97, 3 },
  { 97, 3 },
  { 97, 3 },
  { 97, 3 },
  { 97, 3 },
  { 97, 3 },
  { 97, 3 },
  { 97, 3 },
  { 97, 3 },
  { 97, 3 },
  { 97, 3 },
  { 97, 3 },
  { 97, 3 },
  { 97, 3 },
  { 99, 1 },
  { 99, 2 },
  { 99, 2 },
  { 99, 2 },
  { 99, 2 },
  { 99, 1 },
  { 100, 1 },
  { 100, 2 },
  { 100, 2 },
  { 100, 1 },
  { 100, 3 },
  { 100, 3 },
  { 100, 4 },
  { 102, 1 },
  { 102, 1 },
  { 102, 3 },
  { 102, 1 },
  { 102, 1 },
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
  { 101, 2 },
  { 109, 1 },
  { 109, 1 },
  { 109, 1 },
  { 109, 1 },
  { 103, 4 },
  { 111, 1 },
  { 112, 1 },
  { 112, 3 },
  { 113, 3 },
  { 114, 1 },
  { 114, 3 },
  { 110, 0 },
  { 110, 1 },
  { 110, 1 },
  { 81, 1 },
  { 81, 2 },
  { 84, 2 },
  { 84, 3 },
  { 80, 3 },
  { 88, 3 },
  { 88, 2 },
  { 90, 2 },
  { 91, 2 },
  { 82, 1 },
  { 82, 2 },
  { 82, 2 },
  { 117, 2 },
  { 119, 1 },
  { 119, 3 },
  { 118, 1 },
  { 118, 3 },
  { 116, 4 },
  { 116, 5 },
  { 121, 2 },
  { 121, 1 },
  { 121, 3 },
  { 121, 2 },
  { 121, 2 },
  { 122, 1 },
  { 122, 1 },
  { 122, 1 },
  { 120, 1 },
  { 120, 2 },
  { 115, 6 },
  { 125, 2 },
  { 125, 1 },
  { 126, 4 },
  { 126, 3 },
  { 123, 0 },
  { 123, 1 },
  { 123, 1 },
  { 123, 3 },
  { 127, 1 },
  { 127, 3 },
  { 128, 1 },
  { 128, 3 },
  { 124, 1 },
  { 83, 8 },
  { 129, 1 },
  { 129, 2 },
  { 87, 7 },
  { 87, 7 },
  { 130, 2 },
  { 85, 5 },
  { 85, 7 },
  { 86, 5 },
  { 89, 7 },
  { 131, 0 },
  { 131, 2 },
  { 131, 2 },
  { 132, 4 },
  { 133, 3 },
  { 134, 1 },
  { 92, 8 },
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
{ p->SetRoot(yymsp[0].minor.yy31); }
#line 1284 "cscript.c"
        break;
      case 1:
#line 78 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(translation_unit, yymsp[0].minor.yy31); }
#line 1289 "cscript.c"
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
      case 65:
      case 66:
      case 68:
      case 90:
      case 108:
      case 111:
      case 114:
      case 141:
      case 143:
      case 157:
#line 81 "cscript.in"
{ yygotominor.yy31 = yymsp[0].minor.yy31; }
#line 1327 "cscript.c"
        break;
      case 3:
#line 82 "cscript.in"
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
#line 1341 "cscript.c"
        break;
      case 4:
      case 21:
#line 95 "cscript.in"
{ yygotominor.yy31 = 0; }
#line 1347 "cscript.c"
        break;
      case 19:
      case 99:
#line 114 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(empty_statement); }
#line 1353 "cscript.c"
        break;
      case 24:
#line 130 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(assignment_expression, yymsp[-1].minor.yy144, yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1358 "cscript.c"
        break;
      case 25:
#line 134 "cscript.in"
{ yygotominor.yy144 = op_assign; }
#line 1363 "cscript.c"
        break;
      case 26:
#line 135 "cscript.in"
{ yygotominor.yy144 = op_assadd; }
#line 1368 "cscript.c"
        break;
      case 27:
#line 136 "cscript.in"
{ yygotominor.yy144 = op_asssub; }
#line 1373 "cscript.c"
        break;
      case 28:
#line 137 "cscript.in"
{ yygotominor.yy144 = op_assmul; }
#line 1378 "cscript.c"
        break;
      case 29:
#line 138 "cscript.in"
{ yygotominor.yy144 = op_assdiv; }
#line 1383 "cscript.c"
        break;
      case 30:
#line 139 "cscript.in"
{ yygotominor.yy144 = op_assmod; }
#line 1388 "cscript.c"
        break;
      case 32:
#line 143 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(ternary_expression, yymsp[-4].minor.yy31, yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1393 "cscript.c"
        break;
      case 34:
#line 147 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(binary_expression, op_logor,   yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1398 "cscript.c"
        break;
      case 35:
#line 148 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(binary_expression, op_logand,  yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1403 "cscript.c"
        break;
      case 36:
#line 149 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(binary_expression, op_bitor,   yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1408 "cscript.c"
        break;
      case 37:
#line 150 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(binary_expression, op_bitxor,  yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1413 "cscript.c"
        break;
      case 38:
#line 151 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(binary_expression, op_bitand,  yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1418 "cscript.c"
        break;
      case 39:
#line 152 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(binary_expression, op_eq,   yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1423 "cscript.c"
        break;
      case 40:
#line 153 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(binary_expression, op_ne,   yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1428 "cscript.c"
        break;
      case 41:
#line 154 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(binary_expression, op_lt,   yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1433 "cscript.c"
        break;
      case 42:
#line 155 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(binary_expression, op_le,   yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1438 "cscript.c"
        break;
      case 43:
#line 156 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(binary_expression, op_gt,   yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1443 "cscript.c"
        break;
      case 44:
#line 157 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(binary_expression, op_ge,   yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1448 "cscript.c"
        break;
      case 45:
#line 158 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(binary_expression, op_add,  yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1453 "cscript.c"
        break;
      case 46:
#line 159 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(binary_expression, op_sub,  yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1458 "cscript.c"
        break;
      case 47:
#line 160 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(binary_expression, op_mul,  yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1463 "cscript.c"
        break;
      case 48:
#line 161 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(binary_expression, op_div,  yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1468 "cscript.c"
        break;
      case 49:
#line 162 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(binary_expression, op_mod,  yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1473 "cscript.c"
        break;
      case 50:
#line 163 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(binary_expression, op_seq,  yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1478 "cscript.c"
        break;
      case 51:
#line 164 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(binary_expression, op_sne,  yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1483 "cscript.c"
        break;
      case 53:
#line 168 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(prefix_expression, op_negate, yymsp[0].minor.yy31); }
#line 1488 "cscript.c"
        break;
      case 54:
#line 169 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(prefix_expression, op_preinc, yymsp[0].minor.yy31); }
#line 1493 "cscript.c"
        break;
      case 55:
#line 170 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(prefix_expression, op_predec, yymsp[0].minor.yy31); }
#line 1498 "cscript.c"
        break;
      case 56:
#line 171 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(prefix_expression, op_not,    yymsp[0].minor.yy31); }
#line 1503 "cscript.c"
        break;
      case 59:
#line 176 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(postfix_expression, op_postinc, yymsp[-1].minor.yy31); }
#line 1508 "cscript.c"
        break;
      case 60:
#line 177 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(postfix_expression, op_postdec, yymsp[-1].minor.yy31); }
#line 1513 "cscript.c"
        break;
      case 62:
#line 179 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(member_expression, yymsp[-2].minor.yy31, String(yymsp[0].minor.yy0)); }
#line 1518 "cscript.c"
        break;
      case 63:
#line 180 "cscript.in"
{ yygotominor.yy31 = yymsp[0].minor.yy31; yymsp[0].minor.yy31->m_a3 = yymsp[-2].minor.yy31; }
#line 1523 "cscript.c"
        break;
      case 64:
#line 181 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(index_expression, yymsp[-3].minor.yy31, yymsp[-1].minor.yy31); }
#line 1528 "cscript.c"
        break;
      case 67:
      case 109:
      case 110:
      case 144:
#line 186 "cscript.in"
{ yygotominor.yy31 = yymsp[-1].minor.yy31; }
#line 1536 "cscript.c"
        break;
      case 69:
#line 188 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(this_expression); }
#line 1541 "cscript.c"
        break;
      case 70:
#line 191 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(literal_value, Variant(String(yymsp[0].minor.yy0), Variant::stInt)); }
#line 1546 "cscript.c"
        break;
      case 71:
#line 192 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(literal_value, Variant(hex2dec(String(yymsp[0].minor.yy0).c_str()))); }
#line 1551 "cscript.c"
        break;
      case 72:
#line 193 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(literal_value, Variant(bin2dec(String(yymsp[0].minor.yy0).c_str()))); }
#line 1556 "cscript.c"
        break;
      case 73:
#line 194 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(literal_value, Variant(rom2dec(String(yymsp[0].minor.yy0).c_str()))); }
#line 1561 "cscript.c"
        break;
      case 74:
#line 195 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(literal_value, Variant(String(yymsp[0].minor.yy0), Variant::stReal)); }
#line 1566 "cscript.c"
        break;
      case 75:
#line 196 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(literal_value, Variant(String(yymsp[0].minor.yy0), Variant::stString)); }
#line 1571 "cscript.c"
        break;
      case 76:
#line 197 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(literal_value, Variant(true));    }
#line 1576 "cscript.c"
        break;
      case 77:
#line 198 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(literal_value, Variant(false));   }
#line 1581 "cscript.c"
        break;
      case 78:
#line 199 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(literal_value, Variant());        }
#line 1586 "cscript.c"
        break;
      case 79:
#line 202 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(lvalue, String(yymsp[0].minor.yy0)); }
#line 1591 "cscript.c"
        break;
      case 80:
#line 205 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(list_literal, yymsp[-1].minor.yy31); }
#line 1596 "cscript.c"
        break;
      case 81:
#line 206 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(list_content, yymsp[0].minor.yy31); }
#line 1601 "cscript.c"
        break;
      case 82:
#line 207 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(list_content, yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1606 "cscript.c"
        break;
      case 83:
#line 209 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(list_entry, yymsp[0].minor.yy31); }
#line 1611 "cscript.c"
        break;
      case 84:
#line 212 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(new_expression, String(yymsp[0].minor.yy0)); }
#line 1616 "cscript.c"
        break;
      case 85:
#line 219 "cscript.in"
{ yygotominor.yy31 = new Ast(builtin_type, Variant::stBool);    }
#line 1621 "cscript.c"
        break;
      case 86:
#line 220 "cscript.in"
{ yygotominor.yy31 = new Ast(builtin_type, Variant::stInt);     }
#line 1626 "cscript.c"
        break;
      case 87:
#line 221 "cscript.in"
{ yygotominor.yy31 = new Ast(builtin_type, Variant::stString);  }
#line 1631 "cscript.c"
        break;
      case 88:
#line 222 "cscript.in"
{ yygotominor.yy31 = new Ast(class_type, String(yymsp[0].minor.yy0));            }
#line 1636 "cscript.c"
        break;
      case 89:
#line 230 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(function_call, String(yymsp[-3].minor.yy0), yymsp[-1].minor.yy31); }
#line 1641 "cscript.c"
        break;
      case 91:
      case 94:
      case 137:
      case 139:
#line 237 "cscript.in"
{ yygotominor.yy235 = new AstList; yygotominor.yy235->push_back(yymsp[0].minor.yy31); }
#line 1649 "cscript.c"
        break;
      case 92:
      case 95:
      case 138:
      case 140:
#line 238 "cscript.in"
{ yygotominor.yy235 = yymsp[-2].minor.yy235; yymsp[-2].minor.yy235->push_back(yymsp[0].minor.yy31); }
#line 1657 "cscript.c"
        break;
      case 93:
#line 241 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(named_argument, String(yymsp[-2].minor.yy0), yymsp[0].minor.yy31); }
#line 1662 "cscript.c"
        break;
      case 96:
#line 249 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(positional_arguments, new AstList); }
#line 1667 "cscript.c"
        break;
      case 97:
#line 250 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(positional_arguments, yymsp[0].minor.yy235); }
#line 1672 "cscript.c"
        break;
      case 98:
#line 251 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(named_arguments,      yymsp[0].minor.yy235); }
#line 1677 "cscript.c"
        break;
      case 100:
#line 260 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(expression_statement, yymsp[-1].minor.yy31); }
#line 1682 "cscript.c"
        break;
      case 101:
#line 263 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(compound_statement); }
#line 1687 "cscript.c"
        break;
      case 102:
#line 264 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(compound_statement, yymsp[-1].minor.yy31); }
#line 1692 "cscript.c"
        break;
      case 103:
#line 267 "cscript.in"
{ p->Parse(yymsp[-1].minor.yy0); yygotominor.yy31 = p->GetRoot(); }
#line 1697 "cscript.c"
        break;
      case 104:
#line 270 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(return_statement, yymsp[-1].minor.yy31); }
#line 1702 "cscript.c"
        break;
      case 105:
#line 271 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(return_statement);    }
#line 1707 "cscript.c"
        break;
      case 106:
#line 274 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(break_statement); }
#line 1712 "cscript.c"
        break;
      case 107:
#line 275 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(continue_statement); }
#line 1717 "cscript.c"
        break;
      case 112:
#line 289 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(variable_declaration, String(yymsp[0].minor.yy0));    }
#line 1722 "cscript.c"
        break;
      case 113:
#line 290 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(variable_declaration, String(yymsp[-2].minor.yy0), yymsp[0].minor.yy31); }
#line 1727 "cscript.c"
        break;
      case 115:
#line 293 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(declaration_sequence, yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1732 "cscript.c"
        break;
      case 116:
#line 300 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(class_declaration, String(yymsp[-2].minor.yy0)); }
#line 1737 "cscript.c"
        break;
      case 117:
#line 301 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(class_declaration, String(yymsp[-3].minor.yy0), yymsp[-1].minor.yy235); }
#line 1742 "cscript.c"
        break;
      case 118:
#line 304 "cscript.in"
{ yygotominor.yy31 = yymsp[-1].minor.yy31; yymsp[-1].minor.yy31->m_props["access"] = accessDefault; }
#line 1747 "cscript.c"
        break;
      case 119:
#line 305 "cscript.in"
{ yygotominor.yy31 = yymsp[0].minor.yy31; yymsp[0].minor.yy31->m_props["access"] = accessDefault; }
#line 1752 "cscript.c"
        break;
      case 120:
#line 306 "cscript.in"
{ yygotominor.yy31 = yymsp[-1].minor.yy31; yymsp[-1].minor.yy31->m_props["access"] = yymsp[-2].minor.yy78; }
#line 1757 "cscript.c"
        break;
      case 121:
#line 307 "cscript.in"
{ yygotominor.yy31 = yymsp[0].minor.yy31; yymsp[0].minor.yy31->m_props["access"] = yymsp[-1].minor.yy78; }
#line 1762 "cscript.c"
        break;
      case 122:
#line 308 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(access_specifier, yymsp[-1].minor.yy78); }
#line 1767 "cscript.c"
        break;
      case 123:
#line 312 "cscript.in"
{ yygotominor.yy78 = accessPrivate;   }
#line 1772 "cscript.c"
        break;
      case 124:
#line 313 "cscript.in"
{ yygotominor.yy78 = accessProtected; }
#line 1777 "cscript.c"
        break;
      case 125:
#line 314 "cscript.in"
{ yygotominor.yy78 = accessPublic;    }
#line 1782 "cscript.c"
        break;
      case 126:
#line 318 "cscript.in"
{ 
  yygotominor.yy235 = new AstList;
  yygotominor.yy235->push_back(yymsp[0].minor.yy31);
}
#line 1790 "cscript.c"
        break;
      case 127:
#line 322 "cscript.in"
{ 
  yygotominor.yy235 = yymsp[-1].minor.yy235;
  yygotominor.yy235->push_back(yymsp[0].minor.yy31);
}
#line 1798 "cscript.c"
        break;
      case 128:
#line 333 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(function_declaration, String(yymsp[-4].minor.yy0), yymsp[-2].minor.yy235, yymsp[0].minor.yy31); }
#line 1803 "cscript.c"
        break;
      case 129:
#line 336 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(parameter, String(yymsp[0].minor.yy0), yymsp[-1].minor.yy31); }
#line 1808 "cscript.c"
        break;
      case 130:
#line 337 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(parameter, String(yymsp[0].minor.yy0)); }
#line 1813 "cscript.c"
        break;
      case 131:
#line 340 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(parameter, String(yymsp[-2].minor.yy0), yymsp[-3].minor.yy31,         yymsp[0].minor.yy31); }
#line 1818 "cscript.c"
        break;
      case 132:
#line 341 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(parameter, String(yymsp[-2].minor.yy0), AstData(), yymsp[0].minor.yy31); }
#line 1823 "cscript.c"
        break;
      case 133:
      case 152:
#line 345 "cscript.in"
{ yygotominor.yy235 = new AstList; }
#line 1829 "cscript.c"
        break;
      case 134:
      case 135:
#line 346 "cscript.in"
{ yygotominor.yy235 = yymsp[0].minor.yy235; }
#line 1835 "cscript.c"
        break;
      case 136:
#line 348 "cscript.in"
{ yygotominor.yy235 = yymsp[-2].minor.yy235; yymsp[-2].minor.yy235->adopt(*yymsp[0].minor.yy235); }
#line 1840 "cscript.c"
        break;
      case 142:
#line 370 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(for_statement, yymsp[-5].minor.yy31, yymsp[-4].minor.yy31, yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1845 "cscript.c"
        break;
      case 145:
      case 146:
#line 381 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(foreach_statement, yymsp[-4].minor.yy31, yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1851 "cscript.c"
        break;
      case 147:
#line 383 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(variable_declaration, String(yymsp[0].minor.yy0)); }
#line 1856 "cscript.c"
        break;
      case 148:
#line 394 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(if_statement, yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1861 "cscript.c"
        break;
      case 149:
#line 395 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(if_statement, yymsp[-4].minor.yy31, yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1866 "cscript.c"
        break;
      case 150:
#line 403 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(while_statement, yymsp[-2].minor.yy31,  yymsp[0].minor.yy31); }
#line 1871 "cscript.c"
        break;
      case 151:
#line 411 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(switch_statement, yymsp[-4].minor.yy31, yymsp[-1].minor.yy235); }
#line 1876 "cscript.c"
        break;
      case 153:
      case 154:
#line 416 "cscript.in"
{ yygotominor.yy235 = yymsp[-1].minor.yy235; yygotominor.yy235->push_back(yymsp[0].minor.yy31); }
#line 1882 "cscript.c"
        break;
      case 155:
#line 420 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(switch_case, yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1887 "cscript.c"
        break;
      case 156:
#line 423 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(default_case, yymsp[0].minor.yy31); }
#line 1892 "cscript.c"
        break;
      case 158:
#line 435 "cscript.in"
{ 
  yygotominor.yy31 = new Ast(extern_declaration, String(yymsp[-4].minor.yy0), String(yymsp[-6].minor.yy0), yymsp[-5].minor.yy31, yymsp[-2].minor.yy235); 
}
#line 1899 "cscript.c"
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
#line 1947 "cscript.c"
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
#line 1965 "cscript.c"
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

