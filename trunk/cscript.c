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
#define YYNSTATE 275
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
 /*     0 */   256,  435,  166,  259,    6,  250,  221,  208,  203,  201,
 /*    10 */   196,  188,  168,  173,  164,  265,  264,  262,  258,  135,
 /*    20 */   255,  106,  254,   68,   13,  252,   88,  237,  236,  235,
 /*    30 */   234,  233,  230,   54,   22,  124,   25,  244,  242,  112,
 /*    40 */   122,  154,  152,  151,   14,   55,   56,   57,  156,   96,
 /*    50 */    16,  176,   32,  129,  229,  228,  227,  226,  224,  223,
 /*    60 */   220,  219,  215,  214,  155,  109,   47,   43,   36,  267,
 /*    70 */     3,  274,  140,   20,  147,  149,   67,  141,  148,  163,
 /*    80 */   422,  127,  101,  217,  218,   54,  113,  134,  117,   72,
 /*    90 */    65,  100,   37,   39,   47,   43,   36,   55,   56,   57,
 /*   100 */   216,   96,   16,  157,   32,  266,  229,  228,  227,  226,
 /*   110 */   224,  223,  220,  219,  215,  214,  198,  109,   69,  130,
 /*   120 */   177,  267,    3,  165,  140,   20,  147,  149,   67,  141,
 /*   130 */   118,   73,   22,  127,  101,  115,  162,   54,  113,  134,
 /*   140 */   117,  249,  246,  100,   71,   30,   23,  240,  159,   55,
 /*   150 */    56,   57,   67,   96,   16,  145,   32,  127,  229,  228,
 /*   160 */   227,  226,  224,  223,  220,  219,  215,  214,  263,  109,
 /*   170 */   116,   28,  195,  267,    3,  150,  140,   20,  147,  149,
 /*   180 */    67,  141,  139,  363,  126,  127,  101,  181,  182,  183,
 /*   190 */   113,  134,  117,  102,   60,  100,   98,  256,   10,   35,
 /*   200 */    18,   12,  250,  221,  208,  203,  201,  196,  188,  168,
 /*   210 */   173,  164,  265,  264,  262,  258,  135,  255,  106,  254,
 /*   220 */    68,  171,  252,   88,  237,  236,  235,  234,  233,  230,
 /*   230 */    91,   31,   99,  170,  244,  242,  112,  122,  154,  152,
 /*   240 */   151,   86,  160,  252,   88,  237,  236,  235,  234,  233,
 /*   250 */   230,  213,   34,   59,  256,    7,   14,  251,   12,  250,
 /*   260 */   221,  208,  203,  201,  196,  188,  168,  173,  164,  265,
 /*   270 */   264,  262,  258,  135,  255,  106,  254,   68,   19,  252,
 /*   280 */    88,  237,  236,  235,  234,  233,  230,   74,    2,    1,
 /*   290 */     4,  241,  242,  175,  114,  154,  152,  151,  205,  255,
 /*   300 */   247,  254,   68,   70,  252,   88,  237,  236,  235,  234,
 /*   310 */   233,  230,  125,  256,  248,  133,  200,   15,  190,  221,
 /*   320 */   208,  203,  201,  196,  188,  168,  173,  164,  265,  264,
 /*   330 */   262,  258,  135,  255,   58,  254,   68,   17,  252,   88,
 /*   340 */   237,  236,  235,  234,  233,  230,  243,   88,  237,  236,
 /*   350 */   235,  234,  233,  230,  154,  152,  151,  128,   29,  206,
 /*   360 */   256,   97,   63,  189,    5,  250,  221,  208,  203,  201,
 /*   370 */   196,  188,  168,  173,  164,  265,  264,  262,  258,  135,
 /*   380 */   255,  108,  254,   68,    8,  252,   88,  237,  236,  235,
 /*   390 */   234,  233,  230,  194,   33,   28,   11,  210,  120,  256,
 /*   400 */   231,  154,  152,  151,  222,  221,  208,  203,  201,  196,
 /*   410 */   188,  168,  173,  164,  265,  264,  262,  258,  135,  255,
 /*   420 */    24,  254,   68,    9,  252,   88,  237,  236,  235,  234,
 /*   430 */   233,  230,   25,   62,   21,  436,  436,  436,  256,  436,
 /*   440 */   154,  152,  151,  260,  221,  208,  203,  201,  196,  188,
 /*   450 */   168,  173,  164,  265,  264,  262,  258,  135,  255,  436,
 /*   460 */   254,   68,  436,  252,   88,  237,  236,  235,  234,  233,
 /*   470 */   230,  436,  436,  436,  436,  436,  436,  256,  436,  154,
 /*   480 */   152,  151,  209,  221,  208,  203,  201,  196,  188,  168,
 /*   490 */   173,  164,  265,  264,  262,  258,  135,  255,  436,  254,
 /*   500 */    68,  436,  252,   88,  237,  236,  235,  234,  233,  230,
 /*   510 */   436,  436,  436,  436,  436,  436,  256,  436,  154,  152,
 /*   520 */   151,  197,  221,  208,  203,  201,  196,  188,  168,  173,
 /*   530 */   164,  265,  264,  262,  258,  135,  255,  436,  254,   68,
 /*   540 */   436,  252,   88,  237,  236,  235,  234,  233,  230,  436,
 /*   550 */   436,  436,  436,  436,  436,  256,  436,  154,  152,  151,
 /*   560 */   204,  221,  208,  203,  201,  196,  188,  168,  173,  164,
 /*   570 */   265,  264,  262,  258,  135,  255,  436,  254,   68,  436,
 /*   580 */   252,   88,  237,  236,  235,  234,  233,  230,  436,  436,
 /*   590 */   436,  436,  436,  436,  256,  436,  154,  152,  151,  132,
 /*   600 */   221,  208,  203,  201,  196,  188,  168,  173,  164,  265,
 /*   610 */   264,  262,  258,  135,  255,  436,  254,   68,  436,  252,
 /*   620 */    88,  237,  236,  235,  234,  233,  230,  436,  436,  436,
 /*   630 */   436,  436,  436,  256,  436,  154,  152,  151,  268,  221,
 /*   640 */   208,  203,  201,  196,  188,  168,  173,  164,  265,  264,
 /*   650 */   262,  258,  135,  255,  436,  254,   68,  436,  252,   88,
 /*   660 */   237,  236,  235,  234,  233,  230,  239,   88,  237,  236,
 /*   670 */   235,  234,  233,  230,  154,  152,  151,   50,   53,   52,
 /*   680 */    51,   49,   38,   41,   48,   46,   44,   42,   40,   45,
 /*   690 */    37,   39,   47,   43,   36,  193,  192,  191,  187,  185,
 /*   700 */   184,   27,   54,   48,   46,   44,   42,   40,   45,   37,
 /*   710 */    39,   47,   43,   36,   55,   56,   57,  436,   96,   16,
 /*   720 */   436,   32,  436,  229,  228,  227,  226,  224,  223,  220,
 /*   730 */   219,  215,  214,  436,  109,   54,  436,  172,  267,  136,
 /*   740 */   436,  436,   61,  273,   64,   66,  436,   55,   56,   57,
 /*   750 */   436,   96,   16,  436,   32,  436,  229,  228,  227,  226,
 /*   760 */   224,  223,  220,  219,  215,  214,  436,  109,   54,  436,
 /*   770 */   436,  158,  238,   88,  237,  236,  235,  234,  233,  230,
 /*   780 */    55,   56,   57,  436,   96,   16,  436,   32,  436,  229,
 /*   790 */   228,  227,  226,  224,  223,  220,  219,  215,  214,  436,
 /*   800 */   109,   54,   78,  436,  252,   88,  237,  236,  235,  234,
 /*   810 */   233,  230,  436,   55,   56,   57,  436,   92,   16,   93,
 /*   820 */    32,  436,  229,  228,  227,  226,  224,  223,  220,  219,
 /*   830 */   215,  214,  436,  109,  436,  436,  181,  182,  183,  205,
 /*   840 */   255,  436,  254,   68,  436,  252,   88,  237,  236,  235,
 /*   850 */   234,  233,  230,  436,  436,  436,  123,  202,  131,  199,
 /*   860 */   146,   53,   52,   51,   49,   38,   41,   48,   46,   44,
 /*   870 */    42,   40,   45,   37,   39,   47,   43,   36,   52,   51,
 /*   880 */    49,   38,   41,   48,   46,   44,   42,   40,   45,   37,
 /*   890 */    39,   47,   43,   36,   87,  261,  252,   88,  237,  236,
 /*   900 */   235,  234,  233,  230,  436,  436,  436,  135,  255,  436,
 /*   910 */   254,   68,  436,  252,   88,  237,  236,  235,  234,  104,
 /*   920 */   230,   77,  436,  252,   88,  237,  236,  235,  234,  233,
 /*   930 */   230,  142,   44,   42,   40,   45,   37,   39,   47,   43,
 /*   940 */    36,  436,  436,   26,  143,  436,  436,  436,   51,   49,
 /*   950 */    38,   41,   48,   46,   44,   42,   40,   45,   37,   39,
 /*   960 */    47,   43,   36,   49,   38,   41,   48,   46,   44,   42,
 /*   970 */    40,   45,   37,   39,   47,   43,   36,  211,  255,  436,
 /*   980 */   254,   68,  436,  252,   88,  237,  236,  235,  234,  233,
 /*   990 */   230,  212,  103,  245,   88,  237,  236,  235,  234,  233,
 /*  1000 */   230,  211,  255,  225,  254,   68,  436,  252,   88,  237,
 /*  1010 */   236,  235,  234,  233,  230,   94,  103,  172,  436,  136,
 /*  1020 */   181,  182,  183,  272,   64,  257,  255,  138,  254,   68,
 /*  1030 */   436,  252,   88,  237,  236,  235,  234,  233,  230,   38,
 /*  1040 */    41,   48,   46,   44,   42,   40,   45,   37,   39,   47,
 /*  1050 */    43,   36,  110,  255,  436,  254,   68,  436,  252,   88,
 /*  1060 */   237,  236,  235,  234,  233,  230,  232,  255,  436,  254,
 /*  1070 */    68,  436,  252,   88,  237,  236,  235,  234,  233,  230,
 /*  1080 */   119,  255,  436,  254,   68,  436,  252,   88,  237,  236,
 /*  1090 */   235,  234,  233,  230,  436,  144,  255,  436,  254,   68,
 /*  1100 */   436,  252,   88,  237,  236,  235,  234,  233,  230,  111,
 /*  1110 */   255,  436,  254,   68,  436,  252,   88,  237,  236,  235,
 /*  1120 */   234,  233,  230,  121,  255,  436,  254,   68,  436,  252,
 /*  1130 */    88,  237,  236,  235,  234,  233,  230,  186,  255,  436,
 /*  1140 */   254,   68,  436,  252,   88,  237,  236,  235,  234,  233,
 /*  1150 */   230,  436,  161,  255,  436,  254,   68,  436,  252,   88,
 /*  1160 */   237,  236,  235,  234,  233,  230,   95,  255,  436,  254,
 /*  1170 */    68,  436,  252,   88,  237,  236,  235,  234,  233,  230,
 /*  1180 */   107,  255,  436,  254,   68,  436,  252,   88,  237,  236,
 /*  1190 */   235,  234,  233,  230,  436,  137,  255,  436,  254,   68,
 /*  1200 */   436,  252,   88,  237,  236,  235,  234,  233,  230,  105,
 /*  1210 */   255,  436,  254,   68,  436,  252,   88,  237,  236,  235,
 /*  1220 */   234,  233,  230,  207,  255,  436,  254,   68,  436,  252,
 /*  1230 */    88,  237,  236,  235,  234,  233,  230,  153,  255,  436,
 /*  1240 */   254,   68,  436,  252,   88,  237,  236,  235,  234,  233,
 /*  1250 */   230,  180,  436,  254,   68,  436,  252,   88,  237,  236,
 /*  1260 */   235,  234,  233,  230,  253,  436,  254,   68,  436,  252,
 /*  1270 */    88,  237,  236,  235,  234,  233,  230,  178,  436,  252,
 /*  1280 */    88,  237,  236,  235,  234,  233,  230,   75,  436,  252,
 /*  1290 */    88,  237,  236,  235,  234,  233,  230,   85,  436,  252,
 /*  1300 */    88,  237,  236,  235,  234,  233,  230,   81,  436,  252,
 /*  1310 */    88,  237,  236,  235,  234,  233,  230,   84,  436,  252,
 /*  1320 */    88,  237,  236,  235,  234,  233,  230,   82,  436,  252,
 /*  1330 */    88,  237,  236,  235,  234,  233,  230,  436,   79,  436,
 /*  1340 */   252,   88,  237,  236,  235,  234,  233,  230,  179,  436,
 /*  1350 */   252,   88,  237,  236,  235,  234,  233,  230,   89,  436,
 /*  1360 */   252,   88,  237,  236,  235,  234,  233,  230,   76,  436,
 /*  1370 */   252,   88,  237,  236,  235,  234,  233,  230,   80,  436,
 /*  1380 */   252,   88,  237,  236,  235,  234,  233,  230,  174,  436,
 /*  1390 */   252,   88,  237,  236,  235,  234,  233,  230,   83,  436,
 /*  1400 */   252,   88,  237,  236,  235,  234,  233,  230,  228,  227,
 /*  1410 */   226,  224,  223,  220,  219,  215,  214,   90,  436,  252,
 /*  1420 */    88,  237,  236,  235,  234,  233,  230,  169,  436,  436,
 /*  1430 */   436,  436,   67,  436,  269,  270,  271,  127,  167,  436,
 /*  1440 */   436,  436,  436,   67,  436,  269,  270,  271,  127,
};
static const YYCODETYPE yy_lookahead[] = {
 /*     0 */    74,   75,   76,   77,   78,   79,   80,   81,   82,   83,
 /*    10 */    84,   85,   86,   87,   88,   89,   90,   91,   92,   93,
 /*    20 */    94,  109,   96,   97,   35,   99,  100,  101,  102,  103,
 /*    30 */   104,  105,  106,   15,   19,  123,   26,  125,  126,  127,
 /*    40 */   128,  115,  116,  117,   34,   27,   28,   29,   51,   31,
 /*    50 */    32,  115,   34,  117,   36,   37,   38,   39,   40,   41,
 /*    60 */    42,   43,   44,   45,   51,   47,   16,   17,   18,   51,
 /*    70 */    52,   53,   54,   55,   56,   57,   58,   59,  118,  119,
 /*    80 */    65,   63,   64,  132,  133,   15,   68,   69,   70,   46,
 /*    90 */   131,   73,   14,   15,   16,   17,   18,   27,   28,   29,
 /*   100 */    53,   31,   32,   51,   34,   51,   36,   37,   38,   39,
 /*   110 */    40,   41,   42,   43,   44,   45,  113,   47,   71,   72,
 /*   120 */    26,   51,   52,   53,   54,   55,   56,   57,   58,   59,
 /*   130 */   109,   46,   19,   63,   64,   31,  119,   15,   68,   69,
 /*   140 */    70,   27,   28,   73,   30,   65,   32,  126,   51,   27,
 /*   150 */    28,   29,   58,   31,   32,   31,   34,   63,   36,   37,
 /*   160 */    38,   39,   40,   41,   42,   43,   44,   45,   51,   47,
 /*   170 */    31,   19,  103,   51,   52,   42,   54,   55,   56,   57,
 /*   180 */    58,   59,   31,   31,   31,   63,   64,   48,   49,   50,
 /*   190 */    68,   69,   70,  109,   52,   73,  104,   74,   35,   98,
 /*   200 */    51,   78,   79,   80,   81,   82,   83,   84,   85,   86,
 /*   210 */    87,   88,   89,   90,   91,   92,   93,   94,  109,   96,
 /*   220 */    97,   51,   99,  100,  101,  102,  103,  104,  105,  106,
 /*   230 */    31,   34,  123,   51,  125,  126,  127,  128,  115,  116,
 /*   240 */   117,   97,   51,   99,  100,  101,  102,  103,  104,  105,
 /*   250 */   106,   33,   26,   34,   74,   67,   34,  134,   78,   79,
 /*   260 */    80,   81,   82,   83,   84,   85,   86,   87,   88,   89,
 /*   270 */    90,   91,   92,   93,   94,  109,   96,   97,   46,   99,
 /*   280 */   100,  101,  102,  103,  104,  105,  106,   52,   26,   26,
 /*   290 */    35,  125,  126,   51,  128,  115,  116,  117,   93,   94,
 /*   300 */    51,   96,   97,   50,   99,  100,  101,  102,  103,  104,
 /*   310 */   105,  106,   31,   74,  134,   31,  111,   34,   79,   80,
 /*   320 */    81,   82,   83,   84,   85,   86,   87,   88,   89,   90,
 /*   330 */    91,   92,   93,   94,   34,   96,   97,   46,   99,  100,
 /*   340 */   101,  102,  103,  104,  105,  106,   99,  100,  101,  102,
 /*   350 */   103,  104,  105,  106,  115,  116,  117,   35,   65,   35,
 /*   360 */    74,   35,   46,  124,   78,   79,   80,   81,   82,   83,
 /*   370 */    84,   85,   86,   87,   88,   89,   90,   91,   92,   93,
 /*   380 */    94,   31,   96,   97,   35,   99,  100,  101,  102,  103,
 /*   390 */   104,  105,  106,   33,   19,   19,   35,   31,   31,   74,
 /*   400 */    35,  115,  116,  117,   79,   80,   81,   82,   83,   84,
 /*   410 */    85,   86,   87,   88,   89,   90,   91,   92,   93,   94,
 /*   420 */    34,   96,   97,   35,   99,  100,  101,  102,  103,  104,
 /*   430 */   105,  106,   26,   46,   34,  135,  135,  135,   74,  135,
 /*   440 */   115,  116,  117,   79,   80,   81,   82,   83,   84,   85,
 /*   450 */    86,   87,   88,   89,   90,   91,   92,   93,   94,  135,
 /*   460 */    96,   97,  135,   99,  100,  101,  102,  103,  104,  105,
 /*   470 */   106,  135,  135,  135,  135,  135,  135,   74,  135,  115,
 /*   480 */   116,  117,   79,   80,   81,   82,   83,   84,   85,   86,
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
 /*   760 */    40,   41,   42,   43,   44,   45,  135,   47,   15,  135,
 /*   770 */   135,   51,   99,  100,  101,  102,  103,  104,  105,  106,
 /*   780 */    27,   28,   29,  135,   31,   32,  135,   34,  135,   36,
 /*   790 */    37,   38,   39,   40,   41,   42,   43,   44,   45,  135,
 /*   800 */    47,   15,   97,  135,   99,  100,  101,  102,  103,  104,
 /*   810 */   105,  106,  135,   27,   28,   29,  135,   31,   32,   31,
 /*   820 */    34,  135,   36,   37,   38,   39,   40,   41,   42,   43,
 /*   830 */    44,   45,  135,   47,  135,  135,   48,   49,   50,   93,
 /*   840 */    94,  135,   96,   97,  135,   99,  100,  101,  102,  103,
 /*   850 */   104,  105,  106,  135,  135,  135,  110,  111,  112,  113,
 /*   860 */   114,    2,    3,    4,    5,    6,    7,    8,    9,   10,
 /*   870 */    11,   12,   13,   14,   15,   16,   17,   18,    3,    4,
 /*   880 */     5,    6,    7,    8,    9,   10,   11,   12,   13,   14,
 /*   890 */    15,   16,   17,   18,   97,   81,   99,  100,  101,  102,
 /*   900 */   103,  104,  105,  106,  135,  135,  135,   93,   94,  135,
 /*   910 */    96,   97,  135,   99,  100,  101,  102,  103,  104,  105,
 /*   920 */   106,   97,  135,   99,  100,  101,  102,  103,  104,  105,
 /*   930 */   106,  117,   10,   11,   12,   13,   14,   15,   16,   17,
 /*   940 */    18,  135,  135,  129,  130,  135,  135,  135,    4,    5,
 /*   950 */     6,    7,    8,    9,   10,   11,   12,   13,   14,   15,
 /*   960 */    16,   17,   18,    5,    6,    7,    8,    9,   10,   11,
 /*   970 */    12,   13,   14,   15,   16,   17,   18,   93,   94,  135,
 /*   980 */    96,   97,  135,   99,  100,  101,  102,  103,  104,  105,
 /*   990 */   106,  107,  108,   99,  100,  101,  102,  103,  104,  105,
 /*  1000 */   106,   93,   94,   31,   96,   97,  135,   99,  100,  101,
 /*  1010 */   102,  103,  104,  105,  106,  107,  108,  115,  135,  117,
 /*  1020 */    48,   49,   50,  121,  122,   93,   94,   95,   96,   97,
 /*  1030 */   135,   99,  100,  101,  102,  103,  104,  105,  106,    6,
 /*  1040 */     7,    8,    9,   10,   11,   12,   13,   14,   15,   16,
 /*  1050 */    17,   18,   93,   94,  135,   96,   97,  135,   99,  100,
 /*  1060 */   101,  102,  103,  104,  105,  106,   93,   94,  135,   96,
 /*  1070 */    97,  135,   99,  100,  101,  102,  103,  104,  105,  106,
 /*  1080 */    93,   94,  135,   96,   97,  135,   99,  100,  101,  102,
 /*  1090 */   103,  104,  105,  106,  135,   93,   94,  135,   96,   97,
 /*  1100 */   135,   99,  100,  101,  102,  103,  104,  105,  106,   93,
 /*  1110 */    94,  135,   96,   97,  135,   99,  100,  101,  102,  103,
 /*  1120 */   104,  105,  106,   93,   94,  135,   96,   97,  135,   99,
 /*  1130 */   100,  101,  102,  103,  104,  105,  106,   93,   94,  135,
 /*  1140 */    96,   97,  135,   99,  100,  101,  102,  103,  104,  105,
 /*  1150 */   106,  135,   93,   94,  135,   96,   97,  135,   99,  100,
 /*  1160 */   101,  102,  103,  104,  105,  106,   93,   94,  135,   96,
 /*  1170 */    97,  135,   99,  100,  101,  102,  103,  104,  105,  106,
 /*  1180 */    93,   94,  135,   96,   97,  135,   99,  100,  101,  102,
 /*  1190 */   103,  104,  105,  106,  135,   93,   94,  135,   96,   97,
 /*  1200 */   135,   99,  100,  101,  102,  103,  104,  105,  106,   93,
 /*  1210 */    94,  135,   96,   97,  135,   99,  100,  101,  102,  103,
 /*  1220 */   104,  105,  106,   93,   94,  135,   96,   97,  135,   99,
 /*  1230 */   100,  101,  102,  103,  104,  105,  106,   93,   94,  135,
 /*  1240 */    96,   97,  135,   99,  100,  101,  102,  103,  104,  105,
 /*  1250 */   106,   94,  135,   96,   97,  135,   99,  100,  101,  102,
 /*  1260 */   103,  104,  105,  106,   94,  135,   96,   97,  135,   99,
 /*  1270 */   100,  101,  102,  103,  104,  105,  106,   97,  135,   99,
 /*  1280 */   100,  101,  102,  103,  104,  105,  106,   97,  135,   99,
 /*  1290 */   100,  101,  102,  103,  104,  105,  106,   97,  135,   99,
 /*  1300 */   100,  101,  102,  103,  104,  105,  106,   97,  135,   99,
 /*  1310 */   100,  101,  102,  103,  104,  105,  106,   97,  135,   99,
 /*  1320 */   100,  101,  102,  103,  104,  105,  106,   97,  135,   99,
 /*  1330 */   100,  101,  102,  103,  104,  105,  106,  135,   97,  135,
 /*  1340 */    99,  100,  101,  102,  103,  104,  105,  106,   97,  135,
 /*  1350 */    99,  100,  101,  102,  103,  104,  105,  106,   97,  135,
 /*  1360 */    99,  100,  101,  102,  103,  104,  105,  106,   97,  135,
 /*  1370 */    99,  100,  101,  102,  103,  104,  105,  106,   97,  135,
 /*  1380 */    99,  100,  101,  102,  103,  104,  105,  106,   97,  135,
 /*  1390 */    99,  100,  101,  102,  103,  104,  105,  106,   97,  135,
 /*  1400 */    99,  100,  101,  102,  103,  104,  105,  106,   37,   38,
 /*  1410 */    39,   40,   41,   42,   43,   44,   45,   97,  135,   99,
 /*  1420 */   100,  101,  102,  103,  104,  105,  106,   53,  135,  135,
 /*  1430 */   135,  135,   58,  135,   60,   61,   62,   63,   53,  135,
 /*  1440 */   135,  135,  135,   58,  135,   60,   61,   62,   63,
};
#define YY_SHIFT_USE_DFLT (-12)
#define YY_SHIFT_MAX 153
static const short yy_shift_ofst[] = {
 /*     0 */   122,  122,  122,   18,  122,   70,  122,  122,  122,  122,
 /*    10 */   122,  122,  122,  122,  786,  687,  753,  753,  753,  753,
 /*    20 */   720,  753,  753,  753,  753,  753,  753,  753,  753,  753,
 /*    30 */   753,  753,  753,  753,  753,  753,  753,  753,  753,  753,
 /*    40 */   753,  753,  753,  753,  753,  753,  753,  753,  753,  753,
 /*    50 */   753,  753,  753,  753,  753,  753,  753,  753,  788,  788,
 /*    60 */  1385, 1374,  788,  139,   94,   47,  199,  124,  676, 1371,
 /*    70 */   972,  153,  124,  104,  -12,  859,  875,  944,  958, 1033,
 /*    80 */   695,  695,  922,  922,   78,   78,   78,   78,  114,   50,
 /*    90 */    50,   15,   10,  152,  218,  226,  222,  235,  263,  255,
 /*   100 */   253,  283,  284,  291,  293,  326,  350,  360,  375,  366,
 /*   110 */   365,  388,  387,  400,  316,  406,  376,  386,  367,  361,
 /*   120 */   375,  349,  316,  324,  322,  300,  222,  281,  249,  242,
 /*   130 */   262,  232,  188,  219,  197,  182,  170,  149,  163,  142,
 /*   140 */   133,  151,  117,   80,   97,  113,   85,   52,   43,   -3,
 /*   150 */   191,   13,   54,  -11,
};
#define YY_REDUCE_USE_DFLT (-89)
#define YY_REDUCE_MAX 74
static const short yy_reduce_ofst[] = {
 /*     0 */   -74,  180,  123,  286,  239,  325,  325,  481,  520,  442,
 /*    10 */   364,  403,  325,  559,  746,  814,  908,  884,  932,  205,
 /*    20 */  1002, 1030, 1059, 1087, 1116, 1130, 1102, 1073, 1044, 1016,
 /*    30 */  1144,  987,  959,  973, 1157, 1170, 1291, 1320, 1281, 1261,
 /*    40 */  1220, 1210, 1200, 1180,  797,  144, 1230, 1251, 1301, 1241,
 /*    50 */  1190,  705,  824, 1271,  894,  247,  567,  673,  109,  -88,
 /*    60 */   622,  902,  166,   21,  -64,  -49,  -40,  -40,  101,   92,
 /*    70 */    84,   69,   17,    3,  -41,
};
static const YYACTIONTYPE yy_default[] = {
 /*     0 */   279,  434,  434,  434,  434,  434,  280,  434,  434,  434,
 /*    10 */   434,  434,  432,  434,  371,  434,  434,  434,  296,  434,
 /*    20 */   434,  434,  434,  434,  434,  434,  434,  434,  434,  434,
 /*    30 */   434,  434,  434,  434,  434,  434,  434,  434,  434,  434,
 /*    40 */   434,  434,  434,  434,  434,  434,  434,  434,  434,  434,
 /*    50 */   434,  434,  434,  434,  434,  434,  434,  434,  408,  408,
 /*    60 */   434,  434,  434,  434,  434,  434,  434,  434,  306,  434,
 /*    70 */   434,  434,  434,  434,  427,  309,  310,  311,  312,  313,
 /*    80 */   325,  326,  315,  314,  318,  317,  319,  316,  327,  321,
 /*    90 */   320,  387,  354,  405,  434,  434,  354,  434,  434,  434,
 /*   100 */   434,  434,  434,  356,  341,  434,  434,  434,  404,  434,
 /*   110 */   434,  434,  409,  434,  411,  434,  363,  434,  434,  434,
 /*   120 */   434,  434,  410,  434,  434,  434,  337,  434,  434,  434,
 /*   130 */   434,  372,  423,  434,  434,  434,  434,  434,  434,  434,
 /*   140 */   434,  434,  434,  434,  434,  387,  373,  434,  386,  434,
 /*   150 */   434,  434,  434,  434,  383,  385,  382,  381,  380,  379,
 /*   160 */   378,  388,  390,  389,  289,  377,  275,  391,  287,  392,
 /*   170 */   375,  393,  394,  288,  324,  395,  396,  397,  323,  322,
 /*   180 */   307,  360,  361,  362,  305,  304,  407,  303,  286,  403,
 /*   190 */   416,  302,  301,  300,  339,  338,  285,  420,  370,  369,
 /*   200 */   367,  284,  366,  283,  424,  365,  364,  368,  282,  425,
 /*   210 */   359,  358,  357,  355,  353,  352,  426,  428,  429,  351,
 /*   220 */   350,  281,  278,  349,  348,  363,  347,  346,  345,  344,
 /*   230 */   343,  342,  406,  341,  340,  336,  333,  332,  331,  330,
 /*   240 */   415,  413,  414,  329,  412,  328,  335,  433,  430,  334,
 /*   250 */   277,  431,  308,  299,  298,  295,  294,  297,  293,  276,
 /*   260 */   417,  418,  292,  419,  291,  290,  384,  374,  421,  398,
 /*   270 */   399,  400,  402,  401,  376,
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
 /*  90 */ "positional_argument ::= expression",
 /*  91 */ "positional_argument_list ::= positional_argument",
 /*  92 */ "positional_argument_list ::= positional_argument_list COMMA positional_argument",
 /*  93 */ "named_argument ::= IDENTIFIER COLON expression",
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
#line 1280 "cscript.c"
        break;
      case 1:
#line 78 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(translation_unit, yymsp[0].minor.yy31); }
#line 1285 "cscript.c"
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
#line 1323 "cscript.c"
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
#line 1337 "cscript.c"
        break;
      case 4:
      case 21:
#line 95 "cscript.in"
{ yygotominor.yy31 = 0; }
#line 1343 "cscript.c"
        break;
      case 19:
      case 99:
#line 114 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(empty_statement); }
#line 1349 "cscript.c"
        break;
      case 24:
#line 130 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(assignment_expression, yymsp[-1].minor.yy144, yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1354 "cscript.c"
        break;
      case 25:
#line 134 "cscript.in"
{ yygotominor.yy144 = op_assign; }
#line 1359 "cscript.c"
        break;
      case 26:
#line 135 "cscript.in"
{ yygotominor.yy144 = op_assadd; }
#line 1364 "cscript.c"
        break;
      case 27:
#line 136 "cscript.in"
{ yygotominor.yy144 = op_asssub; }
#line 1369 "cscript.c"
        break;
      case 28:
#line 137 "cscript.in"
{ yygotominor.yy144 = op_assmul; }
#line 1374 "cscript.c"
        break;
      case 29:
#line 138 "cscript.in"
{ yygotominor.yy144 = op_assdiv; }
#line 1379 "cscript.c"
        break;
      case 30:
#line 139 "cscript.in"
{ yygotominor.yy144 = op_assmod; }
#line 1384 "cscript.c"
        break;
      case 32:
#line 143 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(ternary_expression, yymsp[-4].minor.yy31, yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1389 "cscript.c"
        break;
      case 34:
#line 147 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(binary_expression, op_logor,   yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1394 "cscript.c"
        break;
      case 35:
#line 148 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(binary_expression, op_logand,  yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1399 "cscript.c"
        break;
      case 36:
#line 149 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(binary_expression, op_bitor,   yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1404 "cscript.c"
        break;
      case 37:
#line 150 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(binary_expression, op_bitxor,  yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1409 "cscript.c"
        break;
      case 38:
#line 151 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(binary_expression, op_bitand,  yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1414 "cscript.c"
        break;
      case 39:
#line 152 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(binary_expression, op_eq,   yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1419 "cscript.c"
        break;
      case 40:
#line 153 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(binary_expression, op_ne,   yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1424 "cscript.c"
        break;
      case 41:
#line 154 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(binary_expression, op_lt,   yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1429 "cscript.c"
        break;
      case 42:
#line 155 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(binary_expression, op_le,   yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1434 "cscript.c"
        break;
      case 43:
#line 156 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(binary_expression, op_gt,   yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1439 "cscript.c"
        break;
      case 44:
#line 157 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(binary_expression, op_ge,   yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1444 "cscript.c"
        break;
      case 45:
#line 158 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(binary_expression, op_add,  yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1449 "cscript.c"
        break;
      case 46:
#line 159 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(binary_expression, op_sub,  yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1454 "cscript.c"
        break;
      case 47:
#line 160 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(binary_expression, op_mul,  yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1459 "cscript.c"
        break;
      case 48:
#line 161 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(binary_expression, op_div,  yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1464 "cscript.c"
        break;
      case 49:
#line 162 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(binary_expression, op_mod,  yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1469 "cscript.c"
        break;
      case 50:
#line 163 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(binary_expression, op_seq,  yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1474 "cscript.c"
        break;
      case 51:
#line 164 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(binary_expression, op_sne,  yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1479 "cscript.c"
        break;
      case 53:
#line 168 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(prefix_expression, op_negate, yymsp[0].minor.yy31); }
#line 1484 "cscript.c"
        break;
      case 54:
#line 169 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(prefix_expression, op_preinc, yymsp[0].minor.yy31); }
#line 1489 "cscript.c"
        break;
      case 55:
#line 170 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(prefix_expression, op_predec, yymsp[0].minor.yy31); }
#line 1494 "cscript.c"
        break;
      case 56:
#line 171 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(prefix_expression, op_not,    yymsp[0].minor.yy31); }
#line 1499 "cscript.c"
        break;
      case 59:
#line 176 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(postfix_expression, op_postinc, yymsp[-1].minor.yy31); }
#line 1504 "cscript.c"
        break;
      case 60:
#line 177 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(postfix_expression, op_postdec, yymsp[-1].minor.yy31); }
#line 1509 "cscript.c"
        break;
      case 62:
#line 179 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(member_expression, yymsp[-2].minor.yy31, String(yymsp[0].minor.yy0)); }
#line 1514 "cscript.c"
        break;
      case 63:
#line 180 "cscript.in"
{ yygotominor.yy31 = yymsp[0].minor.yy31; yymsp[0].minor.yy31->m_a3 = yymsp[-2].minor.yy31; }
#line 1519 "cscript.c"
        break;
      case 64:
#line 181 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(index_expression, yymsp[-3].minor.yy31, yymsp[-1].minor.yy31); }
#line 1524 "cscript.c"
        break;
      case 67:
      case 109:
      case 110:
      case 144:
#line 186 "cscript.in"
{ yygotominor.yy31 = yymsp[-1].minor.yy31; }
#line 1532 "cscript.c"
        break;
      case 69:
#line 188 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(this_expression); }
#line 1537 "cscript.c"
        break;
      case 70:
#line 191 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(literal_value, Variant(String(yymsp[0].minor.yy0), Variant::stInt)); }
#line 1542 "cscript.c"
        break;
      case 71:
#line 192 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(literal_value, Variant(hex2dec(String(yymsp[0].minor.yy0).c_str()))); }
#line 1547 "cscript.c"
        break;
      case 72:
#line 193 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(literal_value, Variant(bin2dec(String(yymsp[0].minor.yy0).c_str()))); }
#line 1552 "cscript.c"
        break;
      case 73:
#line 194 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(literal_value, Variant(rom2dec(String(yymsp[0].minor.yy0).c_str()))); }
#line 1557 "cscript.c"
        break;
      case 74:
#line 195 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(literal_value, Variant(String(yymsp[0].minor.yy0), Variant::stReal)); }
#line 1562 "cscript.c"
        break;
      case 75:
#line 196 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(literal_value, Variant(String(yymsp[0].minor.yy0), Variant::stString)); }
#line 1567 "cscript.c"
        break;
      case 76:
#line 197 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(literal_value, Variant(true));    }
#line 1572 "cscript.c"
        break;
      case 77:
#line 198 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(literal_value, Variant(false));   }
#line 1577 "cscript.c"
        break;
      case 78:
#line 199 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(literal_value, Variant());        }
#line 1582 "cscript.c"
        break;
      case 79:
#line 202 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(lvalue, String(yymsp[0].minor.yy0)); }
#line 1587 "cscript.c"
        break;
      case 80:
#line 205 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(list_literal, yymsp[-1].minor.yy31); }
#line 1592 "cscript.c"
        break;
      case 81:
#line 206 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(list_content, yymsp[0].minor.yy31); }
#line 1597 "cscript.c"
        break;
      case 82:
#line 207 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(list_content, yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1602 "cscript.c"
        break;
      case 83:
#line 209 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(list_entry, yymsp[0].minor.yy31); }
#line 1607 "cscript.c"
        break;
      case 84:
#line 212 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(new_expression, String(yymsp[0].minor.yy0)); }
#line 1612 "cscript.c"
        break;
      case 85:
#line 219 "cscript.in"
{ yygotominor.yy31 = new Ast(builtin_type, Variant::stBool);    }
#line 1617 "cscript.c"
        break;
      case 86:
#line 220 "cscript.in"
{ yygotominor.yy31 = new Ast(builtin_type, Variant::stInt);     }
#line 1622 "cscript.c"
        break;
      case 87:
#line 221 "cscript.in"
{ yygotominor.yy31 = new Ast(builtin_type, Variant::stString);  }
#line 1627 "cscript.c"
        break;
      case 88:
#line 222 "cscript.in"
{ yygotominor.yy31 = new Ast(class_type, String(yymsp[0].minor.yy0));            }
#line 1632 "cscript.c"
        break;
      case 89:
#line 230 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(function_call, String(yymsp[-3].minor.yy0), yymsp[-1].minor.yy31); }
#line 1637 "cscript.c"
        break;
      case 91:
      case 94:
      case 137:
      case 139:
#line 237 "cscript.in"
{ yygotominor.yy235 = new AstList; yygotominor.yy235->push_back(yymsp[0].minor.yy31); }
#line 1645 "cscript.c"
        break;
      case 92:
      case 95:
      case 138:
      case 140:
#line 238 "cscript.in"
{ yygotominor.yy235 = yymsp[-2].minor.yy235; yymsp[-2].minor.yy235->push_back(yymsp[0].minor.yy31); }
#line 1653 "cscript.c"
        break;
      case 93:
#line 241 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(named_argument, String(yymsp[-2].minor.yy0), yymsp[0].minor.yy31); }
#line 1658 "cscript.c"
        break;
      case 96:
#line 249 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(positional_arguments, new AstList); }
#line 1663 "cscript.c"
        break;
      case 97:
#line 250 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(positional_arguments, yymsp[0].minor.yy235); }
#line 1668 "cscript.c"
        break;
      case 98:
#line 251 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(named_arguments,      yymsp[0].minor.yy235); }
#line 1673 "cscript.c"
        break;
      case 100:
#line 260 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(expression_statement, yymsp[-1].minor.yy31); }
#line 1678 "cscript.c"
        break;
      case 101:
#line 263 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(compound_statement); }
#line 1683 "cscript.c"
        break;
      case 102:
#line 264 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(compound_statement, yymsp[-1].minor.yy31); }
#line 1688 "cscript.c"
        break;
      case 103:
#line 267 "cscript.in"
{ p->Parse(yymsp[-1].minor.yy0); yygotominor.yy31 = p->GetRoot(); }
#line 1693 "cscript.c"
        break;
      case 104:
#line 270 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(return_statement, yymsp[-1].minor.yy31); }
#line 1698 "cscript.c"
        break;
      case 105:
#line 271 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(return_statement);    }
#line 1703 "cscript.c"
        break;
      case 106:
#line 274 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(break_statement); }
#line 1708 "cscript.c"
        break;
      case 107:
#line 275 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(continue_statement); }
#line 1713 "cscript.c"
        break;
      case 112:
#line 289 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(variable_declaration, String(yymsp[0].minor.yy0));    }
#line 1718 "cscript.c"
        break;
      case 113:
#line 290 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(variable_declaration, String(yymsp[-2].minor.yy0), yymsp[0].minor.yy31); }
#line 1723 "cscript.c"
        break;
      case 115:
#line 293 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(declaration_sequence, yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1728 "cscript.c"
        break;
      case 116:
#line 300 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(class_declaration, String(yymsp[-2].minor.yy0)); }
#line 1733 "cscript.c"
        break;
      case 117:
#line 301 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(class_declaration, String(yymsp[-3].minor.yy0), yymsp[-1].minor.yy235); }
#line 1738 "cscript.c"
        break;
      case 118:
#line 304 "cscript.in"
{ yygotominor.yy31 = yymsp[-1].minor.yy31; yymsp[-1].minor.yy31->m_props["access"] = accessDefault; }
#line 1743 "cscript.c"
        break;
      case 119:
#line 305 "cscript.in"
{ yygotominor.yy31 = yymsp[0].minor.yy31; yymsp[0].minor.yy31->m_props["access"] = accessDefault; }
#line 1748 "cscript.c"
        break;
      case 120:
#line 306 "cscript.in"
{ yygotominor.yy31 = yymsp[-1].minor.yy31; yymsp[-1].minor.yy31->m_props["access"] = yymsp[-2].minor.yy78; }
#line 1753 "cscript.c"
        break;
      case 121:
#line 307 "cscript.in"
{ yygotominor.yy31 = yymsp[0].minor.yy31; yymsp[0].minor.yy31->m_props["access"] = yymsp[-1].minor.yy78; }
#line 1758 "cscript.c"
        break;
      case 122:
#line 308 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(access_specifier, yymsp[-1].minor.yy78); }
#line 1763 "cscript.c"
        break;
      case 123:
#line 312 "cscript.in"
{ yygotominor.yy78 = accessPrivate;   }
#line 1768 "cscript.c"
        break;
      case 124:
#line 313 "cscript.in"
{ yygotominor.yy78 = accessProtected; }
#line 1773 "cscript.c"
        break;
      case 125:
#line 314 "cscript.in"
{ yygotominor.yy78 = accessPublic;    }
#line 1778 "cscript.c"
        break;
      case 126:
#line 318 "cscript.in"
{ 
  yygotominor.yy235 = new AstList;
  yygotominor.yy235->push_back(yymsp[0].minor.yy31);
}
#line 1786 "cscript.c"
        break;
      case 127:
#line 322 "cscript.in"
{ 
  yygotominor.yy235 = yymsp[-1].minor.yy235;
  yygotominor.yy235->push_back(yymsp[0].minor.yy31);
}
#line 1794 "cscript.c"
        break;
      case 128:
#line 333 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(function_declaration, String(yymsp[-4].minor.yy0), yymsp[-2].minor.yy235, yymsp[0].minor.yy31); }
#line 1799 "cscript.c"
        break;
      case 129:
#line 336 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(parameter, String(yymsp[0].minor.yy0), yymsp[-1].minor.yy31); }
#line 1804 "cscript.c"
        break;
      case 130:
#line 337 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(parameter, String(yymsp[0].minor.yy0)); }
#line 1809 "cscript.c"
        break;
      case 131:
#line 340 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(parameter, String(yymsp[-2].minor.yy0), yymsp[-3].minor.yy31,         yymsp[0].minor.yy31); }
#line 1814 "cscript.c"
        break;
      case 132:
#line 341 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(parameter, String(yymsp[-2].minor.yy0), AstData(), yymsp[0].minor.yy31); }
#line 1819 "cscript.c"
        break;
      case 133:
      case 152:
#line 345 "cscript.in"
{ yygotominor.yy235 = new AstList; }
#line 1825 "cscript.c"
        break;
      case 134:
      case 135:
#line 346 "cscript.in"
{ yygotominor.yy235 = yymsp[0].minor.yy235; }
#line 1831 "cscript.c"
        break;
      case 136:
#line 348 "cscript.in"
{ yygotominor.yy235 = yymsp[-2].minor.yy235; yymsp[-2].minor.yy235->adopt(*yymsp[0].minor.yy235); }
#line 1836 "cscript.c"
        break;
      case 142:
#line 370 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(for_statement, yymsp[-5].minor.yy31, yymsp[-4].minor.yy31, yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1841 "cscript.c"
        break;
      case 145:
      case 146:
#line 381 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(foreach_statement, yymsp[-4].minor.yy31, yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1847 "cscript.c"
        break;
      case 147:
#line 383 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(variable_declaration, String(yymsp[0].minor.yy0)); }
#line 1852 "cscript.c"
        break;
      case 148:
#line 394 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(if_statement, yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1857 "cscript.c"
        break;
      case 149:
#line 395 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(if_statement, yymsp[-4].minor.yy31, yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1862 "cscript.c"
        break;
      case 150:
#line 403 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(while_statement, yymsp[-2].minor.yy31,  yymsp[0].minor.yy31); }
#line 1867 "cscript.c"
        break;
      case 151:
#line 411 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(switch_statement, yymsp[-4].minor.yy31, yymsp[-1].minor.yy235); }
#line 1872 "cscript.c"
        break;
      case 153:
      case 154:
#line 416 "cscript.in"
{ yygotominor.yy235 = yymsp[-1].minor.yy235; yygotominor.yy235->push_back(yymsp[0].minor.yy31); }
#line 1878 "cscript.c"
        break;
      case 155:
#line 420 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(switch_case, yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1883 "cscript.c"
        break;
      case 156:
#line 423 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(default_case, yymsp[0].minor.yy31); }
#line 1888 "cscript.c"
        break;
      case 158:
#line 435 "cscript.in"
{ 
  yygotominor.yy31 = new Ast(extern_declaration, String(yymsp[-4].minor.yy0), String(yymsp[-6].minor.yy0), yymsp[-5].minor.yy31, yymsp[-2].minor.yy235); 
}
#line 1895 "cscript.c"
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
#line 1943 "cscript.c"
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
#line 1961 "cscript.c"
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

