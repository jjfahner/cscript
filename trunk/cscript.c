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
 /*     0 */   256,  435,  166,  259,    6,  250,  246,  233,  228,  226,
 /*    10 */   221,  188,  168,  173,  164,  265,  264,  262,  258,  135,
 /*    20 */   255,  115,  254,   68,   13,  252,   88,  237,  236,  235,
 /*    30 */   232,  231,  227,   54,  156,  101,   24,  212,  210,  132,
 /*    40 */    99,  154,  152,  151,   14,   56,   55,   57,   72,   96,
 /*    50 */    17,  176,   28,  129,  225,  224,  223,  220,  219,  218,
 /*    60 */   217,  216,  214,  213,  155,  119,   65,  121,  157,  267,
 /*    70 */     3,  274,  140,   20,  147,  149,   67,  141,   54,  148,
 /*    80 */   163,  127,  103,  100,  181,  182,  183,  106,  112,  118,
 /*    90 */    56,   55,   57,  266,   96,   17,   22,   28,   73,  225,
 /*   100 */   224,  223,  220,  219,  218,  217,  216,  214,  213,  197,
 /*   110 */   119,   36,   37,   45,  267,    3,  165,  140,   20,  147,
 /*   120 */   149,   67,  141,   54,  177,   22,  127,  103,  100,  242,
 /*   130 */   243,  117,  106,  112,  118,   56,   55,   57,  241,   96,
 /*   140 */    17,  159,   28,  423,  225,  224,  223,  220,  219,  218,
 /*   150 */   217,  216,  214,  213,  162,  119,   67,   69,  130,  267,
 /*   160 */     3,  127,  140,   20,  147,  149,   67,  141,  172,   31,
 /*   170 */   136,  127,  103,  100,  272,   64,  256,  106,  112,  118,
 /*   180 */    12,  250,  246,  233,  228,  226,  221,  188,  168,  173,
 /*   190 */   164,  265,  264,  262,  258,  135,  255,  115,  254,   68,
 /*   200 */    30,  252,   88,  237,  236,  235,  232,  231,  227,  145,
 /*   210 */   263,   97,  363,  212,  210,  132,   99,  154,  152,  151,
 /*   220 */   257,  255,  138,  254,   68,  196,  252,   88,  237,  236,
 /*   230 */   235,  232,  231,  227,  256,  139,  251,  126,   12,  250,
 /*   240 */   246,  233,  228,  226,  221,  188,  168,  173,  164,  265,
 /*   250 */   264,  262,  258,  135,  255,  113,  254,   68,  150,  252,
 /*   260 */    88,  237,  236,  235,  232,  231,  227,   39,   38,   36,
 /*   270 */    37,   45,  208,  256,  105,  154,  152,  151,  190,  246,
 /*   280 */   233,  228,  226,  221,  188,  168,  173,  164,  265,  264,
 /*   290 */   262,  258,  135,  255,  248,  254,   68,   60,  252,   88,
 /*   300 */   237,  236,  235,  232,  231,  227,  239,   88,  237,  236,
 /*   310 */   235,  232,  231,  227,  154,  152,  151,  128,   10,   34,
 /*   320 */   256,   18,  171,  189,    5,  250,  246,  233,  228,  226,
 /*   330 */   221,  188,  168,  173,  164,  265,  264,  262,  258,  135,
 /*   340 */   255,  115,  254,   68,   92,  252,   88,  237,  236,  235,
 /*   350 */   232,  231,  227,  170,  160,   63,  211,  209,  210,  256,
 /*   360 */   134,  154,  152,  151,  247,  246,  233,  228,  226,  221,
 /*   370 */   188,  168,  173,  164,  265,  264,  262,  258,  135,  255,
 /*   380 */     9,  254,   68,   35,  252,   88,  237,  236,  235,  232,
 /*   390 */   231,  227,  249,  245,   62,   71,   14,   33,  256,   19,
 /*   400 */   154,  152,  151,  260,  246,  233,  228,  226,  221,  188,
 /*   410 */   168,  173,  164,  265,  264,  262,  258,  135,  255,   98,
 /*   420 */   254,   68,    2,  252,   88,  237,  236,  235,  232,  231,
 /*   430 */   227,  215,  175,    1,   15,  125,    4,  256,   26,  154,
 /*   440 */   152,  151,  234,  246,  233,  228,  226,  221,  188,  168,
 /*   450 */   173,  164,  265,  264,  262,  258,  135,  255,   59,  254,
 /*   460 */    68,   70,  252,   88,  237,  236,  235,  232,  231,  227,
 /*   470 */   203,   16,   25,  107,   74,   30,  256,   27,  154,  152,
 /*   480 */   151,  222,  246,  233,  228,  226,  221,  188,  168,  173,
 /*   490 */   164,  265,  264,  262,  258,  135,  255,   58,  254,   68,
 /*   500 */   122,  252,   88,  237,  236,  235,  232,  231,  227,    8,
 /*   510 */   205,  195,    7,   24,  123,  256,   29,  154,  152,  151,
 /*   520 */   229,  246,  233,  228,  226,  221,  188,  168,  173,  164,
 /*   530 */   265,  264,  262,  258,  135,  255,   11,  254,   68,   32,
 /*   540 */   252,   88,  237,  236,  235,  232,  231,  227,  111,  230,
 /*   550 */   436,  436,  436,  436,  256,  436,  154,  152,  151,  110,
 /*   560 */   246,  233,  228,  226,  221,  188,  168,  173,  164,  265,
 /*   570 */   264,  262,  258,  135,  255,  436,  254,   68,  436,  252,
 /*   580 */    88,  237,  236,  235,  232,  231,  227,  436,  436,  436,
 /*   590 */   436,  436,  436,  256,  436,  154,  152,  151,  268,  246,
 /*   600 */   233,  228,  226,  221,  188,  168,  173,  164,  265,  264,
 /*   610 */   262,  258,  135,  255,  436,  254,   68,  436,  252,   88,
 /*   620 */   237,  236,  235,  232,  231,  227,  240,   88,  237,  236,
 /*   630 */   235,  232,  231,  227,  154,  152,  151,   44,   53,   40,
 /*   640 */    52,   51,   50,   48,   49,   41,   47,   46,   43,   42,
 /*   650 */    39,   38,   36,   37,   45,  194,  192,  191,  187,  185,
 /*   660 */   184,   23,   54,   49,   41,   47,   46,   43,   42,   39,
 /*   670 */    38,   36,   37,   45,   56,   55,   57,  436,   96,   17,
 /*   680 */   436,   28,  436,  225,  224,  223,  220,  219,  218,  217,
 /*   690 */   216,  214,  213,  436,  119,   54,  436,  172,  267,  136,
 /*   700 */   436,  436,   61,  273,   64,   66,  436,   56,   55,   57,
 /*   710 */   436,   96,   17,  436,   28,  436,  225,  224,  223,  220,
 /*   720 */   219,  218,  217,  216,  214,  213,  436,  119,   54,  436,
 /*   730 */   436,  158,  238,   88,  237,  236,  235,  232,  231,  227,
 /*   740 */    56,   55,   57,  436,   96,   17,  436,   28,  436,  225,
 /*   750 */   224,  223,  220,  219,  218,  217,  216,  214,  213,  436,
 /*   760 */   119,   54,  174,  436,  252,   88,  237,  236,  235,  232,
 /*   770 */   231,  227,  436,   56,   55,   57,  436,   91,   17,   93,
 /*   780 */    28,  436,  225,  224,  223,  220,  219,  218,  217,  216,
 /*   790 */   214,  213,  436,  119,  436,  436,  181,  182,  183,  202,
 /*   800 */   255,  436,  254,   68,  436,  252,   88,  237,  236,  235,
 /*   810 */   232,  231,  227,  436,  436,  436,  124,  201,  131,  198,
 /*   820 */   146,   53,   40,   52,   51,   50,   48,   49,   41,   47,
 /*   830 */    46,   43,   42,   39,   38,   36,   37,   45,   40,   52,
 /*   840 */    51,   50,   48,   49,   41,   47,   46,   43,   42,   39,
 /*   850 */    38,   36,   37,   45,   79,  261,  252,   88,  237,  236,
 /*   860 */   235,  232,  231,  227,  436,  436,  436,  135,  255,  436,
 /*   870 */   254,   68,  436,  252,   88,  237,  236,  235,  232,  102,
 /*   880 */   227,   75,  436,  252,   88,  237,  236,  235,  232,  231,
 /*   890 */   227,  142,   47,   46,   43,   42,   39,   38,   36,   37,
 /*   900 */    45,  436,  436,   21,  143,  436,  436,  436,   52,   51,
 /*   910 */    50,   48,   49,   41,   47,   46,   43,   42,   39,   38,
 /*   920 */    36,   37,   45,   51,   50,   48,   49,   41,   47,   46,
 /*   930 */    43,   42,   39,   38,   36,   37,   45,  206,  255,  436,
 /*   940 */   254,   68,  436,  252,   88,  237,  236,  235,  232,  231,
 /*   950 */   227,   94,  104,  244,   88,  237,  236,  235,  232,  231,
 /*   960 */   227,  206,  255,  193,  254,   68,  436,  252,   88,  237,
 /*   970 */   236,  235,  232,  231,  227,  207,  104,  436,  436,  436,
 /*   980 */   181,  182,  183,  436,  436,  202,  255,  436,  254,   68,
 /*   990 */   436,  252,   88,  237,  236,  235,  232,  231,  227,  436,
 /*  1000 */   436,  436,  436,  199,   50,   48,   49,   41,   47,   46,
 /*  1010 */    43,   42,   39,   38,   36,   37,   45,  116,  255,  436,
 /*  1020 */   254,   68,  436,  252,   88,  237,  236,  235,  232,  231,
 /*  1030 */   227,  109,  255,  436,  254,   68,  436,  252,   88,  237,
 /*  1040 */   236,  235,  232,  231,  227,  153,  255,  436,  254,   68,
 /*  1050 */   436,  252,   88,  237,  236,  235,  232,  231,  227,  144,
 /*  1060 */   255,  436,  254,   68,  436,  252,   88,  237,  236,  235,
 /*  1070 */   232,  231,  227,  436,  436,  436,  436,  436,  436,  120,
 /*  1080 */   255,  436,  254,   68,  436,  252,   88,  237,  236,  235,
 /*  1090 */   232,  231,  227,  436,  436,  436,  436,  137,  255,  436,
 /*  1100 */   254,   68,  436,  252,   88,  237,  236,  235,  232,  231,
 /*  1110 */   227,  114,  255,  436,  254,   68,  436,  252,   88,  237,
 /*  1120 */   236,  235,  232,  231,  227,  161,  255,  436,  254,   68,
 /*  1130 */   436,  252,   88,  237,  236,  235,  232,  231,  227,  108,
 /*  1140 */   255,  436,  254,   68,  436,  252,   88,  237,  236,  235,
 /*  1150 */   232,  231,  227,   95,  255,  436,  254,   68,  436,  252,
 /*  1160 */    88,  237,  236,  235,  232,  231,  227,  436,  436,  436,
 /*  1170 */   436,  436,  436,  133,  255,  436,  254,   68,  436,  252,
 /*  1180 */    88,  237,  236,  235,  232,  231,  227,  436,  436,  436,
 /*  1190 */   436,  204,  255,  436,  254,   68,  436,  252,   88,  237,
 /*  1200 */   236,  235,  232,  231,  227,  200,  255,  436,  254,   68,
 /*  1210 */   436,  252,   88,  237,  236,  235,  232,  231,  227,  186,
 /*  1220 */   255,  436,  254,   68,  436,  252,   88,  237,  236,  235,
 /*  1230 */   232,  231,  227,  253,  436,  254,   68,  436,  252,   88,
 /*  1240 */   237,  236,  235,  232,  231,  227,  180,  436,  254,   68,
 /*  1250 */   436,  252,   88,  237,  236,  235,  232,  231,  227,   78,
 /*  1260 */   436,  252,   88,  237,  236,  235,  232,  231,  227,  436,
 /*  1270 */   436,   86,  436,  252,   88,  237,  236,  235,  232,  231,
 /*  1280 */   227,   80,  436,  252,   88,  237,  236,  235,  232,  231,
 /*  1290 */   227,  436,   85,  436,  252,   88,  237,  236,  235,  232,
 /*  1300 */   231,  227,   82,  436,  252,   88,  237,  236,  235,  232,
 /*  1310 */   231,  227,   77,  436,  252,   88,  237,  236,  235,  232,
 /*  1320 */   231,  227,  436,   87,  436,  252,   88,  237,  236,  235,
 /*  1330 */   232,  231,  227,   83,  436,  252,   88,  237,  236,  235,
 /*  1340 */   232,  231,  227,   84,  436,  252,   88,  237,  236,  235,
 /*  1350 */   232,  231,  227,   89,  436,  252,   88,  237,  236,  235,
 /*  1360 */   232,  231,  227,   76,  436,  252,   88,  237,  236,  235,
 /*  1370 */   232,  231,  227,   90,  436,  252,   88,  237,  236,  235,
 /*  1380 */   232,  231,  227,  179,  436,  252,   88,  237,  236,  235,
 /*  1390 */   232,  231,  227,   81,  436,  252,   88,  237,  236,  235,
 /*  1400 */   232,  231,  227,  224,  223,  220,  219,  218,  217,  216,
 /*  1410 */   214,  213,  178,  436,  252,   88,  237,  236,  235,  232,
 /*  1420 */   231,  227,  169,  436,  436,  436,  436,   67,  436,  269,
 /*  1430 */   270,  271,  127,  167,  436,  436,  436,  436,   67,  436,
 /*  1440 */   269,  270,  271,  127,
};
static const YYCODETYPE yy_lookahead[] = {
 /*     0 */    74,   75,   76,   77,   78,   79,   80,   81,   82,   83,
 /*    10 */    84,   85,   86,   87,   88,   89,   90,   91,   92,   93,
 /*    20 */    94,  109,   96,   97,   35,   99,  100,  101,  102,  103,
 /*    30 */   104,  105,  106,   15,   51,  123,   26,  125,  126,  127,
 /*    40 */   128,  115,  116,  117,   34,   27,   28,   29,   46,   31,
 /*    50 */    32,  115,   34,  117,   36,   37,   38,   39,   40,   41,
 /*    60 */    42,   43,   44,   45,   51,   47,  131,   31,   51,   51,
 /*    70 */    52,   53,   54,   55,   56,   57,   58,   59,   15,  118,
 /*    80 */   119,   63,   64,   65,   48,   49,   50,   69,   70,   71,
 /*    90 */    27,   28,   29,   51,   31,   32,   19,   34,   46,   36,
 /*   100 */    37,   38,   39,   40,   41,   42,   43,   44,   45,  113,
 /*   110 */    47,   16,   17,   18,   51,   52,   53,   54,   55,   56,
 /*   120 */    57,   58,   59,   15,   26,   19,   63,   64,   65,  132,
 /*   130 */   133,   31,   69,   70,   71,   27,   28,   29,   53,   31,
 /*   140 */    32,   51,   34,   66,   36,   37,   38,   39,   40,   41,
 /*   150 */    42,   43,   44,   45,  119,   47,   58,   72,   73,   51,
 /*   160 */    52,   63,   54,   55,   56,   57,   58,   59,  115,   66,
 /*   170 */   117,   63,   64,   65,  121,  122,   74,   69,   70,   71,
 /*   180 */    78,   79,   80,   81,   82,   83,   84,   85,   86,   87,
 /*   190 */    88,   89,   90,   91,   92,   93,   94,  109,   96,   97,
 /*   200 */    19,   99,  100,  101,  102,  103,  104,  105,  106,   31,
 /*   210 */    51,  123,   31,  125,  126,  127,  128,  115,  116,  117,
 /*   220 */    93,   94,   95,   96,   97,  103,   99,  100,  101,  102,
 /*   230 */   103,  104,  105,  106,   74,   31,  134,   31,   78,   79,
 /*   240 */    80,   81,   82,   83,   84,   85,   86,   87,   88,   89,
 /*   250 */    90,   91,   92,   93,   94,  109,   96,   97,   42,   99,
 /*   260 */   100,  101,  102,  103,  104,  105,  106,   14,   15,   16,
 /*   270 */    17,   18,  126,   74,  109,  115,  116,  117,   79,   80,
 /*   280 */    81,   82,   83,   84,   85,   86,   87,   88,   89,   90,
 /*   290 */    91,   92,   93,   94,  134,   96,   97,   52,   99,  100,
 /*   300 */   101,  102,  103,  104,  105,  106,   99,  100,  101,  102,
 /*   310 */   103,  104,  105,  106,  115,  116,  117,  104,   35,   98,
 /*   320 */    74,   51,   51,  124,   78,   79,   80,   81,   82,   83,
 /*   330 */    84,   85,   86,   87,   88,   89,   90,   91,   92,   93,
 /*   340 */    94,  109,   96,   97,   31,   99,  100,  101,  102,  103,
 /*   350 */   104,  105,  106,   51,   51,   46,   33,  125,  126,   74,
 /*   360 */   128,  115,  116,  117,   79,   80,   81,   82,   83,   84,
 /*   370 */    85,   86,   87,   88,   89,   90,   91,   92,   93,   94,
 /*   380 */    35,   96,   97,   26,   99,  100,  101,  102,  103,  104,
 /*   390 */   105,  106,   27,   28,   46,   30,   34,   32,   74,   46,
 /*   400 */   115,  116,  117,   79,   80,   81,   82,   83,   84,   85,
 /*   410 */    86,   87,   88,   89,   90,   91,   92,   93,   94,   35,
 /*   420 */    96,   97,   26,   99,  100,  101,  102,  103,  104,  105,
 /*   430 */   106,   51,   51,   26,   34,   31,   35,   74,   66,  115,
 /*   440 */   116,  117,   79,   80,   81,   82,   83,   84,   85,   86,
 /*   450 */    87,   88,   89,   90,   91,   92,   93,   94,   34,   96,
 /*   460 */    97,   42,   99,  100,  101,  102,  103,  104,  105,  106,
 /*   470 */    35,   46,   19,   31,   52,   19,   74,   34,  115,  116,
 /*   480 */   117,   79,   80,   81,   82,   83,   84,   85,   86,   87,
 /*   490 */    88,   89,   90,   91,   92,   93,   94,   34,   96,   97,
 /*   500 */    35,   99,  100,  101,  102,  103,  104,  105,  106,   35,
 /*   510 */    31,   33,   68,   26,   31,   74,   34,  115,  116,  117,
 /*   520 */    79,   80,   81,   82,   83,   84,   85,   86,   87,   88,
 /*   530 */    89,   90,   91,   92,   93,   94,   35,   96,   97,   34,
 /*   540 */    99,  100,  101,  102,  103,  104,  105,  106,   31,   35,
 /*   550 */   135,  135,  135,  135,   74,  135,  115,  116,  117,   79,
 /*   560 */    80,   81,   82,   83,   84,   85,   86,   87,   88,   89,
 /*   570 */    90,   91,   92,   93,   94,  135,   96,   97,  135,   99,
 /*   580 */   100,  101,  102,  103,  104,  105,  106,  135,  135,  135,
 /*   590 */   135,  135,  135,   74,  135,  115,  116,  117,   79,   80,
 /*   600 */    81,   82,   83,   84,   85,   86,   87,   88,   89,   90,
 /*   610 */    91,   92,   93,   94,  135,   96,   97,  135,   99,  100,
 /*   620 */   101,  102,  103,  104,  105,  106,   99,  100,  101,  102,
 /*   630 */   103,  104,  105,  106,  115,  116,  117,    1,    2,    3,
 /*   640 */     4,    5,    6,    7,    8,    9,   10,   11,   12,   13,
 /*   650 */    14,   15,   16,   17,   18,   19,   20,   21,   22,   23,
 /*   660 */    24,   25,   15,    8,    9,   10,   11,   12,   13,   14,
 /*   670 */    15,   16,   17,   18,   27,   28,   29,  135,   31,   32,
 /*   680 */   135,   34,  135,   36,   37,   38,   39,   40,   41,   42,
 /*   690 */    43,   44,   45,  135,   47,   15,  135,  115,   51,  117,
 /*   700 */   135,  135,  120,  121,  122,   58,  135,   27,   28,   29,
 /*   710 */   135,   31,   32,  135,   34,  135,   36,   37,   38,   39,
 /*   720 */    40,   41,   42,   43,   44,   45,  135,   47,   15,  135,
 /*   730 */   135,   51,   99,  100,  101,  102,  103,  104,  105,  106,
 /*   740 */    27,   28,   29,  135,   31,   32,  135,   34,  135,   36,
 /*   750 */    37,   38,   39,   40,   41,   42,   43,   44,   45,  135,
 /*   760 */    47,   15,   97,  135,   99,  100,  101,  102,  103,  104,
 /*   770 */   105,  106,  135,   27,   28,   29,  135,   31,   32,   31,
 /*   780 */    34,  135,   36,   37,   38,   39,   40,   41,   42,   43,
 /*   790 */    44,   45,  135,   47,  135,  135,   48,   49,   50,   93,
 /*   800 */    94,  135,   96,   97,  135,   99,  100,  101,  102,  103,
 /*   810 */   104,  105,  106,  135,  135,  135,  110,  111,  112,  113,
 /*   820 */   114,    2,    3,    4,    5,    6,    7,    8,    9,   10,
 /*   830 */    11,   12,   13,   14,   15,   16,   17,   18,    3,    4,
 /*   840 */     5,    6,    7,    8,    9,   10,   11,   12,   13,   14,
 /*   850 */    15,   16,   17,   18,   97,   81,   99,  100,  101,  102,
 /*   860 */   103,  104,  105,  106,  135,  135,  135,   93,   94,  135,
 /*   870 */    96,   97,  135,   99,  100,  101,  102,  103,  104,  105,
 /*   880 */   106,   97,  135,   99,  100,  101,  102,  103,  104,  105,
 /*   890 */   106,  117,   10,   11,   12,   13,   14,   15,   16,   17,
 /*   900 */    18,  135,  135,  129,  130,  135,  135,  135,    4,    5,
 /*   910 */     6,    7,    8,    9,   10,   11,   12,   13,   14,   15,
 /*   920 */    16,   17,   18,    5,    6,    7,    8,    9,   10,   11,
 /*   930 */    12,   13,   14,   15,   16,   17,   18,   93,   94,  135,
 /*   940 */    96,   97,  135,   99,  100,  101,  102,  103,  104,  105,
 /*   950 */   106,  107,  108,   99,  100,  101,  102,  103,  104,  105,
 /*   960 */   106,   93,   94,   31,   96,   97,  135,   99,  100,  101,
 /*   970 */   102,  103,  104,  105,  106,  107,  108,  135,  135,  135,
 /*   980 */    48,   49,   50,  135,  135,   93,   94,  135,   96,   97,
 /*   990 */   135,   99,  100,  101,  102,  103,  104,  105,  106,  135,
 /*  1000 */   135,  135,  135,  111,    6,    7,    8,    9,   10,   11,
 /*  1010 */    12,   13,   14,   15,   16,   17,   18,   93,   94,  135,
 /*  1020 */    96,   97,  135,   99,  100,  101,  102,  103,  104,  105,
 /*  1030 */   106,   93,   94,  135,   96,   97,  135,   99,  100,  101,
 /*  1040 */   102,  103,  104,  105,  106,   93,   94,  135,   96,   97,
 /*  1050 */   135,   99,  100,  101,  102,  103,  104,  105,  106,   93,
 /*  1060 */    94,  135,   96,   97,  135,   99,  100,  101,  102,  103,
 /*  1070 */   104,  105,  106,  135,  135,  135,  135,  135,  135,   93,
 /*  1080 */    94,  135,   96,   97,  135,   99,  100,  101,  102,  103,
 /*  1090 */   104,  105,  106,  135,  135,  135,  135,   93,   94,  135,
 /*  1100 */    96,   97,  135,   99,  100,  101,  102,  103,  104,  105,
 /*  1110 */   106,   93,   94,  135,   96,   97,  135,   99,  100,  101,
 /*  1120 */   102,  103,  104,  105,  106,   93,   94,  135,   96,   97,
 /*  1130 */   135,   99,  100,  101,  102,  103,  104,  105,  106,   93,
 /*  1140 */    94,  135,   96,   97,  135,   99,  100,  101,  102,  103,
 /*  1150 */   104,  105,  106,   93,   94,  135,   96,   97,  135,   99,
 /*  1160 */   100,  101,  102,  103,  104,  105,  106,  135,  135,  135,
 /*  1170 */   135,  135,  135,   93,   94,  135,   96,   97,  135,   99,
 /*  1180 */   100,  101,  102,  103,  104,  105,  106,  135,  135,  135,
 /*  1190 */   135,   93,   94,  135,   96,   97,  135,   99,  100,  101,
 /*  1200 */   102,  103,  104,  105,  106,   93,   94,  135,   96,   97,
 /*  1210 */   135,   99,  100,  101,  102,  103,  104,  105,  106,   93,
 /*  1220 */    94,  135,   96,   97,  135,   99,  100,  101,  102,  103,
 /*  1230 */   104,  105,  106,   94,  135,   96,   97,  135,   99,  100,
 /*  1240 */   101,  102,  103,  104,  105,  106,   94,  135,   96,   97,
 /*  1250 */   135,   99,  100,  101,  102,  103,  104,  105,  106,   97,
 /*  1260 */   135,   99,  100,  101,  102,  103,  104,  105,  106,  135,
 /*  1270 */   135,   97,  135,   99,  100,  101,  102,  103,  104,  105,
 /*  1280 */   106,   97,  135,   99,  100,  101,  102,  103,  104,  105,
 /*  1290 */   106,  135,   97,  135,   99,  100,  101,  102,  103,  104,
 /*  1300 */   105,  106,   97,  135,   99,  100,  101,  102,  103,  104,
 /*  1310 */   105,  106,   97,  135,   99,  100,  101,  102,  103,  104,
 /*  1320 */   105,  106,  135,   97,  135,   99,  100,  101,  102,  103,
 /*  1330 */   104,  105,  106,   97,  135,   99,  100,  101,  102,  103,
 /*  1340 */   104,  105,  106,   97,  135,   99,  100,  101,  102,  103,
 /*  1350 */   104,  105,  106,   97,  135,   99,  100,  101,  102,  103,
 /*  1360 */   104,  105,  106,   97,  135,   99,  100,  101,  102,  103,
 /*  1370 */   104,  105,  106,   97,  135,   99,  100,  101,  102,  103,
 /*  1380 */   104,  105,  106,   97,  135,   99,  100,  101,  102,  103,
 /*  1390 */   104,  105,  106,   97,  135,   99,  100,  101,  102,  103,
 /*  1400 */   104,  105,  106,   37,   38,   39,   40,   41,   42,   43,
 /*  1410 */    44,   45,   97,  135,   99,  100,  101,  102,  103,  104,
 /*  1420 */   105,  106,   53,  135,  135,  135,  135,   58,  135,   60,
 /*  1430 */    61,   62,   63,   53,  135,  135,  135,  135,   58,  135,
 /*  1440 */    60,   61,   62,   63,
};
#define YY_SHIFT_USE_DFLT (-18)
#define YY_SHIFT_MAX 153
static const short yy_shift_ofst[] = {
 /*     0 */   108,  108,  108,   18,  108,   63,  108,  108,  108,  108,
 /*    10 */   108,  108,  108,  108,  746,  647,  713,  713,  713,  713,
 /*    20 */   680,  713,  713,  713,  713,  713,  713,  713,  713,  713,
 /*    30 */   713,  713,  713,  713,  713,  713,  713,  713,  713,  713,
 /*    40 */   713,  713,  713,  713,  713,  713,  713,  713,  713,  713,
 /*    50 */   713,  713,  713,  713,  713,  713,  713,  713,  748,  748,
 /*    60 */  1380, 1369,  748,   36,   98,   85,  313,  178,  636, 1366,
 /*    70 */   932,  206,  178,  100,  -18,  819,  835,  904,  918,  998,
 /*    80 */   655,  655,  882,  882,  253,  253,  253,  253,  365,   95,
 /*    90 */    95,   10,   77,  181,  323,  357,  362,  384,  380,  309,
 /*   100 */   400,  401,  372,  419,  425,  442,  443,  463,  474,  478,
 /*   110 */   444,  453,  505,  517,  514,  483,  501,  487,  482,  479,
 /*   120 */   465,  456,  422,  453,  435,  424,  362,  404,  407,  381,
 /*   130 */   396,  353,  348,  345,  309,  302,  271,  270,  283,  245,
 /*   140 */   216,  204,  159,  103,   90,  106,   52,   17,    2,  -17,
 /*   150 */   303,   13,   42,  -11,
};
#define YY_REDUCE_USE_DFLT (-89)
#define YY_REDUCE_MAX 74
static const short yy_reduce_ofst[] = {
 /*     0 */   -74,  160,  102,  246,  199,  285,  285,  441,  480,  402,
 /*    10 */   324,  363,  285,  519,  706,  774,  868,  844,  127,  892,
 /*    20 */   966, 1004, 1032, 1060, 1098, 1112, 1080, 1046, 1018,  986,
 /*    30 */  1126,  952,  924,  938, 1139, 1152, 1286, 1315, 1276, 1256,
 /*    40 */  1215, 1205, 1195, 1174,  784,  665, 1226, 1246, 1296, 1236,
 /*    50 */  1184,  757, 1162, 1266,  854,  207,  527,  633,   88,  -88,
 /*    60 */   582,   53,  232,  146,  -64,   -3,  -39,  -39,  221,  213,
 /*    70 */   165,  122,   35,   -4,  -65,
};
static const YYACTIONTYPE yy_default[] = {
 /*     0 */   279,  434,  434,  434,  434,  434,  280,  434,  434,  434,
 /*    10 */   434,  434,  433,  434,  371,  434,  434,  434,  296,  434,
 /*    20 */   434,  434,  434,  434,  434,  434,  434,  434,  434,  434,
 /*    30 */   434,  434,  434,  434,  434,  434,  434,  434,  434,  434,
 /*    40 */   434,  434,  434,  434,  434,  434,  434,  434,  434,  434,
 /*    50 */   434,  434,  434,  434,  434,  434,  434,  434,  408,  408,
 /*    60 */   434,  434,  434,  434,  434,  434,  434,  434,  306,  434,
 /*    70 */   434,  434,  434,  434,  428,  309,  310,  311,  312,  313,
 /*    80 */   325,  326,  315,  314,  316,  319,  318,  317,  327,  320,
 /*    90 */   321,  354,  387,  405,  434,  434,  354,  434,  434,  410,
 /*   100 */   434,  434,  341,  434,  356,  434,  434,  434,  434,  434,
 /*   110 */   424,  434,  434,  434,  434,  434,  434,  434,  434,  434,
 /*   120 */   434,  363,  434,  404,  434,  434,  337,  434,  434,  434,
 /*   130 */   434,  372,  409,  434,  411,  434,  434,  434,  434,  434,
 /*   140 */   434,  434,  434,  434,  434,  387,  373,  434,  386,  434,
 /*   150 */   434,  434,  434,  434,  383,  385,  382,  381,  380,  379,
 /*   160 */   378,  388,  390,  389,  289,  377,  275,  391,  287,  392,
 /*   170 */   375,  393,  394,  288,  324,  395,  396,  397,  323,  322,
 /*   180 */   307,  360,  361,  362,  305,  304,  407,  303,  286,  403,
 /*   190 */   416,  302,  301,  363,  300,  339,  338,  370,  369,  367,
 /*   200 */   406,  366,  365,  364,  368,  359,  358,  357,  415,  413,
 /*   210 */   414,  355,  412,  353,  352,  417,  351,  350,  349,  348,
 /*   220 */   347,  285,  421,  346,  345,  344,  284,  343,  283,  425,
 /*   230 */   342,  341,  340,  282,  426,  336,  333,  332,  331,  330,
 /*   240 */   329,  427,  429,  430,  328,  335,  281,  278,  431,  334,
 /*   250 */   277,  432,  308,  299,  298,  295,  294,  297,  293,  276,
 /*   260 */   418,  419,  292,  420,  291,  290,  384,  374,  422,  398,
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
  "EXTERN",        "FOR",           "IN",            "LOWER_THAN_ELSE",
  "ELSE",          "IF",            "WHILE",         "SWITCH",      
  "CASE",          "DEFAULT",       "error",         "main",        
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
 /* 142 */ "extern_declaration ::= EXTERN LIT_STRING type IDENTIFIER LPAREN parameter_list RPAREN SEMICOLON",
 /* 143 */ "for_statement ::= FOR LPAREN for_init_statement expression SEMICOLON expression_opt RPAREN statement",
 /* 144 */ "for_init_statement ::= expression_statement",
 /* 145 */ "for_init_statement ::= variable_declaration SEMICOLON",
 /* 146 */ "foreach_statement ::= FOR LPAREN id_expression IN expression RPAREN statement",
 /* 147 */ "foreach_statement ::= FOR LPAREN foreach_decl IN expression RPAREN statement",
 /* 148 */ "foreach_decl ::= VAR IDENTIFIER",
 /* 149 */ "if_statement ::= IF LPAREN expression RPAREN statement",
 /* 150 */ "if_statement ::= IF LPAREN expression RPAREN statement ELSE statement",
 /* 151 */ "while_statement ::= WHILE LPAREN expression RPAREN statement",
 /* 152 */ "switch_statement ::= SWITCH LPAREN expression RPAREN LBRACE switch_body RBRACE",
 /* 153 */ "switch_body ::=",
 /* 154 */ "switch_body ::= switch_body switch_case",
 /* 155 */ "switch_body ::= switch_body default_case",
 /* 156 */ "switch_case ::= CASE literal COLON case_statements",
 /* 157 */ "default_case ::= DEFAULT COLON case_statements",
 /* 158 */ "case_statements ::= statement_sequence",
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
  { 92, 8 },
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
      case 144:
      case 158:
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
      case 145:
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
      case 153:
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
#line 371 "cscript.in"
{ 
  yygotominor.yy31 = new Ast(extern_declaration, String(yymsp[-4].minor.yy0), yymsp[-2].minor.yy235, String(yymsp[-6].minor.yy0), yymsp[-5].minor.yy31); 
}
#line 1843 "cscript.c"
        break;
      case 143:
#line 381 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(for_statement, yymsp[-5].minor.yy31, yymsp[-4].minor.yy31, yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1848 "cscript.c"
        break;
      case 146:
      case 147:
#line 392 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(foreach_statement, yymsp[-4].minor.yy31, yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1854 "cscript.c"
        break;
      case 148:
#line 394 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(variable_declaration, String(yymsp[0].minor.yy0)); }
#line 1859 "cscript.c"
        break;
      case 149:
#line 405 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(if_statement, yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1864 "cscript.c"
        break;
      case 150:
#line 406 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(if_statement, yymsp[-4].minor.yy31, yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1869 "cscript.c"
        break;
      case 151:
#line 414 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(while_statement, yymsp[-2].minor.yy31,  yymsp[0].minor.yy31); }
#line 1874 "cscript.c"
        break;
      case 152:
#line 422 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(switch_statement, yymsp[-4].minor.yy31, yymsp[-1].minor.yy235); }
#line 1879 "cscript.c"
        break;
      case 154:
      case 155:
#line 427 "cscript.in"
{ yygotominor.yy235 = yymsp[-1].minor.yy235; yygotominor.yy235->push_back(yymsp[0].minor.yy31); }
#line 1885 "cscript.c"
        break;
      case 156:
#line 431 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(switch_case, yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1890 "cscript.c"
        break;
      case 157:
#line 434 "cscript.in"
{ yygotominor.yy31 = p->AllocAst(default_case, yymsp[0].minor.yy31); }
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

