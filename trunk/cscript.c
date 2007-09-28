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
#define YYNOCODE 144
#define YYACTIONTYPE unsigned short int
#define CScriptParseTOKENTYPE  Token 
typedef union {
  CScriptParseTOKENTYPE yy0;
  AccessTypes yy128;
  opcodes yy172;
  Ast* yy235;
  AstList* yy259;
  int yy287;
} YYMINORTYPE;
#ifndef YYSTACKDEPTH
#define YYSTACKDEPTH 100
#endif
#define CScriptParseARG_SDECL  Parser* p ;
#define CScriptParseARG_PDECL , Parser* p 
#define CScriptParseARG_FETCH  Parser* p  = yypParser->p 
#define CScriptParseARG_STORE yypParser->p  = p 
#define YYNSTATE 292
#define YYNRULE 167
#define YYERRORSYMBOL 78
#define YYERRSYMDT yy287
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
 /*     0 */   257,  460,  180,  283,   10,  282,  279,  276,  275,  273,
 /*    10 */   272,  271,  270,  269,  267,  266,  263,  262,  260,  259,
 /*    20 */   258,  137,  254,  103,  252,   70,  176,  250,   94,  240,
 /*    30 */   239,  238,  237,  235,  233,   56,  133,  160,   27,  214,
 /*    40 */   212,  109,  135,  168,  154,  156,   14,   55,   57,   58,
 /*    50 */    66,  112,   16,  210,   31,  125,  232,  230,  229,  228,
 /*    60 */   227,  226,  225,  223,  222,  221,  111,  101,   51,   50,
 /*    70 */    49,  187,    3,  291,  142,   20,  149,  151,   68,  157,
 /*    80 */   178,   77,  147,  146,  127,  113,  174,   56,  150,  118,
 /*    90 */   126,  148,  289,   65,   78,  159,  165,   33,   21,   55,
 /*   100 */    57,   58,  243,  112,   16,   21,   31,   36,  232,  230,
 /*   110 */   229,  228,  227,  226,  225,  223,  222,  221,  107,  101,
 /*   120 */   261,   71,  128,  187,    3,  182,  142,   20,  149,  151,
 /*   130 */    68,  157,  244,  245,   73,  146,  127,  113,   97,   56,
 /*   140 */   179,  118,  126,  148,    3,  442,   78,   75,  256,   33,
 /*   150 */   213,   55,   57,   58,  155,  112,   16,  161,   31,  166,
 /*   160 */   232,  230,  229,  228,  227,  226,  225,  223,  222,  221,
 /*   170 */    67,  101,   68,   99,   61,  187,    3,  146,  142,   20,
 /*   180 */   149,  151,   68,  157,  102,   74,  167,  146,  127,  113,
 /*   190 */   183,  184,  185,  118,  126,  148,  205,   80,   78,  249,
 /*   200 */   248,   33,   76,  257,   32,  115,  153,    7,  282,  279,
 /*   210 */   276,  275,  273,  272,  271,  270,  269,  267,  266,  263,
 /*   220 */   262,  260,  259,  258,  137,  254,  103,  252,   70,   29,
 /*   230 */   250,   94,  240,  239,  238,  237,  235,  233,  204,  170,
 /*   240 */   131,  382,  214,  212,  109,  135,  168,  154,  156,  141,
 /*   250 */    87,  173,  250,   94,  240,  239,  238,  237,  235,  233,
 /*   260 */   253,   74,  257,  172,   34,  268,    7,  282,  279,  276,
 /*   270 */   275,  273,  272,  271,  270,  269,  267,  266,  263,  262,
 /*   280 */   260,  259,  258,  137,  254,  121,  252,   70,  177,  250,
 /*   290 */    94,  240,  239,  238,  237,  235,  233,   53,   52,   51,
 /*   300 */    50,   49,  183,  184,  185,  168,  154,  156,  218,  254,
 /*   310 */   103,  252,   70,  255,  250,   94,  240,  239,  238,  237,
 /*   320 */   235,  233,  219,  104,  265,  257,  211,  212,  144,  114,
 /*   330 */   192,  279,  276,  275,  273,  272,  271,  270,  269,  267,
 /*   340 */   266,  263,  262,  260,  259,  258,  137,  254,  164,  252,
 /*   350 */    70,   13,  250,   94,  240,  239,  238,  237,  235,  233,
 /*   360 */   247,   94,  240,  239,  238,  237,  235,  233,  168,  154,
 /*   370 */   156,  119,   60,  181,  257,  216,  143,  191,    5,  282,
 /*   380 */   279,  276,  275,  273,  272,  271,  270,  269,  267,  266,
 /*   390 */   263,  262,  260,  259,  258,  137,  254,  100,  252,   70,
 /*   400 */    14,  250,   94,  240,  239,  238,  237,  235,  233,   30,
 /*   410 */   105,   17,  280,   26,   59,  257,   22,  168,  154,  156,
 /*   420 */   277,  279,  276,  275,  273,  272,  271,  270,  269,  267,
 /*   430 */   266,  263,  262,  260,  259,  258,  137,  254,  186,  252,
 /*   440 */    70,  217,  250,   94,  240,  239,  238,  237,  235,  233,
 /*   450 */     6,  220,   64,   63,  203,  134,  257,    1,  168,  154,
 /*   460 */   156,  281,  279,  276,  275,  273,  272,  271,  270,  269,
 /*   470 */   267,  266,  263,  262,  260,  259,  258,  137,  254,  234,
 /*   480 */   252,   70,    4,  250,   94,  240,  239,  238,  237,  235,
 /*   490 */   233,   15,   19,    9,   27,    2,   12,  257,   72,  168,
 /*   500 */   154,  156,  122,  279,  276,  275,  273,  272,  271,  270,
 /*   510 */   269,  267,  266,  263,  262,  260,  259,  258,  137,  254,
 /*   520 */    28,  252,   70,  264,  250,   94,  240,  239,  238,  237,
 /*   530 */   235,  233,   24,  138,   79,   18,   35,   29,  257,   11,
 /*   540 */   168,  154,  156,  236,  279,  276,  275,  273,  272,  271,
 /*   550 */   270,  269,  267,  266,  263,  262,  260,  259,  258,  137,
 /*   560 */   254,    8,  252,   70,  461,  250,   94,  240,  239,  238,
 /*   570 */   237,  235,  233,  461,  461,  461,  461,  461,  461,  257,
 /*   580 */   461,  168,  154,  156,  224,  279,  276,  275,  273,  272,
 /*   590 */   271,  270,  269,  267,  266,  263,  262,  260,  259,  258,
 /*   600 */   137,  254,  461,  252,   70,  461,  250,   94,  240,  239,
 /*   610 */   238,  237,  235,  233,  461,  461,  461,  461,  461,  461,
 /*   620 */   257,  461,  168,  154,  156,  231,  279,  276,  275,  273,
 /*   630 */   272,  271,  270,  269,  267,  266,  263,  262,  260,  259,
 /*   640 */   258,  137,  254,  461,  252,   70,  461,  250,   94,  240,
 /*   650 */   239,  238,  237,  235,  233,  461,  461,  461,  461,  461,
 /*   660 */   461,  257,  461,  168,  154,  156,  285,  279,  276,  275,
 /*   670 */   273,  272,  271,  270,  269,  267,  266,  263,  262,  260,
 /*   680 */   259,  258,  137,  254,  461,  252,   70,  461,  250,   94,
 /*   690 */   240,  239,  238,  237,  235,  233,  242,   94,  240,  239,
 /*   700 */   238,  237,  235,  233,  168,  154,  156,   40,   45,   41,
 /*   710 */    54,   39,   48,   47,   44,   37,   38,   43,   42,   46,
 /*   720 */    53,   52,   51,   50,   49,  201,  200,  199,  198,  197,
 /*   730 */   196,   23,   56,   44,   37,   38,   43,   42,   46,   53,
 /*   740 */    52,   51,   50,   49,   55,   57,   58,  461,  112,   16,
 /*   750 */   461,   31,  461,  232,  230,  229,  228,  227,  226,  225,
 /*   760 */   223,  222,  221,  461,  101,   56,  461,  174,  187,  150,
 /*   770 */   461,  461,   62,  290,   65,   69,  461,   55,   57,   58,
 /*   780 */   461,  112,   16,  461,   31,  461,  232,  230,  229,  228,
 /*   790 */   227,  226,  225,  223,  222,  221,  461,  101,   56,  461,
 /*   800 */   461,  175,  241,   94,  240,  239,  238,  237,  235,  233,
 /*   810 */    55,   57,   58,  461,  112,   16,  461,   31,  461,  232,
 /*   820 */   230,  229,  228,  227,  226,  225,  223,  222,  221,  461,
 /*   830 */   101,   56,   96,  461,  250,   94,  240,  239,  238,  237,
 /*   840 */   235,  233,  461,   55,   57,   58,  461,   98,   16,  461,
 /*   850 */    31,  461,  232,  230,  229,  228,  227,  226,  225,  223,
 /*   860 */   222,  221,  461,  101,   45,   41,   54,   39,   48,   47,
 /*   870 */    44,   37,   38,   43,   42,   46,   53,   52,   51,   50,
 /*   880 */    49,  209,  254,  461,  252,   70,  461,  250,   94,  240,
 /*   890 */   239,  238,  237,  235,  233,  461,  461,  461,  158,  208,
 /*   900 */   124,  206,  162,   41,   54,   39,   48,   47,   44,   37,
 /*   910 */    38,   43,   42,   46,   53,   52,   51,   50,   49,  461,
 /*   920 */   461,  461,  461,  195,  218,  254,  461,  252,   70,  278,
 /*   930 */   250,   94,  240,  239,  238,  237,  235,  233,  108,  104,
 /*   940 */   183,  184,  185,  137,  254,  461,  252,   70,  461,  250,
 /*   950 */    94,  240,  239,  238,  237,  106,  233,  461,  461,  461,
 /*   960 */   461,  461,  461,  461,  461,  209,  254,  139,  252,   70,
 /*   970 */   461,  250,   94,  240,  239,  238,  237,  235,  233,   25,
 /*   980 */   140,  461,  461,  207,   54,   39,   48,   47,   44,   37,
 /*   990 */    38,   43,   42,   46,   53,   52,   51,   50,   49,   39,
 /*  1000 */    48,   47,   44,   37,   38,   43,   42,   46,   53,   52,
 /*  1010 */    51,   50,   49,   48,   47,   44,   37,   38,   43,   42,
 /*  1020 */    46,   53,   52,   51,   50,   49,  461,  274,  254,  136,
 /*  1030 */   252,   70,  461,  250,   94,  240,  239,  238,  237,  235,
 /*  1040 */   233,  461,  461,  132,  254,  461,  252,   70,  461,  250,
 /*  1050 */    94,  240,  239,  238,  237,  235,  233,  246,   94,  240,
 /*  1060 */   239,  238,  237,  235,  233,  152,  254,  461,  252,   70,
 /*  1070 */   461,  250,   94,  240,  239,  238,  237,  235,  233,  461,
 /*  1080 */   145,  254,  461,  252,   70,  461,  250,   94,  240,  239,
 /*  1090 */   238,  237,  235,  233,  110,  254,  461,  252,   70,  461,
 /*  1100 */   250,   94,  240,  239,  238,  237,  235,  233,  117,  254,
 /*  1110 */   461,  252,   70,  461,  250,   94,  240,  239,  238,  237,
 /*  1120 */   235,  233,  188,  254,  461,  252,   70,  461,  250,   94,
 /*  1130 */   240,  239,  238,  237,  235,  233,  163,  254,  461,  252,
 /*  1140 */    70,  461,  250,   94,  240,  239,  238,  237,  235,  233,
 /*  1150 */   130,  254,  461,  252,   70,  461,  250,   94,  240,  239,
 /*  1160 */   238,  237,  235,  233,  461,  284,  254,  461,  252,   70,
 /*  1170 */   461,  250,   94,  240,  239,  238,  237,  235,  233,  461,
 /*  1180 */   215,  254,  461,  252,   70,  461,  250,   94,  240,  239,
 /*  1190 */   238,  237,  235,  233,  116,  254,  461,  252,   70,  461,
 /*  1200 */   250,   94,  240,  239,  238,  237,  235,  233,  202,  254,
 /*  1210 */   461,  252,   70,  461,  250,   94,  240,  239,  238,  237,
 /*  1220 */   235,  233,  123,  254,  461,  252,   70,  461,  250,   94,
 /*  1230 */   240,  239,  238,  237,  235,  233,  129,  254,  461,  252,
 /*  1240 */    70,  461,  250,   94,  240,  239,  238,  237,  235,  233,
 /*  1250 */   120,  254,  461,  252,   70,  461,  250,   94,  240,  239,
 /*  1260 */   238,  237,  235,  233,  251,  461,  252,   70,  461,  250,
 /*  1270 */    94,  240,  239,  238,  237,  235,  233,  194,  461,  252,
 /*  1280 */    70,  461,  250,   94,  240,  239,  238,  237,  235,  233,
 /*  1290 */    92,  461,  250,   94,  240,  239,  238,  237,  235,  233,
 /*  1300 */   461,   95,  461,  250,   94,  240,  239,  238,  237,  235,
 /*  1310 */   233,   82,  461,  250,   94,  240,  239,  238,  237,  235,
 /*  1320 */   233,   84,  461,  250,   94,  240,  239,  238,  237,  235,
 /*  1330 */   233,  461,  193,  461,  250,   94,  240,  239,  238,  237,
 /*  1340 */   235,  233,  461,   38,   43,   42,   46,   53,   52,   51,
 /*  1350 */    50,   49,   90,  461,  250,   94,  240,  239,  238,  237,
 /*  1360 */   235,  233,   91,  461,  250,   94,  240,  239,  238,  237,
 /*  1370 */   235,  233,  461,   86,  461,  250,   94,  240,  239,  238,
 /*  1380 */   237,  235,  233,   83,  461,  250,   94,  240,  239,  238,
 /*  1390 */   237,  235,  233,  190,  461,  250,   94,  240,  239,  238,
 /*  1400 */   237,  235,  233,   81,  461,  250,   94,  240,  239,  238,
 /*  1410 */   237,  235,  233,   85,  461,  250,   94,  240,  239,  238,
 /*  1420 */   237,  235,  233,   88,  461,  250,   94,  240,  239,  238,
 /*  1430 */   237,  235,  233,   89,  461,  250,   94,  240,  239,  238,
 /*  1440 */   237,  235,  233,  189,  461,  250,   94,  240,  239,  238,
 /*  1450 */   237,  235,  233,  230,  229,  228,  227,  226,  225,  223,
 /*  1460 */   222,  221,   93,  461,  250,   94,  240,  239,  238,  237,
 /*  1470 */   235,  233,  171,  461,  461,  461,  461,   68,  461,  286,
 /*  1480 */   287,  288,  146,  169,  461,  461,  461,  461,   68,  461,
 /*  1490 */   286,  287,  288,  146,
};
static const YYCODETYPE yy_lookahead[] = {
 /*     0 */    78,   79,   80,   81,   82,   83,   84,   85,   86,   87,
 /*    10 */    88,   89,   90,   91,   92,   93,   94,   95,   96,   97,
 /*    20 */    98,   99,  100,  115,  102,  103,   51,  105,  106,  107,
 /*    30 */   108,  109,  110,  111,  112,   15,  115,  129,   26,  131,
 /*    40 */   132,  133,  134,  121,  122,  123,   34,   27,   28,   29,
 /*    50 */   137,   31,   32,  132,   34,  115,   36,   37,   38,   39,
 /*    60 */    40,   41,   42,   43,   44,   45,  110,   47,   16,   17,
 /*    70 */    18,   51,   52,   53,   54,   55,   56,   57,   58,   59,
 /*    80 */   121,   46,  123,   63,   64,   65,  121,   15,  123,   69,
 /*    90 */    70,   71,  127,  128,   74,  124,  125,   77,   19,   27,
 /*   100 */    28,   29,   53,   31,   32,   19,   34,  104,   36,   37,
 /*   110 */    38,   39,   40,   41,   42,   43,   44,   45,   35,   47,
 /*   120 */    88,   72,   73,   51,   52,   53,   54,   55,   56,   57,
 /*   130 */    58,   59,  138,  139,   46,   63,   64,   65,   31,   15,
 /*   140 */    26,   69,   70,   71,   52,   66,   74,  141,  142,   77,
 /*   150 */    35,   27,   28,   29,   31,   31,   32,   31,   34,   51,
 /*   160 */    36,   37,   38,   39,   40,   41,   42,   43,   44,   45,
 /*   170 */    88,   47,   58,   31,   52,   51,   52,   63,   54,   55,
 /*   180 */    56,   57,   58,   59,   75,   76,   51,   63,   64,   65,
 /*   190 */    48,   49,   50,   69,   70,   71,  119,   52,   74,   27,
 /*   200 */    28,   77,   30,   78,   32,   31,   35,   82,   83,   84,
 /*   210 */    85,   86,   87,   88,   89,   90,   91,   92,   93,   94,
 /*   220 */    95,   96,   97,   98,   99,  100,  115,  102,  103,   19,
 /*   230 */   105,  106,  107,  108,  109,  110,  111,  112,  109,   51,
 /*   240 */   129,   31,  131,  132,  133,  134,  121,  122,  123,   31,
 /*   250 */   103,   51,  105,  106,  107,  108,  109,  110,  111,  112,
 /*   260 */   142,   76,   78,   51,   34,  140,   82,   83,   84,   85,
 /*   270 */    86,   87,   88,   89,   90,   91,   92,   93,   94,   95,
 /*   280 */    96,   97,   98,   99,  100,   31,  102,  103,   51,  105,
 /*   290 */   106,  107,  108,  109,  110,  111,  112,   14,   15,   16,
 /*   300 */    17,   18,   48,   49,   50,  121,  122,  123,   99,  100,
 /*   310 */   115,  102,  103,   88,  105,  106,  107,  108,  109,  110,
 /*   320 */   111,  112,  113,  114,  140,   78,  131,  132,   31,  134,
 /*   330 */    83,   84,   85,   86,   87,   88,   89,   90,   91,   92,
 /*   340 */    93,   94,   95,   96,   97,   98,   99,  100,  125,  102,
 /*   350 */   103,   35,  105,  106,  107,  108,  109,  110,  111,  112,
 /*   360 */   105,  106,  107,  108,  109,  110,  111,  112,  121,  122,
 /*   370 */   123,   31,   34,   51,   78,   31,   42,  130,   82,   83,
 /*   380 */    84,   85,   86,   87,   88,   89,   90,   91,   92,   93,
 /*   390 */    94,   95,   96,   97,   98,   99,  100,   34,  102,  103,
 /*   400 */    34,  105,  106,  107,  108,  109,  110,  111,  112,   66,
 /*   410 */    31,   46,   51,   19,   34,   78,   66,  121,  122,  123,
 /*   420 */    83,   84,   85,   86,   87,   88,   89,   90,   91,   92,
 /*   430 */    93,   94,   95,   96,   97,   98,   99,  100,   51,  102,
 /*   440 */   103,   51,  105,  106,  107,  108,  109,  110,  111,  112,
 /*   450 */    35,   33,   46,   46,   33,   31,   78,   26,  121,  122,
 /*   460 */   123,   83,   84,   85,   86,   87,   88,   89,   90,   91,
 /*   470 */    92,   93,   94,   95,   96,   97,   98,   99,  100,   35,
 /*   480 */   102,  103,   35,  105,  106,  107,  108,  109,  110,  111,
 /*   490 */   112,   34,   51,   35,   26,   26,   35,   78,   42,  121,
 /*   500 */   122,  123,   83,   84,   85,   86,   87,   88,   89,   90,
 /*   510 */    91,   92,   93,   94,   95,   96,   97,   98,   99,  100,
 /*   520 */    34,  102,  103,   51,  105,  106,  107,  108,  109,  110,
 /*   530 */   111,  112,   34,   31,   35,   46,   26,   19,   78,   35,
 /*   540 */   121,  122,  123,   83,   84,   85,   86,   87,   88,   89,
 /*   550 */    90,   91,   92,   93,   94,   95,   96,   97,   98,   99,
 /*   560 */   100,   68,  102,  103,  143,  105,  106,  107,  108,  109,
 /*   570 */   110,  111,  112,  143,  143,  143,  143,  143,  143,   78,
 /*   580 */   143,  121,  122,  123,   83,   84,   85,   86,   87,   88,
 /*   590 */    89,   90,   91,   92,   93,   94,   95,   96,   97,   98,
 /*   600 */    99,  100,  143,  102,  103,  143,  105,  106,  107,  108,
 /*   610 */   109,  110,  111,  112,  143,  143,  143,  143,  143,  143,
 /*   620 */    78,  143,  121,  122,  123,   83,   84,   85,   86,   87,
 /*   630 */    88,   89,   90,   91,   92,   93,   94,   95,   96,   97,
 /*   640 */    98,   99,  100,  143,  102,  103,  143,  105,  106,  107,
 /*   650 */   108,  109,  110,  111,  112,  143,  143,  143,  143,  143,
 /*   660 */   143,   78,  143,  121,  122,  123,   83,   84,   85,   86,
 /*   670 */    87,   88,   89,   90,   91,   92,   93,   94,   95,   96,
 /*   680 */    97,   98,   99,  100,  143,  102,  103,  143,  105,  106,
 /*   690 */   107,  108,  109,  110,  111,  112,  105,  106,  107,  108,
 /*   700 */   109,  110,  111,  112,  121,  122,  123,    1,    2,    3,
 /*   710 */     4,    5,    6,    7,    8,    9,   10,   11,   12,   13,
 /*   720 */    14,   15,   16,   17,   18,   19,   20,   21,   22,   23,
 /*   730 */    24,   25,   15,    8,    9,   10,   11,   12,   13,   14,
 /*   740 */    15,   16,   17,   18,   27,   28,   29,  143,   31,   32,
 /*   750 */   143,   34,  143,   36,   37,   38,   39,   40,   41,   42,
 /*   760 */    43,   44,   45,  143,   47,   15,  143,  121,   51,  123,
 /*   770 */   143,  143,  126,  127,  128,   58,  143,   27,   28,   29,
 /*   780 */   143,   31,   32,  143,   34,  143,   36,   37,   38,   39,
 /*   790 */    40,   41,   42,   43,   44,   45,  143,   47,   15,  143,
 /*   800 */   143,   51,  105,  106,  107,  108,  109,  110,  111,  112,
 /*   810 */    27,   28,   29,  143,   31,   32,  143,   34,  143,   36,
 /*   820 */    37,   38,   39,   40,   41,   42,   43,   44,   45,  143,
 /*   830 */    47,   15,  103,  143,  105,  106,  107,  108,  109,  110,
 /*   840 */   111,  112,  143,   27,   28,   29,  143,   31,   32,  143,
 /*   850 */    34,  143,   36,   37,   38,   39,   40,   41,   42,   43,
 /*   860 */    44,   45,  143,   47,    2,    3,    4,    5,    6,    7,
 /*   870 */     8,    9,   10,   11,   12,   13,   14,   15,   16,   17,
 /*   880 */    18,   99,  100,  143,  102,  103,  143,  105,  106,  107,
 /*   890 */   108,  109,  110,  111,  112,  143,  143,  143,  116,  117,
 /*   900 */   118,  119,  120,    3,    4,    5,    6,    7,    8,    9,
 /*   910 */    10,   11,   12,   13,   14,   15,   16,   17,   18,  143,
 /*   920 */   143,  143,  143,   31,   99,  100,  143,  102,  103,   85,
 /*   930 */   105,  106,  107,  108,  109,  110,  111,  112,  113,  114,
 /*   940 */    48,   49,   50,   99,  100,  143,  102,  103,  143,  105,
 /*   950 */   106,  107,  108,  109,  110,  111,  112,  143,  143,  143,
 /*   960 */   143,  143,  143,  143,  143,   99,  100,  123,  102,  103,
 /*   970 */   143,  105,  106,  107,  108,  109,  110,  111,  112,  135,
 /*   980 */   136,  143,  143,  117,    4,    5,    6,    7,    8,    9,
 /*   990 */    10,   11,   12,   13,   14,   15,   16,   17,   18,    5,
 /*  1000 */     6,    7,    8,    9,   10,   11,   12,   13,   14,   15,
 /*  1010 */    16,   17,   18,    6,    7,    8,    9,   10,   11,   12,
 /*  1020 */    13,   14,   15,   16,   17,   18,  143,   99,  100,  101,
 /*  1030 */   102,  103,  143,  105,  106,  107,  108,  109,  110,  111,
 /*  1040 */   112,  143,  143,   99,  100,  143,  102,  103,  143,  105,
 /*  1050 */   106,  107,  108,  109,  110,  111,  112,  105,  106,  107,
 /*  1060 */   108,  109,  110,  111,  112,   99,  100,  143,  102,  103,
 /*  1070 */   143,  105,  106,  107,  108,  109,  110,  111,  112,  143,
 /*  1080 */    99,  100,  143,  102,  103,  143,  105,  106,  107,  108,
 /*  1090 */   109,  110,  111,  112,   99,  100,  143,  102,  103,  143,
 /*  1100 */   105,  106,  107,  108,  109,  110,  111,  112,   99,  100,
 /*  1110 */   143,  102,  103,  143,  105,  106,  107,  108,  109,  110,
 /*  1120 */   111,  112,   99,  100,  143,  102,  103,  143,  105,  106,
 /*  1130 */   107,  108,  109,  110,  111,  112,   99,  100,  143,  102,
 /*  1140 */   103,  143,  105,  106,  107,  108,  109,  110,  111,  112,
 /*  1150 */    99,  100,  143,  102,  103,  143,  105,  106,  107,  108,
 /*  1160 */   109,  110,  111,  112,  143,   99,  100,  143,  102,  103,
 /*  1170 */   143,  105,  106,  107,  108,  109,  110,  111,  112,  143,
 /*  1180 */    99,  100,  143,  102,  103,  143,  105,  106,  107,  108,
 /*  1190 */   109,  110,  111,  112,   99,  100,  143,  102,  103,  143,
 /*  1200 */   105,  106,  107,  108,  109,  110,  111,  112,   99,  100,
 /*  1210 */   143,  102,  103,  143,  105,  106,  107,  108,  109,  110,
 /*  1220 */   111,  112,   99,  100,  143,  102,  103,  143,  105,  106,
 /*  1230 */   107,  108,  109,  110,  111,  112,   99,  100,  143,  102,
 /*  1240 */   103,  143,  105,  106,  107,  108,  109,  110,  111,  112,
 /*  1250 */    99,  100,  143,  102,  103,  143,  105,  106,  107,  108,
 /*  1260 */   109,  110,  111,  112,  100,  143,  102,  103,  143,  105,
 /*  1270 */   106,  107,  108,  109,  110,  111,  112,  100,  143,  102,
 /*  1280 */   103,  143,  105,  106,  107,  108,  109,  110,  111,  112,
 /*  1290 */   103,  143,  105,  106,  107,  108,  109,  110,  111,  112,
 /*  1300 */   143,  103,  143,  105,  106,  107,  108,  109,  110,  111,
 /*  1310 */   112,  103,  143,  105,  106,  107,  108,  109,  110,  111,
 /*  1320 */   112,  103,  143,  105,  106,  107,  108,  109,  110,  111,
 /*  1330 */   112,  143,  103,  143,  105,  106,  107,  108,  109,  110,
 /*  1340 */   111,  112,  143,   10,   11,   12,   13,   14,   15,   16,
 /*  1350 */    17,   18,  103,  143,  105,  106,  107,  108,  109,  110,
 /*  1360 */   111,  112,  103,  143,  105,  106,  107,  108,  109,  110,
 /*  1370 */   111,  112,  143,  103,  143,  105,  106,  107,  108,  109,
 /*  1380 */   110,  111,  112,  103,  143,  105,  106,  107,  108,  109,
 /*  1390 */   110,  111,  112,  103,  143,  105,  106,  107,  108,  109,
 /*  1400 */   110,  111,  112,  103,  143,  105,  106,  107,  108,  109,
 /*  1410 */   110,  111,  112,  103,  143,  105,  106,  107,  108,  109,
 /*  1420 */   110,  111,  112,  103,  143,  105,  106,  107,  108,  109,
 /*  1430 */   110,  111,  112,  103,  143,  105,  106,  107,  108,  109,
 /*  1440 */   110,  111,  112,  103,  143,  105,  106,  107,  108,  109,
 /*  1450 */   110,  111,  112,   37,   38,   39,   40,   41,   42,   43,
 /*  1460 */    44,   45,  103,  143,  105,  106,  107,  108,  109,  110,
 /*  1470 */   111,  112,   53,  143,  143,  143,  143,   58,  143,   60,
 /*  1480 */    61,   62,   63,   53,  143,  143,  143,  143,   58,  143,
 /*  1490 */    60,   61,   62,   63,
};
#define YY_SHIFT_USE_DFLT (-26)
#define YY_SHIFT_MAX 163
static const short yy_shift_ofst[] = {
 /*     0 */   124,  124,  124,   20,  124,   72,  124,  124,  124,  124,
 /*    10 */   124,  124,  124,  124,  816,  717,  783,  783,  783,  783,
 /*    20 */   750,  783,  783,  783,  783,  783,  783,  783,  783,  783,
 /*    30 */   783,  783,  783,  783,  783,  783,  783,  783,  783,  783,
 /*    40 */   783,  783,  783,  783,  783,  783,  783,  783,  783,  783,
 /*    50 */   783,  783,  783,  783,  783,  783,  783,  783,  783,  142,
 /*    60 */   142, 1430, 1419,  142,  254,  114,   49,  109,  126,  107,
 /*    70 */   706, 1416,  892,  126,   92,  185,  218,  174,   92,   92,
 /*    80 */   -26,  862,  900,  980,  994, 1007,  725,  725, 1333, 1333,
 /*    90 */   283,  283,  283,  283,  172,   52,   52,   79,   12,  210,
 /*   100 */   340,  344,  363,  379,  365,  394,  350,  390,  418,  407,
 /*   110 */   421,  431,  366,  457,  406,  468,  461,  472,  498,  499,
 /*   120 */   504,  518,  493,  510,  489,  502,  486,  456,  469,  441,
 /*   130 */   458,  447,  444,  424,  394,  406,  415,  387,  380,  361,
 /*   140 */   343,  366,  334,  322,  338,  316,  297,  237,  230,  212,
 /*   150 */   200,  188,  171,  145,  135,  122,  108,  123,  115,   88,
 /*   160 */    83,   86,   35,  -25,
};
#define YY_REDUCE_USE_DFLT (-93)
#define YY_REDUCE_MAX 80
static const short yy_reduce_ofst[] = {
 /*     0 */   -78,  184,  125,  296,  247,  378,  337,  378,  542,  460,
 /*    10 */   378,  419,  501,  583,  782,  844,  825,  209,  866,  928,
 /*    20 */  1037, 1066, 1095, 1123, 1151, 1137, 1109, 1081, 1051, 1023,
 /*    30 */   981,  944,  995, 1009,  966, 1177, 1164, 1330, 1359, 1310,
 /*    40 */  1300, 1280, 1259, 1249, 1320, 1208, 1187,  147, 1270, 1340,
 /*    50 */  1290, 1229,  729, 1198, 1218,  952,  255,  591,  697,  -92,
 /*    60 */   111,  646,  -35,  195,  -79,  -41,   -6,    6,  -29,  -29,
 /*    70 */     3,  -44,  -60,  223,  225,  118,  129,   77,   82,   32,
 /*    80 */   -87,
};
static const YYACTIONTYPE yy_default[] = {
 /*     0 */   296,  459,  459,  459,  459,  459,  459,  452,  459,  459,
 /*    10 */   297,  459,  459,  459,  390,  459,  459,  459,  459,  315,
 /*    20 */   459,  459,  459,  459,  459,  459,  459,  459,  459,  459,
 /*    30 */   459,  459,  459,  459,  459,  459,  459,  459,  459,  459,
 /*    40 */   459,  459,  459,  459,  459,  459,  459,  459,  459,  459,
 /*    50 */   459,  459,  459,  459,  459,  459,  459,  459,  459,  427,
 /*    60 */   427,  459,  459,  459,  459,  459,  459,  459,  459,  459,
 /*    70 */   325,  459,  459,  459,  459,  453,  459,  459,  459,  459,
 /*    80 */   447,  328,  329,  330,  331,  332,  344,  345,  333,  334,
 /*    90 */   336,  337,  338,  335,  346,  339,  340,  406,  373,  424,
 /*   100 */   459,  459,  459,  459,  375,  423,  360,  459,  459,  428,
 /*   110 */   459,  459,  373,  459,  430,  459,  459,  459,  459,  459,
 /*   120 */   459,  382,  443,  459,  391,  459,  459,  459,  459,  459,
 /*   130 */   459,  459,  459,  459,  459,  429,  459,  459,  459,  459,
 /*   140 */   459,  356,  459,  459,  459,  459,  459,  459,  459,  459,
 /*   150 */   459,  459,  459,  459,  459,  459,  459,  459,  459,  405,
 /*   160 */   459,  406,  392,  459,  409,  408,  404,  403,  402,  410,
 /*   170 */   401,  411,  400,  412,  413,  399,  398,  414,  415,  416,
 /*   180 */   292,  397,  396,  379,  380,  381,  394,  393,  426,  343,
 /*   190 */   342,  422,  435,  341,  326,  382,  324,  323,  322,  321,
 /*   200 */   320,  319,  425,  358,  357,  389,  388,  386,  385,  384,
 /*   210 */   434,  432,  433,  383,  431,  387,  378,  436,  377,  376,
 /*   220 */   374,  372,  371,  370,  440,  369,  368,  367,  366,  365,
 /*   230 */   364,  444,  363,  362,  361,  360,  445,  359,  355,  352,
 /*   240 */   351,  350,  349,  446,  448,  449,  348,  347,  354,  353,
 /*   250 */   327,  318,  317,  455,  314,  457,  454,  313,  312,  311,
 /*   260 */   310,  456,  309,  308,  458,  450,  307,  306,  451,  305,
 /*   270 */   304,  303,  302,  301,  316,  300,  299,  437,  438,  298,
 /*   280 */   439,  295,  294,  293,  407,  441,  417,  418,  419,  421,
 /*   290 */   420,  395,
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
  "CASE",          "DEFAULT",       "TRY",           "CATCH",       
  "FINALLY",       "THROW",         "error",         "main",        
  "translation_unit",  "statement_sequence_opt",  "statement_sequence",  "statement",   
  "include_statement",  "expression_statement",  "declaration_statement",  "for_statement",
  "compound_statement",  "if_statement",  "while_statement",  "foreach_statement",
  "return_statement",  "switch_statement",  "break_statement",  "continue_statement",
  "extern_declaration",  "try_statement",  "throw_statement",  "expression",  
  "assignment_expression",  "expression_opt",  "conditional_expression",  "binary_expression",
  "assignment_operator",  "unary_expression",  "postfix_expression",  "new_expression",
  "primary_expression",  "function_call",  "literal",       "id_expression",
  "list_literal",  "list_content",  "list_entry",    "type",        
  "argument_list",  "positional_argument",  "positional_argument_list",  "named_argument",
  "named_argument_list",  "function_declaration",  "class_declaration",  "variable_declaration",
  "declarator_sequence",  "declarator",    "class_members",  "class_member",
  "access_specifier",  "parameter_list",  "function_body",  "parameter",   
  "opt_parameter",  "parameters",    "opt_parameters",  "for_init_statement",
  "foreach_decl",  "switch_body",   "switch_case",   "default_case",
  "case_statements",  "catch_block",   "finally_block",
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
 /*  19 */ "statement ::= try_statement",
 /*  20 */ "statement ::= throw_statement",
 /*  21 */ "statement ::= error",
 /*  22 */ "expression ::= assignment_expression",
 /*  23 */ "expression_opt ::=",
 /*  24 */ "expression_opt ::= expression",
 /*  25 */ "assignment_expression ::= conditional_expression",
 /*  26 */ "assignment_expression ::= binary_expression assignment_operator assignment_expression",
 /*  27 */ "assignment_operator ::= ASSIGN",
 /*  28 */ "assignment_operator ::= ASSADD",
 /*  29 */ "assignment_operator ::= ASSSUB",
 /*  30 */ "assignment_operator ::= ASSMUL",
 /*  31 */ "assignment_operator ::= ASSDIV",
 /*  32 */ "assignment_operator ::= ASSMOD",
 /*  33 */ "conditional_expression ::= binary_expression",
 /*  34 */ "conditional_expression ::= binary_expression QUESTION expression COLON assignment_expression",
 /*  35 */ "binary_expression ::= unary_expression",
 /*  36 */ "binary_expression ::= binary_expression LOGOR binary_expression",
 /*  37 */ "binary_expression ::= binary_expression LOGAND binary_expression",
 /*  38 */ "binary_expression ::= binary_expression BITOR binary_expression",
 /*  39 */ "binary_expression ::= binary_expression BITXOR binary_expression",
 /*  40 */ "binary_expression ::= binary_expression BITAND binary_expression",
 /*  41 */ "binary_expression ::= binary_expression EQUALS binary_expression",
 /*  42 */ "binary_expression ::= binary_expression NEQUALS binary_expression",
 /*  43 */ "binary_expression ::= binary_expression ST binary_expression",
 /*  44 */ "binary_expression ::= binary_expression SE binary_expression",
 /*  45 */ "binary_expression ::= binary_expression GT binary_expression",
 /*  46 */ "binary_expression ::= binary_expression GE binary_expression",
 /*  47 */ "binary_expression ::= binary_expression ADDOP binary_expression",
 /*  48 */ "binary_expression ::= binary_expression SUBOP binary_expression",
 /*  49 */ "binary_expression ::= binary_expression MULOP binary_expression",
 /*  50 */ "binary_expression ::= binary_expression DIVOP binary_expression",
 /*  51 */ "binary_expression ::= binary_expression MODOP binary_expression",
 /*  52 */ "binary_expression ::= binary_expression SEQ binary_expression",
 /*  53 */ "binary_expression ::= binary_expression SNE binary_expression",
 /*  54 */ "unary_expression ::= postfix_expression",
 /*  55 */ "unary_expression ::= SUBOP unary_expression",
 /*  56 */ "unary_expression ::= ADDADD unary_expression",
 /*  57 */ "unary_expression ::= SUBSUB unary_expression",
 /*  58 */ "unary_expression ::= NOT unary_expression",
 /*  59 */ "unary_expression ::= new_expression",
 /*  60 */ "postfix_expression ::= primary_expression",
 /*  61 */ "postfix_expression ::= postfix_expression ADDADD",
 /*  62 */ "postfix_expression ::= postfix_expression SUBSUB",
 /*  63 */ "postfix_expression ::= function_call",
 /*  64 */ "postfix_expression ::= postfix_expression DOT IDENTIFIER",
 /*  65 */ "postfix_expression ::= postfix_expression DOT function_call",
 /*  66 */ "postfix_expression ::= postfix_expression LBRACKET expression RBRACKET",
 /*  67 */ "primary_expression ::= literal",
 /*  68 */ "primary_expression ::= id_expression",
 /*  69 */ "primary_expression ::= LPAREN expression RPAREN",
 /*  70 */ "primary_expression ::= list_literal",
 /*  71 */ "primary_expression ::= THIS",
 /*  72 */ "literal ::= LIT_INTEGER",
 /*  73 */ "literal ::= LIT_HEX",
 /*  74 */ "literal ::= LIT_BIN",
 /*  75 */ "literal ::= LIT_ROM",
 /*  76 */ "literal ::= LIT_REAL",
 /*  77 */ "literal ::= LIT_STRING",
 /*  78 */ "literal ::= TRUE",
 /*  79 */ "literal ::= FALSE",
 /*  80 */ "literal ::= NULL",
 /*  81 */ "id_expression ::= IDENTIFIER",
 /*  82 */ "list_literal ::= LBRACKET list_content RBRACKET",
 /*  83 */ "list_content ::= list_entry",
 /*  84 */ "list_content ::= list_entry COMMA list_content",
 /*  85 */ "list_entry ::= expression",
 /*  86 */ "new_expression ::= NEW IDENTIFIER",
 /*  87 */ "type ::= BOOL",
 /*  88 */ "type ::= INT",
 /*  89 */ "type ::= STRING",
 /*  90 */ "type ::= IDENTIFIER",
 /*  91 */ "function_call ::= IDENTIFIER LPAREN argument_list RPAREN",
 /*  92 */ "positional_argument ::= expression",
 /*  93 */ "positional_argument_list ::= positional_argument",
 /*  94 */ "positional_argument_list ::= positional_argument_list COMMA positional_argument",
 /*  95 */ "named_argument ::= IDENTIFIER COLON expression",
 /*  96 */ "named_argument_list ::= named_argument",
 /*  97 */ "named_argument_list ::= named_argument_list COMMA named_argument",
 /*  98 */ "argument_list ::=",
 /*  99 */ "argument_list ::= positional_argument_list",
 /* 100 */ "argument_list ::= named_argument_list",
 /* 101 */ "expression_statement ::= SEMICOLON",
 /* 102 */ "expression_statement ::= expression SEMICOLON",
 /* 103 */ "compound_statement ::= LBRACE RBRACE",
 /* 104 */ "compound_statement ::= LBRACE statement_sequence RBRACE",
 /* 105 */ "include_statement ::= INCLUDE LIT_STRING SEMICOLON",
 /* 106 */ "return_statement ::= RETURN expression SEMICOLON",
 /* 107 */ "return_statement ::= RETURN SEMICOLON",
 /* 108 */ "break_statement ::= BREAK SEMICOLON",
 /* 109 */ "continue_statement ::= CONTINUE SEMICOLON",
 /* 110 */ "declaration_statement ::= function_declaration",
 /* 111 */ "declaration_statement ::= class_declaration SEMICOLON",
 /* 112 */ "declaration_statement ::= variable_declaration SEMICOLON",
 /* 113 */ "variable_declaration ::= VAR declarator_sequence",
 /* 114 */ "declarator ::= IDENTIFIER",
 /* 115 */ "declarator ::= IDENTIFIER ASSIGN expression",
 /* 116 */ "declarator_sequence ::= declarator",
 /* 117 */ "declarator_sequence ::= declarator_sequence COMMA declarator",
 /* 118 */ "class_declaration ::= CLASS IDENTIFIER LBRACE RBRACE",
 /* 119 */ "class_declaration ::= CLASS IDENTIFIER LBRACE class_members RBRACE",
 /* 120 */ "class_member ::= variable_declaration SEMICOLON",
 /* 121 */ "class_member ::= function_declaration",
 /* 122 */ "class_member ::= access_specifier variable_declaration SEMICOLON",
 /* 123 */ "class_member ::= access_specifier function_declaration",
 /* 124 */ "class_member ::= access_specifier COLON",
 /* 125 */ "access_specifier ::= PRIVATE",
 /* 126 */ "access_specifier ::= PROTECTED",
 /* 127 */ "access_specifier ::= PUBLIC",
 /* 128 */ "class_members ::= class_member",
 /* 129 */ "class_members ::= class_members class_member",
 /* 130 */ "function_declaration ::= FUNCTION IDENTIFIER LPAREN parameter_list RPAREN function_body",
 /* 131 */ "parameter ::= type IDENTIFIER",
 /* 132 */ "parameter ::= IDENTIFIER",
 /* 133 */ "opt_parameter ::= type IDENTIFIER ASSIGN expression",
 /* 134 */ "opt_parameter ::= IDENTIFIER ASSIGN expression",
 /* 135 */ "parameter_list ::=",
 /* 136 */ "parameter_list ::= parameters",
 /* 137 */ "parameter_list ::= opt_parameters",
 /* 138 */ "parameter_list ::= parameters COMMA opt_parameters",
 /* 139 */ "parameters ::= parameter",
 /* 140 */ "parameters ::= parameters COMMA parameter",
 /* 141 */ "opt_parameters ::= opt_parameter",
 /* 142 */ "opt_parameters ::= opt_parameters COMMA opt_parameter",
 /* 143 */ "function_body ::= statement",
 /* 144 */ "extern_declaration ::= EXTERN LIT_STRING type IDENTIFIER LPAREN parameter_list RPAREN SEMICOLON",
 /* 145 */ "for_statement ::= FOR LPAREN for_init_statement expression SEMICOLON expression_opt RPAREN statement",
 /* 146 */ "for_init_statement ::= expression_statement",
 /* 147 */ "for_init_statement ::= variable_declaration SEMICOLON",
 /* 148 */ "foreach_statement ::= FOR LPAREN id_expression IN expression RPAREN statement",
 /* 149 */ "foreach_statement ::= FOR LPAREN foreach_decl IN expression RPAREN statement",
 /* 150 */ "foreach_decl ::= VAR IDENTIFIER",
 /* 151 */ "if_statement ::= IF LPAREN expression RPAREN statement",
 /* 152 */ "if_statement ::= IF LPAREN expression RPAREN statement ELSE statement",
 /* 153 */ "while_statement ::= WHILE LPAREN expression RPAREN statement",
 /* 154 */ "switch_statement ::= SWITCH LPAREN expression RPAREN LBRACE switch_body RBRACE",
 /* 155 */ "switch_body ::=",
 /* 156 */ "switch_body ::= switch_body switch_case",
 /* 157 */ "switch_body ::= switch_body default_case",
 /* 158 */ "switch_case ::= CASE literal COLON case_statements",
 /* 159 */ "default_case ::= DEFAULT COLON case_statements",
 /* 160 */ "case_statements ::= statement_sequence",
 /* 161 */ "try_statement ::= TRY compound_statement catch_block",
 /* 162 */ "try_statement ::= TRY compound_statement finally_block",
 /* 163 */ "try_statement ::= TRY compound_statement catch_block finally_block",
 /* 164 */ "catch_block ::= CATCH LPAREN IDENTIFIER RPAREN compound_statement",
 /* 165 */ "finally_block ::= FINALLY compound_statement",
 /* 166 */ "throw_statement ::= THROW expression SEMICOLON",
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
  { 79, 1 },
  { 80, 1 },
  { 82, 1 },
  { 82, 2 },
  { 81, 0 },
  { 81, 1 },
  { 83, 1 },
  { 83, 1 },
  { 83, 1 },
  { 83, 1 },
  { 83, 1 },
  { 83, 1 },
  { 83, 1 },
  { 83, 1 },
  { 83, 1 },
  { 83, 1 },
  { 83, 1 },
  { 83, 1 },
  { 83, 1 },
  { 83, 1 },
  { 83, 1 },
  { 83, 1 },
  { 99, 1 },
  { 101, 0 },
  { 101, 1 },
  { 100, 1 },
  { 100, 3 },
  { 104, 1 },
  { 104, 1 },
  { 104, 1 },
  { 104, 1 },
  { 104, 1 },
  { 104, 1 },
  { 102, 1 },
  { 102, 5 },
  { 103, 1 },
  { 103, 3 },
  { 103, 3 },
  { 103, 3 },
  { 103, 3 },
  { 103, 3 },
  { 103, 3 },
  { 103, 3 },
  { 103, 3 },
  { 103, 3 },
  { 103, 3 },
  { 103, 3 },
  { 103, 3 },
  { 103, 3 },
  { 103, 3 },
  { 103, 3 },
  { 103, 3 },
  { 103, 3 },
  { 103, 3 },
  { 105, 1 },
  { 105, 2 },
  { 105, 2 },
  { 105, 2 },
  { 105, 2 },
  { 105, 1 },
  { 106, 1 },
  { 106, 2 },
  { 106, 2 },
  { 106, 1 },
  { 106, 3 },
  { 106, 3 },
  { 106, 4 },
  { 108, 1 },
  { 108, 1 },
  { 108, 3 },
  { 108, 1 },
  { 108, 1 },
  { 110, 1 },
  { 110, 1 },
  { 110, 1 },
  { 110, 1 },
  { 110, 1 },
  { 110, 1 },
  { 110, 1 },
  { 110, 1 },
  { 110, 1 },
  { 111, 1 },
  { 112, 3 },
  { 113, 1 },
  { 113, 3 },
  { 114, 1 },
  { 107, 2 },
  { 115, 1 },
  { 115, 1 },
  { 115, 1 },
  { 115, 1 },
  { 109, 4 },
  { 117, 1 },
  { 118, 1 },
  { 118, 3 },
  { 119, 3 },
  { 120, 1 },
  { 120, 3 },
  { 116, 0 },
  { 116, 1 },
  { 116, 1 },
  { 85, 1 },
  { 85, 2 },
  { 88, 2 },
  { 88, 3 },
  { 84, 3 },
  { 92, 3 },
  { 92, 2 },
  { 94, 2 },
  { 95, 2 },
  { 86, 1 },
  { 86, 2 },
  { 86, 2 },
  { 123, 2 },
  { 125, 1 },
  { 125, 3 },
  { 124, 1 },
  { 124, 3 },
  { 122, 4 },
  { 122, 5 },
  { 127, 2 },
  { 127, 1 },
  { 127, 3 },
  { 127, 2 },
  { 127, 2 },
  { 128, 1 },
  { 128, 1 },
  { 128, 1 },
  { 126, 1 },
  { 126, 2 },
  { 121, 6 },
  { 131, 2 },
  { 131, 1 },
  { 132, 4 },
  { 132, 3 },
  { 129, 0 },
  { 129, 1 },
  { 129, 1 },
  { 129, 3 },
  { 133, 1 },
  { 133, 3 },
  { 134, 1 },
  { 134, 3 },
  { 130, 1 },
  { 96, 8 },
  { 87, 8 },
  { 135, 1 },
  { 135, 2 },
  { 91, 7 },
  { 91, 7 },
  { 136, 2 },
  { 89, 5 },
  { 89, 7 },
  { 90, 5 },
  { 93, 7 },
  { 137, 0 },
  { 137, 2 },
  { 137, 2 },
  { 138, 4 },
  { 139, 3 },
  { 140, 1 },
  { 97, 3 },
  { 97, 3 },
  { 97, 4 },
  { 141, 5 },
  { 142, 2 },
  { 98, 3 },
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
{ p->SetRoot(yymsp[0].minor.yy235); }
#line 1312 "cscript.c"
        break;
      case 1:
#line 78 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(translation_unit, yymsp[0].minor.yy235); }
#line 1317 "cscript.c"
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
      case 19:
      case 20:
      case 22:
      case 24:
      case 25:
      case 33:
      case 35:
      case 54:
      case 59:
      case 60:
      case 63:
      case 67:
      case 68:
      case 70:
      case 92:
      case 110:
      case 113:
      case 116:
      case 143:
      case 146:
      case 160:
#line 81 "cscript.in"
{ yygotominor.yy235 = yymsp[0].minor.yy235; }
#line 1357 "cscript.c"
        break;
      case 3:
#line 82 "cscript.in"
{ 
  if(yymsp[-1].minor.yy235->m_type == statement_sequence) {
    yygotominor.yy235 = yymsp[-1].minor.yy235;
  }
  else {
    yygotominor.yy235 = p->AllocAst(statement_sequence, new AstList);
    yygotominor.yy235->m_a1.GetList()->push_back(yymsp[-1].minor.yy235);
  }
  yygotominor.yy235->m_a1.GetList()->push_back(yymsp[0].minor.yy235);
}
#line 1371 "cscript.c"
        break;
      case 4:
      case 23:
#line 95 "cscript.in"
{ yygotominor.yy235 = 0; }
#line 1377 "cscript.c"
        break;
      case 21:
      case 101:
#line 116 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(empty_statement); }
#line 1383 "cscript.c"
        break;
      case 26:
#line 132 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(assignment_expression, yymsp[-1].minor.yy172, yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1388 "cscript.c"
        break;
      case 27:
#line 136 "cscript.in"
{ yygotominor.yy172 = op_assign; }
#line 1393 "cscript.c"
        break;
      case 28:
#line 137 "cscript.in"
{ yygotominor.yy172 = op_assadd; }
#line 1398 "cscript.c"
        break;
      case 29:
#line 138 "cscript.in"
{ yygotominor.yy172 = op_asssub; }
#line 1403 "cscript.c"
        break;
      case 30:
#line 139 "cscript.in"
{ yygotominor.yy172 = op_assmul; }
#line 1408 "cscript.c"
        break;
      case 31:
#line 140 "cscript.in"
{ yygotominor.yy172 = op_assdiv; }
#line 1413 "cscript.c"
        break;
      case 32:
#line 141 "cscript.in"
{ yygotominor.yy172 = op_assmod; }
#line 1418 "cscript.c"
        break;
      case 34:
#line 145 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(ternary_expression, yymsp[-4].minor.yy235, yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1423 "cscript.c"
        break;
      case 36:
#line 149 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(binary_expression, op_logor,   yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1428 "cscript.c"
        break;
      case 37:
#line 150 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(binary_expression, op_logand,  yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1433 "cscript.c"
        break;
      case 38:
#line 151 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(binary_expression, op_bitor,   yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1438 "cscript.c"
        break;
      case 39:
#line 152 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(binary_expression, op_bitxor,  yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1443 "cscript.c"
        break;
      case 40:
#line 153 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(binary_expression, op_bitand,  yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1448 "cscript.c"
        break;
      case 41:
#line 154 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(binary_expression, op_eq,   yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1453 "cscript.c"
        break;
      case 42:
#line 155 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(binary_expression, op_ne,   yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1458 "cscript.c"
        break;
      case 43:
#line 156 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(binary_expression, op_lt,   yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1463 "cscript.c"
        break;
      case 44:
#line 157 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(binary_expression, op_le,   yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1468 "cscript.c"
        break;
      case 45:
#line 158 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(binary_expression, op_gt,   yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1473 "cscript.c"
        break;
      case 46:
#line 159 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(binary_expression, op_ge,   yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1478 "cscript.c"
        break;
      case 47:
#line 160 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(binary_expression, op_add,  yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1483 "cscript.c"
        break;
      case 48:
#line 161 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(binary_expression, op_sub,  yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1488 "cscript.c"
        break;
      case 49:
#line 162 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(binary_expression, op_mul,  yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1493 "cscript.c"
        break;
      case 50:
#line 163 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(binary_expression, op_div,  yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1498 "cscript.c"
        break;
      case 51:
#line 164 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(binary_expression, op_mod,  yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1503 "cscript.c"
        break;
      case 52:
#line 165 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(binary_expression, op_seq,  yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1508 "cscript.c"
        break;
      case 53:
#line 166 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(binary_expression, op_sne,  yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1513 "cscript.c"
        break;
      case 55:
#line 170 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(prefix_expression, op_negate, yymsp[0].minor.yy235); }
#line 1518 "cscript.c"
        break;
      case 56:
#line 171 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(prefix_expression, op_preinc, yymsp[0].minor.yy235); }
#line 1523 "cscript.c"
        break;
      case 57:
#line 172 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(prefix_expression, op_predec, yymsp[0].minor.yy235); }
#line 1528 "cscript.c"
        break;
      case 58:
#line 173 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(prefix_expression, op_not,    yymsp[0].minor.yy235); }
#line 1533 "cscript.c"
        break;
      case 61:
#line 178 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(postfix_expression, op_postinc, yymsp[-1].minor.yy235); }
#line 1538 "cscript.c"
        break;
      case 62:
#line 179 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(postfix_expression, op_postdec, yymsp[-1].minor.yy235); }
#line 1543 "cscript.c"
        break;
      case 64:
#line 181 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(member_expression, yymsp[-2].minor.yy235, String(yymsp[0].minor.yy0)); }
#line 1548 "cscript.c"
        break;
      case 65:
#line 182 "cscript.in"
{ yygotominor.yy235 = yymsp[0].minor.yy235; yymsp[0].minor.yy235->m_a3 = yymsp[-2].minor.yy235; }
#line 1553 "cscript.c"
        break;
      case 66:
#line 183 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(index_expression, yymsp[-3].minor.yy235, yymsp[-1].minor.yy235); }
#line 1558 "cscript.c"
        break;
      case 69:
      case 111:
      case 112:
      case 147:
#line 188 "cscript.in"
{ yygotominor.yy235 = yymsp[-1].minor.yy235; }
#line 1566 "cscript.c"
        break;
      case 71:
#line 190 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(this_expression); }
#line 1571 "cscript.c"
        break;
      case 72:
#line 193 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(literal_value, Variant(String(yymsp[0].minor.yy0), Variant::stInt)); }
#line 1576 "cscript.c"
        break;
      case 73:
#line 194 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(literal_value, Variant(hex2dec(String(yymsp[0].minor.yy0).c_str()))); }
#line 1581 "cscript.c"
        break;
      case 74:
#line 195 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(literal_value, Variant(bin2dec(String(yymsp[0].minor.yy0).c_str()))); }
#line 1586 "cscript.c"
        break;
      case 75:
#line 196 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(literal_value, Variant(rom2dec(String(yymsp[0].minor.yy0).c_str()))); }
#line 1591 "cscript.c"
        break;
      case 76:
#line 197 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(literal_value, Variant(String(yymsp[0].minor.yy0), Variant::stReal)); }
#line 1596 "cscript.c"
        break;
      case 77:
#line 198 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(literal_value, Variant(String(yymsp[0].minor.yy0), Variant::stString)); }
#line 1601 "cscript.c"
        break;
      case 78:
#line 199 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(literal_value, Variant(true));    }
#line 1606 "cscript.c"
        break;
      case 79:
#line 200 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(literal_value, Variant(false));   }
#line 1611 "cscript.c"
        break;
      case 80:
#line 201 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(literal_value, Variant());        }
#line 1616 "cscript.c"
        break;
      case 81:
#line 204 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(lvalue, String(yymsp[0].minor.yy0)); }
#line 1621 "cscript.c"
        break;
      case 82:
#line 207 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(list_literal, yymsp[-1].minor.yy235); }
#line 1626 "cscript.c"
        break;
      case 83:
#line 208 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(list_content, yymsp[0].minor.yy235); }
#line 1631 "cscript.c"
        break;
      case 84:
#line 209 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(list_content, yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1636 "cscript.c"
        break;
      case 85:
#line 211 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(list_entry, yymsp[0].minor.yy235); }
#line 1641 "cscript.c"
        break;
      case 86:
#line 214 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(new_expression, String(yymsp[0].minor.yy0)); }
#line 1646 "cscript.c"
        break;
      case 87:
#line 221 "cscript.in"
{ yygotominor.yy235 = new Ast(builtin_type, Variant::stBool);    }
#line 1651 "cscript.c"
        break;
      case 88:
#line 222 "cscript.in"
{ yygotominor.yy235 = new Ast(builtin_type, Variant::stInt);     }
#line 1656 "cscript.c"
        break;
      case 89:
#line 223 "cscript.in"
{ yygotominor.yy235 = new Ast(builtin_type, Variant::stString);  }
#line 1661 "cscript.c"
        break;
      case 90:
#line 224 "cscript.in"
{ yygotominor.yy235 = new Ast(class_type, String(yymsp[0].minor.yy0));            }
#line 1666 "cscript.c"
        break;
      case 91:
#line 232 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(function_call, String(yymsp[-3].minor.yy0), yymsp[-1].minor.yy235); }
#line 1671 "cscript.c"
        break;
      case 93:
      case 96:
      case 139:
      case 141:
#line 239 "cscript.in"
{ yygotominor.yy259 = new AstList; yygotominor.yy259->push_back(yymsp[0].minor.yy235); }
#line 1679 "cscript.c"
        break;
      case 94:
      case 97:
      case 140:
      case 142:
#line 240 "cscript.in"
{ yygotominor.yy259 = yymsp[-2].minor.yy259; yymsp[-2].minor.yy259->push_back(yymsp[0].minor.yy235); }
#line 1687 "cscript.c"
        break;
      case 95:
#line 243 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(named_argument, String(yymsp[-2].minor.yy0), yymsp[0].minor.yy235); }
#line 1692 "cscript.c"
        break;
      case 98:
#line 251 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(positional_arguments, new AstList); }
#line 1697 "cscript.c"
        break;
      case 99:
#line 252 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(positional_arguments, yymsp[0].minor.yy259); }
#line 1702 "cscript.c"
        break;
      case 100:
#line 253 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(named_arguments,      yymsp[0].minor.yy259); }
#line 1707 "cscript.c"
        break;
      case 102:
#line 262 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(expression_statement, yymsp[-1].minor.yy235); }
#line 1712 "cscript.c"
        break;
      case 103:
#line 265 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(compound_statement); }
#line 1717 "cscript.c"
        break;
      case 104:
#line 266 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(compound_statement, yymsp[-1].minor.yy235); }
#line 1722 "cscript.c"
        break;
      case 105:
#line 269 "cscript.in"
{ p->Parse(yymsp[-1].minor.yy0); yygotominor.yy235 = p->GetRoot(); }
#line 1727 "cscript.c"
        break;
      case 106:
#line 272 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(return_statement, yymsp[-1].minor.yy235); }
#line 1732 "cscript.c"
        break;
      case 107:
#line 273 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(return_statement);    }
#line 1737 "cscript.c"
        break;
      case 108:
#line 276 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(break_statement); }
#line 1742 "cscript.c"
        break;
      case 109:
#line 277 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(continue_statement); }
#line 1747 "cscript.c"
        break;
      case 114:
#line 291 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(variable_declaration, String(yymsp[0].minor.yy0));    }
#line 1752 "cscript.c"
        break;
      case 115:
#line 292 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(variable_declaration, String(yymsp[-2].minor.yy0), yymsp[0].minor.yy235); }
#line 1757 "cscript.c"
        break;
      case 117:
#line 295 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(declaration_sequence, yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1762 "cscript.c"
        break;
      case 118:
#line 302 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(class_declaration, String(yymsp[-2].minor.yy0)); }
#line 1767 "cscript.c"
        break;
      case 119:
#line 303 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(class_declaration, String(yymsp[-3].minor.yy0), yymsp[-1].minor.yy259); }
#line 1772 "cscript.c"
        break;
      case 120:
#line 306 "cscript.in"
{ yygotominor.yy235 = yymsp[-1].minor.yy235; yymsp[-1].minor.yy235->m_props["access"] = accessDefault; }
#line 1777 "cscript.c"
        break;
      case 121:
#line 307 "cscript.in"
{ yygotominor.yy235 = yymsp[0].minor.yy235; yymsp[0].minor.yy235->m_props["access"] = accessDefault; }
#line 1782 "cscript.c"
        break;
      case 122:
#line 308 "cscript.in"
{ yygotominor.yy235 = yymsp[-1].minor.yy235; yymsp[-1].minor.yy235->m_props["access"] = yymsp[-2].minor.yy128; }
#line 1787 "cscript.c"
        break;
      case 123:
#line 309 "cscript.in"
{ yygotominor.yy235 = yymsp[0].minor.yy235; yymsp[0].minor.yy235->m_props["access"] = yymsp[-1].minor.yy128; }
#line 1792 "cscript.c"
        break;
      case 124:
#line 310 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(access_specifier, yymsp[-1].minor.yy128); }
#line 1797 "cscript.c"
        break;
      case 125:
#line 314 "cscript.in"
{ yygotominor.yy128 = accessPrivate;   }
#line 1802 "cscript.c"
        break;
      case 126:
#line 315 "cscript.in"
{ yygotominor.yy128 = accessProtected; }
#line 1807 "cscript.c"
        break;
      case 127:
#line 316 "cscript.in"
{ yygotominor.yy128 = accessPublic;    }
#line 1812 "cscript.c"
        break;
      case 128:
#line 320 "cscript.in"
{ 
  yygotominor.yy259 = new AstList;
  yygotominor.yy259->push_back(yymsp[0].minor.yy235);
}
#line 1820 "cscript.c"
        break;
      case 129:
#line 324 "cscript.in"
{ 
  yygotominor.yy259 = yymsp[-1].minor.yy259;
  yygotominor.yy259->push_back(yymsp[0].minor.yy235);
}
#line 1828 "cscript.c"
        break;
      case 130:
#line 335 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(function_declaration, String(yymsp[-4].minor.yy0), yymsp[-2].minor.yy259, yymsp[0].minor.yy235); }
#line 1833 "cscript.c"
        break;
      case 131:
#line 338 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(parameter, String(yymsp[0].minor.yy0), yymsp[-1].minor.yy235); }
#line 1838 "cscript.c"
        break;
      case 132:
#line 339 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(parameter, String(yymsp[0].minor.yy0)); }
#line 1843 "cscript.c"
        break;
      case 133:
#line 342 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(parameter, String(yymsp[-2].minor.yy0), yymsp[-3].minor.yy235,         yymsp[0].minor.yy235); }
#line 1848 "cscript.c"
        break;
      case 134:
#line 343 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(parameter, String(yymsp[-2].minor.yy0), AstData(), yymsp[0].minor.yy235); }
#line 1853 "cscript.c"
        break;
      case 135:
      case 155:
#line 347 "cscript.in"
{ yygotominor.yy259 = new AstList; }
#line 1859 "cscript.c"
        break;
      case 136:
      case 137:
#line 348 "cscript.in"
{ yygotominor.yy259 = yymsp[0].minor.yy259; }
#line 1865 "cscript.c"
        break;
      case 138:
#line 350 "cscript.in"
{ yygotominor.yy259 = yymsp[-2].minor.yy259; yymsp[-2].minor.yy259->adopt(*yymsp[0].minor.yy259); }
#line 1870 "cscript.c"
        break;
      case 144:
#line 373 "cscript.in"
{ 
  yygotominor.yy235 = new Ast(extern_declaration, String(yymsp[-4].minor.yy0), yymsp[-2].minor.yy259, String(yymsp[-6].minor.yy0), yymsp[-5].minor.yy235); 
}
#line 1877 "cscript.c"
        break;
      case 145:
#line 383 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(for_statement, yymsp[-5].minor.yy235, yymsp[-4].minor.yy235, yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1882 "cscript.c"
        break;
      case 148:
      case 149:
#line 394 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(foreach_statement, yymsp[-4].minor.yy235, yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1888 "cscript.c"
        break;
      case 150:
#line 396 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(variable_declaration, String(yymsp[0].minor.yy0)); }
#line 1893 "cscript.c"
        break;
      case 151:
#line 407 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(if_statement, yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1898 "cscript.c"
        break;
      case 152:
#line 408 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(if_statement, yymsp[-4].minor.yy235, yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1903 "cscript.c"
        break;
      case 153:
#line 416 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(while_statement, yymsp[-2].minor.yy235,  yymsp[0].minor.yy235); }
#line 1908 "cscript.c"
        break;
      case 154:
#line 424 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(switch_statement, yymsp[-4].minor.yy235, yymsp[-1].minor.yy259); }
#line 1913 "cscript.c"
        break;
      case 156:
      case 157:
#line 429 "cscript.in"
{ yygotominor.yy259 = yymsp[-1].minor.yy259; yygotominor.yy259->push_back(yymsp[0].minor.yy235); }
#line 1919 "cscript.c"
        break;
      case 158:
#line 433 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(switch_case, yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1924 "cscript.c"
        break;
      case 159:
#line 436 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(default_case, yymsp[0].minor.yy235); }
#line 1929 "cscript.c"
        break;
      case 161:
#line 446 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(try_statement, yymsp[-1].minor.yy235, yymsp[0].minor.yy235); }
#line 1934 "cscript.c"
        break;
      case 162:
#line 447 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(try_statement, yymsp[-1].minor.yy235, AstData(), yymsp[0].minor.yy235); }
#line 1939 "cscript.c"
        break;
      case 163:
#line 448 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(try_statement, yymsp[-2].minor.yy235, yymsp[-1].minor.yy235, yymsp[0].minor.yy235); }
#line 1944 "cscript.c"
        break;
      case 164:
#line 450 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(catch_block, String(yymsp[-2].minor.yy0), yymsp[0].minor.yy235); }
#line 1949 "cscript.c"
        break;
      case 165:
#line 452 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(finally_block, yymsp[0].minor.yy235); }
#line 1954 "cscript.c"
        break;
      case 166:
#line 454 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(throw_statement, yymsp[-1].minor.yy235); }
#line 1959 "cscript.c"
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
#line 2007 "cscript.c"
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
#line 2025 "cscript.c"
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

