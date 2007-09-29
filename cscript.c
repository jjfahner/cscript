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
#define YYNSTATE 296
#define YYNRULE 169
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
 /*     0 */   287,  466,  288,  280,   12,  277,  253,  252,  239,  234,
 /*    10 */   232,  227,  223,  190,  186,  193,  194,  220,  233,  237,
 /*    20 */   274,  137,  286,  100,  285,   70,   65,  279,   94,  264,
 /*    30 */   263,  262,  261,  258,  255,   57,  213,  141,   26,  218,
 /*    40 */   216,  107,  135,  175,  163,  156,   14,   58,   56,   55,
 /*    50 */    61,  112,   17,  178,   25,  152,  254,  251,  250,  246,
 /*    60 */   245,  244,  243,  242,  241,  238,  125,  131,   41,   54,
 /*    70 */    40,  197,    3,  295,  142,   20,  149,  151,   68,  165,
 /*    80 */    27,  159,  166,  150,  140,  128,  174,   57,  155,  126,
 /*    90 */   160,  119,  293,   66,   74,  172,  428,   28,   23,   58,
 /*   100 */    56,   55,  247,  112,   17,  259,   25,  428,  254,  251,
 /*   110 */   250,  246,  245,  244,  243,  242,  241,  238,  103,  131,
 /*   120 */    22,   71,  124,  197,    3,  189,  142,   20,  149,  151,
 /*   130 */    68,  165,   73,  260,    3,  150,  140,  128,   23,   57,
 /*   140 */   179,  126,  160,  119,  127,  448,   74,  105,  265,   28,
 /*   150 */    24,   58,   56,   55,   75,  112,   17,  121,   25,  212,
 /*   160 */   254,  251,  250,  246,  245,  244,  243,  242,  241,  238,
 /*   170 */   130,  131,   68,   97,  214,  197,    3,  150,  142,   20,
 /*   180 */   149,  151,   68,  165,  129,   78,   16,  150,  140,  128,
 /*   190 */   183,  184,  185,  126,  160,  119,  248,  249,   74,  276,
 /*   200 */   275,   28,   76,  287,   33,    8,  167,   11,  277,  253,
 /*   210 */   252,  239,  234,  232,  227,  223,  190,  186,  193,  194,
 /*   220 */   220,  233,  237,  274,  137,  286,  100,  285,   70,  170,
 /*   230 */   279,   94,  264,  263,  262,  261,  258,  255,  161,  173,
 /*   240 */   120,   67,  218,  216,  107,  135,  175,  163,  156,   13,
 /*   250 */    83,    9,  279,   94,  264,  263,  262,  261,  258,  255,
 /*   260 */   257,  177,  287,   78,  176,  269,   11,  277,  253,  252,
 /*   270 */   239,  234,  232,  227,  223,  190,  186,  193,  194,  220,
 /*   280 */   233,  237,  274,  137,  286,  115,  285,   70,  138,  279,
 /*   290 */    94,  264,  263,  262,  261,  258,  255,   39,   37,   41,
 /*   300 */    54,   40,  183,  184,  185,  175,  163,  156,  230,  286,
 /*   310 */   100,  285,   70,  148,  279,   94,  264,  263,  262,  261,
 /*   320 */   258,  255,  231,  158,  272,  287,  215,  216,  102,  111,
 /*   330 */   192,  253,  252,  239,  234,  232,  227,  223,  190,  186,
 /*   340 */   193,  194,  220,  233,  237,  274,  137,  286,  180,  285,
 /*   350 */    70,   60,  279,   94,  264,  263,  262,  261,  258,  255,
 /*   360 */   266,   94,  264,  263,  262,  261,  258,  255,  175,  163,
 /*   370 */   156,   35,   99,  284,  287,  182,  164,  191,    5,  277,
 /*   380 */   253,  252,  239,  234,  232,  227,  223,  190,  186,  193,
 /*   390 */   194,  220,  233,  237,  274,  137,  286,   36,  285,   70,
 /*   400 */    80,  279,   94,  264,  263,  262,  261,  258,  255,    6,
 /*   410 */     2,  187,  204,  206,  143,  287,  236,  175,  163,  156,
 /*   420 */   281,  253,  252,  239,  234,  232,  227,  223,  190,  186,
 /*   430 */   193,  194,  220,  233,  237,  274,  137,  286,    7,  285,
 /*   440 */    70,    4,  279,   94,  264,  263,  262,  261,  258,  255,
 /*   450 */    72,  225,  136,   63,   31,   10,  287,  101,  175,  163,
 /*   460 */   156,  228,  253,  252,  239,  234,  232,  227,  223,  190,
 /*   470 */   186,  193,  194,  220,  233,  237,  274,  137,  286,  196,
 /*   480 */   285,   70,   59,  279,   94,  264,  263,  262,  261,  258,
 /*   490 */   255,  256,   64,   79,  221,   14,   18,  287,  229,  175,
 /*   500 */   163,  156,  271,  253,  252,  239,  234,  232,  227,  223,
 /*   510 */   190,  186,  193,  194,  220,  233,  237,  274,  137,  286,
 /*   520 */    19,  285,   70,   27,  279,   94,  264,  263,  262,  261,
 /*   530 */   258,  255,  116,  118,   15,   34,  211,   26,  287,   32,
 /*   540 */   175,  163,  156,  157,  253,  252,  239,  234,  232,  227,
 /*   550 */   223,  190,  186,  193,  194,  220,  233,  237,  274,  137,
 /*   560 */   286,   77,  285,   70,   21,  279,   94,  264,  263,  262,
 /*   570 */   261,  258,  255,    1,  133,  123,  268,  467,  467,  287,
 /*   580 */   467,  175,  163,  156,  240,  253,  252,  239,  234,  232,
 /*   590 */   227,  223,  190,  186,  193,  194,  220,  233,  237,  274,
 /*   600 */   137,  286,  467,  285,   70,  467,  279,   94,  264,  263,
 /*   610 */   262,  261,  258,  255,  467,  467,  467,  467,  467,  467,
 /*   620 */   287,  467,  175,  163,  156,  235,  253,  252,  239,  234,
 /*   630 */   232,  227,  223,  190,  186,  193,  194,  220,  233,  237,
 /*   640 */   274,  137,  286,  467,  285,   70,  467,  279,   94,  264,
 /*   650 */   263,  262,  261,  258,  255,  467,  467,  467,  467,  467,
 /*   660 */   467,  287,  467,  175,  163,  156,  289,  253,  252,  239,
 /*   670 */   234,  232,  227,  223,  190,  186,  193,  194,  220,  233,
 /*   680 */   237,  274,  137,  286,  467,  285,   70,  467,  279,   94,
 /*   690 */   264,  263,  262,  261,  258,  255,  267,   94,  264,  263,
 /*   700 */   262,  261,  258,  255,  175,  163,  156,   53,   52,   51,
 /*   710 */    50,   49,   38,   47,   48,   46,   45,   44,   43,   42,
 /*   720 */    39,   37,   41,   54,   40,  210,  209,  208,  207,  205,
 /*   730 */   203,   29,   57,   48,   46,   45,   44,   43,   42,   39,
 /*   740 */    37,   41,   54,   40,   58,   56,   55,  467,  112,   17,
 /*   750 */   467,   25,  467,  254,  251,  250,  246,  245,  244,  243,
 /*   760 */   242,  241,  238,  467,  131,   57,  467,  174,  197,  155,
 /*   770 */   467,  467,   62,  294,   66,   69,  467,   58,   56,   55,
 /*   780 */   467,  112,   17,  467,   25,  467,  254,  251,  250,  246,
 /*   790 */   245,  244,  243,  242,  241,  238,  467,  131,   57,  467,
 /*   800 */   467,  181,  273,   94,  264,  263,  262,  261,  258,  255,
 /*   810 */    58,   56,   55,  467,  112,   17,  467,   25,  467,  254,
 /*   820 */   251,  250,  246,  245,  244,  243,  242,  241,  238,  467,
 /*   830 */   131,   57,   82,  467,  279,   94,  264,  263,  262,  261,
 /*   840 */   258,  255,  467,   58,   56,   55,  467,   98,   17,  467,
 /*   850 */    25,  467,  254,  251,  250,  246,  245,  244,  243,  242,
 /*   860 */   241,  238,  467,  131,   52,   51,   50,   49,   38,   47,
 /*   870 */    48,   46,   45,   44,   43,   42,   39,   37,   41,   54,
 /*   880 */    40,  224,  286,  467,  285,   70,  195,  279,   94,  264,
 /*   890 */   263,  262,  261,  258,  255,  467,  282,  467,  106,  222,
 /*   900 */   113,  217,  134,  183,  184,  185,  467,  467,  467,  467,
 /*   910 */   137,  286,  467,  285,   70,  467,  279,   94,  264,  263,
 /*   920 */   262,  261,  114,  255,   84,  467,  279,   94,  264,  263,
 /*   930 */   262,  261,  258,  255,  147,   81,  467,  279,   94,  264,
 /*   940 */   263,  262,  261,  258,  255,  467,   30,  162,  467,  467,
 /*   950 */    51,   50,   49,   38,   47,   48,   46,   45,   44,   43,
 /*   960 */    42,   39,   37,   41,   54,   40,   50,   49,   38,   47,
 /*   970 */    48,   46,   45,   44,   43,   42,   39,   37,   41,   54,
 /*   980 */    40,   49,   38,   47,   48,   46,   45,   44,   43,   42,
 /*   990 */    39,   37,   41,   54,   40,  467,  230,  286,  467,  285,
 /*  1000 */    70,  467,  279,   94,  264,  263,  262,  261,  258,  255,
 /*  1010 */   139,  158,   38,   47,   48,   46,   45,   44,   43,   42,
 /*  1020 */    39,   37,   41,   54,   40,  251,  250,  246,  245,  244,
 /*  1030 */   243,  242,  241,  238,  278,  286,  144,  285,   70,  467,
 /*  1040 */   279,   94,  264,  263,  262,  261,  258,  255,  467,  467,
 /*  1050 */   224,  286,  467,  285,   70,  467,  279,   94,  264,  263,
 /*  1060 */   262,  261,  258,  255,  467,  467,  168,  286,  219,  285,
 /*  1070 */    70,  467,  279,   94,  264,  263,  262,  261,  258,  255,
 /*  1080 */   132,  286,  467,  285,   70,  467,  279,   94,  264,  263,
 /*  1090 */   262,  261,  258,  255,  146,  286,  467,  285,   70,  467,
 /*  1100 */   279,   94,  264,  263,  262,  261,  258,  255,  145,  286,
 /*  1110 */   467,  285,   70,  467,  279,   94,  264,  263,  262,  261,
 /*  1120 */   258,  255,  153,  286,  467,  285,   70,  467,  279,   94,
 /*  1130 */   264,  263,  262,  261,  258,  255,  122,  286,  467,  285,
 /*  1140 */    70,  467,  279,   94,  264,  263,  262,  261,  258,  255,
 /*  1150 */   109,  286,  467,  285,   70,  467,  279,   94,  264,  263,
 /*  1160 */   262,  261,  258,  255,  467,  467,  188,  286,  467,  285,
 /*  1170 */    70,  467,  279,   94,  264,  263,  262,  261,  258,  255,
 /*  1180 */   117,  286,  467,  285,   70,  467,  279,   94,  264,  263,
 /*  1190 */   262,  261,  258,  255,  226,  286,  467,  285,   70,  467,
 /*  1200 */   279,   94,  264,  263,  262,  261,  258,  255,  202,  286,
 /*  1210 */   467,  285,   70,  467,  279,   94,  264,  263,  262,  261,
 /*  1220 */   258,  255,  110,  286,  467,  285,   70,  467,  279,   94,
 /*  1230 */   264,  263,  262,  261,  258,  255,  108,  286,  467,  285,
 /*  1240 */    70,  467,  279,   94,  264,  263,  262,  261,  258,  255,
 /*  1250 */   104,  286,  467,  285,   70,  467,  279,   94,  264,  263,
 /*  1260 */   262,  261,  258,  255,  467,  467,  154,  286,  467,  285,
 /*  1270 */    70,  467,  279,   94,  264,  263,  262,  261,  258,  255,
 /*  1280 */   283,  467,  285,   70,  467,  279,   94,  264,  263,  262,
 /*  1290 */   261,  258,  255,  201,  467,  285,   70,  467,  279,   94,
 /*  1300 */   264,  263,  262,  261,  258,  255,   85,  467,  279,   94,
 /*  1310 */   264,  263,  262,  261,  258,  255,  199,  467,  279,   94,
 /*  1320 */   264,  263,  262,  261,  258,  255,   88,  467,  279,   94,
 /*  1330 */   264,  263,  262,  261,  258,  255,   87,  467,  279,   94,
 /*  1340 */   264,  263,  262,  261,  258,  255,   45,   44,   43,   42,
 /*  1350 */    39,   37,   41,   54,   40,   89,  467,  279,   94,  264,
 /*  1360 */   263,  262,  261,  258,  255,   93,  467,  279,   94,  264,
 /*  1370 */   263,  262,  261,  258,  255,  467,   90,  467,  279,   94,
 /*  1380 */   264,  263,  262,  261,  258,  255,   95,  467,  279,   94,
 /*  1390 */   264,  263,  262,  261,  258,  255,   86,  467,  279,   94,
 /*  1400 */   264,  263,  262,  261,  258,  255,   96,  467,  279,   94,
 /*  1410 */   264,  263,  262,  261,  258,  255,  198,  467,  279,   94,
 /*  1420 */   264,  263,  262,  261,  258,  255,  200,  467,  279,   94,
 /*  1430 */   264,  263,  262,  261,  258,  255,   92,  467,  279,   94,
 /*  1440 */   264,  263,  262,  261,  258,  255,   91,  467,  279,   94,
 /*  1450 */   264,  263,  262,  261,  258,  255,  270,   94,  264,  263,
 /*  1460 */   262,  261,  258,  255,  169,  467,  467,  467,  467,   68,
 /*  1470 */   171,  290,  291,  292,  150,   68,  467,  290,  291,  292,
 /*  1480 */   150,
};
static const YYCODETYPE yy_lookahead[] = {
 /*     0 */    78,   79,   80,   81,   82,   83,   84,   85,   86,   87,
 /*    10 */    88,   89,   90,   91,   92,   93,   94,   95,   96,   97,
 /*    20 */    98,   99,  100,  115,  102,  103,  137,  105,  106,  107,
 /*    30 */   108,  109,  110,  111,  112,   15,  119,  129,   26,  131,
 /*    40 */   132,  133,  134,  121,  122,  123,   34,   27,   28,   29,
 /*    50 */    52,   31,   32,  121,   34,  123,   36,   37,   38,   39,
 /*    60 */    40,   41,   42,   43,   44,   45,   31,   47,   16,   17,
 /*    70 */    18,   51,   52,   53,   54,   55,   56,   57,   58,   59,
 /*    80 */    19,  124,  125,   63,   64,   65,  121,   15,  123,   69,
 /*    90 */    70,   71,  127,  128,   74,   51,   35,   77,   19,   27,
 /*   100 */    28,   29,   53,   31,   32,   88,   34,   46,   36,   37,
 /*   110 */    38,   39,   40,   41,   42,   43,   44,   45,    5,   47,
 /*   120 */    66,   72,   73,   51,   52,   53,   54,   55,   56,   57,
 /*   130 */    58,   59,  141,  142,   52,   63,   64,   65,   19,   15,
 /*   140 */    26,   69,   70,   71,   31,   66,   74,    5,   88,   77,
 /*   150 */    34,   27,   28,   29,   46,   31,   32,  115,   34,  109,
 /*   160 */    36,   37,   38,   39,   40,   41,   42,   43,   44,   45,
 /*   170 */    31,   47,   58,   31,  132,   51,   52,   63,   54,   55,
 /*   180 */    56,   57,   58,   59,   75,   76,   46,   63,   64,   65,
 /*   190 */    48,   49,   50,   69,   70,   71,  138,  139,   74,   27,
 /*   200 */    28,   77,   30,   78,   32,   68,  125,   82,   83,   84,
 /*   210 */    85,   86,   87,   88,   89,   90,   91,   92,   93,   94,
 /*   220 */    95,   96,   97,   98,   99,  100,  115,  102,  103,   51,
 /*   230 */   105,  106,  107,  108,  109,  110,  111,  112,   31,   51,
 /*   240 */   129,   88,  131,  132,  133,  134,  121,  122,  123,   35,
 /*   250 */   103,   35,  105,  106,  107,  108,  109,  110,  111,  112,
 /*   260 */   142,   51,   78,   76,   51,  140,   82,   83,   84,   85,
 /*   270 */    86,   87,   88,   89,   90,   91,   92,   93,   94,   95,
 /*   280 */    96,   97,   98,   99,  100,   31,  102,  103,  115,  105,
 /*   290 */   106,  107,  108,  109,  110,  111,  112,   14,   15,   16,
 /*   300 */    17,   18,   48,   49,   50,  121,  122,  123,   99,  100,
 /*   310 */   115,  102,  103,   31,  105,  106,  107,  108,  109,  110,
 /*   320 */   111,  112,  113,  114,  140,   78,  131,  132,  110,  134,
 /*   330 */    83,   84,   85,   86,   87,   88,   89,   90,   91,   92,
 /*   340 */    93,   94,   95,   96,   97,   98,   99,  100,   51,  102,
 /*   350 */   103,   34,  105,  106,  107,  108,  109,  110,  111,  112,
 /*   360 */   105,  106,  107,  108,  109,  110,  111,  112,  121,  122,
 /*   370 */   123,  104,   31,   51,   78,   51,   31,  130,   82,   83,
 /*   380 */    84,   85,   86,   87,   88,   89,   90,   91,   92,   93,
 /*   390 */    94,   95,   96,   97,   98,   99,  100,   26,  102,  103,
 /*   400 */    52,  105,  106,  107,  108,  109,  110,  111,  112,   35,
 /*   410 */    26,   51,   31,   31,   42,   78,   33,  121,  122,  123,
 /*   420 */    83,   84,   85,   86,   87,   88,   89,   90,   91,   92,
 /*   430 */    93,   94,   95,   96,   97,   98,   99,  100,   35,  102,
 /*   440 */   103,   35,  105,  106,  107,  108,  109,  110,  111,  112,
 /*   450 */    42,   35,   31,   46,   66,   35,   78,   35,  121,  122,
 /*   460 */   123,   83,   84,   85,   86,   87,   88,   89,   90,   91,
 /*   470 */    92,   93,   94,   95,   96,   97,   98,   99,  100,   51,
 /*   480 */   102,  103,   34,  105,  106,  107,  108,  109,  110,  111,
 /*   490 */   112,   35,   46,   46,   51,   34,   46,   78,   31,  121,
 /*   500 */   122,  123,   83,   84,   85,   86,   87,   88,   89,   90,
 /*   510 */    91,   92,   93,   94,   95,   96,   97,   98,   99,  100,
 /*   520 */    51,  102,  103,   19,  105,  106,  107,  108,  109,  110,
 /*   530 */   111,  112,   34,   31,   34,   19,   33,   26,   78,   34,
 /*   540 */   121,  122,  123,   83,   84,   85,   86,   87,   88,   89,
 /*   550 */    90,   91,   92,   93,   94,   95,   96,   97,   98,   99,
 /*   560 */   100,   35,  102,  103,   34,  105,  106,  107,  108,  109,
 /*   570 */   110,  111,  112,   26,   35,   31,   51,  143,  143,   78,
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
 /*   880 */    18,   99,  100,  143,  102,  103,   31,  105,  106,  107,
 /*   890 */   108,  109,  110,  111,  112,  143,   85,  143,  116,  117,
 /*   900 */   118,  119,  120,   48,   49,   50,  143,  143,  143,  143,
 /*   910 */    99,  100,  143,  102,  103,  143,  105,  106,  107,  108,
 /*   920 */   109,  110,  111,  112,  103,  143,  105,  106,  107,  108,
 /*   930 */   109,  110,  111,  112,  123,  103,  143,  105,  106,  107,
 /*   940 */   108,  109,  110,  111,  112,  143,  135,  136,  143,  143,
 /*   950 */     3,    4,    5,    6,    7,    8,    9,   10,   11,   12,
 /*   960 */    13,   14,   15,   16,   17,   18,    4,    5,    6,    7,
 /*   970 */     8,    9,   10,   11,   12,   13,   14,   15,   16,   17,
 /*   980 */    18,    5,    6,    7,    8,    9,   10,   11,   12,   13,
 /*   990 */    14,   15,   16,   17,   18,  143,   99,  100,  143,  102,
 /*  1000 */   103,  143,  105,  106,  107,  108,  109,  110,  111,  112,
 /*  1010 */   113,  114,    6,    7,    8,    9,   10,   11,   12,   13,
 /*  1020 */    14,   15,   16,   17,   18,   37,   38,   39,   40,   41,
 /*  1030 */    42,   43,   44,   45,   99,  100,  101,  102,  103,  143,
 /*  1040 */   105,  106,  107,  108,  109,  110,  111,  112,  143,  143,
 /*  1050 */    99,  100,  143,  102,  103,  143,  105,  106,  107,  108,
 /*  1060 */   109,  110,  111,  112,  143,  143,   99,  100,  117,  102,
 /*  1070 */   103,  143,  105,  106,  107,  108,  109,  110,  111,  112,
 /*  1080 */    99,  100,  143,  102,  103,  143,  105,  106,  107,  108,
 /*  1090 */   109,  110,  111,  112,   99,  100,  143,  102,  103,  143,
 /*  1100 */   105,  106,  107,  108,  109,  110,  111,  112,   99,  100,
 /*  1110 */   143,  102,  103,  143,  105,  106,  107,  108,  109,  110,
 /*  1120 */   111,  112,   99,  100,  143,  102,  103,  143,  105,  106,
 /*  1130 */   107,  108,  109,  110,  111,  112,   99,  100,  143,  102,
 /*  1140 */   103,  143,  105,  106,  107,  108,  109,  110,  111,  112,
 /*  1150 */    99,  100,  143,  102,  103,  143,  105,  106,  107,  108,
 /*  1160 */   109,  110,  111,  112,  143,  143,   99,  100,  143,  102,
 /*  1170 */   103,  143,  105,  106,  107,  108,  109,  110,  111,  112,
 /*  1180 */    99,  100,  143,  102,  103,  143,  105,  106,  107,  108,
 /*  1190 */   109,  110,  111,  112,   99,  100,  143,  102,  103,  143,
 /*  1200 */   105,  106,  107,  108,  109,  110,  111,  112,   99,  100,
 /*  1210 */   143,  102,  103,  143,  105,  106,  107,  108,  109,  110,
 /*  1220 */   111,  112,   99,  100,  143,  102,  103,  143,  105,  106,
 /*  1230 */   107,  108,  109,  110,  111,  112,   99,  100,  143,  102,
 /*  1240 */   103,  143,  105,  106,  107,  108,  109,  110,  111,  112,
 /*  1250 */    99,  100,  143,  102,  103,  143,  105,  106,  107,  108,
 /*  1260 */   109,  110,  111,  112,  143,  143,   99,  100,  143,  102,
 /*  1270 */   103,  143,  105,  106,  107,  108,  109,  110,  111,  112,
 /*  1280 */   100,  143,  102,  103,  143,  105,  106,  107,  108,  109,
 /*  1290 */   110,  111,  112,  100,  143,  102,  103,  143,  105,  106,
 /*  1300 */   107,  108,  109,  110,  111,  112,  103,  143,  105,  106,
 /*  1310 */   107,  108,  109,  110,  111,  112,  103,  143,  105,  106,
 /*  1320 */   107,  108,  109,  110,  111,  112,  103,  143,  105,  106,
 /*  1330 */   107,  108,  109,  110,  111,  112,  103,  143,  105,  106,
 /*  1340 */   107,  108,  109,  110,  111,  112,   10,   11,   12,   13,
 /*  1350 */    14,   15,   16,   17,   18,  103,  143,  105,  106,  107,
 /*  1360 */   108,  109,  110,  111,  112,  103,  143,  105,  106,  107,
 /*  1370 */   108,  109,  110,  111,  112,  143,  103,  143,  105,  106,
 /*  1380 */   107,  108,  109,  110,  111,  112,  103,  143,  105,  106,
 /*  1390 */   107,  108,  109,  110,  111,  112,  103,  143,  105,  106,
 /*  1400 */   107,  108,  109,  110,  111,  112,  103,  143,  105,  106,
 /*  1410 */   107,  108,  109,  110,  111,  112,  103,  143,  105,  106,
 /*  1420 */   107,  108,  109,  110,  111,  112,  103,  143,  105,  106,
 /*  1430 */   107,  108,  109,  110,  111,  112,  103,  143,  105,  106,
 /*  1440 */   107,  108,  109,  110,  111,  112,  103,  143,  105,  106,
 /*  1450 */   107,  108,  109,  110,  111,  112,  105,  106,  107,  108,
 /*  1460 */   109,  110,  111,  112,   53,  143,  143,  143,  143,   58,
 /*  1470 */    53,   60,   61,   62,   63,   58,  143,   60,   61,   62,
 /*  1480 */    63,
};
#define YY_SHIFT_USE_DFLT (-3)
#define YY_SHIFT_MAX 165
static const short yy_shift_ofst[] = {
 /*     0 */   124,  124,  124,   20,  124,   72,  124,  124,  124,  124,
 /*    10 */   124,  124,  124,  124,  816,  717,  783,  783,  783,  783,
 /*    20 */   750,  783,  783,  783,  783,  783,  783,  783,  783,  783,
 /*    30 */   783,  783,  783,  783,  783,  783,  783,  783,  783,  783,
 /*    40 */   783,  783,  783,  783,  783,  783,  783,  783,  783,  783,
 /*    50 */   783,  783,  783,  783,  783,  783,  783,  783,  783,  142,
 /*    60 */   142, 1411, 1417,  142,  254,   49,  114,  109,  207,  341,
 /*    70 */   706,  988,  855,  187,   82,  207,  139,   82,   82,   35,
 /*    80 */    -3,  862,  947,  962,  976, 1006,  725,  725, 1336, 1336,
 /*    90 */   283,  283,  283,  283,  172,   52,   52,   61,   12,   79,
 /*   100 */   113,  348,  384,  381,  403,  382,  416,  407,  420,  422,
 /*   110 */   456,  446,  461,  450,  388,  504,  502,  503,  526,  505,
 /*   120 */   539,  544,  525,  516,  547,  511,  530,  516,  500,  498,
 /*   130 */   461,  467,  469,  443,  447,  446,  448,  428,  421,  383,
 /*   140 */   408,  406,  372,  360,  374,  371,  324,  322,  317,  297,
 /*   150 */   282,  213,  210,  216,  214,  188,  178,  137,  140,  108,
 /*   160 */   116,  119,   54,   44,   -2,  345,
};
#define YY_REDUCE_USE_DFLT (-112)
#define YY_REDUCE_MAX 80
static const short yy_reduce_ofst[] = {
 /*     0 */   -78,  184,  125,  296,  247,  419,  337,  501,  542,  460,
 /*    10 */   378,  419,  419,  583,  782,  811,  209,  897,  951,  935,
 /*    20 */   995, 1023, 1167,  967, 1151, 1123, 1095, 1067, 1037, 1009,
 /*    30 */   981, 1137, 1051, 1081, 1109, 1180, 1193, 1283, 1293, 1303,
 /*    40 */  1313, 1323, 1333, 1343, 1273, 1262, 1252, 1233, 1223, 1203,
 /*    50 */   821,  147,  729,  832, 1213,  255,  591,  697, 1351,  111,
 /*    60 */   -92,  646,  -35,  195,   42,   58,  -68,   -9,  -43,  -43,
 /*    70 */   267,  218,  173,  118,  153,   81,   50,   60,   17,  -83,
 /*    80 */  -111,
};
static const YYACTIONTYPE yy_default[] = {
 /*     0 */   300,  465,  465,  465,  465,  465,  465,  465,  465,  465,
 /*    10 */   465,  458,  301,  465,  394,  465,  465,  465,  465,  319,
 /*    20 */   465,  465,  465,  465,  465,  465,  465,  465,  465,  465,
 /*    30 */   465,  465,  465,  465,  465,  465,  465,  465,  465,  465,
 /*    40 */   465,  465,  465,  465,  465,  465,  465,  465,  465,  465,
 /*    50 */   465,  465,  465,  465,  465,  465,  465,  465,  465,  433,
 /*    60 */   433,  465,  465,  465,  465,  465,  465,  465,  465,  465,
 /*    70 */   329,  465,  465,  459,  465,  465,  465,  465,  465,  465,
 /*    80 */   453,  332,  333,  334,  335,  336,  348,  349,  337,  338,
 /*    90 */   340,  341,  342,  339,  350,  344,  343,  386,  377,  410,
 /*   100 */   465,  465,  465,  465,  465,  465,  465,  434,  465,  465,
 /*   110 */   465,  436,  377,  395,  364,  386,  465,  465,  465,  465,
 /*   120 */   465,  465,  465,  465,  465,  465,  465,  427,  465,  465,
 /*   130 */   360,  465,  465,  465,  396,  435,  465,  465,  465,  465,
 /*   140 */   465,  465,  465,  465,  465,  465,  465,  465,  465,  465,
 /*   150 */   465,  465,  465,  465,  465,  465,  465,  449,  379,  409,
 /*   160 */   465,  410,  465,  465,  465,  465,  412,  413,  411,  414,
 /*   170 */   408,  415,  407,  416,  417,  406,  405,  418,  419,  420,
 /*   180 */   404,  403,  402,  383,  384,  385,  310,  401,  432,  400,
 /*   190 */   309,  426,  441,  311,  312,  386,  398,  397,  347,  346,
 /*   200 */   345,  330,  431,  328,  429,  327,  430,  326,  325,  324,
 /*   210 */   323,  362,  361,  393,  440,  438,  439,  392,  437,  390,
 /*   220 */   313,  442,  389,  308,  388,  387,  391,  307,  446,  382,
 /*   230 */   381,  380,  306,  314,  305,  450,  378,  315,  376,  304,
 /*   240 */   451,  375,  374,  373,  372,  371,  370,  452,  454,  455,
 /*   250 */   369,  368,  303,  302,  367,  366,  365,  461,  364,  463,
 /*   260 */   460,  363,  359,  356,  355,  462,  354,  353,  464,  456,
 /*   270 */   352,  299,  457,  351,  316,  358,  357,  298,  320,  331,
 /*   280 */   297,  443,  444,  322,  445,  321,  318,  317,  296,  447,
 /*   290 */   421,  422,  423,  425,  424,  399,
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
 /* 133 */ "parameter ::= type BITAND IDENTIFIER",
 /* 134 */ "parameter ::= BITAND IDENTIFIER",
 /* 135 */ "opt_parameter ::= type IDENTIFIER ASSIGN expression",
 /* 136 */ "opt_parameter ::= IDENTIFIER ASSIGN expression",
 /* 137 */ "parameter_list ::=",
 /* 138 */ "parameter_list ::= parameters",
 /* 139 */ "parameter_list ::= opt_parameters",
 /* 140 */ "parameter_list ::= parameters COMMA opt_parameters",
 /* 141 */ "parameters ::= parameter",
 /* 142 */ "parameters ::= parameters COMMA parameter",
 /* 143 */ "opt_parameters ::= opt_parameter",
 /* 144 */ "opt_parameters ::= opt_parameters COMMA opt_parameter",
 /* 145 */ "function_body ::= statement",
 /* 146 */ "extern_declaration ::= EXTERN LIT_STRING type IDENTIFIER LPAREN parameter_list RPAREN SEMICOLON",
 /* 147 */ "for_statement ::= FOR LPAREN for_init_statement expression SEMICOLON expression_opt RPAREN statement",
 /* 148 */ "for_init_statement ::= expression_statement",
 /* 149 */ "for_init_statement ::= variable_declaration SEMICOLON",
 /* 150 */ "foreach_statement ::= FOR LPAREN id_expression IN expression RPAREN statement",
 /* 151 */ "foreach_statement ::= FOR LPAREN foreach_decl IN expression RPAREN statement",
 /* 152 */ "foreach_decl ::= VAR IDENTIFIER",
 /* 153 */ "if_statement ::= IF LPAREN expression RPAREN statement",
 /* 154 */ "if_statement ::= IF LPAREN expression RPAREN statement ELSE statement",
 /* 155 */ "while_statement ::= WHILE LPAREN expression RPAREN statement",
 /* 156 */ "switch_statement ::= SWITCH LPAREN expression RPAREN LBRACE switch_body RBRACE",
 /* 157 */ "switch_body ::=",
 /* 158 */ "switch_body ::= switch_body switch_case",
 /* 159 */ "switch_body ::= switch_body default_case",
 /* 160 */ "switch_case ::= CASE literal COLON case_statements",
 /* 161 */ "default_case ::= DEFAULT COLON case_statements",
 /* 162 */ "case_statements ::= statement_sequence",
 /* 163 */ "try_statement ::= TRY compound_statement catch_block",
 /* 164 */ "try_statement ::= TRY compound_statement finally_block",
 /* 165 */ "try_statement ::= TRY compound_statement catch_block finally_block",
 /* 166 */ "catch_block ::= CATCH LPAREN IDENTIFIER RPAREN compound_statement",
 /* 167 */ "finally_block ::= FINALLY compound_statement",
 /* 168 */ "throw_statement ::= THROW expression SEMICOLON",
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
  { 131, 3 },
  { 131, 2 },
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
#line 1314 "cscript.c"
        break;
      case 1:
#line 78 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(translation_unit, yymsp[0].minor.yy235); }
#line 1319 "cscript.c"
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
      case 145:
      case 148:
      case 162:
#line 81 "cscript.in"
{ yygotominor.yy235 = yymsp[0].minor.yy235; }
#line 1359 "cscript.c"
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
#line 1373 "cscript.c"
        break;
      case 4:
      case 23:
#line 95 "cscript.in"
{ yygotominor.yy235 = 0; }
#line 1379 "cscript.c"
        break;
      case 21:
      case 101:
#line 116 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(empty_statement); }
#line 1385 "cscript.c"
        break;
      case 26:
#line 132 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(assignment_expression, yymsp[-1].minor.yy172, yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1390 "cscript.c"
        break;
      case 27:
#line 136 "cscript.in"
{ yygotominor.yy172 = op_assign; }
#line 1395 "cscript.c"
        break;
      case 28:
#line 137 "cscript.in"
{ yygotominor.yy172 = op_assadd; }
#line 1400 "cscript.c"
        break;
      case 29:
#line 138 "cscript.in"
{ yygotominor.yy172 = op_asssub; }
#line 1405 "cscript.c"
        break;
      case 30:
#line 139 "cscript.in"
{ yygotominor.yy172 = op_assmul; }
#line 1410 "cscript.c"
        break;
      case 31:
#line 140 "cscript.in"
{ yygotominor.yy172 = op_assdiv; }
#line 1415 "cscript.c"
        break;
      case 32:
#line 141 "cscript.in"
{ yygotominor.yy172 = op_assmod; }
#line 1420 "cscript.c"
        break;
      case 34:
#line 145 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(ternary_expression, yymsp[-4].minor.yy235, yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1425 "cscript.c"
        break;
      case 36:
#line 149 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(binary_expression, op_logor,   yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1430 "cscript.c"
        break;
      case 37:
#line 150 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(binary_expression, op_logand,  yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1435 "cscript.c"
        break;
      case 38:
#line 151 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(binary_expression, op_bitor,   yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1440 "cscript.c"
        break;
      case 39:
#line 152 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(binary_expression, op_bitxor,  yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1445 "cscript.c"
        break;
      case 40:
#line 153 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(binary_expression, op_bitand,  yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1450 "cscript.c"
        break;
      case 41:
#line 154 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(binary_expression, op_eq,   yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1455 "cscript.c"
        break;
      case 42:
#line 155 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(binary_expression, op_ne,   yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1460 "cscript.c"
        break;
      case 43:
#line 156 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(binary_expression, op_lt,   yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1465 "cscript.c"
        break;
      case 44:
#line 157 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(binary_expression, op_le,   yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1470 "cscript.c"
        break;
      case 45:
#line 158 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(binary_expression, op_gt,   yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1475 "cscript.c"
        break;
      case 46:
#line 159 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(binary_expression, op_ge,   yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1480 "cscript.c"
        break;
      case 47:
#line 160 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(binary_expression, op_add,  yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1485 "cscript.c"
        break;
      case 48:
#line 161 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(binary_expression, op_sub,  yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1490 "cscript.c"
        break;
      case 49:
#line 162 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(binary_expression, op_mul,  yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1495 "cscript.c"
        break;
      case 50:
#line 163 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(binary_expression, op_div,  yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1500 "cscript.c"
        break;
      case 51:
#line 164 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(binary_expression, op_mod,  yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1505 "cscript.c"
        break;
      case 52:
#line 165 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(binary_expression, op_seq,  yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1510 "cscript.c"
        break;
      case 53:
#line 166 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(binary_expression, op_sne,  yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1515 "cscript.c"
        break;
      case 55:
#line 170 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(prefix_expression, op_negate, yymsp[0].minor.yy235); }
#line 1520 "cscript.c"
        break;
      case 56:
#line 171 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(prefix_expression, op_preinc, yymsp[0].minor.yy235); }
#line 1525 "cscript.c"
        break;
      case 57:
#line 172 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(prefix_expression, op_predec, yymsp[0].minor.yy235); }
#line 1530 "cscript.c"
        break;
      case 58:
#line 173 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(prefix_expression, op_not,    yymsp[0].minor.yy235); }
#line 1535 "cscript.c"
        break;
      case 61:
#line 178 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(postfix_expression, op_postinc, yymsp[-1].minor.yy235); }
#line 1540 "cscript.c"
        break;
      case 62:
#line 179 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(postfix_expression, op_postdec, yymsp[-1].minor.yy235); }
#line 1545 "cscript.c"
        break;
      case 64:
#line 181 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(member_expression, yymsp[-2].minor.yy235, String(yymsp[0].minor.yy0)); }
#line 1550 "cscript.c"
        break;
      case 65:
#line 182 "cscript.in"
{ yygotominor.yy235 = yymsp[0].minor.yy235; yymsp[0].minor.yy235->m_a3 = yymsp[-2].minor.yy235; }
#line 1555 "cscript.c"
        break;
      case 66:
#line 183 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(index_expression, yymsp[-3].minor.yy235, yymsp[-1].minor.yy235); }
#line 1560 "cscript.c"
        break;
      case 69:
      case 111:
      case 112:
      case 149:
#line 188 "cscript.in"
{ yygotominor.yy235 = yymsp[-1].minor.yy235; }
#line 1568 "cscript.c"
        break;
      case 71:
#line 190 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(this_expression); }
#line 1573 "cscript.c"
        break;
      case 72:
#line 193 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(literal_value, Variant(String(yymsp[0].minor.yy0), Variant::stInt)); }
#line 1578 "cscript.c"
        break;
      case 73:
#line 194 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(literal_value, Variant(hex2dec(String(yymsp[0].minor.yy0).c_str()))); }
#line 1583 "cscript.c"
        break;
      case 74:
#line 195 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(literal_value, Variant(bin2dec(String(yymsp[0].minor.yy0).c_str()))); }
#line 1588 "cscript.c"
        break;
      case 75:
#line 196 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(literal_value, Variant(rom2dec(String(yymsp[0].minor.yy0).c_str()))); }
#line 1593 "cscript.c"
        break;
      case 76:
#line 197 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(literal_value, Variant(String(yymsp[0].minor.yy0), Variant::stReal)); }
#line 1598 "cscript.c"
        break;
      case 77:
#line 198 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(literal_value, Variant(String(yymsp[0].minor.yy0), Variant::stString)); }
#line 1603 "cscript.c"
        break;
      case 78:
#line 199 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(literal_value, Variant(true));    }
#line 1608 "cscript.c"
        break;
      case 79:
#line 200 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(literal_value, Variant(false));   }
#line 1613 "cscript.c"
        break;
      case 80:
#line 201 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(literal_value, Variant());        }
#line 1618 "cscript.c"
        break;
      case 81:
#line 204 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(lvalue, String(yymsp[0].minor.yy0)); }
#line 1623 "cscript.c"
        break;
      case 82:
#line 207 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(list_literal, yymsp[-1].minor.yy235); }
#line 1628 "cscript.c"
        break;
      case 83:
#line 208 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(list_content, yymsp[0].minor.yy235); }
#line 1633 "cscript.c"
        break;
      case 84:
#line 209 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(list_content, yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1638 "cscript.c"
        break;
      case 85:
#line 211 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(list_entry, yymsp[0].minor.yy235); }
#line 1643 "cscript.c"
        break;
      case 86:
#line 214 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(new_expression, String(yymsp[0].minor.yy0)); }
#line 1648 "cscript.c"
        break;
      case 87:
#line 221 "cscript.in"
{ yygotominor.yy235 = new Ast(builtin_type, Variant::stBool);    }
#line 1653 "cscript.c"
        break;
      case 88:
#line 222 "cscript.in"
{ yygotominor.yy235 = new Ast(builtin_type, Variant::stInt);     }
#line 1658 "cscript.c"
        break;
      case 89:
#line 223 "cscript.in"
{ yygotominor.yy235 = new Ast(builtin_type, Variant::stString);  }
#line 1663 "cscript.c"
        break;
      case 90:
#line 224 "cscript.in"
{ yygotominor.yy235 = new Ast(class_type, String(yymsp[0].minor.yy0));            }
#line 1668 "cscript.c"
        break;
      case 91:
#line 232 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(function_call, String(yymsp[-3].minor.yy0), yymsp[-1].minor.yy235); }
#line 1673 "cscript.c"
        break;
      case 93:
      case 96:
      case 141:
      case 143:
#line 239 "cscript.in"
{ yygotominor.yy259 = new AstList; yygotominor.yy259->push_back(yymsp[0].minor.yy235); }
#line 1681 "cscript.c"
        break;
      case 94:
      case 97:
      case 142:
      case 144:
#line 240 "cscript.in"
{ yygotominor.yy259 = yymsp[-2].minor.yy259; yymsp[-2].minor.yy259->push_back(yymsp[0].minor.yy235); }
#line 1689 "cscript.c"
        break;
      case 95:
#line 243 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(named_argument, String(yymsp[-2].minor.yy0), yymsp[0].minor.yy235); }
#line 1694 "cscript.c"
        break;
      case 98:
#line 251 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(positional_arguments, new AstList); }
#line 1699 "cscript.c"
        break;
      case 99:
#line 252 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(positional_arguments, yymsp[0].minor.yy259); }
#line 1704 "cscript.c"
        break;
      case 100:
#line 253 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(named_arguments,      yymsp[0].minor.yy259); }
#line 1709 "cscript.c"
        break;
      case 102:
#line 262 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(expression_statement, yymsp[-1].minor.yy235); }
#line 1714 "cscript.c"
        break;
      case 103:
#line 265 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(compound_statement); }
#line 1719 "cscript.c"
        break;
      case 104:
#line 266 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(compound_statement, yymsp[-1].minor.yy235); }
#line 1724 "cscript.c"
        break;
      case 105:
#line 269 "cscript.in"
{ p->Parse(yymsp[-1].minor.yy0); yygotominor.yy235 = p->GetRoot(); }
#line 1729 "cscript.c"
        break;
      case 106:
#line 272 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(return_statement, yymsp[-1].minor.yy235); }
#line 1734 "cscript.c"
        break;
      case 107:
#line 273 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(return_statement);    }
#line 1739 "cscript.c"
        break;
      case 108:
#line 276 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(break_statement); }
#line 1744 "cscript.c"
        break;
      case 109:
#line 277 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(continue_statement); }
#line 1749 "cscript.c"
        break;
      case 114:
#line 291 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(variable_declaration, String(yymsp[0].minor.yy0));    }
#line 1754 "cscript.c"
        break;
      case 115:
#line 292 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(variable_declaration, String(yymsp[-2].minor.yy0), yymsp[0].minor.yy235); }
#line 1759 "cscript.c"
        break;
      case 117:
#line 295 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(declaration_sequence, yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1764 "cscript.c"
        break;
      case 118:
#line 302 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(class_declaration, String(yymsp[-2].minor.yy0)); }
#line 1769 "cscript.c"
        break;
      case 119:
#line 303 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(class_declaration, String(yymsp[-3].minor.yy0), yymsp[-1].minor.yy259); }
#line 1774 "cscript.c"
        break;
      case 120:
#line 306 "cscript.in"
{ yygotominor.yy235 = yymsp[-1].minor.yy235; yymsp[-1].minor.yy235->m_props["access"] = accessDefault; }
#line 1779 "cscript.c"
        break;
      case 121:
#line 307 "cscript.in"
{ yygotominor.yy235 = yymsp[0].minor.yy235; yymsp[0].minor.yy235->m_props["access"] = accessDefault; }
#line 1784 "cscript.c"
        break;
      case 122:
#line 308 "cscript.in"
{ yygotominor.yy235 = yymsp[-1].minor.yy235; yymsp[-1].minor.yy235->m_props["access"] = yymsp[-2].minor.yy128; }
#line 1789 "cscript.c"
        break;
      case 123:
#line 309 "cscript.in"
{ yygotominor.yy235 = yymsp[0].minor.yy235; yymsp[0].minor.yy235->m_props["access"] = yymsp[-1].minor.yy128; }
#line 1794 "cscript.c"
        break;
      case 124:
#line 310 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(access_specifier, yymsp[-1].minor.yy128); }
#line 1799 "cscript.c"
        break;
      case 125:
#line 314 "cscript.in"
{ yygotominor.yy128 = accessPrivate;   }
#line 1804 "cscript.c"
        break;
      case 126:
#line 315 "cscript.in"
{ yygotominor.yy128 = accessProtected; }
#line 1809 "cscript.c"
        break;
      case 127:
#line 316 "cscript.in"
{ yygotominor.yy128 = accessPublic;    }
#line 1814 "cscript.c"
        break;
      case 128:
#line 320 "cscript.in"
{ 
  yygotominor.yy259 = new AstList;
  yygotominor.yy259->push_back(yymsp[0].minor.yy235);
}
#line 1822 "cscript.c"
        break;
      case 129:
#line 324 "cscript.in"
{ 
  yygotominor.yy259 = yymsp[-1].minor.yy259;
  yygotominor.yy259->push_back(yymsp[0].minor.yy235);
}
#line 1830 "cscript.c"
        break;
      case 130:
#line 335 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(function_declaration, String(yymsp[-4].minor.yy0), yymsp[-2].minor.yy259, yymsp[0].minor.yy235); }
#line 1835 "cscript.c"
        break;
      case 131:
#line 338 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(parameter, String(yymsp[0].minor.yy0), yymsp[-1].minor.yy235,         AstData(), ptByVal); }
#line 1840 "cscript.c"
        break;
      case 132:
#line 339 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(parameter, String(yymsp[0].minor.yy0), AstData(), AstData(), ptByVal); }
#line 1845 "cscript.c"
        break;
      case 133:
#line 340 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(parameter, String(yymsp[0].minor.yy0), yymsp[-2].minor.yy235,         AstData(), ptByRef); }
#line 1850 "cscript.c"
        break;
      case 134:
#line 341 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(parameter, String(yymsp[0].minor.yy0), AstData(), AstData(), ptByRef); }
#line 1855 "cscript.c"
        break;
      case 135:
#line 344 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(parameter, String(yymsp[-2].minor.yy0), yymsp[-3].minor.yy235,         yymsp[0].minor.yy235, ptByVal); }
#line 1860 "cscript.c"
        break;
      case 136:
#line 345 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(parameter, String(yymsp[-2].minor.yy0), AstData(), yymsp[0].minor.yy235, ptByVal); }
#line 1865 "cscript.c"
        break;
      case 137:
      case 157:
#line 349 "cscript.in"
{ yygotominor.yy259 = new AstList; }
#line 1871 "cscript.c"
        break;
      case 138:
      case 139:
#line 350 "cscript.in"
{ yygotominor.yy259 = yymsp[0].minor.yy259; }
#line 1877 "cscript.c"
        break;
      case 140:
#line 352 "cscript.in"
{ yygotominor.yy259 = yymsp[-2].minor.yy259; yymsp[-2].minor.yy259->adopt(*yymsp[0].minor.yy259); }
#line 1882 "cscript.c"
        break;
      case 146:
#line 375 "cscript.in"
{ 
  yygotominor.yy235 = new Ast(extern_declaration, String(yymsp[-4].minor.yy0), yymsp[-2].minor.yy259, String(yymsp[-6].minor.yy0), yymsp[-5].minor.yy235); 
}
#line 1889 "cscript.c"
        break;
      case 147:
#line 385 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(for_statement, yymsp[-5].minor.yy235, yymsp[-4].minor.yy235, yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1894 "cscript.c"
        break;
      case 150:
      case 151:
#line 396 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(foreach_statement, yymsp[-4].minor.yy235, yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1900 "cscript.c"
        break;
      case 152:
#line 398 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(variable_declaration, String(yymsp[0].minor.yy0)); }
#line 1905 "cscript.c"
        break;
      case 153:
#line 409 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(if_statement, yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1910 "cscript.c"
        break;
      case 154:
#line 410 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(if_statement, yymsp[-4].minor.yy235, yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1915 "cscript.c"
        break;
      case 155:
#line 418 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(while_statement, yymsp[-2].minor.yy235,  yymsp[0].minor.yy235); }
#line 1920 "cscript.c"
        break;
      case 156:
#line 426 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(switch_statement, yymsp[-4].minor.yy235, yymsp[-1].minor.yy259); }
#line 1925 "cscript.c"
        break;
      case 158:
      case 159:
#line 431 "cscript.in"
{ yygotominor.yy259 = yymsp[-1].minor.yy259; yygotominor.yy259->push_back(yymsp[0].minor.yy235); }
#line 1931 "cscript.c"
        break;
      case 160:
#line 435 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(switch_case, yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1936 "cscript.c"
        break;
      case 161:
#line 438 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(default_case, yymsp[0].minor.yy235); }
#line 1941 "cscript.c"
        break;
      case 163:
#line 448 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(try_statement, yymsp[-1].minor.yy235, yymsp[0].minor.yy235); }
#line 1946 "cscript.c"
        break;
      case 164:
#line 449 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(try_statement, yymsp[-1].minor.yy235, AstData(), yymsp[0].minor.yy235); }
#line 1951 "cscript.c"
        break;
      case 165:
#line 450 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(try_statement, yymsp[-2].minor.yy235, yymsp[-1].minor.yy235, yymsp[0].minor.yy235); }
#line 1956 "cscript.c"
        break;
      case 166:
#line 452 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(catch_block, String(yymsp[-2].minor.yy0), yymsp[0].minor.yy235); }
#line 1961 "cscript.c"
        break;
      case 167:
#line 454 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(finally_block, yymsp[0].minor.yy235); }
#line 1966 "cscript.c"
        break;
      case 168:
#line 456 "cscript.in"
{ yygotominor.yy235 = p->AllocAst(throw_statement, yymsp[-1].minor.yy235); }
#line 1971 "cscript.c"
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
#line 2019 "cscript.c"
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
#line 2037 "cscript.c"
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

