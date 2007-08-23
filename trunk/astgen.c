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
#define YYNOCODE 128
#define YYACTIONTYPE unsigned short int
#define CScriptParseTOKENTYPE  Token 
typedef union {
  CScriptParseTOKENTYPE yy0;
  AstList* yy99;
  Ast* yy155;
  opcodes yy246;
  int yy255;
} YYMINORTYPE;
#ifndef YYSTACKDEPTH
#define YYSTACKDEPTH 100
#endif
#define CScriptParseARG_SDECL  Parser* p ;
#define CScriptParseARG_PDECL , Parser* p 
#define CScriptParseARG_FETCH  Parser* p  = yypParser->p 
#define CScriptParseARG_STORE yypParser->p  = p 
#define YYNSTATE 256
#define YYNRULE 147
#define YYERRORSYMBOL 68
#define YYERRSYMDT yy255
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
 /*     0 */   225,  404,  152,  251,   14,  250,  246,  245,  242,  241,
 /*    10 */   239,  237,  236,  235,  234,  232,  229,  228,  226,  126,
 /*    20 */   224,  142,  223,   66,  148,  220,   85,  209,  208,  207,
 /*    30 */    53,  206,  205,  202,   36,   48,   47,  144,  137,  136,
 /*    40 */   133,  110,   56,   54,   55,  167,  106,   17,  143,   26,
 /*    50 */   147,  201,  200,  198,  197,  196,  195,  194,  193,  191,
 /*    60 */    62,  107,  153,    3,  255,  131,   21,  138,  140,  139,
 /*    70 */    64,  116,  111,  100,   97,   53,  164,    4,  125,   91,
 /*    80 */   114,  134,   64,  244,  248,  129,  127,   56,   54,   55,
 /*    90 */   141,  106,   17,  150,   26,  175,  201,  200,  198,  197,
 /*   100 */   196,  195,  194,  193,  191,   99,  107,  153,    3,  151,
 /*   110 */   131,   21,  138,  140,  139,   64,  116,  111,  100,   97,
 /*   120 */   212,  213,  225,  125,   91,  114,    8,  250,  246,  245,
 /*   130 */   242,  241,  239,  237,  236,  235,  234,  232,  229,  228,
 /*   140 */   226,  126,  224,  177,  223,   66,  108,  220,   85,  209,
 /*   150 */   208,  207,  178,  206,  205,  202,  132,   30,  149,  144,
 /*   160 */   137,  136,  133,   40,   44,   45,   46,   50,   42,   49,
 /*   170 */    35,   36,   48,   47,  252,  247,  248,  225,  121,   13,
 /*   180 */   221,    8,  250,  246,  245,  242,  241,  239,  237,  236,
 /*   190 */   235,  234,  232,  229,  228,  226,  126,  224,  392,  223,
 /*   200 */    66,   96,  220,   85,  209,  208,  207,  123,  206,  205,
 /*   210 */   202,    5,  162,  211,  144,  137,  136,  133,   64,   87,
 /*   220 */    53,  220,   85,  209,  208,  207,  190,  206,  205,  202,
 /*   230 */    67,  103,   56,   54,   55,  218,  106,   17,   19,   26,
 /*   240 */    61,  201,  200,  198,  197,  196,  195,  194,  193,  191,
 /*   250 */   157,  107,  153,    3,   68,  131,   21,  138,  140,  139,
 /*   260 */    64,  116,  111,  100,   97,  128,  158,  120,  125,   91,
 /*   270 */   114,  225,   69,   95,   33,    6,  250,  246,  245,  242,
 /*   280 */   241,  239,  237,  236,  235,  234,  232,  229,  228,  226,
 /*   290 */   126,  224,  253,  223,   66,  182,  220,   85,  209,  208,
 /*   300 */   207,   27,  206,  205,  202,   20,   88,  174,  144,  137,
 /*   310 */   136,  133,  225,   64,  173,   31,  100,  185,  246,  245,
 /*   320 */   242,  241,  239,  237,  236,  235,  234,  232,  229,  228,
 /*   330 */   226,  126,  224,  123,  223,   66,  203,  220,   85,  209,
 /*   340 */   208,  207,  146,  206,  205,  202,   24,   30,    7,  144,
 /*   350 */   137,  136,  133,   11,  219,  217,  225,   70,   12,   25,
 /*   360 */   184,  185,  246,  245,  242,  241,  239,  237,  236,  235,
 /*   370 */   234,  232,  229,  228,  226,  126,  224,   23,  223,   66,
 /*   380 */    57,  220,   85,  209,  208,  207,  233,  206,  205,  202,
 /*   390 */     2,  101,   10,  144,  137,  136,  133,   15,  110,   29,
 /*   400 */   225,   63,  168,   18,  240,  230,  246,  245,  242,  241,
 /*   410 */   239,  237,  236,  235,  234,  232,  229,  228,  226,  126,
 /*   420 */   224,    9,  223,   66,   16,  220,   85,  209,  208,  207,
 /*   430 */    58,  206,  205,  202,   94,  112,  225,  144,  137,  136,
 /*   440 */   133,  249,  246,  245,  242,  241,  239,  237,  236,  235,
 /*   450 */   234,  232,  229,  228,  226,  126,  224,  166,  223,   66,
 /*   460 */    60,  220,   85,  209,  208,  207,   71,  206,  205,  202,
 /*   470 */     1,   32,  172,  144,  137,  136,  133,  225,   64,  176,
 /*   480 */    34,  100,  238,  246,  245,  242,  241,  239,  237,  236,
 /*   490 */   235,  234,  232,  229,  228,  226,  126,  224,  187,  223,
 /*   500 */    66,  102,  220,   85,  209,  208,  207,  405,  206,  205,
 /*   510 */   202,  405,  405,  225,  144,  137,  136,  133,  199,  246,
 /*   520 */   245,  242,  241,  239,  237,  236,  235,  234,  232,  229,
 /*   530 */   228,  226,  126,  224,  405,  223,   66,  405,  220,   85,
 /*   540 */   209,  208,  207,  405,  206,  205,  202,  405,  405,  405,
 /*   550 */   144,  137,  136,  133,  225,  405,  405,  405,  405,   92,
 /*   560 */   246,  245,  242,  241,  239,  237,  236,  235,  234,  232,
 /*   570 */   229,  228,  226,  126,  224,  405,  223,   66,  405,  220,
 /*   580 */    85,  209,  208,  207,  405,  206,  205,  202,  405,  405,
 /*   590 */   225,  144,  137,  136,  133,  204,  246,  245,  242,  241,
 /*   600 */   239,  237,  236,  235,  234,  232,  229,  228,  226,  126,
 /*   610 */   224,  405,  223,   66,  405,  220,   85,  209,  208,  207,
 /*   620 */   405,  206,  205,  202,  405,  405,  405,  144,  137,  136,
 /*   630 */   133,  225,  405,  405,  405,  405,  192,  246,  245,  242,
 /*   640 */   241,  239,  237,  236,  235,  234,  232,  229,  228,  226,
 /*   650 */   126,  224,  405,  223,   66,  405,  220,   85,  209,  208,
 /*   660 */   207,  405,  206,  205,  202,  405,  405,  405,  144,  137,
 /*   670 */   136,  133,   39,   38,   52,   41,   51,   43,   37,   40,
 /*   680 */    44,   45,   46,   50,   42,   49,   35,   36,   48,   47,
 /*   690 */   171,  170,  169,  165,  163,  161,   22,   53,   84,  405,
 /*   700 */   220,   85,  209,  208,  207,  405,  206,  205,  202,   56,
 /*   710 */    54,   55,  405,  106,   17,  405,   26,  405,  201,  200,
 /*   720 */   198,  197,  196,  195,  194,  193,  191,   53,  107,  153,
 /*   730 */   405,   49,   35,   36,   48,   47,  405,   65,  405,   56,
 /*   740 */    54,   55,  405,  106,   17,  405,   26,  183,  201,  200,
 /*   750 */   198,  197,  196,  195,  194,  193,  191,   53,  107,   45,
 /*   760 */    46,   50,   42,   49,   35,   36,   48,   47,  405,   56,
 /*   770 */    54,   55,  405,  106,   17,  405,   26,  405,  201,  200,
 /*   780 */   198,  197,  196,  195,  194,  193,  191,   53,  107,  145,
 /*   790 */   201,  200,  198,  197,  196,  195,  194,  193,  191,   56,
 /*   800 */    54,   55,  405,  106,   17,  405,   26,  405,  201,  200,
 /*   810 */   198,  197,  196,  195,  194,  193,  191,  405,  107,   38,
 /*   820 */    52,   41,   51,   43,   37,   40,   44,   45,   46,   50,
 /*   830 */    42,   49,   35,   36,   48,   47,  405,  405,  405,  405,
 /*   840 */   188,  224,  405,  223,   66,  231,  220,   85,  209,  208,
 /*   850 */   207,  405,  206,  205,  202,   89,  186,  126,  224,  405,
 /*   860 */   223,   66,  405,  220,   85,  209,  208,  207,  405,  206,
 /*   870 */   109,  202,  405,  181,  224,  405,  223,   66,  117,  220,
 /*   880 */    85,  209,  208,  207,   90,  206,  205,  202,  405,  405,
 /*   890 */   179,   28,  118,  405,  405,   52,   41,   51,   43,   37,
 /*   900 */    40,   44,   45,   46,   50,   42,   49,   35,   36,   48,
 /*   910 */    47,   41,   51,   43,   37,   40,   44,   45,   46,   50,
 /*   920 */    42,   49,   35,   36,   48,   47,   51,   43,   37,   40,
 /*   930 */    44,   45,   46,   50,   42,   49,   35,   36,   48,   47,
 /*   940 */   188,  224,  405,  223,   66,  405,  220,   85,  209,  208,
 /*   950 */   207,  405,  206,  205,  202,  405,  189,  227,  224,  115,
 /*   960 */   223,   66,  405,  220,   85,  209,  208,  207,  405,  206,
 /*   970 */   205,  202,   43,   37,   40,   44,   45,   46,   50,   42,
 /*   980 */    49,   35,   36,   48,   47,  181,  224,  405,  223,   66,
 /*   990 */   405,  220,   85,  209,  208,  207,  405,  206,  205,  202,
 /*  1000 */   243,  224,  180,  223,   66,  405,  220,   85,  209,  208,
 /*  1010 */   207,  405,  206,  205,  202,   98,  224,  405,  223,   66,
 /*  1020 */   405,  220,   85,  209,  208,  207,  405,  206,  205,  202,
 /*  1030 */   405,  156,  224,  405,  223,   66,  405,  220,   85,  209,
 /*  1040 */   208,  207,  405,  206,  205,  202,  104,  224,  405,  223,
 /*  1050 */    66,  405,  220,   85,  209,  208,  207,  405,  206,  205,
 /*  1060 */   202,  105,  224,  405,  223,   66,  405,  220,   85,  209,
 /*  1070 */   208,  207,  405,  206,  205,  202,  119,  224,  405,  223,
 /*  1080 */    66,  405,  220,   85,  209,  208,  207,  405,  206,  205,
 /*  1090 */   202,  113,  224,  405,  223,   66,  405,  220,   85,  209,
 /*  1100 */   208,  207,  405,  206,  205,  202,  135,  224,  405,  223,
 /*  1110 */    66,  405,  220,   85,  209,  208,  207,  405,  206,  205,
 /*  1120 */   202,  405,   93,  224,  405,  223,   66,  405,  220,   85,
 /*  1130 */   209,  208,  207,  405,  206,  205,  202,  124,  224,  405,
 /*  1140 */   223,   66,  405,  220,   85,  209,  208,  207,  405,  206,
 /*  1150 */   205,  202,  122,  224,  405,  223,   66,  405,  220,   85,
 /*  1160 */   209,  208,  207,  405,  206,  205,  202,  130,  224,  405,
 /*  1170 */   223,   66,  405,  220,   85,  209,  208,  207,  405,  206,
 /*  1180 */   205,  202,  405,  160,  405,  223,   66,  405,  220,   85,
 /*  1190 */   209,  208,  207,  405,  206,  205,  202,  405,  222,  405,
 /*  1200 */   223,   66,  405,  220,   85,  209,  208,  207,  405,  206,
 /*  1210 */   205,  202,   72,  405,  220,   85,  209,  208,  207,  405,
 /*  1220 */   206,  205,  202,   76,  405,  220,   85,  209,  208,  207,
 /*  1230 */   405,  206,  205,  202,  405,  154,  405,  220,   85,  209,
 /*  1240 */   208,  207,  405,  206,  205,  202,  405,   74,  405,  220,
 /*  1250 */    85,  209,  208,  207,  405,  206,  205,  202,   83,  405,
 /*  1260 */   220,   85,  209,  208,  207,  405,  206,  205,  202,   82,
 /*  1270 */   405,  220,   85,  209,  208,  207,  405,  206,  205,  202,
 /*  1280 */    79,  405,  220,   85,  209,  208,  207,  405,  206,  205,
 /*  1290 */   202,  405,   73,  405,  220,   85,  209,  208,  207,  405,
 /*  1300 */   206,  205,  202,   77,  405,  220,   85,  209,  208,  207,
 /*  1310 */   405,  206,  205,  202,  159,  405,  220,   85,  209,  208,
 /*  1320 */   207,  405,  206,  205,  202,   86,  405,  220,   85,  209,
 /*  1330 */   208,  207,  405,  206,  205,  202,   81,  405,  220,   85,
 /*  1340 */   209,  208,  207,  405,  206,  205,  202,   78,  405,  220,
 /*  1350 */    85,  209,  208,  207,  405,  206,  205,  202,   75,  405,
 /*  1360 */   220,   85,  209,  208,  207,  405,  206,  205,  202,   80,
 /*  1370 */   405,  220,   85,  209,  208,  207,  405,  206,  205,  202,
 /*  1380 */   155,  405,  220,   85,  209,  208,  207,  405,  206,  205,
 /*  1390 */   202,  216,   85,  209,  208,  207,  405,  206,  205,  202,
 /*  1400 */   214,   85,  209,  208,  207,  405,  206,  205,  202,  405,
 /*  1410 */   215,   85,  209,  208,  207,  405,  206,  205,  202,  210,
 /*  1420 */    85,  209,  208,  207,  405,  206,  205,  202,  177,  405,
 /*  1430 */   405,  108,  405,  405,  405,  405,   59,  254,
};
static const YYCODETYPE yy_lookahead[] = {
 /*     0 */    68,   69,   70,   71,   72,   73,   74,   75,   76,   77,
 /*    10 */    78,   79,   80,   81,   82,   83,   84,   85,   86,   87,
 /*    20 */    88,   47,   90,   91,   47,   93,   94,   95,   96,   97,
 /*    30 */    15,   99,  100,  101,   16,   17,   18,  105,  106,  107,
 /*    40 */   108,  108,   27,   28,   29,  112,   31,   32,   47,   34,
 /*    50 */    47,   36,   37,   38,   39,   40,   41,   42,   43,   44,
 /*    60 */   123,   46,   47,   48,   49,   50,   51,   52,   53,   54,
 /*    70 */    55,   56,   57,   58,   59,   15,   49,   35,   63,   64,
 /*    80 */    65,  115,   55,  117,  118,  119,  120,   27,   28,   29,
 /*    90 */    47,   31,   32,   47,   34,   97,   36,   37,   38,   39,
 /*   100 */    40,   41,   42,   43,   44,   31,   46,   47,   48,   49,
 /*   110 */    50,   51,   52,   53,   54,   55,   56,   57,   58,   59,
 /*   120 */   124,  125,   68,   63,   64,   65,   72,   73,   74,   75,
 /*   130 */    76,   77,   78,   79,   80,   81,   82,   83,   84,   85,
 /*   140 */    86,   87,   88,  105,   90,   91,  108,   93,   94,   95,
 /*   150 */    96,   97,  114,   99,  100,  101,   41,   19,   47,  105,
 /*   160 */   106,  107,  108,    8,    9,   10,   11,   12,   13,   14,
 /*   170 */    15,   16,   17,   18,  118,  117,  118,   68,  120,   35,
 /*   180 */   126,   72,   73,   74,   75,   76,   77,   78,   79,   80,
 /*   190 */    81,   82,   83,   84,   85,   86,   87,   88,   60,   90,
 /*   200 */    91,   31,   93,   94,   95,   96,   97,   31,   99,  100,
 /*   210 */   101,   35,   49,   49,  105,  106,  107,  108,   55,   91,
 /*   220 */    15,   93,   94,   95,   96,   97,   33,   99,  100,  101,
 /*   230 */    66,   67,   27,   28,   29,  126,   31,   32,   45,   34,
 /*   240 */    45,   36,   37,   38,   39,   40,   41,   42,   43,   44,
 /*   250 */   110,   46,   47,   48,   45,   50,   51,   52,   53,   54,
 /*   260 */    55,   56,   57,   58,   59,  109,  110,   31,   63,   64,
 /*   270 */    65,   68,   45,   99,   92,   72,   73,   74,   75,   76,
 /*   280 */    77,   78,   79,   80,   81,   82,   83,   84,   85,   86,
 /*   290 */    87,   88,   47,   90,   91,   35,   93,   94,   95,   96,
 /*   300 */    97,   34,   99,  100,  101,   45,   31,   49,  105,  106,
 /*   310 */   107,  108,   68,   55,   33,   19,   58,   73,   74,   75,
 /*   320 */    76,   77,   78,   79,   80,   81,   82,   83,   84,   85,
 /*   330 */    86,   87,   88,   31,   90,   91,   35,   93,   94,   95,
 /*   340 */    96,   97,   47,   99,  100,  101,   34,   19,   62,  105,
 /*   350 */   106,  107,  108,   35,   27,   28,   68,   30,   35,   32,
 /*   360 */   116,   73,   74,   75,   76,   77,   78,   79,   80,   81,
 /*   370 */    82,   83,   84,   85,   86,   87,   88,   60,   90,   91,
 /*   380 */    34,   93,   94,   95,   96,   97,   47,   99,  100,  101,
 /*   390 */    26,   31,   35,  105,  106,  107,  108,   34,  108,   34,
 /*   400 */    68,  111,  112,   47,  116,   73,   74,   75,   76,   77,
 /*   410 */    78,   79,   80,   81,   82,   83,   84,   85,   86,   87,
 /*   420 */    88,   35,   90,   91,   34,   93,   94,   95,   96,   97,
 /*   430 */    48,   99,  100,  101,   31,   31,   68,  105,  106,  107,
 /*   440 */   108,   73,   74,   75,   76,   77,   78,   79,   80,   81,
 /*   450 */    82,   83,   84,   85,   86,   87,   88,   47,   90,   91,
 /*   460 */    48,   93,   94,   95,   96,   97,   48,   99,  100,  101,
 /*   470 */    26,   60,   49,  105,  106,  107,  108,   68,   55,   47,
 /*   480 */    26,   58,   73,   74,   75,   76,   77,   78,   79,   80,
 /*   490 */    81,   82,   83,   84,   85,   86,   87,   88,   31,   90,
 /*   500 */    91,   35,   93,   94,   95,   96,   97,  127,   99,  100,
 /*   510 */   101,  127,  127,   68,  105,  106,  107,  108,   73,   74,
 /*   520 */    75,   76,   77,   78,   79,   80,   81,   82,   83,   84,
 /*   530 */    85,   86,   87,   88,  127,   90,   91,  127,   93,   94,
 /*   540 */    95,   96,   97,  127,   99,  100,  101,  127,  127,  127,
 /*   550 */   105,  106,  107,  108,   68,  127,  127,  127,  127,   73,
 /*   560 */    74,   75,   76,   77,   78,   79,   80,   81,   82,   83,
 /*   570 */    84,   85,   86,   87,   88,  127,   90,   91,  127,   93,
 /*   580 */    94,   95,   96,   97,  127,   99,  100,  101,  127,  127,
 /*   590 */    68,  105,  106,  107,  108,   73,   74,   75,   76,   77,
 /*   600 */    78,   79,   80,   81,   82,   83,   84,   85,   86,   87,
 /*   610 */    88,  127,   90,   91,  127,   93,   94,   95,   96,   97,
 /*   620 */   127,   99,  100,  101,  127,  127,  127,  105,  106,  107,
 /*   630 */   108,   68,  127,  127,  127,  127,   73,   74,   75,   76,
 /*   640 */    77,   78,   79,   80,   81,   82,   83,   84,   85,   86,
 /*   650 */    87,   88,  127,   90,   91,  127,   93,   94,   95,   96,
 /*   660 */    97,  127,   99,  100,  101,  127,  127,  127,  105,  106,
 /*   670 */   107,  108,    1,    2,    3,    4,    5,    6,    7,    8,
 /*   680 */     9,   10,   11,   12,   13,   14,   15,   16,   17,   18,
 /*   690 */    19,   20,   21,   22,   23,   24,   25,   15,   91,  127,
 /*   700 */    93,   94,   95,   96,   97,  127,   99,  100,  101,   27,
 /*   710 */    28,   29,  127,   31,   32,  127,   34,  127,   36,   37,
 /*   720 */    38,   39,   40,   41,   42,   43,   44,   15,   46,   47,
 /*   730 */   127,   14,   15,   16,   17,   18,  127,   55,  127,   27,
 /*   740 */    28,   29,  127,   31,   32,  127,   34,   35,   36,   37,
 /*   750 */    38,   39,   40,   41,   42,   43,   44,   15,   46,   10,
 /*   760 */    11,   12,   13,   14,   15,   16,   17,   18,  127,   27,
 /*   770 */    28,   29,  127,   31,   32,  127,   34,  127,   36,   37,
 /*   780 */    38,   39,   40,   41,   42,   43,   44,   15,   46,   47,
 /*   790 */    36,   37,   38,   39,   40,   41,   42,   43,   44,   27,
 /*   800 */    28,   29,  127,   31,   32,  127,   34,  127,   36,   37,
 /*   810 */    38,   39,   40,   41,   42,   43,   44,  127,   46,    2,
 /*   820 */     3,    4,    5,    6,    7,    8,    9,   10,   11,   12,
 /*   830 */    13,   14,   15,   16,   17,   18,  127,  127,  127,  127,
 /*   840 */    87,   88,  127,   90,   91,   75,   93,   94,   95,   96,
 /*   850 */    97,  127,   99,  100,  101,  102,  103,   87,   88,  127,
 /*   860 */    90,   91,  127,   93,   94,   95,   96,   97,  127,   99,
 /*   870 */   100,  101,  127,   87,   88,  127,   90,   91,  108,   93,
 /*   880 */    94,   95,   96,   97,   98,   99,  100,  101,  127,  127,
 /*   890 */   104,  121,  122,  127,  127,    3,    4,    5,    6,    7,
 /*   900 */     8,    9,   10,   11,   12,   13,   14,   15,   16,   17,
 /*   910 */    18,    4,    5,    6,    7,    8,    9,   10,   11,   12,
 /*   920 */    13,   14,   15,   16,   17,   18,    5,    6,    7,    8,
 /*   930 */     9,   10,   11,   12,   13,   14,   15,   16,   17,   18,
 /*   940 */    87,   88,  127,   90,   91,  127,   93,   94,   95,   96,
 /*   950 */    97,  127,   99,  100,  101,  127,  103,   87,   88,   89,
 /*   960 */    90,   91,  127,   93,   94,   95,   96,   97,  127,   99,
 /*   970 */   100,  101,    6,    7,    8,    9,   10,   11,   12,   13,
 /*   980 */    14,   15,   16,   17,   18,   87,   88,  127,   90,   91,
 /*   990 */   127,   93,   94,   95,   96,   97,  127,   99,  100,  101,
 /*  1000 */    87,   88,  104,   90,   91,  127,   93,   94,   95,   96,
 /*  1010 */    97,  127,   99,  100,  101,   87,   88,  127,   90,   91,
 /*  1020 */   127,   93,   94,   95,   96,   97,  127,   99,  100,  101,
 /*  1030 */   127,   87,   88,  127,   90,   91,  127,   93,   94,   95,
 /*  1040 */    96,   97,  127,   99,  100,  101,   87,   88,  127,   90,
 /*  1050 */    91,  127,   93,   94,   95,   96,   97,  127,   99,  100,
 /*  1060 */   101,   87,   88,  127,   90,   91,  127,   93,   94,   95,
 /*  1070 */    96,   97,  127,   99,  100,  101,   87,   88,  127,   90,
 /*  1080 */    91,  127,   93,   94,   95,   96,   97,  127,   99,  100,
 /*  1090 */   101,   87,   88,  127,   90,   91,  127,   93,   94,   95,
 /*  1100 */    96,   97,  127,   99,  100,  101,   87,   88,  127,   90,
 /*  1110 */    91,  127,   93,   94,   95,   96,   97,  127,   99,  100,
 /*  1120 */   101,  127,   87,   88,  127,   90,   91,  127,   93,   94,
 /*  1130 */    95,   96,   97,  127,   99,  100,  101,   87,   88,  127,
 /*  1140 */    90,   91,  127,   93,   94,   95,   96,   97,  127,   99,
 /*  1150 */   100,  101,   87,   88,  127,   90,   91,  127,   93,   94,
 /*  1160 */    95,   96,   97,  127,   99,  100,  101,   87,   88,  127,
 /*  1170 */    90,   91,  127,   93,   94,   95,   96,   97,  127,   99,
 /*  1180 */   100,  101,  127,   88,  127,   90,   91,  127,   93,   94,
 /*  1190 */    95,   96,   97,  127,   99,  100,  101,  127,   88,  127,
 /*  1200 */    90,   91,  127,   93,   94,   95,   96,   97,  127,   99,
 /*  1210 */   100,  101,   91,  127,   93,   94,   95,   96,   97,  127,
 /*  1220 */    99,  100,  101,   91,  127,   93,   94,   95,   96,   97,
 /*  1230 */   127,   99,  100,  101,  127,   91,  127,   93,   94,   95,
 /*  1240 */    96,   97,  127,   99,  100,  101,  127,   91,  127,   93,
 /*  1250 */    94,   95,   96,   97,  127,   99,  100,  101,   91,  127,
 /*  1260 */    93,   94,   95,   96,   97,  127,   99,  100,  101,   91,
 /*  1270 */   127,   93,   94,   95,   96,   97,  127,   99,  100,  101,
 /*  1280 */    91,  127,   93,   94,   95,   96,   97,  127,   99,  100,
 /*  1290 */   101,  127,   91,  127,   93,   94,   95,   96,   97,  127,
 /*  1300 */    99,  100,  101,   91,  127,   93,   94,   95,   96,   97,
 /*  1310 */   127,   99,  100,  101,   91,  127,   93,   94,   95,   96,
 /*  1320 */    97,  127,   99,  100,  101,   91,  127,   93,   94,   95,
 /*  1330 */    96,   97,  127,   99,  100,  101,   91,  127,   93,   94,
 /*  1340 */    95,   96,   97,  127,   99,  100,  101,   91,  127,   93,
 /*  1350 */    94,   95,   96,   97,  127,   99,  100,  101,   91,  127,
 /*  1360 */    93,   94,   95,   96,   97,  127,   99,  100,  101,   91,
 /*  1370 */   127,   93,   94,   95,   96,   97,  127,   99,  100,  101,
 /*  1380 */    91,  127,   93,   94,   95,   96,   97,  127,   99,  100,
 /*  1390 */   101,   93,   94,   95,   96,   97,  127,   99,  100,  101,
 /*  1400 */    93,   94,   95,   96,   97,  127,   99,  100,  101,  127,
 /*  1410 */    93,   94,   95,   96,   97,  127,   99,  100,  101,   93,
 /*  1420 */    94,   95,   96,   97,  127,   99,  100,  101,  105,  127,
 /*  1430 */   127,  108,  127,  127,  127,  127,  113,  114,
};
#define YY_SHIFT_USE_DFLT (-27)
#define YY_SHIFT_MAX 140
static const short yy_shift_ofst[] = {
 /*     0 */   205,  205,  205,   15,  205,  205,   60,  205,  205,  205,
 /*    10 */   205,  205,  205,  205,  205,  682,  712,  772,  772,  772,
 /*    20 */   772,  742,  772,  772,  772,  772,  772,  772,  772,  772,
 /*    30 */   772,  772,  772,  772,  772,  772,  772,  772,  772,  772,
 /*    40 */   772,  772,  772,  772,  772,  772,  772,  772,  772,  772,
 /*    50 */   772,  772,  772,  772,  772,  772,  772,  176,  423,  258,
 /*    60 */   163,  302,  164,   27,  236,  275,  671,  754,  236,  170,
 /*    70 */    74,  -27,  817,  892,  907,  921,  966,  155,  155,  749,
 /*    80 */   749,  717,  717,  717,  717,  327,   18,   18,  138,  193,
 /*    90 */   260,  312,  286,  323,  346,  364,  296,  363,  386,  390,
 /*   100 */   403,  412,  418,  444,  454,  466,  390,  467,  432,  411,
 /*   110 */   410,  404,  382,  356,  365,  357,  360,  339,  317,  318,
 /*   120 */   328,  227,  301,  296,  281,  267,  245,  227,  209,  195,
 /*   130 */   144,  115,  111,   46,   42,    3,  -23,  295,  -26,    1,
 /*   140 */    43,
};
#define YY_REDUCE_USE_DFLT (-69)
#define YY_REDUCE_MAX 71
static const short yy_reduce_ofst[] = {
 /*     0 */   -68,   54,  109,  203,  244,  288,  368,  445,  368,  563,
 /*    10 */   332,  409,  486,  522,  368,  770,  786,  753,  870,  853,
 /*    20 */   898, 1019,  959,  989, 1080, 1050, 1065, 1035, 1004,  974,
 /*    30 */   944,  913,  928, 1110, 1095, 1234, 1223, 1256, 1201, 1121,
 /*    40 */  1278, 1267, 1245, 1212, 1189, 1178, 1167, 1144, 1289,  128,
 /*    50 */   607, 1132, 1156, 1298, 1307, 1326, 1317,  -34, 1323,   38,
 /*    60 */   290,   58,   -4,  -67,  156,  156,  182,  174,  140,   56,
 /*    70 */    -2,  -63,
};
static const YYACTIONTYPE yy_default[] = {
 /*     0 */   260,  403,  403,  403,  403,  403,  403,  403,  402,  403,
 /*    10 */   403,  403,  403,  403,  261,  403,  403,  403,  277,  403,
 /*    20 */   403,  403,  403,  403,  403,  403,  403,  403,  403,  403,
 /*    30 */   403,  403,  403,  403,  403,  403,  403,  403,  403,  403,
 /*    40 */   403,  403,  403,  403,  403,  403,  403,  403,  403,  403,
 /*    50 */   403,  403,  403,  403,  403,  403,  403,  403,  403,  403,
 /*    60 */   403,  403,  403,  403,  403,  403,  287,  403,  403,  403,
 /*    70 */   403,  397,  290,  291,  292,  293,  294,  306,  307,  296,
 /*    80 */   295,  300,  297,  298,  299,  308,  302,  301,  360,  403,
 /*    90 */   403,  403,  393,  403,  403,  403,  403,  403,  403,  318,
 /*   100 */   403,  403,  403,  403,  403,  403,  336,  403,  403,  324,
 /*   110 */   403,  403,  403,  403,  403,  403,  403,  403,  403,  403,
 /*   120 */   360,  385,  403,  377,  403,  403,  403,  384,  359,  383,
 /*   130 */   403,  403,  403,  403,  403,  403,  403,  403,  403,  403,
 /*   140 */   403,  353,  352,  354,  355,  351,  356,  350,  357,  349,
 /*   150 */   358,  348,  256,  345,  305,  304,  361,  363,  362,  303,
 /*   160 */   288,  286,  364,  285,  365,  284,  366,  368,  367,  283,
 /*   170 */   282,  281,  369,  320,  370,  319,  371,  372,  374,  343,
 /*   180 */   344,  342,  322,  321,  375,  386,  338,  341,  340,  339,
 /*   190 */   337,  335,  390,  334,  333,  332,  331,  330,  329,  394,
 /*   200 */   328,  327,  326,  325,  395,  324,  323,  317,  314,  313,
 /*   210 */   312,  396,  398,  399,  311,  310,  309,  316,  400,  315,
 /*   220 */   289,  401,  280,  279,  276,  275,  274,  278,  273,  272,
 /*   230 */   387,  388,  271,  389,  270,  269,  268,  267,  391,  266,
 /*   240 */   376,  265,  264,  378,  379,  263,  262,  380,  381,  259,
 /*   250 */   258,  257,  382,  346,  373,  347,
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
  "INTEGER",       "HEX",           "BIN",           "ROM",         
  "REAL",          "STRING",        "TRUE",          "FALSE",       
  "NULL",          "COMMA",         "NEW",           "SEMICOLON",   
  "LBRACE",        "RBRACE",        "INCLUDE",       "RETURN",      
  "BREAK",         "CONTINUE",      "PAUSE",         "VAR",         
  "STRUCT",        "CLASS",         "FUNCTION",      "FOR",         
  "IN",            "LOWER_THAN_ELSE",  "ELSE",          "IF",          
  "WHILE",         "SWITCH",        "CASE",          "DEFAULT",     
  "error",         "main",          "translation_unit",  "statement_sequence_opt",
  "statement_sequence",  "statement",     "include_statement",  "expression_statement",
  "declaration_statement",  "for_statement",  "compound_statement",  "if_statement",
  "while_statement",  "foreach_statement",  "return_statement",  "switch_statement",
  "break_statement",  "continue_statement",  "pause_statement",  "expression",  
  "assignment_expression",  "expression_opt",  "conditional_expression",  "binary_expression",
  "assignment_operator",  "unary_expression",  "postfix_expression",  "new_expression",
  "primary_expression",  "function_call",  "argument_list",  "literal",     
  "id_expression",  "list_literal",  "list_content",  "list_entry",  
  "argument",      "function_declaration",  "struct_declaration",  "class_declaration",
  "variable_declaration",  "declarator_sequence",  "declarator",    "struct_members",
  "struct_member",  "class_members",  "class_member",  "parameter_list",
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
 /*  83 */ "list_content ::= list_content COMMA list_entry",
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
 /*  98 */ "pause_statement ::= PAUSE SEMICOLON",
 /*  99 */ "declaration_statement ::= function_declaration",
 /* 100 */ "declaration_statement ::= struct_declaration SEMICOLON",
 /* 101 */ "declaration_statement ::= class_declaration SEMICOLON",
 /* 102 */ "declaration_statement ::= variable_declaration SEMICOLON",
 /* 103 */ "variable_declaration ::= VAR declarator_sequence",
 /* 104 */ "declarator ::= IDENTIFIER",
 /* 105 */ "declarator ::= IDENTIFIER ASSIGN expression",
 /* 106 */ "declarator_sequence ::= declarator",
 /* 107 */ "declarator_sequence ::= declarator_sequence COMMA declarator",
 /* 108 */ "struct_declaration ::= STRUCT IDENTIFIER LBRACE RBRACE",
 /* 109 */ "struct_declaration ::= STRUCT IDENTIFIER LBRACE struct_members RBRACE",
 /* 110 */ "struct_member ::= variable_declaration SEMICOLON",
 /* 111 */ "struct_members ::= struct_member",
 /* 112 */ "struct_members ::= struct_members struct_member",
 /* 113 */ "class_declaration ::= CLASS IDENTIFIER LBRACE RBRACE",
 /* 114 */ "class_declaration ::= CLASS IDENTIFIER LBRACE class_members RBRACE",
 /* 115 */ "class_member ::= variable_declaration SEMICOLON",
 /* 116 */ "class_member ::= function_declaration",
 /* 117 */ "class_members ::= class_member",
 /* 118 */ "class_members ::= class_members class_member",
 /* 119 */ "function_declaration ::= FUNCTION IDENTIFIER LPAREN parameter_list RPAREN function_body",
 /* 120 */ "function_declaration ::= FUNCTION IDENTIFIER LPAREN RPAREN function_body",
 /* 121 */ "parameter ::= IDENTIFIER",
 /* 122 */ "opt_parameter ::= IDENTIFIER ASSIGN expression",
 /* 123 */ "parameters ::= parameter",
 /* 124 */ "parameters ::= parameters COMMA parameter",
 /* 125 */ "opt_parameters ::= opt_parameter",
 /* 126 */ "opt_parameters ::= opt_parameters COMMA opt_parameter",
 /* 127 */ "parameter_list ::= parameters",
 /* 128 */ "parameter_list ::= opt_parameters",
 /* 129 */ "parameter_list ::= parameters COMMA opt_parameters",
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
  { 69, 1 },
  { 70, 1 },
  { 72, 1 },
  { 72, 2 },
  { 71, 0 },
  { 71, 1 },
  { 73, 1 },
  { 73, 1 },
  { 73, 1 },
  { 73, 1 },
  { 73, 1 },
  { 73, 1 },
  { 73, 1 },
  { 73, 1 },
  { 73, 1 },
  { 73, 1 },
  { 73, 1 },
  { 73, 1 },
  { 73, 1 },
  { 73, 1 },
  { 87, 1 },
  { 89, 0 },
  { 89, 1 },
  { 88, 1 },
  { 88, 3 },
  { 92, 1 },
  { 92, 1 },
  { 92, 1 },
  { 92, 1 },
  { 92, 1 },
  { 92, 1 },
  { 90, 1 },
  { 90, 5 },
  { 91, 1 },
  { 91, 3 },
  { 91, 3 },
  { 91, 3 },
  { 91, 3 },
  { 91, 3 },
  { 91, 3 },
  { 91, 3 },
  { 91, 3 },
  { 91, 3 },
  { 91, 3 },
  { 91, 3 },
  { 91, 3 },
  { 91, 3 },
  { 91, 3 },
  { 91, 3 },
  { 91, 3 },
  { 91, 3 },
  { 91, 3 },
  { 93, 1 },
  { 93, 2 },
  { 93, 2 },
  { 93, 2 },
  { 93, 2 },
  { 93, 1 },
  { 94, 1 },
  { 94, 2 },
  { 94, 2 },
  { 94, 1 },
  { 94, 3 },
  { 94, 3 },
  { 94, 4 },
  { 97, 3 },
  { 97, 4 },
  { 96, 1 },
  { 96, 1 },
  { 96, 3 },
  { 96, 1 },
  { 99, 1 },
  { 99, 1 },
  { 99, 1 },
  { 99, 1 },
  { 99, 1 },
  { 99, 1 },
  { 99, 1 },
  { 99, 1 },
  { 99, 1 },
  { 100, 1 },
  { 101, 3 },
  { 102, 1 },
  { 102, 3 },
  { 103, 1 },
  { 95, 2 },
  { 104, 1 },
  { 98, 1 },
  { 98, 3 },
  { 75, 1 },
  { 75, 2 },
  { 78, 2 },
  { 78, 3 },
  { 74, 3 },
  { 82, 3 },
  { 82, 2 },
  { 84, 2 },
  { 85, 2 },
  { 86, 2 },
  { 76, 1 },
  { 76, 2 },
  { 76, 2 },
  { 76, 2 },
  { 108, 2 },
  { 110, 1 },
  { 110, 3 },
  { 109, 1 },
  { 109, 3 },
  { 106, 4 },
  { 106, 5 },
  { 112, 2 },
  { 111, 1 },
  { 111, 2 },
  { 107, 4 },
  { 107, 5 },
  { 114, 2 },
  { 114, 1 },
  { 113, 1 },
  { 113, 2 },
  { 105, 6 },
  { 105, 5 },
  { 117, 1 },
  { 118, 3 },
  { 119, 1 },
  { 119, 3 },
  { 120, 1 },
  { 120, 3 },
  { 115, 1 },
  { 115, 1 },
  { 115, 3 },
  { 116, 1 },
  { 77, 8 },
  { 121, 1 },
  { 121, 2 },
  { 81, 7 },
  { 81, 7 },
  { 122, 2 },
  { 79, 5 },
  { 79, 7 },
  { 80, 5 },
  { 83, 7 },
  { 123, 0 },
  { 123, 2 },
  { 123, 2 },
  { 124, 4 },
  { 125, 3 },
  { 126, 1 },
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
{ p->SetRoot(yymsp[0].minor.yy155); }
#line 1248 "astgen.c"
        break;
      case 1:
#line 78 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(translation_unit, yymsp[0].minor.yy155); }
#line 1253 "astgen.c"
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
      case 87:
      case 99:
      case 103:
      case 106:
      case 111:
      case 116:
      case 123:
      case 125:
      case 127:
      case 128:
      case 130:
      case 132:
      case 146:
#line 81 "astgen.in"
{ yygotominor.yy155 = yymsp[0].minor.yy155; }
#line 1297 "astgen.c"
        break;
      case 3:
#line 82 "astgen.in"
{ 
  if(yymsp[-1].minor.yy155->m_type == statement_sequence) {
    yygotominor.yy155 = yymsp[-1].minor.yy155;
  }
  else {
    yygotominor.yy155 = p->AllocAst(statement_sequence, new AstList);
    yygotominor.yy155->m_a1.GetList()->push_back(yymsp[-1].minor.yy155);
  }
  yygotominor.yy155->m_a1.GetList()->push_back(yymsp[0].minor.yy155);
}
#line 1311 "astgen.c"
        break;
      case 4:
      case 21:
#line 95 "astgen.in"
{ yygotominor.yy155 = 0; }
#line 1317 "astgen.c"
        break;
      case 19:
      case 89:
#line 114 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(empty_statement); }
#line 1323 "astgen.c"
        break;
      case 24:
#line 130 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(assignment_expression, yymsp[-1].minor.yy246, yymsp[-2].minor.yy155, yymsp[0].minor.yy155); }
#line 1328 "astgen.c"
        break;
      case 25:
#line 134 "astgen.in"
{ yygotominor.yy246 = op_assign; }
#line 1333 "astgen.c"
        break;
      case 26:
#line 135 "astgen.in"
{ yygotominor.yy246 = op_assadd; }
#line 1338 "astgen.c"
        break;
      case 27:
#line 136 "astgen.in"
{ yygotominor.yy246 = op_asssub; }
#line 1343 "astgen.c"
        break;
      case 28:
#line 137 "astgen.in"
{ yygotominor.yy246 = op_assmul; }
#line 1348 "astgen.c"
        break;
      case 29:
#line 138 "astgen.in"
{ yygotominor.yy246 = op_assdiv; }
#line 1353 "astgen.c"
        break;
      case 30:
#line 139 "astgen.in"
{ yygotominor.yy246 = op_assmod; }
#line 1358 "astgen.c"
        break;
      case 32:
#line 143 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(ternary_expression, yymsp[-4].minor.yy155, yymsp[-2].minor.yy155, yymsp[0].minor.yy155); }
#line 1363 "astgen.c"
        break;
      case 34:
#line 147 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(binary_expression, op_logor,   yymsp[-2].minor.yy155, yymsp[0].minor.yy155); }
#line 1368 "astgen.c"
        break;
      case 35:
#line 148 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(binary_expression, op_logand,  yymsp[-2].minor.yy155, yymsp[0].minor.yy155); }
#line 1373 "astgen.c"
        break;
      case 36:
#line 149 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(binary_expression, op_bitor,   yymsp[-2].minor.yy155, yymsp[0].minor.yy155); }
#line 1378 "astgen.c"
        break;
      case 37:
#line 150 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(binary_expression, op_bitxor,  yymsp[-2].minor.yy155, yymsp[0].minor.yy155); }
#line 1383 "astgen.c"
        break;
      case 38:
#line 151 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(binary_expression, op_bitand,  yymsp[-2].minor.yy155, yymsp[0].minor.yy155); }
#line 1388 "astgen.c"
        break;
      case 39:
#line 152 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(binary_expression, op_eq,   yymsp[-2].minor.yy155, yymsp[0].minor.yy155); }
#line 1393 "astgen.c"
        break;
      case 40:
#line 153 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(binary_expression, op_ne,   yymsp[-2].minor.yy155, yymsp[0].minor.yy155); }
#line 1398 "astgen.c"
        break;
      case 41:
#line 154 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(binary_expression, op_lt,   yymsp[-2].minor.yy155, yymsp[0].minor.yy155); }
#line 1403 "astgen.c"
        break;
      case 42:
#line 155 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(binary_expression, op_le,   yymsp[-2].minor.yy155, yymsp[0].minor.yy155); }
#line 1408 "astgen.c"
        break;
      case 43:
#line 156 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(binary_expression, op_gt,   yymsp[-2].minor.yy155, yymsp[0].minor.yy155); }
#line 1413 "astgen.c"
        break;
      case 44:
#line 157 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(binary_expression, op_ge,   yymsp[-2].minor.yy155, yymsp[0].minor.yy155); }
#line 1418 "astgen.c"
        break;
      case 45:
#line 158 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(binary_expression, op_add,  yymsp[-2].minor.yy155, yymsp[0].minor.yy155); }
#line 1423 "astgen.c"
        break;
      case 46:
#line 159 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(binary_expression, op_sub,  yymsp[-2].minor.yy155, yymsp[0].minor.yy155); }
#line 1428 "astgen.c"
        break;
      case 47:
#line 160 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(binary_expression, op_mul,  yymsp[-2].minor.yy155, yymsp[0].minor.yy155); }
#line 1433 "astgen.c"
        break;
      case 48:
#line 161 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(binary_expression, op_div,  yymsp[-2].minor.yy155, yymsp[0].minor.yy155); }
#line 1438 "astgen.c"
        break;
      case 49:
#line 162 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(binary_expression, op_mod,  yymsp[-2].minor.yy155, yymsp[0].minor.yy155); }
#line 1443 "astgen.c"
        break;
      case 50:
#line 163 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(binary_expression, op_seq,  yymsp[-2].minor.yy155, yymsp[0].minor.yy155); }
#line 1448 "astgen.c"
        break;
      case 51:
#line 164 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(binary_expression, op_sne,  yymsp[-2].minor.yy155, yymsp[0].minor.yy155); }
#line 1453 "astgen.c"
        break;
      case 53:
#line 168 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(prefix_expression, op_negate, yymsp[0].minor.yy155); }
#line 1458 "astgen.c"
        break;
      case 54:
#line 169 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(prefix_expression, op_preinc, yymsp[0].minor.yy155); }
#line 1463 "astgen.c"
        break;
      case 55:
#line 170 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(prefix_expression, op_predec, yymsp[0].minor.yy155); }
#line 1468 "astgen.c"
        break;
      case 56:
#line 171 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(prefix_expression, op_not,    yymsp[0].minor.yy155); }
#line 1473 "astgen.c"
        break;
      case 59:
#line 176 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(postfix_expression, op_postinc, yymsp[-1].minor.yy155); }
#line 1478 "astgen.c"
        break;
      case 60:
#line 177 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(postfix_expression, op_postdec, yymsp[-1].minor.yy155); }
#line 1483 "astgen.c"
        break;
      case 62:
#line 179 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(member_expression, yymsp[-2].minor.yy155, String(yymsp[0].minor.yy0)); }
#line 1488 "astgen.c"
        break;
      case 63:
#line 180 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(member_call, yymsp[-2].minor.yy155, yymsp[0].minor.yy155); }
#line 1493 "astgen.c"
        break;
      case 64:
#line 181 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(index_expression, yymsp[-3].minor.yy155, yymsp[-1].minor.yy155); }
#line 1498 "astgen.c"
        break;
      case 65:
#line 184 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(function_call, String(yymsp[-2].minor.yy0)); }
#line 1503 "astgen.c"
        break;
      case 66:
#line 185 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(function_call, String(yymsp[-3].minor.yy0), yymsp[-1].minor.yy155); }
#line 1508 "astgen.c"
        break;
      case 69:
      case 100:
      case 101:
      case 102:
      case 110:
      case 115:
      case 133:
#line 191 "astgen.in"
{ yygotominor.yy155 = yymsp[-1].minor.yy155; }
#line 1519 "astgen.c"
        break;
      case 71:
#line 195 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(literal, Variant(String(yymsp[0].minor.yy0), Variant::stInt)); }
#line 1524 "astgen.c"
        break;
      case 72:
#line 196 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(literal, Variant(hex2dec(String(yymsp[0].minor.yy0).c_str()))); }
#line 1529 "astgen.c"
        break;
      case 73:
#line 197 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(literal, Variant(bin2dec(String(yymsp[0].minor.yy0).c_str()))); }
#line 1534 "astgen.c"
        break;
      case 74:
#line 198 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(literal, Variant(rom2dec(String(yymsp[0].minor.yy0).c_str()))); }
#line 1539 "astgen.c"
        break;
      case 75:
#line 199 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(literal, Variant(String(yymsp[0].minor.yy0), Variant::stReal)); }
#line 1544 "astgen.c"
        break;
      case 76:
#line 200 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(literal, Variant(String(yymsp[0].minor.yy0), Variant::stString)); }
#line 1549 "astgen.c"
        break;
      case 77:
#line 201 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(literal, Variant(true));    }
#line 1554 "astgen.c"
        break;
      case 78:
#line 202 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(literal, Variant(false));   }
#line 1559 "astgen.c"
        break;
      case 79:
#line 203 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(literal, Variant());        }
#line 1564 "astgen.c"
        break;
      case 80:
#line 206 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(lvalue, String(yymsp[0].minor.yy0)); }
#line 1569 "astgen.c"
        break;
      case 81:
#line 209 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(list_literal, yymsp[-1].minor.yy155); }
#line 1574 "astgen.c"
        break;
      case 82:
#line 210 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(list_content, yymsp[0].minor.yy155); }
#line 1579 "astgen.c"
        break;
      case 83:
#line 211 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(list_content, yymsp[-2].minor.yy155, yymsp[0].minor.yy155); }
#line 1584 "astgen.c"
        break;
      case 84:
#line 212 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(list_entry, yymsp[0].minor.yy155); }
#line 1589 "astgen.c"
        break;
      case 85:
#line 215 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(new_expression, String(yymsp[0].minor.yy0)); }
#line 1594 "astgen.c"
        break;
      case 86:
#line 224 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(argument, yymsp[0].minor.yy155); }
#line 1599 "astgen.c"
        break;
      case 88:
#line 228 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(argument_list, yymsp[-2].minor.yy155, yymsp[0].minor.yy155); }
#line 1604 "astgen.c"
        break;
      case 90:
#line 237 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(expression_statement, yymsp[-1].minor.yy155); }
#line 1609 "astgen.c"
        break;
      case 91:
#line 240 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(compound_statement); }
#line 1614 "astgen.c"
        break;
      case 92:
#line 241 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(compound_statement, yymsp[-1].minor.yy155); }
#line 1619 "astgen.c"
        break;
      case 93:
#line 244 "astgen.in"
{ p->Parse(yymsp[-1].minor.yy0); yygotominor.yy155 = p->GetRoot(); }
#line 1624 "astgen.c"
        break;
      case 94:
#line 247 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(return_statement, yymsp[-1].minor.yy155); }
#line 1629 "astgen.c"
        break;
      case 95:
#line 248 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(return_statement);    }
#line 1634 "astgen.c"
        break;
      case 96:
#line 251 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(break_statement); }
#line 1639 "astgen.c"
        break;
      case 97:
#line 252 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(continue_statement); }
#line 1644 "astgen.c"
        break;
      case 98:
#line 255 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(pause_statement); }
#line 1649 "astgen.c"
        break;
      case 104:
#line 270 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(variable_declaration, String(yymsp[0].minor.yy0));    }
#line 1654 "astgen.c"
        break;
      case 105:
#line 271 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(variable_declaration, String(yymsp[-2].minor.yy0), yymsp[0].minor.yy155); }
#line 1659 "astgen.c"
        break;
      case 107:
#line 274 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(declaration_sequence, yymsp[-2].minor.yy155, yymsp[0].minor.yy155); }
#line 1664 "astgen.c"
        break;
      case 108:
#line 283 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(struct_declaration, String(yymsp[-2].minor.yy0)); }
#line 1669 "astgen.c"
        break;
      case 109:
#line 284 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(struct_declaration, String(yymsp[-3].minor.yy0), yymsp[-1].minor.yy155); }
#line 1674 "astgen.c"
        break;
      case 112:
#line 291 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(struct_members, yymsp[-1].minor.yy155, yymsp[0].minor.yy155); }
#line 1679 "astgen.c"
        break;
      case 113:
#line 298 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(class_declaration, String(yymsp[-2].minor.yy0)); }
#line 1684 "astgen.c"
        break;
      case 114:
#line 299 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(class_declaration, String(yymsp[-3].minor.yy0), yymsp[-1].minor.yy99); }
#line 1689 "astgen.c"
        break;
      case 117:
#line 305 "astgen.in"
{ 
  yygotominor.yy99 = new AstList;
  yygotominor.yy99->push_back(yymsp[0].minor.yy155);
}
#line 1697 "astgen.c"
        break;
      case 118:
#line 309 "astgen.in"
{ 
  yygotominor.yy99 = yymsp[-1].minor.yy99;
  yygotominor.yy99->push_back(yymsp[0].minor.yy155);
}
#line 1705 "astgen.c"
        break;
      case 119:
#line 320 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(function_declaration, String(yymsp[-4].minor.yy0), yymsp[-2].minor.yy155, yymsp[0].minor.yy155); }
#line 1710 "astgen.c"
        break;
      case 120:
#line 321 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(function_declaration, String(yymsp[-3].minor.yy0), AstData(), yymsp[0].minor.yy155); }
#line 1715 "astgen.c"
        break;
      case 121:
#line 324 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(parameter, String(yymsp[0].minor.yy0)); }
#line 1720 "astgen.c"
        break;
      case 122:
#line 327 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(parameter, String(yymsp[-2].minor.yy0), yymsp[0].minor.yy155); }
#line 1725 "astgen.c"
        break;
      case 124:
      case 126:
      case 129:
#line 331 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(parameter_list, yymsp[-2].minor.yy155, yymsp[0].minor.yy155); }
#line 1732 "astgen.c"
        break;
      case 131:
#line 352 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(for_statement, yymsp[-5].minor.yy155, yymsp[-4].minor.yy155, yymsp[-2].minor.yy155, yymsp[0].minor.yy155); }
#line 1737 "astgen.c"
        break;
      case 134:
      case 135:
#line 363 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(foreach_statement, yymsp[-4].minor.yy155, yymsp[-2].minor.yy155, yymsp[0].minor.yy155); }
#line 1743 "astgen.c"
        break;
      case 136:
#line 365 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(variable_declaration, String(yymsp[0].minor.yy0)); }
#line 1748 "astgen.c"
        break;
      case 137:
#line 376 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(if_statement, yymsp[-2].minor.yy155, yymsp[0].minor.yy155); }
#line 1753 "astgen.c"
        break;
      case 138:
#line 377 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(if_statement, yymsp[-4].minor.yy155, yymsp[-2].minor.yy155, yymsp[0].minor.yy155); }
#line 1758 "astgen.c"
        break;
      case 139:
#line 385 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(while_statement, yymsp[-2].minor.yy155,  yymsp[0].minor.yy155); }
#line 1763 "astgen.c"
        break;
      case 140:
#line 393 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(switch_statement, yymsp[-4].minor.yy155, yymsp[-1].minor.yy99); }
#line 1768 "astgen.c"
        break;
      case 141:
#line 397 "astgen.in"
{ yygotominor.yy99 = new AstList; }
#line 1773 "astgen.c"
        break;
      case 142:
      case 143:
#line 398 "astgen.in"
{ yygotominor.yy99 = yymsp[-1].minor.yy99; yygotominor.yy99->push_back(yymsp[0].minor.yy155); }
#line 1779 "astgen.c"
        break;
      case 144:
#line 402 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(switch_case, yymsp[-2].minor.yy155, yymsp[0].minor.yy155); }
#line 1784 "astgen.c"
        break;
      case 145:
#line 405 "astgen.in"
{ yygotominor.yy155 = p->AllocAst(default_case, yymsp[0].minor.yy155); }
#line 1789 "astgen.c"
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
#line 1837 "astgen.c"
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
#line 1855 "astgen.c"
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

