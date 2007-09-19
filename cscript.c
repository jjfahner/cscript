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
#define YYNOCODE 124
#define YYACTIONTYPE unsigned short int
#define CScriptParseTOKENTYPE  Token 
typedef union {
  CScriptParseTOKENTYPE yy0;
  opcodes yy12;
  AccessTypes yy36;
  AstList* yy61;
  Ast* yy181;
  int yy247;
} YYMINORTYPE;
#ifndef YYSTACKDEPTH
#define YYSTACKDEPTH 100
#endif
#define CScriptParseARG_SDECL  Parser* p ;
#define CScriptParseARG_PDECL , Parser* p 
#define CScriptParseARG_FETCH  Parser* p  = yypParser->p 
#define CScriptParseARG_STORE yypParser->p  = p 
#define YYNSTATE 238
#define YYNRULE 140
#define YYERRORSYMBOL 70
#define YYERRSYMDT yy247
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
 /*     0 */   176,  379,  228,  224,   10,  218,  213,  209,  206,  205,
 /*    10 */   195,  192,  190,  187,  185,  183,  180,  178,  125,  172,
 /*    20 */    67,  150,   62,  196,  131,   81,  155,  156,  158,   53,
 /*    30 */   160,  164,  194,  162,   18,   88,  141,  118,  117,  235,
 /*    40 */    57,   51,   52,   54,  167,   90,   17,  166,   28,   94,
 /*    50 */   203,  208,  226,  225,  223,  221,  217,  214,  212,  211,
 /*    60 */   135,   98,  134,    3,  237,  128,   20,  120,  119,   61,
 /*    70 */   122,   53,  132,   61,   97,  101,  110,  153,   97,  109,
 /*    80 */    87,   93,   58,   51,   52,   54,   13,   90,   17,   85,
 /*    90 */    28,  231,  203,  208,  226,  225,  223,  221,  217,  214,
 /*   100 */   212,  211,   96,   98,  134,    3,  129,  128,   20,  120,
 /*   110 */   119,   61,  122,   53,  201,  202,   97,  101,   34,   45,
 /*   120 */    46,  109,   87,   93,  127,   51,   52,   54,    9,   90,
 /*   130 */    17,  230,   28,  138,  203,  208,  226,  225,  223,  221,
 /*   140 */   217,  214,  212,  211,  229,   98,  134,    3,  140,  128,
 /*   150 */    20,  120,  119,   61,  122,  162,    4,   88,   97,  101,
 /*   160 */    56,  236,   57,  109,   87,   93,  176,   66,  152,    8,
 /*   170 */     7,  218,  213,  209,  206,  205,  195,  192,  190,  187,
 /*   180 */   185,  183,  180,  178,  125,  172,  143,  150,   62,  102,
 /*   190 */   131,   81,  155,  156,  158,  145,  160,  164,  194,  184,
 /*   200 */   114,   29,  141,  118,  117,   44,   37,   36,   35,   49,
 /*   210 */    33,   38,   48,   34,   45,   46,  176,   92,  210,   16,
 /*   220 */     7,  218,  213,  209,  206,  205,  195,  192,  190,  187,
 /*   230 */   185,  183,  180,  178,  125,  172,   31,  150,   62,   59,
 /*   240 */   131,   81,  155,  156,  158,   86,  160,  164,  194,  222,
 /*   250 */    21,  182,  141,  118,  117,   65,  133,   74,   22,  131,
 /*   260 */    81,  155,  156,  158,  176,  160,  164,  194,  207,  174,
 /*   270 */   213,  209,  206,  205,  195,  192,  190,  187,  185,  183,
 /*   280 */   180,  178,  125,  172,   23,  150,   62,   12,  131,   81,
 /*   290 */   155,  156,  158,  367,  160,  164,  194,  161,  179,   15,
 /*   300 */   141,  118,  117,   19,    6,    1,  176,   11,  204,  173,
 /*   310 */     5,  218,  213,  209,  206,  205,  195,  192,  190,  187,
 /*   320 */   185,  183,  180,  178,  125,  172,   30,  150,   62,   24,
 /*   330 */   131,   81,  155,  156,  158,   21,  160,  164,  194,  165,
 /*   340 */   176,   14,  141,  118,  117,  181,  213,  209,  206,  205,
 /*   350 */   195,  192,  190,  187,  185,  183,  180,  178,  125,  172,
 /*   360 */   126,  150,   62,    2,  131,   81,  155,  156,  158,   55,
 /*   370 */   160,  164,  194,   32,  159,  380,  141,  118,  117,   61,
 /*   380 */   176,  232,  233,  234,   97,  215,  213,  209,  206,  205,
 /*   390 */   195,  192,  190,  187,  185,  183,  180,  178,  125,  172,
 /*   400 */   113,  150,   62,  197,  131,   81,  155,  156,  158,  380,
 /*   410 */   160,  164,  194,  380,  176,  380,  141,  118,  117,  193,
 /*   420 */   213,  209,  206,  205,  195,  192,  190,  187,  185,  183,
 /*   430 */   180,  178,  125,  172,  380,  150,   62,  380,  131,   81,
 /*   440 */   155,  156,  158,  380,  160,  164,  194,  380,  176,  380,
 /*   450 */   141,  118,  117,  123,  213,  209,  206,  205,  195,  192,
 /*   460 */   190,  187,  185,  183,  180,  178,  125,  172,  380,  150,
 /*   470 */    62,  380,  131,   81,  155,  156,  158,  380,  160,  164,
 /*   480 */   194,  380,  176,  380,  141,  118,  117,  219,  213,  209,
 /*   490 */   206,  205,  195,  192,  190,  187,  185,  183,  180,  178,
 /*   500 */   125,  172,  380,  150,   62,  380,  131,   81,  155,  156,
 /*   510 */   158,  380,  160,  164,  194,  380,  176,  380,  141,  118,
 /*   520 */   117,  227,  213,  209,  206,  205,  195,  192,  190,  187,
 /*   530 */   185,  183,  180,  178,  125,  172,  380,  150,   62,  380,
 /*   540 */   131,   81,  155,  156,  158,  380,  160,  164,  194,  380,
 /*   550 */   176,  380,  141,  118,  117,  188,  213,  209,  206,  205,
 /*   560 */   195,  192,  190,  187,  185,  183,  180,  178,  125,  172,
 /*   570 */   380,  150,   62,  380,  131,   81,  155,  156,  158,  380,
 /*   580 */   160,  164,  194,  380,  380,  380,  141,  118,  117,   43,
 /*   590 */    42,   41,   40,   39,   50,   47,   44,   37,   36,   35,
 /*   600 */    49,   33,   38,   48,   34,   45,   46,  177,  175,  171,
 /*   610 */   170,  169,  168,   27,   53,   82,  380,  131,   81,  155,
 /*   620 */   156,  158,  380,  160,  164,  194,   51,   52,   54,  380,
 /*   630 */    90,   17,  200,   28,  380,  203,  208,  226,  225,  223,
 /*   640 */   221,  217,  214,  212,  211,  380,   98,  134,  157,   53,
 /*   650 */    63,  100,  380,   61,   60,  232,  233,  234,   97,  380,
 /*   660 */   380,   51,   52,   54,  380,   90,   17,  380,   28,  380,
 /*   670 */   203,  208,  226,  225,  223,  221,  217,  214,  212,  211,
 /*   680 */   380,   98,  136,  144,   53,  131,   81,  155,  156,  158,
 /*   690 */   380,  160,  164,  194,  380,  380,   51,   52,   54,  380,
 /*   700 */    90,   17,  380,   28,  380,  203,  208,  226,  225,  223,
 /*   710 */   221,  217,  214,  212,  211,  380,   98,   42,   41,   40,
 /*   720 */    39,   50,   47,   44,   37,   36,   35,   49,   33,   38,
 /*   730 */    48,   34,   45,   46,   41,   40,   39,   50,   47,   44,
 /*   740 */    37,   36,   35,   49,   33,   38,   48,   34,   45,   46,
 /*   750 */   220,   36,   35,   49,   33,   38,   48,   34,   45,   46,
 /*   760 */   380,  125,  172,  380,  150,   62,  380,  131,   81,  155,
 /*   770 */   156,  158,  380,  160,  103,  194,  148,   81,  155,  156,
 /*   780 */   158,  112,  160,  164,  194,   38,   48,   34,   45,   46,
 /*   790 */    26,  115,  380,  380,  380,   40,   39,   50,   47,   44,
 /*   800 */    37,   36,   35,   49,   33,   38,   48,   34,   45,   46,
 /*   810 */   191,  172,  380,  150,   62,  380,  131,   81,  155,  156,
 /*   820 */   158,   84,  160,  164,  194,  198,  172,  186,  150,   62,
 /*   830 */   380,  131,   81,  155,  156,  158,  380,  160,  164,  194,
 /*   840 */   104,   89,  137,  142,  380,   64,  380,   25,  380,  380,
 /*   850 */   198,  172,  380,  150,   62,  380,  131,   81,  155,  156,
 /*   860 */   158,  380,  160,  164,  194,  199,   89,  380,   39,   50,
 /*   870 */    47,   44,   37,   36,   35,   49,   33,   38,   48,   34,
 /*   880 */    45,   46,  380,  380,  216,  172,  108,  150,   62,  380,
 /*   890 */   131,   81,  155,  156,  158,  380,  160,  164,  194,   50,
 /*   900 */    47,   44,   37,   36,   35,   49,   33,   38,   48,   34,
 /*   910 */    45,   46,  191,  172,  380,  150,   62,  380,  131,   81,
 /*   920 */   155,  156,  158,  380,  160,  164,  194,  116,  172,  189,
 /*   930 */   150,   62,  380,  131,   81,  155,  156,  158,  380,  160,
 /*   940 */   164,  194,  105,  172,  380,  150,   62,  380,  131,   81,
 /*   950 */   155,  156,  158,  380,  160,  164,  194,   91,  172,  380,
 /*   960 */   150,   62,  380,  131,   81,  155,  156,  158,  380,  160,
 /*   970 */   164,  194,  380,   99,  172,  380,  150,   62,  380,  131,
 /*   980 */    81,  155,  156,  158,  380,  160,  164,  194,  151,  172,
 /*   990 */   380,  150,   62,  380,  131,   81,  155,  156,  158,  380,
 /*  1000 */   160,  164,  194,  380,  106,  172,  380,  150,   62,  380,
 /*  1010 */   131,   81,  155,  156,  158,  380,  160,  164,  194,  121,
 /*  1020 */   172,  380,  150,   62,  380,  131,   81,  155,  156,  158,
 /*  1030 */   380,  160,  164,  194,  107,  172,  380,  150,   62,  380,
 /*  1040 */   131,   81,  155,  156,  158,  380,  160,  164,  194,  111,
 /*  1050 */   172,  380,  150,   62,  380,  131,   81,  155,  156,  158,
 /*  1060 */   380,  160,  164,  194,  380,   95,  172,  380,  150,   62,
 /*  1070 */   380,  131,   81,  155,  156,  158,  380,  160,  164,  194,
 /*  1080 */   124,  172,  380,  150,   62,  380,  131,   81,  155,  156,
 /*  1090 */   158,  380,  160,  164,  194,  163,  380,  150,   62,  380,
 /*  1100 */   131,   81,  155,  156,  158,  380,  160,  164,  194,  130,
 /*  1110 */   380,  150,   62,  380,  131,   81,  155,  156,  158,  380,
 /*  1120 */   160,  164,  194,  380,   79,  380,  131,   81,  155,  156,
 /*  1130 */   158,  380,  160,  164,  194,   76,  380,  131,   81,  155,
 /*  1140 */   156,  158,  380,  160,  164,  194,   73,  380,  131,   81,
 /*  1150 */   155,  156,  158,  380,  160,  164,  194,  380,   68,  380,
 /*  1160 */   131,   81,  155,  156,  158,  380,  160,  164,  194,  380,
 /*  1170 */    69,  380,  131,   81,  155,  156,  158,  380,  160,  164,
 /*  1180 */   194,   70,  380,  131,   81,  155,  156,  158,  380,  160,
 /*  1190 */   164,  194,  380,  146,  380,  131,   81,  155,  156,  158,
 /*  1200 */   380,  160,  164,  194,   71,  380,  131,   81,  155,  156,
 /*  1210 */   158,  380,  160,  164,  194,   80,  380,  131,   81,  155,
 /*  1220 */   156,  158,  380,  160,  164,  194,   72,  380,  131,   81,
 /*  1230 */   155,  156,  158,  380,  160,  164,  194,   83,  380,  131,
 /*  1240 */    81,  155,  156,  158,  380,  160,  164,  194,   78,  380,
 /*  1250 */   131,   81,  155,  156,  158,  380,  160,  164,  194,   75,
 /*  1260 */   380,  131,   81,  155,  156,  158,  380,  160,  164,  194,
 /*  1270 */   380,   77,  380,  131,   81,  155,  156,  158,  380,  160,
 /*  1280 */   164,  194,  380,  208,  226,  225,  223,  221,  217,  214,
 /*  1290 */   212,  211,  139,  380,  131,   81,  155,  156,  158,  380,
 /*  1300 */   160,  164,  194,  147,   81,  155,  156,  158,  380,  160,
 /*  1310 */   164,  194,  154,   81,  155,  156,  158,  380,  160,  164,
 /*  1320 */   194,  149,   81,  155,  156,  158,  380,  160,  164,  194,
};
static const YYCODETYPE yy_lookahead[] = {
 /*     0 */    70,   71,   72,   73,   74,   75,   76,   77,   78,   79,
 /*    10 */    80,   81,   82,   83,   84,   85,   86,   87,   88,   89,
 /*    20 */    49,   91,   92,   35,   94,   95,   96,   97,   98,   15,
 /*    30 */   100,  101,  102,  106,   46,  108,  106,  107,  108,  112,
 /*    40 */   113,   27,   28,   29,   26,   31,   32,  106,   34,  108,
 /*    50 */    36,   37,   38,   39,   40,   41,   42,   43,   44,   45,
 /*    60 */    48,   47,   48,   49,   50,   51,   52,   53,   54,   55,
 /*    70 */    56,   15,   48,   55,   60,   61,  109,  110,   60,   65,
 /*    80 */    66,   67,  119,   27,   28,   29,   64,   31,   32,  114,
 /*    90 */    34,  116,   36,   37,   38,   39,   40,   41,   42,   43,
 /*   100 */    44,   45,   31,   47,   48,   49,   50,   51,   52,   53,
 /*   110 */    54,   55,   56,   15,  120,  121,   60,   61,   16,   17,
 /*   120 */    18,   65,   66,   67,   42,   27,   28,   29,   35,   31,
 /*   130 */    32,  116,   34,   48,   36,   37,   38,   39,   40,   41,
 /*   140 */    42,   43,   44,   45,   31,   47,   48,   49,   48,   51,
 /*   150 */    52,   53,   54,   55,   56,  106,   35,  108,   60,   61,
 /*   160 */   111,  112,  113,   65,   66,   67,   70,   46,  110,   35,
 /*   170 */    74,   75,   76,   77,   78,   79,   80,   81,   82,   83,
 /*   180 */    84,   85,   86,   87,   88,   89,   48,   91,   92,   31,
 /*   190 */    94,   95,   96,   97,   98,   48,  100,  101,  102,   98,
 /*   200 */    31,   62,  106,  107,  108,    8,    9,   10,   11,   12,
 /*   210 */    13,   14,   15,   16,   17,   18,   70,  100,  122,   34,
 /*   220 */    74,   75,   76,   77,   78,   79,   80,   81,   82,   83,
 /*   230 */    84,   85,   86,   87,   88,   89,   93,   91,   92,   34,
 /*   240 */    94,   95,   96,   97,   98,   31,  100,  101,  102,   48,
 /*   250 */    19,   35,  106,  107,  108,   46,   48,   92,   34,   94,
 /*   260 */    95,   96,   97,   98,   70,  100,  101,  102,  122,   75,
 /*   270 */    76,   77,   78,   79,   80,   81,   82,   83,   84,   85,
 /*   280 */    86,   87,   88,   89,   34,   91,   92,   35,   94,   95,
 /*   290 */    96,   97,   98,   62,  100,  101,  102,   48,   33,   46,
 /*   300 */   106,  107,  108,   48,   35,   26,   70,   35,   33,  115,
 /*   310 */    74,   75,   76,   77,   78,   79,   80,   81,   82,   83,
 /*   320 */    84,   85,   86,   87,   88,   89,   62,   91,   92,   34,
 /*   330 */    94,   95,   96,   97,   98,   19,  100,  101,  102,   48,
 /*   340 */    70,   34,  106,  107,  108,   75,   76,   77,   78,   79,
 /*   350 */    80,   81,   82,   83,   84,   85,   86,   87,   88,   89,
 /*   360 */    35,   91,   92,   26,   94,   95,   96,   97,   98,   49,
 /*   370 */   100,  101,  102,   26,   50,  123,  106,  107,  108,   55,
 /*   380 */    70,   57,   58,   59,   60,   75,   76,   77,   78,   79,
 /*   390 */    80,   81,   82,   83,   84,   85,   86,   87,   88,   89,
 /*   400 */    31,   91,   92,   31,   94,   95,   96,   97,   98,  123,
 /*   410 */   100,  101,  102,  123,   70,  123,  106,  107,  108,   75,
 /*   420 */    76,   77,   78,   79,   80,   81,   82,   83,   84,   85,
 /*   430 */    86,   87,   88,   89,  123,   91,   92,  123,   94,   95,
 /*   440 */    96,   97,   98,  123,  100,  101,  102,  123,   70,  123,
 /*   450 */   106,  107,  108,   75,   76,   77,   78,   79,   80,   81,
 /*   460 */    82,   83,   84,   85,   86,   87,   88,   89,  123,   91,
 /*   470 */    92,  123,   94,   95,   96,   97,   98,  123,  100,  101,
 /*   480 */   102,  123,   70,  123,  106,  107,  108,   75,   76,   77,
 /*   490 */    78,   79,   80,   81,   82,   83,   84,   85,   86,   87,
 /*   500 */    88,   89,  123,   91,   92,  123,   94,   95,   96,   97,
 /*   510 */    98,  123,  100,  101,  102,  123,   70,  123,  106,  107,
 /*   520 */   108,   75,   76,   77,   78,   79,   80,   81,   82,   83,
 /*   530 */    84,   85,   86,   87,   88,   89,  123,   91,   92,  123,
 /*   540 */    94,   95,   96,   97,   98,  123,  100,  101,  102,  123,
 /*   550 */    70,  123,  106,  107,  108,   75,   76,   77,   78,   79,
 /*   560 */    80,   81,   82,   83,   84,   85,   86,   87,   88,   89,
 /*   570 */   123,   91,   92,  123,   94,   95,   96,   97,   98,  123,
 /*   580 */   100,  101,  102,  123,  123,  123,  106,  107,  108,    1,
 /*   590 */     2,    3,    4,    5,    6,    7,    8,    9,   10,   11,
 /*   600 */    12,   13,   14,   15,   16,   17,   18,   19,   20,   21,
 /*   610 */    22,   23,   24,   25,   15,   92,  123,   94,   95,   96,
 /*   620 */    97,   98,  123,  100,  101,  102,   27,   28,   29,  123,
 /*   630 */    31,   32,   50,   34,  123,   36,   37,   38,   39,   40,
 /*   640 */    41,   42,   43,   44,   45,  123,   47,   48,   50,   15,
 /*   650 */    68,   69,  123,   55,   55,   57,   58,   59,   60,  123,
 /*   660 */   123,   27,   28,   29,  123,   31,   32,  123,   34,  123,
 /*   670 */    36,   37,   38,   39,   40,   41,   42,   43,   44,   45,
 /*   680 */   123,   47,   48,   92,   15,   94,   95,   96,   97,   98,
 /*   690 */   123,  100,  101,  102,  123,  123,   27,   28,   29,  123,
 /*   700 */    31,   32,  123,   34,  123,   36,   37,   38,   39,   40,
 /*   710 */    41,   42,   43,   44,   45,  123,   47,    2,    3,    4,
 /*   720 */     5,    6,    7,    8,    9,   10,   11,   12,   13,   14,
 /*   730 */    15,   16,   17,   18,    3,    4,    5,    6,    7,    8,
 /*   740 */     9,   10,   11,   12,   13,   14,   15,   16,   17,   18,
 /*   750 */    77,   10,   11,   12,   13,   14,   15,   16,   17,   18,
 /*   760 */   123,   88,   89,  123,   91,   92,  123,   94,   95,   96,
 /*   770 */    97,   98,  123,  100,  101,  102,   94,   95,   96,   97,
 /*   780 */    98,  108,  100,  101,  102,   14,   15,   16,   17,   18,
 /*   790 */   117,  118,  123,  123,  123,    4,    5,    6,    7,    8,
 /*   800 */     9,   10,   11,   12,   13,   14,   15,   16,   17,   18,
 /*   810 */    88,   89,  123,   91,   92,  123,   94,   95,   96,   97,
 /*   820 */    98,   99,  100,  101,  102,   88,   89,  105,   91,   92,
 /*   830 */   123,   94,   95,   96,   97,   98,  123,  100,  101,  102,
 /*   840 */   103,  104,   27,   28,  123,   30,  123,   32,  123,  123,
 /*   850 */    88,   89,  123,   91,   92,  123,   94,   95,   96,   97,
 /*   860 */    98,  123,  100,  101,  102,  103,  104,  123,    5,    6,
 /*   870 */     7,    8,    9,   10,   11,   12,   13,   14,   15,   16,
 /*   880 */    17,   18,  123,  123,   88,   89,   90,   91,   92,  123,
 /*   890 */    94,   95,   96,   97,   98,  123,  100,  101,  102,    6,
 /*   900 */     7,    8,    9,   10,   11,   12,   13,   14,   15,   16,
 /*   910 */    17,   18,   88,   89,  123,   91,   92,  123,   94,   95,
 /*   920 */    96,   97,   98,  123,  100,  101,  102,   88,   89,  105,
 /*   930 */    91,   92,  123,   94,   95,   96,   97,   98,  123,  100,
 /*   940 */   101,  102,   88,   89,  123,   91,   92,  123,   94,   95,
 /*   950 */    96,   97,   98,  123,  100,  101,  102,   88,   89,  123,
 /*   960 */    91,   92,  123,   94,   95,   96,   97,   98,  123,  100,
 /*   970 */   101,  102,  123,   88,   89,  123,   91,   92,  123,   94,
 /*   980 */    95,   96,   97,   98,  123,  100,  101,  102,   88,   89,
 /*   990 */   123,   91,   92,  123,   94,   95,   96,   97,   98,  123,
 /*  1000 */   100,  101,  102,  123,   88,   89,  123,   91,   92,  123,
 /*  1010 */    94,   95,   96,   97,   98,  123,  100,  101,  102,   88,
 /*  1020 */    89,  123,   91,   92,  123,   94,   95,   96,   97,   98,
 /*  1030 */   123,  100,  101,  102,   88,   89,  123,   91,   92,  123,
 /*  1040 */    94,   95,   96,   97,   98,  123,  100,  101,  102,   88,
 /*  1050 */    89,  123,   91,   92,  123,   94,   95,   96,   97,   98,
 /*  1060 */   123,  100,  101,  102,  123,   88,   89,  123,   91,   92,
 /*  1070 */   123,   94,   95,   96,   97,   98,  123,  100,  101,  102,
 /*  1080 */    88,   89,  123,   91,   92,  123,   94,   95,   96,   97,
 /*  1090 */    98,  123,  100,  101,  102,   89,  123,   91,   92,  123,
 /*  1100 */    94,   95,   96,   97,   98,  123,  100,  101,  102,   89,
 /*  1110 */   123,   91,   92,  123,   94,   95,   96,   97,   98,  123,
 /*  1120 */   100,  101,  102,  123,   92,  123,   94,   95,   96,   97,
 /*  1130 */    98,  123,  100,  101,  102,   92,  123,   94,   95,   96,
 /*  1140 */    97,   98,  123,  100,  101,  102,   92,  123,   94,   95,
 /*  1150 */    96,   97,   98,  123,  100,  101,  102,  123,   92,  123,
 /*  1160 */    94,   95,   96,   97,   98,  123,  100,  101,  102,  123,
 /*  1170 */    92,  123,   94,   95,   96,   97,   98,  123,  100,  101,
 /*  1180 */   102,   92,  123,   94,   95,   96,   97,   98,  123,  100,
 /*  1190 */   101,  102,  123,   92,  123,   94,   95,   96,   97,   98,
 /*  1200 */   123,  100,  101,  102,   92,  123,   94,   95,   96,   97,
 /*  1210 */    98,  123,  100,  101,  102,   92,  123,   94,   95,   96,
 /*  1220 */    97,   98,  123,  100,  101,  102,   92,  123,   94,   95,
 /*  1230 */    96,   97,   98,  123,  100,  101,  102,   92,  123,   94,
 /*  1240 */    95,   96,   97,   98,  123,  100,  101,  102,   92,  123,
 /*  1250 */    94,   95,   96,   97,   98,  123,  100,  101,  102,   92,
 /*  1260 */   123,   94,   95,   96,   97,   98,  123,  100,  101,  102,
 /*  1270 */   123,   92,  123,   94,   95,   96,   97,   98,  123,  100,
 /*  1280 */   101,  102,  123,   37,   38,   39,   40,   41,   42,   43,
 /*  1290 */    44,   45,   92,  123,   94,   95,   96,   97,   98,  123,
 /*  1300 */   100,  101,  102,   94,   95,   96,   97,   98,  123,  100,
 /*  1310 */   101,  102,   94,   95,   96,   97,   98,  123,  100,  101,
 /*  1320 */   102,   94,   95,   96,   97,   98,  123,  100,  101,  102,
};
#define YY_SHIFT_USE_DFLT (-30)
#define YY_SHIFT_MAX 128
static const short yy_shift_ofst[] = {
 /*     0 */    98,   98,   98,   14,   98,   56,   98,   98,   98,   98,
 /*    10 */    98,   98,   98,   98,  599,  669,  669,  669,  669,  669,
 /*    20 */   634,  669,  669,  669,  669,  669,  669,  669,  669,  669,
 /*    30 */   669,  669,  669,  669,  669,  669,  669,  669,  669,  669,
 /*    40 */   669,  669,  669,  669,  669,  669,  669,  669,  669,  669,
 /*    50 */   669,  669,  669,  669,  669,  598,  324,   18,  582,  113,
 /*    60 */   214,  158,  588, 1246,  169,  158,  113,  -30,  715,  731,
 /*    70 */   791,  863,  893,  197,  197,  741,  741,  771,  771,  771,
 /*    80 */   771,  815,  102,  102,  -12,  121,  231,  250,  249,  253,
 /*    90 */   185,  272,  279,  295,  291,  325,  320,  369,  372,  347,
 /*   100 */   337,  307,  316,  264,  275,  269,  255,  265,  252,  224,
 /*   110 */   209,  216,  201,  205,  185,  139,  134,  147,  138,  100,
 /*   120 */    85,   93,   71,   22,   12,  208,  -29,   24,   82,
};
#define YY_REDUCE_USE_DFLT (-74)
#define YY_REDUCE_MAX 67
static const short yy_reduce_ofst[] = {
 /*     0 */   -70,  146,   96,  236,  194,  310,  270,  310,  446,  378,
 /*    10 */   310,  344,  412,  480,  673,  762,  722,  737,  824,  796,
 /*    20 */   992,  900,  931,  869,  977,  946,  916,  885,  961,  839,
 /*    30 */   854, 1020, 1006, 1123, 1101, 1156, 1179, 1167, 1145, 1134,
 /*    40 */  1112, 1089, 1078, 1066, 1043,  591, 1200,  165,  523, 1032,
 /*    50 */  1054,  682, 1227, 1209, 1218,   49,  -73,  -59,   -6,  -25,
 /*    60 */   -33,  -33,  143,  117,  101,   58,   15,  -37,
};
static const YYACTIONTYPE yy_default[] = {
 /*     0 */   242,  378,  378,  378,  378,  378,  378,  377,  378,  378,
 /*    10 */   243,  378,  378,  378,  378,  378,  324,  378,  378,  258,
 /*    20 */   378,  378,  378,  378,  378,  378,  378,  378,  378,  378,
 /*    30 */   378,  378,  378,  378,  378,  378,  378,  378,  378,  378,
 /*    40 */   378,  378,  378,  378,  378,  378,  378,  378,  378,  378,
 /*    50 */   378,  378,  378,  378,  378,  378,  378,  378,  378,  358,
 /*    60 */   378,  378,  268,  378,  378,  378,  378,  372,  271,  272,
 /*    70 */   273,  274,  275,  287,  288,  277,  276,  278,  279,  280,
 /*    80 */   281,  289,  283,  282,  378,  378,  340,  378,  378,  319,
 /*    90 */   317,  378,  378,  378,  378,  378,  378,  378,  378,  378,
 /*   100 */   378,  378,  340,  304,  378,  378,  378,  378,  378,  378,
 /*   110 */   339,  378,  378,  378,  299,  378,  378,  378,  378,  378,
 /*   120 */   378,  378,  378,  368,  378,  378,  378,  378,  378,  330,
 /*   130 */   261,  270,  331,  328,  327,  332,  333,  296,  334,  286,
 /*   140 */   335,  336,  297,  337,  285,  338,  284,  290,  291,  292,
 /*   150 */   260,  341,  343,  342,  293,  294,  295,  344,  298,  345,
 /*   160 */   303,  346,  347,  269,  304,  348,  349,  350,  267,  266,
 /*   170 */   265,  264,  257,  356,  361,  263,  256,  262,  255,  301,
 /*   180 */   254,  365,  305,  253,  300,  252,  325,  251,  369,  326,
 /*   190 */   250,  323,  249,  370,  306,  248,  302,  322,  321,  320,
 /*   200 */   371,  373,  374,  307,  318,  247,  246,  375,  308,  245,
 /*   210 */   376,  316,  315,  244,  314,  241,  259,  313,  240,  362,
 /*   220 */   363,  312,  364,  311,  239,  310,  309,  366,  238,  357,
 /*   230 */   360,  359,  351,  352,  353,  355,  354,  329,
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
  "parameter",     "for_init_statement",  "foreach_decl",  "switch_body", 
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
 /* 120 */ "parameter_list ::=",
 /* 121 */ "parameter_list ::= parameter",
 /* 122 */ "parameter_list ::= parameter_list COMMA parameter",
 /* 123 */ "function_body ::= statement",
 /* 124 */ "for_statement ::= FOR LPAREN for_init_statement expression SEMICOLON expression_opt RPAREN statement",
 /* 125 */ "for_init_statement ::= expression_statement",
 /* 126 */ "for_init_statement ::= variable_declaration SEMICOLON",
 /* 127 */ "foreach_statement ::= FOR LPAREN id_expression IN expression RPAREN statement",
 /* 128 */ "foreach_statement ::= FOR LPAREN foreach_decl IN expression RPAREN statement",
 /* 129 */ "foreach_decl ::= VAR IDENTIFIER",
 /* 130 */ "if_statement ::= IF LPAREN expression RPAREN statement",
 /* 131 */ "if_statement ::= IF LPAREN expression RPAREN statement ELSE statement",
 /* 132 */ "while_statement ::= WHILE LPAREN expression RPAREN statement",
 /* 133 */ "switch_statement ::= SWITCH LPAREN expression RPAREN LBRACE switch_body RBRACE",
 /* 134 */ "switch_body ::=",
 /* 135 */ "switch_body ::= switch_body switch_case",
 /* 136 */ "switch_body ::= switch_body default_case",
 /* 137 */ "switch_case ::= CASE literal COLON case_statements",
 /* 138 */ "default_case ::= DEFAULT COLON case_statements",
 /* 139 */ "case_statements ::= statement_sequence",
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
  { 114, 0 },
  { 114, 1 },
  { 114, 3 },
  { 115, 1 },
  { 79, 8 },
  { 117, 1 },
  { 117, 2 },
  { 83, 7 },
  { 83, 7 },
  { 118, 2 },
  { 81, 5 },
  { 81, 7 },
  { 82, 5 },
  { 85, 7 },
  { 119, 0 },
  { 119, 2 },
  { 119, 2 },
  { 120, 4 },
  { 121, 3 },
  { 122, 1 },
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
{ p->SetRoot(yymsp[0].minor.yy181); }
#line 1207 "cscript.c"
        break;
      case 1:
#line 78 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(translation_unit, yymsp[0].minor.yy181); }
#line 1212 "cscript.c"
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
      case 123:
      case 125:
      case 139:
#line 81 "cscript.in"
{ yygotominor.yy181 = yymsp[0].minor.yy181; }
#line 1249 "cscript.c"
        break;
      case 3:
#line 82 "cscript.in"
{ 
  if(yymsp[-1].minor.yy181->m_type == statement_sequence) {
    yygotominor.yy181 = yymsp[-1].minor.yy181;
  }
  else {
    yygotominor.yy181 = p->AllocAst(statement_sequence, new AstList);
    yygotominor.yy181->m_a1.GetList()->push_back(yymsp[-1].minor.yy181);
  }
  yygotominor.yy181->m_a1.GetList()->push_back(yymsp[0].minor.yy181);
}
#line 1263 "cscript.c"
        break;
      case 4:
      case 20:
#line 95 "cscript.in"
{ yygotominor.yy181 = 0; }
#line 1269 "cscript.c"
        break;
      case 18:
      case 89:
#line 113 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(empty_statement); }
#line 1275 "cscript.c"
        break;
      case 23:
#line 129 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(assignment_expression, yymsp[-1].minor.yy12, yymsp[-2].minor.yy181, yymsp[0].minor.yy181); }
#line 1280 "cscript.c"
        break;
      case 24:
#line 133 "cscript.in"
{ yygotominor.yy12 = op_assign; }
#line 1285 "cscript.c"
        break;
      case 25:
#line 134 "cscript.in"
{ yygotominor.yy12 = op_assadd; }
#line 1290 "cscript.c"
        break;
      case 26:
#line 135 "cscript.in"
{ yygotominor.yy12 = op_asssub; }
#line 1295 "cscript.c"
        break;
      case 27:
#line 136 "cscript.in"
{ yygotominor.yy12 = op_assmul; }
#line 1300 "cscript.c"
        break;
      case 28:
#line 137 "cscript.in"
{ yygotominor.yy12 = op_assdiv; }
#line 1305 "cscript.c"
        break;
      case 29:
#line 138 "cscript.in"
{ yygotominor.yy12 = op_assmod; }
#line 1310 "cscript.c"
        break;
      case 31:
#line 142 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(ternary_expression, yymsp[-4].minor.yy181, yymsp[-2].minor.yy181, yymsp[0].minor.yy181); }
#line 1315 "cscript.c"
        break;
      case 33:
#line 146 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(binary_expression, op_logor,   yymsp[-2].minor.yy181, yymsp[0].minor.yy181); }
#line 1320 "cscript.c"
        break;
      case 34:
#line 147 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(binary_expression, op_logand,  yymsp[-2].minor.yy181, yymsp[0].minor.yy181); }
#line 1325 "cscript.c"
        break;
      case 35:
#line 148 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(binary_expression, op_bitor,   yymsp[-2].minor.yy181, yymsp[0].minor.yy181); }
#line 1330 "cscript.c"
        break;
      case 36:
#line 149 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(binary_expression, op_bitxor,  yymsp[-2].minor.yy181, yymsp[0].minor.yy181); }
#line 1335 "cscript.c"
        break;
      case 37:
#line 150 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(binary_expression, op_bitand,  yymsp[-2].minor.yy181, yymsp[0].minor.yy181); }
#line 1340 "cscript.c"
        break;
      case 38:
#line 151 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(binary_expression, op_eq,   yymsp[-2].minor.yy181, yymsp[0].minor.yy181); }
#line 1345 "cscript.c"
        break;
      case 39:
#line 152 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(binary_expression, op_ne,   yymsp[-2].minor.yy181, yymsp[0].minor.yy181); }
#line 1350 "cscript.c"
        break;
      case 40:
#line 153 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(binary_expression, op_lt,   yymsp[-2].minor.yy181, yymsp[0].minor.yy181); }
#line 1355 "cscript.c"
        break;
      case 41:
#line 154 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(binary_expression, op_le,   yymsp[-2].minor.yy181, yymsp[0].minor.yy181); }
#line 1360 "cscript.c"
        break;
      case 42:
#line 155 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(binary_expression, op_gt,   yymsp[-2].minor.yy181, yymsp[0].minor.yy181); }
#line 1365 "cscript.c"
        break;
      case 43:
#line 156 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(binary_expression, op_ge,   yymsp[-2].minor.yy181, yymsp[0].minor.yy181); }
#line 1370 "cscript.c"
        break;
      case 44:
#line 157 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(binary_expression, op_add,  yymsp[-2].minor.yy181, yymsp[0].minor.yy181); }
#line 1375 "cscript.c"
        break;
      case 45:
#line 158 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(binary_expression, op_sub,  yymsp[-2].minor.yy181, yymsp[0].minor.yy181); }
#line 1380 "cscript.c"
        break;
      case 46:
#line 159 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(binary_expression, op_mul,  yymsp[-2].minor.yy181, yymsp[0].minor.yy181); }
#line 1385 "cscript.c"
        break;
      case 47:
#line 160 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(binary_expression, op_div,  yymsp[-2].minor.yy181, yymsp[0].minor.yy181); }
#line 1390 "cscript.c"
        break;
      case 48:
#line 161 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(binary_expression, op_mod,  yymsp[-2].minor.yy181, yymsp[0].minor.yy181); }
#line 1395 "cscript.c"
        break;
      case 49:
#line 162 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(binary_expression, op_seq,  yymsp[-2].minor.yy181, yymsp[0].minor.yy181); }
#line 1400 "cscript.c"
        break;
      case 50:
#line 163 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(binary_expression, op_sne,  yymsp[-2].minor.yy181, yymsp[0].minor.yy181); }
#line 1405 "cscript.c"
        break;
      case 52:
#line 167 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(prefix_expression, op_negate, yymsp[0].minor.yy181); }
#line 1410 "cscript.c"
        break;
      case 53:
#line 168 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(prefix_expression, op_preinc, yymsp[0].minor.yy181); }
#line 1415 "cscript.c"
        break;
      case 54:
#line 169 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(prefix_expression, op_predec, yymsp[0].minor.yy181); }
#line 1420 "cscript.c"
        break;
      case 55:
#line 170 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(prefix_expression, op_not,    yymsp[0].minor.yy181); }
#line 1425 "cscript.c"
        break;
      case 58:
#line 175 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(postfix_expression, op_postinc, yymsp[-1].minor.yy181); }
#line 1430 "cscript.c"
        break;
      case 59:
#line 176 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(postfix_expression, op_postdec, yymsp[-1].minor.yy181); }
#line 1435 "cscript.c"
        break;
      case 61:
#line 178 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(member_expression, yymsp[-2].minor.yy181, String(yymsp[0].minor.yy0)); }
#line 1440 "cscript.c"
        break;
      case 62:
#line 179 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(member_call, yymsp[-2].minor.yy181, yymsp[0].minor.yy181); }
#line 1445 "cscript.c"
        break;
      case 63:
#line 180 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(index_expression, yymsp[-3].minor.yy181, yymsp[-1].minor.yy181); }
#line 1450 "cscript.c"
        break;
      case 64:
#line 184 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(function_call, String(yymsp[-3].minor.yy0), yymsp[-1].minor.yy61); }
#line 1455 "cscript.c"
        break;
      case 67:
      case 99:
      case 100:
      case 126:
#line 190 "cscript.in"
{ yygotominor.yy181 = yymsp[-1].minor.yy181; }
#line 1463 "cscript.c"
        break;
      case 69:
#line 192 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(this_expression); }
#line 1468 "cscript.c"
        break;
      case 70:
#line 195 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(literal, Variant(String(yymsp[0].minor.yy0), Variant::stInt)); }
#line 1473 "cscript.c"
        break;
      case 71:
#line 196 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(literal, Variant(hex2dec(String(yymsp[0].minor.yy0).c_str()))); }
#line 1478 "cscript.c"
        break;
      case 72:
#line 197 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(literal, Variant(bin2dec(String(yymsp[0].minor.yy0).c_str()))); }
#line 1483 "cscript.c"
        break;
      case 73:
#line 198 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(literal, Variant(rom2dec(String(yymsp[0].minor.yy0).c_str()))); }
#line 1488 "cscript.c"
        break;
      case 74:
#line 199 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(literal, Variant(String(yymsp[0].minor.yy0), Variant::stReal)); }
#line 1493 "cscript.c"
        break;
      case 75:
#line 200 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(literal, Variant(String(yymsp[0].minor.yy0), Variant::stString)); }
#line 1498 "cscript.c"
        break;
      case 76:
#line 201 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(literal, Variant(true));    }
#line 1503 "cscript.c"
        break;
      case 77:
#line 202 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(literal, Variant(false));   }
#line 1508 "cscript.c"
        break;
      case 78:
#line 203 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(literal, Variant());        }
#line 1513 "cscript.c"
        break;
      case 79:
#line 206 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(lvalue, String(yymsp[0].minor.yy0)); }
#line 1518 "cscript.c"
        break;
      case 80:
#line 209 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(list_literal, yymsp[-1].minor.yy181); }
#line 1523 "cscript.c"
        break;
      case 81:
#line 210 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(list_content, yymsp[0].minor.yy181); }
#line 1528 "cscript.c"
        break;
      case 82:
#line 211 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(list_content, yymsp[-2].minor.yy181, yymsp[0].minor.yy181); }
#line 1533 "cscript.c"
        break;
      case 83:
#line 213 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(list_entry, yymsp[0].minor.yy181); }
#line 1538 "cscript.c"
        break;
      case 84:
#line 216 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(new_expression, String(yymsp[0].minor.yy0)); }
#line 1543 "cscript.c"
        break;
      case 86:
      case 120:
      case 134:
#line 229 "cscript.in"
{ yygotominor.yy61 = new AstList; }
#line 1550 "cscript.c"
        break;
      case 87:
      case 121:
#line 230 "cscript.in"
{ yygotominor.yy61 = new AstList; yygotominor.yy61->push_back(yymsp[0].minor.yy181); }
#line 1556 "cscript.c"
        break;
      case 88:
#line 231 "cscript.in"
{ yygotominor.yy61 = yymsp[-2].minor.yy61; yymsp[-2].minor.yy61->push_back(yymsp[0].minor.yy181); }
#line 1561 "cscript.c"
        break;
      case 90:
#line 240 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(expression_statement, yymsp[-1].minor.yy181); }
#line 1566 "cscript.c"
        break;
      case 91:
#line 243 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(compound_statement); }
#line 1571 "cscript.c"
        break;
      case 92:
#line 244 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(compound_statement, yymsp[-1].minor.yy181); }
#line 1576 "cscript.c"
        break;
      case 93:
#line 247 "cscript.in"
{ p->Parse(yymsp[-1].minor.yy0); yygotominor.yy181 = p->GetRoot(); }
#line 1581 "cscript.c"
        break;
      case 94:
#line 250 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(return_statement, yymsp[-1].minor.yy181); }
#line 1586 "cscript.c"
        break;
      case 95:
#line 251 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(return_statement);    }
#line 1591 "cscript.c"
        break;
      case 96:
#line 254 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(break_statement); }
#line 1596 "cscript.c"
        break;
      case 97:
#line 255 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(continue_statement); }
#line 1601 "cscript.c"
        break;
      case 102:
#line 269 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(variable_declaration, String(yymsp[0].minor.yy0));    }
#line 1606 "cscript.c"
        break;
      case 103:
#line 270 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(variable_declaration, String(yymsp[-2].minor.yy0), yymsp[0].minor.yy181); }
#line 1611 "cscript.c"
        break;
      case 105:
#line 273 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(declaration_sequence, yymsp[-2].minor.yy181, yymsp[0].minor.yy181); }
#line 1616 "cscript.c"
        break;
      case 106:
#line 280 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(class_declaration, String(yymsp[-2].minor.yy0)); }
#line 1621 "cscript.c"
        break;
      case 107:
#line 281 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(class_declaration, String(yymsp[-3].minor.yy0), yymsp[-1].minor.yy61); }
#line 1626 "cscript.c"
        break;
      case 108:
#line 284 "cscript.in"
{ yygotominor.yy181 = yymsp[-1].minor.yy181; yymsp[-1].minor.yy181->m_props["access"] = accessDefault; }
#line 1631 "cscript.c"
        break;
      case 109:
#line 285 "cscript.in"
{ yygotominor.yy181 = yymsp[0].minor.yy181; yymsp[0].minor.yy181->m_props["access"] = accessDefault; }
#line 1636 "cscript.c"
        break;
      case 110:
#line 286 "cscript.in"
{ yygotominor.yy181 = yymsp[-1].minor.yy181; yymsp[-1].minor.yy181->m_props["access"] = yymsp[-2].minor.yy36; }
#line 1641 "cscript.c"
        break;
      case 111:
#line 287 "cscript.in"
{ yygotominor.yy181 = yymsp[0].minor.yy181; yymsp[0].minor.yy181->m_props["access"] = yymsp[-1].minor.yy36; }
#line 1646 "cscript.c"
        break;
      case 112:
#line 288 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(access_specifier, yymsp[-1].minor.yy36); }
#line 1651 "cscript.c"
        break;
      case 113:
#line 292 "cscript.in"
{ yygotominor.yy36 = accessPrivate;   }
#line 1656 "cscript.c"
        break;
      case 114:
#line 293 "cscript.in"
{ yygotominor.yy36 = accessProtected; }
#line 1661 "cscript.c"
        break;
      case 115:
#line 294 "cscript.in"
{ yygotominor.yy36 = accessPublic;    }
#line 1666 "cscript.c"
        break;
      case 116:
#line 298 "cscript.in"
{ 
  yygotominor.yy61 = new AstList;
  yygotominor.yy61->push_back(yymsp[0].minor.yy181);
}
#line 1674 "cscript.c"
        break;
      case 117:
#line 302 "cscript.in"
{ 
  yygotominor.yy61 = yymsp[-1].minor.yy61;
  yygotominor.yy61->push_back(yymsp[0].minor.yy181);
}
#line 1682 "cscript.c"
        break;
      case 118:
#line 313 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(function_declaration, String(yymsp[-4].minor.yy0), yymsp[-2].minor.yy61, yymsp[0].minor.yy181); }
#line 1687 "cscript.c"
        break;
      case 119:
#line 316 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(parameter, String(yymsp[0].minor.yy0)); }
#line 1692 "cscript.c"
        break;
      case 122:
#line 322 "cscript.in"
{ yygotominor.yy61 = yymsp[-2].minor.yy61; yygotominor.yy61->push_back(yymsp[0].minor.yy181); }
#line 1697 "cscript.c"
        break;
      case 124:
#line 350 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(for_statement, yymsp[-5].minor.yy181, yymsp[-4].minor.yy181, yymsp[-2].minor.yy181, yymsp[0].minor.yy181); }
#line 1702 "cscript.c"
        break;
      case 127:
      case 128:
#line 361 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(foreach_statement, yymsp[-4].minor.yy181, yymsp[-2].minor.yy181, yymsp[0].minor.yy181); }
#line 1708 "cscript.c"
        break;
      case 129:
#line 363 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(variable_declaration, String(yymsp[0].minor.yy0)); }
#line 1713 "cscript.c"
        break;
      case 130:
#line 374 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(if_statement, yymsp[-2].minor.yy181, yymsp[0].minor.yy181); }
#line 1718 "cscript.c"
        break;
      case 131:
#line 375 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(if_statement, yymsp[-4].minor.yy181, yymsp[-2].minor.yy181, yymsp[0].minor.yy181); }
#line 1723 "cscript.c"
        break;
      case 132:
#line 383 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(while_statement, yymsp[-2].minor.yy181,  yymsp[0].minor.yy181); }
#line 1728 "cscript.c"
        break;
      case 133:
#line 391 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(switch_statement, yymsp[-4].minor.yy181, yymsp[-1].minor.yy61); }
#line 1733 "cscript.c"
        break;
      case 135:
      case 136:
#line 396 "cscript.in"
{ yygotominor.yy61 = yymsp[-1].minor.yy61; yygotominor.yy61->push_back(yymsp[0].minor.yy181); }
#line 1739 "cscript.c"
        break;
      case 137:
#line 400 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(switch_case, yymsp[-2].minor.yy181, yymsp[0].minor.yy181); }
#line 1744 "cscript.c"
        break;
      case 138:
#line 403 "cscript.in"
{ yygotominor.yy181 = p->AllocAst(default_case, yymsp[0].minor.yy181); }
#line 1749 "cscript.c"
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
#line 1797 "cscript.c"
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
#line 1815 "cscript.c"
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

