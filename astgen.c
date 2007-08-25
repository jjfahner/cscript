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
#define YYNOCODE 125
#define YYACTIONTYPE unsigned short int
#define CScriptParseTOKENTYPE  Token 
typedef union {
  CScriptParseTOKENTYPE yy0;
  AstList* yy39;
  opcodes yy134;
  Ast* yy175;
  int yy249;
} YYMINORTYPE;
#ifndef YYSTACKDEPTH
#define YYSTACKDEPTH 100
#endif
#define CScriptParseARG_SDECL  Parser* p ;
#define CScriptParseARG_PDECL , Parser* p 
#define CScriptParseARG_FETCH  Parser* p  = yypParser->p 
#define CScriptParseARG_STORE yypParser->p  = p 
#define YYNSTATE 245
#define YYNRULE 142
#define YYERRORSYMBOL 68
#define YYERRSYMDT yy249
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
 /*     0 */   195,  388,  148,  239,    9,  235,  228,  226,  224,  218,
 /*    10 */   215,  213,  211,  209,  206,  205,  203,  199,  198,  127,
 /*    20 */   192,   16,  190,   64,  200,  185,   83,  154,  152,  150,
 /*    30 */    56,  136,  139,  151,   52,   51,   50,  145,  124,  123,
 /*    40 */    65,   91,   54,   53,   55,  140,  131,   17,  133,   32,
 /*    50 */   142,  158,  164,  169,  171,  175,  176,  177,  179,  182,
 /*    60 */   183,   61,   96,  143,    3,  244,  132,   21,  129,  128,
 /*    70 */   126,   62,  120,  117,  112,   56,  201,  202,  104,   97,
 /*    80 */    94,  114,  197,  233,  237,  107,  115,   54,   53,   55,
 /*    90 */   141,  131,   17,   20,   32,  242,  158,  164,  169,  171,
 /*   100 */   175,  176,  177,  179,  182,  183,  240,   96,  143,    3,
 /*   110 */   135,  132,   21,  129,  128,  126,   62,  120,  117,  112,
 /*   120 */   122,  157,  195,  104,   97,   94,   12,  235,  228,  226,
 /*   130 */   224,  218,  215,  213,  211,  209,  206,  205,  203,  199,
 /*   140 */   198,  127,  192,  184,  190,   64,  144,  185,   83,  154,
 /*   150 */   152,  150,  146,  136,  139,  151,   18,   56,  110,  145,
 /*   160 */   124,  123,   47,   49,   52,   51,   50,  147,  105,   54,
 /*   170 */    53,   55,    4,  131,   17,  156,   32,  207,  158,  164,
 /*   180 */   169,  171,  175,  176,  177,  179,  182,  183,  149,   96,
 /*   190 */   143,    3,  121,  132,   21,  129,  128,  126,   62,  120,
 /*   200 */   117,  112,   67,  241,  195,  104,   97,   94,   12,  235,
 /*   210 */   228,  226,  224,  218,  215,  213,  211,  209,  206,  205,
 /*   220 */   203,  199,  198,  127,  192,   29,  190,   64,  134,  185,
 /*   230 */    83,  154,  152,  150,  119,  136,  139,  151,   29,   58,
 /*   240 */    89,  145,  124,  123,   39,   36,   37,   38,   41,   42,
 /*   250 */    47,   49,   52,   51,   50,  195,   33,  165,   87,  210,
 /*   260 */   174,  228,  226,  224,  218,  215,  213,  211,  209,  206,
 /*   270 */   205,  203,  199,  198,  127,  192,  116,  190,   64,  376,
 /*   280 */   185,   83,  154,  152,  150,   57,  136,  139,  151,  166,
 /*   290 */   105,  118,  145,  124,  123,  167,  195,  137,   66,    5,
 /*   300 */   229,  174,  228,  226,  224,  218,  215,  213,  211,  209,
 /*   310 */   206,  205,  203,  199,  198,  127,  192,    1,  190,   64,
 /*   320 */    10,  185,   83,  154,  152,  150,   69,  136,  139,  151,
 /*   330 */    15,  161,    2,  145,  124,  123,  238,   62,  195,  117,
 /*   340 */    34,  173,    6,  235,  228,  226,  224,  218,  215,  213,
 /*   350 */   211,  209,  206,  205,  203,  199,  198,  127,  192,   90,
 /*   360 */   190,   64,   24,  185,   83,  154,  152,  150,   28,  136,
 /*   370 */   139,  151,    8,  195,  191,  145,  124,  123,  231,  228,
 /*   380 */   226,  224,  218,  215,  213,  211,  209,  206,  205,  203,
 /*   390 */   199,  198,  127,  192,   60,  190,   64,    7,  185,   83,
 /*   400 */   154,  152,  150,   22,  136,  139,  151,   27,  163,   14,
 /*   410 */   145,  124,  123,  195,   62,   26,  117,   11,   98,  228,
 /*   420 */   226,  224,  218,  215,  213,  211,  209,  206,  205,  203,
 /*   430 */   199,  198,  127,  192,   13,  190,   64,   19,  185,   83,
 /*   440 */   154,  152,  150,   25,  136,  139,  151,  389,  195,  222,
 /*   450 */   145,  124,  123,  181,  228,  226,  224,  218,  215,  213,
 /*   460 */   211,  209,  206,  205,  203,  199,  198,  127,  192,  389,
 /*   470 */   190,   64,  389,  185,   83,  154,  152,  150,  389,  136,
 /*   480 */   139,  151,  389,  389,  389,  145,  124,  123,  195,  389,
 /*   490 */   389,  389,  389,  193,  228,  226,  224,  218,  215,  213,
 /*   500 */   211,  209,  206,  205,  203,  199,  198,  127,  192,  389,
 /*   510 */   190,   64,  389,  185,   83,  154,  152,  150,  389,  136,
 /*   520 */   139,  151,  389,  195,  389,  145,  124,  123,  227,  228,
 /*   530 */   226,  224,  218,  215,  213,  211,  209,  206,  205,  203,
 /*   540 */   199,  198,  127,  192,  389,  190,   64,  389,  185,   83,
 /*   550 */   154,  152,  150,  389,  136,  139,  151,  389,  389,  389,
 /*   560 */   145,  124,  123,  195,  389,  389,  389,  389,  219,  228,
 /*   570 */   226,  224,  218,  215,  213,  211,  209,  206,  205,  203,
 /*   580 */   199,  198,  127,  192,  389,  190,   64,  389,  185,   83,
 /*   590 */   154,  152,  150,  389,  136,  139,  151,  389,  195,  389,
 /*   600 */   145,  124,  123,  188,  228,  226,  224,  218,  215,  213,
 /*   610 */   211,  209,  206,  205,  203,  199,  198,  127,  192,  389,
 /*   620 */   190,   64,  389,  185,   83,  154,  152,  150,  389,  136,
 /*   630 */   139,  151,  389,  389,  389,  145,  124,  123,   44,   43,
 /*   640 */    40,   35,   46,   45,   48,   39,   36,   37,   38,   41,
 /*   650 */    42,   47,   49,   52,   51,   50,  234,  230,  225,  223,
 /*   660 */   221,  217,   30,   56,   84,  389,  185,   83,  154,  152,
 /*   670 */   150,  389,  136,  139,  151,   54,   53,   55,  389,  131,
 /*   680 */    17,  389,   32,  389,  158,  164,  169,  171,  175,  176,
 /*   690 */   177,  179,  182,  183,  389,   96,  143,  153,   56,  185,
 /*   700 */    83,  154,  152,  150,   63,  136,  139,  151,  389,  389,
 /*   710 */    54,   53,   55,  389,  131,   17,  389,   32,  196,  158,
 /*   720 */   164,  169,  171,  175,  176,  177,  179,  182,  183,  389,
 /*   730 */    96,   56,   85,  389,  185,   83,  154,  152,  150,  389,
 /*   740 */   136,  139,  151,   54,   53,   55,  389,  131,   17,  389,
 /*   750 */    32,  389,  158,  164,  169,  171,  175,  176,  177,  179,
 /*   760 */   182,  183,  389,   96,  138,  159,   56,  185,   83,  154,
 /*   770 */   152,  150,  389,  136,  139,  151,  389,  389,   54,   53,
 /*   780 */    55,  389,  131,   17,  389,   32,  389,  158,  164,  169,
 /*   790 */   171,  175,  176,  177,  179,  182,  183,  389,   96,   43,
 /*   800 */    40,   35,   46,   45,   48,   39,   36,   37,   38,   41,
 /*   810 */    42,   47,   49,   52,   51,   50,   40,   35,   46,   45,
 /*   820 */    48,   39,   36,   37,   38,   41,   42,   47,   49,   52,
 /*   830 */    51,   50,  220,   74,  389,  185,   83,  154,  152,  150,
 /*   840 */   389,  136,  139,  151,  127,  192,  389,  190,   64,  389,
 /*   850 */   185,   83,  154,  152,  150,  389,  136,  108,  151,  236,
 /*   860 */   237,  389,  109,  168,  101,  185,   83,  154,  152,  150,
 /*   870 */   389,  136,  139,  151,  389,   23,  102,  389,  389,  389,
 /*   880 */    35,   46,   45,   48,   39,   36,   37,   38,   41,   42,
 /*   890 */    47,   49,   52,   51,   50,  204,  192,  389,  190,   64,
 /*   900 */   389,  185,   83,  154,  152,  150,   86,  136,  139,  151,
 /*   910 */   389,  389,  214,   46,   45,   48,   39,   36,   37,   38,
 /*   920 */    41,   42,   47,   49,   52,   51,   50,  389,  389,  389,
 /*   930 */   389,  389,  189,  192,  389,  190,   64,  389,  185,   83,
 /*   940 */   154,  152,  150,  389,  136,  139,  151,   88,  194,   45,
 /*   950 */    48,   39,   36,   37,   38,   41,   42,   47,   49,   52,
 /*   960 */    51,   50,  389,  389,  204,  192,  389,  190,   64,  389,
 /*   970 */   185,   83,  154,  152,  150,  389,  136,  139,  151,  189,
 /*   980 */   192,  212,  190,   64,  389,  185,   83,  154,  152,  150,
 /*   990 */   389,  136,  139,  151,  389,  186,  216,  192,   95,  190,
 /*  1000 */    64,  389,  185,   83,  154,  152,  150,  389,  136,  139,
 /*  1010 */   151,   92,  192,  389,  190,   64,  389,  185,   83,  154,
 /*  1020 */   152,  150,  389,  136,  139,  151,  111,  192,  389,  190,
 /*  1030 */    64,  389,  185,   83,  154,  152,  150,  389,  136,  139,
 /*  1040 */   151,  155,  192,  389,  190,   64,  389,  185,   83,  154,
 /*  1050 */   152,  150,  389,  136,  139,  151,  125,  192,  389,  190,
 /*  1060 */    64,  389,  185,   83,  154,  152,  150,  389,  136,  139,
 /*  1070 */   151,  113,  192,  389,  190,   64,  389,  185,   83,  154,
 /*  1080 */   152,  150,  389,  136,  139,  151,   93,  192,  389,  190,
 /*  1090 */    64,  389,  185,   83,  154,  152,  150,  389,  136,  139,
 /*  1100 */   151,  389,  232,  192,  389,  190,   64,  389,  185,   83,
 /*  1110 */   154,  152,  150,  389,  136,  139,  151,  100,  192,  389,
 /*  1120 */   190,   64,  389,  185,   83,  154,  152,  150,  389,  136,
 /*  1130 */   139,  151,   99,  192,  389,  190,   64,  389,  185,   83,
 /*  1140 */   154,  152,  150,  389,  136,  139,  151,  106,  192,  389,
 /*  1150 */   190,   64,  389,  185,   83,  154,  152,  150,  389,  136,
 /*  1160 */   139,  151,  103,  192,  389,  190,   64,  389,  185,   83,
 /*  1170 */   154,  152,  150,  389,  136,  139,  151,  130,  192,  389,
 /*  1180 */   190,   64,  389,  185,   83,  154,  152,  150,  389,  136,
 /*  1190 */   139,  151,  187,  389,  190,   64,  389,  185,   83,  154,
 /*  1200 */   152,  150,  389,  136,  139,  151,  208,  389,  190,   64,
 /*  1210 */   389,  185,   83,  154,  152,  150,  389,  136,  139,  151,
 /*  1220 */   389,   75,  389,  185,   83,  154,  152,  150,  389,  136,
 /*  1230 */   139,  151,   70,  389,  185,   83,  154,  152,  150,  389,
 /*  1240 */   136,  139,  151,   71,  389,  185,   83,  154,  152,  150,
 /*  1250 */   389,  136,  139,  151,   77,  389,  185,   83,  154,  152,
 /*  1260 */   150,  389,  136,  139,  151,  389,   82,  389,  185,   83,
 /*  1270 */   154,  152,  150,  389,  136,  139,  151,   80,  389,  185,
 /*  1280 */    83,  154,  152,  150,  389,  136,  139,  151,  389,   81,
 /*  1290 */   389,  185,   83,  154,  152,  150,  389,  136,  139,  151,
 /*  1300 */    72,  389,  185,   83,  154,  152,  150,  389,  136,  139,
 /*  1310 */   151,   73,  389,  185,   83,  154,  152,  150,  389,  136,
 /*  1320 */   139,  151,   78,  389,  185,   83,  154,  152,  150,  389,
 /*  1330 */   136,  139,  151,   79,  389,  185,   83,  154,  152,  150,
 /*  1340 */   389,  136,  139,  151,   37,   38,   41,   42,   47,   49,
 /*  1350 */    52,   51,   50,  164,  169,  171,  175,  176,  177,  179,
 /*  1360 */   182,  183,   76,  389,  185,   83,  154,  152,  150,  389,
 /*  1370 */   136,  139,  151,  162,   83,  154,  152,  150,  389,  136,
 /*  1380 */   139,  151,  389,  160,   83,  154,  152,  150,  389,  136,
 /*  1390 */   139,  151,  172,   83,  154,  152,  150,  389,  136,  139,
 /*  1400 */   151,  389,  170,   83,  154,  152,  150,  389,  136,  139,
 /*  1410 */   151,  180,  178,  389,   68,  166,   31,  118,  389,  389,
 /*  1420 */    59,  243,
};
static const YYCODETYPE yy_lookahead[] = {
 /*     0 */    68,   69,   70,   71,   72,   73,   74,   75,   76,   77,
 /*    10 */    78,   79,   80,   81,   82,   83,   84,   85,   86,   87,
 /*    20 */    88,   34,   90,   91,   50,   93,   94,   95,   96,   97,
 /*    30 */    15,   99,  100,  101,   16,   17,   18,  105,  106,  107,
 /*    40 */    66,   67,   27,   28,   29,   48,   31,   32,   42,   34,
 /*    50 */    48,   36,   37,   38,   39,   40,   41,   42,   43,   44,
 /*    60 */    45,  120,   47,   48,   49,   50,   51,   52,   53,   54,
 /*    70 */    55,   56,   57,   58,   59,   15,  121,  122,   63,   64,
 /*    80 */    65,  112,   35,  114,  115,  116,  117,   27,   28,   29,
 /*    90 */    48,   31,   32,   46,   34,   97,   36,   37,   38,   39,
 /*   100 */    40,   41,   42,   43,   44,   45,   48,   47,   48,   49,
 /*   110 */    50,   51,   52,   53,   54,   55,   56,   57,   58,   59,
 /*   120 */   108,  109,   68,   63,   64,   65,   72,   73,   74,   75,
 /*   130 */    76,   77,   78,   79,   80,   81,   82,   83,   84,   85,
 /*   140 */    86,   87,   88,   33,   90,   91,   48,   93,   94,   95,
 /*   150 */    96,   97,   35,   99,  100,  101,   46,   15,   31,  105,
 /*   160 */   106,  107,   14,   15,   16,   17,   18,   48,   31,   27,
 /*   170 */    28,   29,   35,   31,   32,  109,   34,  123,   36,   37,
 /*   180 */    38,   39,   40,   41,   42,   43,   44,   45,   48,   47,
 /*   190 */    48,   49,   31,   51,   52,   53,   54,   55,   56,   57,
 /*   200 */    58,   59,   46,  115,   68,   63,   64,   65,   72,   73,
 /*   210 */    74,   75,   76,   77,   78,   79,   80,   81,   82,   83,
 /*   220 */    84,   85,   86,   87,   88,   19,   90,   91,   31,   93,
 /*   230 */    94,   95,   96,   97,   31,   99,  100,  101,   19,   49,
 /*   240 */    99,  105,  106,  107,    8,    9,   10,   11,   12,   13,
 /*   250 */    14,   15,   16,   17,   18,   68,   92,   48,   31,  123,
 /*   260 */    73,   74,   75,   76,   77,   78,   79,   80,   81,   82,
 /*   270 */    83,   84,   85,   86,   87,   88,   31,   90,   91,   60,
 /*   280 */    93,   94,   95,   96,   97,   34,   99,  100,  101,  105,
 /*   290 */    31,  107,  105,  106,  107,  111,   68,   48,   46,   35,
 /*   300 */   113,   73,   74,   75,   76,   77,   78,   79,   80,   81,
 /*   310 */    82,   83,   84,   85,   86,   87,   88,   26,   90,   91,
 /*   320 */    35,   93,   94,   95,   96,   97,   49,   99,  100,  101,
 /*   330 */    34,   50,   26,  105,  106,  107,   33,   56,   68,   58,
 /*   340 */    26,  113,   72,   73,   74,   75,   76,   77,   78,   79,
 /*   350 */    80,   81,   82,   83,   84,   85,   86,   87,   88,   35,
 /*   360 */    90,   91,   34,   93,   94,   95,   96,   97,   60,   99,
 /*   370 */   100,  101,   35,   68,   31,  105,  106,  107,   73,   74,
 /*   380 */    75,   76,   77,   78,   79,   80,   81,   82,   83,   84,
 /*   390 */    85,   86,   87,   88,   46,   90,   91,   35,   93,   94,
 /*   400 */    95,   96,   97,   34,   99,  100,  101,   19,   50,   62,
 /*   410 */   105,  106,  107,   68,   56,   34,   58,   35,   73,   74,
 /*   420 */    75,   76,   77,   78,   79,   80,   81,   82,   83,   84,
 /*   430 */    85,   86,   87,   88,   35,   90,   91,   48,   93,   94,
 /*   440 */    95,   96,   97,   60,   99,  100,  101,  124,   68,   48,
 /*   450 */   105,  106,  107,   73,   74,   75,   76,   77,   78,   79,
 /*   460 */    80,   81,   82,   83,   84,   85,   86,   87,   88,  124,
 /*   470 */    90,   91,  124,   93,   94,   95,   96,   97,  124,   99,
 /*   480 */   100,  101,  124,  124,  124,  105,  106,  107,   68,  124,
 /*   490 */   124,  124,  124,   73,   74,   75,   76,   77,   78,   79,
 /*   500 */    80,   81,   82,   83,   84,   85,   86,   87,   88,  124,
 /*   510 */    90,   91,  124,   93,   94,   95,   96,   97,  124,   99,
 /*   520 */   100,  101,  124,   68,  124,  105,  106,  107,   73,   74,
 /*   530 */    75,   76,   77,   78,   79,   80,   81,   82,   83,   84,
 /*   540 */    85,   86,   87,   88,  124,   90,   91,  124,   93,   94,
 /*   550 */    95,   96,   97,  124,   99,  100,  101,  124,  124,  124,
 /*   560 */   105,  106,  107,   68,  124,  124,  124,  124,   73,   74,
 /*   570 */    75,   76,   77,   78,   79,   80,   81,   82,   83,   84,
 /*   580 */    85,   86,   87,   88,  124,   90,   91,  124,   93,   94,
 /*   590 */    95,   96,   97,  124,   99,  100,  101,  124,   68,  124,
 /*   600 */   105,  106,  107,   73,   74,   75,   76,   77,   78,   79,
 /*   610 */    80,   81,   82,   83,   84,   85,   86,   87,   88,  124,
 /*   620 */    90,   91,  124,   93,   94,   95,   96,   97,  124,   99,
 /*   630 */   100,  101,  124,  124,  124,  105,  106,  107,    1,    2,
 /*   640 */     3,    4,    5,    6,    7,    8,    9,   10,   11,   12,
 /*   650 */    13,   14,   15,   16,   17,   18,   19,   20,   21,   22,
 /*   660 */    23,   24,   25,   15,   91,  124,   93,   94,   95,   96,
 /*   670 */    97,  124,   99,  100,  101,   27,   28,   29,  124,   31,
 /*   680 */    32,  124,   34,  124,   36,   37,   38,   39,   40,   41,
 /*   690 */    42,   43,   44,   45,  124,   47,   48,   91,   15,   93,
 /*   700 */    94,   95,   96,   97,   56,   99,  100,  101,  124,  124,
 /*   710 */    27,   28,   29,  124,   31,   32,  124,   34,   35,   36,
 /*   720 */    37,   38,   39,   40,   41,   42,   43,   44,   45,  124,
 /*   730 */    47,   15,   91,  124,   93,   94,   95,   96,   97,  124,
 /*   740 */    99,  100,  101,   27,   28,   29,  124,   31,   32,  124,
 /*   750 */    34,  124,   36,   37,   38,   39,   40,   41,   42,   43,
 /*   760 */    44,   45,  124,   47,   48,   91,   15,   93,   94,   95,
 /*   770 */    96,   97,  124,   99,  100,  101,  124,  124,   27,   28,
 /*   780 */    29,  124,   31,   32,  124,   34,  124,   36,   37,   38,
 /*   790 */    39,   40,   41,   42,   43,   44,   45,  124,   47,    2,
 /*   800 */     3,    4,    5,    6,    7,    8,    9,   10,   11,   12,
 /*   810 */    13,   14,   15,   16,   17,   18,    3,    4,    5,    6,
 /*   820 */     7,    8,    9,   10,   11,   12,   13,   14,   15,   16,
 /*   830 */    17,   18,   75,   91,  124,   93,   94,   95,   96,   97,
 /*   840 */   124,   99,  100,  101,   87,   88,  124,   90,   91,  124,
 /*   850 */    93,   94,   95,   96,   97,  124,   99,  100,  101,  114,
 /*   860 */   115,  124,  117,   91,  107,   93,   94,   95,   96,   97,
 /*   870 */   124,   99,  100,  101,  124,  118,  119,  124,  124,  124,
 /*   880 */     4,    5,    6,    7,    8,    9,   10,   11,   12,   13,
 /*   890 */    14,   15,   16,   17,   18,   87,   88,  124,   90,   91,
 /*   900 */   124,   93,   94,   95,   96,   97,   98,   99,  100,  101,
 /*   910 */   124,  124,  104,    5,    6,    7,    8,    9,   10,   11,
 /*   920 */    12,   13,   14,   15,   16,   17,   18,  124,  124,  124,
 /*   930 */   124,  124,   87,   88,  124,   90,   91,  124,   93,   94,
 /*   940 */    95,   96,   97,  124,   99,  100,  101,  102,  103,    6,
 /*   950 */     7,    8,    9,   10,   11,   12,   13,   14,   15,   16,
 /*   960 */    17,   18,  124,  124,   87,   88,  124,   90,   91,  124,
 /*   970 */    93,   94,   95,   96,   97,  124,   99,  100,  101,   87,
 /*   980 */    88,  104,   90,   91,  124,   93,   94,   95,   96,   97,
 /*   990 */   124,   99,  100,  101,  124,  103,   87,   88,   89,   90,
 /*  1000 */    91,  124,   93,   94,   95,   96,   97,  124,   99,  100,
 /*  1010 */   101,   87,   88,  124,   90,   91,  124,   93,   94,   95,
 /*  1020 */    96,   97,  124,   99,  100,  101,   87,   88,  124,   90,
 /*  1030 */    91,  124,   93,   94,   95,   96,   97,  124,   99,  100,
 /*  1040 */   101,   87,   88,  124,   90,   91,  124,   93,   94,   95,
 /*  1050 */    96,   97,  124,   99,  100,  101,   87,   88,  124,   90,
 /*  1060 */    91,  124,   93,   94,   95,   96,   97,  124,   99,  100,
 /*  1070 */   101,   87,   88,  124,   90,   91,  124,   93,   94,   95,
 /*  1080 */    96,   97,  124,   99,  100,  101,   87,   88,  124,   90,
 /*  1090 */    91,  124,   93,   94,   95,   96,   97,  124,   99,  100,
 /*  1100 */   101,  124,   87,   88,  124,   90,   91,  124,   93,   94,
 /*  1110 */    95,   96,   97,  124,   99,  100,  101,   87,   88,  124,
 /*  1120 */    90,   91,  124,   93,   94,   95,   96,   97,  124,   99,
 /*  1130 */   100,  101,   87,   88,  124,   90,   91,  124,   93,   94,
 /*  1140 */    95,   96,   97,  124,   99,  100,  101,   87,   88,  124,
 /*  1150 */    90,   91,  124,   93,   94,   95,   96,   97,  124,   99,
 /*  1160 */   100,  101,   87,   88,  124,   90,   91,  124,   93,   94,
 /*  1170 */    95,   96,   97,  124,   99,  100,  101,   87,   88,  124,
 /*  1180 */    90,   91,  124,   93,   94,   95,   96,   97,  124,   99,
 /*  1190 */   100,  101,   88,  124,   90,   91,  124,   93,   94,   95,
 /*  1200 */    96,   97,  124,   99,  100,  101,   88,  124,   90,   91,
 /*  1210 */   124,   93,   94,   95,   96,   97,  124,   99,  100,  101,
 /*  1220 */   124,   91,  124,   93,   94,   95,   96,   97,  124,   99,
 /*  1230 */   100,  101,   91,  124,   93,   94,   95,   96,   97,  124,
 /*  1240 */    99,  100,  101,   91,  124,   93,   94,   95,   96,   97,
 /*  1250 */   124,   99,  100,  101,   91,  124,   93,   94,   95,   96,
 /*  1260 */    97,  124,   99,  100,  101,  124,   91,  124,   93,   94,
 /*  1270 */    95,   96,   97,  124,   99,  100,  101,   91,  124,   93,
 /*  1280 */    94,   95,   96,   97,  124,   99,  100,  101,  124,   91,
 /*  1290 */   124,   93,   94,   95,   96,   97,  124,   99,  100,  101,
 /*  1300 */    91,  124,   93,   94,   95,   96,   97,  124,   99,  100,
 /*  1310 */   101,   91,  124,   93,   94,   95,   96,   97,  124,   99,
 /*  1320 */   100,  101,   91,  124,   93,   94,   95,   96,   97,  124,
 /*  1330 */    99,  100,  101,   91,  124,   93,   94,   95,   96,   97,
 /*  1340 */   124,   99,  100,  101,   10,   11,   12,   13,   14,   15,
 /*  1350 */    16,   17,   18,   37,   38,   39,   40,   41,   42,   43,
 /*  1360 */    44,   45,   91,  124,   93,   94,   95,   96,   97,  124,
 /*  1370 */    99,  100,  101,   93,   94,   95,   96,   97,  124,   99,
 /*  1380 */   100,  101,  124,   93,   94,   95,   96,   97,  124,   99,
 /*  1390 */   100,  101,   93,   94,   95,   96,   97,  124,   99,  100,
 /*  1400 */   101,  124,   93,   94,   95,   96,   97,  124,   99,  100,
 /*  1410 */   101,   27,   28,  124,   30,  105,   32,  107,  124,  124,
 /*  1420 */   110,  111,
};
#define YY_SHIFT_USE_DFLT (-27)
#define YY_SHIFT_MAX 134
static const short yy_shift_ofst[] = {
 /*     0 */   142,  142,  142,   15,  142,  142,   60,  142,  142,  142,
 /*    10 */   142,  142,  142,  142,  142,  648,  683,  751,  751,  751,
 /*    20 */   751,  716,  751,  751,  751,  751,  751,  751,  751,  751,
 /*    30 */   751,  751,  751,  751,  751,  751,  751,  751,  751,  751,
 /*    40 */   751,  751,  751,  751,  751,  751,  751,  751,  751,  751,
 /*    50 */   751,  751,  751,  751,  751,  751,  751,  137,  281,  358,
 /*    60 */   259,  -26,  161,  227,  637, 1316,  197,  161,  127,  -27,
 /*    70 */   797,  813,  876,  908,  943,  236,  236, 1334, 1334,  148,
 /*    80 */   148,  148,  148, 1384,   18,   18,   47,  219,  110,  291,
 /*    90 */   277,  306,  314,  324,  328,  337,  343,  369,  347,  382,
 /*   100 */   389,  401,  383,  399,  381,  388,  362,  348,  308,  252,
 /*   110 */   -13,  303,  296,  285,  264,  252,  251,  245,  209,  190,
 /*   120 */   203,  206,  156,  140,  119,  117,   98,   42,    2,   -3,
 /*   130 */   249,  -13,    6,   58,  388,
};
#define YY_REDUCE_USE_DFLT (-69)
#define YY_REDUCE_MAX 69
static const short yy_reduce_ofst[] = {
 /*     0 */   -68,   54,  136,  270,  187,  228,  305,  380,  495,  305,
 /*    10 */   420,  345,  305,  455,  530,  757,  808,  845,  892,  909,
 /*    20 */   877, 1090,  984, 1030,  999, 1075, 1045, 1015, 1060,  954,
 /*    30 */   924,  939,  969, 1104, 1118, 1220, 1163, 1186, 1242, 1231,
 /*    40 */  1209, 1198, 1175, 1152, 1141, 1130,  742,  641, 1271,  573,
 /*    50 */   606,  674,  772, 1280, 1309, 1290, 1299,  -31, 1310,  184,
 /*    60 */   745,  -45,   12,   12,  164,  141,   88,   66,   -2,  -59,
};
static const YYACTIONTYPE yy_default[] = {
 /*     0 */   249,  387,  387,  387,  387,  387,  387,  387,  387,  250,
 /*    10 */   387,  387,  386,  387,  387,  387,  387,  387,  387,  266,
 /*    20 */   387,  387,  387,  387,  387,  387,  387,  387,  387,  387,
 /*    30 */   387,  387,  387,  387,  387,  387,  387,  387,  387,  387,
 /*    40 */   387,  387,  387,  387,  387,  387,  387,  387,  387,  387,
 /*    50 */   387,  387,  387,  387,  387,  387,  387,  387,  387,  387,
 /*    60 */   387,  387,  387,  387,  276,  387,  387,  387,  387,  381,
 /*    70 */   279,  280,  281,  282,  283,  295,  296,  285,  284,  287,
 /*    80 */   286,  288,  289,  297,  291,  290,  387,  349,  387,  387,
 /*    90 */   387,  387,  387,  387,  387,  387,  387,  387,  377,  387,
 /*   100 */   387,  387,  387,  387,  387,  361,  387,  367,  313,  369,
 /*   110 */   307,  387,  387,  387,  387,  368,  387,  387,  387,  387,
 /*   120 */   387,  349,  348,  387,  387,  387,  387,  387,  387,  387,
 /*   130 */   387,  326,  387,  387,  387,  338,  312,  340,  341,  313,
 /*   140 */   342,  336,  343,  335,  344,  345,  314,  346,  245,  347,
 /*   150 */   306,  315,  303,  294,  302,  350,  352,  351,  316,  293,
 /*   160 */   301,  353,  300,  354,  317,  355,  356,  358,  292,  318,
 /*   170 */   299,  319,  298,  359,  370,  320,  321,  322,  305,  323,
 /*   180 */   304,  374,  324,  325,  327,  278,  329,  269,  378,  330,
 /*   190 */   268,  331,  265,  379,  328,  264,  310,  311,  263,  262,
 /*   200 */   380,  382,  383,  261,  332,  260,  259,  384,  277,  258,
 /*   210 */   385,  257,  334,  256,  333,  255,  267,  275,  254,  371,
 /*   220 */   372,  274,  373,  273,  253,  272,  252,  375,  251,  360,
 /*   230 */   271,  248,  362,  363,  270,  247,  364,  365,  309,  246,
 /*   240 */   339,  366,  308,  357,  337,
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
  "VAR",           "CLASS",         "FUNCTION",      "FOR",         
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
  "argument",      "function_declaration",  "class_declaration",  "variable_declaration",
  "declarator_sequence",  "declarator",    "class_members",  "class_member",
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
 /* 101 */ "declaration_statement ::= class_declaration SEMICOLON",
 /* 102 */ "declaration_statement ::= variable_declaration SEMICOLON",
 /* 103 */ "variable_declaration ::= VAR declarator_sequence",
 /* 104 */ "declarator ::= IDENTIFIER",
 /* 105 */ "declarator ::= IDENTIFIER ASSIGN expression",
 /* 106 */ "declarator_sequence ::= declarator",
 /* 107 */ "declarator_sequence ::= declarator_sequence COMMA declarator",
 /* 108 */ "class_declaration ::= CLASS IDENTIFIER LBRACE RBRACE",
 /* 109 */ "class_declaration ::= CLASS IDENTIFIER LBRACE class_members RBRACE",
 /* 110 */ "class_member ::= variable_declaration SEMICOLON",
 /* 111 */ "class_member ::= function_declaration",
 /* 112 */ "class_members ::= class_member",
 /* 113 */ "class_members ::= class_members class_member",
 /* 114 */ "function_declaration ::= FUNCTION IDENTIFIER LPAREN parameter_list RPAREN function_body",
 /* 115 */ "function_declaration ::= FUNCTION IDENTIFIER LPAREN RPAREN function_body",
 /* 116 */ "parameter ::= IDENTIFIER",
 /* 117 */ "opt_parameter ::= IDENTIFIER ASSIGN expression",
 /* 118 */ "parameters ::= parameter",
 /* 119 */ "parameters ::= parameters COMMA parameter",
 /* 120 */ "opt_parameters ::= opt_parameter",
 /* 121 */ "opt_parameters ::= opt_parameters COMMA opt_parameter",
 /* 122 */ "parameter_list ::= parameters",
 /* 123 */ "parameter_list ::= opt_parameters",
 /* 124 */ "parameter_list ::= parameters COMMA opt_parameters",
 /* 125 */ "function_body ::= statement",
 /* 126 */ "for_statement ::= FOR LPAREN for_init_statement expression SEMICOLON expression_opt RPAREN statement",
 /* 127 */ "for_init_statement ::= expression_statement",
 /* 128 */ "for_init_statement ::= variable_declaration SEMICOLON",
 /* 129 */ "foreach_statement ::= FOR LPAREN id_expression IN expression RPAREN statement",
 /* 130 */ "foreach_statement ::= FOR LPAREN foreach_decl IN expression RPAREN statement",
 /* 131 */ "foreach_decl ::= VAR IDENTIFIER",
 /* 132 */ "if_statement ::= IF LPAREN expression RPAREN statement",
 /* 133 */ "if_statement ::= IF LPAREN expression RPAREN statement ELSE statement",
 /* 134 */ "while_statement ::= WHILE LPAREN expression RPAREN statement",
 /* 135 */ "switch_statement ::= SWITCH LPAREN expression RPAREN LBRACE switch_body RBRACE",
 /* 136 */ "switch_body ::=",
 /* 137 */ "switch_body ::= switch_body switch_case",
 /* 138 */ "switch_body ::= switch_body default_case",
 /* 139 */ "switch_case ::= CASE literal COLON case_statements",
 /* 140 */ "default_case ::= DEFAULT COLON case_statements",
 /* 141 */ "case_statements ::= statement_sequence",
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
  { 107, 2 },
  { 109, 1 },
  { 109, 3 },
  { 108, 1 },
  { 108, 3 },
  { 106, 4 },
  { 106, 5 },
  { 111, 2 },
  { 111, 1 },
  { 110, 1 },
  { 110, 2 },
  { 105, 6 },
  { 105, 5 },
  { 114, 1 },
  { 115, 3 },
  { 116, 1 },
  { 116, 3 },
  { 117, 1 },
  { 117, 3 },
  { 112, 1 },
  { 112, 1 },
  { 112, 3 },
  { 113, 1 },
  { 77, 8 },
  { 118, 1 },
  { 118, 2 },
  { 81, 7 },
  { 81, 7 },
  { 119, 2 },
  { 79, 5 },
  { 79, 7 },
  { 80, 5 },
  { 83, 7 },
  { 120, 0 },
  { 120, 2 },
  { 120, 2 },
  { 121, 4 },
  { 122, 3 },
  { 123, 1 },
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
{ p->SetRoot(yymsp[0].minor.yy175); }
#line 1232 "astgen.c"
        break;
      case 1:
#line 78 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(translation_unit, yymsp[0].minor.yy175); }
#line 1237 "astgen.c"
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
      case 103:
      case 106:
      case 111:
      case 118:
      case 120:
      case 122:
      case 123:
      case 125:
      case 127:
      case 141:
#line 81 "astgen.in"
{ yygotominor.yy175 = yymsp[0].minor.yy175; }
#line 1280 "astgen.c"
        break;
      case 3:
#line 82 "astgen.in"
{ 
  if(yymsp[-1].minor.yy175->m_type == statement_sequence) {
    yygotominor.yy175 = yymsp[-1].minor.yy175;
  }
  else {
    yygotominor.yy175 = p->AllocAst(statement_sequence, new AstList);
    yygotominor.yy175->m_a1.GetList()->push_back(yymsp[-1].minor.yy175);
  }
  yygotominor.yy175->m_a1.GetList()->push_back(yymsp[0].minor.yy175);
}
#line 1294 "astgen.c"
        break;
      case 4:
      case 21:
#line 95 "astgen.in"
{ yygotominor.yy175 = 0; }
#line 1300 "astgen.c"
        break;
      case 19:
      case 90:
#line 114 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(empty_statement); }
#line 1306 "astgen.c"
        break;
      case 24:
#line 130 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(assignment_expression, yymsp[-1].minor.yy134, yymsp[-2].minor.yy175, yymsp[0].minor.yy175); }
#line 1311 "astgen.c"
        break;
      case 25:
#line 134 "astgen.in"
{ yygotominor.yy134 = op_assign; }
#line 1316 "astgen.c"
        break;
      case 26:
#line 135 "astgen.in"
{ yygotominor.yy134 = op_assadd; }
#line 1321 "astgen.c"
        break;
      case 27:
#line 136 "astgen.in"
{ yygotominor.yy134 = op_asssub; }
#line 1326 "astgen.c"
        break;
      case 28:
#line 137 "astgen.in"
{ yygotominor.yy134 = op_assmul; }
#line 1331 "astgen.c"
        break;
      case 29:
#line 138 "astgen.in"
{ yygotominor.yy134 = op_assdiv; }
#line 1336 "astgen.c"
        break;
      case 30:
#line 139 "astgen.in"
{ yygotominor.yy134 = op_assmod; }
#line 1341 "astgen.c"
        break;
      case 32:
#line 143 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(ternary_expression, yymsp[-4].minor.yy175, yymsp[-2].minor.yy175, yymsp[0].minor.yy175); }
#line 1346 "astgen.c"
        break;
      case 34:
#line 147 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(binary_expression, op_logor,   yymsp[-2].minor.yy175, yymsp[0].minor.yy175); }
#line 1351 "astgen.c"
        break;
      case 35:
#line 148 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(binary_expression, op_logand,  yymsp[-2].minor.yy175, yymsp[0].minor.yy175); }
#line 1356 "astgen.c"
        break;
      case 36:
#line 149 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(binary_expression, op_bitor,   yymsp[-2].minor.yy175, yymsp[0].minor.yy175); }
#line 1361 "astgen.c"
        break;
      case 37:
#line 150 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(binary_expression, op_bitxor,  yymsp[-2].minor.yy175, yymsp[0].minor.yy175); }
#line 1366 "astgen.c"
        break;
      case 38:
#line 151 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(binary_expression, op_bitand,  yymsp[-2].minor.yy175, yymsp[0].minor.yy175); }
#line 1371 "astgen.c"
        break;
      case 39:
#line 152 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(binary_expression, op_eq,   yymsp[-2].minor.yy175, yymsp[0].minor.yy175); }
#line 1376 "astgen.c"
        break;
      case 40:
#line 153 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(binary_expression, op_ne,   yymsp[-2].minor.yy175, yymsp[0].minor.yy175); }
#line 1381 "astgen.c"
        break;
      case 41:
#line 154 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(binary_expression, op_lt,   yymsp[-2].minor.yy175, yymsp[0].minor.yy175); }
#line 1386 "astgen.c"
        break;
      case 42:
#line 155 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(binary_expression, op_le,   yymsp[-2].minor.yy175, yymsp[0].minor.yy175); }
#line 1391 "astgen.c"
        break;
      case 43:
#line 156 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(binary_expression, op_gt,   yymsp[-2].minor.yy175, yymsp[0].minor.yy175); }
#line 1396 "astgen.c"
        break;
      case 44:
#line 157 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(binary_expression, op_ge,   yymsp[-2].minor.yy175, yymsp[0].minor.yy175); }
#line 1401 "astgen.c"
        break;
      case 45:
#line 158 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(binary_expression, op_add,  yymsp[-2].minor.yy175, yymsp[0].minor.yy175); }
#line 1406 "astgen.c"
        break;
      case 46:
#line 159 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(binary_expression, op_sub,  yymsp[-2].minor.yy175, yymsp[0].minor.yy175); }
#line 1411 "astgen.c"
        break;
      case 47:
#line 160 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(binary_expression, op_mul,  yymsp[-2].minor.yy175, yymsp[0].minor.yy175); }
#line 1416 "astgen.c"
        break;
      case 48:
#line 161 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(binary_expression, op_div,  yymsp[-2].minor.yy175, yymsp[0].minor.yy175); }
#line 1421 "astgen.c"
        break;
      case 49:
#line 162 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(binary_expression, op_mod,  yymsp[-2].minor.yy175, yymsp[0].minor.yy175); }
#line 1426 "astgen.c"
        break;
      case 50:
#line 163 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(binary_expression, op_seq,  yymsp[-2].minor.yy175, yymsp[0].minor.yy175); }
#line 1431 "astgen.c"
        break;
      case 51:
#line 164 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(binary_expression, op_sne,  yymsp[-2].minor.yy175, yymsp[0].minor.yy175); }
#line 1436 "astgen.c"
        break;
      case 53:
#line 168 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(prefix_expression, op_negate, yymsp[0].minor.yy175); }
#line 1441 "astgen.c"
        break;
      case 54:
#line 169 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(prefix_expression, op_preinc, yymsp[0].minor.yy175); }
#line 1446 "astgen.c"
        break;
      case 55:
#line 170 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(prefix_expression, op_predec, yymsp[0].minor.yy175); }
#line 1451 "astgen.c"
        break;
      case 56:
#line 171 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(prefix_expression, op_not,    yymsp[0].minor.yy175); }
#line 1456 "astgen.c"
        break;
      case 59:
#line 176 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(postfix_expression, op_postinc, yymsp[-1].minor.yy175); }
#line 1461 "astgen.c"
        break;
      case 60:
#line 177 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(postfix_expression, op_postdec, yymsp[-1].minor.yy175); }
#line 1466 "astgen.c"
        break;
      case 62:
#line 179 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(member_expression, yymsp[-2].minor.yy175, String(yymsp[0].minor.yy0)); }
#line 1471 "astgen.c"
        break;
      case 63:
#line 180 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(member_call, yymsp[-2].minor.yy175, yymsp[0].minor.yy175); }
#line 1476 "astgen.c"
        break;
      case 64:
#line 181 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(index_expression, yymsp[-3].minor.yy175, yymsp[-1].minor.yy175); }
#line 1481 "astgen.c"
        break;
      case 65:
#line 184 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(function_call, String(yymsp[-2].minor.yy0)); }
#line 1486 "astgen.c"
        break;
      case 66:
#line 185 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(function_call, String(yymsp[-3].minor.yy0), yymsp[-1].minor.yy175); }
#line 1491 "astgen.c"
        break;
      case 69:
      case 101:
      case 102:
      case 110:
      case 128:
#line 191 "astgen.in"
{ yygotominor.yy175 = yymsp[-1].minor.yy175; }
#line 1500 "astgen.c"
        break;
      case 71:
#line 193 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(this_expression); }
#line 1505 "astgen.c"
        break;
      case 72:
#line 196 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(literal, Variant(String(yymsp[0].minor.yy0), Variant::stInt)); }
#line 1510 "astgen.c"
        break;
      case 73:
#line 197 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(literal, Variant(hex2dec(String(yymsp[0].minor.yy0).c_str()))); }
#line 1515 "astgen.c"
        break;
      case 74:
#line 198 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(literal, Variant(bin2dec(String(yymsp[0].minor.yy0).c_str()))); }
#line 1520 "astgen.c"
        break;
      case 75:
#line 199 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(literal, Variant(rom2dec(String(yymsp[0].minor.yy0).c_str()))); }
#line 1525 "astgen.c"
        break;
      case 76:
#line 200 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(literal, Variant(String(yymsp[0].minor.yy0), Variant::stReal)); }
#line 1530 "astgen.c"
        break;
      case 77:
#line 201 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(literal, Variant(String(yymsp[0].minor.yy0), Variant::stString)); }
#line 1535 "astgen.c"
        break;
      case 78:
#line 202 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(literal, Variant(true));    }
#line 1540 "astgen.c"
        break;
      case 79:
#line 203 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(literal, Variant(false));   }
#line 1545 "astgen.c"
        break;
      case 80:
#line 204 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(literal, Variant());        }
#line 1550 "astgen.c"
        break;
      case 81:
#line 207 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(lvalue, String(yymsp[0].minor.yy0)); }
#line 1555 "astgen.c"
        break;
      case 82:
#line 210 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(list_literal, yymsp[-1].minor.yy175); }
#line 1560 "astgen.c"
        break;
      case 83:
#line 211 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(list_content, yymsp[0].minor.yy175); }
#line 1565 "astgen.c"
        break;
      case 84:
#line 212 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(list_content, yymsp[-2].minor.yy175, yymsp[0].minor.yy175); }
#line 1570 "astgen.c"
        break;
      case 85:
#line 213 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(list_entry, yymsp[0].minor.yy175); }
#line 1575 "astgen.c"
        break;
      case 86:
#line 216 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(new_expression, String(yymsp[0].minor.yy0)); }
#line 1580 "astgen.c"
        break;
      case 87:
#line 225 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(argument, yymsp[0].minor.yy175); }
#line 1585 "astgen.c"
        break;
      case 89:
#line 229 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(argument_list, yymsp[-2].minor.yy175, yymsp[0].minor.yy175); }
#line 1590 "astgen.c"
        break;
      case 91:
#line 238 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(expression_statement, yymsp[-1].minor.yy175); }
#line 1595 "astgen.c"
        break;
      case 92:
#line 241 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(compound_statement); }
#line 1600 "astgen.c"
        break;
      case 93:
#line 242 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(compound_statement, yymsp[-1].minor.yy175); }
#line 1605 "astgen.c"
        break;
      case 94:
#line 245 "astgen.in"
{ p->Parse(yymsp[-1].minor.yy0); yygotominor.yy175 = p->GetRoot(); }
#line 1610 "astgen.c"
        break;
      case 95:
#line 248 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(return_statement, yymsp[-1].minor.yy175); }
#line 1615 "astgen.c"
        break;
      case 96:
#line 249 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(return_statement);    }
#line 1620 "astgen.c"
        break;
      case 97:
#line 252 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(break_statement); }
#line 1625 "astgen.c"
        break;
      case 98:
#line 253 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(continue_statement); }
#line 1630 "astgen.c"
        break;
      case 99:
#line 256 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(pause_statement); }
#line 1635 "astgen.c"
        break;
      case 104:
#line 270 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(variable_declaration, String(yymsp[0].minor.yy0));    }
#line 1640 "astgen.c"
        break;
      case 105:
#line 271 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(variable_declaration, String(yymsp[-2].minor.yy0), yymsp[0].minor.yy175); }
#line 1645 "astgen.c"
        break;
      case 107:
#line 274 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(declaration_sequence, yymsp[-2].minor.yy175, yymsp[0].minor.yy175); }
#line 1650 "astgen.c"
        break;
      case 108:
#line 281 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(class_declaration, String(yymsp[-2].minor.yy0)); }
#line 1655 "astgen.c"
        break;
      case 109:
#line 282 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(class_declaration, String(yymsp[-3].minor.yy0), yymsp[-1].minor.yy39); }
#line 1660 "astgen.c"
        break;
      case 112:
#line 288 "astgen.in"
{ 
  yygotominor.yy39 = new AstList;
  yygotominor.yy39->push_back(yymsp[0].minor.yy175);
}
#line 1668 "astgen.c"
        break;
      case 113:
#line 292 "astgen.in"
{ 
  yygotominor.yy39 = yymsp[-1].minor.yy39;
  yygotominor.yy39->push_back(yymsp[0].minor.yy175);
}
#line 1676 "astgen.c"
        break;
      case 114:
#line 303 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(function_declaration, String(yymsp[-4].minor.yy0), yymsp[-2].minor.yy175, yymsp[0].minor.yy175); }
#line 1681 "astgen.c"
        break;
      case 115:
#line 304 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(function_declaration, String(yymsp[-3].minor.yy0), AstData(), yymsp[0].minor.yy175); }
#line 1686 "astgen.c"
        break;
      case 116:
#line 307 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(parameter, String(yymsp[0].minor.yy0)); }
#line 1691 "astgen.c"
        break;
      case 117:
#line 310 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(parameter, String(yymsp[-2].minor.yy0), yymsp[0].minor.yy175); }
#line 1696 "astgen.c"
        break;
      case 119:
      case 121:
      case 124:
#line 314 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(parameter_list, yymsp[-2].minor.yy175, yymsp[0].minor.yy175); }
#line 1703 "astgen.c"
        break;
      case 126:
#line 335 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(for_statement, yymsp[-5].minor.yy175, yymsp[-4].minor.yy175, yymsp[-2].minor.yy175, yymsp[0].minor.yy175); }
#line 1708 "astgen.c"
        break;
      case 129:
      case 130:
#line 346 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(foreach_statement, yymsp[-4].minor.yy175, yymsp[-2].minor.yy175, yymsp[0].minor.yy175); }
#line 1714 "astgen.c"
        break;
      case 131:
#line 348 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(variable_declaration, String(yymsp[0].minor.yy0)); }
#line 1719 "astgen.c"
        break;
      case 132:
#line 359 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(if_statement, yymsp[-2].minor.yy175, yymsp[0].minor.yy175); }
#line 1724 "astgen.c"
        break;
      case 133:
#line 360 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(if_statement, yymsp[-4].minor.yy175, yymsp[-2].minor.yy175, yymsp[0].minor.yy175); }
#line 1729 "astgen.c"
        break;
      case 134:
#line 368 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(while_statement, yymsp[-2].minor.yy175,  yymsp[0].minor.yy175); }
#line 1734 "astgen.c"
        break;
      case 135:
#line 376 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(switch_statement, yymsp[-4].minor.yy175, yymsp[-1].minor.yy39); }
#line 1739 "astgen.c"
        break;
      case 136:
#line 380 "astgen.in"
{ yygotominor.yy39 = new AstList; }
#line 1744 "astgen.c"
        break;
      case 137:
      case 138:
#line 381 "astgen.in"
{ yygotominor.yy39 = yymsp[-1].minor.yy39; yygotominor.yy39->push_back(yymsp[0].minor.yy175); }
#line 1750 "astgen.c"
        break;
      case 139:
#line 385 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(switch_case, yymsp[-2].minor.yy175, yymsp[0].minor.yy175); }
#line 1755 "astgen.c"
        break;
      case 140:
#line 388 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(default_case, yymsp[0].minor.yy175); }
#line 1760 "astgen.c"
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
#line 1808 "astgen.c"
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
#line 1826 "astgen.c"
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

