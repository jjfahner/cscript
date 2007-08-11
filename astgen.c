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

#pragma warning(disable:4065)

#line 18 "astgen.c"
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
#define YYNOCODE 112
#define YYACTIONTYPE unsigned short int
#define CScriptParseTOKENTYPE  Token 
typedef union {
  CScriptParseTOKENTYPE yy0;
  Ast* yy25;
  opcodes yy66;
  AstList* yy97;
  int yy223;
} YYMINORTYPE;
#ifndef YYSTACKDEPTH
#define YYSTACKDEPTH 100
#endif
#define CScriptParseARG_SDECL  Parser* p ;
#define CScriptParseARG_PDECL , Parser* p 
#define CScriptParseARG_FETCH  Parser* p  = yypParser->p 
#define CScriptParseARG_STORE yypParser->p  = p 
#define YYNSTATE 223
#define YYNRULE 127
#define YYERRORSYMBOL 59
#define YYERRSYMDT yy223
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
 /*     0 */   194,  351,  132,  219,   13,  218,  214,  213,  210,  209,
 /*    10 */   207,  205,  202,  201,  199,  198,  197,  195,  120,  192,
 /*    20 */   221,  191,   60,   97,  189,   77,  177,  172,  100,  173,
 /*    30 */   170,   56,  153,  194,  136,  108,  106,    8,  218,  214,
 /*    40 */   213,  210,  209,  207,  205,  202,  201,  199,  198,  197,
 /*    50 */   195,  120,  192,  118,  191,   60,  182,  189,   77,  177,
 /*    60 */   172,  127,  173,  170,   45,   46,   47,  136,  108,  106,
 /*    70 */    31,  200,  192,  107,  191,   60,   26,  189,   77,  177,
 /*    80 */   172,  194,  173,  170,  196,    8,  218,  214,  213,  210,
 /*    90 */   209,  207,  205,  202,  201,  199,  198,  197,  195,  120,
 /*   100 */   192,  117,  191,   60,  162,  189,   77,  177,  172,  130,
 /*   110 */   173,  170,  111,  126,   19,  136,  108,  106,   48,   33,
 /*   120 */    35,   36,   40,   42,   43,   44,   45,   46,   47,  194,
 /*   130 */   114,   15,  193,    6,  218,  214,  213,  210,  209,  207,
 /*   140 */   205,  202,  201,  199,  198,  197,  195,  120,  192,   59,
 /*   150 */   191,   60,   54,  189,   77,  177,  172,   63,  173,  170,
 /*   160 */   114,  149,  194,  136,  108,  106,   58,  160,  214,  213,
 /*   170 */   210,  209,  207,  205,  202,  201,  199,  198,  197,  195,
 /*   180 */   120,  192,   59,  191,   60,   23,  189,   77,  177,  172,
 /*   190 */   183,  173,  170,   27,  147,  175,  136,  108,  106,   58,
 /*   200 */   194,  117,   18,    5,  208,  160,  214,  213,  210,  209,
 /*   210 */   207,  205,  202,  201,  199,  198,  197,  195,  120,  192,
 /*   220 */   133,  191,   60,   55,  189,   77,  177,  172,   64,  173,
 /*   230 */   170,  187,  188,  100,  136,  108,  106,  152,  194,   88,
 /*   240 */   126,    7,  159,   96,  214,  213,  210,  209,  207,  205,
 /*   250 */   202,  201,  199,  198,  197,  195,  120,  192,  135,  191,
 /*   260 */    60,  128,  189,   77,  177,  172,  206,  173,  170,  138,
 /*   270 */   194,   12,  136,  108,  106,  217,  214,  213,  210,  209,
 /*   280 */   207,  205,  202,  201,  199,  198,  197,  195,  120,  192,
 /*   290 */   220,  191,   60,   87,  189,   77,  177,  172,  140,  173,
 /*   300 */   170,  104,  194,   15,  136,  108,  106,  179,  214,  213,
 /*   310 */   210,  209,  207,  205,  202,  201,  199,  198,  197,  195,
 /*   320 */   120,  192,   17,  191,   60,   65,  189,   77,  177,  172,
 /*   330 */    32,  173,  170,    1,  194,    4,  136,  108,  106,  174,
 /*   340 */   214,  213,  210,  209,  207,  205,  202,  201,  199,  198,
 /*   350 */   197,  195,  120,  192,  150,  191,   60,   28,  189,   77,
 /*   360 */   177,  172,   25,  173,  170,    9,  194,  151,  136,  108,
 /*   370 */   106,  167,  214,  213,  210,  209,  207,  205,  202,  201,
 /*   380 */   199,  198,  197,  195,  120,  192,   14,  191,   60,   52,
 /*   390 */   189,   77,  177,  172,   92,  173,  170,    2,  194,  171,
 /*   400 */   136,  108,  106,  203,  214,  213,  210,  209,  207,  205,
 /*   410 */   202,  201,  199,  198,  197,  195,  120,  192,   10,  191,
 /*   420 */    60,  186,  189,   77,  177,  172,   49,  173,  170,   53,
 /*   430 */    11,   22,  136,  108,  106,   98,   61,  103,   51,   50,
 /*   440 */   352,   16,  352,   57,   24,  352,  169,  168,  166,  165,
 /*   450 */   164,  163,  352,  125,    3,  222,  119,   20,  112,  110,
 /*   460 */    58,   90,   94,   89,   49,  352,  352,  101,   93,   86,
 /*   470 */    43,   44,   45,   46,   47,  352,   51,   50,  352,   16,
 /*   480 */   352,   57,   24,  352,  169,  168,  166,  165,  164,  163,
 /*   490 */   352,  125,    3,  124,  119,   20,  112,  110,   58,   90,
 /*   500 */    94,   89,   49,  352,  352,  101,   93,   86,   85,  352,
 /*   510 */   212,  216,  121,  123,   51,   50,  352,   16,  352,   57,
 /*   520 */    24,  352,  169,  168,  166,  165,  164,  163,  352,  125,
 /*   530 */     3,  352,  119,   20,  112,  110,   58,   90,   94,   89,
 /*   540 */   352,  352,  352,  101,   93,   86,   37,   38,   41,   34,
 /*   550 */    39,   48,   33,   35,   36,   40,   42,   43,   44,   45,
 /*   560 */    46,   47,  148,  146,  145,  144,  143,  142,   21,   38,
 /*   570 */    41,   34,   39,   48,   33,   35,   36,   40,   42,   43,
 /*   580 */    44,   45,   46,   47,   41,   34,   39,   48,   33,   35,
 /*   590 */    36,   40,   42,   43,   44,   45,   46,   47,   49,   35,
 /*   600 */    36,   40,   42,   43,   44,   45,   46,   47,  352,  352,
 /*   610 */    51,   50,  352,   16,  352,   57,   24,  352,  169,  168,
 /*   620 */   166,  165,  164,  163,  352,  125,  169,  168,  166,  165,
 /*   630 */   164,  163,   58,  185,  184,   62,   30,   34,   39,   48,
 /*   640 */    33,   35,   36,   40,   42,   43,   44,   45,   46,   47,
 /*   650 */   190,   49,  191,   60,  352,  189,   77,  177,  172,  352,
 /*   660 */   173,  170,  352,   51,   50,  352,   16,  352,   83,   24,
 /*   670 */   352,  169,  168,  166,  165,  164,  163,  204,  131,  137,
 /*   680 */   352,  189,   77,  177,  172,  352,  173,  170,  120,  192,
 /*   690 */    49,  191,   60,  352,  189,   77,  177,  113,  352,  173,
 /*   700 */   170,  352,   51,   50,  352,   16,  109,   83,   24,  176,
 /*   710 */   169,  168,  166,  165,  164,  163,   49,   29,  215,  216,
 /*   720 */   352,  115,  352,  352,  352,  352,  352,  352,   51,   50,
 /*   730 */   352,   16,  352,   83,   24,  352,  169,  168,  166,  165,
 /*   740 */   164,  163,  158,  192,  352,  191,   60,  352,  189,   77,
 /*   750 */   177,  172,  352,  173,  170,   81,  157,   39,   48,   33,
 /*   760 */    35,   36,   40,   42,   43,   44,   45,   46,   47,  156,
 /*   770 */   192,  352,  191,   60,  352,  189,   77,  177,  172,   80,
 /*   780 */   173,  170,  352,  352,  154,  158,  192,  352,  191,   60,
 /*   790 */   352,  189,   77,  177,  172,  352,  173,  170,  352,  161,
 /*   800 */   352,  352,  352,  352,  352,  352,  156,  192,  352,  191,
 /*   810 */    60,  352,  189,   77,  177,  172,  352,  173,  170,  211,
 /*   820 */   192,  155,  191,   60,  352,  189,   77,  177,  172,  352,
 /*   830 */   173,  170,  116,  192,  352,  191,   60,  352,  189,   77,
 /*   840 */   177,  172,  352,  173,  170,   84,  192,  352,  191,   60,
 /*   850 */   352,  189,   77,  177,  172,  352,  173,  170,   99,  192,
 /*   860 */   352,  191,   60,  352,  189,   77,  177,  172,  352,  173,
 /*   870 */   170,  129,  192,  352,  191,   60,  352,  189,   77,  177,
 /*   880 */   172,  352,  173,  170,   91,  192,  352,  191,   60,  352,
 /*   890 */   189,   77,  177,  172,  352,  173,  170,  102,  192,  352,
 /*   900 */   191,   60,  352,  189,   77,  177,  172,  352,  173,  170,
 /*   910 */    95,  192,  352,  191,   60,  352,  189,   77,  177,  172,
 /*   920 */   352,  173,  170,  105,  192,  352,  191,   60,  352,  189,
 /*   930 */    77,  177,  172,  352,  173,  170,  122,  192,  352,  191,
 /*   940 */    60,  352,  189,   77,  177,  172,  352,  173,  170,   82,
 /*   950 */   192,  352,  191,   60,  352,  189,   77,  177,  172,  352,
 /*   960 */   173,  170,  141,  352,  191,   60,  352,  189,   77,  177,
 /*   970 */   172,  352,  173,  170,   76,  352,  189,   77,  177,  172,
 /*   980 */   352,  173,  170,  134,  352,  189,   77,  177,  172,  352,
 /*   990 */   173,  170,   78,  352,  189,   77,  177,  172,  352,  173,
 /*  1000 */   170,   71,  352,  189,   77,  177,  172,  352,  173,  170,
 /*  1010 */   352,   79,  352,  189,   77,  177,  172,  352,  173,  170,
 /*  1020 */   352,  139,  352,  189,   77,  177,  172,  352,  173,  170,
 /*  1030 */    74,  352,  189,   77,  177,  172,  352,  173,  170,   67,
 /*  1040 */   352,  189,   77,  177,  172,  352,  173,  170,  352,   68,
 /*  1050 */   352,  189,   77,  177,  172,  352,  173,  170,   75,  352,
 /*  1060 */   189,   77,  177,  172,  352,  173,  170,   70,  352,  189,
 /*  1070 */    77,  177,  172,  352,  173,  170,   66,  352,  189,   77,
 /*  1080 */   177,  172,  352,  173,  170,   72,  352,  189,   77,  177,
 /*  1090 */   172,  352,  173,  170,  352,   73,  352,  189,   77,  177,
 /*  1100 */   172,  352,  173,  170,  352,   69,  352,  189,   77,  177,
 /*  1110 */   172,  352,  173,  170,  180,   77,  177,  172,  352,  173,
 /*  1120 */   170,  178,   77,  177,  172,  352,  173,  170,  181,   77,
 /*  1130 */   177,  172,  352,  173,  170,
};
static const YYCODETYPE yy_lookahead[] = {
 /*     0 */    59,   60,   61,   62,   63,   64,   65,   66,   67,   68,
 /*    10 */    69,   70,   71,   72,   73,   74,   75,   76,   77,   78,
 /*    20 */    40,   80,   81,   88,   83,   84,   85,   86,   95,   88,
 /*    30 */    89,   98,   99,   59,   93,   94,   95,   63,   64,   65,
 /*    40 */    66,   67,   68,   69,   70,   71,   72,   73,   74,   75,
 /*    50 */    76,   77,   78,   35,   80,   81,   30,   83,   84,   85,
 /*    60 */    86,   40,   88,   89,   14,   15,   16,   93,   94,   95,
 /*    70 */    82,   77,   78,   79,   80,   81,   17,   83,   84,   85,
 /*    80 */    86,   59,   88,   89,  110,   63,   64,   65,   66,   67,
 /*    90 */    68,   69,   70,   71,   72,   73,   74,   75,   76,   77,
 /*   100 */    78,   30,   80,   81,   29,   83,   84,   85,   86,   40,
 /*   110 */    88,   89,   96,   97,   39,   93,   94,   95,    6,    7,
 /*   120 */     8,    9,   10,   11,   12,   13,   14,   15,   16,   59,
 /*   130 */    30,   31,  110,   63,   64,   65,   66,   67,   68,   69,
 /*   140 */    70,   71,   72,   73,   74,   75,   76,   77,   78,   30,
 /*   150 */    80,   81,   39,   83,   84,   85,   86,   39,   88,   89,
 /*   160 */    30,   42,   59,   93,   94,   95,   47,   64,   65,   66,
 /*   170 */    67,   68,   69,   70,   71,   72,   73,   74,   75,   76,
 /*   180 */    77,   78,   30,   80,   81,   17,   83,   84,   85,   86,
 /*   190 */    86,   88,   89,   51,   42,   32,   93,   94,   95,   47,
 /*   200 */    59,   30,   39,   32,  101,   64,   65,   66,   67,   68,
 /*   210 */    69,   70,   71,   72,   73,   74,   75,   76,   77,   78,
 /*   220 */    40,   80,   81,  107,   83,   84,   85,   86,   39,   88,
 /*   230 */    89,  108,  109,   95,   93,   94,   95,   99,   59,   96,
 /*   240 */    97,   32,  101,   64,   65,   66,   67,   68,   69,   70,
 /*   250 */    71,   72,   73,   74,   75,   76,   77,   78,   40,   80,
 /*   260 */    81,   97,   83,   84,   85,   86,   40,   88,   89,   40,
 /*   270 */    59,   32,   93,   94,   95,   64,   65,   66,   67,   68,
 /*   280 */    69,   70,   71,   72,   73,   74,   75,   76,   77,   78,
 /*   290 */   103,   80,   81,   30,   83,   84,   85,   86,   40,   88,
 /*   300 */    89,   32,   59,   31,   93,   94,   95,   64,   65,   66,
 /*   310 */    67,   68,   69,   70,   71,   72,   73,   74,   75,   76,
 /*   320 */    77,   78,   40,   80,   81,   41,   83,   84,   85,   86,
 /*   330 */    24,   88,   89,   24,   59,   32,   93,   94,   95,   64,
 /*   340 */    65,   66,   67,   68,   69,   70,   71,   72,   73,   74,
 /*   350 */    75,   76,   77,   78,   29,   80,   81,   31,   83,   84,
 /*   360 */    85,   86,   31,   88,   89,   32,   59,   40,   93,   94,
 /*   370 */    95,   64,   65,   66,   67,   68,   69,   70,   71,   72,
 /*   380 */    73,   74,   75,   76,   77,   78,   31,   80,   81,   31,
 /*   390 */    83,   84,   85,   86,   30,   88,   89,   24,   59,   32,
 /*   400 */    93,   94,   95,   64,   65,   66,   67,   68,   69,   70,
 /*   410 */    71,   72,   73,   74,   75,   76,   77,   78,   53,   80,
 /*   420 */    81,   42,   83,   84,   85,   86,   13,   88,   89,   41,
 /*   430 */    32,   31,   93,   94,   95,   30,   57,   58,   25,   26,
 /*   440 */   111,   28,  111,   30,   31,  111,   33,   34,   35,   36,
 /*   450 */    37,   38,  111,   40,   41,   42,   43,   44,   45,   46,
 /*   460 */    47,   48,   49,   50,   13,  111,  111,   54,   55,   56,
 /*   470 */    12,   13,   14,   15,   16,  111,   25,   26,  111,   28,
 /*   480 */   111,   30,   31,  111,   33,   34,   35,   36,   37,   38,
 /*   490 */   111,   40,   41,   42,   43,   44,   45,   46,   47,   48,
 /*   500 */    49,   50,   13,  111,  111,   54,   55,   56,  100,  111,
 /*   510 */   102,  103,  104,  105,   25,   26,  111,   28,  111,   30,
 /*   520 */    31,  111,   33,   34,   35,   36,   37,   38,  111,   40,
 /*   530 */    41,  111,   43,   44,   45,   46,   47,   48,   49,   50,
 /*   540 */   111,  111,  111,   54,   55,   56,    1,    2,    3,    4,
 /*   550 */     5,    6,    7,    8,    9,   10,   11,   12,   13,   14,
 /*   560 */    15,   16,   17,   18,   19,   20,   21,   22,   23,    2,
 /*   570 */     3,    4,    5,    6,    7,    8,    9,   10,   11,   12,
 /*   580 */    13,   14,   15,   16,    3,    4,    5,    6,    7,    8,
 /*   590 */     9,   10,   11,   12,   13,   14,   15,   16,   13,    8,
 /*   600 */     9,   10,   11,   12,   13,   14,   15,   16,  111,  111,
 /*   610 */    25,   26,  111,   28,  111,   30,   31,  111,   33,   34,
 /*   620 */    35,   36,   37,   38,  111,   40,   33,   34,   35,   36,
 /*   630 */    37,   38,   47,   25,   26,   27,   28,    4,    5,    6,
 /*   640 */     7,    8,    9,   10,   11,   12,   13,   14,   15,   16,
 /*   650 */    78,   13,   80,   81,  111,   83,   84,   85,   86,  111,
 /*   660 */    88,   89,  111,   25,   26,  111,   28,  111,   30,   31,
 /*   670 */   111,   33,   34,   35,   36,   37,   38,   66,   40,   81,
 /*   680 */   111,   83,   84,   85,   86,  111,   88,   89,   77,   78,
 /*   690 */    13,   80,   81,  111,   83,   84,   85,   86,  111,   88,
 /*   700 */    89,  111,   25,   26,  111,   28,   95,   30,   31,   32,
 /*   710 */    33,   34,   35,   36,   37,   38,   13,  106,  102,  103,
 /*   720 */   111,  105,  111,  111,  111,  111,  111,  111,   25,   26,
 /*   730 */   111,   28,  111,   30,   31,  111,   33,   34,   35,   36,
 /*   740 */    37,   38,   77,   78,  111,   80,   81,  111,   83,   84,
 /*   750 */    85,   86,  111,   88,   89,   90,   91,    5,    6,    7,
 /*   760 */     8,    9,   10,   11,   12,   13,   14,   15,   16,   77,
 /*   770 */    78,  111,   80,   81,  111,   83,   84,   85,   86,   87,
 /*   780 */    88,   89,  111,  111,   92,   77,   78,  111,   80,   81,
 /*   790 */   111,   83,   84,   85,   86,  111,   88,   89,  111,   91,
 /*   800 */   111,  111,  111,  111,  111,  111,   77,   78,  111,   80,
 /*   810 */    81,  111,   83,   84,   85,   86,  111,   88,   89,   77,
 /*   820 */    78,   92,   80,   81,  111,   83,   84,   85,   86,  111,
 /*   830 */    88,   89,   77,   78,  111,   80,   81,  111,   83,   84,
 /*   840 */    85,   86,  111,   88,   89,   77,   78,  111,   80,   81,
 /*   850 */   111,   83,   84,   85,   86,  111,   88,   89,   77,   78,
 /*   860 */   111,   80,   81,  111,   83,   84,   85,   86,  111,   88,
 /*   870 */    89,   77,   78,  111,   80,   81,  111,   83,   84,   85,
 /*   880 */    86,  111,   88,   89,   77,   78,  111,   80,   81,  111,
 /*   890 */    83,   84,   85,   86,  111,   88,   89,   77,   78,  111,
 /*   900 */    80,   81,  111,   83,   84,   85,   86,  111,   88,   89,
 /*   910 */    77,   78,  111,   80,   81,  111,   83,   84,   85,   86,
 /*   920 */   111,   88,   89,   77,   78,  111,   80,   81,  111,   83,
 /*   930 */    84,   85,   86,  111,   88,   89,   77,   78,  111,   80,
 /*   940 */    81,  111,   83,   84,   85,   86,  111,   88,   89,   77,
 /*   950 */    78,  111,   80,   81,  111,   83,   84,   85,   86,  111,
 /*   960 */    88,   89,   78,  111,   80,   81,  111,   83,   84,   85,
 /*   970 */    86,  111,   88,   89,   81,  111,   83,   84,   85,   86,
 /*   980 */   111,   88,   89,   81,  111,   83,   84,   85,   86,  111,
 /*   990 */    88,   89,   81,  111,   83,   84,   85,   86,  111,   88,
 /*  1000 */    89,   81,  111,   83,   84,   85,   86,  111,   88,   89,
 /*  1010 */   111,   81,  111,   83,   84,   85,   86,  111,   88,   89,
 /*  1020 */   111,   81,  111,   83,   84,   85,   86,  111,   88,   89,
 /*  1030 */    81,  111,   83,   84,   85,   86,  111,   88,   89,   81,
 /*  1040 */   111,   83,   84,   85,   86,  111,   88,   89,  111,   81,
 /*  1050 */   111,   83,   84,   85,   86,  111,   88,   89,   81,  111,
 /*  1060 */    83,   84,   85,   86,  111,   88,   89,   81,  111,   83,
 /*  1070 */    84,   85,   86,  111,   88,   89,   81,  111,   83,   84,
 /*  1080 */    85,   86,  111,   88,   89,   81,  111,   83,   84,   85,
 /*  1090 */    86,  111,   88,   89,  111,   81,  111,   83,   84,   85,
 /*  1100 */    86,  111,   88,   89,  111,   81,  111,   83,   84,   85,
 /*  1110 */    86,  111,   88,   89,   83,   84,   85,   86,  111,   88,
 /*  1120 */    89,   83,   84,   85,   86,  111,   88,   89,   83,   84,
 /*  1130 */    85,   86,  111,   88,   89,
};
#define YY_SHIFT_USE_DFLT (-21)
#define YY_SHIFT_MAX 123
static const short yy_shift_ofst[] = {
 /*     0 */   489,  489,  489,  413,  489,  489,  451,  489,  489,  489,
 /*    10 */   489,  489,  489,  489,  585,  677,  703,  703,  703,  703,
 /*    20 */   638,  703,  703,  703,  703,  703,  703,  703,  703,  703,
 /*    30 */   703,  703,  703,  703,  703,  703,  703,  703,  703,  703,
 /*    40 */   703,  703,  703,  703,  703,  703,  703,  703,  703,  703,
 /*    50 */   703,  703,  171,  152,   71,  379,  119,  100,  130,  130,
 /*    60 */   545,  593,   26,  263,  130,  -21,  567,  581,  633,  752,
 /*    70 */   112,  591,  591,  458,  458,  458,  458,  608,   50,   50,
 /*    80 */   163,   75,  269,  272,  306,  303,  326,   59,  189,  355,
 /*    90 */   364,  367,  388,  400,  405,  398,  365,  373,  358,  333,
 /*   100 */   327,  331,  325,  309,  284,  282,  258,  239,  229,  226,
 /*   110 */   218,  189,  180,  142,  168,  118,   69,   59,   21,   18,
 /*   120 */   -20,  113,  209,  118,
};
#define YY_REDUCE_USE_DFLT (-68)
#define YY_REDUCE_MAX 65
static const short yy_reduce_ofst[] = {
 /*     0 */   -59,  -26,   22,   70,  141,  103,  211,  307,  211,  243,
 /*    10 */   275,  179,  339,  211,  611,  692,  665,   -6,  729,  708,
 /*    20 */   755,  768,  781,  794,  807,  833,  742,  859,  872,  846,
 /*    30 */   820,  572,  884, 1004, 1024,  977, 1014,  995,  958,  986,
 /*    40 */   949,  968,  893,  911,  930,  940,  598,  902,  920, 1045,
 /*    50 */  1038, 1031,  408,  -67,  616,  123,  138,   16,  143,   16,
 /*    60 */   -12,  -65,  104,  187,  164,  116,
};
static const YYACTIONTYPE yy_default[] = {
 /*     0 */   227,  350,  350,  350,  350,  350,  350,  350,  349,  350,
 /*    10 */   350,  350,  350,  228,  350,  350,  350,  243,  350,  350,
 /*    20 */   350,  350,  350,  350,  350,  350,  350,  350,  350,  350,
 /*    30 */   350,  350,  350,  350,  350,  350,  350,  350,  350,  350,
 /*    40 */   350,  350,  350,  350,  350,  350,  350,  350,  350,  350,
 /*    50 */   350,  350,  350,  350,  350,  350,  350,  293,  350,  350,
 /*    60 */   253,  350,  350,  350,  350,  344,  256,  257,  258,  259,
 /*    70 */   260,  261,  262,  264,  265,  263,  266,  272,  267,  268,
 /*    80 */   350,  350,  350,  293,  350,  350,  350,  350,  313,  350,
 /*    90 */   350,  350,  350,  350,  350,  350,  340,  350,  350,  350,
 /*   100 */   350,  350,  350,  350,  350,  350,  350,  350,  350,  350,
 /*   110 */   350,  314,  350,  284,  315,  334,  350,  326,  350,  350,
 /*   120 */   350,  332,  350,  333,  304,  301,  317,  305,  318,  316,
 /*   130 */   306,  307,  223,  308,  271,  309,  310,  270,  311,  269,
 /*   140 */   312,  254,  252,  251,  250,  249,  248,  319,  247,  320,
 /*   150 */   280,  321,  323,  322,  299,  300,  298,  295,  297,  324,
 /*   160 */   335,  296,  294,  292,  291,  290,  289,  339,  288,  287,
 /*   170 */   286,  285,  284,  283,  341,  282,  281,  276,  275,  342,
 /*   180 */   274,  273,  293,  279,  278,  277,  343,  345,  346,  255,
 /*   190 */   246,  245,  242,  347,  241,  240,  348,  239,  238,  237,
 /*   200 */   244,  236,  235,  336,  337,  234,  338,  233,  325,  232,
 /*   210 */   231,  327,  328,  230,  229,  329,  330,  226,  225,  224,
 /*   220 */   331,  302,  303,
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
  "BITXOR",        "BITAND",        "EQUALS",        "NEQUALS",     
  "ST",            "SE",            "GT",            "GE",          
  "ADDOP",         "SUBOP",         "MULOP",         "DIVOP",       
  "MODOP",         "ASSIGN",        "ASSADD",        "ASSSUB",      
  "ASSMUL",        "ASSDIV",        "ASSMOD",        "QUESTION",    
  "COLON",         "ADDADD",        "SUBSUB",        "DOT",         
  "LBRACKET",      "RBRACKET",      "IDENTIFIER",    "LPAREN",      
  "RPAREN",        "INTEGER",       "REAL",          "STRING",      
  "TRUE",          "FALSE",         "NULL",          "COMMA",       
  "SEMICOLON",     "LBRACE",        "RBRACE",        "INCLUDE",     
  "RETURN",        "BREAK",         "CONTINUE",      "VAR",         
  "STRUCT",        "FUNCTION",      "FOR",           "IN",          
  "LOWER_THAN_ELSE",  "ELSE",          "IF",            "WHILE",       
  "SWITCH",        "CASE",          "DEFAULT",       "error",       
  "main",          "translation_unit",  "statement_sequence_opt",  "statement_sequence",
  "statement",     "include_statement",  "expression_statement",  "declaration_statement",
  "for_statement",  "compound_statement",  "if_statement",  "while_statement",
  "foreach_statement",  "return_statement",  "switch_statement",  "break_statement",
  "continue_statement",  "expression",    "assignment_expression",  "expression_opt",
  "conditional_expression",  "binary_expression",  "assignment_operator",  "unary_expression",
  "postfix_expression",  "primary_expression",  "id_expression",  "argument_list",
  "literal",       "list_literal",  "list_content",  "list_entry",  
  "argument",      "function_declaration",  "struct_declaration",  "variable_declaration",
  "declarator_sequence",  "declarator",    "struct_members",  "struct_member",
  "parameter_list",  "function_body",  "parameter",     "opt_parameter",
  "parameters",    "opt_parameters",  "for_init_statement",  "switch_body", 
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
 /*  49 */ "unary_expression ::= postfix_expression",
 /*  50 */ "unary_expression ::= SUBOP unary_expression",
 /*  51 */ "unary_expression ::= ADDADD unary_expression",
 /*  52 */ "unary_expression ::= SUBSUB unary_expression",
 /*  53 */ "postfix_expression ::= primary_expression",
 /*  54 */ "postfix_expression ::= postfix_expression ADDADD",
 /*  55 */ "postfix_expression ::= postfix_expression SUBSUB",
 /*  56 */ "postfix_expression ::= postfix_expression DOT id_expression",
 /*  57 */ "postfix_expression ::= postfix_expression LBRACKET expression RBRACKET",
 /*  58 */ "postfix_expression ::= IDENTIFIER LPAREN RPAREN",
 /*  59 */ "postfix_expression ::= IDENTIFIER LPAREN argument_list RPAREN",
 /*  60 */ "primary_expression ::= literal",
 /*  61 */ "primary_expression ::= id_expression",
 /*  62 */ "primary_expression ::= LPAREN expression RPAREN",
 /*  63 */ "primary_expression ::= list_literal",
 /*  64 */ "literal ::= INTEGER",
 /*  65 */ "literal ::= REAL",
 /*  66 */ "literal ::= STRING",
 /*  67 */ "literal ::= TRUE",
 /*  68 */ "literal ::= FALSE",
 /*  69 */ "literal ::= NULL",
 /*  70 */ "id_expression ::= IDENTIFIER",
 /*  71 */ "list_literal ::= LBRACKET list_content RBRACKET",
 /*  72 */ "list_content ::= list_entry",
 /*  73 */ "list_content ::= list_content COMMA list_entry",
 /*  74 */ "list_entry ::= expression",
 /*  75 */ "argument ::= expression",
 /*  76 */ "argument_list ::= argument",
 /*  77 */ "argument_list ::= argument_list COMMA argument",
 /*  78 */ "expression_statement ::= SEMICOLON",
 /*  79 */ "expression_statement ::= expression SEMICOLON",
 /*  80 */ "compound_statement ::= LBRACE RBRACE",
 /*  81 */ "compound_statement ::= LBRACE statement_sequence RBRACE",
 /*  82 */ "include_statement ::= INCLUDE STRING SEMICOLON",
 /*  83 */ "return_statement ::= RETURN expression SEMICOLON",
 /*  84 */ "return_statement ::= RETURN SEMICOLON",
 /*  85 */ "break_statement ::= BREAK SEMICOLON",
 /*  86 */ "continue_statement ::= CONTINUE SEMICOLON",
 /*  87 */ "declaration_statement ::= function_declaration",
 /*  88 */ "declaration_statement ::= struct_declaration SEMICOLON",
 /*  89 */ "declaration_statement ::= variable_declaration SEMICOLON",
 /*  90 */ "variable_declaration ::= VAR declarator_sequence",
 /*  91 */ "variable_declaration ::= IDENTIFIER declarator_sequence",
 /*  92 */ "declarator ::= IDENTIFIER",
 /*  93 */ "declarator ::= IDENTIFIER ASSIGN expression",
 /*  94 */ "declarator_sequence ::= declarator",
 /*  95 */ "declarator_sequence ::= declarator_sequence COMMA declarator",
 /*  96 */ "struct_declaration ::= STRUCT IDENTIFIER LBRACE RBRACE",
 /*  97 */ "struct_declaration ::= STRUCT IDENTIFIER LBRACE struct_members RBRACE",
 /*  98 */ "struct_member ::= variable_declaration SEMICOLON",
 /*  99 */ "struct_members ::= struct_member",
 /* 100 */ "struct_members ::= struct_members struct_member",
 /* 101 */ "function_declaration ::= FUNCTION IDENTIFIER LPAREN parameter_list RPAREN function_body",
 /* 102 */ "function_declaration ::= FUNCTION IDENTIFIER LPAREN RPAREN function_body",
 /* 103 */ "parameter ::= IDENTIFIER",
 /* 104 */ "opt_parameter ::= IDENTIFIER ASSIGN expression",
 /* 105 */ "parameters ::= parameter",
 /* 106 */ "parameters ::= parameters COMMA parameter",
 /* 107 */ "opt_parameters ::= opt_parameter",
 /* 108 */ "opt_parameters ::= opt_parameters COMMA opt_parameter",
 /* 109 */ "parameter_list ::= parameters",
 /* 110 */ "parameter_list ::= opt_parameters",
 /* 111 */ "parameter_list ::= parameters COMMA opt_parameters",
 /* 112 */ "function_body ::= statement",
 /* 113 */ "for_statement ::= FOR LPAREN for_init_statement expression SEMICOLON expression_opt RPAREN statement",
 /* 114 */ "for_init_statement ::= expression_statement",
 /* 115 */ "for_init_statement ::= variable_declaration SEMICOLON",
 /* 116 */ "foreach_statement ::= FOR LPAREN id_expression IN expression RPAREN statement",
 /* 117 */ "if_statement ::= IF LPAREN expression RPAREN statement",
 /* 118 */ "if_statement ::= IF LPAREN expression RPAREN statement ELSE statement",
 /* 119 */ "while_statement ::= WHILE LPAREN expression RPAREN statement",
 /* 120 */ "switch_statement ::= SWITCH LPAREN expression RPAREN LBRACE switch_body RBRACE",
 /* 121 */ "switch_body ::=",
 /* 122 */ "switch_body ::= switch_body switch_case",
 /* 123 */ "switch_body ::= switch_body default_case",
 /* 124 */ "switch_case ::= CASE literal COLON case_statements",
 /* 125 */ "default_case ::= DEFAULT COLON case_statements",
 /* 126 */ "case_statements ::= statement_sequence",
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
  { 60, 1 },
  { 61, 1 },
  { 63, 1 },
  { 63, 2 },
  { 62, 0 },
  { 62, 1 },
  { 64, 1 },
  { 64, 1 },
  { 64, 1 },
  { 64, 1 },
  { 64, 1 },
  { 64, 1 },
  { 64, 1 },
  { 64, 1 },
  { 64, 1 },
  { 64, 1 },
  { 64, 1 },
  { 64, 1 },
  { 64, 1 },
  { 77, 1 },
  { 79, 0 },
  { 79, 1 },
  { 78, 1 },
  { 78, 3 },
  { 82, 1 },
  { 82, 1 },
  { 82, 1 },
  { 82, 1 },
  { 82, 1 },
  { 82, 1 },
  { 80, 1 },
  { 80, 5 },
  { 81, 1 },
  { 81, 3 },
  { 81, 3 },
  { 81, 3 },
  { 81, 3 },
  { 81, 3 },
  { 81, 3 },
  { 81, 3 },
  { 81, 3 },
  { 81, 3 },
  { 81, 3 },
  { 81, 3 },
  { 81, 3 },
  { 81, 3 },
  { 81, 3 },
  { 81, 3 },
  { 81, 3 },
  { 83, 1 },
  { 83, 2 },
  { 83, 2 },
  { 83, 2 },
  { 84, 1 },
  { 84, 2 },
  { 84, 2 },
  { 84, 3 },
  { 84, 4 },
  { 84, 3 },
  { 84, 4 },
  { 85, 1 },
  { 85, 1 },
  { 85, 3 },
  { 85, 1 },
  { 88, 1 },
  { 88, 1 },
  { 88, 1 },
  { 88, 1 },
  { 88, 1 },
  { 88, 1 },
  { 86, 1 },
  { 89, 3 },
  { 90, 1 },
  { 90, 3 },
  { 91, 1 },
  { 92, 1 },
  { 87, 1 },
  { 87, 3 },
  { 66, 1 },
  { 66, 2 },
  { 69, 2 },
  { 69, 3 },
  { 65, 3 },
  { 73, 3 },
  { 73, 2 },
  { 75, 2 },
  { 76, 2 },
  { 67, 1 },
  { 67, 2 },
  { 67, 2 },
  { 95, 2 },
  { 95, 2 },
  { 97, 1 },
  { 97, 3 },
  { 96, 1 },
  { 96, 3 },
  { 94, 4 },
  { 94, 5 },
  { 99, 2 },
  { 98, 1 },
  { 98, 2 },
  { 93, 6 },
  { 93, 5 },
  { 102, 1 },
  { 103, 3 },
  { 104, 1 },
  { 104, 3 },
  { 105, 1 },
  { 105, 3 },
  { 100, 1 },
  { 100, 1 },
  { 100, 3 },
  { 101, 1 },
  { 68, 8 },
  { 106, 1 },
  { 106, 2 },
  { 72, 7 },
  { 70, 5 },
  { 70, 7 },
  { 71, 5 },
  { 74, 7 },
  { 107, 0 },
  { 107, 2 },
  { 107, 2 },
  { 108, 4 },
  { 109, 3 },
  { 110, 1 },
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
#line 72 "astgen.in"
{ p->SetRoot(yymsp[0].minor.yy25); }
#line 1136 "astgen.c"
        break;
      case 1:
#line 75 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(translation_unit, yymsp[0].minor.yy25); }
#line 1141 "astgen.c"
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
      case 49:
      case 53:
      case 60:
      case 61:
      case 63:
      case 76:
      case 87:
      case 90:
      case 94:
      case 99:
      case 105:
      case 107:
      case 109:
      case 110:
      case 112:
      case 114:
      case 126:
#line 78 "astgen.in"
{ yygotominor.yy25 = yymsp[0].minor.yy25; }
#line 1181 "astgen.c"
        break;
      case 3:
#line 79 "astgen.in"
{ 
  if(yymsp[-1].minor.yy25->m_type == statement_sequence) {
    yygotominor.yy25 = yymsp[-1].minor.yy25;
  }
  else {
    yygotominor.yy25 = p->AllocAst(statement_sequence, new AstList);
    yygotominor.yy25->m_a1.GetList()->push_back(yymsp[-1].minor.yy25);
  }
  yygotominor.yy25->m_a1.GetList()->push_back(yymsp[0].minor.yy25);
}
#line 1195 "astgen.c"
        break;
      case 4:
      case 20:
#line 92 "astgen.in"
{ yygotominor.yy25 = 0; }
#line 1201 "astgen.c"
        break;
      case 18:
      case 78:
#line 110 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(empty_statement); }
#line 1207 "astgen.c"
        break;
      case 23:
#line 126 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(assignment_expression, yymsp[-1].minor.yy66, yymsp[-2].minor.yy25, yymsp[0].minor.yy25); }
#line 1212 "astgen.c"
        break;
      case 24:
#line 130 "astgen.in"
{ yygotominor.yy66 = op_assign; }
#line 1217 "astgen.c"
        break;
      case 25:
#line 131 "astgen.in"
{ yygotominor.yy66 = op_assadd; }
#line 1222 "astgen.c"
        break;
      case 26:
#line 132 "astgen.in"
{ yygotominor.yy66 = op_asssub; }
#line 1227 "astgen.c"
        break;
      case 27:
#line 133 "astgen.in"
{ yygotominor.yy66 = op_assmul; }
#line 1232 "astgen.c"
        break;
      case 28:
#line 134 "astgen.in"
{ yygotominor.yy66 = op_assdiv; }
#line 1237 "astgen.c"
        break;
      case 29:
#line 135 "astgen.in"
{ yygotominor.yy66 = op_assmod; }
#line 1242 "astgen.c"
        break;
      case 31:
#line 139 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(ternary_expression, yymsp[-4].minor.yy25, yymsp[-2].minor.yy25, yymsp[0].minor.yy25); }
#line 1247 "astgen.c"
        break;
      case 33:
#line 143 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(binary_expression, op_logor,   yymsp[-2].minor.yy25, yymsp[0].minor.yy25); }
#line 1252 "astgen.c"
        break;
      case 34:
#line 144 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(binary_expression, op_logand,  yymsp[-2].minor.yy25, yymsp[0].minor.yy25); }
#line 1257 "astgen.c"
        break;
      case 35:
#line 145 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(binary_expression, op_bitor,   yymsp[-2].minor.yy25, yymsp[0].minor.yy25); }
#line 1262 "astgen.c"
        break;
      case 36:
#line 146 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(binary_expression, op_bitxor,  yymsp[-2].minor.yy25, yymsp[0].minor.yy25); }
#line 1267 "astgen.c"
        break;
      case 37:
#line 147 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(binary_expression, op_bitand,  yymsp[-2].minor.yy25, yymsp[0].minor.yy25); }
#line 1272 "astgen.c"
        break;
      case 38:
#line 148 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(binary_expression, op_eq,   yymsp[-2].minor.yy25, yymsp[0].minor.yy25); }
#line 1277 "astgen.c"
        break;
      case 39:
#line 149 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(binary_expression, op_ne,   yymsp[-2].minor.yy25, yymsp[0].minor.yy25); }
#line 1282 "astgen.c"
        break;
      case 40:
#line 150 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(binary_expression, op_lt,   yymsp[-2].minor.yy25, yymsp[0].minor.yy25); }
#line 1287 "astgen.c"
        break;
      case 41:
#line 151 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(binary_expression, op_le,   yymsp[-2].minor.yy25, yymsp[0].minor.yy25); }
#line 1292 "astgen.c"
        break;
      case 42:
#line 152 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(binary_expression, op_gt,   yymsp[-2].minor.yy25, yymsp[0].minor.yy25); }
#line 1297 "astgen.c"
        break;
      case 43:
#line 153 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(binary_expression, op_ge,   yymsp[-2].minor.yy25, yymsp[0].minor.yy25); }
#line 1302 "astgen.c"
        break;
      case 44:
#line 154 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(binary_expression, op_add,  yymsp[-2].minor.yy25, yymsp[0].minor.yy25); }
#line 1307 "astgen.c"
        break;
      case 45:
#line 155 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(binary_expression, op_sub,  yymsp[-2].minor.yy25, yymsp[0].minor.yy25); }
#line 1312 "astgen.c"
        break;
      case 46:
#line 156 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(binary_expression, op_mul,  yymsp[-2].minor.yy25, yymsp[0].minor.yy25); }
#line 1317 "astgen.c"
        break;
      case 47:
#line 157 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(binary_expression, op_div,  yymsp[-2].minor.yy25, yymsp[0].minor.yy25); }
#line 1322 "astgen.c"
        break;
      case 48:
#line 158 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(binary_expression, op_mod,  yymsp[-2].minor.yy25, yymsp[0].minor.yy25); }
#line 1327 "astgen.c"
        break;
      case 50:
#line 162 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(prefix_expression, op_negate, yymsp[0].minor.yy25); }
#line 1332 "astgen.c"
        break;
      case 51:
#line 163 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(prefix_expression, op_preinc, yymsp[0].minor.yy25); }
#line 1337 "astgen.c"
        break;
      case 52:
#line 164 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(prefix_expression, op_predec, yymsp[0].minor.yy25); }
#line 1342 "astgen.c"
        break;
      case 54:
#line 168 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(postfix_expression, op_postinc, yymsp[-1].minor.yy25); }
#line 1347 "astgen.c"
        break;
      case 55:
#line 169 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(postfix_expression, op_postdec, yymsp[-1].minor.yy25); }
#line 1352 "astgen.c"
        break;
      case 56:
#line 170 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(member_expression, yymsp[-2].minor.yy25, yymsp[0].minor.yy25); }
#line 1357 "astgen.c"
        break;
      case 57:
#line 171 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(index_expression, yymsp[-3].minor.yy25, yymsp[-1].minor.yy25); }
#line 1362 "astgen.c"
        break;
      case 58:
#line 172 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(function_call, String(yymsp[-2].minor.yy0)); }
#line 1367 "astgen.c"
        break;
      case 59:
#line 173 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(function_call, String(yymsp[-3].minor.yy0), yymsp[-1].minor.yy25); }
#line 1372 "astgen.c"
        break;
      case 62:
      case 88:
      case 89:
      case 98:
      case 115:
#line 178 "astgen.in"
{ yygotominor.yy25 = yymsp[-1].minor.yy25; }
#line 1381 "astgen.c"
        break;
      case 64:
#line 182 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(literal, Variant(String(yymsp[0].minor.yy0), Variant::stInt)); }
#line 1386 "astgen.c"
        break;
      case 65:
#line 183 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(literal, Variant(String(yymsp[0].minor.yy0), Variant::stReal)); }
#line 1391 "astgen.c"
        break;
      case 66:
#line 184 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(literal, Variant(String(yymsp[0].minor.yy0), Variant::stString)); }
#line 1396 "astgen.c"
        break;
      case 67:
#line 185 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(literal, Variant(true));    }
#line 1401 "astgen.c"
        break;
      case 68:
#line 186 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(literal, Variant(false));   }
#line 1406 "astgen.c"
        break;
      case 69:
#line 187 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(literal, Variant());        }
#line 1411 "astgen.c"
        break;
      case 70:
#line 190 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(lvalue, String(yymsp[0].minor.yy0)); }
#line 1416 "astgen.c"
        break;
      case 71:
#line 193 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(list_literal, yymsp[-1].minor.yy25); }
#line 1421 "astgen.c"
        break;
      case 72:
#line 195 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(list_content, yymsp[0].minor.yy25); }
#line 1426 "astgen.c"
        break;
      case 73:
#line 196 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(list_content, yymsp[-2].minor.yy25, yymsp[0].minor.yy25); }
#line 1431 "astgen.c"
        break;
      case 74:
#line 198 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(list_entry, yymsp[0].minor.yy25); }
#line 1436 "astgen.c"
        break;
      case 75:
#line 207 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(argument, yymsp[0].minor.yy25); }
#line 1441 "astgen.c"
        break;
      case 77:
#line 211 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(argument_list, yymsp[-2].minor.yy25, yymsp[0].minor.yy25); }
#line 1446 "astgen.c"
        break;
      case 79:
#line 220 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(expression_statement, yymsp[-1].minor.yy25); }
#line 1451 "astgen.c"
        break;
      case 80:
#line 223 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(compound_statement); }
#line 1456 "astgen.c"
        break;
      case 81:
#line 224 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(compound_statement, yymsp[-1].minor.yy25); }
#line 1461 "astgen.c"
        break;
      case 82:
#line 227 "astgen.in"
{ p->Parse(yymsp[-1].minor.yy0); yygotominor.yy25 = p->GetRoot(); }
#line 1466 "astgen.c"
        break;
      case 83:
#line 230 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(return_statement, yymsp[-1].minor.yy25); }
#line 1471 "astgen.c"
        break;
      case 84:
#line 231 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(return_statement);    }
#line 1476 "astgen.c"
        break;
      case 85:
#line 234 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(break_statement); }
#line 1481 "astgen.c"
        break;
      case 86:
#line 235 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(continue_statement); }
#line 1486 "astgen.c"
        break;
      case 91:
#line 248 "astgen.in"
{ yygotominor.yy25 = yymsp[0].minor.yy25; yygotominor.yy25->m_a3 = String(yymsp[-1].minor.yy0); }
#line 1491 "astgen.c"
        break;
      case 92:
#line 250 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(variable_declaration, String(yymsp[0].minor.yy0));    }
#line 1496 "astgen.c"
        break;
      case 93:
#line 251 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(variable_declaration, String(yymsp[-2].minor.yy0), yymsp[0].minor.yy25); }
#line 1501 "astgen.c"
        break;
      case 95:
#line 254 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(declaration_sequence, yymsp[-2].minor.yy25, yymsp[0].minor.yy25); }
#line 1506 "astgen.c"
        break;
      case 96:
#line 263 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(struct_declaration, String(yymsp[-2].minor.yy0)); }
#line 1511 "astgen.c"
        break;
      case 97:
#line 264 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(struct_declaration, String(yymsp[-3].minor.yy0), yymsp[-1].minor.yy25); }
#line 1516 "astgen.c"
        break;
      case 100:
#line 271 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(struct_members, yymsp[-1].minor.yy25, yymsp[0].minor.yy25); }
#line 1521 "astgen.c"
        break;
      case 101:
#line 279 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(function_declaration, String(yymsp[-4].minor.yy0), yymsp[-2].minor.yy25, yymsp[0].minor.yy25); }
#line 1526 "astgen.c"
        break;
      case 102:
#line 280 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(function_declaration, String(yymsp[-3].minor.yy0), AstData(), yymsp[0].minor.yy25); }
#line 1531 "astgen.c"
        break;
      case 103:
#line 283 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(parameter, String(yymsp[0].minor.yy0)); }
#line 1536 "astgen.c"
        break;
      case 104:
#line 286 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(parameter, String(yymsp[-2].minor.yy0), yymsp[0].minor.yy25); }
#line 1541 "astgen.c"
        break;
      case 106:
      case 108:
      case 111:
#line 290 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(parameter_list, yymsp[-2].minor.yy25, yymsp[0].minor.yy25); }
#line 1548 "astgen.c"
        break;
      case 113:
#line 311 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(for_statement, yymsp[-5].minor.yy25, yymsp[-4].minor.yy25, yymsp[-2].minor.yy25, yymsp[0].minor.yy25); }
#line 1553 "astgen.c"
        break;
      case 116:
#line 322 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(foreach_statement, yymsp[-4].minor.yy25, yymsp[-2].minor.yy25, yymsp[0].minor.yy25); }
#line 1558 "astgen.c"
        break;
      case 117:
#line 333 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(if_statement, yymsp[-2].minor.yy25, yymsp[0].minor.yy25); }
#line 1563 "astgen.c"
        break;
      case 118:
#line 334 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(if_statement, yymsp[-4].minor.yy25, yymsp[-2].minor.yy25, yymsp[0].minor.yy25); }
#line 1568 "astgen.c"
        break;
      case 119:
#line 342 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(while_statement, yymsp[-2].minor.yy25,  yymsp[0].minor.yy25); }
#line 1573 "astgen.c"
        break;
      case 120:
#line 350 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(switch_statement, yymsp[-4].minor.yy25, yymsp[-1].minor.yy97); }
#line 1578 "astgen.c"
        break;
      case 121:
#line 354 "astgen.in"
{ yygotominor.yy97 = new AstList; }
#line 1583 "astgen.c"
        break;
      case 122:
      case 123:
#line 355 "astgen.in"
{ yygotominor.yy97 = yymsp[-1].minor.yy97; yygotominor.yy97->push_back(yymsp[0].minor.yy25); }
#line 1589 "astgen.c"
        break;
      case 124:
#line 359 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(switch_case, yymsp[-2].minor.yy25, yymsp[0].minor.yy25); }
#line 1594 "astgen.c"
        break;
      case 125:
#line 362 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(default_case, yymsp[0].minor.yy25); }
#line 1599 "astgen.c"
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
#line 52 "astgen.in"

  p->OnParseFailure();
#line 1647 "astgen.c"
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
#line 55 "astgen.in"

  p->OnSyntaxError();
#line 1665 "astgen.c"
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

