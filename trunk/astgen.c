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
#define YYNOCODE 114
#define YYACTIONTYPE unsigned short int
#define CScriptParseTOKENTYPE  Token 
typedef union {
  CScriptParseTOKENTYPE yy0;
  opcodes yy38;
  Ast* yy81;
  AstList* yy223;
  int yy227;
} YYMINORTYPE;
#ifndef YYSTACKDEPTH
#define YYSTACKDEPTH 100
#endif
#define CScriptParseARG_SDECL  Parser* p ;
#define CScriptParseARG_PDECL , Parser* p 
#define CScriptParseARG_FETCH  Parser* p  = yypParser->p 
#define CScriptParseARG_STORE yypParser->p  = p 
#define YYNSTATE 225
#define YYNRULE 129
#define YYERRORSYMBOL 60
#define YYERRSYMDT yy227
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
 /*     0 */   196,  355,  134,  221,    9,  220,  216,  215,  212,  211,
 /*    10 */   209,  207,  204,  203,  201,  200,  199,  197,  122,  194,
 /*    20 */   116,  193,   60,   15,  191,   76,  180,  179,  129,  175,
 /*    30 */   174,  172,  114,  128,  196,  138,  108,   82,   12,  220,
 /*    40 */   216,  215,  212,  211,  209,  207,  204,  203,  201,  200,
 /*    50 */   199,  197,  122,  194,  125,  193,   60,   29,  191,   76,
 /*    60 */   180,  179,   55,  175,  174,  172,   42,   40,   46,  138,
 /*    70 */   108,   82,   33,   34,   35,   41,   43,   45,   44,   39,
 /*    80 */    42,   40,   46,  196,   88,  128,  195,   12,  220,  216,
 /*    90 */   215,  212,  211,  209,  207,  204,  203,  201,  200,  199,
 /*   100 */   197,  122,  194,  132,  193,   60,  164,  191,   76,  180,
 /*   110 */   179,   21,  175,  174,  172,   19,  189,  190,  138,  108,
 /*   120 */    82,   35,   41,   43,   45,   44,   39,   42,   40,   46,
 /*   130 */   217,  218,  196,  117,   63,  198,    6,  220,  216,  215,
 /*   140 */   212,  211,  209,  207,  204,  203,  201,  200,  199,  197,
 /*   150 */   122,  194,  222,  193,   60,   23,  191,   76,  180,  179,
 /*   160 */   109,  175,  174,  172,    5,  196,    8,  138,  108,   82,
 /*   170 */   162,  216,  215,  212,  211,  209,  207,  204,  203,  201,
 /*   180 */   200,  199,  197,  122,  194,  123,  193,   60,  130,  191,
 /*   190 */    76,  180,  179,   95,  175,  174,  172,  154,  177,   62,
 /*   200 */   138,  108,   82,   95,  196,   18,   56,  155,  210,  162,
 /*   210 */   216,  215,  212,  211,  209,  207,  204,  203,  201,  200,
 /*   220 */   199,  197,  122,  194,  116,  193,   60,  135,  191,   76,
 /*   230 */   180,  179,   97,  175,  174,  172,   32,   54,  137,  138,
 /*   240 */   108,   82,  109,  196,  140,  120,  208,  161,  219,  216,
 /*   250 */   215,  212,  211,  209,  207,  204,  203,  201,  200,  199,
 /*   260 */   197,  122,  194,   10,  193,   60,   14,  191,   76,  180,
 /*   270 */   179,  142,  175,  174,  172,   11,  196,   24,  138,  108,
 /*   280 */    82,  169,  216,  215,  212,  211,  209,  207,  204,  203,
 /*   290 */   201,  200,  199,  197,  122,  194,   26,  193,   60,   15,
 /*   300 */   191,   76,  180,  179,   17,  175,  174,  172,   87,  196,
 /*   310 */    13,  138,  108,   82,  205,  216,  215,  212,  211,  209,
 /*   320 */   207,  204,  203,  201,  200,  199,  197,  122,  194,   31,
 /*   330 */   193,   60,    2,  191,   76,  180,  179,   64,  175,  174,
 /*   340 */   172,    4,  196,  173,  138,  108,   82,  176,  216,  215,
 /*   350 */   212,  211,  209,  207,  204,  203,  201,  200,  199,  197,
 /*   360 */   122,  194,  185,  193,   60,   91,  191,   76,  180,  179,
 /*   370 */    52,  175,  174,  172,    1,  196,   22,  138,  108,   82,
 /*   380 */   181,  216,  215,  212,  211,  209,  207,  204,  203,  201,
 /*   390 */   200,  199,  197,  122,  194,   53,  193,   60,   98,  191,
 /*   400 */    76,  180,  179,  153,  175,  174,  172,  150,  196,    7,
 /*   410 */   138,  108,   82,   93,  216,  215,  212,  211,  209,  207,
 /*   420 */   204,  203,  201,  200,  199,  197,  122,  194,  159,  193,
 /*   430 */    60,  188,  191,   76,  180,  179,   49,  175,  174,  172,
 /*   440 */   356,  356,  356,  138,  108,   82,   61,  101,   50,   51,
 /*   450 */   356,   57,   16,  356,   28,  356,  171,  170,  168,  167,
 /*   460 */   166,  165,  356,  124,  127,    3,  224,  121,   20,  113,
 /*   470 */   110,   59,   90,   96,  106,   49,  356,  356,  104,   92,
 /*   480 */    83,   44,   39,   42,   40,   46,  356,   50,   51,  356,
 /*   490 */    57,   16,  356,   28,  356,  171,  170,  168,  167,  166,
 /*   500 */   165,   58,  124,  127,    3,  126,  121,   20,  113,  110,
 /*   510 */    59,   90,   96,  106,   49,  356,  151,  104,   92,   83,
 /*   520 */   100,   59,  214,  218,  111,  112,   50,   51,  356,   57,
 /*   530 */    16,  356,   28,  356,  171,  170,  168,  167,  166,  165,
 /*   540 */   356,  124,  127,    3,  356,  121,   20,  113,  110,   59,
 /*   550 */    90,   96,  106,  356,  356,  356,  104,   92,   83,   38,
 /*   560 */    36,   47,   48,   37,   33,   34,   35,   41,   43,   45,
 /*   570 */    44,   39,   42,   40,   46,  148,  147,  146,  145,  144,
 /*   580 */   143,   25,   36,   47,   48,   37,   33,   34,   35,   41,
 /*   590 */    43,   45,   44,   39,   42,   40,   46,   49,  184,   76,
 /*   600 */   180,  179,  356,  175,  174,  172,  356,  356,  356,   50,
 /*   610 */    51,  356,   57,   16,  356,   28,  356,  171,  170,  168,
 /*   620 */   167,  166,  165,   49,  124,  127,  171,  170,  168,  167,
 /*   630 */   166,  165,   59,  356,  356,   50,   51,  356,   84,   16,
 /*   640 */   356,   28,  356,  171,  170,  168,  167,  166,  165,  356,
 /*   650 */   124,  133,   47,   48,   37,   33,   34,   35,   41,   43,
 /*   660 */    45,   44,   39,   42,   40,   46,   49,  223,  356,  191,
 /*   670 */    76,  180,  179,  356,  175,  174,  172,  356,   50,   51,
 /*   680 */   356,   84,   16,  206,   28,  178,  171,  170,  168,  167,
 /*   690 */   166,  165,  356,  124,  122,  194,   49,  193,   60,  356,
 /*   700 */   191,   76,  180,  179,  356,  175,  119,  172,   50,   51,
 /*   710 */   356,   84,   16,  107,   28,   58,  171,  170,  168,  167,
 /*   720 */   166,  165,  356,  124,   27,  160,  194,  356,  193,   60,
 /*   730 */   149,  191,   76,  180,  179,   59,  175,  174,  172,   79,
 /*   740 */   158,  157,  194,  356,  193,   60,  356,  191,   76,  180,
 /*   750 */   179,   80,  175,  174,  172,  356,  356,  152,   48,   37,
 /*   760 */    33,   34,   35,   41,   43,   45,   44,   39,   42,   40,
 /*   770 */    46,  356,  157,  194,  356,  193,   60,  356,  191,   76,
 /*   780 */   180,  179,  356,  175,  174,  172,  160,  194,  156,  193,
 /*   790 */    60,  356,  191,   76,  180,  179,  356,  175,  174,  172,
 /*   800 */   356,  163,  356,   37,   33,   34,   35,   41,   43,   45,
 /*   810 */    44,   39,   42,   40,   46,  202,  194,  105,  193,   60,
 /*   820 */   356,  191,   76,  180,  179,  356,  175,  174,  172,   81,
 /*   830 */   194,  356,  193,   60,  356,  191,   76,  180,  179,  356,
 /*   840 */   175,  174,  172,  213,  194,  356,  193,   60,  356,  191,
 /*   850 */    76,  180,  179,  356,  175,  174,  172,  131,  194,  356,
 /*   860 */   193,   60,  356,  191,   76,  180,  179,  356,  175,  174,
 /*   870 */   172,   85,  194,  356,  193,   60,  356,  191,   76,  180,
 /*   880 */   179,  356,  175,  174,  172,   86,  194,  356,  193,   60,
 /*   890 */   356,  191,   76,  180,  179,  356,  175,  174,  172,  356,
 /*   900 */   102,  194,  356,  193,   60,  356,  191,   76,  180,  179,
 /*   910 */   356,  175,  174,  172,   94,  194,  356,  193,   60,  356,
 /*   920 */   191,   76,  180,  179,  356,  175,  174,  172,  118,  194,
 /*   930 */   356,  193,   60,  356,  191,   76,  180,  179,  356,  175,
 /*   940 */   174,  172,  115,  194,  356,  193,   60,  356,  191,   76,
 /*   950 */   180,  179,  356,  175,  174,  172,  103,  194,  356,  193,
 /*   960 */    60,  356,  191,   76,  180,  179,  356,  175,  174,  172,
 /*   970 */    99,  194,  356,  193,   60,  356,  191,   76,  180,  179,
 /*   980 */   356,  175,  174,  172,  141,  356,  193,   60,  356,  191,
 /*   990 */    76,  180,  179,  356,  175,  174,  172,  192,  356,  193,
 /*  1000 */    60,  356,  191,   76,  180,  179,  356,  175,  174,  172,
 /*  1010 */    72,  356,  191,   76,  180,  179,  356,  175,  174,  172,
 /*  1020 */    67,  356,  191,   76,  180,  179,  356,  175,  174,  172,
 /*  1030 */   356,  136,  356,  191,   76,  180,  179,  356,  175,  174,
 /*  1040 */   172,  356,   68,  356,  191,   76,  180,  179,  356,  175,
 /*  1050 */   174,  172,  139,  356,  191,   76,  180,  179,  356,  175,
 /*  1060 */   174,  172,   78,  356,  191,   76,  180,  179,  356,  175,
 /*  1070 */   174,  172,   70,  356,  191,   76,  180,  179,  356,  175,
 /*  1080 */   174,  172,   65,  356,  191,   76,  180,  179,  356,  175,
 /*  1090 */   174,  172,   71,  356,  191,   76,  180,  179,  356,  175,
 /*  1100 */   174,  172,   66,  356,  191,   76,  180,  179,  356,  175,
 /*  1110 */   174,  172,   69,  356,  191,   76,  180,  179,  356,  175,
 /*  1120 */   174,  172,   73,  356,  191,   76,  180,  179,  356,  175,
 /*  1130 */   174,  172,   74,  356,  191,   76,  180,  179,  356,  175,
 /*  1140 */   174,  172,   77,  356,  191,   76,  180,  179,  356,  175,
 /*  1150 */   174,  172,   75,  356,  191,   76,  180,  179,  356,  175,
 /*  1160 */   174,  172,  183,   76,  180,  179,  356,  175,  174,  172,
 /*  1170 */   182,   76,  180,  179,  356,  175,  174,  172,  187,  186,
 /*  1180 */    89,  356,   30,
};
static const YYCODETYPE yy_lookahead[] = {
 /*     0 */    60,   61,   62,   63,   64,   65,   66,   67,   68,   69,
 /*    10 */    70,   71,   72,   73,   74,   75,   76,   77,   78,   79,
 /*    20 */    28,   81,   82,   31,   84,   85,   86,   87,   41,   89,
 /*    30 */    90,   91,   98,   99,   60,   95,   96,   97,   64,   65,
 /*    40 */    66,   67,   68,   69,   70,   71,   72,   73,   74,   75,
 /*    50 */    76,   77,   78,   79,   41,   81,   82,   52,   84,   85,
 /*    60 */    86,   87,  109,   89,   90,   91,   14,   15,   16,   95,
 /*    70 */    96,   97,    6,    7,    8,    9,   10,   11,   12,   13,
 /*    80 */    14,   15,   16,   60,   98,   99,  112,   64,   65,   66,
 /*    90 */    67,   68,   69,   70,   71,   72,   73,   74,   75,   76,
 /*   100 */    77,   78,   79,   41,   81,   82,   30,   84,   85,   86,
 /*   110 */    87,   17,   89,   90,   91,   39,  110,  111,   95,   96,
 /*   120 */    97,    8,    9,   10,   11,   12,   13,   14,   15,   16,
 /*   130 */   104,  105,   60,  107,   39,  112,   64,   65,   66,   67,
 /*   140 */    68,   69,   70,   71,   72,   73,   74,   75,   76,   77,
 /*   150 */    78,   79,  105,   81,   82,   17,   84,   85,   86,   87,
 /*   160 */    28,   89,   90,   91,   32,   60,   32,   95,   96,   97,
 /*   170 */    65,   66,   67,   68,   69,   70,   71,   72,   73,   74,
 /*   180 */    75,   76,   77,   78,   79,   28,   81,   82,   99,   84,
 /*   190 */    85,   86,   87,   97,   89,   90,   91,  101,   32,   39,
 /*   200 */    95,   96,   97,   97,   60,   39,  100,  101,  103,   65,
 /*   210 */    66,   67,   68,   69,   70,   71,   72,   73,   74,   75,
 /*   220 */    76,   77,   78,   79,   28,   81,   82,   41,   84,   85,
 /*   230 */    86,   87,   89,   89,   90,   91,   83,   39,   41,   95,
 /*   240 */    96,   97,   28,   60,   41,   35,   41,  103,   65,   66,
 /*   250 */    67,   68,   69,   70,   71,   72,   73,   74,   75,   76,
 /*   260 */    77,   78,   79,   32,   81,   82,   31,   84,   85,   86,
 /*   270 */    87,   41,   89,   90,   91,   32,   60,   31,   95,   96,
 /*   280 */    97,   65,   66,   67,   68,   69,   70,   71,   72,   73,
 /*   290 */    74,   75,   76,   77,   78,   79,   31,   81,   82,   31,
 /*   300 */    84,   85,   86,   87,   41,   89,   90,   91,   32,   60,
 /*   310 */    32,   95,   96,   97,   65,   66,   67,   68,   69,   70,
 /*   320 */    71,   72,   73,   74,   75,   76,   77,   78,   79,   24,
 /*   330 */    81,   82,   24,   84,   85,   86,   87,   42,   89,   90,
 /*   340 */    91,   32,   60,   32,   95,   96,   97,   65,   66,   67,
 /*   350 */    68,   69,   70,   71,   72,   73,   74,   75,   76,   77,
 /*   360 */    78,   79,   28,   81,   82,   28,   84,   85,   86,   87,
 /*   370 */    31,   89,   90,   91,   24,   60,   31,   95,   96,   97,
 /*   380 */    65,   66,   67,   68,   69,   70,   71,   72,   73,   74,
 /*   390 */    75,   76,   77,   78,   79,   42,   81,   82,   28,   84,
 /*   400 */    85,   86,   87,   41,   89,   90,   91,   30,   60,   54,
 /*   410 */    95,   96,   97,   65,   66,   67,   68,   69,   70,   71,
 /*   420 */    72,   73,   74,   75,   76,   77,   78,   79,   28,   81,
 /*   430 */    82,   43,   84,   85,   86,   87,   13,   89,   90,   91,
 /*   440 */   113,  113,  113,   95,   96,   97,   58,   59,   25,   26,
 /*   450 */   113,   28,   29,  113,   31,  113,   33,   34,   35,   36,
 /*   460 */    37,   38,  113,   40,   41,   42,   43,   44,   45,   46,
 /*   470 */    47,   48,   49,   50,   51,   13,  113,  113,   55,   56,
 /*   480 */    57,   12,   13,   14,   15,   16,  113,   25,   26,  113,
 /*   490 */    28,   29,  113,   31,  113,   33,   34,   35,   36,   37,
 /*   500 */    38,   28,   40,   41,   42,   43,   44,   45,   46,   47,
 /*   510 */    48,   49,   50,   51,   13,  113,   43,   55,   56,   57,
 /*   520 */   102,   48,  104,  105,  106,  107,   25,   26,  113,   28,
 /*   530 */    29,  113,   31,  113,   33,   34,   35,   36,   37,   38,
 /*   540 */   113,   40,   41,   42,  113,   44,   45,   46,   47,   48,
 /*   550 */    49,   50,   51,  113,  113,  113,   55,   56,   57,    1,
 /*   560 */     2,    3,    4,    5,    6,    7,    8,    9,   10,   11,
 /*   570 */    12,   13,   14,   15,   16,   17,   18,   19,   20,   21,
 /*   580 */    22,   23,    2,    3,    4,    5,    6,    7,    8,    9,
 /*   590 */    10,   11,   12,   13,   14,   15,   16,   13,   84,   85,
 /*   600 */    86,   87,  113,   89,   90,   91,  113,  113,  113,   25,
 /*   610 */    26,  113,   28,   29,  113,   31,  113,   33,   34,   35,
 /*   620 */    36,   37,   38,   13,   40,   41,   33,   34,   35,   36,
 /*   630 */    37,   38,   48,  113,  113,   25,   26,  113,   28,   29,
 /*   640 */   113,   31,  113,   33,   34,   35,   36,   37,   38,  113,
 /*   650 */    40,   41,    3,    4,    5,    6,    7,    8,    9,   10,
 /*   660 */    11,   12,   13,   14,   15,   16,   13,   82,  113,   84,
 /*   670 */    85,   86,   87,  113,   89,   90,   91,  113,   25,   26,
 /*   680 */   113,   28,   29,   67,   31,   32,   33,   34,   35,   36,
 /*   690 */    37,   38,  113,   40,   78,   79,   13,   81,   82,  113,
 /*   700 */    84,   85,   86,   87,  113,   89,   90,   91,   25,   26,
 /*   710 */   113,   28,   29,   97,   31,   28,   33,   34,   35,   36,
 /*   720 */    37,   38,  113,   40,  108,   78,   79,  113,   81,   82,
 /*   730 */    43,   84,   85,   86,   87,   48,   89,   90,   91,   92,
 /*   740 */    93,   78,   79,  113,   81,   82,  113,   84,   85,   86,
 /*   750 */    87,   88,   89,   90,   91,  113,  113,   94,    4,    5,
 /*   760 */     6,    7,    8,    9,   10,   11,   12,   13,   14,   15,
 /*   770 */    16,  113,   78,   79,  113,   81,   82,  113,   84,   85,
 /*   780 */    86,   87,  113,   89,   90,   91,   78,   79,   94,   81,
 /*   790 */    82,  113,   84,   85,   86,   87,  113,   89,   90,   91,
 /*   800 */   113,   93,  113,    5,    6,    7,    8,    9,   10,   11,
 /*   810 */    12,   13,   14,   15,   16,   78,   79,   80,   81,   82,
 /*   820 */   113,   84,   85,   86,   87,  113,   89,   90,   91,   78,
 /*   830 */    79,  113,   81,   82,  113,   84,   85,   86,   87,  113,
 /*   840 */    89,   90,   91,   78,   79,  113,   81,   82,  113,   84,
 /*   850 */    85,   86,   87,  113,   89,   90,   91,   78,   79,  113,
 /*   860 */    81,   82,  113,   84,   85,   86,   87,  113,   89,   90,
 /*   870 */    91,   78,   79,  113,   81,   82,  113,   84,   85,   86,
 /*   880 */    87,  113,   89,   90,   91,   78,   79,  113,   81,   82,
 /*   890 */   113,   84,   85,   86,   87,  113,   89,   90,   91,  113,
 /*   900 */    78,   79,  113,   81,   82,  113,   84,   85,   86,   87,
 /*   910 */   113,   89,   90,   91,   78,   79,  113,   81,   82,  113,
 /*   920 */    84,   85,   86,   87,  113,   89,   90,   91,   78,   79,
 /*   930 */   113,   81,   82,  113,   84,   85,   86,   87,  113,   89,
 /*   940 */    90,   91,   78,   79,  113,   81,   82,  113,   84,   85,
 /*   950 */    86,   87,  113,   89,   90,   91,   78,   79,  113,   81,
 /*   960 */    82,  113,   84,   85,   86,   87,  113,   89,   90,   91,
 /*   970 */    78,   79,  113,   81,   82,  113,   84,   85,   86,   87,
 /*   980 */   113,   89,   90,   91,   79,  113,   81,   82,  113,   84,
 /*   990 */    85,   86,   87,  113,   89,   90,   91,   79,  113,   81,
 /*  1000 */    82,  113,   84,   85,   86,   87,  113,   89,   90,   91,
 /*  1010 */    82,  113,   84,   85,   86,   87,  113,   89,   90,   91,
 /*  1020 */    82,  113,   84,   85,   86,   87,  113,   89,   90,   91,
 /*  1030 */   113,   82,  113,   84,   85,   86,   87,  113,   89,   90,
 /*  1040 */    91,  113,   82,  113,   84,   85,   86,   87,  113,   89,
 /*  1050 */    90,   91,   82,  113,   84,   85,   86,   87,  113,   89,
 /*  1060 */    90,   91,   82,  113,   84,   85,   86,   87,  113,   89,
 /*  1070 */    90,   91,   82,  113,   84,   85,   86,   87,  113,   89,
 /*  1080 */    90,   91,   82,  113,   84,   85,   86,   87,  113,   89,
 /*  1090 */    90,   91,   82,  113,   84,   85,   86,   87,  113,   89,
 /*  1100 */    90,   91,   82,  113,   84,   85,   86,   87,  113,   89,
 /*  1110 */    90,   91,   82,  113,   84,   85,   86,   87,  113,   89,
 /*  1120 */    90,   91,   82,  113,   84,   85,   86,   87,  113,   89,
 /*  1130 */    90,   91,   82,  113,   84,   85,   86,   87,  113,   89,
 /*  1140 */    90,   91,   82,  113,   84,   85,   86,   87,  113,   89,
 /*  1150 */    90,   91,   82,  113,   84,   85,   86,   87,  113,   89,
 /*  1160 */    90,   91,   84,   85,   86,   87,  113,   89,   90,   91,
 /*  1170 */    84,   85,   86,   87,  113,   89,   90,   91,   25,   26,
 /*  1180 */    27,  113,   29,
};
#define YY_SHIFT_USE_DFLT (-14)
#define YY_SHIFT_MAX 124
static const short yy_shift_ofst[] = {
 /*     0 */   501,  501,  501,  423,  501,  501,  462,  501,  501,  501,
 /*    10 */   501,  501,  501,  501,  584,  653,  683,  683,  683,  683,
 /*    20 */   610,  683,  683,  683,  683,  683,  683,  683,  683,  683,
 /*    30 */   683,  683,  683,  683,  683,  683,  683,  683,  683,  683,
 /*    40 */   683,  683,  683,  683,  683,  683,  683,  683,  683,  683,
 /*    50 */   683,  683,  132,  687,  214,  388,  473,   -8,  196,  196,
 /*    60 */   558,  593,  196,  157,  -14,  580,  649,  754,  798,   66,
 /*    70 */   113,  113,  469,  469,  469,  469, 1153,   52,   52,   76,
 /*    80 */   166,  231,  230,  246,  268,  276,  305,  295,  160,  334,
 /*    90 */   337,  353,  345,  355,  377,  362,  370,  350,  339,  311,
 /*   100 */   309,  308,  278,  263,  265,  243,  235,  205,  203,   94,
 /*   110 */   197,  198,   95,  186,  160,  134,  138,   95,   62,    5,
 /*   120 */   -13,  210,   13,   94,  400,
};
#define YY_REDUCE_USE_DFLT (-67)
#define YY_REDUCE_MAX 64
static const short yy_reduce_ofst[] = {
 /*     0 */   -60,  -26,   23,   72,  144,  105,  183,  282,  216,  183,
 /*    10 */   315,  249,  183,  348,  616,  663,  647,  737,  694,  708,
 /*    20 */   850,  765,  751,  779,  793,  807,  822,  878,  892,  864,
 /*    30 */   836,  905,  918,  990, 1010, 1040, 1020, 1030, 1000,  980,
 /*    40 */   949,  928,  970, 1050, 1060, 1070,  585,  938,  960,  514,
 /*    50 */  1078, 1086,  418,  106,   26,    6,   96,  -66,  -66,  -14,
 /*    60 */   153,  143,   89,   47,  -47,
};
static const YYACTIONTYPE yy_default[] = {
 /*     0 */   229,  354,  354,  354,  354,  354,  354,  354,  354,  230,
 /*    10 */   354,  354,  353,  354,  354,  354,  354,  245,  354,  354,
 /*    20 */   354,  354,  354,  354,  354,  354,  354,  354,  354,  354,
 /*    30 */   354,  354,  354,  354,  354,  354,  354,  354,  354,  354,
 /*    40 */   354,  354,  354,  354,  354,  354,  354,  354,  354,  354,
 /*    50 */   354,  354,  354,  354,  354,  354,  354,  296,  354,  354,
 /*    60 */   255,  354,  354,  354,  348,  258,  259,  260,  261,  262,
 /*    70 */   263,  264,  266,  265,  267,  268,  274,  269,  270,  354,
 /*    80 */   354,  354,  354,  354,  296,  354,  354,  354,  317,  354,
 /*    90 */   354,  354,  354,  344,  354,  354,  354,  354,  354,  354,
 /*   100 */   354,  354,  354,  354,  354,  354,  354,  354,  354,  330,
 /*   110 */   354,  336,  337,  354,  318,  354,  319,  338,  354,  287,
 /*   120 */   354,  354,  354,  354,  354,  306,  308,  305,  321,  309,
 /*   130 */   322,  320,  310,  311,  225,  312,  272,  313,  314,  271,
 /*   140 */   315,  256,  316,  254,  253,  252,  251,  250,  249,  323,
 /*   150 */   283,  324,  303,  325,  327,  326,  304,  302,  298,  301,
 /*   160 */   300,  328,  339,  299,  297,  295,  294,  293,  292,  343,
 /*   170 */   291,  290,  289,  288,  287,  286,  345,  285,  284,  279,
 /*   180 */   278,  346,  277,  276,  275,  282,  281,  280,  347,  349,
 /*   190 */   350,  257,  248,  247,  244,  351,  243,  242,  352,  241,
 /*   200 */   240,  239,  246,  238,  237,  340,  341,  236,  342,  235,
 /*   210 */   329,  234,  233,  331,  332,  232,  231,  333,  334,  228,
 /*   220 */   227,  226,  335,  273,  307,
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
  "IDENTIFIER",    "LBRACKET",      "RBRACKET",      "LPAREN",      
  "RPAREN",        "INTEGER",       "REAL",          "STRING",      
  "TRUE",          "FALSE",         "NULL",          "COMMA",       
  "NEW",           "SEMICOLON",     "LBRACE",        "RBRACE",      
  "INCLUDE",       "RETURN",        "BREAK",         "CONTINUE",    
  "VAR",           "STRUCT",        "FUNCTION",      "FOR",         
  "IN",            "LOWER_THAN_ELSE",  "ELSE",          "IF",          
  "WHILE",         "SWITCH",        "CASE",          "DEFAULT",     
  "error",         "main",          "translation_unit",  "statement_sequence_opt",
  "statement_sequence",  "statement",     "include_statement",  "expression_statement",
  "declaration_statement",  "for_statement",  "compound_statement",  "if_statement",
  "while_statement",  "foreach_statement",  "return_statement",  "switch_statement",
  "break_statement",  "continue_statement",  "expression",    "assignment_expression",
  "expression_opt",  "conditional_expression",  "binary_expression",  "assignment_operator",
  "unary_expression",  "postfix_expression",  "new_expression",  "primary_expression",
  "argument_list",  "literal",       "id_expression",  "list_literal",
  "list_content",  "list_entry",    "argument",      "function_declaration",
  "struct_declaration",  "variable_declaration",  "declarator_sequence",  "declarator",  
  "struct_members",  "struct_member",  "parameter_list",  "function_body",
  "parameter",     "opt_parameter",  "parameters",    "opt_parameters",
  "for_init_statement",  "switch_body",   "switch_case",   "default_case",
  "case_statements",
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
 /*  53 */ "unary_expression ::= new_expression",
 /*  54 */ "postfix_expression ::= primary_expression",
 /*  55 */ "postfix_expression ::= postfix_expression ADDADD",
 /*  56 */ "postfix_expression ::= postfix_expression SUBSUB",
 /*  57 */ "postfix_expression ::= postfix_expression DOT IDENTIFIER",
 /*  58 */ "postfix_expression ::= postfix_expression LBRACKET expression RBRACKET",
 /*  59 */ "postfix_expression ::= IDENTIFIER LPAREN RPAREN",
 /*  60 */ "postfix_expression ::= IDENTIFIER LPAREN argument_list RPAREN",
 /*  61 */ "primary_expression ::= literal",
 /*  62 */ "primary_expression ::= id_expression",
 /*  63 */ "primary_expression ::= LPAREN expression RPAREN",
 /*  64 */ "primary_expression ::= list_literal",
 /*  65 */ "literal ::= INTEGER",
 /*  66 */ "literal ::= REAL",
 /*  67 */ "literal ::= STRING",
 /*  68 */ "literal ::= TRUE",
 /*  69 */ "literal ::= FALSE",
 /*  70 */ "literal ::= NULL",
 /*  71 */ "id_expression ::= IDENTIFIER",
 /*  72 */ "list_literal ::= LBRACKET list_content RBRACKET",
 /*  73 */ "list_content ::= list_entry",
 /*  74 */ "list_content ::= list_content COMMA list_entry",
 /*  75 */ "list_entry ::= expression",
 /*  76 */ "new_expression ::= NEW IDENTIFIER",
 /*  77 */ "argument ::= expression",
 /*  78 */ "argument_list ::= argument",
 /*  79 */ "argument_list ::= argument_list COMMA argument",
 /*  80 */ "expression_statement ::= SEMICOLON",
 /*  81 */ "expression_statement ::= expression SEMICOLON",
 /*  82 */ "compound_statement ::= LBRACE RBRACE",
 /*  83 */ "compound_statement ::= LBRACE statement_sequence RBRACE",
 /*  84 */ "include_statement ::= INCLUDE STRING SEMICOLON",
 /*  85 */ "return_statement ::= RETURN expression SEMICOLON",
 /*  86 */ "return_statement ::= RETURN SEMICOLON",
 /*  87 */ "break_statement ::= BREAK SEMICOLON",
 /*  88 */ "continue_statement ::= CONTINUE SEMICOLON",
 /*  89 */ "declaration_statement ::= function_declaration",
 /*  90 */ "declaration_statement ::= struct_declaration SEMICOLON",
 /*  91 */ "declaration_statement ::= variable_declaration SEMICOLON",
 /*  92 */ "variable_declaration ::= VAR declarator_sequence",
 /*  93 */ "variable_declaration ::= IDENTIFIER declarator_sequence",
 /*  94 */ "declarator ::= IDENTIFIER",
 /*  95 */ "declarator ::= IDENTIFIER ASSIGN expression",
 /*  96 */ "declarator_sequence ::= declarator",
 /*  97 */ "declarator_sequence ::= declarator_sequence COMMA declarator",
 /*  98 */ "struct_declaration ::= STRUCT IDENTIFIER LBRACE RBRACE",
 /*  99 */ "struct_declaration ::= STRUCT IDENTIFIER LBRACE struct_members RBRACE",
 /* 100 */ "struct_member ::= variable_declaration SEMICOLON",
 /* 101 */ "struct_members ::= struct_member",
 /* 102 */ "struct_members ::= struct_members struct_member",
 /* 103 */ "function_declaration ::= FUNCTION IDENTIFIER LPAREN parameter_list RPAREN function_body",
 /* 104 */ "function_declaration ::= FUNCTION IDENTIFIER LPAREN RPAREN function_body",
 /* 105 */ "parameter ::= IDENTIFIER",
 /* 106 */ "opt_parameter ::= IDENTIFIER ASSIGN expression",
 /* 107 */ "parameters ::= parameter",
 /* 108 */ "parameters ::= parameters COMMA parameter",
 /* 109 */ "opt_parameters ::= opt_parameter",
 /* 110 */ "opt_parameters ::= opt_parameters COMMA opt_parameter",
 /* 111 */ "parameter_list ::= parameters",
 /* 112 */ "parameter_list ::= opt_parameters",
 /* 113 */ "parameter_list ::= parameters COMMA opt_parameters",
 /* 114 */ "function_body ::= statement",
 /* 115 */ "for_statement ::= FOR LPAREN for_init_statement expression SEMICOLON expression_opt RPAREN statement",
 /* 116 */ "for_init_statement ::= expression_statement",
 /* 117 */ "for_init_statement ::= variable_declaration SEMICOLON",
 /* 118 */ "foreach_statement ::= FOR LPAREN id_expression IN expression RPAREN statement",
 /* 119 */ "if_statement ::= IF LPAREN expression RPAREN statement",
 /* 120 */ "if_statement ::= IF LPAREN expression RPAREN statement ELSE statement",
 /* 121 */ "while_statement ::= WHILE LPAREN expression RPAREN statement",
 /* 122 */ "switch_statement ::= SWITCH LPAREN expression RPAREN LBRACE switch_body RBRACE",
 /* 123 */ "switch_body ::=",
 /* 124 */ "switch_body ::= switch_body switch_case",
 /* 125 */ "switch_body ::= switch_body default_case",
 /* 126 */ "switch_case ::= CASE literal COLON case_statements",
 /* 127 */ "default_case ::= DEFAULT COLON case_statements",
 /* 128 */ "case_statements ::= statement_sequence",
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
  { 61, 1 },
  { 62, 1 },
  { 64, 1 },
  { 64, 2 },
  { 63, 0 },
  { 63, 1 },
  { 65, 1 },
  { 65, 1 },
  { 65, 1 },
  { 65, 1 },
  { 65, 1 },
  { 65, 1 },
  { 65, 1 },
  { 65, 1 },
  { 65, 1 },
  { 65, 1 },
  { 65, 1 },
  { 65, 1 },
  { 65, 1 },
  { 78, 1 },
  { 80, 0 },
  { 80, 1 },
  { 79, 1 },
  { 79, 3 },
  { 83, 1 },
  { 83, 1 },
  { 83, 1 },
  { 83, 1 },
  { 83, 1 },
  { 83, 1 },
  { 81, 1 },
  { 81, 5 },
  { 82, 1 },
  { 82, 3 },
  { 82, 3 },
  { 82, 3 },
  { 82, 3 },
  { 82, 3 },
  { 82, 3 },
  { 82, 3 },
  { 82, 3 },
  { 82, 3 },
  { 82, 3 },
  { 82, 3 },
  { 82, 3 },
  { 82, 3 },
  { 82, 3 },
  { 82, 3 },
  { 82, 3 },
  { 84, 1 },
  { 84, 2 },
  { 84, 2 },
  { 84, 2 },
  { 84, 1 },
  { 85, 1 },
  { 85, 2 },
  { 85, 2 },
  { 85, 3 },
  { 85, 4 },
  { 85, 3 },
  { 85, 4 },
  { 87, 1 },
  { 87, 1 },
  { 87, 3 },
  { 87, 1 },
  { 89, 1 },
  { 89, 1 },
  { 89, 1 },
  { 89, 1 },
  { 89, 1 },
  { 89, 1 },
  { 90, 1 },
  { 91, 3 },
  { 92, 1 },
  { 92, 3 },
  { 93, 1 },
  { 86, 2 },
  { 94, 1 },
  { 88, 1 },
  { 88, 3 },
  { 67, 1 },
  { 67, 2 },
  { 70, 2 },
  { 70, 3 },
  { 66, 3 },
  { 74, 3 },
  { 74, 2 },
  { 76, 2 },
  { 77, 2 },
  { 68, 1 },
  { 68, 2 },
  { 68, 2 },
  { 97, 2 },
  { 97, 2 },
  { 99, 1 },
  { 99, 3 },
  { 98, 1 },
  { 98, 3 },
  { 96, 4 },
  { 96, 5 },
  { 101, 2 },
  { 100, 1 },
  { 100, 2 },
  { 95, 6 },
  { 95, 5 },
  { 104, 1 },
  { 105, 3 },
  { 106, 1 },
  { 106, 3 },
  { 107, 1 },
  { 107, 3 },
  { 102, 1 },
  { 102, 1 },
  { 102, 3 },
  { 103, 1 },
  { 69, 8 },
  { 108, 1 },
  { 108, 2 },
  { 73, 7 },
  { 71, 5 },
  { 71, 7 },
  { 72, 5 },
  { 75, 7 },
  { 109, 0 },
  { 109, 2 },
  { 109, 2 },
  { 110, 4 },
  { 111, 3 },
  { 112, 1 },
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
{ p->SetRoot(yymsp[0].minor.yy81); }
#line 1151 "astgen.c"
        break;
      case 1:
#line 75 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(translation_unit, yymsp[0].minor.yy81); }
#line 1156 "astgen.c"
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
      case 54:
      case 61:
      case 62:
      case 64:
      case 78:
      case 89:
      case 92:
      case 96:
      case 101:
      case 107:
      case 109:
      case 111:
      case 112:
      case 114:
      case 116:
      case 128:
#line 78 "astgen.in"
{ yygotominor.yy81 = yymsp[0].minor.yy81; }
#line 1197 "astgen.c"
        break;
      case 3:
#line 79 "astgen.in"
{ 
  if(yymsp[-1].minor.yy81->m_type == statement_sequence) {
    yygotominor.yy81 = yymsp[-1].minor.yy81;
  }
  else {
    yygotominor.yy81 = p->AllocAst(statement_sequence, new AstList);
    yygotominor.yy81->m_a1.GetList()->push_back(yymsp[-1].minor.yy81);
  }
  yygotominor.yy81->m_a1.GetList()->push_back(yymsp[0].minor.yy81);
}
#line 1211 "astgen.c"
        break;
      case 4:
      case 20:
#line 92 "astgen.in"
{ yygotominor.yy81 = 0; }
#line 1217 "astgen.c"
        break;
      case 18:
      case 80:
#line 110 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(empty_statement); }
#line 1223 "astgen.c"
        break;
      case 23:
#line 126 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(assignment_expression, yymsp[-1].minor.yy38, yymsp[-2].minor.yy81, yymsp[0].minor.yy81); }
#line 1228 "astgen.c"
        break;
      case 24:
#line 130 "astgen.in"
{ yygotominor.yy38 = op_assign; }
#line 1233 "astgen.c"
        break;
      case 25:
#line 131 "astgen.in"
{ yygotominor.yy38 = op_assadd; }
#line 1238 "astgen.c"
        break;
      case 26:
#line 132 "astgen.in"
{ yygotominor.yy38 = op_asssub; }
#line 1243 "astgen.c"
        break;
      case 27:
#line 133 "astgen.in"
{ yygotominor.yy38 = op_assmul; }
#line 1248 "astgen.c"
        break;
      case 28:
#line 134 "astgen.in"
{ yygotominor.yy38 = op_assdiv; }
#line 1253 "astgen.c"
        break;
      case 29:
#line 135 "astgen.in"
{ yygotominor.yy38 = op_assmod; }
#line 1258 "astgen.c"
        break;
      case 31:
#line 139 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(ternary_expression, yymsp[-4].minor.yy81, yymsp[-2].minor.yy81, yymsp[0].minor.yy81); }
#line 1263 "astgen.c"
        break;
      case 33:
#line 143 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(binary_expression, op_logor,   yymsp[-2].minor.yy81, yymsp[0].minor.yy81); }
#line 1268 "astgen.c"
        break;
      case 34:
#line 144 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(binary_expression, op_logand,  yymsp[-2].minor.yy81, yymsp[0].minor.yy81); }
#line 1273 "astgen.c"
        break;
      case 35:
#line 145 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(binary_expression, op_bitor,   yymsp[-2].minor.yy81, yymsp[0].minor.yy81); }
#line 1278 "astgen.c"
        break;
      case 36:
#line 146 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(binary_expression, op_bitxor,  yymsp[-2].minor.yy81, yymsp[0].minor.yy81); }
#line 1283 "astgen.c"
        break;
      case 37:
#line 147 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(binary_expression, op_bitand,  yymsp[-2].minor.yy81, yymsp[0].minor.yy81); }
#line 1288 "astgen.c"
        break;
      case 38:
#line 148 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(binary_expression, op_eq,   yymsp[-2].minor.yy81, yymsp[0].minor.yy81); }
#line 1293 "astgen.c"
        break;
      case 39:
#line 149 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(binary_expression, op_ne,   yymsp[-2].minor.yy81, yymsp[0].minor.yy81); }
#line 1298 "astgen.c"
        break;
      case 40:
#line 150 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(binary_expression, op_lt,   yymsp[-2].minor.yy81, yymsp[0].minor.yy81); }
#line 1303 "astgen.c"
        break;
      case 41:
#line 151 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(binary_expression, op_le,   yymsp[-2].minor.yy81, yymsp[0].minor.yy81); }
#line 1308 "astgen.c"
        break;
      case 42:
#line 152 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(binary_expression, op_gt,   yymsp[-2].minor.yy81, yymsp[0].minor.yy81); }
#line 1313 "astgen.c"
        break;
      case 43:
#line 153 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(binary_expression, op_ge,   yymsp[-2].minor.yy81, yymsp[0].minor.yy81); }
#line 1318 "astgen.c"
        break;
      case 44:
#line 154 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(binary_expression, op_add,  yymsp[-2].minor.yy81, yymsp[0].minor.yy81); }
#line 1323 "astgen.c"
        break;
      case 45:
#line 155 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(binary_expression, op_sub,  yymsp[-2].minor.yy81, yymsp[0].minor.yy81); }
#line 1328 "astgen.c"
        break;
      case 46:
#line 156 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(binary_expression, op_mul,  yymsp[-2].minor.yy81, yymsp[0].minor.yy81); }
#line 1333 "astgen.c"
        break;
      case 47:
#line 157 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(binary_expression, op_div,  yymsp[-2].minor.yy81, yymsp[0].minor.yy81); }
#line 1338 "astgen.c"
        break;
      case 48:
#line 158 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(binary_expression, op_mod,  yymsp[-2].minor.yy81, yymsp[0].minor.yy81); }
#line 1343 "astgen.c"
        break;
      case 50:
#line 162 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(prefix_expression, op_negate, yymsp[0].minor.yy81); }
#line 1348 "astgen.c"
        break;
      case 51:
#line 163 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(prefix_expression, op_preinc, yymsp[0].minor.yy81); }
#line 1353 "astgen.c"
        break;
      case 52:
#line 164 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(prefix_expression, op_predec, yymsp[0].minor.yy81); }
#line 1358 "astgen.c"
        break;
      case 55:
#line 169 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(postfix_expression, op_postinc, yymsp[-1].minor.yy81); }
#line 1363 "astgen.c"
        break;
      case 56:
#line 170 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(postfix_expression, op_postdec, yymsp[-1].minor.yy81); }
#line 1368 "astgen.c"
        break;
      case 57:
#line 171 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(member_expression, yymsp[-2].minor.yy81, String(yymsp[0].minor.yy0)); }
#line 1373 "astgen.c"
        break;
      case 58:
#line 172 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(index_expression, yymsp[-3].minor.yy81, yymsp[-1].minor.yy81); }
#line 1378 "astgen.c"
        break;
      case 59:
#line 173 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(function_call, String(yymsp[-2].minor.yy0)); }
#line 1383 "astgen.c"
        break;
      case 60:
#line 174 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(function_call, String(yymsp[-3].minor.yy0), yymsp[-1].minor.yy81); }
#line 1388 "astgen.c"
        break;
      case 63:
      case 90:
      case 91:
      case 100:
      case 117:
#line 179 "astgen.in"
{ yygotominor.yy81 = yymsp[-1].minor.yy81; }
#line 1397 "astgen.c"
        break;
      case 65:
#line 183 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(literal, Variant(String(yymsp[0].minor.yy0), Variant::stInt)); }
#line 1402 "astgen.c"
        break;
      case 66:
#line 184 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(literal, Variant(String(yymsp[0].minor.yy0), Variant::stReal)); }
#line 1407 "astgen.c"
        break;
      case 67:
#line 185 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(literal, Variant(String(yymsp[0].minor.yy0), Variant::stString)); }
#line 1412 "astgen.c"
        break;
      case 68:
#line 186 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(literal, Variant(true));    }
#line 1417 "astgen.c"
        break;
      case 69:
#line 187 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(literal, Variant(false));   }
#line 1422 "astgen.c"
        break;
      case 70:
#line 188 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(literal, Variant());        }
#line 1427 "astgen.c"
        break;
      case 71:
#line 191 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(lvalue, String(yymsp[0].minor.yy0)); }
#line 1432 "astgen.c"
        break;
      case 72:
#line 194 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(list_literal, yymsp[-1].minor.yy81); }
#line 1437 "astgen.c"
        break;
      case 73:
#line 195 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(list_content, yymsp[0].minor.yy81); }
#line 1442 "astgen.c"
        break;
      case 74:
#line 196 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(list_content, yymsp[-2].minor.yy81, yymsp[0].minor.yy81); }
#line 1447 "astgen.c"
        break;
      case 75:
#line 197 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(list_entry, yymsp[0].minor.yy81); }
#line 1452 "astgen.c"
        break;
      case 76:
#line 200 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(new_expression, String(yymsp[0].minor.yy0)); }
#line 1457 "astgen.c"
        break;
      case 77:
#line 209 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(argument, yymsp[0].minor.yy81); }
#line 1462 "astgen.c"
        break;
      case 79:
#line 213 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(argument_list, yymsp[-2].minor.yy81, yymsp[0].minor.yy81); }
#line 1467 "astgen.c"
        break;
      case 81:
#line 222 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(expression_statement, yymsp[-1].minor.yy81); }
#line 1472 "astgen.c"
        break;
      case 82:
#line 225 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(compound_statement); }
#line 1477 "astgen.c"
        break;
      case 83:
#line 226 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(compound_statement, yymsp[-1].minor.yy81); }
#line 1482 "astgen.c"
        break;
      case 84:
#line 229 "astgen.in"
{ p->Parse(yymsp[-1].minor.yy0); yygotominor.yy81 = p->GetRoot(); }
#line 1487 "astgen.c"
        break;
      case 85:
#line 232 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(return_statement, yymsp[-1].minor.yy81); }
#line 1492 "astgen.c"
        break;
      case 86:
#line 233 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(return_statement);    }
#line 1497 "astgen.c"
        break;
      case 87:
#line 236 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(break_statement); }
#line 1502 "astgen.c"
        break;
      case 88:
#line 237 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(continue_statement); }
#line 1507 "astgen.c"
        break;
      case 93:
#line 250 "astgen.in"
{ yygotominor.yy81 = yymsp[0].minor.yy81; yygotominor.yy81->m_a3 = String(yymsp[-1].minor.yy0); }
#line 1512 "astgen.c"
        break;
      case 94:
#line 252 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(variable_declaration, String(yymsp[0].minor.yy0));    }
#line 1517 "astgen.c"
        break;
      case 95:
#line 253 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(variable_declaration, String(yymsp[-2].minor.yy0), yymsp[0].minor.yy81); }
#line 1522 "astgen.c"
        break;
      case 97:
#line 256 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(declaration_sequence, yymsp[-2].minor.yy81, yymsp[0].minor.yy81); }
#line 1527 "astgen.c"
        break;
      case 98:
#line 265 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(struct_declaration, String(yymsp[-2].minor.yy0)); }
#line 1532 "astgen.c"
        break;
      case 99:
#line 266 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(struct_declaration, String(yymsp[-3].minor.yy0), yymsp[-1].minor.yy81); }
#line 1537 "astgen.c"
        break;
      case 102:
#line 273 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(struct_members, yymsp[-1].minor.yy81, yymsp[0].minor.yy81); }
#line 1542 "astgen.c"
        break;
      case 103:
#line 281 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(function_declaration, String(yymsp[-4].minor.yy0), yymsp[-2].minor.yy81, yymsp[0].minor.yy81); }
#line 1547 "astgen.c"
        break;
      case 104:
#line 282 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(function_declaration, String(yymsp[-3].minor.yy0), AstData(), yymsp[0].minor.yy81); }
#line 1552 "astgen.c"
        break;
      case 105:
#line 285 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(parameter, String(yymsp[0].minor.yy0)); }
#line 1557 "astgen.c"
        break;
      case 106:
#line 288 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(parameter, String(yymsp[-2].minor.yy0), yymsp[0].minor.yy81); }
#line 1562 "astgen.c"
        break;
      case 108:
      case 110:
      case 113:
#line 292 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(parameter_list, yymsp[-2].minor.yy81, yymsp[0].minor.yy81); }
#line 1569 "astgen.c"
        break;
      case 115:
#line 313 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(for_statement, yymsp[-5].minor.yy81, yymsp[-4].minor.yy81, yymsp[-2].minor.yy81, yymsp[0].minor.yy81); }
#line 1574 "astgen.c"
        break;
      case 118:
#line 324 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(foreach_statement, yymsp[-4].minor.yy81, yymsp[-2].minor.yy81, yymsp[0].minor.yy81); }
#line 1579 "astgen.c"
        break;
      case 119:
#line 335 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(if_statement, yymsp[-2].minor.yy81, yymsp[0].minor.yy81); }
#line 1584 "astgen.c"
        break;
      case 120:
#line 336 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(if_statement, yymsp[-4].minor.yy81, yymsp[-2].minor.yy81, yymsp[0].minor.yy81); }
#line 1589 "astgen.c"
        break;
      case 121:
#line 344 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(while_statement, yymsp[-2].minor.yy81,  yymsp[0].minor.yy81); }
#line 1594 "astgen.c"
        break;
      case 122:
#line 352 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(switch_statement, yymsp[-4].minor.yy81, yymsp[-1].minor.yy223); }
#line 1599 "astgen.c"
        break;
      case 123:
#line 356 "astgen.in"
{ yygotominor.yy223 = new AstList; }
#line 1604 "astgen.c"
        break;
      case 124:
      case 125:
#line 357 "astgen.in"
{ yygotominor.yy223 = yymsp[-1].minor.yy223; yygotominor.yy223->push_back(yymsp[0].minor.yy81); }
#line 1610 "astgen.c"
        break;
      case 126:
#line 361 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(switch_case, yymsp[-2].minor.yy81, yymsp[0].minor.yy81); }
#line 1615 "astgen.c"
        break;
      case 127:
#line 364 "astgen.in"
{ yygotominor.yy81 = p->AllocAst(default_case, yymsp[0].minor.yy81); }
#line 1620 "astgen.c"
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
#line 1668 "astgen.c"
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
#line 1686 "astgen.c"
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

