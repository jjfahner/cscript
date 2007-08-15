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
#define YYNOCODE 117
#define YYACTIONTYPE unsigned short int
#define CScriptParseTOKENTYPE  Token 
typedef union {
  CScriptParseTOKENTYPE yy0;
  AstList* yy39;
  Ast* yy71;
  opcodes yy206;
  int yy233;
} YYMINORTYPE;
#ifndef YYSTACKDEPTH
#define YYSTACKDEPTH 100
#endif
#define CScriptParseARG_SDECL  Parser* p ;
#define CScriptParseARG_PDECL , Parser* p 
#define CScriptParseARG_FETCH  Parser* p  = yypParser->p 
#define CScriptParseARG_STORE yypParser->p  = p 
#define YYNSTATE 225
#define YYNRULE 131
#define YYERRORSYMBOL 63
#define YYERRSYMDT yy233
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
 /*     0 */   196,  357,  223,  221,    8,  220,  216,  215,  212,  211,
 /*    10 */   209,  207,  204,  203,  201,  200,  199,  197,  117,  194,
 /*    20 */   124,  193,   58,  188,  191,   74,  180,  179,   49,  175,
 /*    30 */   174,  172,   38,   35,   43,  133,   83,   79,   59,   98,
 /*    40 */    50,   51,  149,  101,   16,    7,   23,   57,  171,  170,
 /*    50 */   168,  167,  166,  165,  164,  163,  160,   24,   92,  125,
 /*    60 */     3,  224,  121,   20,  113,  111,   57,   87,   91,   99,
 /*    70 */    49,  127,  151,  109,  107,   96,   95,   57,  214,  218,
 /*    80 */   108,  112,   50,   51,   55,  101,   16,   61,   23,  222,
 /*    90 */   171,  170,  168,  167,  166,  165,  164,  163,  160,  173,
 /*   100 */    92,  125,    3,  122,  121,   20,  113,  111,   57,   87,
 /*   110 */    91,   99,  189,  190,  196,  109,  107,   96,   10,  220,
 /*   120 */   216,  215,  212,  211,  209,  207,  204,  203,  201,  200,
 /*   130 */   199,  197,  117,  194,  119,  193,   58,  177,  191,   74,
 /*   140 */   180,  179,   90,  175,  174,  172,  154,   19,  130,  133,
 /*   150 */    83,   79,   39,   41,   45,   47,   46,   44,   42,   37,
 /*   160 */    38,   35,   43,  132,  196,  144,  195,    9,   10,  220,
 /*   170 */   216,  215,  212,  211,  209,  207,  204,  203,  201,  200,
 /*   180 */   199,  197,  117,  194,  159,  193,   58,   85,  191,   74,
 /*   190 */   180,  179,   21,  175,  174,  172,   18,   49,   54,  133,
 /*   200 */    83,   79,   42,   37,   38,   35,   43,  106,   94,   50,
 /*   210 */    51,    5,  101,   16,   32,   23,  198,  171,  170,  168,
 /*   220 */   167,  166,  165,  164,  163,  160,   26,   92,  125,    3,
 /*   230 */   106,  121,   20,  113,  111,   57,   87,   91,   99,   80,
 /*   240 */   145,  196,  109,  107,   96,    6,  220,  216,  215,  212,
 /*   250 */   211,  209,  207,  204,  203,  201,  200,  199,  197,  117,
 /*   260 */   194,   13,  193,   58,  123,  191,   74,  180,  179,  208,
 /*   270 */   175,  174,  172,   27,  196,  137,  133,   83,   79,  162,
 /*   280 */   216,  215,  212,  211,  209,  207,  204,  203,  201,  200,
 /*   290 */   199,  197,  117,  194,   60,  193,   58,   11,  191,   74,
 /*   300 */   180,  179,   31,  175,  174,  172,   15,   62,   17,  133,
 /*   310 */    83,   79,  135,  196,   14,  185,    2,  161,  162,  216,
 /*   320 */   215,  212,  211,  209,  207,  204,  203,  201,  200,  199,
 /*   330 */   197,  117,  194,   28,  193,   58,   12,  191,   74,  180,
 /*   340 */   179,   82,  175,  174,  172,   25,   89,    4,  133,   83,
 /*   350 */    79,   90,  196,  146,   56,  155,  210,  176,  216,  215,
 /*   360 */   212,  211,  209,  207,  204,  203,  201,  200,  199,  197,
 /*   370 */   117,  194,    1,  193,   58,   53,  191,   74,  180,  179,
 /*   380 */    52,  175,  174,  172,  153,  196,  156,  133,   83,   79,
 /*   390 */   219,  216,  215,  212,  211,  209,  207,  204,  203,  201,
 /*   400 */   200,  199,  197,  117,  194,   93,  193,   58,  118,  191,
 /*   410 */    74,  180,  179,  358,  175,  174,  172,  358,  358,  358,
 /*   420 */   133,   83,   79,  196,  358,  358,  358,  358,  205,  216,
 /*   430 */   215,  212,  211,  209,  207,  204,  203,  201,  200,  199,
 /*   440 */   197,  117,  194,  358,  193,   58,  358,  191,   74,  180,
 /*   450 */   179,  358,  175,  174,  172,  358,  196,  358,  133,   83,
 /*   460 */    79,  110,  216,  215,  212,  211,  209,  207,  204,  203,
 /*   470 */   201,  200,  199,  197,  117,  194,  358,  193,   58,  358,
 /*   480 */   191,   74,  180,  179,  358,  175,  174,  172,  358,  358,
 /*   490 */   358,  133,   83,   79,  196,  217,  218,  358,  114,  181,
 /*   500 */   216,  215,  212,  211,  209,  207,  204,  203,  201,  200,
 /*   510 */   199,  197,  117,  194,  358,  193,   58,  358,  191,   74,
 /*   520 */   180,  179,  358,  175,  174,  172,  358,  196,  358,  133,
 /*   530 */    83,   79,  169,  216,  215,  212,  211,  209,  207,  204,
 /*   540 */   203,  201,  200,  199,  197,  117,  194,  358,  193,   58,
 /*   550 */   358,  191,   74,  180,  179,  358,  175,  174,  172,  358,
 /*   560 */   358,  358,  133,   83,   79,   40,   33,   34,   48,   36,
 /*   570 */    39,   41,   45,   47,   46,   44,   42,   37,   38,   35,
 /*   580 */    43,  142,  141,  140,  139,  138,  136,   29,   49,   45,
 /*   590 */    47,   46,   44,   42,   37,   38,   35,   43,  358,  358,
 /*   600 */    50,   51,  358,  101,   16,  358,   23,  358,  171,  170,
 /*   610 */   168,  167,  166,  165,  164,  163,  160,   49,   92,  125,
 /*   620 */   187,  186,   84,  358,   30,  358,   57,  358,  358,   50,
 /*   630 */    51,  358,  101,   16,  358,   23,  178,  171,  170,  168,
 /*   640 */   167,  166,  165,  164,  163,  160,   49,   92,  171,  170,
 /*   650 */   168,  167,  166,  165,  164,  163,  160,  358,   50,   51,
 /*   660 */   358,  101,   16,  358,   23,  358,  171,  170,  168,  167,
 /*   670 */   166,  165,  164,  163,  160,   49,   92,  128,  183,   74,
 /*   680 */   180,  179,  358,  175,  174,  172,  358,   50,   51,  358,
 /*   690 */   101,   16,  358,   23,  358,  171,  170,  168,  167,  166,
 /*   700 */   165,  164,  163,  160,  358,   92,   33,   34,   48,   36,
 /*   710 */    39,   41,   45,   47,   46,   44,   42,   37,   38,   35,
 /*   720 */    43,   34,   48,   36,   39,   41,   45,   47,   46,   44,
 /*   730 */    42,   37,   38,   35,   43,  206,   73,  358,  191,   74,
 /*   740 */   180,  179,  358,  175,  174,  172,  117,  194,  358,  193,
 /*   750 */    58,  358,  191,   74,  180,  179,  358,  175,  103,  172,
 /*   760 */   358,  182,   74,  180,  179,  104,  175,  174,  172,  358,
 /*   770 */   358,  358,  358,  358,  358,  358,   22,  150,  194,  358,
 /*   780 */   193,   58,  358,  191,   74,  180,  179,   77,  175,  174,
 /*   790 */   172,  157,  194,  147,  193,   58,  358,  191,   74,  180,
 /*   800 */   179,  358,  175,  174,  172,   78,  152,   48,   36,   39,
 /*   810 */    41,   45,   47,   46,   44,   42,   37,   38,   35,   43,
 /*   820 */   184,   74,  180,  179,  358,  175,  174,  172,  150,  194,
 /*   830 */   358,  193,   58,  358,  191,   74,  180,  179,  358,  175,
 /*   840 */   174,  172,  358,  358,  148,   36,   39,   41,   45,   47,
 /*   850 */    46,   44,   42,   37,   38,   35,   43,  358,  202,  194,
 /*   860 */   102,  193,   58,  358,  191,   74,  180,  179,  358,  175,
 /*   870 */   174,  172,  157,  194,  358,  193,   58,  358,  191,   74,
 /*   880 */   180,  179,  358,  175,  174,  172,  358,  158,  116,  194,
 /*   890 */   358,  193,   58,  358,  191,   74,  180,  179,  358,  175,
 /*   900 */   174,  172,  358,  358,  358,  358,  358,  358,  358,  358,
 /*   910 */   105,  194,  358,  193,   58,  358,  191,   74,  180,  179,
 /*   920 */   358,  175,  174,  172,  358,  358,  143,  194,  358,  193,
 /*   930 */    58,  358,  191,   74,  180,  179,  358,  175,  174,  172,
 /*   940 */   115,  194,  358,  193,   58,  358,  191,   74,  180,  179,
 /*   950 */   358,  175,  174,  172,   86,  194,  358,  193,   58,  358,
 /*   960 */   191,   74,  180,  179,  358,  175,  174,  172,  358,  100,
 /*   970 */   194,  358,  193,   58,  358,  191,   74,  180,  179,  358,
 /*   980 */   175,  174,  172,  213,  194,  358,  193,   58,  358,  191,
 /*   990 */    74,  180,  179,  358,  175,  174,  172,  358,   81,  194,
 /*  1000 */   358,  193,   58,  358,  191,   74,  180,  179,  358,  175,
 /*  1010 */   174,  172,  120,  194,  358,  193,   58,  358,  191,   74,
 /*  1020 */   180,  179,  358,  175,  174,  172,   97,  194,  358,  193,
 /*  1030 */    58,  358,  191,   74,  180,  179,  358,  175,  174,  172,
 /*  1040 */    88,  194,  358,  193,   58,  358,  191,   74,  180,  179,
 /*  1050 */   358,  175,  174,  172,  358,  134,  358,  193,   58,  358,
 /*  1060 */   191,   74,  180,  179,  358,  175,  174,  172,  358,  192,
 /*  1070 */   358,  193,   58,  358,  191,   74,  180,  179,  358,  175,
 /*  1080 */   174,  172,   72,  358,  191,   74,  180,  179,  358,  175,
 /*  1090 */   174,  172,   70,  358,  191,   74,  180,  179,  358,  175,
 /*  1100 */   174,  172,   66,  358,  191,   74,  180,  179,  358,  175,
 /*  1110 */   174,  172,  126,  358,  191,   74,  180,  179,  358,  175,
 /*  1120 */   174,  172,  358,   64,  358,  191,   74,  180,  179,  358,
 /*  1130 */   175,  174,  172,  129,  358,  191,   74,  180,  179,  358,
 /*  1140 */   175,  174,  172,   75,  358,  191,   74,  180,  179,  358,
 /*  1150 */   175,  174,  172,   68,  358,  191,   74,  180,  179,  358,
 /*  1160 */   175,  174,  172,   65,  358,  191,   74,  180,  179,  358,
 /*  1170 */   175,  174,  172,   63,  358,  191,   74,  180,  179,  358,
 /*  1180 */   175,  174,  172,   69,  358,  191,   74,  180,  179,  358,
 /*  1190 */   175,  174,  172,  131,  358,  191,   74,  180,  179,  358,
 /*  1200 */   175,  174,  172,   76,  358,  191,   74,  180,  179,  358,
 /*  1210 */   175,  174,  172,   71,  358,  191,   74,  180,  179,  358,
 /*  1220 */   175,  174,  172,   67,  358,  191,   74,  180,  179,  358,
 /*  1230 */   175,  174,  172,
};
static const YYCODETYPE yy_lookahead[] = {
 /*     0 */    63,   64,   65,   66,   67,   68,   69,   70,   71,   72,
 /*    10 */    73,   74,   75,   76,   77,   78,   79,   80,   81,   82,
 /*    20 */    44,   84,   85,   46,   87,   88,   89,   90,   13,   92,
 /*    30 */    93,   94,   14,   15,   16,   98,   99,  100,   61,   62,
 /*    40 */    25,   26,   46,   28,   29,   32,   31,   51,   33,   34,
 /*    50 */    35,   36,   37,   38,   39,   40,   41,   17,   43,   44,
 /*    60 */    45,   46,   47,   48,   49,   50,   51,   52,   53,   54,
 /*    70 */    13,   44,   46,   58,   59,   60,  105,   51,  107,  108,
 /*    80 */   109,  110,   25,   26,  112,   28,   29,   42,   31,  108,
 /*    90 */    33,   34,   35,   36,   37,   38,   39,   40,   41,   32,
 /*   100 */    43,   44,   45,   46,   47,   48,   49,   50,   51,   52,
 /*   110 */    53,   54,  113,  114,   63,   58,   59,   60,   67,   68,
 /*   120 */    69,   70,   71,   72,   73,   74,   75,   76,   77,   78,
 /*   130 */    79,   80,   81,   82,   28,   84,   85,   32,   87,   88,
 /*   140 */    89,   90,  100,   92,   93,   94,  104,   42,   44,   98,
 /*   150 */    99,  100,    6,    7,    8,    9,   10,   11,   12,   13,
 /*   160 */    14,   15,   16,   44,   63,  102,  115,   57,   67,   68,
 /*   170 */    69,   70,   71,   72,   73,   74,   75,   76,   77,   78,
 /*   180 */    79,   80,   81,   82,   30,   84,   85,   28,   87,   88,
 /*   190 */    89,   90,   31,   92,   93,   94,   42,   13,   42,   98,
 /*   200 */    99,  100,   12,   13,   14,   15,   16,   28,   92,   25,
 /*   210 */    26,   32,   28,   29,   86,   31,  115,   33,   34,   35,
 /*   220 */    36,   37,   38,   39,   40,   41,   31,   43,   44,   45,
 /*   230 */    28,   47,   48,   49,   50,   51,   52,   53,   54,  101,
 /*   240 */   102,   63,   58,   59,   60,   67,   68,   69,   70,   71,
 /*   250 */    72,   73,   74,   75,   76,   77,   78,   79,   80,   81,
 /*   260 */    82,   32,   84,   85,   44,   87,   88,   89,   90,   44,
 /*   270 */    92,   93,   94,   55,   63,   44,   98,   99,  100,   68,
 /*   280 */    69,   70,   71,   72,   73,   74,   75,   76,   77,   78,
 /*   290 */    79,   80,   81,   82,   42,   84,   85,   32,   87,   88,
 /*   300 */    89,   90,   24,   92,   93,   94,   31,   45,   44,   98,
 /*   310 */    99,  100,   44,   63,   31,   28,   24,  106,   68,   69,
 /*   320 */    70,   71,   72,   73,   74,   75,   76,   77,   78,   79,
 /*   330 */    80,   81,   82,   17,   84,   85,   32,   87,   88,   89,
 /*   340 */    90,   32,   92,   93,   94,   31,   28,   32,   98,   99,
 /*   350 */   100,  100,   63,   30,  103,  104,  106,   68,   69,   70,
 /*   360 */    71,   72,   73,   74,   75,   76,   77,   78,   79,   80,
 /*   370 */    81,   82,   24,   84,   85,   45,   87,   88,   89,   90,
 /*   380 */    31,   92,   93,   94,   44,   63,   28,   98,   99,  100,
 /*   390 */    68,   69,   70,   71,   72,   73,   74,   75,   76,   77,
 /*   400 */    78,   79,   80,   81,   82,   28,   84,   85,   38,   87,
 /*   410 */    88,   89,   90,  116,   92,   93,   94,  116,  116,  116,
 /*   420 */    98,   99,  100,   63,  116,  116,  116,  116,   68,   69,
 /*   430 */    70,   71,   72,   73,   74,   75,   76,   77,   78,   79,
 /*   440 */    80,   81,   82,  116,   84,   85,  116,   87,   88,   89,
 /*   450 */    90,  116,   92,   93,   94,  116,   63,  116,   98,   99,
 /*   460 */   100,   68,   69,   70,   71,   72,   73,   74,   75,   76,
 /*   470 */    77,   78,   79,   80,   81,   82,  116,   84,   85,  116,
 /*   480 */    87,   88,   89,   90,  116,   92,   93,   94,  116,  116,
 /*   490 */   116,   98,   99,  100,   63,  107,  108,  116,  110,   68,
 /*   500 */    69,   70,   71,   72,   73,   74,   75,   76,   77,   78,
 /*   510 */    79,   80,   81,   82,  116,   84,   85,  116,   87,   88,
 /*   520 */    89,   90,  116,   92,   93,   94,  116,   63,  116,   98,
 /*   530 */    99,  100,   68,   69,   70,   71,   72,   73,   74,   75,
 /*   540 */    76,   77,   78,   79,   80,   81,   82,  116,   84,   85,
 /*   550 */   116,   87,   88,   89,   90,  116,   92,   93,   94,  116,
 /*   560 */   116,  116,   98,   99,  100,    1,    2,    3,    4,    5,
 /*   570 */     6,    7,    8,    9,   10,   11,   12,   13,   14,   15,
 /*   580 */    16,   17,   18,   19,   20,   21,   22,   23,   13,    8,
 /*   590 */     9,   10,   11,   12,   13,   14,   15,   16,  116,  116,
 /*   600 */    25,   26,  116,   28,   29,  116,   31,  116,   33,   34,
 /*   610 */    35,   36,   37,   38,   39,   40,   41,   13,   43,   44,
 /*   620 */    25,   26,   27,  116,   29,  116,   51,  116,  116,   25,
 /*   630 */    26,  116,   28,   29,  116,   31,   32,   33,   34,   35,
 /*   640 */    36,   37,   38,   39,   40,   41,   13,   43,   33,   34,
 /*   650 */    35,   36,   37,   38,   39,   40,   41,  116,   25,   26,
 /*   660 */   116,   28,   29,  116,   31,  116,   33,   34,   35,   36,
 /*   670 */    37,   38,   39,   40,   41,   13,   43,   44,   87,   88,
 /*   680 */    89,   90,  116,   92,   93,   94,  116,   25,   26,  116,
 /*   690 */    28,   29,  116,   31,  116,   33,   34,   35,   36,   37,
 /*   700 */    38,   39,   40,   41,  116,   43,    2,    3,    4,    5,
 /*   710 */     6,    7,    8,    9,   10,   11,   12,   13,   14,   15,
 /*   720 */    16,    3,    4,    5,    6,    7,    8,    9,   10,   11,
 /*   730 */    12,   13,   14,   15,   16,   70,   85,  116,   87,   88,
 /*   740 */    89,   90,  116,   92,   93,   94,   81,   82,  116,   84,
 /*   750 */    85,  116,   87,   88,   89,   90,  116,   92,   93,   94,
 /*   760 */   116,   87,   88,   89,   90,  100,   92,   93,   94,  116,
 /*   770 */   116,  116,  116,  116,  116,  116,  111,   81,   82,  116,
 /*   780 */    84,   85,  116,   87,   88,   89,   90,   91,   92,   93,
 /*   790 */    94,   81,   82,   97,   84,   85,  116,   87,   88,   89,
 /*   800 */    90,  116,   92,   93,   94,   95,   96,    4,    5,    6,
 /*   810 */     7,    8,    9,   10,   11,   12,   13,   14,   15,   16,
 /*   820 */    87,   88,   89,   90,  116,   92,   93,   94,   81,   82,
 /*   830 */   116,   84,   85,  116,   87,   88,   89,   90,  116,   92,
 /*   840 */    93,   94,  116,  116,   97,    5,    6,    7,    8,    9,
 /*   850 */    10,   11,   12,   13,   14,   15,   16,  116,   81,   82,
 /*   860 */    83,   84,   85,  116,   87,   88,   89,   90,  116,   92,
 /*   870 */    93,   94,   81,   82,  116,   84,   85,  116,   87,   88,
 /*   880 */    89,   90,  116,   92,   93,   94,  116,   96,   81,   82,
 /*   890 */   116,   84,   85,  116,   87,   88,   89,   90,  116,   92,
 /*   900 */    93,   94,  116,  116,  116,  116,  116,  116,  116,  116,
 /*   910 */    81,   82,  116,   84,   85,  116,   87,   88,   89,   90,
 /*   920 */   116,   92,   93,   94,  116,  116,   81,   82,  116,   84,
 /*   930 */    85,  116,   87,   88,   89,   90,  116,   92,   93,   94,
 /*   940 */    81,   82,  116,   84,   85,  116,   87,   88,   89,   90,
 /*   950 */   116,   92,   93,   94,   81,   82,  116,   84,   85,  116,
 /*   960 */    87,   88,   89,   90,  116,   92,   93,   94,  116,   81,
 /*   970 */    82,  116,   84,   85,  116,   87,   88,   89,   90,  116,
 /*   980 */    92,   93,   94,   81,   82,  116,   84,   85,  116,   87,
 /*   990 */    88,   89,   90,  116,   92,   93,   94,  116,   81,   82,
 /*  1000 */   116,   84,   85,  116,   87,   88,   89,   90,  116,   92,
 /*  1010 */    93,   94,   81,   82,  116,   84,   85,  116,   87,   88,
 /*  1020 */    89,   90,  116,   92,   93,   94,   81,   82,  116,   84,
 /*  1030 */    85,  116,   87,   88,   89,   90,  116,   92,   93,   94,
 /*  1040 */    81,   82,  116,   84,   85,  116,   87,   88,   89,   90,
 /*  1050 */   116,   92,   93,   94,  116,   82,  116,   84,   85,  116,
 /*  1060 */    87,   88,   89,   90,  116,   92,   93,   94,  116,   82,
 /*  1070 */   116,   84,   85,  116,   87,   88,   89,   90,  116,   92,
 /*  1080 */    93,   94,   85,  116,   87,   88,   89,   90,  116,   92,
 /*  1090 */    93,   94,   85,  116,   87,   88,   89,   90,  116,   92,
 /*  1100 */    93,   94,   85,  116,   87,   88,   89,   90,  116,   92,
 /*  1110 */    93,   94,   85,  116,   87,   88,   89,   90,  116,   92,
 /*  1120 */    93,   94,  116,   85,  116,   87,   88,   89,   90,  116,
 /*  1130 */    92,   93,   94,   85,  116,   87,   88,   89,   90,  116,
 /*  1140 */    92,   93,   94,   85,  116,   87,   88,   89,   90,  116,
 /*  1150 */    92,   93,   94,   85,  116,   87,   88,   89,   90,  116,
 /*  1160 */    92,   93,   94,   85,  116,   87,   88,   89,   90,  116,
 /*  1170 */    92,   93,   94,   85,  116,   87,   88,   89,   90,  116,
 /*  1180 */    92,   93,   94,   85,  116,   87,   88,   89,   90,  116,
 /*  1190 */    92,   93,   94,   85,  116,   87,   88,   89,   90,  116,
 /*  1200 */    92,   93,   94,   85,  116,   87,   88,   89,   90,  116,
 /*  1210 */    92,   93,   94,   85,  116,   87,   88,   89,   90,  116,
 /*  1220 */    92,   93,   94,   85,  116,   87,   88,   89,   90,  116,
 /*  1230 */    92,   93,   94,
};
#define YY_SHIFT_USE_DFLT (-25)
#define YY_SHIFT_MAX 121
static const short yy_shift_ofst[] = {
 /*     0 */   184,  184,  184,   15,  184,  184,   57,  184,  184,  184,
 /*    10 */   184,  184,  184,  184,  575,  604,  662,  662,  662,  662,
 /*    20 */   633,  662,  662,  662,  662,  662,  662,  662,  662,  662,
 /*    30 */   662,  662,  662,  662,  662,  662,  662,  662,  662,  662,
 /*    40 */   662,  662,  662,  662,  662,  662,  662,  662,  662,  662,
 /*    50 */   662,  662,  179,   -4,  202,  -23,   26,  159,  564,  615,
 /*    60 */   159,  106,  -25,  704,  718,  803,  840,  146,  581,  581,
 /*    70 */   190,  190,  190,  190,  595,   18,   18,  105,  154,  231,
 /*    80 */   252,  278,  262,  268,  287,  316,  309,  318,  323,  330,
 /*    90 */   340,  377,  358,  349,  348,  315,  314,  304,  292,  283,
 /*   100 */   264,  275,  265,  218,  225,  229,   40,  195,  156,  161,
 /*   110 */   110,  119,   45,  104,   45,   27,   13,  220,  -24,   40,
 /*   120 */    67,  370,
};
#define YY_REDUCE_USE_DFLT (-64)
#define YY_REDUCE_MAX 62
static const short yy_reduce_ofst[] = {
 /*     0 */   -63,   51,  101,  178,  211,  250,  322,  393,  322,  289,
 /*    10 */   322,  360,  431,  464,  665,  696,  710,  777,  791,  747,
 /*    20 */   859,  807,  888,  931,  902,  873,  945,  829,  845,  917,
 /*    30 */   959,  973,  987, 1038, 1078, 1048, 1138, 1118, 1108, 1098,
 /*    40 */  1088, 1068, 1058, 1027, 1007, 1128,  651,  997, 1017,  733,
 /*    50 */   591,  674,  -29,  251,  388,   -1,   42,  138,  128,  116,
 /*    60 */    63,  -19,  -28,
};
static const YYACTIONTYPE yy_default[] = {
 /*     0 */   229,  356,  356,  356,  356,  356,  356,  356,  230,  356,
 /*    10 */   355,  356,  356,  356,  356,  356,  356,  245,  356,  356,
 /*    20 */   356,  356,  356,  356,  356,  356,  356,  356,  356,  356,
 /*    30 */   356,  356,  356,  356,  356,  356,  356,  356,  356,  356,
 /*    40 */   356,  356,  356,  356,  356,  356,  356,  356,  356,  356,
 /*    50 */   356,  356,  356,  356,  356,  356,  356,  356,  255,  356,
 /*    60 */   356,  356,  350,  258,  259,  260,  261,  262,  264,  263,
 /*    70 */   268,  265,  266,  267,  274,  269,  270,  356,  356,  356,
 /*    80 */   320,  356,  356,  356,  356,  321,  356,  356,  356,  356,
 /*    90 */   356,  356,  356,  356,  356,  356,  356,  356,  356,  356,
 /*   100 */   356,  299,  356,  287,  356,  356,  332,  356,  338,  356,
 /*   110 */   346,  356,  339,  356,  340,  356,  356,  356,  356,  356,
 /*   120 */   356,  356,  311,  309,  312,  308,  273,  313,  314,  272,
 /*   130 */   315,  271,  316,  317,  256,  318,  254,  319,  253,  252,
 /*   140 */   251,  250,  249,  322,  324,  323,  283,  306,  307,  325,
 /*   150 */   305,  326,  301,  327,  329,  328,  304,  303,  302,  300,
 /*   160 */   298,  330,  341,  297,  296,  295,  294,  293,  292,  345,
 /*   170 */   291,  290,  289,  288,  287,  286,  347,  285,  284,  279,
 /*   180 */   278,  348,  277,  276,  275,  282,  281,  280,  349,  351,
 /*   190 */   352,  257,  248,  247,  244,  353,  243,  242,  354,  241,
 /*   200 */   240,  239,  246,  238,  237,  342,  343,  236,  344,  235,
 /*   210 */   331,  234,  233,  333,  334,  232,  231,  335,  336,  228,
 /*   220 */   227,  226,  337,  225,  310,
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
  "RPAREN",        "INTEGER",       "HEX",           "BIN",         
  "ROM",           "REAL",          "STRING",        "TRUE",        
  "FALSE",         "NULL",          "COMMA",         "NEW",         
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
  "postfix_expression",  "new_expression",  "primary_expression",  "argument_list",
  "literal",       "id_expression",  "list_literal",  "list_content",
  "list_entry",    "argument",      "function_declaration",  "struct_declaration",
  "variable_declaration",  "declarator_sequence",  "declarator",    "struct_members",
  "struct_member",  "parameter_list",  "function_body",  "parameter",   
  "opt_parameter",  "parameters",    "opt_parameters",  "for_init_statement",
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
 /*  66 */ "literal ::= HEX",
 /*  67 */ "literal ::= BIN",
 /*  68 */ "literal ::= ROM",
 /*  69 */ "literal ::= REAL",
 /*  70 */ "literal ::= STRING",
 /*  71 */ "literal ::= TRUE",
 /*  72 */ "literal ::= FALSE",
 /*  73 */ "literal ::= NULL",
 /*  74 */ "id_expression ::= IDENTIFIER",
 /*  75 */ "list_literal ::= LBRACKET list_content RBRACKET",
 /*  76 */ "list_content ::= list_entry",
 /*  77 */ "list_content ::= list_content COMMA list_entry",
 /*  78 */ "list_entry ::= expression",
 /*  79 */ "new_expression ::= NEW IDENTIFIER",
 /*  80 */ "argument ::= expression",
 /*  81 */ "argument_list ::= argument",
 /*  82 */ "argument_list ::= argument_list COMMA argument",
 /*  83 */ "expression_statement ::= SEMICOLON",
 /*  84 */ "expression_statement ::= expression SEMICOLON",
 /*  85 */ "compound_statement ::= LBRACE RBRACE",
 /*  86 */ "compound_statement ::= LBRACE statement_sequence RBRACE",
 /*  87 */ "include_statement ::= INCLUDE STRING SEMICOLON",
 /*  88 */ "return_statement ::= RETURN expression SEMICOLON",
 /*  89 */ "return_statement ::= RETURN SEMICOLON",
 /*  90 */ "break_statement ::= BREAK SEMICOLON",
 /*  91 */ "continue_statement ::= CONTINUE SEMICOLON",
 /*  92 */ "declaration_statement ::= function_declaration",
 /*  93 */ "declaration_statement ::= struct_declaration SEMICOLON",
 /*  94 */ "declaration_statement ::= variable_declaration SEMICOLON",
 /*  95 */ "variable_declaration ::= VAR declarator_sequence",
 /*  96 */ "declarator ::= IDENTIFIER",
 /*  97 */ "declarator ::= IDENTIFIER ASSIGN expression",
 /*  98 */ "declarator_sequence ::= declarator",
 /*  99 */ "declarator_sequence ::= declarator_sequence COMMA declarator",
 /* 100 */ "struct_declaration ::= STRUCT IDENTIFIER LBRACE RBRACE",
 /* 101 */ "struct_declaration ::= STRUCT IDENTIFIER LBRACE struct_members RBRACE",
 /* 102 */ "struct_member ::= variable_declaration SEMICOLON",
 /* 103 */ "struct_members ::= struct_member",
 /* 104 */ "struct_members ::= struct_members struct_member",
 /* 105 */ "function_declaration ::= FUNCTION IDENTIFIER LPAREN parameter_list RPAREN function_body",
 /* 106 */ "function_declaration ::= FUNCTION IDENTIFIER LPAREN RPAREN function_body",
 /* 107 */ "parameter ::= IDENTIFIER",
 /* 108 */ "opt_parameter ::= IDENTIFIER ASSIGN expression",
 /* 109 */ "parameters ::= parameter",
 /* 110 */ "parameters ::= parameters COMMA parameter",
 /* 111 */ "opt_parameters ::= opt_parameter",
 /* 112 */ "opt_parameters ::= opt_parameters COMMA opt_parameter",
 /* 113 */ "parameter_list ::= parameters",
 /* 114 */ "parameter_list ::= opt_parameters",
 /* 115 */ "parameter_list ::= parameters COMMA opt_parameters",
 /* 116 */ "function_body ::= statement",
 /* 117 */ "for_statement ::= FOR LPAREN for_init_statement expression SEMICOLON expression_opt RPAREN statement",
 /* 118 */ "for_init_statement ::= expression_statement",
 /* 119 */ "for_init_statement ::= variable_declaration SEMICOLON",
 /* 120 */ "foreach_statement ::= FOR LPAREN id_expression IN expression RPAREN statement",
 /* 121 */ "if_statement ::= IF LPAREN expression RPAREN statement",
 /* 122 */ "if_statement ::= IF LPAREN expression RPAREN statement ELSE statement",
 /* 123 */ "while_statement ::= WHILE LPAREN expression RPAREN statement",
 /* 124 */ "switch_statement ::= SWITCH LPAREN expression RPAREN LBRACE switch_body RBRACE",
 /* 125 */ "switch_body ::=",
 /* 126 */ "switch_body ::= switch_body switch_case",
 /* 127 */ "switch_body ::= switch_body default_case",
 /* 128 */ "switch_case ::= CASE literal COLON case_statements",
 /* 129 */ "default_case ::= DEFAULT COLON case_statements",
 /* 130 */ "case_statements ::= statement_sequence",
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
  { 64, 1 },
  { 65, 1 },
  { 67, 1 },
  { 67, 2 },
  { 66, 0 },
  { 66, 1 },
  { 68, 1 },
  { 68, 1 },
  { 68, 1 },
  { 68, 1 },
  { 68, 1 },
  { 68, 1 },
  { 68, 1 },
  { 68, 1 },
  { 68, 1 },
  { 68, 1 },
  { 68, 1 },
  { 68, 1 },
  { 68, 1 },
  { 81, 1 },
  { 83, 0 },
  { 83, 1 },
  { 82, 1 },
  { 82, 3 },
  { 86, 1 },
  { 86, 1 },
  { 86, 1 },
  { 86, 1 },
  { 86, 1 },
  { 86, 1 },
  { 84, 1 },
  { 84, 5 },
  { 85, 1 },
  { 85, 3 },
  { 85, 3 },
  { 85, 3 },
  { 85, 3 },
  { 85, 3 },
  { 85, 3 },
  { 85, 3 },
  { 85, 3 },
  { 85, 3 },
  { 85, 3 },
  { 85, 3 },
  { 85, 3 },
  { 85, 3 },
  { 85, 3 },
  { 85, 3 },
  { 85, 3 },
  { 87, 1 },
  { 87, 2 },
  { 87, 2 },
  { 87, 2 },
  { 87, 1 },
  { 88, 1 },
  { 88, 2 },
  { 88, 2 },
  { 88, 3 },
  { 88, 4 },
  { 88, 3 },
  { 88, 4 },
  { 90, 1 },
  { 90, 1 },
  { 90, 3 },
  { 90, 1 },
  { 92, 1 },
  { 92, 1 },
  { 92, 1 },
  { 92, 1 },
  { 92, 1 },
  { 92, 1 },
  { 92, 1 },
  { 92, 1 },
  { 92, 1 },
  { 93, 1 },
  { 94, 3 },
  { 95, 1 },
  { 95, 3 },
  { 96, 1 },
  { 89, 2 },
  { 97, 1 },
  { 91, 1 },
  { 91, 3 },
  { 70, 1 },
  { 70, 2 },
  { 73, 2 },
  { 73, 3 },
  { 69, 3 },
  { 77, 3 },
  { 77, 2 },
  { 79, 2 },
  { 80, 2 },
  { 71, 1 },
  { 71, 2 },
  { 71, 2 },
  { 100, 2 },
  { 102, 1 },
  { 102, 3 },
  { 101, 1 },
  { 101, 3 },
  { 99, 4 },
  { 99, 5 },
  { 104, 2 },
  { 103, 1 },
  { 103, 2 },
  { 98, 6 },
  { 98, 5 },
  { 107, 1 },
  { 108, 3 },
  { 109, 1 },
  { 109, 3 },
  { 110, 1 },
  { 110, 3 },
  { 105, 1 },
  { 105, 1 },
  { 105, 3 },
  { 106, 1 },
  { 72, 8 },
  { 111, 1 },
  { 111, 2 },
  { 76, 7 },
  { 74, 5 },
  { 74, 7 },
  { 75, 5 },
  { 78, 7 },
  { 112, 0 },
  { 112, 2 },
  { 112, 2 },
  { 113, 4 },
  { 114, 3 },
  { 115, 1 },
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
#line 74 "astgen.in"
{ p->SetRoot(yymsp[0].minor.yy71); }
#line 1167 "astgen.c"
        break;
      case 1:
#line 77 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(translation_unit, yymsp[0].minor.yy71); }
#line 1172 "astgen.c"
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
      case 81:
      case 92:
      case 95:
      case 98:
      case 103:
      case 109:
      case 111:
      case 113:
      case 114:
      case 116:
      case 118:
      case 130:
#line 80 "astgen.in"
{ yygotominor.yy71 = yymsp[0].minor.yy71; }
#line 1213 "astgen.c"
        break;
      case 3:
#line 81 "astgen.in"
{ 
  if(yymsp[-1].minor.yy71->m_type == statement_sequence) {
    yygotominor.yy71 = yymsp[-1].minor.yy71;
  }
  else {
    yygotominor.yy71 = p->AllocAst(statement_sequence, new AstList);
    yygotominor.yy71->m_a1.GetList()->push_back(yymsp[-1].minor.yy71);
  }
  yygotominor.yy71->m_a1.GetList()->push_back(yymsp[0].minor.yy71);
}
#line 1227 "astgen.c"
        break;
      case 4:
      case 20:
#line 94 "astgen.in"
{ yygotominor.yy71 = 0; }
#line 1233 "astgen.c"
        break;
      case 18:
      case 83:
#line 112 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(empty_statement); }
#line 1239 "astgen.c"
        break;
      case 23:
#line 128 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(assignment_expression, yymsp[-1].minor.yy206, yymsp[-2].minor.yy71, yymsp[0].minor.yy71); }
#line 1244 "astgen.c"
        break;
      case 24:
#line 132 "astgen.in"
{ yygotominor.yy206 = op_assign; }
#line 1249 "astgen.c"
        break;
      case 25:
#line 133 "astgen.in"
{ yygotominor.yy206 = op_assadd; }
#line 1254 "astgen.c"
        break;
      case 26:
#line 134 "astgen.in"
{ yygotominor.yy206 = op_asssub; }
#line 1259 "astgen.c"
        break;
      case 27:
#line 135 "astgen.in"
{ yygotominor.yy206 = op_assmul; }
#line 1264 "astgen.c"
        break;
      case 28:
#line 136 "astgen.in"
{ yygotominor.yy206 = op_assdiv; }
#line 1269 "astgen.c"
        break;
      case 29:
#line 137 "astgen.in"
{ yygotominor.yy206 = op_assmod; }
#line 1274 "astgen.c"
        break;
      case 31:
#line 141 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(ternary_expression, yymsp[-4].minor.yy71, yymsp[-2].minor.yy71, yymsp[0].minor.yy71); }
#line 1279 "astgen.c"
        break;
      case 33:
#line 145 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(binary_expression, op_logor,   yymsp[-2].minor.yy71, yymsp[0].minor.yy71); }
#line 1284 "astgen.c"
        break;
      case 34:
#line 146 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(binary_expression, op_logand,  yymsp[-2].minor.yy71, yymsp[0].minor.yy71); }
#line 1289 "astgen.c"
        break;
      case 35:
#line 147 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(binary_expression, op_bitor,   yymsp[-2].minor.yy71, yymsp[0].minor.yy71); }
#line 1294 "astgen.c"
        break;
      case 36:
#line 148 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(binary_expression, op_bitxor,  yymsp[-2].minor.yy71, yymsp[0].minor.yy71); }
#line 1299 "astgen.c"
        break;
      case 37:
#line 149 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(binary_expression, op_bitand,  yymsp[-2].minor.yy71, yymsp[0].minor.yy71); }
#line 1304 "astgen.c"
        break;
      case 38:
#line 150 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(binary_expression, op_eq,   yymsp[-2].minor.yy71, yymsp[0].minor.yy71); }
#line 1309 "astgen.c"
        break;
      case 39:
#line 151 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(binary_expression, op_ne,   yymsp[-2].minor.yy71, yymsp[0].minor.yy71); }
#line 1314 "astgen.c"
        break;
      case 40:
#line 152 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(binary_expression, op_lt,   yymsp[-2].minor.yy71, yymsp[0].minor.yy71); }
#line 1319 "astgen.c"
        break;
      case 41:
#line 153 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(binary_expression, op_le,   yymsp[-2].minor.yy71, yymsp[0].minor.yy71); }
#line 1324 "astgen.c"
        break;
      case 42:
#line 154 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(binary_expression, op_gt,   yymsp[-2].minor.yy71, yymsp[0].minor.yy71); }
#line 1329 "astgen.c"
        break;
      case 43:
#line 155 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(binary_expression, op_ge,   yymsp[-2].minor.yy71, yymsp[0].minor.yy71); }
#line 1334 "astgen.c"
        break;
      case 44:
#line 156 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(binary_expression, op_add,  yymsp[-2].minor.yy71, yymsp[0].minor.yy71); }
#line 1339 "astgen.c"
        break;
      case 45:
#line 157 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(binary_expression, op_sub,  yymsp[-2].minor.yy71, yymsp[0].minor.yy71); }
#line 1344 "astgen.c"
        break;
      case 46:
#line 158 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(binary_expression, op_mul,  yymsp[-2].minor.yy71, yymsp[0].minor.yy71); }
#line 1349 "astgen.c"
        break;
      case 47:
#line 159 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(binary_expression, op_div,  yymsp[-2].minor.yy71, yymsp[0].minor.yy71); }
#line 1354 "astgen.c"
        break;
      case 48:
#line 160 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(binary_expression, op_mod,  yymsp[-2].minor.yy71, yymsp[0].minor.yy71); }
#line 1359 "astgen.c"
        break;
      case 50:
#line 164 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(prefix_expression, op_negate, yymsp[0].minor.yy71); }
#line 1364 "astgen.c"
        break;
      case 51:
#line 165 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(prefix_expression, op_preinc, yymsp[0].minor.yy71); }
#line 1369 "astgen.c"
        break;
      case 52:
#line 166 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(prefix_expression, op_predec, yymsp[0].minor.yy71); }
#line 1374 "astgen.c"
        break;
      case 55:
#line 171 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(postfix_expression, op_postinc, yymsp[-1].minor.yy71); }
#line 1379 "astgen.c"
        break;
      case 56:
#line 172 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(postfix_expression, op_postdec, yymsp[-1].minor.yy71); }
#line 1384 "astgen.c"
        break;
      case 57:
#line 173 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(member_expression, yymsp[-2].minor.yy71, String(yymsp[0].minor.yy0)); }
#line 1389 "astgen.c"
        break;
      case 58:
#line 174 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(index_expression, yymsp[-3].minor.yy71, yymsp[-1].minor.yy71); }
#line 1394 "astgen.c"
        break;
      case 59:
#line 175 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(function_call, String(yymsp[-2].minor.yy0)); }
#line 1399 "astgen.c"
        break;
      case 60:
#line 176 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(function_call, String(yymsp[-3].minor.yy0), yymsp[-1].minor.yy71); }
#line 1404 "astgen.c"
        break;
      case 63:
      case 93:
      case 94:
      case 102:
      case 119:
#line 181 "astgen.in"
{ yygotominor.yy71 = yymsp[-1].minor.yy71; }
#line 1413 "astgen.c"
        break;
      case 65:
#line 185 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(literal, Variant(String(yymsp[0].minor.yy0), Variant::stInt)); }
#line 1418 "astgen.c"
        break;
      case 66:
#line 186 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(literal, Variant(hex2dec(String(yymsp[0].minor.yy0).c_str()))); }
#line 1423 "astgen.c"
        break;
      case 67:
#line 187 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(literal, Variant(bin2dec(String(yymsp[0].minor.yy0).c_str()))); }
#line 1428 "astgen.c"
        break;
      case 68:
#line 188 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(literal, Variant(rom2dec(String(yymsp[0].minor.yy0).c_str()))); }
#line 1433 "astgen.c"
        break;
      case 69:
#line 189 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(literal, Variant(String(yymsp[0].minor.yy0), Variant::stReal)); }
#line 1438 "astgen.c"
        break;
      case 70:
#line 190 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(literal, Variant(String(yymsp[0].minor.yy0), Variant::stString)); }
#line 1443 "astgen.c"
        break;
      case 71:
#line 191 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(literal, Variant(true));    }
#line 1448 "astgen.c"
        break;
      case 72:
#line 192 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(literal, Variant(false));   }
#line 1453 "astgen.c"
        break;
      case 73:
#line 193 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(literal, Variant());        }
#line 1458 "astgen.c"
        break;
      case 74:
#line 196 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(lvalue, String(yymsp[0].minor.yy0)); }
#line 1463 "astgen.c"
        break;
      case 75:
#line 199 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(list_literal, yymsp[-1].minor.yy71); }
#line 1468 "astgen.c"
        break;
      case 76:
#line 200 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(list_content, yymsp[0].minor.yy71); }
#line 1473 "astgen.c"
        break;
      case 77:
#line 201 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(list_content, yymsp[-2].minor.yy71, yymsp[0].minor.yy71); }
#line 1478 "astgen.c"
        break;
      case 78:
#line 202 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(list_entry, yymsp[0].minor.yy71); }
#line 1483 "astgen.c"
        break;
      case 79:
#line 205 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(new_expression, String(yymsp[0].minor.yy0)); }
#line 1488 "astgen.c"
        break;
      case 80:
#line 214 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(argument, yymsp[0].minor.yy71); }
#line 1493 "astgen.c"
        break;
      case 82:
#line 218 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(argument_list, yymsp[-2].minor.yy71, yymsp[0].minor.yy71); }
#line 1498 "astgen.c"
        break;
      case 84:
#line 227 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(expression_statement, yymsp[-1].minor.yy71); }
#line 1503 "astgen.c"
        break;
      case 85:
#line 230 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(compound_statement); }
#line 1508 "astgen.c"
        break;
      case 86:
#line 231 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(compound_statement, yymsp[-1].minor.yy71); }
#line 1513 "astgen.c"
        break;
      case 87:
#line 234 "astgen.in"
{ p->Parse(yymsp[-1].minor.yy0); yygotominor.yy71 = p->GetRoot(); }
#line 1518 "astgen.c"
        break;
      case 88:
#line 237 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(return_statement, yymsp[-1].minor.yy71); }
#line 1523 "astgen.c"
        break;
      case 89:
#line 238 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(return_statement);    }
#line 1528 "astgen.c"
        break;
      case 90:
#line 241 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(break_statement); }
#line 1533 "astgen.c"
        break;
      case 91:
#line 242 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(continue_statement); }
#line 1538 "astgen.c"
        break;
      case 96:
#line 256 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(variable_declaration, String(yymsp[0].minor.yy0));    }
#line 1543 "astgen.c"
        break;
      case 97:
#line 257 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(variable_declaration, String(yymsp[-2].minor.yy0), yymsp[0].minor.yy71); }
#line 1548 "astgen.c"
        break;
      case 99:
#line 260 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(declaration_sequence, yymsp[-2].minor.yy71, yymsp[0].minor.yy71); }
#line 1553 "astgen.c"
        break;
      case 100:
#line 269 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(struct_declaration, String(yymsp[-2].minor.yy0)); }
#line 1558 "astgen.c"
        break;
      case 101:
#line 270 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(struct_declaration, String(yymsp[-3].minor.yy0), yymsp[-1].minor.yy71); }
#line 1563 "astgen.c"
        break;
      case 104:
#line 277 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(struct_members, yymsp[-1].minor.yy71, yymsp[0].minor.yy71); }
#line 1568 "astgen.c"
        break;
      case 105:
#line 285 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(function_declaration, String(yymsp[-4].minor.yy0), yymsp[-2].minor.yy71, yymsp[0].minor.yy71); }
#line 1573 "astgen.c"
        break;
      case 106:
#line 286 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(function_declaration, String(yymsp[-3].minor.yy0), AstData(), yymsp[0].minor.yy71); }
#line 1578 "astgen.c"
        break;
      case 107:
#line 289 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(parameter, String(yymsp[0].minor.yy0)); }
#line 1583 "astgen.c"
        break;
      case 108:
#line 292 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(parameter, String(yymsp[-2].minor.yy0), yymsp[0].minor.yy71); }
#line 1588 "astgen.c"
        break;
      case 110:
      case 112:
      case 115:
#line 296 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(parameter_list, yymsp[-2].minor.yy71, yymsp[0].minor.yy71); }
#line 1595 "astgen.c"
        break;
      case 117:
#line 317 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(for_statement, yymsp[-5].minor.yy71, yymsp[-4].minor.yy71, yymsp[-2].minor.yy71, yymsp[0].minor.yy71); }
#line 1600 "astgen.c"
        break;
      case 120:
#line 328 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(foreach_statement, yymsp[-4].minor.yy71, yymsp[-2].minor.yy71, yymsp[0].minor.yy71); }
#line 1605 "astgen.c"
        break;
      case 121:
#line 339 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(if_statement, yymsp[-2].minor.yy71, yymsp[0].minor.yy71); }
#line 1610 "astgen.c"
        break;
      case 122:
#line 340 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(if_statement, yymsp[-4].minor.yy71, yymsp[-2].minor.yy71, yymsp[0].minor.yy71); }
#line 1615 "astgen.c"
        break;
      case 123:
#line 348 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(while_statement, yymsp[-2].minor.yy71,  yymsp[0].minor.yy71); }
#line 1620 "astgen.c"
        break;
      case 124:
#line 356 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(switch_statement, yymsp[-4].minor.yy71, yymsp[-1].minor.yy39); }
#line 1625 "astgen.c"
        break;
      case 125:
#line 360 "astgen.in"
{ yygotominor.yy39 = new AstList; }
#line 1630 "astgen.c"
        break;
      case 126:
      case 127:
#line 361 "astgen.in"
{ yygotominor.yy39 = yymsp[-1].minor.yy39; yygotominor.yy39->push_back(yymsp[0].minor.yy71); }
#line 1636 "astgen.c"
        break;
      case 128:
#line 365 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(switch_case, yymsp[-2].minor.yy71, yymsp[0].minor.yy71); }
#line 1641 "astgen.c"
        break;
      case 129:
#line 368 "astgen.in"
{ yygotominor.yy71 = p->AllocAst(default_case, yymsp[0].minor.yy71); }
#line 1646 "astgen.c"
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
#line 1694 "astgen.c"
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
#line 1712 "astgen.c"
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

