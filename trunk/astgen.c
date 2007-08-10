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
#define YYNSTATE 220
#define YYNRULE 126
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
 /*     0 */   189,  347,  218,  215,    9,  214,  210,  207,  206,  204,
 /*    10 */   202,  199,  198,  196,  195,  194,  192,  191,  120,  188,
 /*    20 */   216,  187,   58,  100,  182,   76,  173,  168,  150,  169,
 /*    30 */   166,   92,  125,  189,  136,  102,   88,    8,  214,  210,
 /*    40 */   207,  206,  204,  202,  199,  198,  196,  195,  194,  192,
 /*    50 */   191,  120,  188,   12,  187,   58,  147,  182,   76,  173,
 /*    60 */   168,   57,  169,  166,   46,   47,   36,  136,  102,   88,
 /*    70 */    37,   38,   39,   41,   42,   43,   44,   45,   46,   47,
 /*    80 */    36,  189,  112,  125,  193,    8,  214,  210,  207,  206,
 /*    90 */   204,  202,  199,  198,  196,  195,  194,  192,  191,  120,
 /*   100 */   188,   54,  187,   58,  158,  182,   76,  173,  168,  117,
 /*   110 */   169,  166,  114,   15,   18,  136,  102,   88,   53,  197,
 /*   120 */   188,  105,  187,   58,  127,  182,   76,  173,  168,  189,
 /*   130 */   169,  166,  190,    6,  214,  210,  207,  206,  204,  202,
 /*   140 */   199,  198,  196,  195,  194,  192,  191,  120,  188,   56,
 /*   150 */   187,   58,   26,  182,   76,  173,  168,  167,  169,  166,
 /*   160 */   184,  185,  189,  136,  102,   88,  179,  157,  210,  207,
 /*   170 */   206,  204,  202,  199,  198,  196,  195,  194,  192,  191,
 /*   180 */   120,  188,   21,  187,   58,  178,  182,   76,  173,  168,
 /*   190 */    27,  169,  166,  217,  126,  170,  136,  102,   88,  116,
 /*   200 */   189,    5,   17,   60,  205,  157,  210,  207,  206,  204,
 /*   210 */   202,  199,  198,  196,  195,  194,  192,  191,  120,  188,
 /*   220 */   122,  187,   58,    7,  182,   76,  173,  168,  130,  169,
 /*   230 */   166,  114,   61,   97,  136,  102,   88,  133,  189,  203,
 /*   240 */   116,   11,  156,  200,  210,  207,  206,  204,  202,  199,
 /*   250 */   198,  196,  195,  194,  192,  191,  120,  188,  135,  187,
 /*   260 */    58,   85,  182,   76,  173,  168,   32,  169,  166,   31,
 /*   270 */   189,   14,  136,  102,   88,  211,  210,  207,  206,  204,
 /*   280 */   202,  199,  198,  196,  195,  194,  192,  191,  120,  188,
 /*   290 */    19,  187,   58,   13,  182,   76,  173,  168,  138,  169,
 /*   300 */   166,   15,  189,    1,  136,  102,   88,  164,  210,  207,
 /*   310 */   206,  204,  202,  199,  198,  196,  195,  194,  192,  191,
 /*   320 */   120,  188,   64,  187,   58,  149,  182,   76,  173,  168,
 /*   330 */     4,  169,  166,   23,  189,   63,  136,  102,   88,  171,
 /*   340 */   210,  207,  206,  204,  202,  199,  198,  196,  195,  194,
 /*   350 */   192,  191,  120,  188,  140,  187,   58,    2,  182,   76,
 /*   360 */   173,  168,   30,  169,  166,   98,  189,   93,  136,  102,
 /*   370 */    88,  176,  210,  207,  206,  204,  202,  199,  198,  196,
 /*   380 */   195,  194,  192,  191,  120,  188,   10,  187,   58,   25,
 /*   390 */   182,   76,  173,  168,  146,  169,  166,   52,  189,  348,
 /*   400 */   136,  102,   88,  111,  210,  207,  206,  204,  202,  199,
 /*   410 */   198,  196,  195,  194,  192,  191,  120,  188,  348,  187,
 /*   420 */    58,  183,  182,   76,  173,  168,   51,  169,  166,  348,
 /*   430 */   348,  348,  136,  102,   88,  348,   59,  101,   50,   49,
 /*   440 */   348,   16,  348,   55,   24,  348,  165,  163,  162,  161,
 /*   450 */   160,  159,  348,  123,    3,  219,  118,   20,  108,  106,
 /*   460 */    57,   96,   90,   82,   51,  348,  348,  113,   87,   89,
 /*   470 */    86,  348,  209,  213,  121,  109,   50,   49,  348,   16,
 /*   480 */   348,   55,   24,  348,  165,  163,  162,  161,  160,  159,
 /*   490 */   348,  123,    3,  124,  118,   20,  108,  106,   57,   96,
 /*   500 */    90,   82,   51,  348,  348,  113,   87,   89,   44,   45,
 /*   510 */    46,   47,   36,  348,   50,   49,  348,   16,  348,   55,
 /*   520 */    24,  348,  165,  163,  162,  161,  160,  159,  348,  123,
 /*   530 */     3,  348,  118,   20,  108,  106,   57,   96,   90,   82,
 /*   540 */   348,  348,  348,  113,   87,   89,   33,   40,   34,   48,
 /*   550 */    35,   37,   38,   39,   41,   42,   43,   44,   45,   46,
 /*   560 */    47,   36,  145,  144,  143,  142,  141,  139,   28,   40,
 /*   570 */    34,   48,   35,   37,   38,   39,   41,   42,   43,   44,
 /*   580 */    45,   46,   47,   36,   34,   48,   35,   37,   38,   39,
 /*   590 */    41,   42,   43,   44,   45,   46,   47,   36,   51,   39,
 /*   600 */    41,   42,   43,   44,   45,   46,   47,   36,  348,  348,
 /*   610 */    50,   49,  348,   16,  348,   84,   24,  348,  165,  163,
 /*   620 */   162,  161,  160,  159,  348,  123,  165,  163,  162,  161,
 /*   630 */   160,  159,   57,  181,  180,   62,   29,   48,   35,   37,
 /*   640 */    38,   39,   41,   42,   43,   44,   45,   46,   47,   36,
 /*   650 */   186,   51,  187,   58,  348,  182,   76,  173,  168,  348,
 /*   660 */   169,  166,  348,   50,   49,  348,   16,  348,   84,   24,
 /*   670 */   348,  165,  163,  162,  161,  160,  159,   51,  131,  134,
 /*   680 */   348,  182,   76,  173,  168,  348,  169,  166,  348,   50,
 /*   690 */    49,  348,   16,  348,   84,   24,  172,  165,  163,  162,
 /*   700 */   161,  160,  159,  152,  188,  201,  187,   58,  348,  182,
 /*   710 */    76,  173,  168,   79,  169,  166,  120,  188,  148,  187,
 /*   720 */    58,  348,  182,   76,  173,   91,  348,  169,  166,  137,
 /*   730 */   348,  187,   58,  107,  182,   76,  173,  168,  348,  169,
 /*   740 */   166,  177,   76,  173,  168,   22,  169,  166,  348,  348,
 /*   750 */   348,   35,   37,   38,   39,   41,   42,   43,   44,   45,
 /*   760 */    46,   47,   36,  348,   72,   51,  182,   76,  173,  168,
 /*   770 */   348,  169,  166,  348,  348,  348,  348,   50,   49,  348,
 /*   780 */    16,  348,   84,   24,  348,  165,  163,  162,  161,  160,
 /*   790 */   159,  348,  348,  348,  154,  188,  348,  187,   58,  348,
 /*   800 */   182,   76,  173,  168,  348,  169,  166,   80,  153,  348,
 /*   810 */   348,  152,  188,  348,  187,   58,  348,  182,   76,  173,
 /*   820 */   168,  348,  169,  166,  348,  348,  151,  348,  348,  348,
 /*   830 */   154,  188,  348,  187,   58,  348,  182,   76,  173,  168,
 /*   840 */   348,  169,  166,  132,  155,  182,   76,  173,  168,  348,
 /*   850 */   169,  166,  348,  348,  348,  103,  188,  348,  187,   58,
 /*   860 */   348,  182,   76,  173,  168,  348,  169,  166,   83,  188,
 /*   870 */   348,  187,   58,  348,  182,   76,  173,  168,  348,  169,
 /*   880 */   166,  110,  188,  348,  187,   58,  348,  182,   76,  173,
 /*   890 */   168,  348,  169,  166,  348,  115,  188,  348,  187,   58,
 /*   900 */   348,  182,   76,  173,  168,  348,  169,  166,   95,  188,
 /*   910 */   348,  187,   58,  348,  182,   76,  173,  168,  348,  169,
 /*   920 */   166,  348,  208,  188,  348,  187,   58,  348,  182,   76,
 /*   930 */   173,  168,  348,  169,  166,  348,  119,  188,  348,  187,
 /*   940 */    58,  348,  182,   76,  173,  168,  348,  169,  166,  128,
 /*   950 */   188,  348,  187,   58,  348,  182,   76,  173,  168,  348,
 /*   960 */   169,  166,   81,  188,  348,  187,   58,  348,  182,   76,
 /*   970 */   173,  168,  348,  169,  166,  348,  104,  188,  348,  187,
 /*   980 */    58,  348,  182,   76,  173,  168,  348,  169,  166,   94,
 /*   990 */   188,  348,  187,   58,  348,  182,   76,  173,  168,  348,
 /*  1000 */   169,  166,   73,  348,  182,   76,  173,  168,  348,  169,
 /*  1010 */   166,   68,  348,  182,   76,  173,  168,  348,  169,  166,
 /*  1020 */   348,   74,  348,  182,   76,  173,  168,  348,  169,  166,
 /*  1030 */    65,  348,  182,   76,  173,  168,  348,  169,  166,   75,
 /*  1040 */   348,  182,   76,  173,  168,  348,  169,  166,   67,  348,
 /*  1050 */   182,   76,  173,  168,  348,  169,  166,  348,   70,  348,
 /*  1060 */   182,   76,  173,  168,  348,  169,  166,  129,  348,  182,
 /*  1070 */    76,  173,  168,  348,  169,  166,  348,   69,  348,  182,
 /*  1080 */    76,  173,  168,  348,  169,  166,   71,  348,  182,   76,
 /*  1090 */   173,  168,  348,  169,  166,   66,  348,  182,   76,  173,
 /*  1100 */   168,  348,  169,  166,  348,   78,  348,  182,   76,  173,
 /*  1110 */   168,  348,  169,  166,   77,  348,  182,   76,  173,  168,
 /*  1120 */   348,  169,  166,  174,   76,  173,  168,  348,  169,  166,
 /*  1130 */   175,   76,  173,  168,  348,  169,  166,  212,  213,  348,
 /*  1140 */    99,
};
static const YYCODETYPE yy_lookahead[] = {
 /*     0 */    59,   60,   61,   62,   63,   64,   65,   66,   67,   68,
 /*    10 */    69,   70,   71,   72,   73,   74,   75,   76,   77,   78,
 /*    20 */    40,   80,   81,   94,   83,   84,   85,   86,   99,   88,
 /*    30 */    89,   96,   97,   59,   93,   94,   95,   63,   64,   65,
 /*    40 */    66,   67,   68,   69,   70,   71,   72,   73,   74,   75,
 /*    50 */    76,   77,   78,   32,   80,   81,   42,   83,   84,   85,
 /*    60 */    86,   47,   88,   89,   14,   15,   16,   93,   94,   95,
 /*    70 */     6,    7,    8,    9,   10,   11,   12,   13,   14,   15,
 /*    80 */    16,   59,   96,   97,  110,   63,   64,   65,   66,   67,
 /*    90 */    68,   69,   70,   71,   72,   73,   74,   75,   76,   77,
 /*   100 */    78,  107,   80,   81,   29,   83,   84,   85,   86,   35,
 /*   110 */    88,   89,   30,   31,   39,   93,   94,   95,   39,   77,
 /*   120 */    78,   79,   80,   81,   40,   83,   84,   85,   86,   59,
 /*   130 */    88,   89,  110,   63,   64,   65,   66,   67,   68,   69,
 /*   140 */    70,   71,   72,   73,   74,   75,   76,   77,   78,   98,
 /*   150 */    80,   81,   17,   83,   84,   85,   86,   32,   88,   89,
 /*   160 */   108,  109,   59,   93,   94,   95,   86,   64,   65,   66,
 /*   170 */    67,   68,   69,   70,   71,   72,   73,   74,   75,   76,
 /*   180 */    77,   78,   17,   80,   81,   30,   83,   84,   85,   86,
 /*   190 */    31,   88,   89,  103,   97,   32,   93,   94,   95,   30,
 /*   200 */    59,   32,   39,   39,  101,   64,   65,   66,   67,   68,
 /*   210 */    69,   70,   71,   72,   73,   74,   75,   76,   77,   78,
 /*   220 */    30,   80,   81,   53,   83,   84,   85,   86,   40,   88,
 /*   230 */    89,   30,   39,   88,   93,   94,   95,   40,   59,   40,
 /*   240 */    30,   32,  101,   64,   65,   66,   67,   68,   69,   70,
 /*   250 */    71,   72,   73,   74,   75,   76,   77,   78,   40,   80,
 /*   260 */    81,   32,   83,   84,   85,   86,   82,   88,   89,   24,
 /*   270 */    59,   31,   93,   94,   95,   64,   65,   66,   67,   68,
 /*   280 */    69,   70,   71,   72,   73,   74,   75,   76,   77,   78,
 /*   290 */    40,   80,   81,   32,   83,   84,   85,   86,   40,   88,
 /*   300 */    89,   31,   59,   24,   93,   94,   95,   64,   65,   66,
 /*   310 */    67,   68,   69,   70,   71,   72,   73,   74,   75,   76,
 /*   320 */    77,   78,   41,   80,   81,   40,   83,   84,   85,   86,
 /*   330 */    32,   88,   89,   31,   59,   41,   93,   94,   95,   64,
 /*   340 */    65,   66,   67,   68,   69,   70,   71,   72,   73,   74,
 /*   350 */    75,   76,   77,   78,   40,   80,   81,   24,   83,   84,
 /*   360 */    85,   86,   31,   88,   89,   30,   59,   30,   93,   94,
 /*   370 */    95,   64,   65,   66,   67,   68,   69,   70,   71,   72,
 /*   380 */    73,   74,   75,   76,   77,   78,   32,   80,   81,   51,
 /*   390 */    83,   84,   85,   86,   29,   88,   89,   31,   59,  111,
 /*   400 */    93,   94,   95,   64,   65,   66,   67,   68,   69,   70,
 /*   410 */    71,   72,   73,   74,   75,   76,   77,   78,  111,   80,
 /*   420 */    81,   42,   83,   84,   85,   86,   13,   88,   89,  111,
 /*   430 */   111,  111,   93,   94,   95,  111,   57,   58,   25,   26,
 /*   440 */   111,   28,  111,   30,   31,  111,   33,   34,   35,   36,
 /*   450 */    37,   38,  111,   40,   41,   42,   43,   44,   45,   46,
 /*   460 */    47,   48,   49,   50,   13,  111,  111,   54,   55,   56,
 /*   470 */   100,  111,  102,  103,  104,  105,   25,   26,  111,   28,
 /*   480 */   111,   30,   31,  111,   33,   34,   35,   36,   37,   38,
 /*   490 */   111,   40,   41,   42,   43,   44,   45,   46,   47,   48,
 /*   500 */    49,   50,   13,  111,  111,   54,   55,   56,   12,   13,
 /*   510 */    14,   15,   16,  111,   25,   26,  111,   28,  111,   30,
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
 /*   670 */   111,   33,   34,   35,   36,   37,   38,   13,   40,   81,
 /*   680 */   111,   83,   84,   85,   86,  111,   88,   89,  111,   25,
 /*   690 */    26,  111,   28,  111,   30,   31,   32,   33,   34,   35,
 /*   700 */    36,   37,   38,   77,   78,   66,   80,   81,  111,   83,
 /*   710 */    84,   85,   86,   87,   88,   89,   77,   78,   92,   80,
 /*   720 */    81,  111,   83,   84,   85,   86,  111,   88,   89,   78,
 /*   730 */   111,   80,   81,   94,   83,   84,   85,   86,  111,   88,
 /*   740 */    89,   83,   84,   85,   86,  106,   88,   89,  111,  111,
 /*   750 */   111,    5,    6,    7,    8,    9,   10,   11,   12,   13,
 /*   760 */    14,   15,   16,  111,   81,   13,   83,   84,   85,   86,
 /*   770 */   111,   88,   89,  111,  111,  111,  111,   25,   26,  111,
 /*   780 */    28,  111,   30,   31,  111,   33,   34,   35,   36,   37,
 /*   790 */    38,  111,  111,  111,   77,   78,  111,   80,   81,  111,
 /*   800 */    83,   84,   85,   86,  111,   88,   89,   90,   91,  111,
 /*   810 */   111,   77,   78,  111,   80,   81,  111,   83,   84,   85,
 /*   820 */    86,  111,   88,   89,  111,  111,   92,  111,  111,  111,
 /*   830 */    77,   78,  111,   80,   81,  111,   83,   84,   85,   86,
 /*   840 */   111,   88,   89,   81,   91,   83,   84,   85,   86,  111,
 /*   850 */    88,   89,  111,  111,  111,   77,   78,  111,   80,   81,
 /*   860 */   111,   83,   84,   85,   86,  111,   88,   89,   77,   78,
 /*   870 */   111,   80,   81,  111,   83,   84,   85,   86,  111,   88,
 /*   880 */    89,   77,   78,  111,   80,   81,  111,   83,   84,   85,
 /*   890 */    86,  111,   88,   89,  111,   77,   78,  111,   80,   81,
 /*   900 */   111,   83,   84,   85,   86,  111,   88,   89,   77,   78,
 /*   910 */   111,   80,   81,  111,   83,   84,   85,   86,  111,   88,
 /*   920 */    89,  111,   77,   78,  111,   80,   81,  111,   83,   84,
 /*   930 */    85,   86,  111,   88,   89,  111,   77,   78,  111,   80,
 /*   940 */    81,  111,   83,   84,   85,   86,  111,   88,   89,   77,
 /*   950 */    78,  111,   80,   81,  111,   83,   84,   85,   86,  111,
 /*   960 */    88,   89,   77,   78,  111,   80,   81,  111,   83,   84,
 /*   970 */    85,   86,  111,   88,   89,  111,   77,   78,  111,   80,
 /*   980 */    81,  111,   83,   84,   85,   86,  111,   88,   89,   77,
 /*   990 */    78,  111,   80,   81,  111,   83,   84,   85,   86,  111,
 /*  1000 */    88,   89,   81,  111,   83,   84,   85,   86,  111,   88,
 /*  1010 */    89,   81,  111,   83,   84,   85,   86,  111,   88,   89,
 /*  1020 */   111,   81,  111,   83,   84,   85,   86,  111,   88,   89,
 /*  1030 */    81,  111,   83,   84,   85,   86,  111,   88,   89,   81,
 /*  1040 */   111,   83,   84,   85,   86,  111,   88,   89,   81,  111,
 /*  1050 */    83,   84,   85,   86,  111,   88,   89,  111,   81,  111,
 /*  1060 */    83,   84,   85,   86,  111,   88,   89,   81,  111,   83,
 /*  1070 */    84,   85,   86,  111,   88,   89,  111,   81,  111,   83,
 /*  1080 */    84,   85,   86,  111,   88,   89,   81,  111,   83,   84,
 /*  1090 */    85,   86,  111,   88,   89,   81,  111,   83,   84,   85,
 /*  1100 */    86,  111,   88,   89,  111,   81,  111,   83,   84,   85,
 /*  1110 */    86,  111,   88,   89,   81,  111,   83,   84,   85,   86,
 /*  1120 */   111,   88,   89,   83,   84,   85,   86,  111,   88,   89,
 /*  1130 */    83,   84,   85,   86,  111,   88,   89,  102,  103,  111,
 /*  1140 */   105,
};
#define YY_SHIFT_USE_DFLT (-21)
#define YY_SHIFT_MAX 122
static const short yy_shift_ofst[] = {
 /*     0 */   489,  489,  489,  413,  489,  489,  451,  489,  489,  489,
 /*    10 */   489,  489,  489,  489,  585,  664,  752,  752,  752,  752,
 /*    20 */   638,  752,  752,  752,  752,  752,  752,  752,  752,  752,
 /*    30 */   752,  752,  752,  752,  752,  752,  752,  752,  752,  752,
 /*    40 */   752,  752,  752,  752,  752,  752,  752,  752,  752,  752,
 /*    50 */   752,  752,  169,  210,  379,   82,   14,  201,  545,  593,
 /*    60 */   201,  190,  155,  -21,  -21,  567,  581,  633,  746,   64,
 /*    70 */   591,  591,  496,  496,  496,  496,  608,   50,   50,  163,
 /*    80 */    75,  229,  240,  261,  270,  281,  298,  302,  314,  331,
 /*    90 */   337,  338,  164,  366,  365,  354,  335,  333,  294,  193,
 /*   100 */   285,  279,  258,  250,  245,  209,  218,  199,  197,  193,
 /*   110 */   188,  170,  164,  159,  165,  125,  135,   84,   74,   21,
 /*   120 */   -20,   79,  135,
};
#define YY_REDUCE_USE_DFLT (-72)
#define YY_REDUCE_MAX 64
static const short yy_reduce_ofst[] = {
 /*     0 */   -59,  -26,   22,   70,  141,  103,  211,  275,  211,  211,
 /*    10 */   243,  179,  339,  307,  639,  626,  717,  734,  753,   42,
 /*    20 */   804,  872,  778,  791,  818,  831,  845,  859,  899,  912,
 /*    30 */   885,  651,  572,  949,  967,  996,  986, 1005,  977,  958,
 /*    40 */  1014,  683,  921,  940, 1024, 1033,  598,  762,  930, 1040,
 /*    50 */  1047,  658,  370, 1035,   52,  -14,  -71,  -65,  184,  145,
 /*    60 */    97,   90,   80,   51,   -6,
};
static const YYACTIONTYPE yy_default[] = {
 /*     0 */   224,  346,  346,  346,  346,  346,  346,  346,  345,  225,
 /*    10 */   346,  346,  346,  346,  346,  346,  346,  346,  346,  240,
 /*    20 */   346,  346,  346,  346,  346,  346,  346,  346,  346,  346,
 /*    30 */   346,  346,  346,  346,  346,  346,  346,  346,  346,  346,
 /*    40 */   346,  346,  346,  346,  346,  346,  346,  346,  346,  346,
 /*    50 */   346,  346,  346,  346,  346,  290,  346,  346,  250,  346,
 /*    60 */   346,  346,  346,  318,  340,  253,  254,  255,  256,  257,
 /*    70 */   259,  258,  261,  262,  263,  260,  269,  265,  264,  346,
 /*    80 */   346,  346,  346,  346,  290,  346,  346,  346,  346,  346,
 /*    90 */   346,  281,  310,  346,  346,  346,  346,  346,  346,  330,
 /*   100 */   346,  346,  346,  346,  346,  346,  346,  346,  346,  329,
 /*   110 */   346,  336,  311,  346,  312,  346,  322,  346,  346,  346,
 /*   120 */   346,  328,  346,  298,  301,  314,  315,  302,  313,  268,
 /*   130 */   303,  304,  267,  305,  266,  306,  307,  251,  308,  249,
 /*   140 */   309,  248,  247,  246,  245,  244,  277,  316,  296,  317,
 /*   150 */   319,  297,  295,  292,  294,  293,  320,  331,  291,  289,
 /*   160 */   288,  287,  286,  285,  335,  284,  283,  282,  281,  280,
 /*   170 */   279,  337,  278,  273,  272,  271,  338,  270,  290,  276,
 /*   180 */   275,  274,  252,  339,  341,  342,  243,  242,  239,  238,
 /*   190 */   343,  237,  236,  344,  235,  234,  233,  241,  232,  231,
 /*   200 */   332,  333,  230,  334,  229,  321,  228,  227,  323,  324,
 /*   210 */   226,  223,  325,  326,  222,  221,  299,  327,  220,  300,
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
  "argument",      "function_declaration",  "var_declaration",  "struct_declaration",
  "declarator_sequence",  "declarator",    "struct_members",  "struct_declseq",
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
 /*  88 */ "declaration_statement ::= var_declaration SEMICOLON",
 /*  89 */ "declaration_statement ::= struct_declaration SEMICOLON",
 /*  90 */ "var_declaration ::= VAR declarator_sequence",
 /*  91 */ "struct_declaration ::= IDENTIFIER declarator_sequence",
 /*  92 */ "declarator ::= IDENTIFIER",
 /*  93 */ "declarator ::= IDENTIFIER ASSIGN expression",
 /*  94 */ "declarator_sequence ::= declarator",
 /*  95 */ "declarator_sequence ::= declarator_sequence COMMA declarator",
 /*  96 */ "struct_declaration ::= STRUCT IDENTIFIER LBRACE struct_members RBRACE",
 /*  97 */ "struct_declseq ::= var_declaration SEMICOLON",
 /*  98 */ "struct_members ::=",
 /*  99 */ "struct_members ::= struct_members struct_declseq",
 /* 100 */ "function_declaration ::= FUNCTION IDENTIFIER LPAREN parameter_list RPAREN function_body",
 /* 101 */ "function_declaration ::= FUNCTION IDENTIFIER LPAREN RPAREN function_body",
 /* 102 */ "parameter ::= IDENTIFIER",
 /* 103 */ "opt_parameter ::= IDENTIFIER ASSIGN expression",
 /* 104 */ "parameters ::= parameter",
 /* 105 */ "parameters ::= parameters COMMA parameter",
 /* 106 */ "opt_parameters ::= opt_parameter",
 /* 107 */ "opt_parameters ::= opt_parameters COMMA opt_parameter",
 /* 108 */ "parameter_list ::= parameters",
 /* 109 */ "parameter_list ::= opt_parameters",
 /* 110 */ "parameter_list ::= parameters COMMA opt_parameters",
 /* 111 */ "function_body ::= statement",
 /* 112 */ "for_statement ::= FOR LPAREN for_init_statement expression SEMICOLON expression_opt RPAREN statement",
 /* 113 */ "for_init_statement ::= expression_statement",
 /* 114 */ "for_init_statement ::= var_declaration SEMICOLON",
 /* 115 */ "foreach_statement ::= FOR LPAREN id_expression IN expression RPAREN statement",
 /* 116 */ "if_statement ::= IF LPAREN expression RPAREN statement",
 /* 117 */ "if_statement ::= IF LPAREN expression RPAREN statement ELSE statement",
 /* 118 */ "while_statement ::= WHILE LPAREN expression RPAREN statement",
 /* 119 */ "switch_statement ::= SWITCH LPAREN expression RPAREN LBRACE switch_body RBRACE",
 /* 120 */ "switch_body ::=",
 /* 121 */ "switch_body ::= switch_body switch_case",
 /* 122 */ "switch_body ::= switch_body default_case",
 /* 123 */ "switch_case ::= CASE literal COLON case_statements",
 /* 124 */ "default_case ::= DEFAULT COLON case_statements",
 /* 125 */ "case_statements ::= statement_sequence",
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
  { 94, 2 },
  { 95, 2 },
  { 97, 1 },
  { 97, 3 },
  { 96, 1 },
  { 96, 3 },
  { 95, 5 },
  { 99, 2 },
  { 98, 0 },
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
#line 1135 "astgen.c"
        break;
      case 1:
#line 75 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(translation_unit, yymsp[0].minor.yy25); }
#line 1140 "astgen.c"
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
      case 104:
      case 106:
      case 108:
      case 109:
      case 111:
      case 113:
      case 125:
#line 78 "astgen.in"
{ yygotominor.yy25 = yymsp[0].minor.yy25; }
#line 1179 "astgen.c"
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
#line 1193 "astgen.c"
        break;
      case 4:
      case 20:
      case 98:
#line 92 "astgen.in"
{ yygotominor.yy25 = 0; }
#line 1200 "astgen.c"
        break;
      case 18:
      case 78:
#line 110 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(empty_statement); }
#line 1206 "astgen.c"
        break;
      case 23:
#line 126 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(assignment_expression, yymsp[-1].minor.yy66, yymsp[-2].minor.yy25, yymsp[0].minor.yy25); }
#line 1211 "astgen.c"
        break;
      case 24:
#line 130 "astgen.in"
{ yygotominor.yy66 = op_assign; }
#line 1216 "astgen.c"
        break;
      case 25:
#line 131 "astgen.in"
{ yygotominor.yy66 = op_assadd; }
#line 1221 "astgen.c"
        break;
      case 26:
#line 132 "astgen.in"
{ yygotominor.yy66 = op_asssub; }
#line 1226 "astgen.c"
        break;
      case 27:
#line 133 "astgen.in"
{ yygotominor.yy66 = op_assmul; }
#line 1231 "astgen.c"
        break;
      case 28:
#line 134 "astgen.in"
{ yygotominor.yy66 = op_assdiv; }
#line 1236 "astgen.c"
        break;
      case 29:
#line 135 "astgen.in"
{ yygotominor.yy66 = op_assmod; }
#line 1241 "astgen.c"
        break;
      case 31:
#line 139 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(ternary_expression, yymsp[-4].minor.yy25, yymsp[-2].minor.yy25, yymsp[0].minor.yy25); }
#line 1246 "astgen.c"
        break;
      case 33:
#line 143 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(binary_expression, op_logor,   yymsp[-2].minor.yy25, yymsp[0].minor.yy25); }
#line 1251 "astgen.c"
        break;
      case 34:
#line 144 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(binary_expression, op_logand,  yymsp[-2].minor.yy25, yymsp[0].minor.yy25); }
#line 1256 "astgen.c"
        break;
      case 35:
#line 145 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(binary_expression, op_bitor,   yymsp[-2].minor.yy25, yymsp[0].minor.yy25); }
#line 1261 "astgen.c"
        break;
      case 36:
#line 146 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(binary_expression, op_bitxor,  yymsp[-2].minor.yy25, yymsp[0].minor.yy25); }
#line 1266 "astgen.c"
        break;
      case 37:
#line 147 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(binary_expression, op_bitand,  yymsp[-2].minor.yy25, yymsp[0].minor.yy25); }
#line 1271 "astgen.c"
        break;
      case 38:
#line 148 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(binary_expression, op_eq,   yymsp[-2].minor.yy25, yymsp[0].minor.yy25); }
#line 1276 "astgen.c"
        break;
      case 39:
#line 149 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(binary_expression, op_ne,   yymsp[-2].minor.yy25, yymsp[0].minor.yy25); }
#line 1281 "astgen.c"
        break;
      case 40:
#line 150 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(binary_expression, op_lt,   yymsp[-2].minor.yy25, yymsp[0].minor.yy25); }
#line 1286 "astgen.c"
        break;
      case 41:
#line 151 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(binary_expression, op_le,   yymsp[-2].minor.yy25, yymsp[0].minor.yy25); }
#line 1291 "astgen.c"
        break;
      case 42:
#line 152 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(binary_expression, op_gt,   yymsp[-2].minor.yy25, yymsp[0].minor.yy25); }
#line 1296 "astgen.c"
        break;
      case 43:
#line 153 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(binary_expression, op_ge,   yymsp[-2].minor.yy25, yymsp[0].minor.yy25); }
#line 1301 "astgen.c"
        break;
      case 44:
#line 154 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(binary_expression, op_add,  yymsp[-2].minor.yy25, yymsp[0].minor.yy25); }
#line 1306 "astgen.c"
        break;
      case 45:
#line 155 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(binary_expression, op_sub,  yymsp[-2].minor.yy25, yymsp[0].minor.yy25); }
#line 1311 "astgen.c"
        break;
      case 46:
#line 156 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(binary_expression, op_mul,  yymsp[-2].minor.yy25, yymsp[0].minor.yy25); }
#line 1316 "astgen.c"
        break;
      case 47:
#line 157 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(binary_expression, op_div,  yymsp[-2].minor.yy25, yymsp[0].minor.yy25); }
#line 1321 "astgen.c"
        break;
      case 48:
#line 158 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(binary_expression, op_mod,  yymsp[-2].minor.yy25, yymsp[0].minor.yy25); }
#line 1326 "astgen.c"
        break;
      case 50:
#line 162 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(prefix_expression, op_negate, yymsp[0].minor.yy25); }
#line 1331 "astgen.c"
        break;
      case 51:
#line 163 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(prefix_expression, op_preinc, yymsp[0].minor.yy25); }
#line 1336 "astgen.c"
        break;
      case 52:
#line 164 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(prefix_expression, op_predec, yymsp[0].minor.yy25); }
#line 1341 "astgen.c"
        break;
      case 54:
#line 168 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(postfix_expression, op_postinc, yymsp[-1].minor.yy25); }
#line 1346 "astgen.c"
        break;
      case 55:
#line 169 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(postfix_expression, op_postdec, yymsp[-1].minor.yy25); }
#line 1351 "astgen.c"
        break;
      case 56:
#line 170 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(member_expression, yymsp[-2].minor.yy25, yymsp[0].minor.yy25); }
#line 1356 "astgen.c"
        break;
      case 57:
#line 171 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(index_expression, yymsp[-3].minor.yy25, yymsp[-1].minor.yy25); }
#line 1361 "astgen.c"
        break;
      case 58:
#line 172 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(function_call, String(yymsp[-2].minor.yy0)); }
#line 1366 "astgen.c"
        break;
      case 59:
#line 173 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(function_call, String(yymsp[-3].minor.yy0), yymsp[-1].minor.yy25); }
#line 1371 "astgen.c"
        break;
      case 62:
      case 88:
      case 89:
      case 97:
      case 114:
#line 178 "astgen.in"
{ yygotominor.yy25 = yymsp[-1].minor.yy25; }
#line 1380 "astgen.c"
        break;
      case 64:
#line 182 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(literal, Variant(String(yymsp[0].minor.yy0), Variant::stInt)); }
#line 1385 "astgen.c"
        break;
      case 65:
#line 183 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(literal, Variant(String(yymsp[0].minor.yy0), Variant::stReal)); }
#line 1390 "astgen.c"
        break;
      case 66:
#line 184 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(literal, Variant(String(yymsp[0].minor.yy0), Variant::stString)); }
#line 1395 "astgen.c"
        break;
      case 67:
#line 185 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(literal, Variant(true));    }
#line 1400 "astgen.c"
        break;
      case 68:
#line 186 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(literal, Variant(false));   }
#line 1405 "astgen.c"
        break;
      case 69:
#line 187 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(literal, Variant());        }
#line 1410 "astgen.c"
        break;
      case 70:
#line 190 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(lvalue, String(yymsp[0].minor.yy0)); }
#line 1415 "astgen.c"
        break;
      case 71:
#line 193 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(list_literal, yymsp[-1].minor.yy25); }
#line 1420 "astgen.c"
        break;
      case 72:
#line 195 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(list_content, yymsp[0].minor.yy25); }
#line 1425 "astgen.c"
        break;
      case 73:
#line 196 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(list_content, yymsp[-2].minor.yy25, yymsp[0].minor.yy25); }
#line 1430 "astgen.c"
        break;
      case 74:
#line 198 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(list_entry, yymsp[0].minor.yy25); }
#line 1435 "astgen.c"
        break;
      case 75:
#line 207 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(argument, yymsp[0].minor.yy25); }
#line 1440 "astgen.c"
        break;
      case 77:
#line 211 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(argument_list, yymsp[-2].minor.yy25, yymsp[0].minor.yy25); }
#line 1445 "astgen.c"
        break;
      case 79:
#line 220 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(expression_statement, yymsp[-1].minor.yy25); }
#line 1450 "astgen.c"
        break;
      case 80:
#line 223 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(compound_statement); }
#line 1455 "astgen.c"
        break;
      case 81:
#line 224 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(compound_statement, yymsp[-1].minor.yy25); }
#line 1460 "astgen.c"
        break;
      case 82:
#line 227 "astgen.in"
{ p->Parse(yymsp[-1].minor.yy0); yygotominor.yy25 = p->GetRoot(); }
#line 1465 "astgen.c"
        break;
      case 83:
#line 230 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(return_statement, yymsp[-1].minor.yy25); }
#line 1470 "astgen.c"
        break;
      case 84:
#line 231 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(return_statement);    }
#line 1475 "astgen.c"
        break;
      case 85:
#line 234 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(break_statement); }
#line 1480 "astgen.c"
        break;
      case 86:
#line 235 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(continue_statement); }
#line 1485 "astgen.c"
        break;
      case 91:
#line 248 "astgen.in"
{ yygotominor.yy25 = yymsp[0].minor.yy25; yygotominor.yy25->m_a3 = String(yymsp[-1].minor.yy0); }
#line 1490 "astgen.c"
        break;
      case 92:
#line 250 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(variable_declaration, String(yymsp[0].minor.yy0));    }
#line 1495 "astgen.c"
        break;
      case 93:
#line 251 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(variable_declaration, String(yymsp[-2].minor.yy0), yymsp[0].minor.yy25); }
#line 1500 "astgen.c"
        break;
      case 95:
#line 254 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(declaration_sequence, yymsp[-2].minor.yy25, yymsp[0].minor.yy25); }
#line 1505 "astgen.c"
        break;
      case 96:
#line 263 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(struct_declaration, String(yymsp[-3].minor.yy0), yymsp[-1].minor.yy25); }
#line 1510 "astgen.c"
        break;
      case 99:
#line 270 "astgen.in"
{ 
  if(yymsp[-1].minor.yy25) {
    yygotominor.yy25 = p->AllocAst(struct_members, yymsp[-1].minor.yy25, yymsp[0].minor.yy25); 
  }
  else {
    yygotominor.yy25 = yymsp[0].minor.yy25;
  }
}
#line 1522 "astgen.c"
        break;
      case 100:
#line 285 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(function_declaration, String(yymsp[-4].minor.yy0), yymsp[-2].minor.yy25, yymsp[0].minor.yy25); }
#line 1527 "astgen.c"
        break;
      case 101:
#line 286 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(function_declaration, String(yymsp[-3].minor.yy0), AstData(), yymsp[0].minor.yy25); }
#line 1532 "astgen.c"
        break;
      case 102:
#line 289 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(parameter, String(yymsp[0].minor.yy0)); }
#line 1537 "astgen.c"
        break;
      case 103:
#line 292 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(parameter, String(yymsp[-2].minor.yy0), yymsp[0].minor.yy25); }
#line 1542 "astgen.c"
        break;
      case 105:
      case 107:
      case 110:
#line 296 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(parameter_list, yymsp[-2].minor.yy25, yymsp[0].minor.yy25); }
#line 1549 "astgen.c"
        break;
      case 112:
#line 317 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(for_statement, yymsp[-5].minor.yy25, yymsp[-4].minor.yy25, yymsp[-2].minor.yy25, yymsp[0].minor.yy25); }
#line 1554 "astgen.c"
        break;
      case 115:
#line 328 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(foreach_statement, yymsp[-4].minor.yy25, yymsp[-2].minor.yy25, yymsp[0].minor.yy25); }
#line 1559 "astgen.c"
        break;
      case 116:
#line 339 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(if_statement, yymsp[-2].minor.yy25, yymsp[0].minor.yy25); }
#line 1564 "astgen.c"
        break;
      case 117:
#line 340 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(if_statement, yymsp[-4].minor.yy25, yymsp[-2].minor.yy25, yymsp[0].minor.yy25); }
#line 1569 "astgen.c"
        break;
      case 118:
#line 348 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(while_statement, yymsp[-2].minor.yy25,  yymsp[0].minor.yy25); }
#line 1574 "astgen.c"
        break;
      case 119:
#line 356 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(switch_statement, yymsp[-4].minor.yy25, yymsp[-1].minor.yy97); }
#line 1579 "astgen.c"
        break;
      case 120:
#line 360 "astgen.in"
{ yygotominor.yy97 = new AstList; }
#line 1584 "astgen.c"
        break;
      case 121:
      case 122:
#line 361 "astgen.in"
{ yygotominor.yy97 = yymsp[-1].minor.yy97; yygotominor.yy97->push_back(yymsp[0].minor.yy25); }
#line 1590 "astgen.c"
        break;
      case 123:
#line 365 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(switch_case, yymsp[-2].minor.yy25, yymsp[0].minor.yy25); }
#line 1595 "astgen.c"
        break;
      case 124:
#line 368 "astgen.in"
{ yygotominor.yy25 = p->AllocAst(default_case, yymsp[0].minor.yy25); }
#line 1600 "astgen.c"
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
#line 1648 "astgen.c"
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
#line 1666 "astgen.c"
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

