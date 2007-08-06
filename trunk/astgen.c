/* Driver template for the LEMON parser generator.
** The author disclaims copyright to this source code.
*/
/* First off, code is include which follows the "include" declaration
** in the input file. */
#include <stdio.h>
#line 30 "astgen.in"


#include "tokens.h"
#include "ast.h"

#pragma warning(disable:4065)

#line 17 "astgen.c"
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
**    AstGenParseTOKENTYPE     is the data type used for minor tokens given 
**                       directly to the parser from the tokenizer.
**    YYMINORTYPE        is the data type used for all minor tokens.
**                       This is typically a union of many types, one of
**                       which is AstGenParseTOKENTYPE.  The entry in the union
**                       for base tokens is called "yy0".
**    YYSTACKDEPTH       is the maximum depth of the parser's stack.  If
**                       zero the stack is dynamically sized using realloc()
**    AstGenParseARG_SDECL     A static variable declaration for the %extra_argument
**    AstGenParseARG_PDECL     A parameter declaration for the %extra_argument
**    AstGenParseARG_STORE     Code to store %extra_argument into yypParser
**    AstGenParseARG_FETCH     Code to extract %extra_argument from yypParser
**    YYNSTATE           the combined number of states.
**    YYNRULE            the number of rules in the grammar
**    YYERRORSYMBOL      is the code number of the error symbol.  If not
**                       defined, then do no error processing.
*/
#define YYCODETYPE unsigned char
#define YYNOCODE 105
#define YYACTIONTYPE unsigned short int
#define AstGenParseTOKENTYPE  Token 
typedef union {
  AstGenParseTOKENTYPE yy0;
  opcodes yy94;
  AstList* yy103;
  Ast* yy183;
  int yy209;
} YYMINORTYPE;
#ifndef YYSTACKDEPTH
#define YYSTACKDEPTH 100
#endif
#define AstGenParseARG_SDECL  AstGen* p ;
#define AstGenParseARG_PDECL , AstGen* p 
#define AstGenParseARG_FETCH  AstGen* p  = yypParser->p 
#define AstGenParseARG_STORE yypParser->p  = p 
#define YYNSTATE 198
#define YYNRULE 113
#define YYERRORSYMBOL 58
#define YYERRSYMDT yy209
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
 /*     0 */   312,  120,    9,  188,  183,  179,  176,  175,  165,  162,
 /*    10 */   160,  157,  155,  153,  150,  148,  106,  146,  191,  142,
 /*    20 */    55,   28,  113,   71,  152,  178,   31,  174,  194,   42,
 /*    30 */    44,   47,   75,  135,    7,  188,  183,  179,  176,  175,
 /*    40 */   165,  162,  160,  157,  155,  153,  150,  148,  106,  146,
 /*    50 */   117,  142,   55,    1,  113,   71,  152,  178,  103,  174,
 /*    60 */   194,  137,  130,  131,   75,  135,   33,   46,   37,   45,
 /*    70 */    32,   38,   42,   44,   47,  123,  180,    7,  188,  183,
 /*    80 */   179,  176,  175,  165,  162,  160,  157,  155,  153,  150,
 /*    90 */   148,  106,  146,   13,  142,   55,  184,  113,   71,  152,
 /*   100 */   178,   53,  174,  194,  129,  130,  131,   75,  135,  126,
 /*   110 */    71,  152,  178,    5,  174,  194,   51,  182,  125,  177,
 /*   120 */    58,    6,  188,  183,  179,  176,  175,  165,  162,  160,
 /*   130 */   157,  155,  153,  150,  148,  106,  146,  111,  142,   55,
 /*   140 */   116,  113,   71,  152,  178,    2,  174,  194,   51,  127,
 /*   150 */    17,   75,  135,  144,  183,  179,  176,  175,  165,  162,
 /*   160 */   160,  157,  155,  153,  150,  148,  106,  146,  193,  142,
 /*   170 */    55,   19,  113,   71,  152,  178,  192,  174,  194,   76,
 /*   180 */    95,  196,   75,  135,  107,  112,   57,   29,  143,  144,
 /*   190 */   183,  179,  176,  175,  165,  162,  160,  157,  155,  153,
 /*   200 */   150,  148,  106,  146,   21,  142,   55,  170,  113,   71,
 /*   210 */   152,  178,  122,  174,  194,  192,  166,    4,   75,  135,
 /*   220 */    91,   56,   99,   18,  195,   82,  183,  179,  176,  175,
 /*   230 */   165,  162,  160,  157,  155,  153,  150,  148,  106,  146,
 /*   240 */    54,  142,   55,   59,  113,   71,  152,  178,   30,  174,
 /*   250 */   194,  171,  172,   93,   75,  135,  185,  183,  179,  176,
 /*   260 */   175,  165,  162,  160,  157,  155,  153,  150,  148,  106,
 /*   270 */   146,   14,  142,   55,  149,  113,   71,  152,  178,   48,
 /*   280 */   174,  194,  119,   90,   22,   75,  135,    8,   10,   25,
 /*   290 */    26,   50,   49,   11,   16,   12,   83,   27,   15,  173,
 /*   300 */   169,  168,  140,  136,  133,  313,  109,    3,  197,  104,
 /*   310 */    20,   98,   96,   52,   92,   89,  313,  313,  313,   85,
 /*   320 */    81,   79,  163,  183,  179,  176,  175,  165,  162,  160,
 /*   330 */   157,  155,  153,  150,  148,  106,  146,  313,  142,   55,
 /*   340 */   313,  113,   71,  152,  178,  313,  174,  194,  313,  313,
 /*   350 */   313,   75,  135,  313,  151,  183,  179,  176,  175,  165,
 /*   360 */   162,  160,  157,  155,  153,  150,  148,  106,  146,  313,
 /*   370 */   142,   55,  313,  113,   71,  152,  178,  313,  174,  194,
 /*   380 */   313,  313,  313,   75,  135,  189,  183,  179,  176,  175,
 /*   390 */   165,  162,  160,  157,  155,  153,  150,  148,  106,  146,
 /*   400 */   313,  142,   55,  313,  113,   71,  152,  178,   48,  174,
 /*   410 */   194,  313,  313,  313,   75,  135,  313,  313,  313,  313,
 /*   420 */    50,   49,  313,   16,  313,   83,   27,  313,  173,  169,
 /*   430 */   168,  140,  136,  133,  313,  109,    3,  114,  104,   20,
 /*   440 */    98,   96,   52,   92,   89,  313,  313,  313,   85,   81,
 /*   450 */    79,  158,  183,  179,  176,  175,  165,  162,  160,  157,
 /*   460 */   155,  153,  150,  148,  106,  146,  313,  142,   55,  313,
 /*   470 */   113,   71,  152,  178,   48,  174,  194,  313,  313,  313,
 /*   480 */    75,  135,  313,  313,  313,  313,   50,   49,  313,   16,
 /*   490 */   313,   83,   27,  313,  173,  169,  168,  140,  136,  133,
 /*   500 */   313,  109,    3,  313,  104,   20,   98,   96,   52,   92,
 /*   510 */    89,  313,  313,  313,   85,   81,   79,   41,   40,   39,
 /*   520 */    43,   36,   35,   34,   33,   46,   37,   45,   32,   38,
 /*   530 */    42,   44,   47,  154,  156,  161,  164,  187,  181,   24,
 /*   540 */    40,   39,   43,   36,   35,   34,   33,   46,   37,   45,
 /*   550 */    32,   38,   42,   44,   47,   39,   43,   36,   35,   34,
 /*   560 */    33,   46,   37,   45,   32,   38,   42,   44,   47,   48,
 /*   570 */    35,   34,   33,   46,   37,   45,   32,   38,   42,   44,
 /*   580 */    47,   50,   49,  313,   16,  313,   83,   27,  313,  173,
 /*   590 */   169,  168,  140,  136,  133,  313,  109,   69,  313,  113,
 /*   600 */    71,  152,  178,   52,  174,  194,  313,  313,   43,   36,
 /*   610 */    35,   34,   33,   46,   37,   45,   32,   38,   42,   44,
 /*   620 */    47,  118,   48,  142,   55,  313,  113,   71,  152,  178,
 /*   630 */   313,  174,  194,  313,   50,   49,  313,   16,  313,   83,
 /*   640 */    27,  313,  173,  169,  168,  140,  136,  133,   48,  121,
 /*   650 */    60,  313,  113,   71,  152,  178,  313,  174,  194,  313,
 /*   660 */    50,   49,  313,   16,  313,   83,   27,  159,  173,  169,
 /*   670 */   168,  140,  136,  133,  190,  313,   68,  313,  113,   71,
 /*   680 */   152,  178,  313,  174,  194,  106,  146,  313,  142,   55,
 /*   690 */   313,  113,   71,  152,  102,  313,  174,  194,  313,  313,
 /*   700 */   167,   74,  142,   55,  313,  113,   71,  152,  178,   23,
 /*   710 */   174,  194,  313,  313,  313,   36,   35,   34,   33,   46,
 /*   720 */    37,   45,   32,   38,   42,   44,   47,  313,  110,   48,
 /*   730 */   113,   71,  152,  178,  313,  174,  194,  313,  313,  313,
 /*   740 */   313,   50,   49,  313,   16,  313,   83,   27,  313,  173,
 /*   750 */   169,  168,  140,  136,  133,  173,  169,  168,  140,  136,
 /*   760 */   133,  128,  146,  313,  142,   55,  313,  113,   71,  152,
 /*   770 */   178,  313,  174,  194,   77,  132,  313,  138,  146,  313,
 /*   780 */   142,   55,  313,  113,   71,  152,  178,   78,  174,  194,
 /*   790 */   313,  313,  145,  128,  146,  313,  142,   55,  313,  113,
 /*   800 */    71,  152,  178,  313,  174,  194,  108,  124,  113,   71,
 /*   810 */   152,  178,  313,  174,  194,  313,  313,  186,  146,  105,
 /*   820 */   142,   55,  313,  113,   71,  152,  178,  313,  174,  194,
 /*   830 */   138,  146,  313,  142,   55,  313,  113,   71,  152,  178,
 /*   840 */   313,  174,  194,   87,  146,  141,  142,   55,  313,  113,
 /*   850 */    71,  152,  178,  313,  174,  194,  101,  146,  313,  142,
 /*   860 */    55,  313,  113,   71,  152,  178,  313,  174,  194,   94,
 /*   870 */   146,  313,  142,   55,  313,  113,   71,  152,  178,  313,
 /*   880 */   174,  194,  313,  100,  146,  313,  142,   55,  313,  113,
 /*   890 */    71,  152,  178,  313,  174,  194,   84,  146,  313,  142,
 /*   900 */    55,  313,  113,   71,  152,  178,  313,  174,  194,   80,
 /*   910 */   146,  313,  142,   55,  313,  113,   71,  152,  178,  313,
 /*   920 */   174,  194,   97,  146,  313,  142,   55,  313,  113,   71,
 /*   930 */   152,  178,  313,  174,  194,   86,  146,  313,  142,   55,
 /*   940 */   313,  113,   71,  152,  178,  313,  174,  194,   88,  146,
 /*   950 */   313,  142,   55,  313,  113,   71,  152,  178,  313,  174,
 /*   960 */   194,  313,  134,  146,  313,  142,   55,  313,  113,   71,
 /*   970 */   152,  178,  313,  174,  194,   72,  313,  113,   71,  152,
 /*   980 */   178,  313,  174,  194,   63,  313,  113,   71,  152,  178,
 /*   990 */   313,  174,  194,   62,  313,  113,   71,  152,  178,  313,
 /*  1000 */   174,  194,   67,  313,  113,   71,  152,  178,  313,  174,
 /*  1010 */   194,   73,  313,  113,   71,  152,  178,  313,  174,  194,
 /*  1020 */   313,   64,  313,  113,   71,  152,  178,  313,  174,  194,
 /*  1030 */   313,   61,  313,  113,   71,  152,  178,  313,  174,  194,
 /*  1040 */    65,  313,  113,   71,  152,  178,  313,  174,  194,   70,
 /*  1050 */   313,  113,   71,  152,  178,  313,  174,  194,   66,  313,
 /*  1060 */   113,   71,  152,  178,  313,  174,  194,  115,  313,  113,
 /*  1070 */    71,  152,  178,  313,  174,  194,  313,  147,   71,  152,
 /*  1080 */   178,  313,  174,  194,  139,   71,  152,  178,  313,  174,
 /*  1090 */   194,   32,   38,   42,   44,   47,
};
static const YYCODETYPE yy_lookahead[] = {
 /*     0 */    59,   60,   61,   62,   63,   64,   65,   66,   67,   68,
 /*    10 */    69,   70,   71,   72,   73,   74,   75,   76,   40,   78,
 /*    20 */    79,   50,   81,   82,   83,   84,   24,   86,   87,   14,
 /*    30 */    15,   16,   91,   92,   61,   62,   63,   64,   65,   66,
 /*    40 */    67,   68,   69,   70,   71,   72,   73,   74,   75,   76,
 /*    50 */    40,   78,   79,   24,   81,   82,   83,   84,   35,   86,
 /*    60 */    87,   93,   94,   95,   91,   92,    8,    9,   10,   11,
 /*    70 */    12,   13,   14,   15,   16,   40,  103,   61,   62,   63,
 /*    80 */    64,   65,   66,   67,   68,   69,   70,   71,   72,   73,
 /*    90 */    74,   75,   76,   32,   78,   79,   32,   81,   82,   83,
 /*   100 */    84,  100,   86,   87,   93,   94,   95,   91,   92,   81,
 /*   110 */    82,   83,   84,   32,   86,   87,   39,   40,   40,  103,
 /*   120 */    39,   61,   62,   63,   64,   65,   66,   67,   68,   69,
 /*   130 */    70,   71,   72,   73,   74,   75,   76,   40,   78,   79,
 /*   140 */    29,   81,   82,   83,   84,   24,   86,   87,   39,   40,
 /*   150 */    39,   91,   92,   62,   63,   64,   65,   66,   67,   68,
 /*   160 */    69,   70,   71,   72,   73,   74,   75,   76,   98,   78,
 /*   170 */    79,   40,   81,   82,   83,   84,   30,   86,   87,   96,
 /*   180 */    86,   98,   91,   92,   25,   26,   27,   28,   97,   62,
 /*   190 */    63,   64,   65,   66,   67,   68,   69,   70,   71,   72,
 /*   200 */    73,   74,   75,   76,   17,   78,   79,   42,   81,   82,
 /*   210 */    83,   84,   30,   86,   87,   30,   32,   32,   91,   92,
 /*   220 */    30,   56,   57,   39,   97,   62,   63,   64,   65,   66,
 /*   230 */    67,   68,   69,   70,   71,   72,   73,   74,   75,   76,
 /*   240 */    31,   78,   79,   41,   81,   82,   83,   84,   80,   86,
 /*   250 */    87,  101,  102,   30,   91,   92,   62,   63,   64,   65,
 /*   260 */    66,   67,   68,   69,   70,   71,   72,   73,   74,   75,
 /*   270 */    76,   31,   78,   79,   29,   81,   82,   83,   84,   13,
 /*   280 */    86,   87,   84,   32,   31,   91,   92,   32,   32,   31,
 /*   290 */    31,   25,   26,   32,   28,   52,   30,   31,   31,   33,
 /*   300 */    34,   35,   36,   37,   38,  104,   40,   41,   42,   43,
 /*   310 */    44,   45,   46,   47,   48,   49,  104,  104,  104,   53,
 /*   320 */    54,   55,   62,   63,   64,   65,   66,   67,   68,   69,
 /*   330 */    70,   71,   72,   73,   74,   75,   76,  104,   78,   79,
 /*   340 */   104,   81,   82,   83,   84,  104,   86,   87,  104,  104,
 /*   350 */   104,   91,   92,  104,   62,   63,   64,   65,   66,   67,
 /*   360 */    68,   69,   70,   71,   72,   73,   74,   75,   76,  104,
 /*   370 */    78,   79,  104,   81,   82,   83,   84,  104,   86,   87,
 /*   380 */   104,  104,  104,   91,   92,   62,   63,   64,   65,   66,
 /*   390 */    67,   68,   69,   70,   71,   72,   73,   74,   75,   76,
 /*   400 */   104,   78,   79,  104,   81,   82,   83,   84,   13,   86,
 /*   410 */    87,  104,  104,  104,   91,   92,  104,  104,  104,  104,
 /*   420 */    25,   26,  104,   28,  104,   30,   31,  104,   33,   34,
 /*   430 */    35,   36,   37,   38,  104,   40,   41,   42,   43,   44,
 /*   440 */    45,   46,   47,   48,   49,  104,  104,  104,   53,   54,
 /*   450 */    55,   62,   63,   64,   65,   66,   67,   68,   69,   70,
 /*   460 */    71,   72,   73,   74,   75,   76,  104,   78,   79,  104,
 /*   470 */    81,   82,   83,   84,   13,   86,   87,  104,  104,  104,
 /*   480 */    91,   92,  104,  104,  104,  104,   25,   26,  104,   28,
 /*   490 */   104,   30,   31,  104,   33,   34,   35,   36,   37,   38,
 /*   500 */   104,   40,   41,  104,   43,   44,   45,   46,   47,   48,
 /*   510 */    49,  104,  104,  104,   53,   54,   55,    1,    2,    3,
 /*   520 */     4,    5,    6,    7,    8,    9,   10,   11,   12,   13,
 /*   530 */    14,   15,   16,   17,   18,   19,   20,   21,   22,   23,
 /*   540 */     2,    3,    4,    5,    6,    7,    8,    9,   10,   11,
 /*   550 */    12,   13,   14,   15,   16,    3,    4,    5,    6,    7,
 /*   560 */     8,    9,   10,   11,   12,   13,   14,   15,   16,   13,
 /*   570 */     6,    7,    8,    9,   10,   11,   12,   13,   14,   15,
 /*   580 */    16,   25,   26,  104,   28,  104,   30,   31,  104,   33,
 /*   590 */    34,   35,   36,   37,   38,  104,   40,   79,  104,   81,
 /*   600 */    82,   83,   84,   47,   86,   87,  104,  104,    4,    5,
 /*   610 */     6,    7,    8,    9,   10,   11,   12,   13,   14,   15,
 /*   620 */    16,   76,   13,   78,   79,  104,   81,   82,   83,   84,
 /*   630 */   104,   86,   87,  104,   25,   26,  104,   28,  104,   30,
 /*   640 */    31,  104,   33,   34,   35,   36,   37,   38,   13,   40,
 /*   650 */    79,  104,   81,   82,   83,   84,  104,   86,   87,  104,
 /*   660 */    25,   26,  104,   28,  104,   30,   31,   32,   33,   34,
 /*   670 */    35,   36,   37,   38,   64,  104,   79,  104,   81,   82,
 /*   680 */    83,   84,  104,   86,   87,   75,   76,  104,   78,   79,
 /*   690 */   104,   81,   82,   83,   84,  104,   86,   87,  104,  104,
 /*   700 */    76,   91,   78,   79,  104,   81,   82,   83,   84,   99,
 /*   710 */    86,   87,  104,  104,  104,    5,    6,    7,    8,    9,
 /*   720 */    10,   11,   12,   13,   14,   15,   16,  104,   79,   13,
 /*   730 */    81,   82,   83,   84,  104,   86,   87,  104,  104,  104,
 /*   740 */   104,   25,   26,  104,   28,  104,   30,   31,  104,   33,
 /*   750 */    34,   35,   36,   37,   38,   33,   34,   35,   36,   37,
 /*   760 */    38,   75,   76,  104,   78,   79,  104,   81,   82,   83,
 /*   770 */    84,  104,   86,   87,   88,   89,  104,   75,   76,  104,
 /*   780 */    78,   79,  104,   81,   82,   83,   84,   85,   86,   87,
 /*   790 */   104,  104,   90,   75,   76,  104,   78,   79,  104,   81,
 /*   800 */    82,   83,   84,  104,   86,   87,   79,   89,   81,   82,
 /*   810 */    83,   84,  104,   86,   87,  104,  104,   75,   76,   77,
 /*   820 */    78,   79,  104,   81,   82,   83,   84,  104,   86,   87,
 /*   830 */    75,   76,  104,   78,   79,  104,   81,   82,   83,   84,
 /*   840 */   104,   86,   87,   75,   76,   90,   78,   79,  104,   81,
 /*   850 */    82,   83,   84,  104,   86,   87,   75,   76,  104,   78,
 /*   860 */    79,  104,   81,   82,   83,   84,  104,   86,   87,   75,
 /*   870 */    76,  104,   78,   79,  104,   81,   82,   83,   84,  104,
 /*   880 */    86,   87,  104,   75,   76,  104,   78,   79,  104,   81,
 /*   890 */    82,   83,   84,  104,   86,   87,   75,   76,  104,   78,
 /*   900 */    79,  104,   81,   82,   83,   84,  104,   86,   87,   75,
 /*   910 */    76,  104,   78,   79,  104,   81,   82,   83,   84,  104,
 /*   920 */    86,   87,   75,   76,  104,   78,   79,  104,   81,   82,
 /*   930 */    83,   84,  104,   86,   87,   75,   76,  104,   78,   79,
 /*   940 */   104,   81,   82,   83,   84,  104,   86,   87,   75,   76,
 /*   950 */   104,   78,   79,  104,   81,   82,   83,   84,  104,   86,
 /*   960 */    87,  104,   75,   76,  104,   78,   79,  104,   81,   82,
 /*   970 */    83,   84,  104,   86,   87,   79,  104,   81,   82,   83,
 /*   980 */    84,  104,   86,   87,   79,  104,   81,   82,   83,   84,
 /*   990 */   104,   86,   87,   79,  104,   81,   82,   83,   84,  104,
 /*  1000 */    86,   87,   79,  104,   81,   82,   83,   84,  104,   86,
 /*  1010 */    87,   79,  104,   81,   82,   83,   84,  104,   86,   87,
 /*  1020 */   104,   79,  104,   81,   82,   83,   84,  104,   86,   87,
 /*  1030 */   104,   79,  104,   81,   82,   83,   84,  104,   86,   87,
 /*  1040 */    79,  104,   81,   82,   83,   84,  104,   86,   87,   79,
 /*  1050 */   104,   81,   82,   83,   84,  104,   86,   87,   79,  104,
 /*  1060 */    81,   82,   83,   84,  104,   86,   87,   79,  104,   81,
 /*  1070 */    82,   83,   84,  104,   86,   87,  104,   81,   82,   83,
 /*  1080 */    84,  104,   86,   87,   81,   82,   83,   84,  104,   86,
 /*  1090 */    87,   12,   13,   14,   15,   16,
};
#define YY_SHIFT_USE_DFLT (-30)
#define YY_SHIFT_MAX 106
static const short yy_shift_ofst[] = {
 /*     0 */   461,  461,  461,  266,  461,  461,  395,  461,  461,  461,
 /*    10 */   461,  461,  461,  461,  556,  635,  716,  716,  716,  716,
 /*    20 */   609,  716,  716,  716,  716,  716,  716,  716,  716,  716,
 /*    30 */   716,  716,  716,  716,  716,  716,  716,  716,  716,  716,
 /*    40 */   716,  716,  716,  716,  716,  716,  716,  716,  716,  716,
 /*    50 */   716,  223,  223,  165,  185,  516,  722,  182,  146,  -30,
 /*    60 */   538,  552,  604,  710,  564,   58,   58, 1079, 1079, 1079,
 /*    70 */  1079,  159,   15,   15,   77,  109,   81,  111,  184,  253,
 /*    80 */   256,  259,  243,  267,  261,  258,  255,  251,  245,  240,
 /*    90 */   202,  209,  190,  187,  131,  121,   78,   64,   35,   29,
 /*   100 */     2,  -22,  -29,   10,   23,   61,   97,
};
#define YY_REDUCE_USE_DFLT (-60)
#define YY_REDUCE_MAX 59
static const short yy_reduce_ofst[] = {
 /*     0 */   -59,  -27,   16,   60,  127,   91,  194,  194,  292,  194,
 /*    10 */   260,  163,  389,  323,  610,  702,  686,  718,  755,  742,
 /*    20 */   781,  887,  768,  794,  808,  821,  834,  847,  860,  873,
 /*    30 */   545,  624,  896,  923,  961,  979,  942,  970,  932,  914,
 /*    40 */   952,  571,  988,  905,  649,  518,  597,  727,   28,  996,
 /*    50 */  1003,   11,  -32,  150,   83,  168,   94,  198,   70,    1,
};
static const YYACTIONTYPE yy_default[] = {
 /*     0 */   201,  311,  311,  311,  311,  311,  311,  310,  311,  202,
 /*    10 */   311,  311,  311,  311,  311,  311,  311,  311,  311,  216,
 /*    20 */   311,  311,  311,  311,  311,  311,  311,  311,  311,  311,
 /*    30 */   311,  311,  311,  311,  311,  311,  311,  311,  311,  311,
 /*    40 */   311,  311,  311,  311,  311,  311,  311,  311,  311,  311,
 /*    50 */   311,  311,  311,  311,  311,  226,  311,  311,  311,  305,
 /*    60 */   229,  230,  231,  232,  233,  235,  234,  236,  237,  239,
 /*    70 */   238,  245,  240,  241,  311,  311,  311,  311,  311,  311,
 /*    80 */   311,  311,  301,  266,  311,  311,  311,  311,  311,  311,
 /*    90 */   311,  311,  311,  289,  311,  311,  311,  311,  311,  311,
 /*   100 */   311,  311,  257,  311,  311,  311,  311,  250,  244,  274,
 /*   110 */   243,  275,  251,  228,  277,  242,  267,  278,  219,  252,
 /*   120 */   198,  280,  266,  281,  269,  282,  246,  283,  270,  286,
 /*   130 */   287,  288,  268,  265,  290,  284,  264,  285,  271,  247,
 /*   140 */   263,  273,  218,  291,  296,  272,  215,  248,  214,  253,
 /*   150 */   213,  300,  249,  212,  220,  211,  221,  210,  302,  254,
 /*   160 */   209,  222,  208,  303,  223,  207,  255,  227,  262,  261,
 /*   170 */   304,  306,  307,  260,  256,  206,  205,  308,  257,  204,
 /*   180 */   309,  225,  299,  203,  258,  200,  217,  224,  199,  297,
 /*   190 */   298,  279,  293,  295,  259,  292,  294,  276,
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
  AstGenParseARG_SDECL                /* A place to hold %extra_argument */
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
void AstGenParseTrace(FILE *TraceFILE, char *zTracePrompt){
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
  "FUNCTION",      "FOR",           "IN",            "LOWER_THAN_ELSE",
  "ELSE",          "IF",            "WHILE",         "SWITCH",      
  "CASE",          "DEFAULT",       "error",         "translation_unit",
  "statement_sequence_opt",  "statement_sequence",  "statement",     "include_statement",
  "expression_statement",  "declaration_statement",  "for_statement",  "compound_statement",
  "if_statement",  "while_statement",  "foreach_statement",  "return_statement",
  "switch_statement",  "break_statement",  "continue_statement",  "expression",  
  "assignment_expression",  "expression_opt",  "conditional_expression",  "binary_expression",
  "assignment_operator",  "unary_expression",  "postfix_expression",  "primary_expression",
  "id_expression",  "argument_list",  "literal",       "list_literal",
  "list_content",  "list_entry",    "argument",      "declaration_sequence",
  "function_declaration",  "variable_declaration",  "simple_declaration",  "init_declaration",
  "parameter_list",  "function_body",  "parameter",     "for_init_statement",
  "switch_body",   "switch_case",   "default_case",  "case_statements",
};
#endif /* NDEBUG */

#ifndef NDEBUG
/* For tracing reduce actions, the names of all rules are required.
*/
static const char *const yyRuleName[] = {
 /*   0 */ "translation_unit ::= statement_sequence_opt",
 /*   1 */ "statement_sequence ::= statement",
 /*   2 */ "statement_sequence ::= statement_sequence statement",
 /*   3 */ "statement_sequence_opt ::=",
 /*   4 */ "statement_sequence_opt ::= statement_sequence",
 /*   5 */ "statement ::= include_statement",
 /*   6 */ "statement ::= expression_statement",
 /*   7 */ "statement ::= declaration_statement",
 /*   8 */ "statement ::= for_statement",
 /*   9 */ "statement ::= compound_statement",
 /*  10 */ "statement ::= if_statement",
 /*  11 */ "statement ::= while_statement",
 /*  12 */ "statement ::= foreach_statement",
 /*  13 */ "statement ::= return_statement",
 /*  14 */ "statement ::= switch_statement",
 /*  15 */ "statement ::= break_statement",
 /*  16 */ "statement ::= continue_statement",
 /*  17 */ "expression ::= assignment_expression",
 /*  18 */ "expression_opt ::=",
 /*  19 */ "expression_opt ::= expression",
 /*  20 */ "assignment_expression ::= conditional_expression",
 /*  21 */ "assignment_expression ::= binary_expression assignment_operator assignment_expression",
 /*  22 */ "assignment_operator ::= ASSIGN",
 /*  23 */ "assignment_operator ::= ASSADD",
 /*  24 */ "assignment_operator ::= ASSSUB",
 /*  25 */ "assignment_operator ::= ASSMUL",
 /*  26 */ "assignment_operator ::= ASSDIV",
 /*  27 */ "assignment_operator ::= ASSMOD",
 /*  28 */ "conditional_expression ::= binary_expression",
 /*  29 */ "conditional_expression ::= binary_expression QUESTION expression COLON assignment_expression",
 /*  30 */ "binary_expression ::= unary_expression",
 /*  31 */ "binary_expression ::= binary_expression LOGOR binary_expression",
 /*  32 */ "binary_expression ::= binary_expression LOGAND binary_expression",
 /*  33 */ "binary_expression ::= binary_expression BITOR binary_expression",
 /*  34 */ "binary_expression ::= binary_expression BITXOR binary_expression",
 /*  35 */ "binary_expression ::= binary_expression BITAND binary_expression",
 /*  36 */ "binary_expression ::= binary_expression EQUALS binary_expression",
 /*  37 */ "binary_expression ::= binary_expression NEQUALS binary_expression",
 /*  38 */ "binary_expression ::= binary_expression ST binary_expression",
 /*  39 */ "binary_expression ::= binary_expression SE binary_expression",
 /*  40 */ "binary_expression ::= binary_expression GT binary_expression",
 /*  41 */ "binary_expression ::= binary_expression GE binary_expression",
 /*  42 */ "binary_expression ::= binary_expression ADDOP binary_expression",
 /*  43 */ "binary_expression ::= binary_expression SUBOP binary_expression",
 /*  44 */ "binary_expression ::= binary_expression MULOP binary_expression",
 /*  45 */ "binary_expression ::= binary_expression DIVOP binary_expression",
 /*  46 */ "binary_expression ::= binary_expression MODOP binary_expression",
 /*  47 */ "unary_expression ::= postfix_expression",
 /*  48 */ "unary_expression ::= SUBOP unary_expression",
 /*  49 */ "unary_expression ::= ADDADD unary_expression",
 /*  50 */ "unary_expression ::= SUBSUB unary_expression",
 /*  51 */ "postfix_expression ::= primary_expression",
 /*  52 */ "postfix_expression ::= postfix_expression ADDADD",
 /*  53 */ "postfix_expression ::= postfix_expression SUBSUB",
 /*  54 */ "postfix_expression ::= postfix_expression DOT id_expression",
 /*  55 */ "postfix_expression ::= postfix_expression LBRACKET expression RBRACKET",
 /*  56 */ "postfix_expression ::= IDENTIFIER LPAREN RPAREN",
 /*  57 */ "postfix_expression ::= IDENTIFIER LPAREN argument_list RPAREN",
 /*  58 */ "primary_expression ::= literal",
 /*  59 */ "primary_expression ::= id_expression",
 /*  60 */ "primary_expression ::= LPAREN expression RPAREN",
 /*  61 */ "primary_expression ::= list_literal",
 /*  62 */ "literal ::= INTEGER",
 /*  63 */ "literal ::= REAL",
 /*  64 */ "literal ::= STRING",
 /*  65 */ "literal ::= TRUE",
 /*  66 */ "literal ::= FALSE",
 /*  67 */ "literal ::= NULL",
 /*  68 */ "id_expression ::= IDENTIFIER",
 /*  69 */ "list_literal ::= LBRACKET list_content RBRACKET",
 /*  70 */ "list_content ::= list_entry",
 /*  71 */ "list_content ::= list_content COMMA list_entry",
 /*  72 */ "list_entry ::= expression",
 /*  73 */ "argument ::= expression",
 /*  74 */ "argument_list ::= argument",
 /*  75 */ "argument_list ::= argument_list COMMA argument",
 /*  76 */ "expression_statement ::= SEMICOLON",
 /*  77 */ "expression_statement ::= expression SEMICOLON",
 /*  78 */ "compound_statement ::= LBRACE RBRACE",
 /*  79 */ "compound_statement ::= LBRACE statement_sequence RBRACE",
 /*  80 */ "include_statement ::= INCLUDE STRING SEMICOLON",
 /*  81 */ "return_statement ::= RETURN expression SEMICOLON",
 /*  82 */ "return_statement ::= RETURN SEMICOLON",
 /*  83 */ "break_statement ::= BREAK SEMICOLON",
 /*  84 */ "continue_statement ::= CONTINUE SEMICOLON",
 /*  85 */ "declaration_statement ::= declaration_sequence SEMICOLON",
 /*  86 */ "declaration_statement ::= function_declaration",
 /*  87 */ "declaration_sequence ::= VAR variable_declaration",
 /*  88 */ "declaration_sequence ::= declaration_sequence COMMA variable_declaration",
 /*  89 */ "variable_declaration ::= simple_declaration",
 /*  90 */ "variable_declaration ::= init_declaration",
 /*  91 */ "simple_declaration ::= IDENTIFIER",
 /*  92 */ "init_declaration ::= IDENTIFIER ASSIGN expression",
 /*  93 */ "function_declaration ::= FUNCTION IDENTIFIER LPAREN parameter_list RPAREN function_body",
 /*  94 */ "function_declaration ::= FUNCTION IDENTIFIER LPAREN RPAREN function_body",
 /*  95 */ "parameter ::= IDENTIFIER",
 /*  96 */ "parameter_list ::= parameter",
 /*  97 */ "parameter_list ::= parameter_list COMMA parameter",
 /*  98 */ "function_body ::= statement",
 /*  99 */ "for_statement ::= FOR LPAREN for_init_statement expression SEMICOLON expression_opt RPAREN statement",
 /* 100 */ "for_init_statement ::= expression_statement",
 /* 101 */ "for_init_statement ::= declaration_sequence SEMICOLON",
 /* 102 */ "foreach_statement ::= FOR LPAREN id_expression IN expression RPAREN statement",
 /* 103 */ "if_statement ::= IF LPAREN expression RPAREN statement",
 /* 104 */ "if_statement ::= IF LPAREN expression RPAREN statement ELSE statement",
 /* 105 */ "while_statement ::= WHILE LPAREN expression RPAREN statement",
 /* 106 */ "switch_statement ::= SWITCH LPAREN expression RPAREN LBRACE switch_body RBRACE",
 /* 107 */ "switch_body ::=",
 /* 108 */ "switch_body ::= switch_body switch_case",
 /* 109 */ "switch_body ::= switch_body default_case",
 /* 110 */ "switch_case ::= CASE literal COLON case_statements",
 /* 111 */ "default_case ::= DEFAULT COLON case_statements",
 /* 112 */ "case_statements ::= statement_sequence",
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
** to AstGenParse and AstGenParseFree.
*/
void *AstGenParseAlloc(void *(*mallocProc)(size_t)){
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
**       obtained from AstGenParseAlloc.
** <li>  A pointer to a function used to reclaim memory obtained
**       from malloc.
** </ul>
*/
void AstGenParseFree(
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
   AstGenParseARG_FETCH;
   yypParser->yyidx--;
#ifndef NDEBUG
   if( yyTraceFILE ){
     fprintf(yyTraceFILE,"%sStack Overflow!\n",yyTracePrompt);
   }
#endif
   while( yypParser->yyidx>=0 ) yy_pop_parser_stack(yypParser);
   /* Here code is inserted which will execute if the parser
   ** stack every overflows */
   AstGenParseARG_STORE; /* Suppress warning about unused %extra_argument var */
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
  { 59, 1 },
  { 61, 1 },
  { 61, 2 },
  { 60, 0 },
  { 60, 1 },
  { 62, 1 },
  { 62, 1 },
  { 62, 1 },
  { 62, 1 },
  { 62, 1 },
  { 62, 1 },
  { 62, 1 },
  { 62, 1 },
  { 62, 1 },
  { 62, 1 },
  { 62, 1 },
  { 62, 1 },
  { 75, 1 },
  { 77, 0 },
  { 77, 1 },
  { 76, 1 },
  { 76, 3 },
  { 80, 1 },
  { 80, 1 },
  { 80, 1 },
  { 80, 1 },
  { 80, 1 },
  { 80, 1 },
  { 78, 1 },
  { 78, 5 },
  { 79, 1 },
  { 79, 3 },
  { 79, 3 },
  { 79, 3 },
  { 79, 3 },
  { 79, 3 },
  { 79, 3 },
  { 79, 3 },
  { 79, 3 },
  { 79, 3 },
  { 79, 3 },
  { 79, 3 },
  { 79, 3 },
  { 79, 3 },
  { 79, 3 },
  { 79, 3 },
  { 79, 3 },
  { 81, 1 },
  { 81, 2 },
  { 81, 2 },
  { 81, 2 },
  { 82, 1 },
  { 82, 2 },
  { 82, 2 },
  { 82, 3 },
  { 82, 4 },
  { 82, 3 },
  { 82, 4 },
  { 83, 1 },
  { 83, 1 },
  { 83, 3 },
  { 83, 1 },
  { 86, 1 },
  { 86, 1 },
  { 86, 1 },
  { 86, 1 },
  { 86, 1 },
  { 86, 1 },
  { 84, 1 },
  { 87, 3 },
  { 88, 1 },
  { 88, 3 },
  { 89, 1 },
  { 90, 1 },
  { 85, 1 },
  { 85, 3 },
  { 64, 1 },
  { 64, 2 },
  { 67, 2 },
  { 67, 3 },
  { 63, 3 },
  { 71, 3 },
  { 71, 2 },
  { 73, 2 },
  { 74, 2 },
  { 65, 2 },
  { 65, 1 },
  { 91, 2 },
  { 91, 3 },
  { 93, 1 },
  { 93, 1 },
  { 94, 1 },
  { 95, 3 },
  { 92, 6 },
  { 92, 5 },
  { 98, 1 },
  { 96, 1 },
  { 96, 3 },
  { 97, 1 },
  { 66, 8 },
  { 99, 1 },
  { 99, 2 },
  { 70, 7 },
  { 68, 5 },
  { 68, 7 },
  { 69, 5 },
  { 72, 7 },
  { 100, 0 },
  { 100, 2 },
  { 100, 2 },
  { 101, 4 },
  { 102, 3 },
  { 103, 1 },
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
  AstGenParseARG_FETCH;
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
#line 71 "astgen.in"
{ p->SetRoot(yymsp[0].minor.yy183); }
#line 1091 "astgen.c"
        break;
      case 1:
      case 4:
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
      case 20:
      case 28:
      case 30:
      case 47:
      case 51:
      case 58:
      case 59:
      case 61:
      case 74:
      case 86:
      case 87:
      case 89:
      case 90:
      case 96:
      case 98:
      case 100:
      case 112:
#line 74 "astgen.in"
{ yygotominor.yy183 = yymsp[0].minor.yy183; }
#line 1128 "astgen.c"
        break;
      case 2:
#line 75 "astgen.in"
{ 
  if(yymsp[-1].minor.yy183->m_type == statement_sequence) {
    yygotominor.yy183 = yymsp[-1].minor.yy183;
  }
  else {
    yygotominor.yy183 = new Ast(statement_sequence, new AstList);
    any_cast<AstList*>(yygotominor.yy183->m_a1)->push_back(yymsp[-1].minor.yy183);
  }
  any_cast<AstList*>(yygotominor.yy183->m_a1)->push_back(yymsp[0].minor.yy183);
}
#line 1142 "astgen.c"
        break;
      case 3:
      case 18:
#line 88 "astgen.in"
{ yygotominor.yy183 = 0; }
#line 1148 "astgen.c"
        break;
      case 21:
#line 119 "astgen.in"
{ yygotominor.yy183 = new Ast(assignment_expression, yymsp[-1].minor.yy94, yymsp[-2].minor.yy183, yymsp[0].minor.yy183); }
#line 1153 "astgen.c"
        break;
      case 22:
#line 123 "astgen.in"
{ yygotominor.yy94 = op_assign; }
#line 1158 "astgen.c"
        break;
      case 23:
#line 124 "astgen.in"
{ yygotominor.yy94 = op_assadd; }
#line 1163 "astgen.c"
        break;
      case 24:
#line 125 "astgen.in"
{ yygotominor.yy94 = op_asssub; }
#line 1168 "astgen.c"
        break;
      case 25:
#line 126 "astgen.in"
{ yygotominor.yy94 = op_assmul; }
#line 1173 "astgen.c"
        break;
      case 26:
#line 127 "astgen.in"
{ yygotominor.yy94 = op_assdiv; }
#line 1178 "astgen.c"
        break;
      case 27:
#line 128 "astgen.in"
{ yygotominor.yy94 = op_assmod; }
#line 1183 "astgen.c"
        break;
      case 29:
#line 132 "astgen.in"
{ yygotominor.yy183 = new Ast(ternary_expression, yymsp[-4].minor.yy183, yymsp[-2].minor.yy183, yymsp[0].minor.yy183); }
#line 1188 "astgen.c"
        break;
      case 31:
#line 136 "astgen.in"
{ yygotominor.yy183 = new Ast(binary_expression, op_logor,   yymsp[-2].minor.yy183, yymsp[0].minor.yy183); }
#line 1193 "astgen.c"
        break;
      case 32:
#line 137 "astgen.in"
{ yygotominor.yy183 = new Ast(binary_expression, op_logand,  yymsp[-2].minor.yy183, yymsp[0].minor.yy183); }
#line 1198 "astgen.c"
        break;
      case 33:
#line 138 "astgen.in"
{ yygotominor.yy183 = new Ast(binary_expression, op_bitor,   yymsp[-2].minor.yy183, yymsp[0].minor.yy183); }
#line 1203 "astgen.c"
        break;
      case 34:
#line 139 "astgen.in"
{ yygotominor.yy183 = new Ast(binary_expression, op_bitxor,  yymsp[-2].minor.yy183, yymsp[0].minor.yy183); }
#line 1208 "astgen.c"
        break;
      case 35:
#line 140 "astgen.in"
{ yygotominor.yy183 = new Ast(binary_expression, op_bitand,  yymsp[-2].minor.yy183, yymsp[0].minor.yy183); }
#line 1213 "astgen.c"
        break;
      case 36:
#line 141 "astgen.in"
{ yygotominor.yy183 = new Ast(binary_expression, op_eq,   yymsp[-2].minor.yy183, yymsp[0].minor.yy183); }
#line 1218 "astgen.c"
        break;
      case 37:
#line 142 "astgen.in"
{ yygotominor.yy183 = new Ast(binary_expression, op_ne,   yymsp[-2].minor.yy183, yymsp[0].minor.yy183); }
#line 1223 "astgen.c"
        break;
      case 38:
#line 143 "astgen.in"
{ yygotominor.yy183 = new Ast(binary_expression, op_lt,   yymsp[-2].minor.yy183, yymsp[0].minor.yy183); }
#line 1228 "astgen.c"
        break;
      case 39:
#line 144 "astgen.in"
{ yygotominor.yy183 = new Ast(binary_expression, op_le,   yymsp[-2].minor.yy183, yymsp[0].minor.yy183); }
#line 1233 "astgen.c"
        break;
      case 40:
#line 145 "astgen.in"
{ yygotominor.yy183 = new Ast(binary_expression, op_gt,   yymsp[-2].minor.yy183, yymsp[0].minor.yy183); }
#line 1238 "astgen.c"
        break;
      case 41:
#line 146 "astgen.in"
{ yygotominor.yy183 = new Ast(binary_expression, op_ge,   yymsp[-2].minor.yy183, yymsp[0].minor.yy183); }
#line 1243 "astgen.c"
        break;
      case 42:
#line 147 "astgen.in"
{ yygotominor.yy183 = new Ast(binary_expression, op_add,  yymsp[-2].minor.yy183, yymsp[0].minor.yy183); }
#line 1248 "astgen.c"
        break;
      case 43:
#line 148 "astgen.in"
{ yygotominor.yy183 = new Ast(binary_expression, op_sub,  yymsp[-2].minor.yy183, yymsp[0].minor.yy183); }
#line 1253 "astgen.c"
        break;
      case 44:
#line 149 "astgen.in"
{ yygotominor.yy183 = new Ast(binary_expression, op_mul,  yymsp[-2].minor.yy183, yymsp[0].minor.yy183); }
#line 1258 "astgen.c"
        break;
      case 45:
#line 150 "astgen.in"
{ yygotominor.yy183 = new Ast(binary_expression, op_div,  yymsp[-2].minor.yy183, yymsp[0].minor.yy183); }
#line 1263 "astgen.c"
        break;
      case 46:
#line 151 "astgen.in"
{ yygotominor.yy183 = new Ast(binary_expression, op_mod,  yymsp[-2].minor.yy183, yymsp[0].minor.yy183); }
#line 1268 "astgen.c"
        break;
      case 48:
#line 155 "astgen.in"
{ yygotominor.yy183 = new Ast(prefix_expression, op_negate, yymsp[0].minor.yy183); }
#line 1273 "astgen.c"
        break;
      case 49:
#line 156 "astgen.in"
{ yygotominor.yy183 = new Ast(prefix_expression, op_preinc, yymsp[0].minor.yy183); }
#line 1278 "astgen.c"
        break;
      case 50:
#line 157 "astgen.in"
{ yygotominor.yy183 = new Ast(prefix_expression, op_predec, yymsp[0].minor.yy183); }
#line 1283 "astgen.c"
        break;
      case 52:
#line 161 "astgen.in"
{ yygotominor.yy183 = new Ast(postfix_expression, op_postinc, yymsp[-1].minor.yy183); }
#line 1288 "astgen.c"
        break;
      case 53:
#line 162 "astgen.in"
{ yygotominor.yy183 = new Ast(postfix_expression, op_postdec, yymsp[-1].minor.yy183); }
#line 1293 "astgen.c"
        break;
      case 54:
#line 163 "astgen.in"
{ yygotominor.yy183 = new Ast(member_expression, yymsp[-2].minor.yy183, yymsp[0].minor.yy183); }
#line 1298 "astgen.c"
        break;
      case 55:
#line 164 "astgen.in"
{ yygotominor.yy183 = new Ast(index_expression, yymsp[-3].minor.yy183, yymsp[-1].minor.yy183); }
#line 1303 "astgen.c"
        break;
      case 56:
#line 165 "astgen.in"
{ yygotominor.yy183 = new Ast(function_call, String(yymsp[-2].minor.yy0)); }
#line 1308 "astgen.c"
        break;
      case 57:
#line 166 "astgen.in"
{ yygotominor.yy183 = new Ast(function_call, String(yymsp[-3].minor.yy0), yymsp[-1].minor.yy183); }
#line 1313 "astgen.c"
        break;
      case 60:
      case 85:
      case 101:
#line 171 "astgen.in"
{ yygotominor.yy183 = yymsp[-1].minor.yy183; }
#line 1320 "astgen.c"
        break;
      case 62:
#line 175 "astgen.in"
{ yygotominor.yy183 = new Ast(literal, Variant(String(yymsp[0].minor.yy0), Variant::stInt)); }
#line 1325 "astgen.c"
        break;
      case 63:
#line 176 "astgen.in"
{ yygotominor.yy183 = new Ast(literal, Variant(String(yymsp[0].minor.yy0), Variant::stReal)); }
#line 1330 "astgen.c"
        break;
      case 64:
#line 177 "astgen.in"
{ yygotominor.yy183 = new Ast(literal, Variant(String(yymsp[0].minor.yy0), Variant::stString)); }
#line 1335 "astgen.c"
        break;
      case 65:
#line 178 "astgen.in"
{ yygotominor.yy183 = new Ast(literal, Variant(true));    }
#line 1340 "astgen.c"
        break;
      case 66:
#line 179 "astgen.in"
{ yygotominor.yy183 = new Ast(literal, Variant(false));   }
#line 1345 "astgen.c"
        break;
      case 67:
#line 180 "astgen.in"
{ yygotominor.yy183 = new Ast(literal, Variant());        }
#line 1350 "astgen.c"
        break;
      case 68:
#line 183 "astgen.in"
{ yygotominor.yy183 = new Ast(lvalue, String(yymsp[0].minor.yy0)); }
#line 1355 "astgen.c"
        break;
      case 69:
#line 186 "astgen.in"
{ yygotominor.yy183 = new Ast(list_literal, yymsp[-1].minor.yy183); }
#line 1360 "astgen.c"
        break;
      case 70:
#line 188 "astgen.in"
{ yygotominor.yy183 = new Ast(list_content, yymsp[0].minor.yy183); }
#line 1365 "astgen.c"
        break;
      case 71:
#line 189 "astgen.in"
{ yygotominor.yy183 = new Ast(list_content, yymsp[-2].minor.yy183, yymsp[0].minor.yy183); }
#line 1370 "astgen.c"
        break;
      case 72:
#line 191 "astgen.in"
{ yygotominor.yy183 = new Ast(list_entry, yymsp[0].minor.yy183); }
#line 1375 "astgen.c"
        break;
      case 73:
#line 200 "astgen.in"
{ yygotominor.yy183 = new Ast(argument, yymsp[0].minor.yy183); }
#line 1380 "astgen.c"
        break;
      case 75:
#line 204 "astgen.in"
{ yygotominor.yy183 = new Ast(argument_list, yymsp[-2].minor.yy183, yymsp[0].minor.yy183); }
#line 1385 "astgen.c"
        break;
      case 76:
#line 212 "astgen.in"
{ yygotominor.yy183 = new Ast(empty_statement); }
#line 1390 "astgen.c"
        break;
      case 77:
#line 213 "astgen.in"
{ yygotominor.yy183 = new Ast(expression_statement, yymsp[-1].minor.yy183); }
#line 1395 "astgen.c"
        break;
      case 78:
#line 216 "astgen.in"
{ yygotominor.yy183 = new Ast(compound_statement); }
#line 1400 "astgen.c"
        break;
      case 79:
#line 217 "astgen.in"
{ yygotominor.yy183 = new Ast(compound_statement, yymsp[-1].minor.yy183); }
#line 1405 "astgen.c"
        break;
      case 80:
#line 220 "astgen.in"
{ p->Parse(yymsp[-1].minor.yy0); yygotominor.yy183 = p->GetRoot(); }
#line 1410 "astgen.c"
        break;
      case 81:
#line 223 "astgen.in"
{ yygotominor.yy183 = new Ast(return_statement, yymsp[-1].minor.yy183); }
#line 1415 "astgen.c"
        break;
      case 82:
#line 224 "astgen.in"
{ yygotominor.yy183 = new Ast(return_statement);    }
#line 1420 "astgen.c"
        break;
      case 83:
#line 227 "astgen.in"
{ yygotominor.yy183 = new Ast(break_statement); }
#line 1425 "astgen.c"
        break;
      case 84:
#line 228 "astgen.in"
{ yygotominor.yy183 = new Ast(continue_statement); }
#line 1430 "astgen.c"
        break;
      case 88:
#line 241 "astgen.in"
{ yygotominor.yy183 = new Ast(declaration_sequence, yymsp[-2].minor.yy183, yymsp[0].minor.yy183); }
#line 1435 "astgen.c"
        break;
      case 91:
#line 248 "astgen.in"
{ yygotominor.yy183 = new Ast(variable_declaration, String(yymsp[0].minor.yy0)); }
#line 1440 "astgen.c"
        break;
      case 92:
#line 251 "astgen.in"
{ yygotominor.yy183 = new Ast(variable_declaration, String(yymsp[-2].minor.yy0), yymsp[0].minor.yy183); }
#line 1445 "astgen.c"
        break;
      case 93:
#line 254 "astgen.in"
{ yygotominor.yy183 = new Ast(function_declaration, String(yymsp[-4].minor.yy0), yymsp[-2].minor.yy183, yymsp[0].minor.yy183); }
#line 1450 "astgen.c"
        break;
      case 94:
#line 255 "astgen.in"
{ yygotominor.yy183 = new Ast(function_declaration, String(yymsp[-3].minor.yy0), any(), yymsp[0].minor.yy183); }
#line 1455 "astgen.c"
        break;
      case 95:
#line 258 "astgen.in"
{ yygotominor.yy183 = new Ast(parameter, String(yymsp[0].minor.yy0)); }
#line 1460 "astgen.c"
        break;
      case 97:
#line 262 "astgen.in"
{ yygotominor.yy183 = new Ast(parameter_list, yymsp[-2].minor.yy183, yymsp[0].minor.yy183); }
#line 1465 "astgen.c"
        break;
      case 99:
#line 274 "astgen.in"
{ yygotominor.yy183 = new Ast(for_statement, yymsp[-5].minor.yy183, yymsp[-4].minor.yy183, yymsp[-2].minor.yy183, yymsp[0].minor.yy183); }
#line 1470 "astgen.c"
        break;
      case 102:
#line 285 "astgen.in"
{ yygotominor.yy183 = new Ast(foreach_statement, yymsp[-4].minor.yy183, yymsp[-2].minor.yy183, yymsp[0].minor.yy183); }
#line 1475 "astgen.c"
        break;
      case 103:
#line 296 "astgen.in"
{ yygotominor.yy183 = new Ast(if_statement, yymsp[-2].minor.yy183, yymsp[0].minor.yy183); }
#line 1480 "astgen.c"
        break;
      case 104:
#line 297 "astgen.in"
{ yygotominor.yy183 = new Ast(if_statement, yymsp[-4].minor.yy183, yymsp[-2].minor.yy183, yymsp[0].minor.yy183); }
#line 1485 "astgen.c"
        break;
      case 105:
#line 305 "astgen.in"
{ yygotominor.yy183 = new Ast(while_statement, yymsp[-2].minor.yy183,  yymsp[0].minor.yy183); }
#line 1490 "astgen.c"
        break;
      case 106:
#line 313 "astgen.in"
{ yygotominor.yy183 = new Ast(switch_statement, yymsp[-4].minor.yy183, yymsp[-1].minor.yy103); }
#line 1495 "astgen.c"
        break;
      case 107:
#line 317 "astgen.in"
{ yygotominor.yy103 = new AstList; }
#line 1500 "astgen.c"
        break;
      case 108:
      case 109:
#line 318 "astgen.in"
{ yygotominor.yy103 = yymsp[-1].minor.yy103; yygotominor.yy103->push_back(yymsp[0].minor.yy183); }
#line 1506 "astgen.c"
        break;
      case 110:
#line 322 "astgen.in"
{ yygotominor.yy183 = new Ast(switch_case, yymsp[-2].minor.yy183, yymsp[0].minor.yy183); }
#line 1511 "astgen.c"
        break;
      case 111:
#line 325 "astgen.in"
{ yygotominor.yy183 = new Ast(default_case, yymsp[0].minor.yy183); }
#line 1516 "astgen.c"
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
  AstGenParseARG_FETCH;
#ifndef NDEBUG
  if( yyTraceFILE ){
    fprintf(yyTraceFILE,"%sFail!\n",yyTracePrompt);
  }
#endif
  while( yypParser->yyidx>=0 ) yy_pop_parser_stack(yypParser);
  /* Here code is inserted which will be executed whenever the
  ** parser fails */
#line 51 "astgen.in"

  p->OnParseFailure();
#line 1564 "astgen.c"
  AstGenParseARG_STORE; /* Suppress warning about unused %extra_argument variable */
}

/*
** The following code executes when a syntax error first occurs.
*/
static void yy_syntax_error(
  yyParser *yypParser,           /* The parser */
  int yymajor,                   /* The major type of the error token */
  YYMINORTYPE yyminor            /* The minor type of the error token */
){
  AstGenParseARG_FETCH;
#define TOKEN (yyminor.yy0)
#line 54 "astgen.in"

  p->OnSyntaxError();
#line 1582 "astgen.c"
  AstGenParseARG_STORE; /* Suppress warning about unused %extra_argument variable */
}

/*
** The following is executed when the parser accepts
*/
static void yy_accept(
  yyParser *yypParser           /* The parser */
){
  AstGenParseARG_FETCH;
#ifndef NDEBUG
  if( yyTraceFILE ){
    fprintf(yyTraceFILE,"%sAccept!\n",yyTracePrompt);
  }
#endif
  while( yypParser->yyidx>=0 ) yy_pop_parser_stack(yypParser);
  /* Here code is inserted which will be executed whenever the
  ** parser accepts */
  AstGenParseARG_STORE; /* Suppress warning about unused %extra_argument variable */
}

/* The main parser program.
** The first argument is a pointer to a structure obtained from
** "AstGenParseAlloc" which describes the current state of the parser.
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
void AstGenParse(
  void *yyp,                   /* The parser */
  int yymajor,                 /* The major token code number */
  AstGenParseTOKENTYPE yyminor       /* The value for the token */
  AstGenParseARG_PDECL               /* Optional %extra_argument parameter */
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
  AstGenParseARG_STORE;

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

