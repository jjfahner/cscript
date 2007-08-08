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
#define YYNOCODE 109
#define YYACTIONTYPE unsigned short int
#define AstGenParseTOKENTYPE  Token 
typedef union {
  AstGenParseTOKENTYPE yy0;
  Ast* yy31;
  AstList* yy127;
  opcodes yy198;
  int yy217;
} YYMINORTYPE;
#ifndef YYSTACKDEPTH
#define YYSTACKDEPTH 100
#endif
#define AstGenParseARG_SDECL  AstGen* p ;
#define AstGenParseARG_PDECL , AstGen* p 
#define AstGenParseARG_FETCH  AstGen* p  = yypParser->p 
#define AstGenParseARG_STORE yypParser->p  = p 
#define YYNSTATE 208
#define YYNRULE 120
#define YYERRORSYMBOL 58
#define YYERRSYMDT yy217
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
 /*     0 */   329,  206,  204,   12,  203,  199,  198,  195,  194,  192,
 /*    10 */   189,  188,  186,  185,  184,  182,  180,  111,  179,  118,
 /*    20 */   177,   57,   13,  175,   73,  163,  158,    7,  160,  156,
 /*    30 */   130,  131,  132,   76,  136,   11,  203,  199,  198,  195,
 /*    40 */   194,  192,  189,  188,  186,  185,  184,  182,  180,  111,
 /*    50 */   179,  169,  177,   57,   59,  175,   73,  163,  158,  168,
 /*    60 */   160,  156,   38,   37,   45,   76,  136,   41,   39,   42,
 /*    70 */    44,   46,   47,   43,   40,   33,   38,   37,   45,  157,
 /*    80 */   178,   11,  203,  199,  198,  195,  194,  192,  189,  188,
 /*    90 */   186,  185,  184,  182,  180,  111,  179,  205,  177,   57,
 /*   100 */   121,  175,   73,  163,  158,  112,  160,  156,  138,  131,
 /*   110 */   132,   76,  136,  187,  179,   98,  177,   57,  109,  175,
 /*   120 */    73,  163,  158,   88,  160,  156,  181,   29,    6,  203,
 /*   130 */   199,  198,  195,  194,  192,  189,  188,  186,  185,  184,
 /*   140 */   182,  180,  111,  179,   32,  177,   57,  148,  175,   73,
 /*   150 */   163,  158,   55,  160,  156,   54,  183,   19,   76,  136,
 /*   160 */   145,  199,  198,  195,  194,  192,  189,  188,  186,  185,
 /*   170 */   184,  182,  180,  111,  179,   84,  177,   57,   56,  175,
 /*   180 */    73,  163,  158,   24,  160,  156,   54,  128,   25,   76,
 /*   190 */   136,  174,  170,   60,   30,  144,  145,  199,  198,  195,
 /*   200 */   194,  192,  189,  188,  186,  185,  184,  182,  180,  111,
 /*   210 */   179,  100,  177,   57,  171,  175,   73,  163,  158,    9,
 /*   220 */   160,  156,  100,  161,    4,   76,  136,   15,   58,   92,
 /*   230 */    18,  193,  202,  199,  198,  195,  194,  192,  189,  188,
 /*   240 */   186,  185,  184,  182,  180,  111,  179,  114,  177,   57,
 /*   250 */    10,  175,   73,  163,  158,    8,  160,  156,   23,  172,
 /*   260 */   173,   76,  136,  152,  199,  198,  195,  194,  192,  189,
 /*   270 */   188,  186,  185,  184,  182,  180,  111,  179,   17,  177,
 /*   280 */    57,  124,  175,   73,  163,  158,   50,  160,  156,   61,
 /*   290 */    27,   31,   76,  136,  200,  201,    2,  107,   51,   49,
 /*   300 */   126,   16,   14,   95,   21,   28,  155,  154,  153,  151,
 /*   310 */   150,  149,    5,  116,    3,  207,  110,   20,   80,   83,
 /*   320 */    53,   86,   91,   81,   52,   89,  103,  101,   93,  190,
 /*   330 */   199,  198,  195,  194,  192,  189,  188,  186,  185,  184,
 /*   340 */   182,  180,  111,  179,    1,  177,   57,  139,  175,   73,
 /*   350 */   163,  158,  330,  160,  156,  330,  330,  330,   76,  136,
 /*   360 */   330,  330,  164,  199,  198,  195,  194,  192,  189,  188,
 /*   370 */   186,  185,  184,  182,  180,  111,  179,  330,  177,   57,
 /*   380 */   330,  175,   73,  163,  158,  330,  160,  156,  330,  330,
 /*   390 */   330,   76,  136,  159,  199,  198,  195,  194,  192,  189,
 /*   400 */   188,  186,  185,  184,  182,  180,  111,  179,  330,  177,
 /*   410 */    57,  330,  175,   73,  163,  158,   50,  160,  156,  330,
 /*   420 */   330,  330,   76,  136,  330,  330,  330,  330,   51,   49,
 /*   430 */   330,   16,  330,   95,   21,  330,  155,  154,  153,  151,
 /*   440 */   150,  149,  330,  116,    3,  115,  110,   20,   80,   83,
 /*   450 */    53,   86,   91,  330,  330,  330,  103,  101,   93,  113,
 /*   460 */   199,  198,  195,  194,  192,  189,  188,  186,  185,  184,
 /*   470 */   182,  180,  111,  179,  330,  177,   57,  330,  175,   73,
 /*   480 */   163,  158,   50,  160,  156,  330,  330,  330,   76,  136,
 /*   490 */   330,  330,  330,  330,   51,   49,  330,   16,  330,   95,
 /*   500 */    21,  330,  155,  154,  153,  151,  150,  149,  330,  116,
 /*   510 */     3,  330,  110,   20,   80,   83,   53,   86,   91,  330,
 /*   520 */   330,  330,  103,  101,   93,   36,   34,   35,   48,   41,
 /*   530 */    39,   42,   44,   46,   47,   43,   40,   33,   38,   37,
 /*   540 */    45,  137,  134,  133,  129,  127,  125,   26,   34,   35,
 /*   550 */    48,   41,   39,   42,   44,   46,   47,   43,   40,   33,
 /*   560 */    38,   37,   45,   35,   48,   41,   39,   42,   44,   46,
 /*   570 */    47,   43,   40,   33,   38,   37,   45,   50,   39,   42,
 /*   580 */    44,   46,   47,   43,   40,   33,   38,   37,   45,   51,
 /*   590 */    49,  330,   16,  330,   95,   21,  330,  155,  154,  153,
 /*   600 */   151,  150,  149,  330,  116,   69,  330,  175,   73,  163,
 /*   610 */   158,   53,  160,  156,  330,  330,   48,   41,   39,   42,
 /*   620 */    44,   46,   47,   43,   40,   33,   38,   37,   45,  123,
 /*   630 */    50,  177,   57,  330,  175,   73,  163,  158,  330,  160,
 /*   640 */   156,  330,   51,   49,  330,   16,  330,   95,   21,  330,
 /*   650 */   155,  154,  153,  151,  150,  149,  191,  122,   67,  330,
 /*   660 */   175,   73,  163,  158,  330,  160,  156,  111,  179,  330,
 /*   670 */   177,   57,  330,  175,   73,  163,   94,  330,  160,  156,
 /*   680 */   330,  330,  330,   79,   50,   44,   46,   47,   43,   40,
 /*   690 */    33,   38,   37,   45,   22,  330,   51,   49,   50,   16,
 /*   700 */   330,   95,   21,  162,  155,  154,  153,  151,  150,  149,
 /*   710 */    51,   49,  330,   16,  330,   95,   21,  330,  155,  154,
 /*   720 */   153,  151,  150,  149,  146,  179,  330,  177,   57,  330,
 /*   730 */   175,   73,  163,  158,  330,  160,  156,   78,  143,  330,
 /*   740 */   330,  330,  330,  330,  142,  179,  330,  177,   57,  330,
 /*   750 */   175,   73,  163,  158,   77,  160,  156,  330,  330,  140,
 /*   760 */   146,  179,  330,  177,   57,  330,  175,   73,  163,  158,
 /*   770 */   330,  160,  156,  330,  147,  142,  179,  330,  177,   57,
 /*   780 */   330,  175,   73,  163,  158,  330,  160,  156,   82,  179,
 /*   790 */   141,  177,   57,  330,  175,   73,  163,  158,  330,  160,
 /*   800 */   156,  105,  179,  330,  177,   57,  330,  175,   73,  163,
 /*   810 */   158,  330,  160,  156,  106,  179,  330,  177,   57,  330,
 /*   820 */   175,   73,  163,  158,  330,  160,  156,   96,  179,  330,
 /*   830 */   177,   57,  330,  175,   73,  163,  158,  330,  160,  156,
 /*   840 */    99,  179,  330,  177,   57,  330,  175,   73,  163,  158,
 /*   850 */   330,  160,  156,   97,  179,  330,  177,   57,  330,  175,
 /*   860 */    73,  163,  158,  330,  160,  156,   87,  179,  330,  177,
 /*   870 */    57,  330,  175,   73,  163,  158,  330,  160,  156,  196,
 /*   880 */   179,  330,  177,   57,  330,  175,   73,  163,  158,  330,
 /*   890 */   160,  156,  108,  179,  330,  177,   57,  330,  175,   73,
 /*   900 */   163,  158,  330,  160,  156,   85,  179,  330,  177,   57,
 /*   910 */   330,  175,   73,  163,  158,  330,  160,  156,  135,  179,
 /*   920 */   330,  177,   57,  330,  175,   73,  163,  158,  330,  160,
 /*   930 */   156,  176,  330,  177,   57,  330,  175,   73,  163,  158,
 /*   940 */   330,  160,  156,   72,  330,  175,   73,  163,  158,  330,
 /*   950 */   160,  156,  330,   71,  330,  175,   73,  163,  158,  330,
 /*   960 */   160,  156,  330,   65,  330,  175,   73,  163,  158,  330,
 /*   970 */   160,  156,   70,  330,  175,   73,  163,  158,  330,  160,
 /*   980 */   156,  117,  330,  175,   73,  163,  158,  330,  160,  156,
 /*   990 */    75,  330,  175,   73,  163,  158,  330,  160,  156,  120,
 /*  1000 */   330,  175,   73,  163,  158,  330,  160,  156,   66,  330,
 /*  1010 */   175,   73,  163,  158,  330,  160,  156,   64,  330,  175,
 /*  1020 */    73,  163,  158,  330,  160,  156,   68,  330,  175,   73,
 /*  1030 */   163,  158,  330,  160,  156,  330,  119,  330,  175,   73,
 /*  1040 */   163,  158,  330,  160,  156,  330,   74,  330,  175,   73,
 /*  1050 */   163,  158,  330,  160,  156,   62,  330,  175,   73,  163,
 /*  1060 */   158,  330,  160,  156,   63,  330,  175,   73,  163,  158,
 /*  1070 */   330,  160,  156,  167,   73,  163,  158,  330,  160,  156,
 /*  1080 */   166,   73,  163,  158,  330,  160,  156,  165,   73,  163,
 /*  1090 */   158,  330,  160,  156,  155,  154,  153,  151,  150,  149,
 /*  1100 */    40,   33,   38,   37,   45,   90,  330,  197,  201,  102,
 /*  1110 */   104,
};
static const YYCODETYPE yy_lookahead[] = {
 /*     0 */    59,   60,   61,   62,   63,   64,   65,   66,   67,   68,
 /*    10 */    69,   70,   71,   72,   73,   74,   75,   76,   77,   40,
 /*    20 */    79,   80,   52,   82,   83,   84,   85,   32,   87,   88,
 /*    30 */    94,   95,   96,   92,   93,   62,   63,   64,   65,   66,
 /*    40 */    67,   68,   69,   70,   71,   72,   73,   74,   75,   76,
 /*    50 */    77,   85,   79,   80,   39,   82,   83,   84,   85,   30,
 /*    60 */    87,   88,   14,   15,   16,   92,   93,    5,    6,    7,
 /*    70 */     8,    9,   10,   11,   12,   13,   14,   15,   16,   32,
 /*    80 */   107,   62,   63,   64,   65,   66,   67,   68,   69,   70,
 /*    90 */    71,   72,   73,   74,   75,   76,   77,  100,   79,   80,
 /*   100 */    40,   82,   83,   84,   85,   30,   87,   88,   94,   95,
 /*   110 */    96,   92,   93,   76,   77,   78,   79,   80,   35,   82,
 /*   120 */    83,   84,   85,   87,   87,   88,  107,   31,   62,   63,
 /*   130 */    64,   65,   66,   67,   68,   69,   70,   71,   72,   73,
 /*   140 */    74,   75,   76,   77,   81,   79,   80,   29,   82,   83,
 /*   150 */    84,   85,   39,   87,   88,   39,   40,   39,   92,   93,
 /*   160 */    63,   64,   65,   66,   67,   68,   69,   70,   71,   72,
 /*   170 */    73,   74,   75,   76,   77,   30,   79,   80,  104,   82,
 /*   180 */    83,   84,   85,   31,   87,   88,   39,   40,   17,   92,
 /*   190 */    93,   25,   26,   27,   28,   98,   63,   64,   65,   66,
 /*   200 */    67,   68,   69,   70,   71,   72,   73,   74,   75,   76,
 /*   210 */    77,   30,   79,   80,   42,   82,   83,   84,   85,   32,
 /*   220 */    87,   88,   30,   32,   32,   92,   93,   31,   56,   57,
 /*   230 */    39,   98,   63,   64,   65,   66,   67,   68,   69,   70,
 /*   240 */    71,   72,   73,   74,   75,   76,   77,   40,   79,   80,
 /*   250 */    32,   82,   83,   84,   85,   32,   87,   88,   50,  105,
 /*   260 */   106,   92,   93,   63,   64,   65,   66,   67,   68,   69,
 /*   270 */    70,   71,   72,   73,   74,   75,   76,   77,   40,   79,
 /*   280 */    80,   40,   82,   83,   84,   85,   13,   87,   88,   41,
 /*   290 */    31,   24,   92,   93,   99,  100,   24,  102,   25,   26,
 /*   300 */    40,   28,   31,   30,   31,   17,   33,   34,   35,   36,
 /*   310 */    37,   38,   32,   40,   41,   42,   43,   44,   45,   46,
 /*   320 */    47,   48,   49,   32,   31,   30,   53,   54,   55,   63,
 /*   330 */    64,   65,   66,   67,   68,   69,   70,   71,   72,   73,
 /*   340 */    74,   75,   76,   77,   24,   79,   80,   29,   82,   83,
 /*   350 */    84,   85,  108,   87,   88,  108,  108,  108,   92,   93,
 /*   360 */   108,  108,   63,   64,   65,   66,   67,   68,   69,   70,
 /*   370 */    71,   72,   73,   74,   75,   76,   77,  108,   79,   80,
 /*   380 */   108,   82,   83,   84,   85,  108,   87,   88,  108,  108,
 /*   390 */   108,   92,   93,   63,   64,   65,   66,   67,   68,   69,
 /*   400 */    70,   71,   72,   73,   74,   75,   76,   77,  108,   79,
 /*   410 */    80,  108,   82,   83,   84,   85,   13,   87,   88,  108,
 /*   420 */   108,  108,   92,   93,  108,  108,  108,  108,   25,   26,
 /*   430 */   108,   28,  108,   30,   31,  108,   33,   34,   35,   36,
 /*   440 */    37,   38,  108,   40,   41,   42,   43,   44,   45,   46,
 /*   450 */    47,   48,   49,  108,  108,  108,   53,   54,   55,   63,
 /*   460 */    64,   65,   66,   67,   68,   69,   70,   71,   72,   73,
 /*   470 */    74,   75,   76,   77,  108,   79,   80,  108,   82,   83,
 /*   480 */    84,   85,   13,   87,   88,  108,  108,  108,   92,   93,
 /*   490 */   108,  108,  108,  108,   25,   26,  108,   28,  108,   30,
 /*   500 */    31,  108,   33,   34,   35,   36,   37,   38,  108,   40,
 /*   510 */    41,  108,   43,   44,   45,   46,   47,   48,   49,  108,
 /*   520 */   108,  108,   53,   54,   55,    1,    2,    3,    4,    5,
 /*   530 */     6,    7,    8,    9,   10,   11,   12,   13,   14,   15,
 /*   540 */    16,   17,   18,   19,   20,   21,   22,   23,    2,    3,
 /*   550 */     4,    5,    6,    7,    8,    9,   10,   11,   12,   13,
 /*   560 */    14,   15,   16,    3,    4,    5,    6,    7,    8,    9,
 /*   570 */    10,   11,   12,   13,   14,   15,   16,   13,    6,    7,
 /*   580 */     8,    9,   10,   11,   12,   13,   14,   15,   16,   25,
 /*   590 */    26,  108,   28,  108,   30,   31,  108,   33,   34,   35,
 /*   600 */    36,   37,   38,  108,   40,   80,  108,   82,   83,   84,
 /*   610 */    85,   47,   87,   88,  108,  108,    4,    5,    6,    7,
 /*   620 */     8,    9,   10,   11,   12,   13,   14,   15,   16,   77,
 /*   630 */    13,   79,   80,  108,   82,   83,   84,   85,  108,   87,
 /*   640 */    88,  108,   25,   26,  108,   28,  108,   30,   31,  108,
 /*   650 */    33,   34,   35,   36,   37,   38,   65,   40,   80,  108,
 /*   660 */    82,   83,   84,   85,  108,   87,   88,   76,   77,  108,
 /*   670 */    79,   80,  108,   82,   83,   84,   85,  108,   87,   88,
 /*   680 */   108,  108,  108,   92,   13,    8,    9,   10,   11,   12,
 /*   690 */    13,   14,   15,   16,  103,  108,   25,   26,   13,   28,
 /*   700 */   108,   30,   31,   32,   33,   34,   35,   36,   37,   38,
 /*   710 */    25,   26,  108,   28,  108,   30,   31,  108,   33,   34,
 /*   720 */    35,   36,   37,   38,   76,   77,  108,   79,   80,  108,
 /*   730 */    82,   83,   84,   85,  108,   87,   88,   89,   90,  108,
 /*   740 */   108,  108,  108,  108,   76,   77,  108,   79,   80,  108,
 /*   750 */    82,   83,   84,   85,   86,   87,   88,  108,  108,   91,
 /*   760 */    76,   77,  108,   79,   80,  108,   82,   83,   84,   85,
 /*   770 */   108,   87,   88,  108,   90,   76,   77,  108,   79,   80,
 /*   780 */   108,   82,   83,   84,   85,  108,   87,   88,   76,   77,
 /*   790 */    91,   79,   80,  108,   82,   83,   84,   85,  108,   87,
 /*   800 */    88,   76,   77,  108,   79,   80,  108,   82,   83,   84,
 /*   810 */    85,  108,   87,   88,   76,   77,  108,   79,   80,  108,
 /*   820 */    82,   83,   84,   85,  108,   87,   88,   76,   77,  108,
 /*   830 */    79,   80,  108,   82,   83,   84,   85,  108,   87,   88,
 /*   840 */    76,   77,  108,   79,   80,  108,   82,   83,   84,   85,
 /*   850 */   108,   87,   88,   76,   77,  108,   79,   80,  108,   82,
 /*   860 */    83,   84,   85,  108,   87,   88,   76,   77,  108,   79,
 /*   870 */    80,  108,   82,   83,   84,   85,  108,   87,   88,   76,
 /*   880 */    77,  108,   79,   80,  108,   82,   83,   84,   85,  108,
 /*   890 */    87,   88,   76,   77,  108,   79,   80,  108,   82,   83,
 /*   900 */    84,   85,  108,   87,   88,   76,   77,  108,   79,   80,
 /*   910 */   108,   82,   83,   84,   85,  108,   87,   88,   76,   77,
 /*   920 */   108,   79,   80,  108,   82,   83,   84,   85,  108,   87,
 /*   930 */    88,   77,  108,   79,   80,  108,   82,   83,   84,   85,
 /*   940 */   108,   87,   88,   80,  108,   82,   83,   84,   85,  108,
 /*   950 */    87,   88,  108,   80,  108,   82,   83,   84,   85,  108,
 /*   960 */    87,   88,  108,   80,  108,   82,   83,   84,   85,  108,
 /*   970 */    87,   88,   80,  108,   82,   83,   84,   85,  108,   87,
 /*   980 */    88,   80,  108,   82,   83,   84,   85,  108,   87,   88,
 /*   990 */    80,  108,   82,   83,   84,   85,  108,   87,   88,   80,
 /*  1000 */   108,   82,   83,   84,   85,  108,   87,   88,   80,  108,
 /*  1010 */    82,   83,   84,   85,  108,   87,   88,   80,  108,   82,
 /*  1020 */    83,   84,   85,  108,   87,   88,   80,  108,   82,   83,
 /*  1030 */    84,   85,  108,   87,   88,  108,   80,  108,   82,   83,
 /*  1040 */    84,   85,  108,   87,   88,  108,   80,  108,   82,   83,
 /*  1050 */    84,   85,  108,   87,   88,   80,  108,   82,   83,   84,
 /*  1060 */    85,  108,   87,   88,   80,  108,   82,   83,   84,   85,
 /*  1070 */   108,   87,   88,   82,   83,   84,   85,  108,   87,   88,
 /*  1080 */    82,   83,   84,   85,  108,   87,   88,   82,   83,   84,
 /*  1090 */    85,  108,   87,   88,   33,   34,   35,   36,   37,   38,
 /*  1100 */    12,   13,   14,   15,   16,   97,  108,   99,  100,  101,
 /*  1110 */   102,
};
#define YY_SHIFT_USE_DFLT (-31)
#define YY_SHIFT_MAX 113
static const short yy_shift_ofst[] = {
 /*     0 */   469,  469,  469,  273,  469,  469,  403,  469,  469,  469,
 /*    10 */   469,  469,  469,  469,  564,  671,  685,  685,  685,  685,
 /*    20 */   617,  685,  685,  685,  685,  685,  685,  685,  685,  685,
 /*    30 */   685,  685,  685,  685,  685,  685,  685,  685,  685,  685,
 /*    40 */   685,  685,  685,  685,  685,  685,  685,  685,  685,  685,
 /*    50 */   685,  685,  192,  145,  145,  181,  172,  524, 1061,   75,
 /*    60 */    29,  -31,  546,  560,  612,   62,  572,  677,  677, 1088,
 /*    70 */  1088, 1088, 1088,  166,   48,   48,  147,  191,  118,  116,
 /*    80 */   241,  248,  267,  260,  288,  291,  295,  318,  320,  293,
 /*    90 */   280,  271,  272,  259,  208,  196,  238,  223,  218,  187,
 /*   100 */   171,  152,  113,   96,   15,   60,   47,   15,   -5,  -21,
 /*   110 */    83,  207,  171,  -30,
};
#define YY_REDUCE_USE_DFLT (-65)
#define YY_REDUCE_MAX 61
static const short yy_reduce_ofst[] = {
 /*     0 */   -59,  -27,   19,   66,  133,   97,  169,  396,  299,  200,
 /*    10 */   266,  169,  169,  330,  591,  668,  648,   37,  699,  684,
 /*    20 */   725,  738,  751,  764,  777,  803,  712,  829,  842,  816,
 /*    30 */   790,  552,  854,  966,  984,  937,  975,  956,  919,  946,
 /*    40 */   910,  928,  578,  873,  892,  901,  525,  863,  883, 1005,
 /*    50 */   991,  998, 1008,   14,  -64,  195,  154,   63,   36,   -3,
 /*    60 */   -34,   74,
};
static const YYACTIONTYPE yy_default[] = {
 /*     0 */   212,  328,  328,  328,  328,  328,  328,  328,  328,  328,
 /*    10 */   328,  327,  213,  328,  328,  328,  328,  227,  328,  328,
 /*    20 */   328,  328,  328,  328,  328,  328,  328,  328,  328,  328,
 /*    30 */   328,  328,  328,  328,  328,  328,  328,  328,  328,  328,
 /*    40 */   328,  328,  328,  328,  328,  328,  328,  328,  328,  328,
 /*    50 */   328,  328,  328,  328,  328,  328,  328,  237,  328,  328,
 /*    60 */   328,  322,  240,  241,  242,  243,  244,  246,  245,  248,
 /*    70 */   247,  250,  249,  256,  252,  251,  328,  328,  328,  328,
 /*    80 */   328,  328,  328,  328,  300,  328,  328,  328,  328,  328,
 /*    90 */   328,  328,  328,  328,  268,  277,  328,  328,  328,  328,
 /*   100 */   304,  328,  310,  328,  311,  328,  328,  312,  328,  328,
 /*   110 */   328,  328,  328,  318,  286,  288,  285,  255,  289,  254,
 /*   120 */   253,  290,  291,  238,  292,  236,  293,  235,  294,  234,
 /*   130 */   297,  298,  299,  233,  232,  301,  295,  231,  296,  264,
 /*   140 */   283,  284,  282,  279,  302,  313,  281,  280,  278,  276,
 /*   150 */   275,  274,  317,  273,  272,  271,  270,  269,  268,  319,
 /*   160 */   267,  266,  265,  260,  320,  259,  258,  257,  277,  263,
 /*   170 */   262,  321,  323,  324,  261,  239,  230,  229,  325,  226,
 /*   180 */   225,  326,  224,  316,  223,  222,  221,  228,  220,  219,
 /*   190 */   314,  315,  218,  303,  217,  216,  305,  306,  215,  214,
 /*   200 */   307,  308,  211,  210,  209,  309,  208,  287,
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
  "CASE",          "DEFAULT",       "error",         "main",        
  "translation_unit",  "statement_sequence_opt",  "statement_sequence",  "statement",   
  "include_statement",  "expression_statement",  "declaration_statement",  "for_statement",
  "compound_statement",  "if_statement",  "while_statement",  "foreach_statement",
  "return_statement",  "switch_statement",  "break_statement",  "continue_statement",
  "expression",    "assignment_expression",  "expression_opt",  "conditional_expression",
  "binary_expression",  "assignment_operator",  "unary_expression",  "postfix_expression",
  "primary_expression",  "id_expression",  "argument_list",  "literal",     
  "list_literal",  "list_content",  "list_entry",    "argument",    
  "declaration_sequence",  "function_declaration",  "variable_declaration",  "simple_declaration",
  "init_declaration",  "parameter_list",  "function_body",  "parameter",   
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
 /*  18 */ "expression ::= assignment_expression",
 /*  19 */ "expression_opt ::=",
 /*  20 */ "expression_opt ::= expression",
 /*  21 */ "assignment_expression ::= conditional_expression",
 /*  22 */ "assignment_expression ::= binary_expression assignment_operator assignment_expression",
 /*  23 */ "assignment_operator ::= ASSIGN",
 /*  24 */ "assignment_operator ::= ASSADD",
 /*  25 */ "assignment_operator ::= ASSSUB",
 /*  26 */ "assignment_operator ::= ASSMUL",
 /*  27 */ "assignment_operator ::= ASSDIV",
 /*  28 */ "assignment_operator ::= ASSMOD",
 /*  29 */ "conditional_expression ::= binary_expression",
 /*  30 */ "conditional_expression ::= binary_expression QUESTION expression COLON assignment_expression",
 /*  31 */ "binary_expression ::= unary_expression",
 /*  32 */ "binary_expression ::= binary_expression LOGOR binary_expression",
 /*  33 */ "binary_expression ::= binary_expression LOGAND binary_expression",
 /*  34 */ "binary_expression ::= binary_expression BITOR binary_expression",
 /*  35 */ "binary_expression ::= binary_expression BITXOR binary_expression",
 /*  36 */ "binary_expression ::= binary_expression BITAND binary_expression",
 /*  37 */ "binary_expression ::= binary_expression EQUALS binary_expression",
 /*  38 */ "binary_expression ::= binary_expression NEQUALS binary_expression",
 /*  39 */ "binary_expression ::= binary_expression ST binary_expression",
 /*  40 */ "binary_expression ::= binary_expression SE binary_expression",
 /*  41 */ "binary_expression ::= binary_expression GT binary_expression",
 /*  42 */ "binary_expression ::= binary_expression GE binary_expression",
 /*  43 */ "binary_expression ::= binary_expression ADDOP binary_expression",
 /*  44 */ "binary_expression ::= binary_expression SUBOP binary_expression",
 /*  45 */ "binary_expression ::= binary_expression MULOP binary_expression",
 /*  46 */ "binary_expression ::= binary_expression DIVOP binary_expression",
 /*  47 */ "binary_expression ::= binary_expression MODOP binary_expression",
 /*  48 */ "unary_expression ::= postfix_expression",
 /*  49 */ "unary_expression ::= SUBOP unary_expression",
 /*  50 */ "unary_expression ::= ADDADD unary_expression",
 /*  51 */ "unary_expression ::= SUBSUB unary_expression",
 /*  52 */ "postfix_expression ::= primary_expression",
 /*  53 */ "postfix_expression ::= postfix_expression ADDADD",
 /*  54 */ "postfix_expression ::= postfix_expression SUBSUB",
 /*  55 */ "postfix_expression ::= postfix_expression DOT id_expression",
 /*  56 */ "postfix_expression ::= postfix_expression LBRACKET expression RBRACKET",
 /*  57 */ "postfix_expression ::= IDENTIFIER LPAREN RPAREN",
 /*  58 */ "postfix_expression ::= IDENTIFIER LPAREN argument_list RPAREN",
 /*  59 */ "primary_expression ::= literal",
 /*  60 */ "primary_expression ::= id_expression",
 /*  61 */ "primary_expression ::= LPAREN expression RPAREN",
 /*  62 */ "primary_expression ::= list_literal",
 /*  63 */ "literal ::= INTEGER",
 /*  64 */ "literal ::= REAL",
 /*  65 */ "literal ::= STRING",
 /*  66 */ "literal ::= TRUE",
 /*  67 */ "literal ::= FALSE",
 /*  68 */ "literal ::= NULL",
 /*  69 */ "id_expression ::= IDENTIFIER",
 /*  70 */ "list_literal ::= LBRACKET list_content RBRACKET",
 /*  71 */ "list_content ::= list_entry",
 /*  72 */ "list_content ::= list_content COMMA list_entry",
 /*  73 */ "list_entry ::= expression",
 /*  74 */ "argument ::= expression",
 /*  75 */ "argument_list ::= argument",
 /*  76 */ "argument_list ::= argument_list COMMA argument",
 /*  77 */ "expression_statement ::= SEMICOLON",
 /*  78 */ "expression_statement ::= expression SEMICOLON",
 /*  79 */ "compound_statement ::= LBRACE RBRACE",
 /*  80 */ "compound_statement ::= LBRACE statement_sequence RBRACE",
 /*  81 */ "include_statement ::= INCLUDE STRING SEMICOLON",
 /*  82 */ "return_statement ::= RETURN expression SEMICOLON",
 /*  83 */ "return_statement ::= RETURN SEMICOLON",
 /*  84 */ "break_statement ::= BREAK SEMICOLON",
 /*  85 */ "continue_statement ::= CONTINUE SEMICOLON",
 /*  86 */ "declaration_statement ::= declaration_sequence SEMICOLON",
 /*  87 */ "declaration_statement ::= function_declaration",
 /*  88 */ "declaration_sequence ::= VAR variable_declaration",
 /*  89 */ "declaration_sequence ::= declaration_sequence COMMA variable_declaration",
 /*  90 */ "variable_declaration ::= simple_declaration",
 /*  91 */ "variable_declaration ::= init_declaration",
 /*  92 */ "simple_declaration ::= IDENTIFIER",
 /*  93 */ "init_declaration ::= IDENTIFIER ASSIGN expression",
 /*  94 */ "function_declaration ::= FUNCTION IDENTIFIER LPAREN parameter_list RPAREN function_body",
 /*  95 */ "function_declaration ::= FUNCTION IDENTIFIER LPAREN RPAREN function_body",
 /*  96 */ "parameter ::= IDENTIFIER",
 /*  97 */ "opt_parameter ::= IDENTIFIER ASSIGN expression",
 /*  98 */ "parameters ::= parameter",
 /*  99 */ "parameters ::= parameters COMMA parameter",
 /* 100 */ "opt_parameters ::= opt_parameter",
 /* 101 */ "opt_parameters ::= opt_parameters COMMA opt_parameter",
 /* 102 */ "parameter_list ::= parameters",
 /* 103 */ "parameter_list ::= opt_parameters",
 /* 104 */ "parameter_list ::= parameters COMMA opt_parameters",
 /* 105 */ "function_body ::= statement",
 /* 106 */ "for_statement ::= FOR LPAREN for_init_statement expression SEMICOLON expression_opt RPAREN statement",
 /* 107 */ "for_init_statement ::= expression_statement",
 /* 108 */ "for_init_statement ::= declaration_sequence SEMICOLON",
 /* 109 */ "foreach_statement ::= FOR LPAREN id_expression IN expression RPAREN statement",
 /* 110 */ "if_statement ::= IF LPAREN expression RPAREN statement",
 /* 111 */ "if_statement ::= IF LPAREN expression RPAREN statement ELSE statement",
 /* 112 */ "while_statement ::= WHILE LPAREN expression RPAREN statement",
 /* 113 */ "switch_statement ::= SWITCH LPAREN expression RPAREN LBRACE switch_body RBRACE",
 /* 114 */ "switch_body ::=",
 /* 115 */ "switch_body ::= switch_body switch_case",
 /* 116 */ "switch_body ::= switch_body default_case",
 /* 117 */ "switch_case ::= CASE literal COLON case_statements",
 /* 118 */ "default_case ::= DEFAULT COLON case_statements",
 /* 119 */ "case_statements ::= statement_sequence",
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
  { 60, 1 },
  { 62, 1 },
  { 62, 2 },
  { 61, 0 },
  { 61, 1 },
  { 63, 1 },
  { 63, 1 },
  { 63, 1 },
  { 63, 1 },
  { 63, 1 },
  { 63, 1 },
  { 63, 1 },
  { 63, 1 },
  { 63, 1 },
  { 63, 1 },
  { 63, 1 },
  { 63, 1 },
  { 76, 1 },
  { 78, 0 },
  { 78, 1 },
  { 77, 1 },
  { 77, 3 },
  { 81, 1 },
  { 81, 1 },
  { 81, 1 },
  { 81, 1 },
  { 81, 1 },
  { 81, 1 },
  { 79, 1 },
  { 79, 5 },
  { 80, 1 },
  { 80, 3 },
  { 80, 3 },
  { 80, 3 },
  { 80, 3 },
  { 80, 3 },
  { 80, 3 },
  { 80, 3 },
  { 80, 3 },
  { 80, 3 },
  { 80, 3 },
  { 80, 3 },
  { 80, 3 },
  { 80, 3 },
  { 80, 3 },
  { 80, 3 },
  { 80, 3 },
  { 82, 1 },
  { 82, 2 },
  { 82, 2 },
  { 82, 2 },
  { 83, 1 },
  { 83, 2 },
  { 83, 2 },
  { 83, 3 },
  { 83, 4 },
  { 83, 3 },
  { 83, 4 },
  { 84, 1 },
  { 84, 1 },
  { 84, 3 },
  { 84, 1 },
  { 87, 1 },
  { 87, 1 },
  { 87, 1 },
  { 87, 1 },
  { 87, 1 },
  { 87, 1 },
  { 85, 1 },
  { 88, 3 },
  { 89, 1 },
  { 89, 3 },
  { 90, 1 },
  { 91, 1 },
  { 86, 1 },
  { 86, 3 },
  { 65, 1 },
  { 65, 2 },
  { 68, 2 },
  { 68, 3 },
  { 64, 3 },
  { 72, 3 },
  { 72, 2 },
  { 74, 2 },
  { 75, 2 },
  { 66, 2 },
  { 66, 1 },
  { 92, 2 },
  { 92, 3 },
  { 94, 1 },
  { 94, 1 },
  { 95, 1 },
  { 96, 3 },
  { 93, 6 },
  { 93, 5 },
  { 99, 1 },
  { 100, 3 },
  { 101, 1 },
  { 101, 3 },
  { 102, 1 },
  { 102, 3 },
  { 97, 1 },
  { 97, 1 },
  { 97, 3 },
  { 98, 1 },
  { 67, 8 },
  { 103, 1 },
  { 103, 2 },
  { 71, 7 },
  { 69, 5 },
  { 69, 7 },
  { 70, 5 },
  { 73, 7 },
  { 104, 0 },
  { 104, 2 },
  { 104, 2 },
  { 105, 4 },
  { 106, 3 },
  { 107, 1 },
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
{ p->SetRoot(yymsp[0].minor.yy31); }
#line 1113 "astgen.c"
        break;
      case 1:
#line 74 "astgen.in"
{ yygotominor.yy31 = new Ast(translation_unit, yymsp[0].minor.yy31); }
#line 1118 "astgen.c"
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
      case 21:
      case 29:
      case 31:
      case 48:
      case 52:
      case 59:
      case 60:
      case 62:
      case 75:
      case 87:
      case 88:
      case 90:
      case 91:
      case 98:
      case 100:
      case 102:
      case 103:
      case 105:
      case 107:
      case 119:
#line 77 "astgen.in"
{ yygotominor.yy31 = yymsp[0].minor.yy31; }
#line 1158 "astgen.c"
        break;
      case 3:
#line 78 "astgen.in"
{ 
  if(yymsp[-1].minor.yy31->m_type == statement_sequence) {
    yygotominor.yy31 = yymsp[-1].minor.yy31;
  }
  else {
    yygotominor.yy31 = new Ast(statement_sequence, new AstList);
    any_cast<AstList*>(yygotominor.yy31->m_a1)->push_back(yymsp[-1].minor.yy31);
  }
  any_cast<AstList*>(yygotominor.yy31->m_a1)->push_back(yymsp[0].minor.yy31);
}
#line 1172 "astgen.c"
        break;
      case 4:
      case 19:
#line 91 "astgen.in"
{ yygotominor.yy31 = 0; }
#line 1178 "astgen.c"
        break;
      case 22:
#line 122 "astgen.in"
{ yygotominor.yy31 = new Ast(assignment_expression, yymsp[-1].minor.yy198, yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1183 "astgen.c"
        break;
      case 23:
#line 126 "astgen.in"
{ yygotominor.yy198 = op_assign; }
#line 1188 "astgen.c"
        break;
      case 24:
#line 127 "astgen.in"
{ yygotominor.yy198 = op_assadd; }
#line 1193 "astgen.c"
        break;
      case 25:
#line 128 "astgen.in"
{ yygotominor.yy198 = op_asssub; }
#line 1198 "astgen.c"
        break;
      case 26:
#line 129 "astgen.in"
{ yygotominor.yy198 = op_assmul; }
#line 1203 "astgen.c"
        break;
      case 27:
#line 130 "astgen.in"
{ yygotominor.yy198 = op_assdiv; }
#line 1208 "astgen.c"
        break;
      case 28:
#line 131 "astgen.in"
{ yygotominor.yy198 = op_assmod; }
#line 1213 "astgen.c"
        break;
      case 30:
#line 135 "astgen.in"
{ yygotominor.yy31 = new Ast(ternary_expression, yymsp[-4].minor.yy31, yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1218 "astgen.c"
        break;
      case 32:
#line 139 "astgen.in"
{ yygotominor.yy31 = new Ast(binary_expression, op_logor,   yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1223 "astgen.c"
        break;
      case 33:
#line 140 "astgen.in"
{ yygotominor.yy31 = new Ast(binary_expression, op_logand,  yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1228 "astgen.c"
        break;
      case 34:
#line 141 "astgen.in"
{ yygotominor.yy31 = new Ast(binary_expression, op_bitor,   yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1233 "astgen.c"
        break;
      case 35:
#line 142 "astgen.in"
{ yygotominor.yy31 = new Ast(binary_expression, op_bitxor,  yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1238 "astgen.c"
        break;
      case 36:
#line 143 "astgen.in"
{ yygotominor.yy31 = new Ast(binary_expression, op_bitand,  yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1243 "astgen.c"
        break;
      case 37:
#line 144 "astgen.in"
{ yygotominor.yy31 = new Ast(binary_expression, op_eq,   yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1248 "astgen.c"
        break;
      case 38:
#line 145 "astgen.in"
{ yygotominor.yy31 = new Ast(binary_expression, op_ne,   yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1253 "astgen.c"
        break;
      case 39:
#line 146 "astgen.in"
{ yygotominor.yy31 = new Ast(binary_expression, op_lt,   yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1258 "astgen.c"
        break;
      case 40:
#line 147 "astgen.in"
{ yygotominor.yy31 = new Ast(binary_expression, op_le,   yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1263 "astgen.c"
        break;
      case 41:
#line 148 "astgen.in"
{ yygotominor.yy31 = new Ast(binary_expression, op_gt,   yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1268 "astgen.c"
        break;
      case 42:
#line 149 "astgen.in"
{ yygotominor.yy31 = new Ast(binary_expression, op_ge,   yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1273 "astgen.c"
        break;
      case 43:
#line 150 "astgen.in"
{ yygotominor.yy31 = new Ast(binary_expression, op_add,  yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1278 "astgen.c"
        break;
      case 44:
#line 151 "astgen.in"
{ yygotominor.yy31 = new Ast(binary_expression, op_sub,  yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1283 "astgen.c"
        break;
      case 45:
#line 152 "astgen.in"
{ yygotominor.yy31 = new Ast(binary_expression, op_mul,  yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1288 "astgen.c"
        break;
      case 46:
#line 153 "astgen.in"
{ yygotominor.yy31 = new Ast(binary_expression, op_div,  yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1293 "astgen.c"
        break;
      case 47:
#line 154 "astgen.in"
{ yygotominor.yy31 = new Ast(binary_expression, op_mod,  yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1298 "astgen.c"
        break;
      case 49:
#line 158 "astgen.in"
{ yygotominor.yy31 = new Ast(prefix_expression, op_negate, yymsp[0].minor.yy31); }
#line 1303 "astgen.c"
        break;
      case 50:
#line 159 "astgen.in"
{ yygotominor.yy31 = new Ast(prefix_expression, op_preinc, yymsp[0].minor.yy31); }
#line 1308 "astgen.c"
        break;
      case 51:
#line 160 "astgen.in"
{ yygotominor.yy31 = new Ast(prefix_expression, op_predec, yymsp[0].minor.yy31); }
#line 1313 "astgen.c"
        break;
      case 53:
#line 164 "astgen.in"
{ yygotominor.yy31 = new Ast(postfix_expression, op_postinc, yymsp[-1].minor.yy31); }
#line 1318 "astgen.c"
        break;
      case 54:
#line 165 "astgen.in"
{ yygotominor.yy31 = new Ast(postfix_expression, op_postdec, yymsp[-1].minor.yy31); }
#line 1323 "astgen.c"
        break;
      case 55:
#line 166 "astgen.in"
{ yygotominor.yy31 = new Ast(member_expression, yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1328 "astgen.c"
        break;
      case 56:
#line 167 "astgen.in"
{ yygotominor.yy31 = new Ast(index_expression, yymsp[-3].minor.yy31, yymsp[-1].minor.yy31); }
#line 1333 "astgen.c"
        break;
      case 57:
#line 168 "astgen.in"
{ yygotominor.yy31 = new Ast(function_call, String(yymsp[-2].minor.yy0)); }
#line 1338 "astgen.c"
        break;
      case 58:
#line 169 "astgen.in"
{ yygotominor.yy31 = new Ast(function_call, String(yymsp[-3].minor.yy0), yymsp[-1].minor.yy31); }
#line 1343 "astgen.c"
        break;
      case 61:
      case 86:
      case 108:
#line 174 "astgen.in"
{ yygotominor.yy31 = yymsp[-1].minor.yy31; }
#line 1350 "astgen.c"
        break;
      case 63:
#line 178 "astgen.in"
{ yygotominor.yy31 = new Ast(literal, Variant(String(yymsp[0].minor.yy0), Variant::stInt)); }
#line 1355 "astgen.c"
        break;
      case 64:
#line 179 "astgen.in"
{ yygotominor.yy31 = new Ast(literal, Variant(String(yymsp[0].minor.yy0), Variant::stReal)); }
#line 1360 "astgen.c"
        break;
      case 65:
#line 180 "astgen.in"
{ yygotominor.yy31 = new Ast(literal, Variant(String(yymsp[0].minor.yy0), Variant::stString)); }
#line 1365 "astgen.c"
        break;
      case 66:
#line 181 "astgen.in"
{ yygotominor.yy31 = new Ast(literal, Variant(true));    }
#line 1370 "astgen.c"
        break;
      case 67:
#line 182 "astgen.in"
{ yygotominor.yy31 = new Ast(literal, Variant(false));   }
#line 1375 "astgen.c"
        break;
      case 68:
#line 183 "astgen.in"
{ yygotominor.yy31 = new Ast(literal, Variant());        }
#line 1380 "astgen.c"
        break;
      case 69:
#line 186 "astgen.in"
{ yygotominor.yy31 = new Ast(lvalue, String(yymsp[0].minor.yy0)); }
#line 1385 "astgen.c"
        break;
      case 70:
#line 189 "astgen.in"
{ yygotominor.yy31 = new Ast(list_literal, yymsp[-1].minor.yy31); }
#line 1390 "astgen.c"
        break;
      case 71:
#line 191 "astgen.in"
{ yygotominor.yy31 = new Ast(list_content, yymsp[0].minor.yy31); }
#line 1395 "astgen.c"
        break;
      case 72:
#line 192 "astgen.in"
{ yygotominor.yy31 = new Ast(list_content, yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1400 "astgen.c"
        break;
      case 73:
#line 194 "astgen.in"
{ yygotominor.yy31 = new Ast(list_entry, yymsp[0].minor.yy31); }
#line 1405 "astgen.c"
        break;
      case 74:
#line 203 "astgen.in"
{ yygotominor.yy31 = new Ast(argument, yymsp[0].minor.yy31); }
#line 1410 "astgen.c"
        break;
      case 76:
#line 207 "astgen.in"
{ yygotominor.yy31 = new Ast(argument_list, yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1415 "astgen.c"
        break;
      case 77:
#line 215 "astgen.in"
{ yygotominor.yy31 = new Ast(empty_statement); }
#line 1420 "astgen.c"
        break;
      case 78:
#line 216 "astgen.in"
{ yygotominor.yy31 = new Ast(expression_statement, yymsp[-1].minor.yy31); }
#line 1425 "astgen.c"
        break;
      case 79:
#line 219 "astgen.in"
{ yygotominor.yy31 = new Ast(compound_statement); }
#line 1430 "astgen.c"
        break;
      case 80:
#line 220 "astgen.in"
{ yygotominor.yy31 = new Ast(compound_statement, yymsp[-1].minor.yy31); }
#line 1435 "astgen.c"
        break;
      case 81:
#line 223 "astgen.in"
{ p->Parse(yymsp[-1].minor.yy0); yygotominor.yy31 = p->GetRoot(); }
#line 1440 "astgen.c"
        break;
      case 82:
#line 226 "astgen.in"
{ yygotominor.yy31 = new Ast(return_statement, yymsp[-1].minor.yy31); }
#line 1445 "astgen.c"
        break;
      case 83:
#line 227 "astgen.in"
{ yygotominor.yy31 = new Ast(return_statement);    }
#line 1450 "astgen.c"
        break;
      case 84:
#line 230 "astgen.in"
{ yygotominor.yy31 = new Ast(break_statement); }
#line 1455 "astgen.c"
        break;
      case 85:
#line 231 "astgen.in"
{ yygotominor.yy31 = new Ast(continue_statement); }
#line 1460 "astgen.c"
        break;
      case 89:
#line 244 "astgen.in"
{ yygotominor.yy31 = new Ast(declaration_sequence, yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1465 "astgen.c"
        break;
      case 92:
#line 251 "astgen.in"
{ yygotominor.yy31 = new Ast(variable_declaration, String(yymsp[0].minor.yy0)); }
#line 1470 "astgen.c"
        break;
      case 93:
#line 254 "astgen.in"
{ yygotominor.yy31 = new Ast(variable_declaration, String(yymsp[-2].minor.yy0), yymsp[0].minor.yy31); }
#line 1475 "astgen.c"
        break;
      case 94:
#line 262 "astgen.in"
{ yygotominor.yy31 = new Ast(function_declaration, String(yymsp[-4].minor.yy0), yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1480 "astgen.c"
        break;
      case 95:
#line 263 "astgen.in"
{ yygotominor.yy31 = new Ast(function_declaration, String(yymsp[-3].minor.yy0), any(), yymsp[0].minor.yy31); }
#line 1485 "astgen.c"
        break;
      case 96:
#line 266 "astgen.in"
{ yygotominor.yy31 = new Ast(parameter, String(yymsp[0].minor.yy0)); }
#line 1490 "astgen.c"
        break;
      case 97:
#line 269 "astgen.in"
{ yygotominor.yy31 = new Ast(parameter, String(yymsp[-2].minor.yy0), yymsp[0].minor.yy31); }
#line 1495 "astgen.c"
        break;
      case 99:
      case 101:
      case 104:
#line 273 "astgen.in"
{ yygotominor.yy31 = new Ast(parameter_list, yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1502 "astgen.c"
        break;
      case 106:
#line 294 "astgen.in"
{ yygotominor.yy31 = new Ast(for_statement, yymsp[-5].minor.yy31, yymsp[-4].minor.yy31, yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1507 "astgen.c"
        break;
      case 109:
#line 305 "astgen.in"
{ yygotominor.yy31 = new Ast(foreach_statement, yymsp[-4].minor.yy31, yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1512 "astgen.c"
        break;
      case 110:
#line 316 "astgen.in"
{ yygotominor.yy31 = new Ast(if_statement, yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1517 "astgen.c"
        break;
      case 111:
#line 317 "astgen.in"
{ yygotominor.yy31 = new Ast(if_statement, yymsp[-4].minor.yy31, yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1522 "astgen.c"
        break;
      case 112:
#line 325 "astgen.in"
{ yygotominor.yy31 = new Ast(while_statement, yymsp[-2].minor.yy31,  yymsp[0].minor.yy31); }
#line 1527 "astgen.c"
        break;
      case 113:
#line 333 "astgen.in"
{ yygotominor.yy31 = new Ast(switch_statement, yymsp[-4].minor.yy31, yymsp[-1].minor.yy127); }
#line 1532 "astgen.c"
        break;
      case 114:
#line 337 "astgen.in"
{ yygotominor.yy127 = new AstList; }
#line 1537 "astgen.c"
        break;
      case 115:
      case 116:
#line 338 "astgen.in"
{ yygotominor.yy127 = yymsp[-1].minor.yy127; yygotominor.yy127->push_back(yymsp[0].minor.yy31); }
#line 1543 "astgen.c"
        break;
      case 117:
#line 342 "astgen.in"
{ yygotominor.yy31 = new Ast(switch_case, yymsp[-2].minor.yy31, yymsp[0].minor.yy31); }
#line 1548 "astgen.c"
        break;
      case 118:
#line 345 "astgen.in"
{ yygotominor.yy31 = new Ast(default_case, yymsp[0].minor.yy31); }
#line 1553 "astgen.c"
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
#line 1601 "astgen.c"
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
#line 1619 "astgen.c"
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

