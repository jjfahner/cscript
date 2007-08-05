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
#define YYNOCODE 93
#define YYACTIONTYPE unsigned short int
#define AstGenParseTOKENTYPE  Token 
typedef union {
  AstGenParseTOKENTYPE yy0;
  Ast* yy127;
  opcodes yy174;
  int yy185;
} YYMINORTYPE;
#ifndef YYSTACKDEPTH
#define YYSTACKDEPTH 100
#endif
#define AstGenParseARG_SDECL  AstGen* p ;
#define AstGenParseARG_PDECL , AstGen* p 
#define AstGenParseARG_FETCH  AstGen* p  = yypParser->p 
#define AstGenParseARG_STORE yypParser->p  = p 
#define YYNSTATE 174
#define YYNRULE 101
#define YYERRORSYMBOL 53
#define YYERRSYMDT yy185
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
 /*     0 */   276,   95,    6,  164,  159,  155,  153,  150,  148,  146,
 /*    10 */   143,  141,  139,   73,  135,   11,  126,   50,   21,  109,
 /*    20 */    64,   99,  145,    9,  113,  167,  130,  123,  124,   67,
 /*    30 */   128,  137,  159,  155,  153,  150,  148,  146,  143,  141,
 /*    40 */   139,   73,  135,   91,  126,   50,   86,  109,   64,   99,
 /*    50 */   145,  131,  113,  167,   43,   39,   40,   67,  128,  102,
 /*    60 */   100,   52,   25,  171,    4,  164,  159,  155,  153,  150,
 /*    70 */   148,  146,  143,  141,  139,   73,  135,   23,  126,   50,
 /*    80 */     5,  109,   64,   99,  145,   49,  113,  167,  122,  123,
 /*    90 */   124,   67,  128,  137,  159,  155,  153,  150,  148,  146,
 /*   100 */   143,  141,  139,   73,  135,   18,  126,   50,  147,  109,
 /*   110 */    64,   99,  145,    7,  113,  167,   47,  120,   15,   67,
 /*   120 */   128,   47,  158,  117,  168,  136,    3,   37,   36,   34,
 /*   130 */    33,   38,   31,   30,   29,   42,   32,   41,   28,   35,
 /*   140 */    43,   39,   40,  129,  125,  121,  119,  116,  115,   19,
 /*   150 */   144,  159,  155,  153,  150,  148,  146,  143,  141,  139,
 /*   160 */    73,  135,    8,  126,   50,   96,  109,   64,   99,  145,
 /*   170 */   170,  113,  167,   70,   94,  172,   67,  128,  165,  159,
 /*   180 */   155,  153,  150,  148,  146,  143,  141,  139,   73,  135,
 /*   190 */    12,  126,   50,  169,  109,   64,   99,  145,   46,  113,
 /*   200 */   167,  112,    2,   20,   67,  128,  168,  114,   14,   51,
 /*   210 */    45,   44,   26,   13,   10,   78,   24,   16,  163,  160,
 /*   220 */   157,  154,  152,  149,   27,  106,    1,  173,   72,   17,
 /*   230 */    48,   89,   87,   46,   76,  108,   77,   82,   97,   64,
 /*   240 */    99,  145,  277,  113,  167,   45,   44,  277,   13,  277,
 /*   250 */    78,   24,  277,  163,  160,  157,  154,  152,  149,  277,
 /*   260 */   106,    1,  111,   72,   17,   48,   89,   87,  277,  277,
 /*   270 */   277,   77,   82,  161,  159,  155,  153,  150,  148,  146,
 /*   280 */   143,  141,  139,   73,  135,  277,  126,   50,  277,  109,
 /*   290 */    64,   99,  145,  277,  113,  167,  277,  277,  277,   67,
 /*   300 */   128,  277,  277,  151,  159,  155,  153,  150,  148,  146,
 /*   310 */   143,  141,  139,   73,  135,  277,  126,   50,  277,  109,
 /*   320 */    64,   99,  145,  277,  113,  167,  277,  277,  277,   67,
 /*   330 */   128,  156,  159,  155,  153,  150,  148,  146,  143,  141,
 /*   340 */   139,   73,  135,  277,  126,   50,  277,  109,   64,   99,
 /*   350 */   145,  277,  113,  167,  277,  277,  277,   67,  128,  277,
 /*   360 */   277,   81,  159,  155,  153,  150,  148,  146,  143,  141,
 /*   370 */   139,   73,  135,  277,  126,   50,  277,  109,   64,   99,
 /*   380 */   145,   46,  113,  167,  277,  277,  277,   67,  128,  277,
 /*   390 */   277,  277,  277,   45,   44,  277,   13,  277,   78,   24,
 /*   400 */   277,  163,  160,  157,  154,  152,  149,  277,  106,    1,
 /*   410 */   277,   72,   17,   48,   89,   87,  277,  277,  277,   77,
 /*   420 */    82,  277,   36,   34,   33,   38,   31,   30,   29,   42,
 /*   430 */    32,   41,   28,   35,   43,   39,   40,   34,   33,   38,
 /*   440 */    31,   30,   29,   42,   32,   41,   28,   35,   43,   39,
 /*   450 */    40,   46,   31,   30,   29,   42,   32,   41,   28,   35,
 /*   460 */    43,   39,   40,   45,   44,  277,   13,  277,   78,   24,
 /*   470 */   277,  163,  160,  157,  154,  152,  149,   46,  106,   93,
 /*   480 */    64,   99,  145,   48,  113,  167,  277,  277,  277,   45,
 /*   490 */    44,  277,   13,  277,   78,   24,  104,  163,  160,  157,
 /*   500 */   154,  152,  149,  166,   98,   64,   99,  145,  277,  113,
 /*   510 */   167,   73,  135,  277,  126,   50,  277,  109,   64,   99,
 /*   520 */    84,  277,  113,  167,  277,  140,  135,   68,  126,   50,
 /*   530 */   277,  109,   64,   99,  145,   22,  113,  167,  277,  142,
 /*   540 */    33,   38,   31,   30,   29,   42,   32,   41,   28,   35,
 /*   550 */    43,   39,   40,  277,   46,   29,   42,   32,   41,   28,
 /*   560 */    35,   43,   39,   40,  277,  277,   45,   44,  277,   13,
 /*   570 */   277,   78,   24,  277,  163,  160,  157,  154,  152,  149,
 /*   580 */    46,  118,  277,   63,  277,  109,   64,   99,  145,  277,
 /*   590 */   113,  167,   45,   44,  277,   13,  277,   78,   24,  277,
 /*   600 */   163,  160,  157,  154,  152,  149,  277,  277,  277,  140,
 /*   610 */   135,  277,  126,   50,  277,  109,   64,   99,  145,  277,
 /*   620 */   113,  167,   69,  138,   54,  277,  109,   64,   99,  145,
 /*   630 */   277,  113,  167,  277,  277,  134,  135,  277,  126,   50,
 /*   640 */   277,  109,   64,   99,  145,   71,  113,  167,  277,  277,
 /*   650 */   132,   38,   31,   30,   29,   42,   32,   41,   28,   35,
 /*   660 */    43,   39,   40,  162,  135,   88,  126,   50,  277,  109,
 /*   670 */    64,   99,  145,  277,  113,  167,  277,  134,  135,  277,
 /*   680 */   126,   50,  277,  109,   64,   99,  145,  277,  113,  167,
 /*   690 */    75,  135,  133,  126,   50,  277,  109,   64,   99,  145,
 /*   700 */   277,  113,  167,   85,  135,  277,  126,   50,  277,  109,
 /*   710 */    64,   99,  145,  277,  113,  167,  277,  277,   83,  135,
 /*   720 */   277,  126,   50,  277,  109,   64,   99,  145,  277,  113,
 /*   730 */   167,   92,  135,  277,  126,   50,  277,  109,   64,   99,
 /*   740 */   145,  277,  113,  167,  277,   80,  135,  277,  126,   50,
 /*   750 */   277,  109,   64,   99,  145,  277,  113,  167,   79,  135,
 /*   760 */   277,  126,   50,  277,  109,   64,   99,  145,  277,  113,
 /*   770 */   167,   74,  135,  277,  126,   50,  277,  109,   64,   99,
 /*   780 */   145,  277,  113,  167,  277,  277,  127,  135,  277,  126,
 /*   790 */    50,  277,  109,   64,   99,  145,  277,  113,  167,   90,
 /*   800 */   135,  277,  126,   50,  277,  109,   64,   99,  145,  277,
 /*   810 */   113,  167,  107,  277,  126,   50,  277,  109,   64,   99,
 /*   820 */   145,  277,  113,  167,  277,  110,  277,  126,   50,  277,
 /*   830 */   109,   64,   99,  145,  277,  113,  167,   60,  277,  109,
 /*   840 */    64,   99,  145,  277,  113,  167,   65,  277,  109,   64,
 /*   850 */    99,  145,  277,  113,  167,  101,  277,  109,   64,   99,
 /*   860 */   145,  277,  113,  167,   53,  277,  109,   64,   99,  145,
 /*   870 */   277,  113,  167,  277,  103,  277,  109,   64,   99,  145,
 /*   880 */   277,  113,  167,   56,  277,  109,   64,   99,  145,  277,
 /*   890 */   113,  167,  105,  277,  109,   64,   99,  145,  277,  113,
 /*   900 */   167,  277,   61,  277,  109,   64,   99,  145,  277,  113,
 /*   910 */   167,   58,  277,  109,   64,   99,  145,  277,  113,  167,
 /*   920 */    62,  277,  109,   64,   99,  145,  277,  113,  167,   66,
 /*   930 */   277,  109,   64,   99,  145,  277,  113,  167,   57,  277,
 /*   940 */   109,   64,   99,  145,  277,  113,  167,  277,   55,  277,
 /*   950 */   109,   64,   99,  145,  277,  113,  167,   59,  277,  109,
 /*   960 */    64,   99,  145,  277,  113,  167,   28,   35,   43,   39,
 /*   970 */    40,
};
static const YYCODETYPE yy_lookahead[] = {
 /*     0 */    54,   55,   56,   57,   58,   59,   60,   61,   62,   63,
 /*    10 */    64,   65,   66,   67,   68,   31,   70,   71,   17,   73,
 /*    20 */    74,   75,   76,   32,   78,   79,   85,   86,   87,   83,
 /*    30 */    84,   57,   58,   59,   60,   61,   62,   63,   64,   65,
 /*    40 */    66,   67,   68,   30,   70,   71,   30,   73,   74,   75,
 /*    50 */    76,   29,   78,   79,   14,   15,   16,   83,   84,   25,
 /*    60 */    26,   27,   28,   89,   56,   57,   58,   59,   60,   61,
 /*    70 */    62,   63,   64,   65,   66,   67,   68,   48,   70,   71,
 /*    80 */    32,   73,   74,   75,   76,   31,   78,   79,   85,   86,
 /*    90 */    87,   83,   84,   57,   58,   59,   60,   61,   62,   63,
 /*   100 */    64,   65,   66,   67,   68,   31,   70,   71,   29,   73,
 /*   110 */    74,   75,   76,   50,   78,   79,   39,   40,   39,   83,
 /*   120 */    84,   39,   40,   40,   30,   89,   32,    1,    2,    3,
 /*   130 */     4,    5,    6,    7,    8,    9,   10,   11,   12,   13,
 /*   140 */    14,   15,   16,   17,   18,   19,   20,   21,   22,   23,
 /*   150 */    57,   58,   59,   60,   61,   62,   63,   64,   65,   66,
 /*   160 */    67,   68,   32,   70,   71,   76,   73,   74,   75,   76,
 /*   170 */    32,   78,   79,   88,   30,   90,   83,   84,   57,   58,
 /*   180 */    59,   60,   61,   62,   63,   64,   65,   66,   67,   68,
 /*   190 */    31,   70,   71,   90,   73,   74,   75,   76,   13,   78,
 /*   200 */    79,   32,   32,   31,   83,   84,   30,   40,   39,   39,
 /*   210 */    25,   26,   72,   28,   32,   30,   31,   40,   33,   34,
 /*   220 */    35,   36,   37,   38,   24,   40,   41,   42,   43,   44,
 /*   230 */    45,   46,   47,   13,   35,   40,   51,   52,   73,   74,
 /*   240 */    75,   76,   92,   78,   79,   25,   26,   92,   28,   92,
 /*   250 */    30,   31,   92,   33,   34,   35,   36,   37,   38,   92,
 /*   260 */    40,   41,   42,   43,   44,   45,   46,   47,   92,   92,
 /*   270 */    92,   51,   52,   57,   58,   59,   60,   61,   62,   63,
 /*   280 */    64,   65,   66,   67,   68,   92,   70,   71,   92,   73,
 /*   290 */    74,   75,   76,   92,   78,   79,   92,   92,   92,   83,
 /*   300 */    84,   92,   92,   57,   58,   59,   60,   61,   62,   63,
 /*   310 */    64,   65,   66,   67,   68,   92,   70,   71,   92,   73,
 /*   320 */    74,   75,   76,   92,   78,   79,   92,   92,   92,   83,
 /*   330 */    84,   57,   58,   59,   60,   61,   62,   63,   64,   65,
 /*   340 */    66,   67,   68,   92,   70,   71,   92,   73,   74,   75,
 /*   350 */    76,   92,   78,   79,   92,   92,   92,   83,   84,   92,
 /*   360 */    92,   57,   58,   59,   60,   61,   62,   63,   64,   65,
 /*   370 */    66,   67,   68,   92,   70,   71,   92,   73,   74,   75,
 /*   380 */    76,   13,   78,   79,   92,   92,   92,   83,   84,   92,
 /*   390 */    92,   92,   92,   25,   26,   92,   28,   92,   30,   31,
 /*   400 */    92,   33,   34,   35,   36,   37,   38,   92,   40,   41,
 /*   410 */    92,   43,   44,   45,   46,   47,   92,   92,   92,   51,
 /*   420 */    52,   92,    2,    3,    4,    5,    6,    7,    8,    9,
 /*   430 */    10,   11,   12,   13,   14,   15,   16,    3,    4,    5,
 /*   440 */     6,    7,    8,    9,   10,   11,   12,   13,   14,   15,
 /*   450 */    16,   13,    6,    7,    8,    9,   10,   11,   12,   13,
 /*   460 */    14,   15,   16,   25,   26,   92,   28,   92,   30,   31,
 /*   470 */    92,   33,   34,   35,   36,   37,   38,   13,   40,   73,
 /*   480 */    74,   75,   76,   45,   78,   79,   92,   92,   92,   25,
 /*   490 */    26,   92,   28,   92,   30,   31,   32,   33,   34,   35,
 /*   500 */    36,   37,   38,   59,   73,   74,   75,   76,   92,   78,
 /*   510 */    79,   67,   68,   92,   70,   71,   92,   73,   74,   75,
 /*   520 */    76,   92,   78,   79,   92,   67,   68,   83,   70,   71,
 /*   530 */    92,   73,   74,   75,   76,   91,   78,   79,   92,   81,
 /*   540 */     4,    5,    6,    7,    8,    9,   10,   11,   12,   13,
 /*   550 */    14,   15,   16,   92,   13,    8,    9,   10,   11,   12,
 /*   560 */    13,   14,   15,   16,   92,   92,   25,   26,   92,   28,
 /*   570 */    92,   30,   31,   92,   33,   34,   35,   36,   37,   38,
 /*   580 */    13,   40,   92,   71,   92,   73,   74,   75,   76,   92,
 /*   590 */    78,   79,   25,   26,   92,   28,   92,   30,   31,   92,
 /*   600 */    33,   34,   35,   36,   37,   38,   92,   92,   92,   67,
 /*   610 */    68,   92,   70,   71,   92,   73,   74,   75,   76,   92,
 /*   620 */    78,   79,   80,   81,   71,   92,   73,   74,   75,   76,
 /*   630 */    92,   78,   79,   92,   92,   67,   68,   92,   70,   71,
 /*   640 */    92,   73,   74,   75,   76,   77,   78,   79,   92,   92,
 /*   650 */    82,    5,    6,    7,    8,    9,   10,   11,   12,   13,
 /*   660 */    14,   15,   16,   67,   68,   69,   70,   71,   92,   73,
 /*   670 */    74,   75,   76,   92,   78,   79,   92,   67,   68,   92,
 /*   680 */    70,   71,   92,   73,   74,   75,   76,   92,   78,   79,
 /*   690 */    67,   68,   82,   70,   71,   92,   73,   74,   75,   76,
 /*   700 */    92,   78,   79,   67,   68,   92,   70,   71,   92,   73,
 /*   710 */    74,   75,   76,   92,   78,   79,   92,   92,   67,   68,
 /*   720 */    92,   70,   71,   92,   73,   74,   75,   76,   92,   78,
 /*   730 */    79,   67,   68,   92,   70,   71,   92,   73,   74,   75,
 /*   740 */    76,   92,   78,   79,   92,   67,   68,   92,   70,   71,
 /*   750 */    92,   73,   74,   75,   76,   92,   78,   79,   67,   68,
 /*   760 */    92,   70,   71,   92,   73,   74,   75,   76,   92,   78,
 /*   770 */    79,   67,   68,   92,   70,   71,   92,   73,   74,   75,
 /*   780 */    76,   92,   78,   79,   92,   92,   67,   68,   92,   70,
 /*   790 */    71,   92,   73,   74,   75,   76,   92,   78,   79,   67,
 /*   800 */    68,   92,   70,   71,   92,   73,   74,   75,   76,   92,
 /*   810 */    78,   79,   68,   92,   70,   71,   92,   73,   74,   75,
 /*   820 */    76,   92,   78,   79,   92,   68,   92,   70,   71,   92,
 /*   830 */    73,   74,   75,   76,   92,   78,   79,   71,   92,   73,
 /*   840 */    74,   75,   76,   92,   78,   79,   71,   92,   73,   74,
 /*   850 */    75,   76,   92,   78,   79,   71,   92,   73,   74,   75,
 /*   860 */    76,   92,   78,   79,   71,   92,   73,   74,   75,   76,
 /*   870 */    92,   78,   79,   92,   71,   92,   73,   74,   75,   76,
 /*   880 */    92,   78,   79,   71,   92,   73,   74,   75,   76,   92,
 /*   890 */    78,   79,   71,   92,   73,   74,   75,   76,   92,   78,
 /*   900 */    79,   92,   71,   92,   73,   74,   75,   76,   92,   78,
 /*   910 */    79,   71,   92,   73,   74,   75,   76,   92,   78,   79,
 /*   920 */    71,   92,   73,   74,   75,   76,   92,   78,   79,   71,
 /*   930 */    92,   73,   74,   75,   76,   92,   78,   79,   71,   92,
 /*   940 */    73,   74,   75,   76,   92,   78,   79,   92,   71,   92,
 /*   950 */    73,   74,   75,   76,   92,   78,   79,   71,   92,   73,
 /*   960 */    74,   75,   76,   92,   78,   79,   12,   13,   14,   15,
 /*   970 */    16,
};
#define YY_SHIFT_USE_DFLT (-17)
#define YY_SHIFT_MAX 92
static const short yy_shift_ofst[] = {
 /*     0 */   368,  185,  368,  368,  220,  368,  368,  368,  368,  368,
 /*    10 */   368,  438,  464,  567,  567,  567,  567,  541,  567,  567,
 /*    20 */   567,  567,  567,  567,  567,  567,  567,  567,  567,  567,
 /*    30 */   567,  567,  567,  567,  567,  567,  567,  567,  567,  567,
 /*    40 */   567,  567,  567,  567,  567,  567,  567,   16,   16,   94,
 /*    50 */   126,  176,  144,  420,  434,  536,  646,  446,  547,  547,
 /*    60 */   954,  954,  954,  954,   34,   40,   40,   77,   82,   79,
 /*    70 */   170,  169,  199,  195,  200,  182,  167,  172,  159,  138,
 /*    80 */   130,   63,   74,   48,   29,  177,    1,  -16,   -9,   13,
 /*    90 */    22,   54,   83,
};
#define YY_REDUCE_USE_DFLT (-60)
#define YY_REDUCE_MAX 52
static const short yy_reduce_ofst[] = {
 /*     0 */   -54,    8,   36,  -26,  216,  274,  216,  246,   93,  121,
 /*    10 */   304,  444,  568,  542,  610,  458,  596,  664,  651,  704,
 /*    20 */   623,  719,  636,  678,  691,  732,  757,  744,  858,  849,
 /*    30 */   886,  840,  831,  812,  877,  775,  553,  793,  867,  803,
 /*    40 */   821,  512,  766,  784,  431,  165,  406,    3,  -59,   85,
 /*    50 */   140,  103,   89,
};
static const YYACTIONTYPE yy_default[] = {
 /*     0 */   177,  275,  275,  275,  275,  275,  178,  275,  275,  275,
 /*    10 */   275,  275,  275,  275,  275,  275,  189,  275,  275,  275,
 /*    20 */   275,  275,  275,  275,  275,  275,  275,  275,  275,  275,
 /*    30 */   275,  275,  275,  275,  275,  275,  275,  275,  275,  275,
 /*    40 */   275,  275,  275,  275,  275,  275,  275,  275,  275,  275,
 /*    50 */   199,  275,  275,  202,  203,  204,  205,  206,  207,  208,
 /*    60 */   210,  211,  209,  212,  218,  214,  213,  275,  275,  275,
 /*    70 */   275,  275,  275,  275,  275,  275,  275,  275,  239,  275,
 /*    80 */   275,  272,  275,  275,  230,  275,  260,  275,  275,  275,
 /*    90 */   275,  275,  275,  219,  239,  174,  225,  220,  221,  222,
 /*   100 */   224,  215,  223,  216,  227,  217,  247,  200,  248,  201,
 /*   110 */   192,  250,  228,  229,  251,  198,  197,  252,  253,  196,
 /*   120 */   254,  195,  257,  258,  259,  194,  191,  261,  255,  193,
 /*   130 */   256,  226,  245,  246,  244,  188,  262,  267,  241,  187,
 /*   140 */   243,  186,  242,  185,  271,  230,  184,  240,  183,  238,
 /*   150 */   182,  273,  237,  181,  236,  180,  274,  235,  270,  179,
 /*   160 */   234,  176,  190,  233,  175,  268,  269,  232,  264,  266,
 /*   170 */   231,  263,  265,  249,
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
  "RETURN",        "VAR",           "FUNCTION",      "FOR",         
  "IN",            "LOWER_THAN_ELSE",  "ELSE",          "IF",          
  "WHILE",         "error",         "translation_unit",  "statement_sequence_opt",
  "statement_sequence",  "statement",     "include_statement",  "expression_statement",
  "declaration_statement",  "for_statement",  "compound_statement",  "if_statement",
  "while_statement",  "foreach_statement",  "return_statement",  "expression",  
  "assignment_expression",  "expression_opt",  "conditional_expression",  "binary_expression",
  "assignment_operator",  "unary_expression",  "postfix_expression",  "primary_expression",
  "id_expression",  "argument_list",  "literal",       "list_literal",
  "list_content",  "list_entry",    "argument",      "declaration_sequence",
  "function_declaration",  "variable_declaration",  "simple_declaration",  "init_declaration",
  "parameter_list",  "function_body",  "parameter",     "for_init_statement",
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
 /*  14 */ "expression ::= assignment_expression",
 /*  15 */ "expression_opt ::=",
 /*  16 */ "expression_opt ::= expression",
 /*  17 */ "assignment_expression ::= conditional_expression",
 /*  18 */ "assignment_expression ::= binary_expression assignment_operator assignment_expression",
 /*  19 */ "assignment_operator ::= ASSIGN",
 /*  20 */ "assignment_operator ::= ASSADD",
 /*  21 */ "assignment_operator ::= ASSSUB",
 /*  22 */ "assignment_operator ::= ASSMUL",
 /*  23 */ "assignment_operator ::= ASSDIV",
 /*  24 */ "assignment_operator ::= ASSMOD",
 /*  25 */ "conditional_expression ::= binary_expression",
 /*  26 */ "conditional_expression ::= binary_expression QUESTION expression COLON assignment_expression",
 /*  27 */ "binary_expression ::= unary_expression",
 /*  28 */ "binary_expression ::= binary_expression LOGOR binary_expression",
 /*  29 */ "binary_expression ::= binary_expression LOGAND binary_expression",
 /*  30 */ "binary_expression ::= binary_expression BITOR binary_expression",
 /*  31 */ "binary_expression ::= binary_expression BITXOR binary_expression",
 /*  32 */ "binary_expression ::= binary_expression BITAND binary_expression",
 /*  33 */ "binary_expression ::= binary_expression EQUALS binary_expression",
 /*  34 */ "binary_expression ::= binary_expression NEQUALS binary_expression",
 /*  35 */ "binary_expression ::= binary_expression ST binary_expression",
 /*  36 */ "binary_expression ::= binary_expression SE binary_expression",
 /*  37 */ "binary_expression ::= binary_expression GT binary_expression",
 /*  38 */ "binary_expression ::= binary_expression GE binary_expression",
 /*  39 */ "binary_expression ::= binary_expression ADDOP binary_expression",
 /*  40 */ "binary_expression ::= binary_expression SUBOP binary_expression",
 /*  41 */ "binary_expression ::= binary_expression MULOP binary_expression",
 /*  42 */ "binary_expression ::= binary_expression DIVOP binary_expression",
 /*  43 */ "binary_expression ::= binary_expression MODOP binary_expression",
 /*  44 */ "unary_expression ::= postfix_expression",
 /*  45 */ "unary_expression ::= SUBOP unary_expression",
 /*  46 */ "unary_expression ::= ADDADD unary_expression",
 /*  47 */ "unary_expression ::= SUBSUB unary_expression",
 /*  48 */ "postfix_expression ::= primary_expression",
 /*  49 */ "postfix_expression ::= postfix_expression ADDADD",
 /*  50 */ "postfix_expression ::= postfix_expression SUBSUB",
 /*  51 */ "postfix_expression ::= postfix_expression DOT id_expression",
 /*  52 */ "postfix_expression ::= postfix_expression LBRACKET expression RBRACKET",
 /*  53 */ "postfix_expression ::= IDENTIFIER LPAREN RPAREN",
 /*  54 */ "postfix_expression ::= IDENTIFIER LPAREN argument_list RPAREN",
 /*  55 */ "primary_expression ::= literal",
 /*  56 */ "primary_expression ::= id_expression",
 /*  57 */ "primary_expression ::= LPAREN expression RPAREN",
 /*  58 */ "primary_expression ::= list_literal",
 /*  59 */ "literal ::= INTEGER",
 /*  60 */ "literal ::= REAL",
 /*  61 */ "literal ::= STRING",
 /*  62 */ "literal ::= TRUE",
 /*  63 */ "literal ::= FALSE",
 /*  64 */ "literal ::= NULL",
 /*  65 */ "id_expression ::= IDENTIFIER",
 /*  66 */ "list_literal ::= LBRACKET list_content RBRACKET",
 /*  67 */ "list_content ::= list_entry",
 /*  68 */ "list_content ::= list_content COMMA list_entry",
 /*  69 */ "list_entry ::= expression",
 /*  70 */ "argument ::= expression",
 /*  71 */ "argument_list ::= argument",
 /*  72 */ "argument_list ::= argument_list COMMA argument",
 /*  73 */ "expression_statement ::= SEMICOLON",
 /*  74 */ "expression_statement ::= expression SEMICOLON",
 /*  75 */ "compound_statement ::= LBRACE RBRACE",
 /*  76 */ "compound_statement ::= LBRACE statement_sequence RBRACE",
 /*  77 */ "include_statement ::= INCLUDE STRING SEMICOLON",
 /*  78 */ "return_statement ::= RETURN expression SEMICOLON",
 /*  79 */ "return_statement ::= RETURN SEMICOLON",
 /*  80 */ "declaration_statement ::= declaration_sequence SEMICOLON",
 /*  81 */ "declaration_statement ::= function_declaration",
 /*  82 */ "declaration_sequence ::= VAR variable_declaration",
 /*  83 */ "declaration_sequence ::= declaration_sequence COMMA variable_declaration",
 /*  84 */ "variable_declaration ::= simple_declaration",
 /*  85 */ "variable_declaration ::= init_declaration",
 /*  86 */ "simple_declaration ::= IDENTIFIER",
 /*  87 */ "init_declaration ::= IDENTIFIER ASSIGN expression",
 /*  88 */ "function_declaration ::= FUNCTION IDENTIFIER LPAREN parameter_list RPAREN function_body",
 /*  89 */ "function_declaration ::= FUNCTION IDENTIFIER LPAREN RPAREN function_body",
 /*  90 */ "parameter ::= IDENTIFIER",
 /*  91 */ "parameter_list ::= parameter",
 /*  92 */ "parameter_list ::= parameter_list COMMA parameter",
 /*  93 */ "function_body ::= statement",
 /*  94 */ "for_statement ::= FOR LPAREN for_init_statement expression SEMICOLON expression_opt RPAREN statement",
 /*  95 */ "for_init_statement ::= expression_statement",
 /*  96 */ "for_init_statement ::= declaration_sequence SEMICOLON",
 /*  97 */ "foreach_statement ::= FOR LPAREN id_expression IN expression RPAREN statement",
 /*  98 */ "if_statement ::= IF LPAREN expression RPAREN statement",
 /*  99 */ "if_statement ::= IF LPAREN expression RPAREN statement ELSE statement",
 /* 100 */ "while_statement ::= WHILE LPAREN expression RPAREN statement",
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
  { 54, 1 },
  { 56, 1 },
  { 56, 2 },
  { 55, 0 },
  { 55, 1 },
  { 57, 1 },
  { 57, 1 },
  { 57, 1 },
  { 57, 1 },
  { 57, 1 },
  { 57, 1 },
  { 57, 1 },
  { 57, 1 },
  { 57, 1 },
  { 67, 1 },
  { 69, 0 },
  { 69, 1 },
  { 68, 1 },
  { 68, 3 },
  { 72, 1 },
  { 72, 1 },
  { 72, 1 },
  { 72, 1 },
  { 72, 1 },
  { 72, 1 },
  { 70, 1 },
  { 70, 5 },
  { 71, 1 },
  { 71, 3 },
  { 71, 3 },
  { 71, 3 },
  { 71, 3 },
  { 71, 3 },
  { 71, 3 },
  { 71, 3 },
  { 71, 3 },
  { 71, 3 },
  { 71, 3 },
  { 71, 3 },
  { 71, 3 },
  { 71, 3 },
  { 71, 3 },
  { 71, 3 },
  { 71, 3 },
  { 73, 1 },
  { 73, 2 },
  { 73, 2 },
  { 73, 2 },
  { 74, 1 },
  { 74, 2 },
  { 74, 2 },
  { 74, 3 },
  { 74, 4 },
  { 74, 3 },
  { 74, 4 },
  { 75, 1 },
  { 75, 1 },
  { 75, 3 },
  { 75, 1 },
  { 78, 1 },
  { 78, 1 },
  { 78, 1 },
  { 78, 1 },
  { 78, 1 },
  { 78, 1 },
  { 76, 1 },
  { 79, 3 },
  { 80, 1 },
  { 80, 3 },
  { 81, 1 },
  { 82, 1 },
  { 77, 1 },
  { 77, 3 },
  { 59, 1 },
  { 59, 2 },
  { 62, 2 },
  { 62, 3 },
  { 58, 3 },
  { 66, 3 },
  { 66, 2 },
  { 60, 2 },
  { 60, 1 },
  { 83, 2 },
  { 83, 3 },
  { 85, 1 },
  { 85, 1 },
  { 86, 1 },
  { 87, 3 },
  { 84, 6 },
  { 84, 5 },
  { 90, 1 },
  { 88, 1 },
  { 88, 3 },
  { 89, 1 },
  { 61, 8 },
  { 91, 1 },
  { 91, 2 },
  { 65, 7 },
  { 63, 5 },
  { 63, 7 },
  { 64, 5 },
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
{ p->SetRoot(yymsp[0].minor.yy127); }
#line 1036 "astgen.c"
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
      case 16:
      case 17:
      case 25:
      case 27:
      case 44:
      case 48:
      case 55:
      case 56:
      case 58:
      case 71:
      case 81:
      case 82:
      case 84:
      case 85:
      case 91:
      case 93:
      case 95:
#line 74 "astgen.in"
{ yygotominor.yy127 = yymsp[0].minor.yy127; }
#line 1069 "astgen.c"
        break;
      case 2:
#line 75 "astgen.in"
{ 
  if(yymsp[-1].minor.yy127->m_type == statement_sequence) {
    yygotominor.yy127 = yymsp[-1].minor.yy127;
  }
  else {
    yygotominor.yy127 = new Ast(statement_sequence, new AstList);
    any_cast<AstList*>(yygotominor.yy127->m_a1)->push_back(yymsp[-1].minor.yy127);
  }
  any_cast<AstList*>(yygotominor.yy127->m_a1)->push_back(yymsp[0].minor.yy127);
}
#line 1083 "astgen.c"
        break;
      case 3:
      case 15:
#line 88 "astgen.in"
{ yygotominor.yy127 = 0; }
#line 1089 "astgen.c"
        break;
      case 18:
#line 116 "astgen.in"
{ yygotominor.yy127 = new Ast(assignment_expression, yymsp[-1].minor.yy174, yymsp[-2].minor.yy127, yymsp[0].minor.yy127); }
#line 1094 "astgen.c"
        break;
      case 19:
#line 120 "astgen.in"
{ yygotominor.yy174 = op_assign; }
#line 1099 "astgen.c"
        break;
      case 20:
#line 121 "astgen.in"
{ yygotominor.yy174 = op_assadd; }
#line 1104 "astgen.c"
        break;
      case 21:
#line 122 "astgen.in"
{ yygotominor.yy174 = op_asssub; }
#line 1109 "astgen.c"
        break;
      case 22:
#line 123 "astgen.in"
{ yygotominor.yy174 = op_assmul; }
#line 1114 "astgen.c"
        break;
      case 23:
#line 124 "astgen.in"
{ yygotominor.yy174 = op_assdiv; }
#line 1119 "astgen.c"
        break;
      case 24:
#line 125 "astgen.in"
{ yygotominor.yy174 = op_assmod; }
#line 1124 "astgen.c"
        break;
      case 26:
#line 129 "astgen.in"
{ yygotominor.yy127 = new Ast(ternary_expression, yymsp[-4].minor.yy127, yymsp[-2].minor.yy127, yymsp[0].minor.yy127); }
#line 1129 "astgen.c"
        break;
      case 28:
#line 133 "astgen.in"
{ yygotominor.yy127 = new Ast(binary_expression, op_logor,   yymsp[-2].minor.yy127, yymsp[0].minor.yy127); }
#line 1134 "astgen.c"
        break;
      case 29:
#line 134 "astgen.in"
{ yygotominor.yy127 = new Ast(binary_expression, op_logand,  yymsp[-2].minor.yy127, yymsp[0].minor.yy127); }
#line 1139 "astgen.c"
        break;
      case 30:
#line 135 "astgen.in"
{ yygotominor.yy127 = new Ast(binary_expression, op_bitor,   yymsp[-2].minor.yy127, yymsp[0].minor.yy127); }
#line 1144 "astgen.c"
        break;
      case 31:
#line 136 "astgen.in"
{ yygotominor.yy127 = new Ast(binary_expression, op_bitxor,  yymsp[-2].minor.yy127, yymsp[0].minor.yy127); }
#line 1149 "astgen.c"
        break;
      case 32:
#line 137 "astgen.in"
{ yygotominor.yy127 = new Ast(binary_expression, op_bitand,  yymsp[-2].minor.yy127, yymsp[0].minor.yy127); }
#line 1154 "astgen.c"
        break;
      case 33:
#line 138 "astgen.in"
{ yygotominor.yy127 = new Ast(binary_expression, op_eq,   yymsp[-2].minor.yy127, yymsp[0].minor.yy127); }
#line 1159 "astgen.c"
        break;
      case 34:
#line 139 "astgen.in"
{ yygotominor.yy127 = new Ast(binary_expression, op_ne,   yymsp[-2].minor.yy127, yymsp[0].minor.yy127); }
#line 1164 "astgen.c"
        break;
      case 35:
#line 140 "astgen.in"
{ yygotominor.yy127 = new Ast(binary_expression, op_lt,   yymsp[-2].minor.yy127, yymsp[0].minor.yy127); }
#line 1169 "astgen.c"
        break;
      case 36:
#line 141 "astgen.in"
{ yygotominor.yy127 = new Ast(binary_expression, op_le,   yymsp[-2].minor.yy127, yymsp[0].minor.yy127); }
#line 1174 "astgen.c"
        break;
      case 37:
#line 142 "astgen.in"
{ yygotominor.yy127 = new Ast(binary_expression, op_gt,   yymsp[-2].minor.yy127, yymsp[0].minor.yy127); }
#line 1179 "astgen.c"
        break;
      case 38:
#line 143 "astgen.in"
{ yygotominor.yy127 = new Ast(binary_expression, op_ge,   yymsp[-2].minor.yy127, yymsp[0].minor.yy127); }
#line 1184 "astgen.c"
        break;
      case 39:
#line 144 "astgen.in"
{ yygotominor.yy127 = new Ast(binary_expression, op_add,  yymsp[-2].minor.yy127, yymsp[0].minor.yy127); }
#line 1189 "astgen.c"
        break;
      case 40:
#line 145 "astgen.in"
{ yygotominor.yy127 = new Ast(binary_expression, op_sub,  yymsp[-2].minor.yy127, yymsp[0].minor.yy127); }
#line 1194 "astgen.c"
        break;
      case 41:
#line 146 "astgen.in"
{ yygotominor.yy127 = new Ast(binary_expression, op_mul,  yymsp[-2].minor.yy127, yymsp[0].minor.yy127); }
#line 1199 "astgen.c"
        break;
      case 42:
#line 147 "astgen.in"
{ yygotominor.yy127 = new Ast(binary_expression, op_div,  yymsp[-2].minor.yy127, yymsp[0].minor.yy127); }
#line 1204 "astgen.c"
        break;
      case 43:
#line 148 "astgen.in"
{ yygotominor.yy127 = new Ast(binary_expression, op_mod,  yymsp[-2].minor.yy127, yymsp[0].minor.yy127); }
#line 1209 "astgen.c"
        break;
      case 45:
#line 152 "astgen.in"
{ yygotominor.yy127 = new Ast(prefix_expression, op_negate, yymsp[0].minor.yy127); }
#line 1214 "astgen.c"
        break;
      case 46:
#line 153 "astgen.in"
{ yygotominor.yy127 = new Ast(prefix_expression, op_preinc, yymsp[0].minor.yy127); }
#line 1219 "astgen.c"
        break;
      case 47:
#line 154 "astgen.in"
{ yygotominor.yy127 = new Ast(prefix_expression, op_predec, yymsp[0].minor.yy127); }
#line 1224 "astgen.c"
        break;
      case 49:
#line 158 "astgen.in"
{ yygotominor.yy127 = new Ast(postfix_expression, op_postinc, yymsp[-1].minor.yy127); }
#line 1229 "astgen.c"
        break;
      case 50:
#line 159 "astgen.in"
{ yygotominor.yy127 = new Ast(postfix_expression, op_postdec, yymsp[-1].minor.yy127); }
#line 1234 "astgen.c"
        break;
      case 51:
#line 160 "astgen.in"
{ yygotominor.yy127 = new Ast(member_expression, yymsp[-2].minor.yy127, yymsp[0].minor.yy127); }
#line 1239 "astgen.c"
        break;
      case 52:
#line 161 "astgen.in"
{ yygotominor.yy127 = new Ast(index_expression, yymsp[-3].minor.yy127, yymsp[-1].minor.yy127); }
#line 1244 "astgen.c"
        break;
      case 53:
#line 162 "astgen.in"
{ yygotominor.yy127 = new Ast(function_call, String(yymsp[-2].minor.yy0)); }
#line 1249 "astgen.c"
        break;
      case 54:
#line 163 "astgen.in"
{ yygotominor.yy127 = new Ast(function_call, String(yymsp[-3].minor.yy0), yymsp[-1].minor.yy127); }
#line 1254 "astgen.c"
        break;
      case 57:
      case 80:
      case 96:
#line 168 "astgen.in"
{ yygotominor.yy127 = yymsp[-1].minor.yy127; }
#line 1261 "astgen.c"
        break;
      case 59:
#line 172 "astgen.in"
{ yygotominor.yy127 = new Ast(literal, Variant(String(yymsp[0].minor.yy0), Variant::stInt)); }
#line 1266 "astgen.c"
        break;
      case 60:
#line 173 "astgen.in"
{ yygotominor.yy127 = new Ast(literal, Variant(String(yymsp[0].minor.yy0), Variant::stReal)); }
#line 1271 "astgen.c"
        break;
      case 61:
#line 174 "astgen.in"
{ yygotominor.yy127 = new Ast(literal, Variant(String(yymsp[0].minor.yy0), Variant::stString)); }
#line 1276 "astgen.c"
        break;
      case 62:
#line 175 "astgen.in"
{ yygotominor.yy127 = new Ast(literal, Variant(true));    }
#line 1281 "astgen.c"
        break;
      case 63:
#line 176 "astgen.in"
{ yygotominor.yy127 = new Ast(literal, Variant(false));   }
#line 1286 "astgen.c"
        break;
      case 64:
#line 177 "astgen.in"
{ yygotominor.yy127 = new Ast(literal, Variant());        }
#line 1291 "astgen.c"
        break;
      case 65:
#line 180 "astgen.in"
{ yygotominor.yy127 = new Ast(lvalue, String(yymsp[0].minor.yy0)); }
#line 1296 "astgen.c"
        break;
      case 66:
#line 183 "astgen.in"
{ yygotominor.yy127 = new Ast(list_literal, yymsp[-1].minor.yy127); }
#line 1301 "astgen.c"
        break;
      case 67:
#line 185 "astgen.in"
{ yygotominor.yy127 = new Ast(list_content, yymsp[0].minor.yy127); }
#line 1306 "astgen.c"
        break;
      case 68:
#line 186 "astgen.in"
{ yygotominor.yy127 = new Ast(list_content, yymsp[-2].minor.yy127, yymsp[0].minor.yy127); }
#line 1311 "astgen.c"
        break;
      case 69:
#line 188 "astgen.in"
{ yygotominor.yy127 = new Ast(list_entry, yymsp[0].minor.yy127); }
#line 1316 "astgen.c"
        break;
      case 70:
#line 197 "astgen.in"
{ yygotominor.yy127 = new Ast(argument, yymsp[0].minor.yy127); }
#line 1321 "astgen.c"
        break;
      case 72:
#line 201 "astgen.in"
{ yygotominor.yy127 = new Ast(argument_list, yymsp[-2].minor.yy127, yymsp[0].minor.yy127); }
#line 1326 "astgen.c"
        break;
      case 73:
#line 209 "astgen.in"
{ yygotominor.yy127 = new Ast(empty_statement); }
#line 1331 "astgen.c"
        break;
      case 74:
#line 210 "astgen.in"
{ yygotominor.yy127 = new Ast(expression_statement, yymsp[-1].minor.yy127); }
#line 1336 "astgen.c"
        break;
      case 75:
#line 213 "astgen.in"
{ yygotominor.yy127 = new Ast(compound_statement); }
#line 1341 "astgen.c"
        break;
      case 76:
#line 214 "astgen.in"
{ yygotominor.yy127 = new Ast(compound_statement, yymsp[-1].minor.yy127); }
#line 1346 "astgen.c"
        break;
      case 77:
#line 217 "astgen.in"
{ p->Parse(yymsp[-1].minor.yy0); yygotominor.yy127 = p->GetRoot(); }
#line 1351 "astgen.c"
        break;
      case 78:
#line 220 "astgen.in"
{ yygotominor.yy127 = new Ast(return_statement, yymsp[-1].minor.yy127); }
#line 1356 "astgen.c"
        break;
      case 79:
#line 221 "astgen.in"
{ yygotominor.yy127 = new Ast(return_statement);    }
#line 1361 "astgen.c"
        break;
      case 83:
#line 234 "astgen.in"
{ yygotominor.yy127 = new Ast(declaration_sequence, yymsp[-2].minor.yy127, yymsp[0].minor.yy127); }
#line 1366 "astgen.c"
        break;
      case 86:
#line 241 "astgen.in"
{ yygotominor.yy127 = new Ast(variable_declaration, String(yymsp[0].minor.yy0)); }
#line 1371 "astgen.c"
        break;
      case 87:
#line 244 "astgen.in"
{ yygotominor.yy127 = new Ast(variable_declaration, String(yymsp[-2].minor.yy0), yymsp[0].minor.yy127); }
#line 1376 "astgen.c"
        break;
      case 88:
#line 247 "astgen.in"
{ yygotominor.yy127 = new Ast(function_declaration, String(yymsp[-4].minor.yy0), yymsp[-2].minor.yy127, yymsp[0].minor.yy127); }
#line 1381 "astgen.c"
        break;
      case 89:
#line 248 "astgen.in"
{ yygotominor.yy127 = new Ast(function_declaration, String(yymsp[-3].minor.yy0), any(), yymsp[0].minor.yy127); }
#line 1386 "astgen.c"
        break;
      case 90:
#line 251 "astgen.in"
{ yygotominor.yy127 = new Ast(parameter, String(yymsp[0].minor.yy0)); }
#line 1391 "astgen.c"
        break;
      case 92:
#line 255 "astgen.in"
{ yygotominor.yy127 = new Ast(parameter_list, yymsp[-2].minor.yy127, yymsp[0].minor.yy127); }
#line 1396 "astgen.c"
        break;
      case 94:
#line 267 "astgen.in"
{ yygotominor.yy127 = new Ast(for_statement, yymsp[-5].minor.yy127, yymsp[-4].minor.yy127, yymsp[-2].minor.yy127, yymsp[0].minor.yy127); }
#line 1401 "astgen.c"
        break;
      case 97:
#line 278 "astgen.in"
{ yygotominor.yy127 = new Ast(foreach_statement, yymsp[-4].minor.yy127, yymsp[-2].minor.yy127, yymsp[0].minor.yy127); }
#line 1406 "astgen.c"
        break;
      case 98:
#line 289 "astgen.in"
{ yygotominor.yy127 = new Ast(if_statement, yymsp[-2].minor.yy127, yymsp[0].minor.yy127); }
#line 1411 "astgen.c"
        break;
      case 99:
#line 290 "astgen.in"
{ yygotominor.yy127 = new Ast(if_statement, yymsp[-4].minor.yy127, yymsp[-2].minor.yy127, yymsp[0].minor.yy127); }
#line 1416 "astgen.c"
        break;
      case 100:
#line 298 "astgen.in"
{ yygotominor.yy127 = new Ast(while_statement, yymsp[-2].minor.yy127,  yymsp[0].minor.yy127); }
#line 1421 "astgen.c"
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
#line 1469 "astgen.c"
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
#line 1487 "astgen.c"
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

