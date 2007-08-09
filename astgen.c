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
#define YYNOCODE 113
#define YYACTIONTYPE unsigned short int
#define CScriptParseTOKENTYPE  Token 
typedef union {
  CScriptParseTOKENTYPE yy0;
  opcodes yy110;
  Ast* yy151;
  AstList* yy183;
  int yy225;
} YYMINORTYPE;
#ifndef YYSTACKDEPTH
#define YYSTACKDEPTH 100
#endif
#define CScriptParseARG_SDECL  Parser* p ;
#define CScriptParseARG_PDECL , Parser* p 
#define CScriptParseARG_FETCH  Parser* p  = yypParser->p 
#define CScriptParseARG_STORE yypParser->p  = p 
#define YYNSTATE 218
#define YYNRULE 126
#define YYERRORSYMBOL 59
#define YYERRSYMDT yy225
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
 /*     0 */   189,  345,  216,  214,   10,  213,  209,  208,  205,  204,
 /*    10 */   202,  199,  198,  196,  195,  194,  192,  190,  112,  187,
 /*    20 */   122,  186,   58,  147,  184,   75,  172,  167,   55,  168,
 /*    30 */   165,   54,  146,  189,   79,  137,  138,    9,  213,  209,
 /*    40 */   208,  205,  204,  202,  199,  198,  196,  195,  194,  192,
 /*    50 */   190,  112,  187,   53,  186,   58,   21,  184,   75,  172,
 /*    60 */   167,  121,  168,  165,   34,   41,   48,   79,  137,  138,
 /*    70 */    54,  193,  197,  187,   88,  186,   58,   19,  184,   75,
 /*    80 */   172,  167,  189,  168,  165,  191,    9,  213,  209,  208,
 /*    90 */   205,  204,  202,  199,  198,  196,  195,  194,  192,  190,
 /*   100 */   112,  187,  125,  186,   58,   56,  184,   75,  172,  167,
 /*   110 */   127,  168,  165,  140,  132,  133,   79,  137,  138,   45,
 /*   120 */    43,   42,   40,   39,   38,   44,   35,   47,   37,   36,
 /*   130 */    33,   34,   41,   48,  188,   98,   64,    4,  184,   75,
 /*   140 */   172,  167,  189,  168,  165,  118,    6,  213,  209,  208,
 /*   150 */   205,  204,  202,  199,  198,  196,  195,  194,  192,  190,
 /*   160 */   112,  187,   61,  186,   58,   57,  184,   75,  172,  167,
 /*   170 */    24,  168,  165,  131,  132,  133,   79,  137,  138,  153,
 /*   180 */   187,  119,  186,   58,    2,  184,   75,  172,  167,  215,
 /*   190 */   168,  165,   82,  152,  189,  180,  179,   60,   29,  155,
 /*   200 */   209,  208,  205,  204,  202,  199,  198,  196,  195,  194,
 /*   210 */   192,  190,  112,  187,   14,  186,   58,  157,  184,   75,
 /*   220 */   172,  167,  115,  168,  165,  182,  183,   18,   79,  137,
 /*   230 */   138,   36,   33,   34,   41,   48,  189,  154,   54,  129,
 /*   240 */   178,  155,  209,  208,  205,  204,  202,  199,  198,  196,
 /*   250 */   195,  194,  192,  190,  112,  187,    5,  186,   58,  177,
 /*   260 */   184,   75,  172,  167,    8,  168,  165,  170,   31,   78,
 /*   270 */    79,  137,  138,   52,   17,  189,  148,   85,  145,  203,
 /*   280 */   200,  209,  208,  205,  204,  202,  199,  198,  196,  195,
 /*   290 */   194,  192,  190,  112,  187,  102,  186,   58,   98,  184,
 /*   300 */    75,  172,  167,   63,  168,  165,  107,  189,   23,   79,
 /*   310 */   137,  138,  162,  209,  208,  205,  204,  202,  199,  198,
 /*   320 */   196,  195,  194,  192,  190,  112,  187,   99,  186,   58,
 /*   330 */    62,  184,   75,  172,  167,   13,  168,  165,   28,  189,
 /*   340 */    27,   79,  137,  138,  212,  209,  208,  205,  204,  202,
 /*   350 */   199,  198,  196,  195,  194,  192,  190,  112,  187,  166,
 /*   360 */   186,   58,   96,  184,   75,  172,  167,   32,  168,  165,
 /*   370 */     1,  189,   11,   79,  137,  138,  174,  209,  208,  205,
 /*   380 */   204,  202,  199,  198,  196,  195,  194,  192,  190,  112,
 /*   390 */   187,   26,  186,   58,   15,  184,   75,  172,  167,    7,
 /*   400 */   168,  165,   12,  189,  346,   79,  137,  138,  169,  209,
 /*   410 */   208,  205,  204,  202,  199,  198,  196,  195,  194,  192,
 /*   420 */   190,  112,  187,  346,  186,   58,  346,  184,   75,  172,
 /*   430 */   167,  346,  168,  165,  346,  189,  346,   79,  137,  138,
 /*   440 */    84,  209,  208,  205,  204,  202,  199,  198,  196,  195,
 /*   450 */   194,  192,  190,  112,  187,  346,  186,   58,  181,  184,
 /*   460 */    75,  172,  167,   50,  168,  165,  346,  346,  346,   79,
 /*   470 */   137,  138,  346,   59,   93,   49,   51,  346,   16,  346,
 /*   480 */    89,   30,  346,  164,  163,  161,  160,  159,  158,  346,
 /*   490 */   124,    3,  217,  117,   20,  111,  110,   55,   94,  101,
 /*   500 */   106,   50,  346,  346,   92,   83,   95,  104,  346,  207,
 /*   510 */   211,  114,  109,   49,   51,  346,   16,  346,   89,   30,
 /*   520 */   346,  164,  163,  161,  160,  159,  158,  346,  124,    3,
 /*   530 */   120,  117,   20,  111,  110,   55,   94,  101,  106,   50,
 /*   540 */   346,  346,   92,   83,   95,  210,  211,  346,  105,  346,
 /*   550 */   346,   49,   51,  346,   16,  346,   89,   30,  346,  164,
 /*   560 */   163,  161,  160,  159,  158,  346,  124,    3,  346,  117,
 /*   570 */    20,  111,  110,   55,   94,  101,  106,  346,  346,  346,
 /*   580 */    92,   83,   95,   46,   45,   43,   42,   40,   39,   38,
 /*   590 */    44,   35,   47,   37,   36,   33,   34,   41,   48,  144,
 /*   600 */   143,  142,  141,  139,  135,   25,   43,   42,   40,   39,
 /*   610 */    38,   44,   35,   47,   37,   36,   33,   34,   41,   48,
 /*   620 */    50,   39,   38,   44,   35,   47,   37,   36,   33,   34,
 /*   630 */    41,   48,   49,   51,  346,   16,  346,   89,   30,  346,
 /*   640 */   164,  163,  161,  160,  159,  158,  346,  124,  128,  346,
 /*   650 */   184,   75,  172,  167,   55,  168,  165,  346,  346,   42,
 /*   660 */    40,   39,   38,   44,   35,   47,   37,   36,   33,   34,
 /*   670 */    41,   48,  346,   50,   44,   35,   47,   37,   36,   33,
 /*   680 */    34,   41,   48,  346,  346,   49,   51,  346,   16,  346,
 /*   690 */    89,   30,  346,  164,  163,  161,  160,  159,  158,   50,
 /*   700 */   123,   73,  346,  184,   75,  172,  167,  346,  168,  165,
 /*   710 */   346,   49,   51,  346,   16,  346,   89,   30,  171,  164,
 /*   720 */   163,  161,  160,  159,  158,  151,  187,  201,  186,   58,
 /*   730 */   346,  184,   75,  172,  167,   81,  168,  165,  112,  187,
 /*   740 */   149,  186,   58,  346,  184,   75,  172,  108,  346,  168,
 /*   750 */   165,  346,  346,  346,   80,   40,   39,   38,   44,   35,
 /*   760 */    47,   37,   36,   33,   34,   41,   48,  346,   22,   50,
 /*   770 */    67,  346,  184,   75,  172,  167,  346,  168,  165,  346,
 /*   780 */   346,   49,   51,  346,   16,  346,   89,   30,  346,  164,
 /*   790 */   163,  161,  160,  159,  158,  153,  187,  346,  186,   58,
 /*   800 */   346,  184,   75,  172,  167,  346,  168,  165,  346,  156,
 /*   810 */   151,  187,  346,  186,   58,  346,  184,   75,  172,  167,
 /*   820 */   346,  168,  165,   91,  187,  150,  186,   58,  346,  184,
 /*   830 */    75,  172,  167,  346,  168,  165,  116,  187,  346,  186,
 /*   840 */    58,  346,  184,   75,  172,  167,  346,  168,  165,  346,
 /*   850 */    66,  346,  184,   75,  172,  167,  346,  168,  165,  113,
 /*   860 */   187,  346,  186,   58,  346,  184,   75,  172,  167,  346,
 /*   870 */   168,  165,  103,  187,  346,  186,   58,  346,  184,   75,
 /*   880 */   172,  167,  346,  168,  165,   87,  187,  346,  186,   58,
 /*   890 */   346,  184,   75,  172,  167,  346,  168,  165,  346,   90,
 /*   900 */   187,  346,  186,   58,  346,  184,   75,  172,  167,  346,
 /*   910 */   168,  165,   97,  187,  346,  186,   58,  346,  184,   75,
 /*   920 */   172,  167,  346,  168,  165,  206,  187,  346,  186,   58,
 /*   930 */   346,  184,   75,  172,  167,  346,  168,  165,   86,  187,
 /*   940 */   346,  186,   58,  346,  184,   75,  172,  167,  346,  168,
 /*   950 */   165,  136,  187,  346,  186,   58,  346,  184,   75,  172,
 /*   960 */   167,  346,  168,  165,  100,  187,  346,  186,   58,  346,
 /*   970 */   184,   75,  172,  167,  346,  168,  165,  346,  134,  346,
 /*   980 */   186,   58,  346,  184,   75,  172,  167,  346,  168,  165,
 /*   990 */   185,  346,  186,   58,  346,  184,   75,  172,  167,  346,
 /*  1000 */   168,  165,   68,  346,  184,   75,  172,  167,  346,  168,
 /*  1010 */   165,   76,  346,  184,   75,  172,  167,  346,  168,  165,
 /*  1020 */    69,  346,  184,   75,  172,  167,  346,  168,  165,  130,
 /*  1030 */   346,  184,   75,  172,  167,  346,  168,  165,  346,   70,
 /*  1040 */   346,  184,   75,  172,  167,  346,  168,  165,  346,   74,
 /*  1050 */   346,  184,   75,  172,  167,  346,  168,  165,   77,  346,
 /*  1060 */   184,   75,  172,  167,  346,  168,  165,  126,  346,  184,
 /*  1070 */    75,  172,  167,  346,  168,  165,  346,   72,  346,  184,
 /*  1080 */    75,  172,  167,  346,  168,  165,   71,  346,  184,   75,
 /*  1090 */   172,  167,  346,  168,  165,   65,  346,  184,   75,  172,
 /*  1100 */   167,  346,  168,  165,  175,   75,  172,  167,  346,  168,
 /*  1110 */   165,  173,   75,  172,  167,  346,  168,  165,  176,   75,
 /*  1120 */   172,  167,  346,  168,  165,  164,  163,  161,  160,  159,
 /*  1130 */   158,
};
static const YYCODETYPE yy_lookahead[] = {
 /*     0 */    59,   60,   61,   62,   63,   64,   65,   66,   67,   68,
 /*    10 */    69,   70,   71,   72,   73,   74,   75,   76,   77,   78,
 /*    20 */    40,   80,   81,   42,   83,   84,   85,   86,   47,   88,
 /*    30 */    89,   39,   40,   59,   93,   94,   95,   63,   64,   65,
 /*    40 */    66,   67,   68,   69,   70,   71,   72,   73,   74,   75,
 /*    50 */    76,   77,   78,   39,   80,   81,   17,   83,   84,   85,
 /*    60 */    86,   40,   88,   89,   14,   15,   16,   93,   94,   95,
 /*    70 */    39,   40,   77,   78,   79,   80,   81,   40,   83,   84,
 /*    80 */    85,   86,   59,   88,   89,  111,   63,   64,   65,   66,
 /*    90 */    67,   68,   69,   70,   71,   72,   73,   74,   75,   76,
 /*   100 */    77,   78,   40,   80,   81,  108,   83,   84,   85,   86,
 /*   110 */    40,   88,   89,   96,   97,   98,   93,   94,   95,    2,
 /*   120 */     3,    4,    5,    6,    7,    8,    9,   10,   11,   12,
 /*   130 */    13,   14,   15,   16,  111,   30,   81,   32,   83,   84,
 /*   140 */    85,   86,   59,   88,   89,   35,   63,   64,   65,   66,
 /*   150 */    67,   68,   69,   70,   71,   72,   73,   74,   75,   76,
 /*   160 */    77,   78,   39,   80,   81,   99,   83,   84,   85,   86,
 /*   170 */    51,   88,   89,   96,   97,   98,   93,   94,   95,   77,
 /*   180 */    78,   40,   80,   81,   24,   83,   84,   85,   86,  104,
 /*   190 */    88,   89,   90,   91,   59,   25,   26,   27,   28,   64,
 /*   200 */    65,   66,   67,   68,   69,   70,   71,   72,   73,   74,
 /*   210 */    75,   76,   77,   78,   31,   80,   81,   29,   83,   84,
 /*   220 */    85,   86,   30,   88,   89,  109,  110,   39,   93,   94,
 /*   230 */    95,   12,   13,   14,   15,   16,   59,  102,   39,   40,
 /*   240 */    86,   64,   65,   66,   67,   68,   69,   70,   71,   72,
 /*   250 */    73,   74,   75,   76,   77,   78,   32,   80,   81,   30,
 /*   260 */    83,   84,   85,   86,   32,   88,   89,   32,   82,   93,
 /*   270 */    93,   94,   95,   31,   39,   59,  100,   30,   29,  102,
 /*   280 */    64,   65,   66,   67,   68,   69,   70,   71,   72,   73,
 /*   290 */    74,   75,   76,   77,   78,   30,   80,   81,   30,   83,
 /*   300 */    84,   85,   86,   41,   88,   89,   88,   59,   31,   93,
 /*   310 */    94,   95,   64,   65,   66,   67,   68,   69,   70,   71,
 /*   320 */    72,   73,   74,   75,   76,   77,   78,   32,   80,   81,
 /*   330 */    41,   83,   84,   85,   86,   53,   88,   89,   17,   59,
 /*   340 */    31,   93,   94,   95,   64,   65,   66,   67,   68,   69,
 /*   350 */    70,   71,   72,   73,   74,   75,   76,   77,   78,   32,
 /*   360 */    80,   81,   30,   83,   84,   85,   86,   24,   88,   89,
 /*   370 */    24,   59,   32,   93,   94,   95,   64,   65,   66,   67,
 /*   380 */    68,   69,   70,   71,   72,   73,   74,   75,   76,   77,
 /*   390 */    78,   31,   80,   81,   31,   83,   84,   85,   86,   32,
 /*   400 */    88,   89,   32,   59,  112,   93,   94,   95,   64,   65,
 /*   410 */    66,   67,   68,   69,   70,   71,   72,   73,   74,   75,
 /*   420 */    76,   77,   78,  112,   80,   81,  112,   83,   84,   85,
 /*   430 */    86,  112,   88,   89,  112,   59,  112,   93,   94,   95,
 /*   440 */    64,   65,   66,   67,   68,   69,   70,   71,   72,   73,
 /*   450 */    74,   75,   76,   77,   78,  112,   80,   81,   42,   83,
 /*   460 */    84,   85,   86,   13,   88,   89,  112,  112,  112,   93,
 /*   470 */    94,   95,  112,   57,   58,   25,   26,  112,   28,  112,
 /*   480 */    30,   31,  112,   33,   34,   35,   36,   37,   38,  112,
 /*   490 */    40,   41,   42,   43,   44,   45,   46,   47,   48,   49,
 /*   500 */    50,   13,  112,  112,   54,   55,   56,  101,  112,  103,
 /*   510 */   104,  105,  106,   25,   26,  112,   28,  112,   30,   31,
 /*   520 */   112,   33,   34,   35,   36,   37,   38,  112,   40,   41,
 /*   530 */    42,   43,   44,   45,   46,   47,   48,   49,   50,   13,
 /*   540 */   112,  112,   54,   55,   56,  103,  104,  112,  106,  112,
 /*   550 */   112,   25,   26,  112,   28,  112,   30,   31,  112,   33,
 /*   560 */    34,   35,   36,   37,   38,  112,   40,   41,  112,   43,
 /*   570 */    44,   45,   46,   47,   48,   49,   50,  112,  112,  112,
 /*   580 */    54,   55,   56,    1,    2,    3,    4,    5,    6,    7,
 /*   590 */     8,    9,   10,   11,   12,   13,   14,   15,   16,   17,
 /*   600 */    18,   19,   20,   21,   22,   23,    3,    4,    5,    6,
 /*   610 */     7,    8,    9,   10,   11,   12,   13,   14,   15,   16,
 /*   620 */    13,    6,    7,    8,    9,   10,   11,   12,   13,   14,
 /*   630 */    15,   16,   25,   26,  112,   28,  112,   30,   31,  112,
 /*   640 */    33,   34,   35,   36,   37,   38,  112,   40,   81,  112,
 /*   650 */    83,   84,   85,   86,   47,   88,   89,  112,  112,    4,
 /*   660 */     5,    6,    7,    8,    9,   10,   11,   12,   13,   14,
 /*   670 */    15,   16,  112,   13,    8,    9,   10,   11,   12,   13,
 /*   680 */    14,   15,   16,  112,  112,   25,   26,  112,   28,  112,
 /*   690 */    30,   31,  112,   33,   34,   35,   36,   37,   38,   13,
 /*   700 */    40,   81,  112,   83,   84,   85,   86,  112,   88,   89,
 /*   710 */   112,   25,   26,  112,   28,  112,   30,   31,   32,   33,
 /*   720 */    34,   35,   36,   37,   38,   77,   78,   66,   80,   81,
 /*   730 */   112,   83,   84,   85,   86,   87,   88,   89,   77,   78,
 /*   740 */    92,   80,   81,  112,   83,   84,   85,   86,  112,   88,
 /*   750 */    89,  112,  112,  112,   93,    5,    6,    7,    8,    9,
 /*   760 */    10,   11,   12,   13,   14,   15,   16,  112,  107,   13,
 /*   770 */    81,  112,   83,   84,   85,   86,  112,   88,   89,  112,
 /*   780 */   112,   25,   26,  112,   28,  112,   30,   31,  112,   33,
 /*   790 */    34,   35,   36,   37,   38,   77,   78,  112,   80,   81,
 /*   800 */   112,   83,   84,   85,   86,  112,   88,   89,  112,   91,
 /*   810 */    77,   78,  112,   80,   81,  112,   83,   84,   85,   86,
 /*   820 */   112,   88,   89,   77,   78,   92,   80,   81,  112,   83,
 /*   830 */    84,   85,   86,  112,   88,   89,   77,   78,  112,   80,
 /*   840 */    81,  112,   83,   84,   85,   86,  112,   88,   89,  112,
 /*   850 */    81,  112,   83,   84,   85,   86,  112,   88,   89,   77,
 /*   860 */    78,  112,   80,   81,  112,   83,   84,   85,   86,  112,
 /*   870 */    88,   89,   77,   78,  112,   80,   81,  112,   83,   84,
 /*   880 */    85,   86,  112,   88,   89,   77,   78,  112,   80,   81,
 /*   890 */   112,   83,   84,   85,   86,  112,   88,   89,  112,   77,
 /*   900 */    78,  112,   80,   81,  112,   83,   84,   85,   86,  112,
 /*   910 */    88,   89,   77,   78,  112,   80,   81,  112,   83,   84,
 /*   920 */    85,   86,  112,   88,   89,   77,   78,  112,   80,   81,
 /*   930 */   112,   83,   84,   85,   86,  112,   88,   89,   77,   78,
 /*   940 */   112,   80,   81,  112,   83,   84,   85,   86,  112,   88,
 /*   950 */    89,   77,   78,  112,   80,   81,  112,   83,   84,   85,
 /*   960 */    86,  112,   88,   89,   77,   78,  112,   80,   81,  112,
 /*   970 */    83,   84,   85,   86,  112,   88,   89,  112,   78,  112,
 /*   980 */    80,   81,  112,   83,   84,   85,   86,  112,   88,   89,
 /*   990 */    78,  112,   80,   81,  112,   83,   84,   85,   86,  112,
 /*  1000 */    88,   89,   81,  112,   83,   84,   85,   86,  112,   88,
 /*  1010 */    89,   81,  112,   83,   84,   85,   86,  112,   88,   89,
 /*  1020 */    81,  112,   83,   84,   85,   86,  112,   88,   89,   81,
 /*  1030 */   112,   83,   84,   85,   86,  112,   88,   89,  112,   81,
 /*  1040 */   112,   83,   84,   85,   86,  112,   88,   89,  112,   81,
 /*  1050 */   112,   83,   84,   85,   86,  112,   88,   89,   81,  112,
 /*  1060 */    83,   84,   85,   86,  112,   88,   89,   81,  112,   83,
 /*  1070 */    84,   85,   86,  112,   88,   89,  112,   81,  112,   83,
 /*  1080 */    84,   85,   86,  112,   88,   89,   81,  112,   83,   84,
 /*  1090 */    85,   86,  112,   88,   89,   81,  112,   83,   84,   85,
 /*  1100 */    86,  112,   88,   89,   83,   84,   85,   86,  112,   88,
 /*  1110 */    89,   83,   84,   85,   86,  112,   88,   89,   83,   84,
 /*  1120 */    85,   86,  112,   88,   89,   33,   34,   35,   36,   37,
 /*  1130 */    38,
};
#define YY_SHIFT_USE_DFLT (-21)
#define YY_SHIFT_MAX 118
static const short yy_shift_ofst[] = {
 /*     0 */   526,  526,  526,  450,  526,  526,  488,  526,  526,  526,
 /*    10 */   526,  526,  526,  526,  607,  686,  756,  756,  756,  756,
 /*    20 */   660,  756,  756,  756,  756,  756,  756,  756,  756,  756,
 /*    30 */   756,  756,  756,  756,  756,  756,  756,  756,  756,  756,
 /*    40 */   756,  756,  756,  756,  756,  756,  756,  756,  756,  756,
 /*    50 */   756,  756,  105,  268,  247,  247,  416,  -19,  582, 1092,
 /*    60 */   229,  192,  -21,  -21,  117,  603,  655,  750,  615,  666,
 /*    70 */   666,  219,  219,  219,  219,  170,   50,   50,   -8,  199,
 /*    80 */    31,  235,  188,  277,  282,  321,  327,  343,  340,  363,
 /*    90 */   370,  367,  360,  346,  332,  309,  289,  295,   39,  262,
 /*   100 */   249,  265,  242,  232,  224,  123,  183,  160,  119,  123,
 /*   110 */    70,   62,   21,  -20,   14,   39,   37,  110,  141,
};
#define YY_REDUCE_USE_DFLT (-60)
#define YY_REDUCE_MAX 63
static const short yy_reduce_ofst[] = {
 /*     0 */   -59,  -26,   23,   83,  177,  135,  280,  312,  248,  280,
 /*    10 */   280,  216,  376,  344,  661,  648,  102,  733,  718,   -5,
 /*    20 */   782,  848,  759,  746,  795,  808,  822,  835,  874,  887,
 /*    30 */   861,  912,  900,  930,  948,  968,  977,  996,  958,  939,
 /*    40 */   921,  567,  689,  769, 1005, 1014,   55,  620,  986, 1021,
 /*    50 */  1035, 1028,  406,  442,   77,   17,  116,  176,  186,  218,
 /*    60 */   154,   85,   66,   -3,
};
static const YYACTIONTYPE yy_default[] = {
 /*     0 */   222,  344,  344,  344,  344,  344,  344,  344,  344,  343,
 /*    10 */   223,  344,  344,  344,  344,  344,  344,  344,  344,  238,
 /*    20 */   344,  344,  344,  344,  344,  344,  344,  344,  344,  344,
 /*    30 */   344,  344,  344,  344,  344,  344,  344,  344,  344,  344,
 /*    40 */   344,  344,  344,  344,  344,  344,  344,  344,  344,  344,
 /*    50 */   344,  344,  344,  344,  344,  344,  344,  344,  248,  344,
 /*    60 */   344,  344,  316,  338,  251,  252,  253,  254,  255,  256,
 /*    70 */   257,  258,  261,  260,  259,  267,  263,  262,  344,  344,
 /*    80 */   344,  344,  344,  344,  334,  312,  344,  344,  344,  288,
 /*    90 */   344,  344,  344,  344,  344,  344,  344,  344,  320,  344,
 /*   100 */   344,  344,  344,  344,  344,  328,  344,  344,  279,  327,
 /*   110 */   344,  344,  344,  344,  326,  344,  344,  344,  344,  300,
 /*   120 */   299,  297,  301,  302,  296,  303,  266,  304,  265,  305,
 /*   130 */   264,  309,  310,  311,  249,  247,  313,  306,  307,  246,
 /*   140 */   308,  245,  244,  243,  242,  275,  315,  314,  317,  294,
 /*   150 */   295,  293,  290,  292,  318,  329,  291,  289,  287,  286,
 /*   160 */   285,  284,  333,  283,  282,  281,  280,  279,  278,  335,
 /*   170 */   277,  276,  271,  270,  336,  269,  268,  288,  274,  273,
 /*   180 */   272,  337,  339,  340,  250,  241,  240,  237,  341,  236,
 /*   190 */   235,  342,  234,  332,  233,  232,  231,  239,  230,  229,
 /*   200 */   330,  331,  228,  319,  227,  226,  321,  322,  225,  224,
 /*   210 */   323,  324,  221,  220,  219,  325,  218,  298,
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
  "argument",      "declaration_sequence",  "struct_declaration",  "function_declaration",
  "variable_declaration",  "simple_declaration",  "init_declaration",  "struct_members",
  "struct_declseq",  "parameter_list",  "function_body",  "parameter",   
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
 /*  87 */ "declaration_statement ::= declaration_sequence SEMICOLON",
 /*  88 */ "declaration_statement ::= struct_declaration",
 /*  89 */ "declaration_statement ::= function_declaration",
 /*  90 */ "declaration_sequence ::= VAR variable_declaration",
 /*  91 */ "declaration_sequence ::= declaration_sequence COMMA variable_declaration",
 /*  92 */ "variable_declaration ::= simple_declaration",
 /*  93 */ "variable_declaration ::= init_declaration",
 /*  94 */ "simple_declaration ::= IDENTIFIER",
 /*  95 */ "init_declaration ::= IDENTIFIER ASSIGN expression",
 /*  96 */ "struct_declaration ::= STRUCT IDENTIFIER LBRACE struct_members RBRACE",
 /*  97 */ "struct_declseq ::= declaration_sequence SEMICOLON",
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
 /* 114 */ "for_init_statement ::= declaration_sequence SEMICOLON",
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
  { 67, 2 },
  { 67, 1 },
  { 67, 1 },
  { 93, 2 },
  { 93, 3 },
  { 96, 1 },
  { 96, 1 },
  { 97, 1 },
  { 98, 3 },
  { 94, 5 },
  { 100, 2 },
  { 99, 0 },
  { 99, 2 },
  { 95, 6 },
  { 95, 5 },
  { 103, 1 },
  { 104, 3 },
  { 105, 1 },
  { 105, 3 },
  { 106, 1 },
  { 106, 3 },
  { 101, 1 },
  { 101, 1 },
  { 101, 3 },
  { 102, 1 },
  { 68, 8 },
  { 107, 1 },
  { 107, 2 },
  { 72, 7 },
  { 70, 5 },
  { 70, 7 },
  { 71, 5 },
  { 74, 7 },
  { 108, 0 },
  { 108, 2 },
  { 108, 2 },
  { 109, 4 },
  { 110, 3 },
  { 111, 1 },
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
{ p->SetRoot(yymsp[0].minor.yy151); }
#line 1132 "astgen.c"
        break;
      case 1:
#line 75 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(translation_unit, yymsp[0].minor.yy151); }
#line 1137 "astgen.c"
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
      case 88:
      case 89:
      case 90:
      case 92:
      case 93:
      case 104:
      case 106:
      case 108:
      case 109:
      case 111:
      case 113:
      case 125:
#line 78 "astgen.in"
{ yygotominor.yy151 = yymsp[0].minor.yy151; }
#line 1178 "astgen.c"
        break;
      case 3:
#line 79 "astgen.in"
{ 
  if(yymsp[-1].minor.yy151->m_type == statement_sequence) {
    yygotominor.yy151 = yymsp[-1].minor.yy151;
  }
  else {
    yygotominor.yy151 = p->AllocAst(statement_sequence, new AstList);
    yygotominor.yy151->m_a1.GetList()->push_back(yymsp[-1].minor.yy151);
  }
  yygotominor.yy151->m_a1.GetList()->push_back(yymsp[0].minor.yy151);
}
#line 1192 "astgen.c"
        break;
      case 4:
      case 20:
      case 98:
#line 92 "astgen.in"
{ yygotominor.yy151 = 0; }
#line 1199 "astgen.c"
        break;
      case 18:
      case 78:
#line 110 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(empty_statement); }
#line 1205 "astgen.c"
        break;
      case 23:
#line 126 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(assignment_expression, yymsp[-1].minor.yy110, yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1210 "astgen.c"
        break;
      case 24:
#line 130 "astgen.in"
{ yygotominor.yy110 = op_assign; }
#line 1215 "astgen.c"
        break;
      case 25:
#line 131 "astgen.in"
{ yygotominor.yy110 = op_assadd; }
#line 1220 "astgen.c"
        break;
      case 26:
#line 132 "astgen.in"
{ yygotominor.yy110 = op_asssub; }
#line 1225 "astgen.c"
        break;
      case 27:
#line 133 "astgen.in"
{ yygotominor.yy110 = op_assmul; }
#line 1230 "astgen.c"
        break;
      case 28:
#line 134 "astgen.in"
{ yygotominor.yy110 = op_assdiv; }
#line 1235 "astgen.c"
        break;
      case 29:
#line 135 "astgen.in"
{ yygotominor.yy110 = op_assmod; }
#line 1240 "astgen.c"
        break;
      case 31:
#line 139 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(ternary_expression, yymsp[-4].minor.yy151, yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1245 "astgen.c"
        break;
      case 33:
#line 143 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(binary_expression, op_logor,   yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1250 "astgen.c"
        break;
      case 34:
#line 144 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(binary_expression, op_logand,  yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1255 "astgen.c"
        break;
      case 35:
#line 145 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(binary_expression, op_bitor,   yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1260 "astgen.c"
        break;
      case 36:
#line 146 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(binary_expression, op_bitxor,  yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1265 "astgen.c"
        break;
      case 37:
#line 147 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(binary_expression, op_bitand,  yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1270 "astgen.c"
        break;
      case 38:
#line 148 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(binary_expression, op_eq,   yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1275 "astgen.c"
        break;
      case 39:
#line 149 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(binary_expression, op_ne,   yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1280 "astgen.c"
        break;
      case 40:
#line 150 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(binary_expression, op_lt,   yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1285 "astgen.c"
        break;
      case 41:
#line 151 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(binary_expression, op_le,   yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1290 "astgen.c"
        break;
      case 42:
#line 152 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(binary_expression, op_gt,   yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1295 "astgen.c"
        break;
      case 43:
#line 153 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(binary_expression, op_ge,   yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1300 "astgen.c"
        break;
      case 44:
#line 154 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(binary_expression, op_add,  yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1305 "astgen.c"
        break;
      case 45:
#line 155 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(binary_expression, op_sub,  yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1310 "astgen.c"
        break;
      case 46:
#line 156 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(binary_expression, op_mul,  yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1315 "astgen.c"
        break;
      case 47:
#line 157 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(binary_expression, op_div,  yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1320 "astgen.c"
        break;
      case 48:
#line 158 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(binary_expression, op_mod,  yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1325 "astgen.c"
        break;
      case 50:
#line 162 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(prefix_expression, op_negate, yymsp[0].minor.yy151); }
#line 1330 "astgen.c"
        break;
      case 51:
#line 163 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(prefix_expression, op_preinc, yymsp[0].minor.yy151); }
#line 1335 "astgen.c"
        break;
      case 52:
#line 164 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(prefix_expression, op_predec, yymsp[0].minor.yy151); }
#line 1340 "astgen.c"
        break;
      case 54:
#line 168 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(postfix_expression, op_postinc, yymsp[-1].minor.yy151); }
#line 1345 "astgen.c"
        break;
      case 55:
#line 169 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(postfix_expression, op_postdec, yymsp[-1].minor.yy151); }
#line 1350 "astgen.c"
        break;
      case 56:
#line 170 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(member_expression, yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1355 "astgen.c"
        break;
      case 57:
#line 171 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(index_expression, yymsp[-3].minor.yy151, yymsp[-1].minor.yy151); }
#line 1360 "astgen.c"
        break;
      case 58:
#line 172 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(function_call, String(yymsp[-2].minor.yy0)); }
#line 1365 "astgen.c"
        break;
      case 59:
#line 173 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(function_call, String(yymsp[-3].minor.yy0), yymsp[-1].minor.yy151); }
#line 1370 "astgen.c"
        break;
      case 62:
      case 87:
      case 97:
      case 114:
#line 178 "astgen.in"
{ yygotominor.yy151 = yymsp[-1].minor.yy151; }
#line 1378 "astgen.c"
        break;
      case 64:
#line 182 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(literal, Variant(String(yymsp[0].minor.yy0), Variant::stInt)); }
#line 1383 "astgen.c"
        break;
      case 65:
#line 183 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(literal, Variant(String(yymsp[0].minor.yy0), Variant::stReal)); }
#line 1388 "astgen.c"
        break;
      case 66:
#line 184 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(literal, Variant(String(yymsp[0].minor.yy0), Variant::stString)); }
#line 1393 "astgen.c"
        break;
      case 67:
#line 185 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(literal, Variant(true));    }
#line 1398 "astgen.c"
        break;
      case 68:
#line 186 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(literal, Variant(false));   }
#line 1403 "astgen.c"
        break;
      case 69:
#line 187 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(literal, Variant());        }
#line 1408 "astgen.c"
        break;
      case 70:
#line 190 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(lvalue, String(yymsp[0].minor.yy0)); }
#line 1413 "astgen.c"
        break;
      case 71:
#line 193 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(list_literal, yymsp[-1].minor.yy151); }
#line 1418 "astgen.c"
        break;
      case 72:
#line 195 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(list_content, yymsp[0].minor.yy151); }
#line 1423 "astgen.c"
        break;
      case 73:
#line 196 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(list_content, yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1428 "astgen.c"
        break;
      case 74:
#line 198 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(list_entry, yymsp[0].minor.yy151); }
#line 1433 "astgen.c"
        break;
      case 75:
#line 207 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(argument, yymsp[0].minor.yy151); }
#line 1438 "astgen.c"
        break;
      case 77:
#line 211 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(argument_list, yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1443 "astgen.c"
        break;
      case 79:
#line 220 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(expression_statement, yymsp[-1].minor.yy151); }
#line 1448 "astgen.c"
        break;
      case 80:
#line 223 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(compound_statement); }
#line 1453 "astgen.c"
        break;
      case 81:
#line 224 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(compound_statement, yymsp[-1].minor.yy151); }
#line 1458 "astgen.c"
        break;
      case 82:
#line 227 "astgen.in"
{ p->Parse(yymsp[-1].minor.yy0); yygotominor.yy151 = p->GetRoot(); }
#line 1463 "astgen.c"
        break;
      case 83:
#line 230 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(return_statement, yymsp[-1].minor.yy151); }
#line 1468 "astgen.c"
        break;
      case 84:
#line 231 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(return_statement);    }
#line 1473 "astgen.c"
        break;
      case 85:
#line 234 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(break_statement); }
#line 1478 "astgen.c"
        break;
      case 86:
#line 235 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(continue_statement); }
#line 1483 "astgen.c"
        break;
      case 91:
#line 249 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(declaration_sequence, yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1488 "astgen.c"
        break;
      case 94:
#line 256 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(variable_declaration, String(yymsp[0].minor.yy0)); }
#line 1493 "astgen.c"
        break;
      case 95:
#line 259 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(variable_declaration, String(yymsp[-2].minor.yy0), yymsp[0].minor.yy151); }
#line 1498 "astgen.c"
        break;
      case 96:
#line 267 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(struct_declaration, String(yymsp[-3].minor.yy0), yymsp[-1].minor.yy151); }
#line 1503 "astgen.c"
        break;
      case 99:
#line 274 "astgen.in"
{ 
  if(yymsp[-1].minor.yy151) {
    yygotominor.yy151 = p->AllocAst(struct_members, yymsp[-1].minor.yy151, yymsp[0].minor.yy151); 
  }
  else {
    yygotominor.yy151 = yymsp[0].minor.yy151;
  }
}
#line 1515 "astgen.c"
        break;
      case 100:
#line 289 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(function_declaration, String(yymsp[-4].minor.yy0), yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1520 "astgen.c"
        break;
      case 101:
#line 290 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(function_declaration, String(yymsp[-3].minor.yy0), AstData(), yymsp[0].minor.yy151); }
#line 1525 "astgen.c"
        break;
      case 102:
#line 293 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(parameter, String(yymsp[0].minor.yy0)); }
#line 1530 "astgen.c"
        break;
      case 103:
#line 296 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(parameter, String(yymsp[-2].minor.yy0), yymsp[0].minor.yy151); }
#line 1535 "astgen.c"
        break;
      case 105:
      case 107:
      case 110:
#line 300 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(parameter_list, yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1542 "astgen.c"
        break;
      case 112:
#line 321 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(for_statement, yymsp[-5].minor.yy151, yymsp[-4].minor.yy151, yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1547 "astgen.c"
        break;
      case 115:
#line 332 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(foreach_statement, yymsp[-4].minor.yy151, yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1552 "astgen.c"
        break;
      case 116:
#line 343 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(if_statement, yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1557 "astgen.c"
        break;
      case 117:
#line 344 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(if_statement, yymsp[-4].minor.yy151, yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1562 "astgen.c"
        break;
      case 118:
#line 352 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(while_statement, yymsp[-2].minor.yy151,  yymsp[0].minor.yy151); }
#line 1567 "astgen.c"
        break;
      case 119:
#line 360 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(switch_statement, yymsp[-4].minor.yy151, yymsp[-1].minor.yy183); }
#line 1572 "astgen.c"
        break;
      case 120:
#line 364 "astgen.in"
{ yygotominor.yy183 = new AstList; }
#line 1577 "astgen.c"
        break;
      case 121:
      case 122:
#line 365 "astgen.in"
{ yygotominor.yy183 = yymsp[-1].minor.yy183; yygotominor.yy183->push_back(yymsp[0].minor.yy151); }
#line 1583 "astgen.c"
        break;
      case 123:
#line 369 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(switch_case, yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1588 "astgen.c"
        break;
      case 124:
#line 372 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(default_case, yymsp[0].minor.yy151); }
#line 1593 "astgen.c"
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
#line 1641 "astgen.c"
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
#line 1659 "astgen.c"
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

