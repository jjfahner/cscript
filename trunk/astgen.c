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
#define YYNOCODE 121
#define YYACTIONTYPE unsigned short int
#define CScriptParseTOKENTYPE  Token 
typedef union {
  CScriptParseTOKENTYPE yy0;
  AstList* yy55;
  Ast* yy151;
  opcodes yy174;
  int yy241;
} YYMINORTYPE;
#ifndef YYSTACKDEPTH
#define YYSTACKDEPTH 100
#endif
#define CScriptParseARG_SDECL  Parser* p ;
#define CScriptParseARG_PDECL , Parser* p 
#define CScriptParseARG_FETCH  Parser* p  = yypParser->p 
#define CScriptParseARG_STORE yypParser->p  = p 
#define YYNSTATE 238
#define YYNRULE 136
#define YYERRORSYMBOL 66
#define YYERRSYMDT yy241
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
 /*     0 */   209,  375,  236,  234,   13,  233,  229,  228,  225,  224,
 /*    10 */   222,  220,  219,  218,  217,  215,  212,  211,  123,  208,
 /*    20 */   135,  207,   64,  194,  205,   82,  192,  191,   53,  188,
 /*    30 */   186,  184,   51,   42,   38,  139,  124,  122,   65,  108,
 /*    40 */    54,   55,   56,  157,  100,   17,  138,   25,   63,  183,
 /*    50 */   181,  180,  179,  178,  177,  176,  174,  173,  136,   94,
 /*    60 */   142,    3,  237,  128,   21,  130,  127,   63,  105,  110,
 /*    70 */    97,   53,  115,  151,   89,   92,   95,   48,   49,   51,
 /*    80 */    42,   38,  112,   54,   55,   56,  160,  100,   17,   27,
 /*    90 */    25,   23,  183,  181,  180,  179,  178,  177,  176,  174,
 /*   100 */   173,  141,   94,  142,    3,  137,  128,   21,  130,  127,
 /*   110 */    63,  105,  110,   97,  195,  196,  209,   89,   92,   95,
 /*   120 */     8,  233,  229,  228,  225,  224,  222,  220,  219,  218,
 /*   130 */   217,  215,  212,  211,  123,  208,   66,  207,   64,  120,
 /*   140 */   205,   82,  192,  191,  120,  188,  186,  184,    5,   53,
 /*   150 */   155,  139,  124,  122,  101,   63,  227,  231,  121,  119,
 /*   160 */    60,   54,   55,   56,  140,  100,   17,  133,   25,  204,
 /*   170 */   183,  181,  180,  179,  178,  177,  176,  174,  173,  143,
 /*   180 */    94,  142,    3,  189,  128,   21,  130,  127,   63,  105,
 /*   190 */   110,   97,  150,   20,  209,   89,   92,   95,    8,  233,
 /*   200 */   229,  228,  225,  224,  222,  220,  219,  218,  217,  215,
 /*   210 */   212,  211,  123,  208,  172,  207,   64,   59,  205,   82,
 /*   220 */   192,  191,   96,  188,  186,  184,   18,   24,  126,  139,
 /*   230 */   124,  122,   44,   43,   41,   40,   35,   45,   48,   49,
 /*   240 */    51,   42,   38,  209,  235,  104,   10,  201,  168,  229,
 /*   250 */   228,  225,  224,  222,  220,  219,  218,  217,  215,  212,
 /*   260 */   211,  123,  208,   31,  207,   64,  363,  205,   82,  192,
 /*   270 */   191,   33,  188,  186,  184,  216,   67,   86,  139,  124,
 /*   280 */   122,  112,  209,   11,   61,  161,  167,  168,  229,  228,
 /*   290 */   225,  224,  222,  220,  219,  218,  217,  215,  212,  211,
 /*   300 */   123,  208,  129,  207,   64,   19,  205,   82,  192,  191,
 /*   310 */    14,  188,  186,  184,  159,   32,  103,  139,  124,  122,
 /*   320 */   230,  231,  209,  125,   12,  223,    6,  233,  229,  228,
 /*   330 */   225,  224,  222,  220,  219,  218,  217,  215,  212,  211,
 /*   340 */   123,  208,  185,  207,   64,    9,  205,   82,  192,  191,
 /*   350 */   162,  188,  186,  184,   22,  209,    1,  139,  124,  122,
 /*   360 */    91,  229,  228,  225,  224,  222,  220,  219,  218,  217,
 /*   370 */   215,  212,  211,  123,  208,    7,  207,   64,  169,  205,
 /*   380 */    82,  192,  191,   58,  188,  186,  184,   28,  200,  209,
 /*   390 */   139,  124,  122,  107,  213,  229,  228,  225,  224,  222,
 /*   400 */   220,  219,  218,  217,  215,  212,  211,  123,  208,    2,
 /*   410 */   207,   64,   24,  205,   82,  192,  191,  102,  188,  186,
 /*   420 */   184,   15,  209,   57,  139,  124,  122,  187,  229,  228,
 /*   430 */   225,  224,  222,  220,  219,  218,  217,  215,  212,  211,
 /*   440 */   123,  208,   68,  207,   64,   34,  205,   82,  192,  191,
 /*   450 */     4,  188,  186,  184,   16,  376,  209,  139,  124,  122,
 /*   460 */   376,  182,  229,  228,  225,  224,  222,  220,  219,  218,
 /*   470 */   217,  215,  212,  211,  123,  208,  376,  207,   64,  376,
 /*   480 */   205,   82,  192,  191,  376,  188,  186,  184,  376,  209,
 /*   490 */   376,  139,  124,  122,  221,  229,  228,  225,  224,  222,
 /*   500 */   220,  219,  218,  217,  215,  212,  211,  123,  208,  376,
 /*   510 */   207,   64,  376,  205,   82,  192,  191,  376,  188,  186,
 /*   520 */   184,  376,  376,  209,  139,  124,  122,  376,  232,  229,
 /*   530 */   228,  225,  224,  222,  220,  219,  218,  217,  215,  212,
 /*   540 */   211,  123,  208,  376,  207,   64,  376,  205,   82,  192,
 /*   550 */   191,  376,  188,  186,  184,  376,  209,  376,  139,  124,
 /*   560 */   122,  175,  229,  228,  225,  224,  222,  220,  219,  218,
 /*   570 */   217,  215,  212,  211,  123,  208,  376,  207,   64,  376,
 /*   580 */   205,   82,  192,  191,  376,  188,  186,  184,  376,  376,
 /*   590 */   376,  139,  124,  122,   36,   37,   52,   39,   50,   46,
 /*   600 */    47,   44,   43,   41,   40,   35,   45,   48,   49,   51,
 /*   610 */    42,   38,  158,  156,  154,  153,  152,  148,   29,   53,
 /*   620 */    41,   40,   35,   45,   48,   49,   51,   42,   38,  376,
 /*   630 */   376,   54,   55,   56,  376,  100,   17,  376,   25,  376,
 /*   640 */   183,  181,  180,  179,  178,  177,  176,  174,  173,   53,
 /*   650 */    94,  142,  203,  202,  376,  106,  376,   30,   62,  376,
 /*   660 */   376,   54,   55,   56,  376,  100,   17,  376,   25,  190,
 /*   670 */   183,  181,  180,  179,  178,  177,  176,  174,  173,   53,
 /*   680 */    94,  183,  181,  180,  179,  178,  177,  176,  174,  173,
 /*   690 */   376,   54,   55,   56,  376,  100,   17,  376,   25,  376,
 /*   700 */   183,  181,  180,  179,  178,  177,  176,  174,  173,   53,
 /*   710 */    94,  134,  197,   82,  192,  191,  376,  188,  186,  184,
 /*   720 */   376,   54,   55,   56,  376,  100,   17,  376,   25,  376,
 /*   730 */   183,  181,  180,  179,  178,  177,  176,  174,  173,  376,
 /*   740 */    94,   37,   52,   39,   50,   46,   47,   44,   43,   41,
 /*   750 */    40,   35,   45,   48,   49,   51,   42,   38,   52,   39,
 /*   760 */    50,   46,   47,   44,   43,   41,   40,   35,   45,   48,
 /*   770 */    49,   51,   42,   38,   39,   50,   46,   47,   44,   43,
 /*   780 */    41,   40,   35,   45,   48,   49,   51,   42,   38,  376,
 /*   790 */   170,  208,  376,  207,   64,  214,  205,   82,  192,  191,
 /*   800 */   376,  188,  186,  184,   87,  166,  123,  208,  376,  207,
 /*   810 */    64,  376,  205,   82,  192,  191,  376,  188,  131,  184,
 /*   820 */   170,  208,  376,  207,   64,  116,  205,   82,  192,  191,
 /*   830 */   376,  188,  186,  184,  376,  171,   26,  117,  376,  376,
 /*   840 */   376,  376,   50,   46,   47,   44,   43,   41,   40,   35,
 /*   850 */    45,   48,   49,   51,   42,   38,  165,  208,  376,  207,
 /*   860 */    64,  376,  205,   82,  192,  191,   85,  188,  186,  184,
 /*   870 */   376,  376,  163,   46,   47,   44,   43,   41,   40,   35,
 /*   880 */    45,   48,   49,   51,   42,   38,  165,  208,  376,  207,
 /*   890 */    64,  376,  205,   82,  192,  191,  376,  188,  186,  184,
 /*   900 */   376,  376,  164,  210,  208,  114,  207,   64,  376,  205,
 /*   910 */    82,  192,  191,  376,  188,  186,  184,   90,  208,  376,
 /*   920 */   207,   64,  376,  205,   82,  192,  191,  376,  188,  186,
 /*   930 */   184,  109,  208,  376,  207,   64,  376,  205,   82,  192,
 /*   940 */   191,  376,  188,  186,  184,   99,  208,  376,  207,   64,
 /*   950 */   376,  205,   82,  192,  191,  376,  188,  186,  184,   88,
 /*   960 */   208,  376,  207,   64,  376,  205,   82,  192,  191,  376,
 /*   970 */   188,  186,  184,  376,   98,  208,  376,  207,   64,  376,
 /*   980 */   205,   82,  192,  191,  376,  188,  186,  184,  132,  208,
 /*   990 */   376,  207,   64,  376,  205,   82,  192,  191,  376,  188,
 /*  1000 */   186,  184,  226,  208,  376,  207,   64,  376,  205,   82,
 /*  1010 */   192,  191,  376,  188,  186,  184,  118,  208,  376,  207,
 /*  1020 */    64,  376,  205,   82,  192,  191,  376,  188,  186,  184,
 /*  1030 */   113,  208,  376,  207,   64,  376,  205,   82,  192,  191,
 /*  1040 */   376,  188,  186,  184,  149,  208,  376,  207,   64,  376,
 /*  1050 */   205,   82,  192,  191,  376,  188,  186,  184,  376,  111,
 /*  1060 */   208,  376,  207,   64,  376,  205,   82,  192,  191,  376,
 /*  1070 */   188,  186,  184,   93,  208,  376,  207,   64,  376,  205,
 /*  1080 */    82,  192,  191,  376,  188,  186,  184,  206,  376,  207,
 /*  1090 */    64,  376,  205,   82,  192,  191,  376,  188,  186,  184,
 /*  1100 */   147,  376,  207,   64,  376,  205,   82,  192,  191,  376,
 /*  1110 */   188,  186,  184,  146,  376,  205,   82,  192,  191,  376,
 /*  1120 */   188,  186,  184,   73,  376,  205,   82,  192,  191,  376,
 /*  1130 */   188,  186,  184,   71,  376,  205,   82,  192,  191,  376,
 /*  1140 */   188,  186,  184,  376,   83,  376,  205,   82,  192,  191,
 /*  1150 */   376,  188,  186,  184,   75,  376,  205,   82,  192,  191,
 /*  1160 */   376,  188,  186,  184,  376,   74,  376,  205,   82,  192,
 /*  1170 */   191,  376,  188,  186,  184,   79,  376,  205,   82,  192,
 /*  1180 */   191,  376,  188,  186,  184,  376,   77,  376,  205,   82,
 /*  1190 */   192,  191,  376,  188,  186,  184,   76,  376,  205,   82,
 /*  1200 */   192,  191,  376,  188,  186,  184,  145,  376,  205,   82,
 /*  1210 */   192,  191,  376,  188,  186,  184,   78,  376,  205,   82,
 /*  1220 */   192,  191,  376,  188,  186,  184,   81,  376,  205,   82,
 /*  1230 */   192,  191,  376,  188,  186,  184,   72,  376,  205,   82,
 /*  1240 */   192,  191,  376,  188,  186,  184,  144,  376,  205,   82,
 /*  1250 */   192,  191,  376,  188,  186,  184,   80,  376,  205,   82,
 /*  1260 */   192,  191,  376,  188,  186,  184,   70,  376,  205,   82,
 /*  1270 */   192,  191,  376,  188,  186,  184,  376,   84,  376,  205,
 /*  1280 */    82,  192,  191,  376,  188,  186,  184,   69,  376,  205,
 /*  1290 */    82,  192,  191,  376,  188,  186,  184,  198,   82,  192,
 /*  1300 */   191,  376,  188,  186,  184,  199,   82,  192,  191,  376,
 /*  1310 */   188,  186,  184,  376,  193,   82,  192,  191,  376,  188,
 /*  1320 */   186,  184,
};
static const YYCODETYPE yy_lookahead[] = {
 /*     0 */    66,   67,   68,   69,   70,   71,   72,   73,   74,   75,
 /*    10 */    76,   77,   78,   79,   80,   81,   82,   83,   84,   85,
 /*    20 */    47,   87,   88,   49,   90,   91,   92,   93,   15,   95,
 /*    30 */    96,   97,   16,   17,   18,  101,  102,  103,   64,   65,
 /*    40 */    27,   28,   29,   49,   31,   32,   47,   34,   54,   36,
 /*    50 */    37,   38,   39,   40,   41,   42,   43,   44,   47,   46,
 /*    60 */    47,   48,   49,   50,   51,   52,   53,   54,   55,   56,
 /*    70 */    57,   15,  104,  105,   61,   62,   63,   14,   15,   16,
 /*    80 */    17,   18,  103,   27,   28,   29,  107,   31,   32,   19,
 /*    90 */    34,   58,   36,   37,   38,   39,   40,   41,   42,   43,
 /*   100 */    44,   47,   46,   47,   48,   49,   50,   51,   52,   53,
 /*   110 */    54,   55,   56,   57,  117,  118,   66,   61,   62,   63,
 /*   120 */    70,   71,   72,   73,   74,   75,   76,   77,   78,   79,
 /*   130 */    80,   81,   82,   83,   84,   85,   45,   87,   88,   31,
 /*   140 */    90,   91,   92,   93,   31,   95,   96,   97,   35,   15,
 /*   150 */    49,  101,  102,  103,  108,   54,  110,  111,  112,  113,
 /*   160 */   116,   27,   28,   29,   47,   31,   32,   47,   34,  119,
 /*   170 */    36,   37,   38,   39,   40,   41,   42,   43,   44,   47,
 /*   180 */    46,   47,   48,   35,   50,   51,   52,   53,   54,   55,
 /*   190 */    56,   57,  105,   45,   66,   61,   62,   63,   70,   71,
 /*   200 */    72,   73,   74,   75,   76,   77,   78,   79,   80,   81,
 /*   210 */    82,   83,   84,   85,   33,   87,   88,   45,   90,   91,
 /*   220 */    92,   93,   31,   95,   96,   97,   45,   19,   31,  101,
 /*   230 */   102,  103,    8,    9,   10,   11,   12,   13,   14,   15,
 /*   240 */    16,   17,   18,   66,  111,   95,   35,  119,   71,   72,
 /*   250 */    73,   74,   75,   76,   77,   78,   79,   80,   81,   82,
 /*   260 */    83,   84,   85,   58,   87,   88,   58,   90,   91,   92,
 /*   270 */    93,   89,   95,   96,   97,   47,   45,   31,  101,  102,
 /*   280 */   103,  103,   66,   35,  106,  107,  109,   71,   72,   73,
 /*   290 */    74,   75,   76,   77,   78,   79,   80,   81,   82,   83,
 /*   300 */    84,   85,   41,   87,   88,   47,   90,   91,   92,   93,
 /*   310 */    35,   95,   96,   97,   47,   34,   31,  101,  102,  103,
 /*   320 */   110,  111,   66,  113,   60,  109,   70,   71,   72,   73,
 /*   330 */    74,   75,   76,   77,   78,   79,   80,   81,   82,   83,
 /*   340 */    84,   85,   35,   87,   88,   35,   90,   91,   92,   93,
 /*   350 */    33,   95,   96,   97,   34,   66,   26,  101,  102,  103,
 /*   360 */    71,   72,   73,   74,   75,   76,   77,   78,   79,   80,
 /*   370 */    81,   82,   83,   84,   85,   35,   87,   88,   31,   90,
 /*   380 */    91,   92,   93,   48,   95,   96,   97,   34,   31,   66,
 /*   390 */   101,  102,  103,   31,   71,   72,   73,   74,   75,   76,
 /*   400 */    77,   78,   79,   80,   81,   82,   83,   84,   85,   26,
 /*   410 */    87,   88,   19,   90,   91,   92,   93,   35,   95,   96,
 /*   420 */    97,   34,   66,   34,  101,  102,  103,   71,   72,   73,
 /*   430 */    74,   75,   76,   77,   78,   79,   80,   81,   82,   83,
 /*   440 */    84,   85,   48,   87,   88,   26,   90,   91,   92,   93,
 /*   450 */    35,   95,   96,   97,   34,  120,   66,  101,  102,  103,
 /*   460 */   120,   71,   72,   73,   74,   75,   76,   77,   78,   79,
 /*   470 */    80,   81,   82,   83,   84,   85,  120,   87,   88,  120,
 /*   480 */    90,   91,   92,   93,  120,   95,   96,   97,  120,   66,
 /*   490 */   120,  101,  102,  103,   71,   72,   73,   74,   75,   76,
 /*   500 */    77,   78,   79,   80,   81,   82,   83,   84,   85,  120,
 /*   510 */    87,   88,  120,   90,   91,   92,   93,  120,   95,   96,
 /*   520 */    97,  120,  120,   66,  101,  102,  103,  120,   71,   72,
 /*   530 */    73,   74,   75,   76,   77,   78,   79,   80,   81,   82,
 /*   540 */    83,   84,   85,  120,   87,   88,  120,   90,   91,   92,
 /*   550 */    93,  120,   95,   96,   97,  120,   66,  120,  101,  102,
 /*   560 */   103,   71,   72,   73,   74,   75,   76,   77,   78,   79,
 /*   570 */    80,   81,   82,   83,   84,   85,  120,   87,   88,  120,
 /*   580 */    90,   91,   92,   93,  120,   95,   96,   97,  120,  120,
 /*   590 */   120,  101,  102,  103,    1,    2,    3,    4,    5,    6,
 /*   600 */     7,    8,    9,   10,   11,   12,   13,   14,   15,   16,
 /*   610 */    17,   18,   19,   20,   21,   22,   23,   24,   25,   15,
 /*   620 */    10,   11,   12,   13,   14,   15,   16,   17,   18,  120,
 /*   630 */   120,   27,   28,   29,  120,   31,   32,  120,   34,  120,
 /*   640 */    36,   37,   38,   39,   40,   41,   42,   43,   44,   15,
 /*   650 */    46,   47,   27,   28,  120,   30,  120,   32,   54,  120,
 /*   660 */   120,   27,   28,   29,  120,   31,   32,  120,   34,   35,
 /*   670 */    36,   37,   38,   39,   40,   41,   42,   43,   44,   15,
 /*   680 */    46,   36,   37,   38,   39,   40,   41,   42,   43,   44,
 /*   690 */   120,   27,   28,   29,  120,   31,   32,  120,   34,  120,
 /*   700 */    36,   37,   38,   39,   40,   41,   42,   43,   44,   15,
 /*   710 */    46,   47,   90,   91,   92,   93,  120,   95,   96,   97,
 /*   720 */   120,   27,   28,   29,  120,   31,   32,  120,   34,  120,
 /*   730 */    36,   37,   38,   39,   40,   41,   42,   43,   44,  120,
 /*   740 */    46,    2,    3,    4,    5,    6,    7,    8,    9,   10,
 /*   750 */    11,   12,   13,   14,   15,   16,   17,   18,    3,    4,
 /*   760 */     5,    6,    7,    8,    9,   10,   11,   12,   13,   14,
 /*   770 */    15,   16,   17,   18,    4,    5,    6,    7,    8,    9,
 /*   780 */    10,   11,   12,   13,   14,   15,   16,   17,   18,  120,
 /*   790 */    84,   85,  120,   87,   88,   73,   90,   91,   92,   93,
 /*   800 */   120,   95,   96,   97,   98,   99,   84,   85,  120,   87,
 /*   810 */    88,  120,   90,   91,   92,   93,  120,   95,   96,   97,
 /*   820 */    84,   85,  120,   87,   88,  103,   90,   91,   92,   93,
 /*   830 */   120,   95,   96,   97,  120,   99,  114,  115,  120,  120,
 /*   840 */   120,  120,    5,    6,    7,    8,    9,   10,   11,   12,
 /*   850 */    13,   14,   15,   16,   17,   18,   84,   85,  120,   87,
 /*   860 */    88,  120,   90,   91,   92,   93,   94,   95,   96,   97,
 /*   870 */   120,  120,  100,    6,    7,    8,    9,   10,   11,   12,
 /*   880 */    13,   14,   15,   16,   17,   18,   84,   85,  120,   87,
 /*   890 */    88,  120,   90,   91,   92,   93,  120,   95,   96,   97,
 /*   900 */   120,  120,  100,   84,   85,   86,   87,   88,  120,   90,
 /*   910 */    91,   92,   93,  120,   95,   96,   97,   84,   85,  120,
 /*   920 */    87,   88,  120,   90,   91,   92,   93,  120,   95,   96,
 /*   930 */    97,   84,   85,  120,   87,   88,  120,   90,   91,   92,
 /*   940 */    93,  120,   95,   96,   97,   84,   85,  120,   87,   88,
 /*   950 */   120,   90,   91,   92,   93,  120,   95,   96,   97,   84,
 /*   960 */    85,  120,   87,   88,  120,   90,   91,   92,   93,  120,
 /*   970 */    95,   96,   97,  120,   84,   85,  120,   87,   88,  120,
 /*   980 */    90,   91,   92,   93,  120,   95,   96,   97,   84,   85,
 /*   990 */   120,   87,   88,  120,   90,   91,   92,   93,  120,   95,
 /*  1000 */    96,   97,   84,   85,  120,   87,   88,  120,   90,   91,
 /*  1010 */    92,   93,  120,   95,   96,   97,   84,   85,  120,   87,
 /*  1020 */    88,  120,   90,   91,   92,   93,  120,   95,   96,   97,
 /*  1030 */    84,   85,  120,   87,   88,  120,   90,   91,   92,   93,
 /*  1040 */   120,   95,   96,   97,   84,   85,  120,   87,   88,  120,
 /*  1050 */    90,   91,   92,   93,  120,   95,   96,   97,  120,   84,
 /*  1060 */    85,  120,   87,   88,  120,   90,   91,   92,   93,  120,
 /*  1070 */    95,   96,   97,   84,   85,  120,   87,   88,  120,   90,
 /*  1080 */    91,   92,   93,  120,   95,   96,   97,   85,  120,   87,
 /*  1090 */    88,  120,   90,   91,   92,   93,  120,   95,   96,   97,
 /*  1100 */    85,  120,   87,   88,  120,   90,   91,   92,   93,  120,
 /*  1110 */    95,   96,   97,   88,  120,   90,   91,   92,   93,  120,
 /*  1120 */    95,   96,   97,   88,  120,   90,   91,   92,   93,  120,
 /*  1130 */    95,   96,   97,   88,  120,   90,   91,   92,   93,  120,
 /*  1140 */    95,   96,   97,  120,   88,  120,   90,   91,   92,   93,
 /*  1150 */   120,   95,   96,   97,   88,  120,   90,   91,   92,   93,
 /*  1160 */   120,   95,   96,   97,  120,   88,  120,   90,   91,   92,
 /*  1170 */    93,  120,   95,   96,   97,   88,  120,   90,   91,   92,
 /*  1180 */    93,  120,   95,   96,   97,  120,   88,  120,   90,   91,
 /*  1190 */    92,   93,  120,   95,   96,   97,   88,  120,   90,   91,
 /*  1200 */    92,   93,  120,   95,   96,   97,   88,  120,   90,   91,
 /*  1210 */    92,   93,  120,   95,   96,   97,   88,  120,   90,   91,
 /*  1220 */    92,   93,  120,   95,   96,   97,   88,  120,   90,   91,
 /*  1230 */    92,   93,  120,   95,   96,   97,   88,  120,   90,   91,
 /*  1240 */    92,   93,  120,   95,   96,   97,   88,  120,   90,   91,
 /*  1250 */    92,   93,  120,   95,   96,   97,   88,  120,   90,   91,
 /*  1260 */    92,   93,  120,   95,   96,   97,   88,  120,   90,   91,
 /*  1270 */    92,   93,  120,   95,   96,   97,  120,   88,  120,   90,
 /*  1280 */    91,   92,   93,  120,   95,   96,   97,   88,  120,   90,
 /*  1290 */    91,   92,   93,  120,   95,   96,   97,   90,   91,   92,
 /*  1300 */    93,  120,   95,   96,   97,   90,   91,   92,   93,  120,
 /*  1310 */    95,   96,   97,  120,   90,   91,   92,   93,  120,   95,
 /*  1320 */    96,   97,
};
#define YY_SHIFT_USE_DFLT (-28)
#define YY_SHIFT_MAX 132
static const short yy_shift_ofst[] = {
 /*     0 */   134,  134,  134,   13,  134,  134,   56,  134,  134,  134,
 /*    10 */   134,  134,  134,  134,  134,  604,  634,  694,  694,  694,
 /*    20 */   694,  664,  694,  694,  694,  694,  694,  694,  694,  694,
 /*    30 */   694,  694,  694,  694,  694,  694,  694,  694,  694,  694,
 /*    40 */   694,  694,  694,  694,  694,  694,  694,  694,  694,  694,
 /*    50 */   694,  694,  694,  694,  694,  694,  694,  113,  101,  108,
 /*    60 */   -26,   -6,  246,  191,  593,  645,  197,  191,  -28,  739,
 /*    70 */   755,  770,  837,  867,  224,  224,  610,  610,   63,   63,
 /*    80 */    63,   63,  625,   16,   16,  148,  208,  181,  275,  281,
 /*    90 */   310,  264,  320,  340,  347,  353,  393,  387,  382,  419,
 /*   100 */   420,  415,  394,  389,  383,  362,  357,  335,  330,  317,
 /*   110 */   285,  307,  267,  258,  248,  231,  228,  205,  211,   91,
 /*   120 */    70,  172,  132,  117,   54,   91,   70,   -1,  261,  -27,
 /*   130 */    11,   33,  120,
};
#define YY_REDUCE_USE_DFLT (-67)
#define YY_REDUCE_MAX 68
static const short yy_reduce_ofst[] = {
 /*     0 */   -66,   50,  128,  256,  177,  216,  457,  356,  457,  289,
 /*    10 */   423,  323,  390,  457,  490,  722,  772,  706,  736,  819,
 /*    20 */   802,  904,  989,  875,  960,  975,  946,  918,  890,  861,
 /*    30 */   847,  932,  833, 1002, 1015, 1168, 1199, 1178, 1158, 1148,
 /*    40 */  1138, 1128, 1118, 1108, 1098, 1087, 1077, 1066, 1189, 1056,
 /*    50 */  1035, 1025, 1045, 1215, 1207,  622, 1224,   46,  178,  210,
 /*    60 */    -3,  -21,  -32,  -32,  182,  150,  133,   87,   44,
};
static const YYACTIONTYPE yy_default[] = {
 /*     0 */   242,  374,  374,  374,  374,  374,  374,  374,  373,  374,
 /*    10 */   374,  374,  374,  243,  374,  374,  374,  374,  374,  258,
 /*    20 */   374,  374,  374,  374,  374,  374,  374,  374,  374,  374,
 /*    30 */   374,  374,  374,  374,  374,  374,  374,  374,  374,  374,
 /*    40 */   374,  374,  374,  374,  374,  374,  374,  374,  374,  374,
 /*    50 */   374,  374,  374,  374,  374,  374,  374,  374,  374,  374,
 /*    60 */   374,  374,  374,  374,  268,  374,  374,  374,  368,  271,
 /*    70 */   272,  273,  274,  275,  287,  288,  277,  276,  278,  281,
 /*    80 */   280,  279,  289,  283,  282,  374,  337,  374,  374,  374,
 /*    90 */   374,  364,  374,  374,  374,  374,  337,  374,  374,  374,
 /*   100 */   315,  374,  374,  374,  374,  374,  374,  374,  374,  374,
 /*   110 */   374,  374,  374,  374,  374,  336,  374,  374,  374,  355,
 /*   120 */   348,  354,  374,  374,  374,  356,  374,  374,  374,  374,
 /*   130 */   374,  303,  374,  329,  330,  328,  331,  327,  332,  333,
 /*   140 */   325,  334,  324,  335,  286,  285,  284,  269,  267,  338,
 /*   150 */   340,  339,  266,  265,  264,  341,  263,  342,  262,  343,
 /*   160 */   345,  344,  299,  322,  323,  321,  317,  346,  357,  320,
 /*   170 */   319,  318,  316,  314,  313,  361,  312,  311,  310,  309,
 /*   180 */   308,  307,  365,  306,  305,  304,  303,  366,  302,  301,
 /*   190 */   300,  295,  294,  293,  367,  369,  370,  292,  291,  290,
 /*   200 */   298,  371,  297,  296,  372,  270,  261,  260,  257,  256,
 /*   210 */   259,  255,  254,  358,  359,  253,  360,  252,  251,  250,
 /*   220 */   249,  362,  248,  347,  247,  246,  349,  350,  245,  244,
 /*   230 */   351,  352,  241,  240,  239,  353,  238,  326,
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
  "INTEGER",       "HEX",           "BIN",           "ROM",         
  "REAL",          "STRING",        "TRUE",          "FALSE",       
  "NULL",          "COMMA",         "NEW",           "SEMICOLON",   
  "LBRACE",        "RBRACE",        "INCLUDE",       "RETURN",      
  "BREAK",         "CONTINUE",      "VAR",           "STRUCT",      
  "FUNCTION",      "FOR",           "IN",            "LOWER_THAN_ELSE",
  "ELSE",          "IF",            "WHILE",         "SWITCH",      
  "CASE",          "DEFAULT",       "error",         "main",        
  "translation_unit",  "statement_sequence_opt",  "statement_sequence",  "statement",   
  "include_statement",  "expression_statement",  "declaration_statement",  "for_statement",
  "compound_statement",  "if_statement",  "while_statement",  "foreach_statement",
  "return_statement",  "switch_statement",  "break_statement",  "continue_statement",
  "expression",    "assignment_expression",  "expression_opt",  "conditional_expression",
  "binary_expression",  "assignment_operator",  "unary_expression",  "postfix_expression",
  "new_expression",  "primary_expression",  "argument_list",  "literal",     
  "id_expression",  "list_literal",  "list_content",  "list_entry",  
  "argument",      "function_declaration",  "struct_declaration",  "variable_declaration",
  "declarator_sequence",  "declarator",    "struct_members",  "struct_member",
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
 /*  60 */ "postfix_expression ::= postfix_expression DOT IDENTIFIER",
 /*  61 */ "postfix_expression ::= postfix_expression LBRACKET expression RBRACKET",
 /*  62 */ "postfix_expression ::= IDENTIFIER LPAREN RPAREN",
 /*  63 */ "postfix_expression ::= IDENTIFIER LPAREN argument_list RPAREN",
 /*  64 */ "primary_expression ::= literal",
 /*  65 */ "primary_expression ::= id_expression",
 /*  66 */ "primary_expression ::= LPAREN expression RPAREN",
 /*  67 */ "primary_expression ::= list_literal",
 /*  68 */ "literal ::= INTEGER",
 /*  69 */ "literal ::= HEX",
 /*  70 */ "literal ::= BIN",
 /*  71 */ "literal ::= ROM",
 /*  72 */ "literal ::= REAL",
 /*  73 */ "literal ::= STRING",
 /*  74 */ "literal ::= TRUE",
 /*  75 */ "literal ::= FALSE",
 /*  76 */ "literal ::= NULL",
 /*  77 */ "id_expression ::= IDENTIFIER",
 /*  78 */ "list_literal ::= LBRACKET list_content RBRACKET",
 /*  79 */ "list_content ::= list_entry",
 /*  80 */ "list_content ::= list_content COMMA list_entry",
 /*  81 */ "list_entry ::= expression",
 /*  82 */ "new_expression ::= NEW IDENTIFIER",
 /*  83 */ "argument ::= expression",
 /*  84 */ "argument_list ::= argument",
 /*  85 */ "argument_list ::= argument_list COMMA argument",
 /*  86 */ "expression_statement ::= SEMICOLON",
 /*  87 */ "expression_statement ::= expression SEMICOLON",
 /*  88 */ "compound_statement ::= LBRACE RBRACE",
 /*  89 */ "compound_statement ::= LBRACE statement_sequence RBRACE",
 /*  90 */ "include_statement ::= INCLUDE STRING SEMICOLON",
 /*  91 */ "return_statement ::= RETURN expression SEMICOLON",
 /*  92 */ "return_statement ::= RETURN SEMICOLON",
 /*  93 */ "break_statement ::= BREAK SEMICOLON",
 /*  94 */ "continue_statement ::= CONTINUE SEMICOLON",
 /*  95 */ "declaration_statement ::= function_declaration",
 /*  96 */ "declaration_statement ::= struct_declaration SEMICOLON",
 /*  97 */ "declaration_statement ::= variable_declaration SEMICOLON",
 /*  98 */ "variable_declaration ::= VAR declarator_sequence",
 /*  99 */ "declarator ::= IDENTIFIER",
 /* 100 */ "declarator ::= IDENTIFIER ASSIGN expression",
 /* 101 */ "declarator_sequence ::= declarator",
 /* 102 */ "declarator_sequence ::= declarator_sequence COMMA declarator",
 /* 103 */ "struct_declaration ::= STRUCT IDENTIFIER LBRACE RBRACE",
 /* 104 */ "struct_declaration ::= STRUCT IDENTIFIER LBRACE struct_members RBRACE",
 /* 105 */ "struct_member ::= variable_declaration SEMICOLON",
 /* 106 */ "struct_members ::= struct_member",
 /* 107 */ "struct_members ::= struct_members struct_member",
 /* 108 */ "function_declaration ::= FUNCTION IDENTIFIER LPAREN parameter_list RPAREN function_body",
 /* 109 */ "function_declaration ::= FUNCTION IDENTIFIER LPAREN RPAREN function_body",
 /* 110 */ "parameter ::= IDENTIFIER",
 /* 111 */ "opt_parameter ::= IDENTIFIER ASSIGN expression",
 /* 112 */ "parameters ::= parameter",
 /* 113 */ "parameters ::= parameters COMMA parameter",
 /* 114 */ "opt_parameters ::= opt_parameter",
 /* 115 */ "opt_parameters ::= opt_parameters COMMA opt_parameter",
 /* 116 */ "parameter_list ::= parameters",
 /* 117 */ "parameter_list ::= opt_parameters",
 /* 118 */ "parameter_list ::= parameters COMMA opt_parameters",
 /* 119 */ "function_body ::= statement",
 /* 120 */ "for_statement ::= FOR LPAREN for_init_statement expression SEMICOLON expression_opt RPAREN statement",
 /* 121 */ "for_init_statement ::= expression_statement",
 /* 122 */ "for_init_statement ::= variable_declaration SEMICOLON",
 /* 123 */ "foreach_statement ::= FOR LPAREN id_expression IN expression RPAREN statement",
 /* 124 */ "foreach_statement ::= FOR LPAREN foreach_decl IN expression RPAREN statement",
 /* 125 */ "foreach_decl ::= VAR IDENTIFIER",
 /* 126 */ "if_statement ::= IF LPAREN expression RPAREN statement",
 /* 127 */ "if_statement ::= IF LPAREN expression RPAREN statement ELSE statement",
 /* 128 */ "while_statement ::= WHILE LPAREN expression RPAREN statement",
 /* 129 */ "switch_statement ::= SWITCH LPAREN expression RPAREN LBRACE switch_body RBRACE",
 /* 130 */ "switch_body ::=",
 /* 131 */ "switch_body ::= switch_body switch_case",
 /* 132 */ "switch_body ::= switch_body default_case",
 /* 133 */ "switch_case ::= CASE literal COLON case_statements",
 /* 134 */ "default_case ::= DEFAULT COLON case_statements",
 /* 135 */ "case_statements ::= statement_sequence",
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
  { 67, 1 },
  { 68, 1 },
  { 70, 1 },
  { 70, 2 },
  { 69, 0 },
  { 69, 1 },
  { 71, 1 },
  { 71, 1 },
  { 71, 1 },
  { 71, 1 },
  { 71, 1 },
  { 71, 1 },
  { 71, 1 },
  { 71, 1 },
  { 71, 1 },
  { 71, 1 },
  { 71, 1 },
  { 71, 1 },
  { 71, 1 },
  { 84, 1 },
  { 86, 0 },
  { 86, 1 },
  { 85, 1 },
  { 85, 3 },
  { 89, 1 },
  { 89, 1 },
  { 89, 1 },
  { 89, 1 },
  { 89, 1 },
  { 89, 1 },
  { 87, 1 },
  { 87, 5 },
  { 88, 1 },
  { 88, 3 },
  { 88, 3 },
  { 88, 3 },
  { 88, 3 },
  { 88, 3 },
  { 88, 3 },
  { 88, 3 },
  { 88, 3 },
  { 88, 3 },
  { 88, 3 },
  { 88, 3 },
  { 88, 3 },
  { 88, 3 },
  { 88, 3 },
  { 88, 3 },
  { 88, 3 },
  { 88, 3 },
  { 88, 3 },
  { 90, 1 },
  { 90, 2 },
  { 90, 2 },
  { 90, 2 },
  { 90, 2 },
  { 90, 1 },
  { 91, 1 },
  { 91, 2 },
  { 91, 2 },
  { 91, 3 },
  { 91, 4 },
  { 91, 3 },
  { 91, 4 },
  { 93, 1 },
  { 93, 1 },
  { 93, 3 },
  { 93, 1 },
  { 95, 1 },
  { 95, 1 },
  { 95, 1 },
  { 95, 1 },
  { 95, 1 },
  { 95, 1 },
  { 95, 1 },
  { 95, 1 },
  { 95, 1 },
  { 96, 1 },
  { 97, 3 },
  { 98, 1 },
  { 98, 3 },
  { 99, 1 },
  { 92, 2 },
  { 100, 1 },
  { 94, 1 },
  { 94, 3 },
  { 73, 1 },
  { 73, 2 },
  { 76, 2 },
  { 76, 3 },
  { 72, 3 },
  { 80, 3 },
  { 80, 2 },
  { 82, 2 },
  { 83, 2 },
  { 74, 1 },
  { 74, 2 },
  { 74, 2 },
  { 103, 2 },
  { 105, 1 },
  { 105, 3 },
  { 104, 1 },
  { 104, 3 },
  { 102, 4 },
  { 102, 5 },
  { 107, 2 },
  { 106, 1 },
  { 106, 2 },
  { 101, 6 },
  { 101, 5 },
  { 110, 1 },
  { 111, 3 },
  { 112, 1 },
  { 112, 3 },
  { 113, 1 },
  { 113, 3 },
  { 108, 1 },
  { 108, 1 },
  { 108, 3 },
  { 109, 1 },
  { 75, 8 },
  { 114, 1 },
  { 114, 2 },
  { 79, 7 },
  { 79, 7 },
  { 115, 2 },
  { 77, 5 },
  { 77, 7 },
  { 78, 5 },
  { 81, 7 },
  { 116, 0 },
  { 116, 2 },
  { 116, 2 },
  { 117, 4 },
  { 118, 3 },
  { 119, 1 },
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
{ p->SetRoot(yymsp[0].minor.yy151); }
#line 1198 "astgen.c"
        break;
      case 1:
#line 78 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(translation_unit, yymsp[0].minor.yy151); }
#line 1203 "astgen.c"
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
      case 64:
      case 65:
      case 67:
      case 84:
      case 95:
      case 98:
      case 101:
      case 106:
      case 112:
      case 114:
      case 116:
      case 117:
      case 119:
      case 121:
      case 135:
#line 81 "astgen.in"
{ yygotominor.yy151 = yymsp[0].minor.yy151; }
#line 1244 "astgen.c"
        break;
      case 3:
#line 82 "astgen.in"
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
#line 1258 "astgen.c"
        break;
      case 4:
      case 20:
#line 95 "astgen.in"
{ yygotominor.yy151 = 0; }
#line 1264 "astgen.c"
        break;
      case 18:
      case 86:
#line 113 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(empty_statement); }
#line 1270 "astgen.c"
        break;
      case 23:
#line 129 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(assignment_expression, yymsp[-1].minor.yy174, yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1275 "astgen.c"
        break;
      case 24:
#line 133 "astgen.in"
{ yygotominor.yy174 = op_assign; }
#line 1280 "astgen.c"
        break;
      case 25:
#line 134 "astgen.in"
{ yygotominor.yy174 = op_assadd; }
#line 1285 "astgen.c"
        break;
      case 26:
#line 135 "astgen.in"
{ yygotominor.yy174 = op_asssub; }
#line 1290 "astgen.c"
        break;
      case 27:
#line 136 "astgen.in"
{ yygotominor.yy174 = op_assmul; }
#line 1295 "astgen.c"
        break;
      case 28:
#line 137 "astgen.in"
{ yygotominor.yy174 = op_assdiv; }
#line 1300 "astgen.c"
        break;
      case 29:
#line 138 "astgen.in"
{ yygotominor.yy174 = op_assmod; }
#line 1305 "astgen.c"
        break;
      case 31:
#line 142 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(ternary_expression, yymsp[-4].minor.yy151, yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1310 "astgen.c"
        break;
      case 33:
#line 146 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(binary_expression, op_logor,   yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1315 "astgen.c"
        break;
      case 34:
#line 147 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(binary_expression, op_logand,  yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1320 "astgen.c"
        break;
      case 35:
#line 148 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(binary_expression, op_bitor,   yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1325 "astgen.c"
        break;
      case 36:
#line 149 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(binary_expression, op_bitxor,  yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1330 "astgen.c"
        break;
      case 37:
#line 150 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(binary_expression, op_bitand,  yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1335 "astgen.c"
        break;
      case 38:
#line 151 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(binary_expression, op_eq,   yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1340 "astgen.c"
        break;
      case 39:
#line 152 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(binary_expression, op_ne,   yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1345 "astgen.c"
        break;
      case 40:
#line 153 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(binary_expression, op_lt,   yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1350 "astgen.c"
        break;
      case 41:
#line 154 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(binary_expression, op_le,   yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1355 "astgen.c"
        break;
      case 42:
#line 155 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(binary_expression, op_gt,   yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1360 "astgen.c"
        break;
      case 43:
#line 156 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(binary_expression, op_ge,   yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1365 "astgen.c"
        break;
      case 44:
#line 157 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(binary_expression, op_add,  yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1370 "astgen.c"
        break;
      case 45:
#line 158 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(binary_expression, op_sub,  yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1375 "astgen.c"
        break;
      case 46:
#line 159 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(binary_expression, op_mul,  yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1380 "astgen.c"
        break;
      case 47:
#line 160 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(binary_expression, op_div,  yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1385 "astgen.c"
        break;
      case 48:
#line 161 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(binary_expression, op_mod,  yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1390 "astgen.c"
        break;
      case 49:
#line 162 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(binary_expression, op_seq,  yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1395 "astgen.c"
        break;
      case 50:
#line 163 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(binary_expression, op_sne,  yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1400 "astgen.c"
        break;
      case 52:
#line 167 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(prefix_expression, op_negate, yymsp[0].minor.yy151); }
#line 1405 "astgen.c"
        break;
      case 53:
#line 168 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(prefix_expression, op_preinc, yymsp[0].minor.yy151); }
#line 1410 "astgen.c"
        break;
      case 54:
#line 169 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(prefix_expression, op_predec, yymsp[0].minor.yy151); }
#line 1415 "astgen.c"
        break;
      case 55:
#line 170 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(prefix_expression, op_not,    yymsp[0].minor.yy151); }
#line 1420 "astgen.c"
        break;
      case 58:
#line 175 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(postfix_expression, op_postinc, yymsp[-1].minor.yy151); }
#line 1425 "astgen.c"
        break;
      case 59:
#line 176 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(postfix_expression, op_postdec, yymsp[-1].minor.yy151); }
#line 1430 "astgen.c"
        break;
      case 60:
#line 177 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(member_expression, yymsp[-2].minor.yy151, String(yymsp[0].minor.yy0)); }
#line 1435 "astgen.c"
        break;
      case 61:
#line 178 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(index_expression, yymsp[-3].minor.yy151, yymsp[-1].minor.yy151); }
#line 1440 "astgen.c"
        break;
      case 62:
#line 179 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(function_call, String(yymsp[-2].minor.yy0)); }
#line 1445 "astgen.c"
        break;
      case 63:
#line 180 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(function_call, String(yymsp[-3].minor.yy0), yymsp[-1].minor.yy151); }
#line 1450 "astgen.c"
        break;
      case 66:
      case 96:
      case 97:
      case 105:
      case 122:
#line 185 "astgen.in"
{ yygotominor.yy151 = yymsp[-1].minor.yy151; }
#line 1459 "astgen.c"
        break;
      case 68:
#line 189 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(literal, Variant(String(yymsp[0].minor.yy0), Variant::stInt)); }
#line 1464 "astgen.c"
        break;
      case 69:
#line 190 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(literal, Variant(hex2dec(String(yymsp[0].minor.yy0).c_str()))); }
#line 1469 "astgen.c"
        break;
      case 70:
#line 191 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(literal, Variant(bin2dec(String(yymsp[0].minor.yy0).c_str()))); }
#line 1474 "astgen.c"
        break;
      case 71:
#line 192 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(literal, Variant(rom2dec(String(yymsp[0].minor.yy0).c_str()))); }
#line 1479 "astgen.c"
        break;
      case 72:
#line 193 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(literal, Variant(String(yymsp[0].minor.yy0), Variant::stReal)); }
#line 1484 "astgen.c"
        break;
      case 73:
#line 194 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(literal, Variant(String(yymsp[0].minor.yy0), Variant::stString)); }
#line 1489 "astgen.c"
        break;
      case 74:
#line 195 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(literal, Variant(true));    }
#line 1494 "astgen.c"
        break;
      case 75:
#line 196 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(literal, Variant(false));   }
#line 1499 "astgen.c"
        break;
      case 76:
#line 197 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(literal, Variant());        }
#line 1504 "astgen.c"
        break;
      case 77:
#line 200 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(lvalue, String(yymsp[0].minor.yy0)); }
#line 1509 "astgen.c"
        break;
      case 78:
#line 203 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(list_literal, yymsp[-1].minor.yy151); }
#line 1514 "astgen.c"
        break;
      case 79:
#line 204 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(list_content, yymsp[0].minor.yy151); }
#line 1519 "astgen.c"
        break;
      case 80:
#line 205 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(list_content, yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1524 "astgen.c"
        break;
      case 81:
#line 206 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(list_entry, yymsp[0].minor.yy151); }
#line 1529 "astgen.c"
        break;
      case 82:
#line 209 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(new_expression, String(yymsp[0].minor.yy0)); }
#line 1534 "astgen.c"
        break;
      case 83:
#line 218 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(argument, yymsp[0].minor.yy151); }
#line 1539 "astgen.c"
        break;
      case 85:
#line 222 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(argument_list, yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1544 "astgen.c"
        break;
      case 87:
#line 231 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(expression_statement, yymsp[-1].minor.yy151); }
#line 1549 "astgen.c"
        break;
      case 88:
#line 234 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(compound_statement); }
#line 1554 "astgen.c"
        break;
      case 89:
#line 235 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(compound_statement, yymsp[-1].minor.yy151); }
#line 1559 "astgen.c"
        break;
      case 90:
#line 238 "astgen.in"
{ p->Parse(yymsp[-1].minor.yy0); yygotominor.yy151 = p->GetRoot(); }
#line 1564 "astgen.c"
        break;
      case 91:
#line 241 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(return_statement, yymsp[-1].minor.yy151); }
#line 1569 "astgen.c"
        break;
      case 92:
#line 242 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(return_statement);    }
#line 1574 "astgen.c"
        break;
      case 93:
#line 245 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(break_statement); }
#line 1579 "astgen.c"
        break;
      case 94:
#line 246 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(continue_statement); }
#line 1584 "astgen.c"
        break;
      case 99:
#line 260 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(variable_declaration, String(yymsp[0].minor.yy0));    }
#line 1589 "astgen.c"
        break;
      case 100:
#line 261 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(variable_declaration, String(yymsp[-2].minor.yy0), yymsp[0].minor.yy151); }
#line 1594 "astgen.c"
        break;
      case 102:
#line 264 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(declaration_sequence, yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1599 "astgen.c"
        break;
      case 103:
#line 273 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(struct_declaration, String(yymsp[-2].minor.yy0)); }
#line 1604 "astgen.c"
        break;
      case 104:
#line 274 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(struct_declaration, String(yymsp[-3].minor.yy0), yymsp[-1].minor.yy151); }
#line 1609 "astgen.c"
        break;
      case 107:
#line 281 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(struct_members, yymsp[-1].minor.yy151, yymsp[0].minor.yy151); }
#line 1614 "astgen.c"
        break;
      case 108:
#line 289 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(function_declaration, String(yymsp[-4].minor.yy0), yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1619 "astgen.c"
        break;
      case 109:
#line 290 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(function_declaration, String(yymsp[-3].minor.yy0), AstData(), yymsp[0].minor.yy151); }
#line 1624 "astgen.c"
        break;
      case 110:
#line 293 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(parameter, String(yymsp[0].minor.yy0)); }
#line 1629 "astgen.c"
        break;
      case 111:
#line 296 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(parameter, String(yymsp[-2].minor.yy0), yymsp[0].minor.yy151); }
#line 1634 "astgen.c"
        break;
      case 113:
      case 115:
      case 118:
#line 300 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(parameter_list, yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1641 "astgen.c"
        break;
      case 120:
#line 321 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(for_statement, yymsp[-5].minor.yy151, yymsp[-4].minor.yy151, yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1646 "astgen.c"
        break;
      case 123:
      case 124:
#line 332 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(foreach_statement, yymsp[-4].minor.yy151, yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1652 "astgen.c"
        break;
      case 125:
#line 334 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(variable_declaration, String(yymsp[0].minor.yy0)); }
#line 1657 "astgen.c"
        break;
      case 126:
#line 345 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(if_statement, yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1662 "astgen.c"
        break;
      case 127:
#line 346 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(if_statement, yymsp[-4].minor.yy151, yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1667 "astgen.c"
        break;
      case 128:
#line 354 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(while_statement, yymsp[-2].minor.yy151,  yymsp[0].minor.yy151); }
#line 1672 "astgen.c"
        break;
      case 129:
#line 362 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(switch_statement, yymsp[-4].minor.yy151, yymsp[-1].minor.yy55); }
#line 1677 "astgen.c"
        break;
      case 130:
#line 366 "astgen.in"
{ yygotominor.yy55 = new AstList; }
#line 1682 "astgen.c"
        break;
      case 131:
      case 132:
#line 367 "astgen.in"
{ yygotominor.yy55 = yymsp[-1].minor.yy55; yygotominor.yy55->push_back(yymsp[0].minor.yy151); }
#line 1688 "astgen.c"
        break;
      case 133:
#line 371 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(switch_case, yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1693 "astgen.c"
        break;
      case 134:
#line 374 "astgen.in"
{ yygotominor.yy151 = p->AllocAst(default_case, yymsp[0].minor.yy151); }
#line 1698 "astgen.c"
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
#line 1746 "astgen.c"
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
#line 1764 "astgen.c"
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

