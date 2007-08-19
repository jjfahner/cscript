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
#define YYNOCODE 120
#define YYACTIONTYPE unsigned short int
#define CScriptParseTOKENTYPE  Token 
typedef union {
  CScriptParseTOKENTYPE yy0;
  AstList* yy1;
  opcodes yy26;
  Ast* yy235;
  int yy239;
} YYMINORTYPE;
#ifndef YYSTACKDEPTH
#define YYSTACKDEPTH 100
#endif
#define CScriptParseARG_SDECL  Parser* p ;
#define CScriptParseARG_PDECL , Parser* p 
#define CScriptParseARG_FETCH  Parser* p  = yypParser->p 
#define CScriptParseARG_STORE yypParser->p  = p 
#define YYNSTATE 231
#define YYNRULE 134
#define YYERRORSYMBOL 66
#define YYERRSYMDT yy239
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
 /*     0 */   202,  366,  132,  227,    9,  226,  222,  221,  218,  217,
 /*    10 */   215,  213,  210,  209,  207,  206,  205,  203,  123,  200,
 /*    20 */   130,  199,   61,  194,  197,   79,  185,  184,   51,  180,
 /*    30 */   179,  177,   46,   47,   37,  139,   92,   96,   62,  104,
 /*    40 */    52,   53,   54,  157,   86,   16,  178,   23,   60,  176,
 /*    50 */   174,  173,  172,  171,  170,  169,  166,  165,  122,   97,
 /*    60 */   129,    3,  230,  124,   20,  113,   85,   60,  121,   95,
 /*    70 */    87,   51,  101,  151,  117,   89,  107,   44,   45,   46,
 /*    80 */    47,   37,  128,   52,   53,   54,  155,   86,   16,  159,
 /*    90 */    23,   60,  176,  174,  173,  172,  171,  170,  169,  166,
 /*   100 */   165,  112,   97,  129,    3,  127,  124,   20,  113,   85,
 /*   110 */    60,  121,   95,   87,  195,  196,  202,  117,   89,  107,
 /*   120 */    13,  226,  222,  221,  218,  217,  215,  213,  210,  209,
 /*   130 */   207,  206,  205,  203,  123,  200,   64,  199,   61,  181,
 /*   140 */   197,   79,  185,  184,   26,  180,  179,  177,   51,   17,
 /*   150 */   133,  139,   92,   96,  103,   58,  220,  224,  114,  115,
 /*   160 */    52,   53,   54,   22,   86,   16,  228,   23,  204,  176,
 /*   170 */   174,  173,  172,  171,  170,  169,  166,  165,  119,   97,
 /*   180 */   129,    3,  160,  124,   20,  113,   85,   60,  121,   95,
 /*   190 */    87,   57,  125,  202,  117,   89,  107,   13,  226,  222,
 /*   200 */   221,  218,  217,  215,  213,  210,  209,  207,  206,  205,
 /*   210 */   203,  123,  200,  136,  199,   61,  150,  197,   79,  185,
 /*   220 */   184,  112,  180,  179,  177,    5,  105,  100,  139,   92,
 /*   230 */    96,  208,  200,  108,  199,   61,   24,  197,   79,  185,
 /*   240 */   184,  202,  180,  179,  177,  201,  168,  222,  221,  218,
 /*   250 */   217,  215,  213,  210,  209,  207,  206,  205,  203,  123,
 /*   260 */   200,  214,  199,   61,  148,  197,   79,  185,  184,   32,
 /*   270 */   180,  179,  177,  164,   12,   91,  139,   92,   96,  223,
 /*   280 */   224,  202,  118,   28,  216,   18,  168,  222,  221,  218,
 /*   290 */   217,  215,  213,  210,  209,  207,  206,  205,  203,  123,
 /*   300 */   200,   94,  199,   61,   19,  197,   79,  185,  184,  138,
 /*   310 */   180,  179,  177,   27,   15,    1,  139,   92,   96,  119,
 /*   320 */    14,  202,   59,  161,  167,    6,  226,  222,  221,  218,
 /*   330 */   217,  215,  213,  210,  209,  207,  206,  205,  203,  123,
 /*   340 */   200,    4,  199,   61,   31,  197,   79,  185,  184,   21,
 /*   350 */   180,  179,  177,   11,  202,   63,  139,   92,   96,  102,
 /*   360 */   222,  221,  218,  217,  215,  213,  210,  209,  207,  206,
 /*   370 */   205,  203,  123,  200,  191,  199,   61,    2,  197,   79,
 /*   380 */   185,  184,   56,  180,  179,  177,  141,    8,  202,  139,
 /*   390 */    92,   96,    7,  225,  222,  221,  218,  217,  215,  213,
 /*   400 */   210,  209,  207,  206,  205,  203,  123,  200,   55,  199,
 /*   410 */    61,  158,  197,   79,  185,  184,   65,  180,  179,  177,
 /*   420 */   143,  202,   93,  139,   92,   96,  182,  222,  221,  218,
 /*   430 */   217,  215,  213,  210,  209,  207,  206,  205,  203,  123,
 /*   440 */   200,   10,  199,   61,  367,  197,   79,  185,  184,  367,
 /*   450 */   180,  179,  177,  367,  367,  202,  139,   92,   96,  367,
 /*   460 */   175,  222,  221,  218,  217,  215,  213,  210,  209,  207,
 /*   470 */   206,  205,  203,  123,  200,  367,  199,   61,  367,  197,
 /*   480 */    79,  185,  184,  367,  180,  179,  177,  367,  202,  367,
 /*   490 */   139,   92,   96,  211,  222,  221,  218,  217,  215,  213,
 /*   500 */   210,  209,  207,  206,  205,  203,  123,  200,  367,  199,
 /*   510 */    61,  367,  197,   79,  185,  184,  367,  180,  179,  177,
 /*   520 */   367,  367,  202,  139,   92,   96,  367,  187,  222,  221,
 /*   530 */   218,  217,  215,  213,  210,  209,  207,  206,  205,  203,
 /*   540 */   123,  200,  367,  199,   61,  367,  197,   79,  185,  184,
 /*   550 */   367,  180,  179,  177,  367,  367,  367,  139,   92,   96,
 /*   560 */    40,   33,   34,   35,   50,   48,   49,   36,   38,   39,
 /*   570 */    41,   42,   43,   44,   45,   46,   47,   37,  147,  146,
 /*   580 */   145,  144,  142,  140,   29,   51,   36,   38,   39,   41,
 /*   590 */    42,   43,   44,   45,   46,   47,   37,   52,   53,   54,
 /*   600 */   367,   86,   16,  367,   23,  367,  176,  174,  173,  172,
 /*   610 */   171,  170,  169,  166,  165,   51,   97,  129,  193,  192,
 /*   620 */   367,   90,  367,   30,   60,  367,  367,   52,   53,   54,
 /*   630 */   367,   86,   16,  367,   23,  183,  176,  174,  173,  172,
 /*   640 */   171,  170,  169,  166,  165,   51,   97,  176,  174,  173,
 /*   650 */   172,  171,  170,  169,  166,  165,  367,   52,   53,   54,
 /*   660 */   367,   86,   16,  367,   23,  367,  176,  174,  173,  172,
 /*   670 */   171,  170,  169,  166,  165,   51,   97,  134,   39,   41,
 /*   680 */    42,   43,   44,   45,   46,   47,   37,   52,   53,   54,
 /*   690 */   367,   86,   16,  367,   23,  367,  176,  174,  173,  172,
 /*   700 */   171,  170,  169,  166,  165,  367,   97,   33,   34,   35,
 /*   710 */    50,   48,   49,   36,   38,   39,   41,   42,   43,   44,
 /*   720 */    45,   46,   47,   37,   34,   35,   50,   48,   49,   36,
 /*   730 */    38,   39,   41,   42,   43,   44,   45,   46,   47,   37,
 /*   740 */    35,   50,   48,   49,   36,   38,   39,   41,   42,   43,
 /*   750 */    44,   45,   46,   47,   37,  367,  162,  200,  367,  199,
 /*   760 */    61,  212,  197,   79,  185,  184,  367,  180,  179,  177,
 /*   770 */    82,  156,  123,  200,  367,  199,   61,  367,  197,   79,
 /*   780 */   185,  184,  367,  180,  111,  177,  162,  200,  367,  199,
 /*   790 */    61,  110,  197,   79,  185,  184,  367,  180,  179,  177,
 /*   800 */   367,  163,   25,  367,  367,  367,  367,  367,   50,   48,
 /*   810 */    49,   36,   38,   39,   41,   42,   43,   44,   45,   46,
 /*   820 */    47,   37,  154,  200,  367,  199,   61,  367,  197,   79,
 /*   830 */   185,  184,   83,  180,  179,  177,  367,  367,  152,   48,
 /*   840 */    49,   36,   38,   39,   41,   42,   43,   44,   45,   46,
 /*   850 */    47,   37,  154,  200,  367,  199,   61,  367,  197,   79,
 /*   860 */   185,  184,  367,  180,  179,  177,  120,  200,  153,  199,
 /*   870 */    61,  367,  197,   79,  185,  184,  367,  180,  179,  177,
 /*   880 */   106,  200,  367,  199,   61,  367,  197,   79,  185,  184,
 /*   890 */   367,  180,  179,  177,  219,  200,  367,  199,   61,  367,
 /*   900 */   197,   79,  185,  184,  367,  180,  179,  177,  109,  200,
 /*   910 */   367,  199,   61,  367,  197,   79,  185,  184,  367,  180,
 /*   920 */   179,  177,  367,  116,  200,  367,  199,   61,  367,  197,
 /*   930 */    79,  185,  184,  367,  180,  179,  177,   88,  200,  367,
 /*   940 */   199,   61,  367,  197,   79,  185,  184,  367,  180,  179,
 /*   950 */   177,  126,  200,  367,  199,   61,  367,  197,   79,  185,
 /*   960 */   184,  367,  180,  179,  177,   84,  200,  367,  199,   61,
 /*   970 */   367,  197,   79,  185,  184,  367,  180,  179,  177,   99,
 /*   980 */   200,  367,  199,   61,  367,  197,   79,  185,  184,  367,
 /*   990 */   180,  179,  177,  149,  200,  367,  199,   61,  367,  197,
 /*  1000 */    79,  185,  184,  367,  180,  179,  177,  367,   98,  200,
 /*  1010 */   367,  199,   61,  367,  197,   79,  185,  184,  367,  180,
 /*  1020 */   179,  177,  137,  367,  199,   61,  367,  197,   79,  185,
 /*  1030 */   184,  367,  180,  179,  177,  198,  367,  199,   61,  367,
 /*  1040 */   197,   79,  185,  184,  367,  180,  179,  177,   71,  367,
 /*  1050 */   197,   79,  185,  184,  367,  180,  179,  177,   72,  367,
 /*  1060 */   197,   79,  185,  184,  367,  180,  179,  177,   70,  367,
 /*  1070 */   197,   79,  185,  184,  367,  180,  179,  177,  367,  131,
 /*  1080 */   367,  197,   79,  185,  184,  367,  180,  179,  177,  135,
 /*  1090 */   367,  197,   79,  185,  184,  367,  180,  179,  177,  367,
 /*  1100 */    81,  367,  197,   79,  185,  184,  367,  180,  179,  177,
 /*  1110 */    80,  367,  197,   79,  185,  184,  367,  180,  179,  177,
 /*  1120 */   367,   78,  367,  197,   79,  185,  184,  367,  180,  179,
 /*  1130 */   177,   75,  367,  197,   79,  185,  184,  367,  180,  179,
 /*  1140 */   177,   76,  367,  197,   79,  185,  184,  367,  180,  179,
 /*  1150 */   177,   66,  367,  197,   79,  185,  184,  367,  180,  179,
 /*  1160 */   177,   67,  367,  197,   79,  185,  184,  367,  180,  179,
 /*  1170 */   177,   77,  367,  197,   79,  185,  184,  367,  180,  179,
 /*  1180 */   177,   68,  367,  197,   79,  185,  184,  367,  180,  179,
 /*  1190 */   177,   73,  367,  197,   79,  185,  184,  367,  180,  179,
 /*  1200 */   177,   69,  367,  197,   79,  185,  184,  367,  180,  179,
 /*  1210 */   177,  367,  229,  367,  197,   79,  185,  184,  367,  180,
 /*  1220 */   179,  177,   74,  367,  197,   79,  185,  184,  367,  180,
 /*  1230 */   179,  177,  188,   79,  185,  184,  367,  180,  179,  177,
 /*  1240 */   189,   79,  185,  184,  367,  180,  179,  177,  367,  190,
 /*  1250 */    79,  185,  184,  367,  180,  179,  177,  186,   79,  185,
 /*  1260 */   184,  367,  180,  179,  177,
};
static const YYCODETYPE yy_lookahead[] = {
 /*     0 */    66,   67,   68,   69,   70,   71,   72,   73,   74,   75,
 /*    10 */    76,   77,   78,   79,   80,   81,   82,   83,   84,   85,
 /*    20 */    47,   87,   88,   49,   90,   91,   92,   93,   15,   95,
 /*    30 */    96,   97,   16,   17,   18,  101,  102,  103,   64,   65,
 /*    40 */    27,   28,   29,   49,   31,   32,   35,   34,   54,   36,
 /*    50 */    37,   38,   39,   40,   41,   42,   43,   44,   41,   46,
 /*    60 */    47,   48,   49,   50,   51,   52,   53,   54,   55,   56,
 /*    70 */    57,   15,  104,  105,   61,   62,   63,   14,   15,   16,
 /*    80 */    17,   18,   47,   27,   28,   29,   49,   31,   32,   47,
 /*    90 */    34,   54,   36,   37,   38,   39,   40,   41,   42,   43,
 /*   100 */    44,   31,   46,   47,   48,   49,   50,   51,   52,   53,
 /*   110 */    54,   55,   56,   57,  116,  117,   66,   61,   62,   63,
 /*   120 */    70,   71,   72,   73,   74,   75,   76,   77,   78,   79,
 /*   130 */    80,   81,   82,   83,   84,   85,   45,   87,   88,   35,
 /*   140 */    90,   91,   92,   93,   34,   95,   96,   97,   15,   45,
 /*   150 */    47,  101,  102,  103,  108,  115,  110,  111,  112,  113,
 /*   160 */    27,   28,   29,   19,   31,   32,  111,   34,  118,   36,
 /*   170 */    37,   38,   39,   40,   41,   42,   43,   44,  103,   46,
 /*   180 */    47,   48,  107,   50,   51,   52,   53,   54,   55,   56,
 /*   190 */    57,   45,   31,   66,   61,   62,   63,   70,   71,   72,
 /*   200 */    73,   74,   75,   76,   77,   78,   79,   80,   81,   82,
 /*   210 */    83,   84,   85,   47,   87,   88,  105,   90,   91,   92,
 /*   220 */    93,   31,   95,   96,   97,   35,   31,   95,  101,  102,
 /*   230 */   103,   84,   85,   86,   87,   88,   58,   90,   91,   92,
 /*   240 */    93,   66,   95,   96,   97,  118,   71,   72,   73,   74,
 /*   250 */    75,   76,   77,   78,   79,   80,   81,   82,   83,   84,
 /*   260 */    85,   47,   87,   88,   33,   90,   91,   92,   93,   89,
 /*   270 */    95,   96,   97,   33,   35,   31,  101,  102,  103,  110,
 /*   280 */   111,   66,  113,   34,  109,   45,   71,   72,   73,   74,
 /*   290 */    75,   76,   77,   78,   79,   80,   81,   82,   83,   84,
 /*   300 */    85,   35,   87,   88,   47,   90,   91,   92,   93,   47,
 /*   310 */    95,   96,   97,   19,   34,   26,  101,  102,  103,  103,
 /*   320 */    34,   66,  106,  107,  109,   70,   71,   72,   73,   74,
 /*   330 */    75,   76,   77,   78,   79,   80,   81,   82,   83,   84,
 /*   340 */    85,   35,   87,   88,   26,   90,   91,   92,   93,   34,
 /*   350 */    95,   96,   97,   60,   66,   45,  101,  102,  103,   71,
 /*   360 */    72,   73,   74,   75,   76,   77,   78,   79,   80,   81,
 /*   370 */    82,   83,   84,   85,   31,   87,   88,   26,   90,   91,
 /*   380 */    92,   93,   48,   95,   96,   97,   47,   35,   66,  101,
 /*   390 */   102,  103,   35,   71,   72,   73,   74,   75,   76,   77,
 /*   400 */    78,   79,   80,   81,   82,   83,   84,   85,   34,   87,
 /*   410 */    88,   31,   90,   91,   92,   93,   48,   95,   96,   97,
 /*   420 */    47,   66,   31,  101,  102,  103,   71,   72,   73,   74,
 /*   430 */    75,   76,   77,   78,   79,   80,   81,   82,   83,   84,
 /*   440 */    85,   35,   87,   88,  119,   90,   91,   92,   93,  119,
 /*   450 */    95,   96,   97,  119,  119,   66,  101,  102,  103,  119,
 /*   460 */    71,   72,   73,   74,   75,   76,   77,   78,   79,   80,
 /*   470 */    81,   82,   83,   84,   85,  119,   87,   88,  119,   90,
 /*   480 */    91,   92,   93,  119,   95,   96,   97,  119,   66,  119,
 /*   490 */   101,  102,  103,   71,   72,   73,   74,   75,   76,   77,
 /*   500 */    78,   79,   80,   81,   82,   83,   84,   85,  119,   87,
 /*   510 */    88,  119,   90,   91,   92,   93,  119,   95,   96,   97,
 /*   520 */   119,  119,   66,  101,  102,  103,  119,   71,   72,   73,
 /*   530 */    74,   75,   76,   77,   78,   79,   80,   81,   82,   83,
 /*   540 */    84,   85,  119,   87,   88,  119,   90,   91,   92,   93,
 /*   550 */   119,   95,   96,   97,  119,  119,  119,  101,  102,  103,
 /*   560 */     1,    2,    3,    4,    5,    6,    7,    8,    9,   10,
 /*   570 */    11,   12,   13,   14,   15,   16,   17,   18,   19,   20,
 /*   580 */    21,   22,   23,   24,   25,   15,    8,    9,   10,   11,
 /*   590 */    12,   13,   14,   15,   16,   17,   18,   27,   28,   29,
 /*   600 */   119,   31,   32,  119,   34,  119,   36,   37,   38,   39,
 /*   610 */    40,   41,   42,   43,   44,   15,   46,   47,   27,   28,
 /*   620 */   119,   30,  119,   32,   54,  119,  119,   27,   28,   29,
 /*   630 */   119,   31,   32,  119,   34,   35,   36,   37,   38,   39,
 /*   640 */    40,   41,   42,   43,   44,   15,   46,   36,   37,   38,
 /*   650 */    39,   40,   41,   42,   43,   44,  119,   27,   28,   29,
 /*   660 */   119,   31,   32,  119,   34,  119,   36,   37,   38,   39,
 /*   670 */    40,   41,   42,   43,   44,   15,   46,   47,   10,   11,
 /*   680 */    12,   13,   14,   15,   16,   17,   18,   27,   28,   29,
 /*   690 */   119,   31,   32,  119,   34,  119,   36,   37,   38,   39,
 /*   700 */    40,   41,   42,   43,   44,  119,   46,    2,    3,    4,
 /*   710 */     5,    6,    7,    8,    9,   10,   11,   12,   13,   14,
 /*   720 */    15,   16,   17,   18,    3,    4,    5,    6,    7,    8,
 /*   730 */     9,   10,   11,   12,   13,   14,   15,   16,   17,   18,
 /*   740 */     4,    5,    6,    7,    8,    9,   10,   11,   12,   13,
 /*   750 */    14,   15,   16,   17,   18,  119,   84,   85,  119,   87,
 /*   760 */    88,   73,   90,   91,   92,   93,  119,   95,   96,   97,
 /*   770 */    98,   99,   84,   85,  119,   87,   88,  119,   90,   91,
 /*   780 */    92,   93,  119,   95,   96,   97,   84,   85,  119,   87,
 /*   790 */    88,  103,   90,   91,   92,   93,  119,   95,   96,   97,
 /*   800 */   119,   99,  114,  119,  119,  119,  119,  119,    5,    6,
 /*   810 */     7,    8,    9,   10,   11,   12,   13,   14,   15,   16,
 /*   820 */    17,   18,   84,   85,  119,   87,   88,  119,   90,   91,
 /*   830 */    92,   93,   94,   95,   96,   97,  119,  119,  100,    6,
 /*   840 */     7,    8,    9,   10,   11,   12,   13,   14,   15,   16,
 /*   850 */    17,   18,   84,   85,  119,   87,   88,  119,   90,   91,
 /*   860 */    92,   93,  119,   95,   96,   97,   84,   85,  100,   87,
 /*   870 */    88,  119,   90,   91,   92,   93,  119,   95,   96,   97,
 /*   880 */    84,   85,  119,   87,   88,  119,   90,   91,   92,   93,
 /*   890 */   119,   95,   96,   97,   84,   85,  119,   87,   88,  119,
 /*   900 */    90,   91,   92,   93,  119,   95,   96,   97,   84,   85,
 /*   910 */   119,   87,   88,  119,   90,   91,   92,   93,  119,   95,
 /*   920 */    96,   97,  119,   84,   85,  119,   87,   88,  119,   90,
 /*   930 */    91,   92,   93,  119,   95,   96,   97,   84,   85,  119,
 /*   940 */    87,   88,  119,   90,   91,   92,   93,  119,   95,   96,
 /*   950 */    97,   84,   85,  119,   87,   88,  119,   90,   91,   92,
 /*   960 */    93,  119,   95,   96,   97,   84,   85,  119,   87,   88,
 /*   970 */   119,   90,   91,   92,   93,  119,   95,   96,   97,   84,
 /*   980 */    85,  119,   87,   88,  119,   90,   91,   92,   93,  119,
 /*   990 */    95,   96,   97,   84,   85,  119,   87,   88,  119,   90,
 /*  1000 */    91,   92,   93,  119,   95,   96,   97,  119,   84,   85,
 /*  1010 */   119,   87,   88,  119,   90,   91,   92,   93,  119,   95,
 /*  1020 */    96,   97,   85,  119,   87,   88,  119,   90,   91,   92,
 /*  1030 */    93,  119,   95,   96,   97,   85,  119,   87,   88,  119,
 /*  1040 */    90,   91,   92,   93,  119,   95,   96,   97,   88,  119,
 /*  1050 */    90,   91,   92,   93,  119,   95,   96,   97,   88,  119,
 /*  1060 */    90,   91,   92,   93,  119,   95,   96,   97,   88,  119,
 /*  1070 */    90,   91,   92,   93,  119,   95,   96,   97,  119,   88,
 /*  1080 */   119,   90,   91,   92,   93,  119,   95,   96,   97,   88,
 /*  1090 */   119,   90,   91,   92,   93,  119,   95,   96,   97,  119,
 /*  1100 */    88,  119,   90,   91,   92,   93,  119,   95,   96,   97,
 /*  1110 */    88,  119,   90,   91,   92,   93,  119,   95,   96,   97,
 /*  1120 */   119,   88,  119,   90,   91,   92,   93,  119,   95,   96,
 /*  1130 */    97,   88,  119,   90,   91,   92,   93,  119,   95,   96,
 /*  1140 */    97,   88,  119,   90,   91,   92,   93,  119,   95,   96,
 /*  1150 */    97,   88,  119,   90,   91,   92,   93,  119,   95,   96,
 /*  1160 */    97,   88,  119,   90,   91,   92,   93,  119,   95,   96,
 /*  1170 */    97,   88,  119,   90,   91,   92,   93,  119,   95,   96,
 /*  1180 */    97,   88,  119,   90,   91,   92,   93,  119,   95,   96,
 /*  1190 */    97,   88,  119,   90,   91,   92,   93,  119,   95,   96,
 /*  1200 */    97,   88,  119,   90,   91,   92,   93,  119,   95,   96,
 /*  1210 */    97,  119,   88,  119,   90,   91,   92,   93,  119,   95,
 /*  1220 */    96,   97,   88,  119,   90,   91,   92,   93,  119,   95,
 /*  1230 */    96,   97,   90,   91,   92,   93,  119,   95,   96,   97,
 /*  1240 */    90,   91,   92,   93,  119,   95,   96,   97,  119,   90,
 /*  1250 */    91,   92,   93,  119,   95,   96,   97,   90,   91,   92,
 /*  1260 */    93,  119,   95,   96,   97,
};
#define YY_SHIFT_USE_DFLT (-28)
#define YY_SHIFT_MAX 126
static const short yy_shift_ofst[] = {
 /*     0 */   133,  133,  133,   13,  133,  133,   56,  133,  133,  133,
 /*    10 */   133,  133,  133,  133,  570,  600,  660,  660,  660,  660,
 /*    20 */   630,  660,  660,  660,  660,  660,  660,  660,  660,  660,
 /*    30 */   660,  660,  660,  660,  660,  660,  660,  660,  660,  660,
 /*    40 */   660,  660,  660,  660,  660,  660,  660,  660,  660,  660,
 /*    50 */   660,  660,  660,  660,  660,  190,   37,   70,  -26,   -6,
 /*    60 */   195,  559,  611,  195,  161,  -28,  705,  721,  736,  803,
 /*    70 */   833,  578,  578,  668,  668,   63,   63,   63,   63,  591,
 /*    80 */    16,   16,  240,  104,  266,  262,  280,  286,  318,  315,
 /*    90 */   343,  334,  339,  374,  368,  391,  373,  380,  357,  352,
 /*   100 */   351,  310,  293,  306,  289,  294,  257,  249,  239,  231,
 /*   110 */   214,  178,  144,  166,  146,   91,  103,  110,   91,   42,
 /*   120 */    11,  244,  -27,   35,   17,  144,  406,
};
#define YY_REDUCE_USE_DFLT (-67)
#define YY_REDUCE_MAX 65
static const short yy_reduce_ofst[] = {
 /*     0 */   -66,   50,  127,  255,  215,  175,  322,  389,  456,  322,
 /*    10 */   288,  355,  422,  322,  688,  738,  672,  768,  702,  147,
 /*    20 */   839,  895,  810,  782,  924,  796,  867,  909,  881,  853,
 /*    30 */   824,  937,  950, 1073, 1093, 1113, 1134,  991, 1103, 1083,
 /*    40 */  1063, 1053, 1043, 1033, 1022, 1012, 1001, 1124,  970,  960,
 /*    50 */   980, 1159, 1150, 1142, 1167,   46,  216,  169,   -2,   75,
 /*    60 */   -32,  180,  132,  111,   55,   40,
};
static const YYACTIONTYPE yy_default[] = {
 /*     0 */   235,  365,  365,  365,  365,  365,  365,  365,  365,  236,
 /*    10 */   365,  365,  365,  364,  365,  365,  365,  365,  365,  251,
 /*    20 */   365,  365,  365,  365,  365,  365,  365,  365,  365,  365,
 /*    30 */   365,  365,  365,  365,  365,  365,  365,  365,  365,  365,
 /*    40 */   365,  365,  365,  365,  365,  365,  365,  365,  365,  365,
 /*    50 */   365,  365,  365,  365,  365,  365,  365,  365,  365,  365,
 /*    60 */   365,  261,  365,  365,  365,  359,  264,  265,  266,  267,
 /*    70 */   268,  281,  280,  270,  269,  273,  272,  271,  274,  282,
 /*    80 */   275,  276,  365,  365,  365,  365,  308,  365,  365,  365,
 /*    90 */   365,  365,  365,  365,  365,  365,  365,  365,  365,  365,
 /*   100 */   365,  329,  355,  365,  365,  330,  365,  365,  365,  365,
 /*   110 */   365,  296,  341,  365,  347,  348,  365,  365,  349,  365,
 /*   120 */   365,  365,  365,  365,  365,  365,  365,  320,  318,  317,
 /*   130 */   321,  279,  231,  322,  323,  277,  324,  262,  325,  326,
 /*   140 */   260,  327,  259,  328,  258,  257,  256,  255,  292,  331,
 /*   150 */   333,  332,  315,  316,  314,  334,  310,  335,  313,  336,
 /*   160 */   338,  337,  312,  311,  309,  307,  306,  339,  350,  305,
 /*   170 */   304,  303,  302,  301,  300,  354,  299,  298,  297,  296,
 /*   180 */   295,  294,  356,  293,  288,  287,  286,  357,  285,  284,
 /*   190 */   283,  291,  290,  289,  358,  360,  361,  263,  254,  253,
 /*   200 */   250,  362,  249,  248,  363,  247,  246,  245,  252,  244,
 /*   210 */   243,  351,  352,  242,  353,  241,  340,  240,  239,  342,
 /*   220 */   343,  238,  237,  344,  345,  234,  233,  232,  346,  278,
 /*   230 */   319,
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
 /* 124 */ "if_statement ::= IF LPAREN expression RPAREN statement",
 /* 125 */ "if_statement ::= IF LPAREN expression RPAREN statement ELSE statement",
 /* 126 */ "while_statement ::= WHILE LPAREN expression RPAREN statement",
 /* 127 */ "switch_statement ::= SWITCH LPAREN expression RPAREN LBRACE switch_body RBRACE",
 /* 128 */ "switch_body ::=",
 /* 129 */ "switch_body ::= switch_body switch_case",
 /* 130 */ "switch_body ::= switch_body default_case",
 /* 131 */ "switch_case ::= CASE literal COLON case_statements",
 /* 132 */ "default_case ::= DEFAULT COLON case_statements",
 /* 133 */ "case_statements ::= statement_sequence",
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
  { 77, 5 },
  { 77, 7 },
  { 78, 5 },
  { 81, 7 },
  { 115, 0 },
  { 115, 2 },
  { 115, 2 },
  { 116, 4 },
  { 117, 3 },
  { 118, 1 },
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
{ p->SetRoot(yymsp[0].minor.yy235); }
#line 1181 "astgen.c"
        break;
      case 1:
#line 78 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(translation_unit, yymsp[0].minor.yy235); }
#line 1186 "astgen.c"
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
      case 133:
#line 81 "astgen.in"
{ yygotominor.yy235 = yymsp[0].minor.yy235; }
#line 1227 "astgen.c"
        break;
      case 3:
#line 82 "astgen.in"
{ 
  if(yymsp[-1].minor.yy235->m_type == statement_sequence) {
    yygotominor.yy235 = yymsp[-1].minor.yy235;
  }
  else {
    yygotominor.yy235 = p->AllocAst(statement_sequence, new AstList);
    yygotominor.yy235->m_a1.GetList()->push_back(yymsp[-1].minor.yy235);
  }
  yygotominor.yy235->m_a1.GetList()->push_back(yymsp[0].minor.yy235);
}
#line 1241 "astgen.c"
        break;
      case 4:
      case 20:
#line 95 "astgen.in"
{ yygotominor.yy235 = 0; }
#line 1247 "astgen.c"
        break;
      case 18:
      case 86:
#line 113 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(empty_statement); }
#line 1253 "astgen.c"
        break;
      case 23:
#line 129 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(assignment_expression, yymsp[-1].minor.yy26, yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1258 "astgen.c"
        break;
      case 24:
#line 133 "astgen.in"
{ yygotominor.yy26 = op_assign; }
#line 1263 "astgen.c"
        break;
      case 25:
#line 134 "astgen.in"
{ yygotominor.yy26 = op_assadd; }
#line 1268 "astgen.c"
        break;
      case 26:
#line 135 "astgen.in"
{ yygotominor.yy26 = op_asssub; }
#line 1273 "astgen.c"
        break;
      case 27:
#line 136 "astgen.in"
{ yygotominor.yy26 = op_assmul; }
#line 1278 "astgen.c"
        break;
      case 28:
#line 137 "astgen.in"
{ yygotominor.yy26 = op_assdiv; }
#line 1283 "astgen.c"
        break;
      case 29:
#line 138 "astgen.in"
{ yygotominor.yy26 = op_assmod; }
#line 1288 "astgen.c"
        break;
      case 31:
#line 142 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(ternary_expression, yymsp[-4].minor.yy235, yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1293 "astgen.c"
        break;
      case 33:
#line 146 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(binary_expression, op_logor,   yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1298 "astgen.c"
        break;
      case 34:
#line 147 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(binary_expression, op_logand,  yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1303 "astgen.c"
        break;
      case 35:
#line 148 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(binary_expression, op_bitor,   yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1308 "astgen.c"
        break;
      case 36:
#line 149 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(binary_expression, op_bitxor,  yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1313 "astgen.c"
        break;
      case 37:
#line 150 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(binary_expression, op_bitand,  yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1318 "astgen.c"
        break;
      case 38:
#line 151 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(binary_expression, op_eq,   yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1323 "astgen.c"
        break;
      case 39:
#line 152 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(binary_expression, op_ne,   yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1328 "astgen.c"
        break;
      case 40:
#line 153 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(binary_expression, op_lt,   yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1333 "astgen.c"
        break;
      case 41:
#line 154 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(binary_expression, op_le,   yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1338 "astgen.c"
        break;
      case 42:
#line 155 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(binary_expression, op_gt,   yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1343 "astgen.c"
        break;
      case 43:
#line 156 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(binary_expression, op_ge,   yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1348 "astgen.c"
        break;
      case 44:
#line 157 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(binary_expression, op_add,  yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1353 "astgen.c"
        break;
      case 45:
#line 158 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(binary_expression, op_sub,  yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1358 "astgen.c"
        break;
      case 46:
#line 159 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(binary_expression, op_mul,  yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1363 "astgen.c"
        break;
      case 47:
#line 160 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(binary_expression, op_div,  yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1368 "astgen.c"
        break;
      case 48:
#line 161 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(binary_expression, op_mod,  yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1373 "astgen.c"
        break;
      case 49:
#line 162 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(binary_expression, op_seq,  yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1378 "astgen.c"
        break;
      case 50:
#line 163 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(binary_expression, op_sne,  yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1383 "astgen.c"
        break;
      case 52:
#line 167 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(prefix_expression, op_negate, yymsp[0].minor.yy235); }
#line 1388 "astgen.c"
        break;
      case 53:
#line 168 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(prefix_expression, op_preinc, yymsp[0].minor.yy235); }
#line 1393 "astgen.c"
        break;
      case 54:
#line 169 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(prefix_expression, op_predec, yymsp[0].minor.yy235); }
#line 1398 "astgen.c"
        break;
      case 55:
#line 170 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(prefix_expression, op_not,    yymsp[0].minor.yy235); }
#line 1403 "astgen.c"
        break;
      case 58:
#line 175 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(postfix_expression, op_postinc, yymsp[-1].minor.yy235); }
#line 1408 "astgen.c"
        break;
      case 59:
#line 176 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(postfix_expression, op_postdec, yymsp[-1].minor.yy235); }
#line 1413 "astgen.c"
        break;
      case 60:
#line 177 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(member_expression, yymsp[-2].minor.yy235, String(yymsp[0].minor.yy0)); }
#line 1418 "astgen.c"
        break;
      case 61:
#line 178 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(index_expression, yymsp[-3].minor.yy235, yymsp[-1].minor.yy235); }
#line 1423 "astgen.c"
        break;
      case 62:
#line 179 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(function_call, String(yymsp[-2].minor.yy0)); }
#line 1428 "astgen.c"
        break;
      case 63:
#line 180 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(function_call, String(yymsp[-3].minor.yy0), yymsp[-1].minor.yy235); }
#line 1433 "astgen.c"
        break;
      case 66:
      case 96:
      case 97:
      case 105:
      case 122:
#line 185 "astgen.in"
{ yygotominor.yy235 = yymsp[-1].minor.yy235; }
#line 1442 "astgen.c"
        break;
      case 68:
#line 189 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(literal, Variant(String(yymsp[0].minor.yy0), Variant::stInt)); }
#line 1447 "astgen.c"
        break;
      case 69:
#line 190 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(literal, Variant(hex2dec(String(yymsp[0].minor.yy0).c_str()))); }
#line 1452 "astgen.c"
        break;
      case 70:
#line 191 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(literal, Variant(bin2dec(String(yymsp[0].minor.yy0).c_str()))); }
#line 1457 "astgen.c"
        break;
      case 71:
#line 192 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(literal, Variant(rom2dec(String(yymsp[0].minor.yy0).c_str()))); }
#line 1462 "astgen.c"
        break;
      case 72:
#line 193 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(literal, Variant(String(yymsp[0].minor.yy0), Variant::stReal)); }
#line 1467 "astgen.c"
        break;
      case 73:
#line 194 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(literal, Variant(String(yymsp[0].minor.yy0), Variant::stString)); }
#line 1472 "astgen.c"
        break;
      case 74:
#line 195 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(literal, Variant(true));    }
#line 1477 "astgen.c"
        break;
      case 75:
#line 196 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(literal, Variant(false));   }
#line 1482 "astgen.c"
        break;
      case 76:
#line 197 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(literal, Variant());        }
#line 1487 "astgen.c"
        break;
      case 77:
#line 200 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(lvalue, String(yymsp[0].minor.yy0)); }
#line 1492 "astgen.c"
        break;
      case 78:
#line 203 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(list_literal, yymsp[-1].minor.yy235); }
#line 1497 "astgen.c"
        break;
      case 79:
#line 204 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(list_content, yymsp[0].minor.yy235); }
#line 1502 "astgen.c"
        break;
      case 80:
#line 205 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(list_content, yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1507 "astgen.c"
        break;
      case 81:
#line 206 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(list_entry, yymsp[0].minor.yy235); }
#line 1512 "astgen.c"
        break;
      case 82:
#line 209 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(new_expression, String(yymsp[0].minor.yy0)); }
#line 1517 "astgen.c"
        break;
      case 83:
#line 218 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(argument, yymsp[0].minor.yy235); }
#line 1522 "astgen.c"
        break;
      case 85:
#line 222 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(argument_list, yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1527 "astgen.c"
        break;
      case 87:
#line 231 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(expression_statement, yymsp[-1].minor.yy235); }
#line 1532 "astgen.c"
        break;
      case 88:
#line 234 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(compound_statement); }
#line 1537 "astgen.c"
        break;
      case 89:
#line 235 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(compound_statement, yymsp[-1].minor.yy235); }
#line 1542 "astgen.c"
        break;
      case 90:
#line 238 "astgen.in"
{ p->Parse(yymsp[-1].minor.yy0); yygotominor.yy235 = p->GetRoot(); }
#line 1547 "astgen.c"
        break;
      case 91:
#line 241 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(return_statement, yymsp[-1].minor.yy235); }
#line 1552 "astgen.c"
        break;
      case 92:
#line 242 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(return_statement);    }
#line 1557 "astgen.c"
        break;
      case 93:
#line 245 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(break_statement); }
#line 1562 "astgen.c"
        break;
      case 94:
#line 246 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(continue_statement); }
#line 1567 "astgen.c"
        break;
      case 99:
#line 260 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(variable_declaration, String(yymsp[0].minor.yy0));    }
#line 1572 "astgen.c"
        break;
      case 100:
#line 261 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(variable_declaration, String(yymsp[-2].minor.yy0), yymsp[0].minor.yy235); }
#line 1577 "astgen.c"
        break;
      case 102:
#line 264 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(declaration_sequence, yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1582 "astgen.c"
        break;
      case 103:
#line 273 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(struct_declaration, String(yymsp[-2].minor.yy0)); }
#line 1587 "astgen.c"
        break;
      case 104:
#line 274 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(struct_declaration, String(yymsp[-3].minor.yy0), yymsp[-1].minor.yy235); }
#line 1592 "astgen.c"
        break;
      case 107:
#line 281 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(struct_members, yymsp[-1].minor.yy235, yymsp[0].minor.yy235); }
#line 1597 "astgen.c"
        break;
      case 108:
#line 289 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(function_declaration, String(yymsp[-4].minor.yy0), yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1602 "astgen.c"
        break;
      case 109:
#line 290 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(function_declaration, String(yymsp[-3].minor.yy0), AstData(), yymsp[0].minor.yy235); }
#line 1607 "astgen.c"
        break;
      case 110:
#line 293 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(parameter, String(yymsp[0].minor.yy0)); }
#line 1612 "astgen.c"
        break;
      case 111:
#line 296 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(parameter, String(yymsp[-2].minor.yy0), yymsp[0].minor.yy235); }
#line 1617 "astgen.c"
        break;
      case 113:
      case 115:
      case 118:
#line 300 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(parameter_list, yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1624 "astgen.c"
        break;
      case 120:
#line 321 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(for_statement, yymsp[-5].minor.yy235, yymsp[-4].minor.yy235, yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1629 "astgen.c"
        break;
      case 123:
#line 332 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(foreach_statement, yymsp[-4].minor.yy235, yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1634 "astgen.c"
        break;
      case 124:
#line 343 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(if_statement, yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1639 "astgen.c"
        break;
      case 125:
#line 344 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(if_statement, yymsp[-4].minor.yy235, yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1644 "astgen.c"
        break;
      case 126:
#line 352 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(while_statement, yymsp[-2].minor.yy235,  yymsp[0].minor.yy235); }
#line 1649 "astgen.c"
        break;
      case 127:
#line 360 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(switch_statement, yymsp[-4].minor.yy235, yymsp[-1].minor.yy1); }
#line 1654 "astgen.c"
        break;
      case 128:
#line 364 "astgen.in"
{ yygotominor.yy1 = new AstList; }
#line 1659 "astgen.c"
        break;
      case 129:
      case 130:
#line 365 "astgen.in"
{ yygotominor.yy1 = yymsp[-1].minor.yy1; yygotominor.yy1->push_back(yymsp[0].minor.yy235); }
#line 1665 "astgen.c"
        break;
      case 131:
#line 369 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(switch_case, yymsp[-2].minor.yy235, yymsp[0].minor.yy235); }
#line 1670 "astgen.c"
        break;
      case 132:
#line 372 "astgen.in"
{ yygotominor.yy235 = p->AllocAst(default_case, yymsp[0].minor.yy235); }
#line 1675 "astgen.c"
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
#line 1723 "astgen.c"
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
#line 1741 "astgen.c"
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

