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
#define YYNOCODE 125
#define YYACTIONTYPE unsigned short int
#define CScriptParseTOKENTYPE  Token 
typedef union {
  CScriptParseTOKENTYPE yy0;
  AstList* yy39;
  opcodes yy134;
  Ast* yy175;
  int yy249;
} YYMINORTYPE;
#ifndef YYSTACKDEPTH
#define YYSTACKDEPTH 100
#endif
#define CScriptParseARG_SDECL  Parser* p ;
#define CScriptParseARG_PDECL , Parser* p 
#define CScriptParseARG_FETCH  Parser* p  = yypParser->p 
#define CScriptParseARG_STORE yypParser->p  = p 
#define YYNSTATE 251
#define YYNRULE 143
#define YYERRORSYMBOL 67
#define YYERRSYMDT yy249
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
 /*     0 */   229,  395,  149,  234,    8,  215,  150,  248,  246,  245,
 /*    10 */   244,  241,  240,  237,  236,  232,  231,  230,  123,  227,
 /*    20 */   145,  224,   66,  146,  221,   84,  210,  209,   55,  203,
 /*    30 */   202,  200,   45,   44,   41,  139,  138,  136,  134,  113,
 /*    40 */    56,   54,   53,  162,  116,   17,  142,   26,   61,  198,
 /*    50 */   197,  196,  195,  193,  192,  191,  190,  189,  143,  121,
 /*    60 */   155,    3,  250,  133,   21,  135,  137,   64,  124,  103,
 /*    70 */    90,   99,   55,  207,  208,   93,   92,   98,   47,   46,
 /*    80 */    45,   44,   41,   62,   56,   54,   53,  159,  116,   17,
 /*    90 */    68,   26,   64,  198,  197,  196,  195,  193,  192,  191,
 /*   100 */   190,  189,  147,  121,  155,    3,  148,  133,   21,  135,
 /*   110 */   137,   64,  124,  103,   90,   99,  130,  153,  229,   93,
 /*   120 */    92,   98,   11,  215,  150,  248,  246,  245,  244,  241,
 /*   130 */   240,  237,  236,  232,  231,  230,  123,  227,  188,  224,
 /*   140 */    66,  157,  221,   84,  210,  209,   64,  203,  202,  200,
 /*   150 */    20,  140,  204,  139,  138,  136,  134,  127,  227,   55,
 /*   160 */   224,   66,   19,  221,   84,  210,  209,   69,  203,  202,
 /*   170 */   200,   56,   54,   53,  213,  116,   17,  247,   26,  125,
 /*   180 */   198,  197,  196,  195,  193,  192,  191,  190,  189,   12,
 /*   190 */   121,  155,    3,   31,  133,   21,  135,  137,   64,  124,
 /*   200 */   103,   90,   99,   31,  152,  229,   93,   92,   98,   11,
 /*   210 */   215,  150,  248,  246,  245,  244,  241,  240,  237,  236,
 /*   220 */   232,  231,  230,  123,  227,   24,  224,   66,  126,  221,
 /*   230 */    84,  210,  209,  383,  203,  202,  200,  118,  122,  154,
 /*   240 */   139,  138,  136,  134,   38,   49,   51,   50,   36,   48,
 /*   250 */    47,   46,   45,   44,   41,   33,   60,   87,  229,  242,
 /*   260 */   243,  216,  112,  180,  150,  248,  246,  245,  244,  241,
 /*   270 */   240,  237,  236,  232,  231,  230,  123,  227,  184,  224,
 /*   280 */    66,  119,  221,   84,  210,  209,  119,  203,  202,  200,
 /*   290 */     5,    2,  129,  139,  138,  136,  134,   91,  220,  219,
 /*   300 */   229,  110,   32,   30,  179,  180,  150,  248,  246,  245,
 /*   310 */   244,  241,  240,  237,  236,  232,  231,  230,  123,  227,
 /*   320 */     1,  224,   66,  206,  221,   84,  210,  209,   14,  203,
 /*   330 */   202,  200,   57,   16,   22,  139,  138,  136,  134,   67,
 /*   340 */   120,   28,  161,  229,    4,  176,  235,    6,  215,  150,
 /*   350 */   248,  246,  245,  244,  241,  240,  237,  236,  232,  231,
 /*   360 */   230,  123,  227,    9,  224,   66,    7,  221,   84,  210,
 /*   370 */   209,   58,  203,  202,  200,   25,  171,  229,  139,  138,
 /*   380 */   136,  134,  199,  150,  248,  246,  245,  244,  241,  240,
 /*   390 */   237,  236,  232,  231,  230,  123,  227,  218,  224,   66,
 /*   400 */    70,  221,   84,  210,  209,   29,  203,  202,  200,   15,
 /*   410 */    10,  229,  139,  138,  136,  134,  178,  150,  248,  246,
 /*   420 */   245,  244,  241,  240,  237,  236,  232,  231,  230,  123,
 /*   430 */   227,   34,  224,   66,  228,  221,   84,  210,  209,  108,
 /*   440 */   203,  202,  200,  201,   13,  229,  139,  138,  136,  134,
 /*   450 */   225,  150,  248,  246,  245,  244,  241,  240,  237,  236,
 /*   460 */   232,  231,  230,  123,  227,  111,  224,   66,   18,  221,
 /*   470 */    84,  210,  209,  141,  203,  202,  200,  396,  396,  229,
 /*   480 */   139,  138,  136,  134,  187,  150,  248,  246,  245,  244,
 /*   490 */   241,  240,  237,  236,  232,  231,  230,  123,  227,  396,
 /*   500 */   224,   66,  396,  221,   84,  210,  209,  396,  203,  202,
 /*   510 */   200,  396,  396,  229,  139,  138,  136,  134,  194,  150,
 /*   520 */   248,  246,  245,  244,  241,  240,  237,  236,  232,  231,
 /*   530 */   230,  123,  227,  396,  224,   66,  396,  221,   84,  210,
 /*   540 */   209,  396,  203,  202,  200,  396,  396,  229,  139,  138,
 /*   550 */   136,  134,  100,  150,  248,  246,  245,  244,  241,  240,
 /*   560 */   237,  236,  232,  231,  230,  123,  227,  396,  224,   66,
 /*   570 */   396,  221,   84,  210,  209,  396,  203,  202,  200,  396,
 /*   580 */   396,  229,  139,  138,  136,  134,  233,  150,  248,  246,
 /*   590 */   245,  244,  241,  240,  237,  236,  232,  231,  230,  123,
 /*   600 */   227,  396,  224,   66,  396,  221,   84,  210,  209,  396,
 /*   610 */   203,  202,  200,  396,  396,  396,  139,  138,  136,  134,
 /*   620 */    40,   42,   43,   52,   37,   39,   35,   38,   49,   51,
 /*   630 */    50,   36,   48,   47,   46,   45,   44,   41,  175,  174,
 /*   640 */   170,  168,  166,  165,   23,   55,   51,   50,   36,   48,
 /*   650 */    47,   46,   45,   44,   41,  169,  396,   56,   54,   53,
 /*   660 */    64,  116,   17,   90,   26,  396,  198,  197,  196,  195,
 /*   670 */   193,  192,  191,  190,  189,   55,  121,  155,  114,  396,
 /*   680 */   239,  243,  131,  128,   65,  396,  396,   56,   54,   53,
 /*   690 */   396,  116,   17,  396,   26,  205,  198,  197,  196,  195,
 /*   700 */   193,  192,  191,  190,  189,   55,  121,  198,  197,  196,
 /*   710 */   195,  193,  192,  191,  190,  189,  396,   56,   54,   53,
 /*   720 */   396,  116,   17,  396,   26,  396,  198,  197,  196,  195,
 /*   730 */   193,  192,  191,  190,  189,   55,  121,  144,  214,   84,
 /*   740 */   210,  209,  396,  203,  202,  200,  396,   56,   54,   53,
 /*   750 */   396,  116,   17,  396,   26,  396,  198,  197,  196,  195,
 /*   760 */   193,  192,  191,  190,  189,  396,  121,   42,   43,   52,
 /*   770 */    37,   39,   35,   38,   49,   51,   50,   36,   48,   47,
 /*   780 */    46,   45,   44,   41,   43,   52,   37,   39,   35,   38,
 /*   790 */    49,   51,   50,   36,   48,   47,   46,   45,   44,   41,
 /*   800 */    52,   37,   39,   35,   38,   49,   51,   50,   36,   48,
 /*   810 */    47,   46,   45,   44,   41,  396,  396,  185,  227,  396,
 /*   820 */   224,   66,  226,  221,   84,  210,  209,  396,  203,  202,
 /*   830 */   200,   88,  183,  123,  227,  396,  224,   66,  396,  221,
 /*   840 */    84,  210,  209,  396,  203,  117,  200,  396,  182,  227,
 /*   850 */   396,  224,   66,  101,  221,   84,  210,  209,   89,  203,
 /*   860 */   202,  200,  172,  396,  177,   97,   27,  109,  396,  396,
 /*   870 */    59,  249,   37,   39,   35,   38,   49,   51,   50,   36,
 /*   880 */    48,   47,   46,   45,   44,   41,   39,   35,   38,   49,
 /*   890 */    51,   50,   36,   48,   47,   46,   45,   44,   41,  185,
 /*   900 */   227,  396,  224,   66,  396,  221,   84,  210,  209,  396,
 /*   910 */   203,  202,  200,  396,  186,  222,  227,   96,  224,   66,
 /*   920 */   396,  221,   84,  210,  209,  396,  203,  202,  200,  396,
 /*   930 */   182,  227,  396,  224,   66,  396,  221,   84,  210,  209,
 /*   940 */   396,  203,  202,  200,   95,  227,  181,  224,   66,  396,
 /*   950 */   221,   84,  210,  209,  396,  203,  202,  200,  115,  227,
 /*   960 */   396,  224,   66,  396,  221,   84,  210,  209,  396,  203,
 /*   970 */   202,  200,  107,  227,  396,  224,   66,  396,  221,   84,
 /*   980 */   210,  209,  396,  203,  202,  200,  105,  227,  396,  224,
 /*   990 */    66,  396,  221,   84,  210,  209,  396,  203,  202,  200,
 /*  1000 */   132,  227,  396,  224,   66,  396,  221,   84,  210,  209,
 /*  1010 */   396,  203,  202,  200,  104,  227,  396,  224,   66,  396,
 /*  1020 */   221,   84,  210,  209,  396,  203,  202,  200,  151,  227,
 /*  1030 */   396,  224,   66,  396,  221,   84,  210,  209,  396,  203,
 /*  1040 */   202,  200,  102,  227,  396,  224,   66,  396,  221,   84,
 /*  1050 */   210,  209,  396,  203,  202,  200,  238,  227,  396,  224,
 /*  1060 */    66,  396,  221,   84,  210,  209,  396,  203,  202,  200,
 /*  1070 */   106,  227,  396,  224,   66,  396,  221,   84,  210,  209,
 /*  1080 */   396,  203,  202,  200,   94,  227,  396,  224,   66,  396,
 /*  1090 */   221,   84,  210,  209,  396,  203,  202,  200,  223,  396,
 /*  1100 */   224,   66,  396,  221,   84,  210,  209,  396,  203,  202,
 /*  1110 */   200,  396,  164,  396,  224,   66,  396,  221,   84,  210,
 /*  1120 */   209,  396,  203,  202,  200,   80,  396,  221,   84,  210,
 /*  1130 */   209,  396,  203,  202,  200,   81,  396,  221,   84,  210,
 /*  1140 */   209,  396,  203,  202,  200,   74,  396,  221,   84,  210,
 /*  1150 */   209,  396,  203,  202,  200,   79,  396,  221,   84,  210,
 /*  1160 */   209,  396,  203,  202,  200,  396,   86,  396,  221,   84,
 /*  1170 */   210,  209,  396,  203,  202,  200,  396,   85,  396,  221,
 /*  1180 */    84,  210,  209,  396,  203,  202,  200,  160,  396,  221,
 /*  1190 */    84,  210,  209,  396,  203,  202,  200,  396,  158,  396,
 /*  1200 */   221,   84,  210,  209,  396,  203,  202,  200,   73,  396,
 /*  1210 */   221,   84,  210,  209,  396,  203,  202,  200,   72,  396,
 /*  1220 */   221,   84,  210,  209,  396,  203,  202,  200,  156,  396,
 /*  1230 */   221,   84,  210,  209,  396,  203,  202,  200,   71,  396,
 /*  1240 */   221,   84,  210,  209,  396,  203,  202,  200,   77,  396,
 /*  1250 */   221,   84,  210,  209,  396,  203,  202,  200,   78,  396,
 /*  1260 */   221,   84,  210,  209,  396,  203,  202,  200,  396,   76,
 /*  1270 */   396,  221,   84,  210,  209,  396,  203,  202,  200,   75,
 /*  1280 */   396,  221,   84,  210,  209,  396,  203,  202,  200,  396,
 /*  1290 */    82,  396,  221,   84,  210,  209,  396,  203,  202,  200,
 /*  1300 */    83,  396,  221,   84,  210,  209,  396,  203,  202,  200,
 /*  1310 */   212,   84,  210,  209,  396,  203,  202,  200,  217,   84,
 /*  1320 */   210,  209,  396,  203,  202,  200,  396,  211,   84,  210,
 /*  1330 */   209,  396,  203,  202,  200,  172,  113,  167,   97,   63,
 /*  1340 */   163,  396,   64,  396,  173,   90,
};
static const YYCODETYPE yy_lookahead[] = {
 /*     0 */    67,   68,   69,   70,   71,   72,   73,   74,   75,   76,
 /*    10 */    77,   78,   79,   80,   81,   82,   83,   84,   85,   86,
 /*    20 */    47,   88,   89,   47,   91,   92,   93,   94,   15,   96,
 /*    30 */    97,   98,   16,   17,   18,  102,  103,  104,  105,  105,
 /*    40 */    27,   28,   29,  109,   31,   32,   47,   34,   45,   36,
 /*    50 */    37,   38,   39,   40,   41,   42,   43,   44,   47,   46,
 /*    60 */    47,   48,   49,   50,   51,   52,   53,   54,   55,   56,
 /*    70 */    57,   58,   15,  121,  122,   62,   63,   64,   14,   15,
 /*    80 */    16,   17,   18,  120,   27,   28,   29,   49,   31,   32,
 /*    90 */    45,   34,   54,   36,   37,   38,   39,   40,   41,   42,
 /*   100 */    43,   44,   47,   46,   47,   48,   49,   50,   51,   52,
 /*   110 */    53,   54,   55,   56,   57,   58,  106,  107,   67,   62,
 /*   120 */    63,   64,   71,   72,   73,   74,   75,   76,   77,   78,
 /*   130 */    79,   80,   81,   82,   83,   84,   85,   86,   33,   88,
 /*   140 */    89,   49,   91,   92,   93,   94,   54,   96,   97,   98,
 /*   150 */    45,   47,   35,  102,  103,  104,  105,   85,   86,   15,
 /*   160 */    88,   89,   45,   91,   92,   93,   94,   45,   96,   97,
 /*   170 */    98,   27,   28,   29,  123,   31,   32,  115,   34,   31,
 /*   180 */    36,   37,   38,   39,   40,   41,   42,   43,   44,   35,
 /*   190 */    46,   47,   48,   19,   50,   51,   52,   53,   54,   55,
 /*   200 */    56,   57,   58,   19,  107,   67,   62,   63,   64,   71,
 /*   210 */    72,   73,   74,   75,   76,   77,   78,   79,   80,   81,
 /*   220 */    82,   83,   84,   85,   86,   19,   88,   89,   31,   91,
 /*   230 */    92,   93,   94,   59,   96,   97,   98,   96,   31,   47,
 /*   240 */   102,  103,  104,  105,    8,    9,   10,   11,   12,   13,
 /*   250 */    14,   15,   16,   17,   18,   90,   48,   31,   67,  114,
 /*   260 */   115,  123,  117,   72,   73,   74,   75,   76,   77,   78,
 /*   270 */    79,   80,   81,   82,   83,   84,   85,   86,   31,   88,
 /*   280 */    89,   31,   91,   92,   93,   94,   31,   96,   97,   98,
 /*   290 */    35,   26,   41,  102,  103,  104,  105,   31,   27,   28,
 /*   300 */    67,   30,   59,   32,  113,   72,   73,   74,   75,   76,
 /*   310 */    77,   78,   79,   80,   81,   82,   83,   84,   85,   86,
 /*   320 */    26,   88,   89,   49,   91,   92,   93,   94,   35,   96,
 /*   330 */    97,   98,   34,   34,   34,  102,  103,  104,  105,   65,
 /*   340 */    66,   34,   47,   67,   35,   33,  113,   71,   72,   73,
 /*   350 */    74,   75,   76,   77,   78,   79,   80,   81,   82,   83,
 /*   360 */    84,   85,   86,   35,   88,   89,   35,   91,   92,   93,
 /*   370 */    94,   48,   96,   97,   98,   34,   47,   67,  102,  103,
 /*   380 */   104,  105,   72,   73,   74,   75,   76,   77,   78,   79,
 /*   390 */    80,   81,   82,   83,   84,   85,   86,   31,   88,   89,
 /*   400 */    48,   91,   92,   93,   94,   59,   96,   97,   98,   34,
 /*   410 */    61,   67,  102,  103,  104,  105,   72,   73,   74,   75,
 /*   420 */    76,   77,   78,   79,   80,   81,   82,   83,   84,   85,
 /*   430 */    86,   26,   88,   89,   47,   91,   92,   93,   94,   35,
 /*   440 */    96,   97,   98,   35,   35,   67,  102,  103,  104,  105,
 /*   450 */    72,   73,   74,   75,   76,   77,   78,   79,   80,   81,
 /*   460 */    82,   83,   84,   85,   86,   31,   88,   89,   47,   91,
 /*   470 */    92,   93,   94,   47,   96,   97,   98,  124,  124,   67,
 /*   480 */   102,  103,  104,  105,   72,   73,   74,   75,   76,   77,
 /*   490 */    78,   79,   80,   81,   82,   83,   84,   85,   86,  124,
 /*   500 */    88,   89,  124,   91,   92,   93,   94,  124,   96,   97,
 /*   510 */    98,  124,  124,   67,  102,  103,  104,  105,   72,   73,
 /*   520 */    74,   75,   76,   77,   78,   79,   80,   81,   82,   83,
 /*   530 */    84,   85,   86,  124,   88,   89,  124,   91,   92,   93,
 /*   540 */    94,  124,   96,   97,   98,  124,  124,   67,  102,  103,
 /*   550 */   104,  105,   72,   73,   74,   75,   76,   77,   78,   79,
 /*   560 */    80,   81,   82,   83,   84,   85,   86,  124,   88,   89,
 /*   570 */   124,   91,   92,   93,   94,  124,   96,   97,   98,  124,
 /*   580 */   124,   67,  102,  103,  104,  105,   72,   73,   74,   75,
 /*   590 */    76,   77,   78,   79,   80,   81,   82,   83,   84,   85,
 /*   600 */    86,  124,   88,   89,  124,   91,   92,   93,   94,  124,
 /*   610 */    96,   97,   98,  124,  124,  124,  102,  103,  104,  105,
 /*   620 */     1,    2,    3,    4,    5,    6,    7,    8,    9,   10,
 /*   630 */    11,   12,   13,   14,   15,   16,   17,   18,   19,   20,
 /*   640 */    21,   22,   23,   24,   25,   15,   10,   11,   12,   13,
 /*   650 */    14,   15,   16,   17,   18,   49,  124,   27,   28,   29,
 /*   660 */    54,   31,   32,   57,   34,  124,   36,   37,   38,   39,
 /*   670 */    40,   41,   42,   43,   44,   15,   46,   47,  112,  124,
 /*   680 */   114,  115,  116,  117,   54,  124,  124,   27,   28,   29,
 /*   690 */   124,   31,   32,  124,   34,   35,   36,   37,   38,   39,
 /*   700 */    40,   41,   42,   43,   44,   15,   46,   36,   37,   38,
 /*   710 */    39,   40,   41,   42,   43,   44,  124,   27,   28,   29,
 /*   720 */   124,   31,   32,  124,   34,  124,   36,   37,   38,   39,
 /*   730 */    40,   41,   42,   43,   44,   15,   46,   47,   91,   92,
 /*   740 */    93,   94,  124,   96,   97,   98,  124,   27,   28,   29,
 /*   750 */   124,   31,   32,  124,   34,  124,   36,   37,   38,   39,
 /*   760 */    40,   41,   42,   43,   44,  124,   46,    2,    3,    4,
 /*   770 */     5,    6,    7,    8,    9,   10,   11,   12,   13,   14,
 /*   780 */    15,   16,   17,   18,    3,    4,    5,    6,    7,    8,
 /*   790 */     9,   10,   11,   12,   13,   14,   15,   16,   17,   18,
 /*   800 */     4,    5,    6,    7,    8,    9,   10,   11,   12,   13,
 /*   810 */    14,   15,   16,   17,   18,  124,  124,   85,   86,  124,
 /*   820 */    88,   89,   74,   91,   92,   93,   94,  124,   96,   97,
 /*   830 */    98,   99,  100,   85,   86,  124,   88,   89,  124,   91,
 /*   840 */    92,   93,   94,  124,   96,   97,   98,  124,   85,   86,
 /*   850 */   124,   88,   89,  105,   91,   92,   93,   94,   95,   96,
 /*   860 */    97,   98,  102,  124,  101,  105,  118,  119,  124,  124,
 /*   870 */   110,  111,    5,    6,    7,    8,    9,   10,   11,   12,
 /*   880 */    13,   14,   15,   16,   17,   18,    6,    7,    8,    9,
 /*   890 */    10,   11,   12,   13,   14,   15,   16,   17,   18,   85,
 /*   900 */    86,  124,   88,   89,  124,   91,   92,   93,   94,  124,
 /*   910 */    96,   97,   98,  124,  100,   85,   86,   87,   88,   89,
 /*   920 */   124,   91,   92,   93,   94,  124,   96,   97,   98,  124,
 /*   930 */    85,   86,  124,   88,   89,  124,   91,   92,   93,   94,
 /*   940 */   124,   96,   97,   98,   85,   86,  101,   88,   89,  124,
 /*   950 */    91,   92,   93,   94,  124,   96,   97,   98,   85,   86,
 /*   960 */   124,   88,   89,  124,   91,   92,   93,   94,  124,   96,
 /*   970 */    97,   98,   85,   86,  124,   88,   89,  124,   91,   92,
 /*   980 */    93,   94,  124,   96,   97,   98,   85,   86,  124,   88,
 /*   990 */    89,  124,   91,   92,   93,   94,  124,   96,   97,   98,
 /*  1000 */    85,   86,  124,   88,   89,  124,   91,   92,   93,   94,
 /*  1010 */   124,   96,   97,   98,   85,   86,  124,   88,   89,  124,
 /*  1020 */    91,   92,   93,   94,  124,   96,   97,   98,   85,   86,
 /*  1030 */   124,   88,   89,  124,   91,   92,   93,   94,  124,   96,
 /*  1040 */    97,   98,   85,   86,  124,   88,   89,  124,   91,   92,
 /*  1050 */    93,   94,  124,   96,   97,   98,   85,   86,  124,   88,
 /*  1060 */    89,  124,   91,   92,   93,   94,  124,   96,   97,   98,
 /*  1070 */    85,   86,  124,   88,   89,  124,   91,   92,   93,   94,
 /*  1080 */   124,   96,   97,   98,   85,   86,  124,   88,   89,  124,
 /*  1090 */    91,   92,   93,   94,  124,   96,   97,   98,   86,  124,
 /*  1100 */    88,   89,  124,   91,   92,   93,   94,  124,   96,   97,
 /*  1110 */    98,  124,   86,  124,   88,   89,  124,   91,   92,   93,
 /*  1120 */    94,  124,   96,   97,   98,   89,  124,   91,   92,   93,
 /*  1130 */    94,  124,   96,   97,   98,   89,  124,   91,   92,   93,
 /*  1140 */    94,  124,   96,   97,   98,   89,  124,   91,   92,   93,
 /*  1150 */    94,  124,   96,   97,   98,   89,  124,   91,   92,   93,
 /*  1160 */    94,  124,   96,   97,   98,  124,   89,  124,   91,   92,
 /*  1170 */    93,   94,  124,   96,   97,   98,  124,   89,  124,   91,
 /*  1180 */    92,   93,   94,  124,   96,   97,   98,   89,  124,   91,
 /*  1190 */    92,   93,   94,  124,   96,   97,   98,  124,   89,  124,
 /*  1200 */    91,   92,   93,   94,  124,   96,   97,   98,   89,  124,
 /*  1210 */    91,   92,   93,   94,  124,   96,   97,   98,   89,  124,
 /*  1220 */    91,   92,   93,   94,  124,   96,   97,   98,   89,  124,
 /*  1230 */    91,   92,   93,   94,  124,   96,   97,   98,   89,  124,
 /*  1240 */    91,   92,   93,   94,  124,   96,   97,   98,   89,  124,
 /*  1250 */    91,   92,   93,   94,  124,   96,   97,   98,   89,  124,
 /*  1260 */    91,   92,   93,   94,  124,   96,   97,   98,  124,   89,
 /*  1270 */   124,   91,   92,   93,   94,  124,   96,   97,   98,   89,
 /*  1280 */   124,   91,   92,   93,   94,  124,   96,   97,   98,  124,
 /*  1290 */    89,  124,   91,   92,   93,   94,  124,   96,   97,   98,
 /*  1300 */    89,  124,   91,   92,   93,   94,  124,   96,   97,   98,
 /*  1310 */    91,   92,   93,   94,  124,   96,   97,   98,   91,   92,
 /*  1320 */    93,   94,  124,   96,   97,   98,  124,   91,   92,   93,
 /*  1330 */    94,  124,   96,   97,   98,  102,  105,   49,  105,  108,
 /*  1340 */   109,  124,   54,  124,  111,   57,
};
#define YY_SHIFT_USE_DFLT (-28)
#define YY_SHIFT_MAX 138
static const short yy_shift_ofst[] = {
 /*     0 */   144,  144,  144,   13,  144,  144,   57,  144,  144,  144,
 /*    10 */   144,  144,  144,  144,  144,  630,  660,  720,  720,  720,
 /*    20 */   720,  690,  720,  720,  720,  720,  720,  720,  720,  720,
 /*    30 */   720,  720,  720,  720,  720,  720,  720,  720,  720,  720,
 /*    40 */   720,  720,  720,  720,  720,  720,  720,  720,  720,  720,
 /*    50 */   720,  720,  720,  720,  720,  720,  720,  255, 1288,  606,
 /*    60 */    92,  250,  274,   38,  197,  226,  619,  671,  197,  148,
 /*    70 */   -28,  765,  781,  796,  867,  880,  236,  236,  636,  636,
 /*    80 */    64,   64,   64,   64,  271,   16,   16,  174,  105,  117,
 /*    90 */   266,  298,  300,  307,  328,  312,  331,  329,  341,  375,
 /*   100 */   349,  387,  408,  434,  421,  409,  404,  405,  352,  346,
 /*   110 */   366,  323,  122,  295,  309,  293,  299,  243,  294,  206,
 /*   120 */   265,  247,  208,  192,  207,  206,  184,  154,  122,   55,
 /*   130 */    45,    3,  -24,  251,  -27,   -1,   11,  104,  426,
};
#define YY_REDUCE_USE_DFLT (-68)
#define YY_REDUCE_MAX 70
static const short yy_reduce_ofst[] = {
 /*     0 */   -67,   51,  138,  276,  191,  233,  344,  378,  344,  310,
 /*    10 */   446,  344,  412,  480,  514,  748,  763,  732,  830,  845,
 /*    20 */   814,  915,  999,  887,  971,  985,  957,  929,  901,  873,
 /*    30 */   859,  943,   72, 1012, 1026, 1180, 1211, 1190, 1169, 1159,
 /*    40 */  1149, 1139, 1129, 1119, 1109, 1098, 1088, 1077, 1201, 1066,
 /*    50 */  1046, 1036, 1056, 1236, 1219, 1227,  647,  566,  760, 1233,
 /*    60 */  1231,  145,  -48,  -66,   10,   10,  165,  141,   97,   62,
 /*    70 */   -37,
};
static const YYACTIONTYPE yy_default[] = {
 /*     0 */   255,  394,  394,  394,  394,  394,  394,  394,  256,  394,
 /*    10 */   394,  393,  394,  394,  394,  394,  394,  394,  271,  394,
 /*    20 */   394,  394,  394,  394,  394,  394,  394,  394,  394,  394,
 /*    30 */   394,  394,  394,  394,  394,  394,  394,  394,  394,  394,
 /*    40 */   394,  394,  394,  394,  394,  394,  394,  394,  394,  394,
 /*    50 */   394,  394,  394,  394,  394,  394,  394,  394,  394,  394,
 /*    60 */   394,  394,  394,  394,  394,  394,  281,  394,  394,  394,
 /*    70 */   388,  284,  285,  286,  287,  288,  301,  300,  289,  290,
 /*    80 */   291,  292,  294,  293,  302,  296,  295,  351,  394,  394,
 /*    90 */   394,  394,  394,  394,  394,  394,  394,  394,  394,  394,
 /*   100 */   384,  394,  394,  394,  394,  394,  394,  394,  394,  394,
 /*   110 */   394,  394,  376,  394,  394,  394,  328,  316,  394,  368,
 /*   120 */   394,  394,  394,  394,  394,  394,  351,  394,  375,  394,
 /*   130 */   350,  374,  394,  394,  394,  394,  394,  394,  394,  346,
 /*   140 */   345,  347,  344,  348,  343,  349,  342,  341,  340,  251,
 /*   150 */   257,  352,  354,  353,  338,  337,  299,  355,  298,  356,
 /*   160 */   297,  357,  359,  358,  282,  280,  279,  360,  278,  361,
 /*   170 */   277,  362,  363,  365,  276,  275,  312,  335,  254,  366,
 /*   180 */   377,  336,  334,  330,  333,  332,  331,  381,  329,  327,
 /*   190 */   326,  325,  324,  323,  385,  322,  321,  320,  319,  386,
 /*   200 */   318,  317,  316,  315,  314,  313,  387,  389,  390,  308,
 /*   210 */   307,  306,  305,  391,  304,  253,  392,  303,  311,  310,
 /*   220 */   309,  283,  272,  274,  273,  378,  379,  270,  380,  269,
 /*   230 */   268,  267,  266,  382,  252,  367,  265,  264,  369,  370,
 /*   240 */   263,  262,  371,  372,  261,  260,  259,  373,  258,  364,
 /*   250 */   339,
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
  "CLASS",         "FUNCTION",      "FOR",           "IN",          
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
  "class_declaration",  "variable_declaration",  "declarator_sequence",  "declarator",  
  "struct_members",  "struct_member",  "class_members",  "class_member",
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
 /*  97 */ "declaration_statement ::= class_declaration SEMICOLON",
 /*  98 */ "declaration_statement ::= variable_declaration SEMICOLON",
 /*  99 */ "variable_declaration ::= VAR declarator_sequence",
 /* 100 */ "declarator ::= IDENTIFIER",
 /* 101 */ "declarator ::= IDENTIFIER ASSIGN expression",
 /* 102 */ "declarator_sequence ::= declarator",
 /* 103 */ "declarator_sequence ::= declarator_sequence COMMA declarator",
 /* 104 */ "struct_declaration ::= STRUCT IDENTIFIER LBRACE RBRACE",
 /* 105 */ "struct_declaration ::= STRUCT IDENTIFIER LBRACE struct_members RBRACE",
 /* 106 */ "struct_member ::= variable_declaration SEMICOLON",
 /* 107 */ "struct_members ::= struct_member",
 /* 108 */ "struct_members ::= struct_members struct_member",
 /* 109 */ "class_declaration ::= CLASS IDENTIFIER LBRACE RBRACE",
 /* 110 */ "class_declaration ::= CLASS IDENTIFIER LBRACE class_members RBRACE",
 /* 111 */ "class_member ::= variable_declaration SEMICOLON",
 /* 112 */ "class_member ::= function_declaration",
 /* 113 */ "class_members ::= class_member",
 /* 114 */ "class_members ::= class_members class_member",
 /* 115 */ "function_declaration ::= FUNCTION IDENTIFIER LPAREN parameter_list RPAREN function_body",
 /* 116 */ "function_declaration ::= FUNCTION IDENTIFIER LPAREN RPAREN function_body",
 /* 117 */ "parameter ::= IDENTIFIER",
 /* 118 */ "opt_parameter ::= IDENTIFIER ASSIGN expression",
 /* 119 */ "parameters ::= parameter",
 /* 120 */ "parameters ::= parameters COMMA parameter",
 /* 121 */ "opt_parameters ::= opt_parameter",
 /* 122 */ "opt_parameters ::= opt_parameters COMMA opt_parameter",
 /* 123 */ "parameter_list ::= parameters",
 /* 124 */ "parameter_list ::= opt_parameters",
 /* 125 */ "parameter_list ::= parameters COMMA opt_parameters",
 /* 126 */ "function_body ::= statement",
 /* 127 */ "for_statement ::= FOR LPAREN for_init_statement expression SEMICOLON expression_opt RPAREN statement",
 /* 128 */ "for_init_statement ::= expression_statement",
 /* 129 */ "for_init_statement ::= variable_declaration SEMICOLON",
 /* 130 */ "foreach_statement ::= FOR LPAREN id_expression IN expression RPAREN statement",
 /* 131 */ "foreach_statement ::= FOR LPAREN foreach_decl IN expression RPAREN statement",
 /* 132 */ "foreach_decl ::= VAR IDENTIFIER",
 /* 133 */ "if_statement ::= IF LPAREN expression RPAREN statement",
 /* 134 */ "if_statement ::= IF LPAREN expression RPAREN statement ELSE statement",
 /* 135 */ "while_statement ::= WHILE LPAREN expression RPAREN statement",
 /* 136 */ "switch_statement ::= SWITCH LPAREN expression RPAREN LBRACE switch_body RBRACE",
 /* 137 */ "switch_body ::=",
 /* 138 */ "switch_body ::= switch_body switch_case",
 /* 139 */ "switch_body ::= switch_body default_case",
 /* 140 */ "switch_case ::= CASE literal COLON case_statements",
 /* 141 */ "default_case ::= DEFAULT COLON case_statements",
 /* 142 */ "case_statements ::= statement_sequence",
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
  { 68, 1 },
  { 69, 1 },
  { 71, 1 },
  { 71, 2 },
  { 70, 0 },
  { 70, 1 },
  { 72, 1 },
  { 72, 1 },
  { 72, 1 },
  { 72, 1 },
  { 72, 1 },
  { 72, 1 },
  { 72, 1 },
  { 72, 1 },
  { 72, 1 },
  { 72, 1 },
  { 72, 1 },
  { 72, 1 },
  { 72, 1 },
  { 85, 1 },
  { 87, 0 },
  { 87, 1 },
  { 86, 1 },
  { 86, 3 },
  { 90, 1 },
  { 90, 1 },
  { 90, 1 },
  { 90, 1 },
  { 90, 1 },
  { 90, 1 },
  { 88, 1 },
  { 88, 5 },
  { 89, 1 },
  { 89, 3 },
  { 89, 3 },
  { 89, 3 },
  { 89, 3 },
  { 89, 3 },
  { 89, 3 },
  { 89, 3 },
  { 89, 3 },
  { 89, 3 },
  { 89, 3 },
  { 89, 3 },
  { 89, 3 },
  { 89, 3 },
  { 89, 3 },
  { 89, 3 },
  { 89, 3 },
  { 89, 3 },
  { 89, 3 },
  { 91, 1 },
  { 91, 2 },
  { 91, 2 },
  { 91, 2 },
  { 91, 2 },
  { 91, 1 },
  { 92, 1 },
  { 92, 2 },
  { 92, 2 },
  { 92, 3 },
  { 92, 4 },
  { 92, 3 },
  { 92, 4 },
  { 94, 1 },
  { 94, 1 },
  { 94, 3 },
  { 94, 1 },
  { 96, 1 },
  { 96, 1 },
  { 96, 1 },
  { 96, 1 },
  { 96, 1 },
  { 96, 1 },
  { 96, 1 },
  { 96, 1 },
  { 96, 1 },
  { 97, 1 },
  { 98, 3 },
  { 99, 1 },
  { 99, 3 },
  { 100, 1 },
  { 93, 2 },
  { 101, 1 },
  { 95, 1 },
  { 95, 3 },
  { 74, 1 },
  { 74, 2 },
  { 77, 2 },
  { 77, 3 },
  { 73, 3 },
  { 81, 3 },
  { 81, 2 },
  { 83, 2 },
  { 84, 2 },
  { 75, 1 },
  { 75, 2 },
  { 75, 2 },
  { 75, 2 },
  { 105, 2 },
  { 107, 1 },
  { 107, 3 },
  { 106, 1 },
  { 106, 3 },
  { 103, 4 },
  { 103, 5 },
  { 109, 2 },
  { 108, 1 },
  { 108, 2 },
  { 104, 4 },
  { 104, 5 },
  { 111, 2 },
  { 111, 1 },
  { 110, 1 },
  { 110, 2 },
  { 102, 6 },
  { 102, 5 },
  { 114, 1 },
  { 115, 3 },
  { 116, 1 },
  { 116, 3 },
  { 117, 1 },
  { 117, 3 },
  { 112, 1 },
  { 112, 1 },
  { 112, 3 },
  { 113, 1 },
  { 76, 8 },
  { 118, 1 },
  { 118, 2 },
  { 80, 7 },
  { 80, 7 },
  { 119, 2 },
  { 78, 5 },
  { 78, 7 },
  { 79, 5 },
  { 82, 7 },
  { 120, 0 },
  { 120, 2 },
  { 120, 2 },
  { 121, 4 },
  { 122, 3 },
  { 123, 1 },
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
{ p->SetRoot(yymsp[0].minor.yy175); }
#line 1220 "astgen.c"
        break;
      case 1:
#line 78 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(translation_unit, yymsp[0].minor.yy175); }
#line 1225 "astgen.c"
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
      case 99:
      case 102:
      case 107:
      case 112:
      case 119:
      case 121:
      case 123:
      case 124:
      case 126:
      case 128:
      case 142:
#line 81 "astgen.in"
{ yygotominor.yy175 = yymsp[0].minor.yy175; }
#line 1267 "astgen.c"
        break;
      case 3:
#line 82 "astgen.in"
{ 
  if(yymsp[-1].minor.yy175->m_type == statement_sequence) {
    yygotominor.yy175 = yymsp[-1].minor.yy175;
  }
  else {
    yygotominor.yy175 = p->AllocAst(statement_sequence, new AstList);
    yygotominor.yy175->m_a1.GetList()->push_back(yymsp[-1].minor.yy175);
  }
  yygotominor.yy175->m_a1.GetList()->push_back(yymsp[0].minor.yy175);
}
#line 1281 "astgen.c"
        break;
      case 4:
      case 20:
#line 95 "astgen.in"
{ yygotominor.yy175 = 0; }
#line 1287 "astgen.c"
        break;
      case 18:
      case 86:
#line 113 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(empty_statement); }
#line 1293 "astgen.c"
        break;
      case 23:
#line 129 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(assignment_expression, yymsp[-1].minor.yy134, yymsp[-2].minor.yy175, yymsp[0].minor.yy175); }
#line 1298 "astgen.c"
        break;
      case 24:
#line 133 "astgen.in"
{ yygotominor.yy134 = op_assign; }
#line 1303 "astgen.c"
        break;
      case 25:
#line 134 "astgen.in"
{ yygotominor.yy134 = op_assadd; }
#line 1308 "astgen.c"
        break;
      case 26:
#line 135 "astgen.in"
{ yygotominor.yy134 = op_asssub; }
#line 1313 "astgen.c"
        break;
      case 27:
#line 136 "astgen.in"
{ yygotominor.yy134 = op_assmul; }
#line 1318 "astgen.c"
        break;
      case 28:
#line 137 "astgen.in"
{ yygotominor.yy134 = op_assdiv; }
#line 1323 "astgen.c"
        break;
      case 29:
#line 138 "astgen.in"
{ yygotominor.yy134 = op_assmod; }
#line 1328 "astgen.c"
        break;
      case 31:
#line 142 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(ternary_expression, yymsp[-4].minor.yy175, yymsp[-2].minor.yy175, yymsp[0].minor.yy175); }
#line 1333 "astgen.c"
        break;
      case 33:
#line 146 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(binary_expression, op_logor,   yymsp[-2].minor.yy175, yymsp[0].minor.yy175); }
#line 1338 "astgen.c"
        break;
      case 34:
#line 147 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(binary_expression, op_logand,  yymsp[-2].minor.yy175, yymsp[0].minor.yy175); }
#line 1343 "astgen.c"
        break;
      case 35:
#line 148 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(binary_expression, op_bitor,   yymsp[-2].minor.yy175, yymsp[0].minor.yy175); }
#line 1348 "astgen.c"
        break;
      case 36:
#line 149 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(binary_expression, op_bitxor,  yymsp[-2].minor.yy175, yymsp[0].minor.yy175); }
#line 1353 "astgen.c"
        break;
      case 37:
#line 150 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(binary_expression, op_bitand,  yymsp[-2].minor.yy175, yymsp[0].minor.yy175); }
#line 1358 "astgen.c"
        break;
      case 38:
#line 151 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(binary_expression, op_eq,   yymsp[-2].minor.yy175, yymsp[0].minor.yy175); }
#line 1363 "astgen.c"
        break;
      case 39:
#line 152 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(binary_expression, op_ne,   yymsp[-2].minor.yy175, yymsp[0].minor.yy175); }
#line 1368 "astgen.c"
        break;
      case 40:
#line 153 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(binary_expression, op_lt,   yymsp[-2].minor.yy175, yymsp[0].minor.yy175); }
#line 1373 "astgen.c"
        break;
      case 41:
#line 154 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(binary_expression, op_le,   yymsp[-2].minor.yy175, yymsp[0].minor.yy175); }
#line 1378 "astgen.c"
        break;
      case 42:
#line 155 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(binary_expression, op_gt,   yymsp[-2].minor.yy175, yymsp[0].minor.yy175); }
#line 1383 "astgen.c"
        break;
      case 43:
#line 156 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(binary_expression, op_ge,   yymsp[-2].minor.yy175, yymsp[0].minor.yy175); }
#line 1388 "astgen.c"
        break;
      case 44:
#line 157 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(binary_expression, op_add,  yymsp[-2].minor.yy175, yymsp[0].minor.yy175); }
#line 1393 "astgen.c"
        break;
      case 45:
#line 158 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(binary_expression, op_sub,  yymsp[-2].minor.yy175, yymsp[0].minor.yy175); }
#line 1398 "astgen.c"
        break;
      case 46:
#line 159 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(binary_expression, op_mul,  yymsp[-2].minor.yy175, yymsp[0].minor.yy175); }
#line 1403 "astgen.c"
        break;
      case 47:
#line 160 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(binary_expression, op_div,  yymsp[-2].minor.yy175, yymsp[0].minor.yy175); }
#line 1408 "astgen.c"
        break;
      case 48:
#line 161 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(binary_expression, op_mod,  yymsp[-2].minor.yy175, yymsp[0].minor.yy175); }
#line 1413 "astgen.c"
        break;
      case 49:
#line 162 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(binary_expression, op_seq,  yymsp[-2].minor.yy175, yymsp[0].minor.yy175); }
#line 1418 "astgen.c"
        break;
      case 50:
#line 163 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(binary_expression, op_sne,  yymsp[-2].minor.yy175, yymsp[0].minor.yy175); }
#line 1423 "astgen.c"
        break;
      case 52:
#line 167 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(prefix_expression, op_negate, yymsp[0].minor.yy175); }
#line 1428 "astgen.c"
        break;
      case 53:
#line 168 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(prefix_expression, op_preinc, yymsp[0].minor.yy175); }
#line 1433 "astgen.c"
        break;
      case 54:
#line 169 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(prefix_expression, op_predec, yymsp[0].minor.yy175); }
#line 1438 "astgen.c"
        break;
      case 55:
#line 170 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(prefix_expression, op_not,    yymsp[0].minor.yy175); }
#line 1443 "astgen.c"
        break;
      case 58:
#line 175 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(postfix_expression, op_postinc, yymsp[-1].minor.yy175); }
#line 1448 "astgen.c"
        break;
      case 59:
#line 176 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(postfix_expression, op_postdec, yymsp[-1].minor.yy175); }
#line 1453 "astgen.c"
        break;
      case 60:
#line 177 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(member_expression, yymsp[-2].minor.yy175, String(yymsp[0].minor.yy0)); }
#line 1458 "astgen.c"
        break;
      case 61:
#line 178 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(index_expression, yymsp[-3].minor.yy175, yymsp[-1].minor.yy175); }
#line 1463 "astgen.c"
        break;
      case 62:
#line 179 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(function_call, String(yymsp[-2].minor.yy0)); }
#line 1468 "astgen.c"
        break;
      case 63:
#line 180 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(function_call, String(yymsp[-3].minor.yy0), yymsp[-1].minor.yy175); }
#line 1473 "astgen.c"
        break;
      case 66:
      case 96:
      case 97:
      case 98:
      case 106:
      case 111:
      case 129:
#line 185 "astgen.in"
{ yygotominor.yy175 = yymsp[-1].minor.yy175; }
#line 1484 "astgen.c"
        break;
      case 68:
#line 189 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(literal, Variant(String(yymsp[0].minor.yy0), Variant::stInt)); }
#line 1489 "astgen.c"
        break;
      case 69:
#line 190 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(literal, Variant(hex2dec(String(yymsp[0].minor.yy0).c_str()))); }
#line 1494 "astgen.c"
        break;
      case 70:
#line 191 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(literal, Variant(bin2dec(String(yymsp[0].minor.yy0).c_str()))); }
#line 1499 "astgen.c"
        break;
      case 71:
#line 192 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(literal, Variant(rom2dec(String(yymsp[0].minor.yy0).c_str()))); }
#line 1504 "astgen.c"
        break;
      case 72:
#line 193 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(literal, Variant(String(yymsp[0].minor.yy0), Variant::stReal)); }
#line 1509 "astgen.c"
        break;
      case 73:
#line 194 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(literal, Variant(String(yymsp[0].minor.yy0), Variant::stString)); }
#line 1514 "astgen.c"
        break;
      case 74:
#line 195 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(literal, Variant(true));    }
#line 1519 "astgen.c"
        break;
      case 75:
#line 196 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(literal, Variant(false));   }
#line 1524 "astgen.c"
        break;
      case 76:
#line 197 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(literal, Variant());        }
#line 1529 "astgen.c"
        break;
      case 77:
#line 200 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(lvalue, String(yymsp[0].minor.yy0)); }
#line 1534 "astgen.c"
        break;
      case 78:
#line 203 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(list_literal, yymsp[-1].minor.yy175); }
#line 1539 "astgen.c"
        break;
      case 79:
#line 204 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(list_content, yymsp[0].minor.yy175); }
#line 1544 "astgen.c"
        break;
      case 80:
#line 205 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(list_content, yymsp[-2].minor.yy175, yymsp[0].minor.yy175); }
#line 1549 "astgen.c"
        break;
      case 81:
#line 206 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(list_entry, yymsp[0].minor.yy175); }
#line 1554 "astgen.c"
        break;
      case 82:
#line 209 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(new_expression, String(yymsp[0].minor.yy0)); }
#line 1559 "astgen.c"
        break;
      case 83:
#line 218 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(argument, yymsp[0].minor.yy175); }
#line 1564 "astgen.c"
        break;
      case 85:
#line 222 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(argument_list, yymsp[-2].minor.yy175, yymsp[0].minor.yy175); }
#line 1569 "astgen.c"
        break;
      case 87:
#line 231 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(expression_statement, yymsp[-1].minor.yy175); }
#line 1574 "astgen.c"
        break;
      case 88:
#line 234 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(compound_statement); }
#line 1579 "astgen.c"
        break;
      case 89:
#line 235 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(compound_statement, yymsp[-1].minor.yy175); }
#line 1584 "astgen.c"
        break;
      case 90:
#line 238 "astgen.in"
{ p->Parse(yymsp[-1].minor.yy0); yygotominor.yy175 = p->GetRoot(); }
#line 1589 "astgen.c"
        break;
      case 91:
#line 241 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(return_statement, yymsp[-1].minor.yy175); }
#line 1594 "astgen.c"
        break;
      case 92:
#line 242 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(return_statement);    }
#line 1599 "astgen.c"
        break;
      case 93:
#line 245 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(break_statement); }
#line 1604 "astgen.c"
        break;
      case 94:
#line 246 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(continue_statement); }
#line 1609 "astgen.c"
        break;
      case 100:
#line 261 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(variable_declaration, String(yymsp[0].minor.yy0));    }
#line 1614 "astgen.c"
        break;
      case 101:
#line 262 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(variable_declaration, String(yymsp[-2].minor.yy0), yymsp[0].minor.yy175); }
#line 1619 "astgen.c"
        break;
      case 103:
#line 265 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(declaration_sequence, yymsp[-2].minor.yy175, yymsp[0].minor.yy175); }
#line 1624 "astgen.c"
        break;
      case 104:
#line 274 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(struct_declaration, String(yymsp[-2].minor.yy0)); }
#line 1629 "astgen.c"
        break;
      case 105:
#line 275 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(struct_declaration, String(yymsp[-3].minor.yy0), yymsp[-1].minor.yy175); }
#line 1634 "astgen.c"
        break;
      case 108:
#line 282 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(struct_members, yymsp[-1].minor.yy175, yymsp[0].minor.yy175); }
#line 1639 "astgen.c"
        break;
      case 109:
#line 289 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(class_declaration, String(yymsp[-2].minor.yy0)); }
#line 1644 "astgen.c"
        break;
      case 110:
#line 290 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(class_declaration, String(yymsp[-3].minor.yy0), yymsp[-1].minor.yy39); }
#line 1649 "astgen.c"
        break;
      case 113:
#line 296 "astgen.in"
{ 
  yygotominor.yy39 = new AstList;
  yygotominor.yy39->push_back(yymsp[0].minor.yy175);
}
#line 1657 "astgen.c"
        break;
      case 114:
#line 300 "astgen.in"
{ 
  yygotominor.yy39 = yymsp[-1].minor.yy39;
  yygotominor.yy39->push_back(yymsp[0].minor.yy175);
}
#line 1665 "astgen.c"
        break;
      case 115:
#line 311 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(function_declaration, String(yymsp[-4].minor.yy0), yymsp[-2].minor.yy175, yymsp[0].minor.yy175); }
#line 1670 "astgen.c"
        break;
      case 116:
#line 312 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(function_declaration, String(yymsp[-3].minor.yy0), AstData(), yymsp[0].minor.yy175); }
#line 1675 "astgen.c"
        break;
      case 117:
#line 315 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(parameter, String(yymsp[0].minor.yy0)); }
#line 1680 "astgen.c"
        break;
      case 118:
#line 318 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(parameter, String(yymsp[-2].minor.yy0), yymsp[0].minor.yy175); }
#line 1685 "astgen.c"
        break;
      case 120:
      case 122:
      case 125:
#line 322 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(parameter_list, yymsp[-2].minor.yy175, yymsp[0].minor.yy175); }
#line 1692 "astgen.c"
        break;
      case 127:
#line 343 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(for_statement, yymsp[-5].minor.yy175, yymsp[-4].minor.yy175, yymsp[-2].minor.yy175, yymsp[0].minor.yy175); }
#line 1697 "astgen.c"
        break;
      case 130:
      case 131:
#line 354 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(foreach_statement, yymsp[-4].minor.yy175, yymsp[-2].minor.yy175, yymsp[0].minor.yy175); }
#line 1703 "astgen.c"
        break;
      case 132:
#line 356 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(variable_declaration, String(yymsp[0].minor.yy0)); }
#line 1708 "astgen.c"
        break;
      case 133:
#line 367 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(if_statement, yymsp[-2].minor.yy175, yymsp[0].minor.yy175); }
#line 1713 "astgen.c"
        break;
      case 134:
#line 368 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(if_statement, yymsp[-4].minor.yy175, yymsp[-2].minor.yy175, yymsp[0].minor.yy175); }
#line 1718 "astgen.c"
        break;
      case 135:
#line 376 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(while_statement, yymsp[-2].minor.yy175,  yymsp[0].minor.yy175); }
#line 1723 "astgen.c"
        break;
      case 136:
#line 384 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(switch_statement, yymsp[-4].minor.yy175, yymsp[-1].minor.yy39); }
#line 1728 "astgen.c"
        break;
      case 137:
#line 388 "astgen.in"
{ yygotominor.yy39 = new AstList; }
#line 1733 "astgen.c"
        break;
      case 138:
      case 139:
#line 389 "astgen.in"
{ yygotominor.yy39 = yymsp[-1].minor.yy39; yygotominor.yy39->push_back(yymsp[0].minor.yy175); }
#line 1739 "astgen.c"
        break;
      case 140:
#line 393 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(switch_case, yymsp[-2].minor.yy175, yymsp[0].minor.yy175); }
#line 1744 "astgen.c"
        break;
      case 141:
#line 396 "astgen.in"
{ yygotominor.yy175 = p->AllocAst(default_case, yymsp[0].minor.yy175); }
#line 1749 "astgen.c"
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
#line 1797 "astgen.c"
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
#line 1815 "astgen.c"
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

