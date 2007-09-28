/* Driver template for the LEMON parser generator.
** The author disclaims copyright to this source code.
*/
/* First off, code is include which follows the "include" declaration
** in the input file. */
#include <stdio.h>
#line 30 "cscript.in"


#include "tokens.h"
#include "parser.h"
#include "ast.h"
#include "astlist.h"
#include "convert.h"

#pragma warning(disable:4065)

#line 20 "cscript.c"
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
#define YYNOCODE 141
#define YYACTIONTYPE unsigned short int
#define CScriptParseTOKENTYPE  Token 
typedef union {
  CScriptParseTOKENTYPE yy0;
  AstList* yy15;
  opcodes yy54;
  AccessTypes yy68;
  Ast* yy151;
  int yy281;
} YYMINORTYPE;
#ifndef YYSTACKDEPTH
#define YYSTACKDEPTH 100
#endif
#define CScriptParseARG_SDECL  Parser* p ;
#define CScriptParseARG_PDECL , Parser* p 
#define CScriptParseARG_FETCH  Parser* p  = yypParser->p 
#define CScriptParseARG_STORE yypParser->p  = p 
#define YYNSTATE 287
#define YYNRULE 163
#define YYERRORSYMBOL 77
#define YYERRSYMDT yy281
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
 /*     0 */   266,  451,  279,  271,    6,  262,  248,  235,  230,  228,
 /*    10 */   223,  190,  181,  182,  278,  277,  276,  274,  270,  268,
 /*    20 */   267,  137,  265,  107,  264,   69,   28,  258,   91,  250,
 /*    30 */   247,  246,  242,  241,  239,   56,   66,  131,  377,  214,
 /*    40 */   212,  112,  136,  167,  154,  156,   24,   55,   57,   58,
 /*    50 */   175,  102,   17,  178,   32,  144,  238,  237,  234,  233,
 /*    60 */   232,  229,  227,  226,  225,  222,   22,  105,   43,   38,
 /*    70 */    39,  187,    3,  286,  142,   20,  149,  151,   68,  155,
 /*    80 */   159,  165,  275,  141,  129,  111,    3,   56,  135,  106,
 /*    90 */   147,  116,  257,  255,   76,   73,   29,   34,  127,   55,
 /*   100 */    57,   58,  243,  102,   17,  210,   32,   22,  238,  237,
 /*   110 */   234,  233,  232,  229,  227,  226,  225,  222,  162,  105,
 /*   120 */   256,   70,  145,  187,    3,  180,  142,   20,  149,  151,
 /*   130 */    68,  155,  244,  245,  153,  141,  129,  111,  174,   56,
 /*   140 */   148,  106,  147,  116,  284,   65,   76,  166,   29,   23,
 /*   150 */   206,   55,   57,   58,  437,  102,   17,   14,   32,   61,
 /*   160 */   238,  237,  234,  233,  232,  229,  227,  226,  225,  222,
 /*   170 */   123,  105,   10,   94,  205,  187,    3,  168,  142,   20,
 /*   180 */   149,  151,   68,  155,  134,   19,  164,  141,  129,  111,
 /*   190 */   183,  184,  185,  106,  147,  116,  170,  266,   76,  161,
 /*   200 */    29,   13,  262,  248,  235,  230,  228,  223,  190,  181,
 /*   210 */   182,  278,  277,  276,  274,  270,  268,  267,  137,  265,
 /*   220 */   173,  264,   69,   99,  258,   91,  250,  247,  246,  242,
 /*   230 */   241,  239,  253,   91,  250,  247,  246,  242,  241,  239,
 /*   240 */   167,  154,  156,  261,   30,  264,   69,  126,  258,   91,
 /*   250 */   250,  247,  246,  242,  241,  239,  266,   11,   36,  263,
 /*   260 */    13,  262,  248,  235,  230,  228,  223,  190,  181,  182,
 /*   270 */   278,  277,  276,  274,  270,  268,  267,  137,  265,  107,
 /*   280 */   264,   69,    2,  258,   91,  250,  247,  246,  242,  241,
 /*   290 */   239,  177,   96,  124,  176,  214,  212,  112,  136,  167,
 /*   300 */   154,  156,  219,  265,  107,  264,   69,  143,  258,   91,
 /*   310 */   250,  247,  246,  242,  241,  239,  100,   98,  260,  266,
 /*   320 */   211,  212,   72,  117,  192,  248,  235,  230,  228,  223,
 /*   330 */   190,  181,  182,  278,  277,  276,  274,  270,  268,  267,
 /*   340 */   137,  265,   35,  264,   69,  140,  258,   91,  250,  247,
 /*   350 */   246,  242,  241,  239,  254,   91,  250,  247,  246,  242,
 /*   360 */   241,  239,  167,  154,  156,   16,  103,   60,  266,  259,
 /*   370 */   221,  191,    5,  262,  248,  235,  230,  228,  223,  190,
 /*   380 */   181,  182,  278,  277,  276,  274,  270,  268,  267,  137,
 /*   390 */   265,   18,  264,   69,   33,  258,   91,  250,  247,  246,
 /*   400 */   242,  241,  239,   48,   46,   43,   38,   39,  186,  266,
 /*   410 */    14,  167,  154,  156,  249,  248,  235,  230,  228,  223,
 /*   420 */   190,  181,  182,  278,  277,  276,  274,  270,  268,  267,
 /*   430 */   137,  265,   64,  264,   69,   59,  258,   91,  250,  247,
 /*   440 */   246,  242,  241,  239,  119,    9,  218,   28,   26,  109,
 /*   450 */   266,   27,  167,  154,  156,  272,  248,  235,  230,  228,
 /*   460 */   223,  190,  181,  182,  278,  277,  276,  274,  270,  268,
 /*   470 */   267,  137,  265,   75,  264,   69,    4,  258,   91,  250,
 /*   480 */   247,  246,  242,  241,  239,    8,   71,  132,    7,   63,
 /*   490 */     1,  266,  130,  167,  154,  156,  280,  248,  235,  230,
 /*   500 */   228,  223,  190,  181,  182,  278,  277,  276,  274,  270,
 /*   510 */   268,  267,  137,  265,   15,  264,   69,  128,  258,   91,
 /*   520 */   250,  247,  246,  242,  241,  239,  204,   74,  217,   12,
 /*   530 */   115,   23,  266,   31,  167,  154,  156,  224,  248,  235,
 /*   540 */   230,  228,  223,  190,  181,  182,  278,  277,  276,  274,
 /*   550 */   270,  268,  267,  137,  265,  240,  264,   69,  215,  258,
 /*   560 */    91,  250,  247,  246,  242,  241,  239,  120,   77,  452,
 /*   570 */   452,  452,  452,  266,  452,  167,  154,  156,  231,  248,
 /*   580 */   235,  230,  228,  223,  190,  181,  182,  278,  277,  276,
 /*   590 */   274,  270,  268,  267,  137,  265,  452,  264,   69,  452,
 /*   600 */   258,   91,  250,  247,  246,  242,  241,  239,  452,  452,
 /*   610 */   452,  452,  452,  452,  266,  452,  167,  154,  156,  236,
 /*   620 */   248,  235,  230,  228,  223,  190,  181,  182,  278,  277,
 /*   630 */   276,  274,  270,  268,  267,  137,  265,  452,  264,   69,
 /*   640 */   452,  258,   91,  250,  247,  246,  242,  241,  239,  452,
 /*   650 */   452,  452,  452,  452,  452,  266,  452,  167,  154,  156,
 /*   660 */   110,  248,  235,  230,  228,  223,  190,  181,  182,  278,
 /*   670 */   277,  276,  274,  270,  268,  267,  137,  265,  452,  264,
 /*   680 */    69,  452,  258,   91,  250,  247,  246,  242,  241,  239,
 /*   690 */   251,   91,  250,  247,  246,  242,  241,  239,  167,  154,
 /*   700 */   156,   37,   41,   42,   44,   45,   40,   47,   51,   54,
 /*   710 */    53,   52,   50,   49,   48,   46,   43,   38,   39,  203,
 /*   720 */   201,  200,  199,  198,  197,   21,   56,   51,   54,   53,
 /*   730 */    52,   50,   49,   48,   46,   43,   38,   39,   55,   57,
 /*   740 */    58,  452,  102,   17,  452,   32,  195,  238,  237,  234,
 /*   750 */   233,  232,  229,  227,  226,  225,  222,  452,  105,   56,
 /*   760 */   452,  452,  187,  183,  184,  185,  452,  452,  452,   67,
 /*   770 */   452,   55,   57,   58,  452,  102,   17,  452,   32,  452,
 /*   780 */   238,  237,  234,  233,  232,  229,  227,  226,  225,  222,
 /*   790 */   452,  105,   56,  452,  452,  172,  252,   91,  250,  247,
 /*   800 */   246,  242,  241,  239,   55,   57,   58,  452,  102,   17,
 /*   810 */   452,   32,  452,  238,  237,  234,  233,  232,  229,  227,
 /*   820 */   226,  225,  222,  452,  105,   56,   86,  452,  258,   91,
 /*   830 */   250,  247,  246,  242,  241,  239,  452,   55,   57,   58,
 /*   840 */   452,   95,   17,  452,   32,  452,  238,  237,  234,  233,
 /*   850 */   232,  229,  227,  226,  225,  222,  452,  105,  213,  265,
 /*   860 */   452,  264,   69,  452,  258,   91,  250,  247,  246,  242,
 /*   870 */   241,  239,  452,  452,  452,  121,  209,  138,  207,  125,
 /*   880 */    41,   42,   44,   45,   40,   47,   51,   54,   53,   52,
 /*   890 */    50,   49,   48,   46,   43,   38,   39,  452,  273,   40,
 /*   900 */    47,   51,   54,   53,   52,   50,   49,   48,   46,   43,
 /*   910 */    38,   39,  137,  265,  179,  264,   69,  452,  258,   91,
 /*   920 */   250,  247,  246,  242,  101,  239,  237,  234,  233,  232,
 /*   930 */   229,  227,  226,  225,  222,   88,  157,  258,   91,  250,
 /*   940 */   247,  246,  242,  241,  239,  452,   68,  452,   25,  158,
 /*   950 */   452,  141,   42,   44,   45,   40,   47,   51,   54,   53,
 /*   960 */    52,   50,   49,   48,   46,   43,   38,   39,   44,   45,
 /*   970 */    40,   47,   51,   54,   53,   52,   50,   49,   48,   46,
 /*   980 */    43,   38,   39,  219,  265,  452,  264,   69,  452,  258,
 /*   990 */    91,  250,  247,  246,  242,  241,  239,  220,   98,   45,
 /*  1000 */    40,   47,   51,   54,   53,   52,   50,   49,   48,   46,
 /*  1010 */    43,   38,   39,  452,  213,  265,  452,  264,   69,  452,
 /*  1020 */   258,   91,  250,  247,  246,  242,  241,  239,  452,  452,
 /*  1030 */   452,  452,  208,  452,  452,  269,  265,  152,  264,   69,
 /*  1040 */   452,  258,   91,  250,  247,  246,  242,  241,  239,  452,
 /*  1050 */   452,  160,  265,  452,  264,   69,  452,  258,   91,  250,
 /*  1060 */   247,  246,  242,  241,  239,  114,  265,  452,  264,   69,
 /*  1070 */   452,  258,   91,  250,  247,  246,  242,  241,  239,  118,
 /*  1080 */   265,  452,  264,   69,  452,  258,   91,  250,  247,  246,
 /*  1090 */   242,  241,  239,  163,  265,  452,  264,   69,  452,  258,
 /*  1100 */    91,  250,  247,  246,  242,  241,  239,  216,  265,  452,
 /*  1110 */   264,   69,  452,  258,   91,  250,  247,  246,  242,  241,
 /*  1120 */   239,  188,  265,  452,  264,   69,  452,  258,   91,  250,
 /*  1130 */   247,  246,  242,  241,  239,   97,  265,  452,  264,   69,
 /*  1140 */   452,  258,   91,  250,  247,  246,  242,  241,  239,  452,
 /*  1150 */   108,  265,  452,  264,   69,  452,  258,   91,  250,  247,
 /*  1160 */   246,  242,  241,  239,  122,  265,  452,  264,   69,  452,
 /*  1170 */   258,   91,  250,  247,  246,  242,  241,  239,  202,  265,
 /*  1180 */   452,  264,   69,  452,  258,   91,  250,  247,  246,  242,
 /*  1190 */   241,  239,  104,  265,  452,  264,   69,  452,  258,   91,
 /*  1200 */   250,  247,  246,  242,  241,  239,  150,  265,  452,  264,
 /*  1210 */    69,  452,  258,   91,  250,  247,  246,  242,  241,  239,
 /*  1220 */   139,  265,  452,  264,   69,  452,  258,   91,  250,  247,
 /*  1230 */   246,  242,  241,  239,  146,  265,  452,  264,   69,  452,
 /*  1240 */   258,   91,  250,  247,  246,  242,  241,  239,  452,  113,
 /*  1250 */   265,  452,  264,   69,  452,  258,   91,  250,  247,  246,
 /*  1260 */   242,  241,  239,  196,  452,  264,   69,  452,  258,   91,
 /*  1270 */   250,  247,  246,  242,  241,  239,   90,  452,  258,   91,
 /*  1280 */   250,  247,  246,  242,  241,  239,  452,   87,  452,  258,
 /*  1290 */    91,  250,  247,  246,  242,  241,  239,  452,   89,  452,
 /*  1300 */   258,   91,  250,  247,  246,  242,  241,  239,   85,  452,
 /*  1310 */   258,   91,  250,  247,  246,  242,  241,  239,   93,  452,
 /*  1320 */   258,   91,  250,  247,  246,  242,  241,  239,  452,   83,
 /*  1330 */   452,  258,   91,  250,  247,  246,  242,  241,  239,  452,
 /*  1340 */    53,   52,   50,   49,   48,   46,   43,   38,   39,   92,
 /*  1350 */   452,  258,   91,  250,  247,  246,  242,  241,  239,   82,
 /*  1360 */   452,  258,   91,  250,  247,  246,  242,  241,  239,   81,
 /*  1370 */   452,  258,   91,  250,  247,  246,  242,  241,  239,   78,
 /*  1380 */   452,  258,   91,  250,  247,  246,  242,  241,  239,  452,
 /*  1390 */   193,  452,  258,   91,  250,  247,  246,  242,  241,  239,
 /*  1400 */   452,  189,  452,  258,   91,  250,  247,  246,  242,  241,
 /*  1410 */   239,   84,  452,  258,   91,  250,  247,  246,  242,  241,
 /*  1420 */   239,   79,  452,  258,   91,  250,  247,  246,  242,  241,
 /*  1430 */   239,  452,  452,  452,   80,  452,  258,   91,  250,  247,
 /*  1440 */   246,  242,  241,  239,  194,  452,  258,   91,  250,  247,
 /*  1450 */   246,  242,  241,  239,  452,  169,  452,  452,  452,  452,
 /*  1460 */    68,  171,  281,  282,  283,  141,   68,  452,  281,  282,
 /*  1470 */   283,  141,  174,  133,  148,  452,  452,   62,  285,   65,
 /*  1480 */   452,  452,  452,  452,  452,  452,  452,  452,  452,  452,
 /*  1490 */   183,  184,  185,
};
static const YYCODETYPE yy_lookahead[] = {
 /*     0 */    77,   78,   79,   80,   81,   82,   83,   84,   85,   86,
 /*    10 */    87,   88,   89,   90,   91,   92,   93,   94,   95,   96,
 /*    20 */    97,   98,   99,  114,  101,  102,   19,  104,  105,  106,
 /*    30 */   107,  108,  109,  110,  111,   15,  136,  128,   31,  130,
 /*    40 */   131,  132,  133,  120,  121,  122,   66,   27,   28,   29,
 /*    50 */    51,   31,   32,  120,   34,  122,   36,   37,   38,   39,
 /*    60 */    40,   41,   42,   43,   44,   45,   19,   47,   16,   17,
 /*    70 */    18,   51,   52,   53,   54,   55,   56,   57,   58,   59,
 /*    80 */   123,  124,   51,   63,   64,   65,   52,   15,  114,   69,
 /*    90 */    70,   71,   27,   28,   74,   30,   76,   32,   87,   27,
 /*   100 */    28,   29,   53,   31,   32,  131,   34,   19,   36,   37,
 /*   110 */    38,   39,   40,   41,   42,   43,   44,   45,   51,   47,
 /*   120 */    87,   72,   73,   51,   52,   53,   54,   55,   56,   57,
 /*   130 */    58,   59,  137,  138,   31,   63,   64,   65,  120,   15,
 /*   140 */   122,   69,   70,   71,  126,  127,   74,   51,   76,   26,
 /*   150 */   118,   27,   28,   29,   66,   31,   32,   34,   34,   52,
 /*   160 */    36,   37,   38,   39,   40,   41,   42,   43,   44,   45,
 /*   170 */    31,   47,   35,   31,  108,   51,   52,   51,   54,   55,
 /*   180 */    56,   57,   58,   59,   31,   51,  124,   63,   64,   65,
 /*   190 */    48,   49,   50,   69,   70,   71,   51,   77,   74,   31,
 /*   200 */    76,   81,   82,   83,   84,   85,   86,   87,   88,   89,
 /*   210 */    90,   91,   92,   93,   94,   95,   96,   97,   98,   99,
 /*   220 */    51,  101,  102,  114,  104,  105,  106,  107,  108,  109,
 /*   230 */   110,  111,  104,  105,  106,  107,  108,  109,  110,  111,
 /*   240 */   120,  121,  122,   99,   34,  101,  102,  109,  104,  105,
 /*   250 */   106,  107,  108,  109,  110,  111,   77,   35,  103,  139,
 /*   260 */    81,   82,   83,   84,   85,   86,   87,   88,   89,   90,
 /*   270 */    91,   92,   93,   94,   95,   96,   97,   98,   99,  114,
 /*   280 */   101,  102,   26,  104,  105,  106,  107,  108,  109,  110,
 /*   290 */   111,   51,   31,  128,   51,  130,  131,  132,  133,  120,
 /*   300 */   121,  122,   98,   99,  114,  101,  102,   42,  104,  105,
 /*   310 */   106,  107,  108,  109,  110,  111,  112,  113,  139,   77,
 /*   320 */   130,  131,   46,  133,   82,   83,   84,   85,   86,   87,
 /*   330 */    88,   89,   90,   91,   92,   93,   94,   95,   96,   97,
 /*   340 */    98,   99,   26,  101,  102,   31,  104,  105,  106,  107,
 /*   350 */   108,  109,  110,  111,  104,  105,  106,  107,  108,  109,
 /*   360 */   110,  111,  120,  121,  122,   46,   31,   34,   77,   51,
 /*   370 */    33,  129,   81,   82,   83,   84,   85,   86,   87,   88,
 /*   380 */    89,   90,   91,   92,   93,   94,   95,   96,   97,   98,
 /*   390 */    99,   46,  101,  102,   66,  104,  105,  106,  107,  108,
 /*   400 */   109,  110,  111,   14,   15,   16,   17,   18,   51,   77,
 /*   410 */    34,  120,  121,  122,   82,   83,   84,   85,   86,   87,
 /*   420 */    88,   89,   90,   91,   92,   93,   94,   95,   96,   97,
 /*   430 */    98,   99,   46,  101,  102,   34,  104,  105,  106,  107,
 /*   440 */   108,  109,  110,  111,   31,   35,   31,   19,   19,   31,
 /*   450 */    77,   34,  120,  121,  122,   82,   83,   84,   85,   86,
 /*   460 */    87,   88,   89,   90,   91,   92,   93,   94,   95,   96,
 /*   470 */    97,   98,   99,   35,  101,  102,   35,  104,  105,  106,
 /*   480 */   107,  108,  109,  110,  111,   35,   42,   31,   68,   46,
 /*   490 */    26,   77,   34,  120,  121,  122,   82,   83,   84,   85,
 /*   500 */    86,   87,   88,   89,   90,   91,   92,   93,   94,   95,
 /*   510 */    96,   97,   98,   99,   34,  101,  102,   75,  104,  105,
 /*   520 */   106,  107,  108,  109,  110,  111,   33,   46,   51,   35,
 /*   530 */    35,   26,   77,   34,  120,  121,  122,   82,   83,   84,
 /*   540 */    85,   86,   87,   88,   89,   90,   91,   92,   93,   94,
 /*   550 */    95,   96,   97,   98,   99,   35,  101,  102,   35,  104,
 /*   560 */   105,  106,  107,  108,  109,  110,  111,   35,   52,  140,
 /*   570 */   140,  140,  140,   77,  140,  120,  121,  122,   82,   83,
 /*   580 */    84,   85,   86,   87,   88,   89,   90,   91,   92,   93,
 /*   590 */    94,   95,   96,   97,   98,   99,  140,  101,  102,  140,
 /*   600 */   104,  105,  106,  107,  108,  109,  110,  111,  140,  140,
 /*   610 */   140,  140,  140,  140,   77,  140,  120,  121,  122,   82,
 /*   620 */    83,   84,   85,   86,   87,   88,   89,   90,   91,   92,
 /*   630 */    93,   94,   95,   96,   97,   98,   99,  140,  101,  102,
 /*   640 */   140,  104,  105,  106,  107,  108,  109,  110,  111,  140,
 /*   650 */   140,  140,  140,  140,  140,   77,  140,  120,  121,  122,
 /*   660 */    82,   83,   84,   85,   86,   87,   88,   89,   90,   91,
 /*   670 */    92,   93,   94,   95,   96,   97,   98,   99,  140,  101,
 /*   680 */   102,  140,  104,  105,  106,  107,  108,  109,  110,  111,
 /*   690 */   104,  105,  106,  107,  108,  109,  110,  111,  120,  121,
 /*   700 */   122,    1,    2,    3,    4,    5,    6,    7,    8,    9,
 /*   710 */    10,   11,   12,   13,   14,   15,   16,   17,   18,   19,
 /*   720 */    20,   21,   22,   23,   24,   25,   15,    8,    9,   10,
 /*   730 */    11,   12,   13,   14,   15,   16,   17,   18,   27,   28,
 /*   740 */    29,  140,   31,   32,  140,   34,   31,   36,   37,   38,
 /*   750 */    39,   40,   41,   42,   43,   44,   45,  140,   47,   15,
 /*   760 */   140,  140,   51,   48,   49,   50,  140,  140,  140,   58,
 /*   770 */   140,   27,   28,   29,  140,   31,   32,  140,   34,  140,
 /*   780 */    36,   37,   38,   39,   40,   41,   42,   43,   44,   45,
 /*   790 */   140,   47,   15,  140,  140,   51,  104,  105,  106,  107,
 /*   800 */   108,  109,  110,  111,   27,   28,   29,  140,   31,   32,
 /*   810 */   140,   34,  140,   36,   37,   38,   39,   40,   41,   42,
 /*   820 */    43,   44,   45,  140,   47,   15,  102,  140,  104,  105,
 /*   830 */   106,  107,  108,  109,  110,  111,  140,   27,   28,   29,
 /*   840 */   140,   31,   32,  140,   34,  140,   36,   37,   38,   39,
 /*   850 */    40,   41,   42,   43,   44,   45,  140,   47,   98,   99,
 /*   860 */   140,  101,  102,  140,  104,  105,  106,  107,  108,  109,
 /*   870 */   110,  111,  140,  140,  140,  115,  116,  117,  118,  119,
 /*   880 */     2,    3,    4,    5,    6,    7,    8,    9,   10,   11,
 /*   890 */    12,   13,   14,   15,   16,   17,   18,  140,   84,    6,
 /*   900 */     7,    8,    9,   10,   11,   12,   13,   14,   15,   16,
 /*   910 */    17,   18,   98,   99,   26,  101,  102,  140,  104,  105,
 /*   920 */   106,  107,  108,  109,  110,  111,   37,   38,   39,   40,
 /*   930 */    41,   42,   43,   44,   45,  102,  122,  104,  105,  106,
 /*   940 */   107,  108,  109,  110,  111,  140,   58,  140,  134,  135,
 /*   950 */   140,   63,    3,    4,    5,    6,    7,    8,    9,   10,
 /*   960 */    11,   12,   13,   14,   15,   16,   17,   18,    4,    5,
 /*   970 */     6,    7,    8,    9,   10,   11,   12,   13,   14,   15,
 /*   980 */    16,   17,   18,   98,   99,  140,  101,  102,  140,  104,
 /*   990 */   105,  106,  107,  108,  109,  110,  111,  112,  113,    5,
 /*  1000 */     6,    7,    8,    9,   10,   11,   12,   13,   14,   15,
 /*  1010 */    16,   17,   18,  140,   98,   99,  140,  101,  102,  140,
 /*  1020 */   104,  105,  106,  107,  108,  109,  110,  111,  140,  140,
 /*  1030 */   140,  140,  116,  140,  140,   98,   99,  100,  101,  102,
 /*  1040 */   140,  104,  105,  106,  107,  108,  109,  110,  111,  140,
 /*  1050 */   140,   98,   99,  140,  101,  102,  140,  104,  105,  106,
 /*  1060 */   107,  108,  109,  110,  111,   98,   99,  140,  101,  102,
 /*  1070 */   140,  104,  105,  106,  107,  108,  109,  110,  111,   98,
 /*  1080 */    99,  140,  101,  102,  140,  104,  105,  106,  107,  108,
 /*  1090 */   109,  110,  111,   98,   99,  140,  101,  102,  140,  104,
 /*  1100 */   105,  106,  107,  108,  109,  110,  111,   98,   99,  140,
 /*  1110 */   101,  102,  140,  104,  105,  106,  107,  108,  109,  110,
 /*  1120 */   111,   98,   99,  140,  101,  102,  140,  104,  105,  106,
 /*  1130 */   107,  108,  109,  110,  111,   98,   99,  140,  101,  102,
 /*  1140 */   140,  104,  105,  106,  107,  108,  109,  110,  111,  140,
 /*  1150 */    98,   99,  140,  101,  102,  140,  104,  105,  106,  107,
 /*  1160 */   108,  109,  110,  111,   98,   99,  140,  101,  102,  140,
 /*  1170 */   104,  105,  106,  107,  108,  109,  110,  111,   98,   99,
 /*  1180 */   140,  101,  102,  140,  104,  105,  106,  107,  108,  109,
 /*  1190 */   110,  111,   98,   99,  140,  101,  102,  140,  104,  105,
 /*  1200 */   106,  107,  108,  109,  110,  111,   98,   99,  140,  101,
 /*  1210 */   102,  140,  104,  105,  106,  107,  108,  109,  110,  111,
 /*  1220 */    98,   99,  140,  101,  102,  140,  104,  105,  106,  107,
 /*  1230 */   108,  109,  110,  111,   98,   99,  140,  101,  102,  140,
 /*  1240 */   104,  105,  106,  107,  108,  109,  110,  111,  140,   98,
 /*  1250 */    99,  140,  101,  102,  140,  104,  105,  106,  107,  108,
 /*  1260 */   109,  110,  111,   99,  140,  101,  102,  140,  104,  105,
 /*  1270 */   106,  107,  108,  109,  110,  111,  102,  140,  104,  105,
 /*  1280 */   106,  107,  108,  109,  110,  111,  140,  102,  140,  104,
 /*  1290 */   105,  106,  107,  108,  109,  110,  111,  140,  102,  140,
 /*  1300 */   104,  105,  106,  107,  108,  109,  110,  111,  102,  140,
 /*  1310 */   104,  105,  106,  107,  108,  109,  110,  111,  102,  140,
 /*  1320 */   104,  105,  106,  107,  108,  109,  110,  111,  140,  102,
 /*  1330 */   140,  104,  105,  106,  107,  108,  109,  110,  111,  140,
 /*  1340 */    10,   11,   12,   13,   14,   15,   16,   17,   18,  102,
 /*  1350 */   140,  104,  105,  106,  107,  108,  109,  110,  111,  102,
 /*  1360 */   140,  104,  105,  106,  107,  108,  109,  110,  111,  102,
 /*  1370 */   140,  104,  105,  106,  107,  108,  109,  110,  111,  102,
 /*  1380 */   140,  104,  105,  106,  107,  108,  109,  110,  111,  140,
 /*  1390 */   102,  140,  104,  105,  106,  107,  108,  109,  110,  111,
 /*  1400 */   140,  102,  140,  104,  105,  106,  107,  108,  109,  110,
 /*  1410 */   111,  102,  140,  104,  105,  106,  107,  108,  109,  110,
 /*  1420 */   111,  102,  140,  104,  105,  106,  107,  108,  109,  110,
 /*  1430 */   111,  140,  140,  140,  102,  140,  104,  105,  106,  107,
 /*  1440 */   108,  109,  110,  111,  102,  140,  104,  105,  106,  107,
 /*  1450 */   108,  109,  110,  111,  140,   53,  140,  140,  140,  140,
 /*  1460 */    58,   53,   60,   61,   62,   63,   58,  140,   60,   61,
 /*  1470 */    62,   63,  120,   31,  122,  140,  140,  125,  126,  127,
 /*  1480 */   140,  140,  140,  140,  140,  140,  140,  140,  140,  140,
 /*  1490 */    48,   49,   50,
};
#define YY_SHIFT_USE_DFLT (-21)
#define YY_SHIFT_MAX 161
static const short yy_shift_ofst[] = {
 /*     0 */   124,  124,  124,   20,  124,   72,  124,  124,  124,  124,
 /*    10 */   124,  124,  124,  124,  810,  711,  777,  777,  777,  777,
 /*    20 */   744,  777,  777,  777,  777,  777,  777,  777,  777,  777,
 /*    30 */   777,  777,  777,  777,  777,  777,  777,  777,  777,  777,
 /*    40 */   777,  777,  777,  777,  777,  777,  777,  777,  777,  777,
 /*    50 */   777,  777,  777,  777,  777,  777,  777,  777,  777,  142,
 /*    60 */   142, 1402, 1408,  142, 1442,  888,   49,  261,  168,  700,
 /*    70 */   889,  715,  168,  153,  139,   34,   34,  -21,  878,  949,
 /*    80 */   964,  994,  893,  719,  719, 1330, 1330,  389,  389,  389,
 /*    90 */   389,   65,   52,   52,    7,  123,   88,  316,  319,  335,
 /*   100 */   337,  328,  376,  401,  410,  415,  417,  418,  450,  429,
 /*   110 */   420,  480,  443,  493,  494,  477,  499,  386,  532,  429,
 /*   120 */   516,  523,  520,  505,  495,  481,  464,  442,  458,  444,
 /*   130 */   456,  441,  438,  428,  376,  413,  386,  357,  345,  318,
 /*   140 */   333,  314,  265,  243,  240,  256,  222,  210,  169,  145,
 /*   150 */   134,  126,  137,  107,   96,  103,   67,   31,  -20,  276,
 /*   160 */    -1,   47,
};
#define YY_REDUCE_USE_DFLT (-101)
#define YY_REDUCE_MAX 77
static const short yy_reduce_ofst[] = {
 /*     0 */   -77,  179,  120,  291,  242,  332,  332,  496,  578,  455,
 /*    10 */   373,  414,  537,  332,  760,  814,  885,  204,  916,  937,
 /*    20 */   953, 1037,  995, 1009, 1136, 1108, 1080, 1052, 1023, 1122,
 /*    30 */   967,  981, 1066, 1094, 1151, 1164,  144, 1277, 1288, 1299,
 /*    40 */  1309, 1319, 1332, 1342, 1267, 1257, 1247, 1227, 1216, 1196,
 /*    50 */  1174,  724,  833, 1185, 1206,  128,  250,  692,  586,  165,
 /*    60 */   -91, 1352,   18,  190,  -26,  -67,   -5,  -43,  -43,  155,
 /*    70 */   138,  109,   62,   66,   32,   33,   11, -100,
};
static const YYACTIONTYPE yy_default[] = {
 /*     0 */   291,  450,  450,  450,  450,  450,  292,  450,  450,  450,
 /*    10 */   450,  450,  450,  447,  385,  450,  450,  450,  450,  310,
 /*    20 */   450,  450,  450,  450,  450,  450,  450,  450,  450,  450,
 /*    30 */   450,  450,  450,  450,  450,  450,  450,  450,  450,  450,
 /*    40 */   450,  450,  450,  450,  450,  450,  450,  450,  450,  450,
 /*    50 */   450,  450,  450,  450,  450,  450,  450,  450,  450,  422,
 /*    60 */   422,  450,  450,  450,  450,  450,  450,  450,  450,  320,
 /*    70 */   450,  450,  450,  450,  450,  450,  450,  442,  323,  324,
 /*    80 */   325,  326,  327,  340,  339,  329,  328,  330,  331,  333,
 /*    90 */   332,  341,  335,  334,  419,  368,  401,  450,  370,  450,
 /*   100 */   450,  355,  368,  450,  450,  450,  450,  450,  450,  418,
 /*   110 */   438,  450,  423,  450,  450,  450,  450,  425,  450,  450,
 /*   120 */   450,  450,  450,  450,  450,  387,  450,  450,  450,  450,
 /*   130 */   450,  450,  450,  377,  351,  450,  424,  450,  386,  450,
 /*   140 */   450,  450,  450,  450,  450,  450,  450,  450,  450,  450,
 /*   150 */   450,  450,  450,  450,  450,  450,  450,  450,  450,  400,
 /*   160 */   450,  401,  399,  402,  404,  403,  398,  397,  396,  405,
 /*   170 */   395,  406,  394,  407,  408,  393,  392,  409,  410,  411,
 /*   180 */   391,  299,  300,  374,  375,  376,  389,  388,  421,  338,
 /*   190 */   298,  417,  430,  337,  336,  377,  321,  319,  318,  317,
 /*   200 */   316,  315,  420,  314,  353,  352,  384,  383,  381,  380,
 /*   210 */   429,  427,  428,  379,  426,  378,  382,  431,  373,  372,
 /*   220 */   371,  369,  367,  297,  435,  366,  365,  364,  296,  363,
 /*   230 */   295,  439,  362,  361,  360,  294,  440,  359,  358,  357,
 /*   240 */   356,  355,  354,  441,  443,  444,  350,  347,  293,  290,
 /*   250 */   346,  345,  344,  343,  342,  349,  448,  348,  322,  449,
 /*   260 */   445,  313,  289,  446,  312,  309,  308,  307,  306,  311,
 /*   270 */   305,  288,  432,  433,  304,  434,  303,  302,  301,  287,
 /*   280 */   436,  412,  413,  414,  416,  415,  390,
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
  "THIS",          "LIT_INTEGER",   "LIT_HEX",       "LIT_BIN",     
  "LIT_ROM",       "LIT_REAL",      "LIT_STRING",    "TRUE",        
  "FALSE",         "NULL",          "COMMA",         "NEW",         
  "BOOL",          "INT",           "STRING",        "SEMICOLON",   
  "LBRACE",        "RBRACE",        "INCLUDE",       "RETURN",      
  "BREAK",         "CONTINUE",      "VAR",           "CLASS",       
  "PRIVATE",       "PROTECTED",     "PUBLIC",        "FUNCTION",    
  "EXTERN",        "FOR",           "IN",            "LOWER_THAN_ELSE",
  "ELSE",          "IF",            "WHILE",         "SWITCH",      
  "CASE",          "DEFAULT",       "TRY",           "CATCH",       
  "THROW",         "error",         "main",          "translation_unit",
  "statement_sequence_opt",  "statement_sequence",  "statement",     "include_statement",
  "expression_statement",  "declaration_statement",  "for_statement",  "compound_statement",
  "if_statement",  "while_statement",  "foreach_statement",  "return_statement",
  "switch_statement",  "break_statement",  "continue_statement",  "extern_declaration",
  "try_statement",  "throw_statement",  "expression",    "assignment_expression",
  "expression_opt",  "conditional_expression",  "binary_expression",  "assignment_operator",
  "unary_expression",  "postfix_expression",  "new_expression",  "primary_expression",
  "function_call",  "literal",       "id_expression",  "list_literal",
  "list_content",  "list_entry",    "type",          "argument_list",
  "positional_argument",  "positional_argument_list",  "named_argument",  "named_argument_list",
  "function_declaration",  "class_declaration",  "variable_declaration",  "declarator_sequence",
  "declarator",    "class_members",  "class_member",  "access_specifier",
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
 /*  18 */ "statement ::= extern_declaration",
 /*  19 */ "statement ::= try_statement",
 /*  20 */ "statement ::= throw_statement",
 /*  21 */ "statement ::= error",
 /*  22 */ "expression ::= assignment_expression",
 /*  23 */ "expression_opt ::=",
 /*  24 */ "expression_opt ::= expression",
 /*  25 */ "assignment_expression ::= conditional_expression",
 /*  26 */ "assignment_expression ::= binary_expression assignment_operator assignment_expression",
 /*  27 */ "assignment_operator ::= ASSIGN",
 /*  28 */ "assignment_operator ::= ASSADD",
 /*  29 */ "assignment_operator ::= ASSSUB",
 /*  30 */ "assignment_operator ::= ASSMUL",
 /*  31 */ "assignment_operator ::= ASSDIV",
 /*  32 */ "assignment_operator ::= ASSMOD",
 /*  33 */ "conditional_expression ::= binary_expression",
 /*  34 */ "conditional_expression ::= binary_expression QUESTION expression COLON assignment_expression",
 /*  35 */ "binary_expression ::= unary_expression",
 /*  36 */ "binary_expression ::= binary_expression LOGOR binary_expression",
 /*  37 */ "binary_expression ::= binary_expression LOGAND binary_expression",
 /*  38 */ "binary_expression ::= binary_expression BITOR binary_expression",
 /*  39 */ "binary_expression ::= binary_expression BITXOR binary_expression",
 /*  40 */ "binary_expression ::= binary_expression BITAND binary_expression",
 /*  41 */ "binary_expression ::= binary_expression EQUALS binary_expression",
 /*  42 */ "binary_expression ::= binary_expression NEQUALS binary_expression",
 /*  43 */ "binary_expression ::= binary_expression ST binary_expression",
 /*  44 */ "binary_expression ::= binary_expression SE binary_expression",
 /*  45 */ "binary_expression ::= binary_expression GT binary_expression",
 /*  46 */ "binary_expression ::= binary_expression GE binary_expression",
 /*  47 */ "binary_expression ::= binary_expression ADDOP binary_expression",
 /*  48 */ "binary_expression ::= binary_expression SUBOP binary_expression",
 /*  49 */ "binary_expression ::= binary_expression MULOP binary_expression",
 /*  50 */ "binary_expression ::= binary_expression DIVOP binary_expression",
 /*  51 */ "binary_expression ::= binary_expression MODOP binary_expression",
 /*  52 */ "binary_expression ::= binary_expression SEQ binary_expression",
 /*  53 */ "binary_expression ::= binary_expression SNE binary_expression",
 /*  54 */ "unary_expression ::= postfix_expression",
 /*  55 */ "unary_expression ::= SUBOP unary_expression",
 /*  56 */ "unary_expression ::= ADDADD unary_expression",
 /*  57 */ "unary_expression ::= SUBSUB unary_expression",
 /*  58 */ "unary_expression ::= NOT unary_expression",
 /*  59 */ "unary_expression ::= new_expression",
 /*  60 */ "postfix_expression ::= primary_expression",
 /*  61 */ "postfix_expression ::= postfix_expression ADDADD",
 /*  62 */ "postfix_expression ::= postfix_expression SUBSUB",
 /*  63 */ "postfix_expression ::= function_call",
 /*  64 */ "postfix_expression ::= postfix_expression DOT IDENTIFIER",
 /*  65 */ "postfix_expression ::= postfix_expression DOT function_call",
 /*  66 */ "postfix_expression ::= postfix_expression LBRACKET expression RBRACKET",
 /*  67 */ "primary_expression ::= literal",
 /*  68 */ "primary_expression ::= id_expression",
 /*  69 */ "primary_expression ::= LPAREN expression RPAREN",
 /*  70 */ "primary_expression ::= list_literal",
 /*  71 */ "primary_expression ::= THIS",
 /*  72 */ "literal ::= LIT_INTEGER",
 /*  73 */ "literal ::= LIT_HEX",
 /*  74 */ "literal ::= LIT_BIN",
 /*  75 */ "literal ::= LIT_ROM",
 /*  76 */ "literal ::= LIT_REAL",
 /*  77 */ "literal ::= LIT_STRING",
 /*  78 */ "literal ::= TRUE",
 /*  79 */ "literal ::= FALSE",
 /*  80 */ "literal ::= NULL",
 /*  81 */ "id_expression ::= IDENTIFIER",
 /*  82 */ "list_literal ::= LBRACKET list_content RBRACKET",
 /*  83 */ "list_content ::= list_entry",
 /*  84 */ "list_content ::= list_entry COMMA list_content",
 /*  85 */ "list_entry ::= expression",
 /*  86 */ "new_expression ::= NEW IDENTIFIER",
 /*  87 */ "type ::= BOOL",
 /*  88 */ "type ::= INT",
 /*  89 */ "type ::= STRING",
 /*  90 */ "type ::= IDENTIFIER",
 /*  91 */ "function_call ::= IDENTIFIER LPAREN argument_list RPAREN",
 /*  92 */ "positional_argument ::= expression",
 /*  93 */ "positional_argument_list ::= positional_argument",
 /*  94 */ "positional_argument_list ::= positional_argument_list COMMA positional_argument",
 /*  95 */ "named_argument ::= IDENTIFIER COLON expression",
 /*  96 */ "named_argument_list ::= named_argument",
 /*  97 */ "named_argument_list ::= named_argument_list COMMA named_argument",
 /*  98 */ "argument_list ::=",
 /*  99 */ "argument_list ::= positional_argument_list",
 /* 100 */ "argument_list ::= named_argument_list",
 /* 101 */ "expression_statement ::= SEMICOLON",
 /* 102 */ "expression_statement ::= expression SEMICOLON",
 /* 103 */ "compound_statement ::= LBRACE RBRACE",
 /* 104 */ "compound_statement ::= LBRACE statement_sequence RBRACE",
 /* 105 */ "include_statement ::= INCLUDE LIT_STRING SEMICOLON",
 /* 106 */ "return_statement ::= RETURN expression SEMICOLON",
 /* 107 */ "return_statement ::= RETURN SEMICOLON",
 /* 108 */ "break_statement ::= BREAK SEMICOLON",
 /* 109 */ "continue_statement ::= CONTINUE SEMICOLON",
 /* 110 */ "declaration_statement ::= function_declaration",
 /* 111 */ "declaration_statement ::= class_declaration SEMICOLON",
 /* 112 */ "declaration_statement ::= variable_declaration SEMICOLON",
 /* 113 */ "variable_declaration ::= VAR declarator_sequence",
 /* 114 */ "declarator ::= IDENTIFIER",
 /* 115 */ "declarator ::= IDENTIFIER ASSIGN expression",
 /* 116 */ "declarator_sequence ::= declarator",
 /* 117 */ "declarator_sequence ::= declarator_sequence COMMA declarator",
 /* 118 */ "class_declaration ::= CLASS IDENTIFIER LBRACE RBRACE",
 /* 119 */ "class_declaration ::= CLASS IDENTIFIER LBRACE class_members RBRACE",
 /* 120 */ "class_member ::= variable_declaration SEMICOLON",
 /* 121 */ "class_member ::= function_declaration",
 /* 122 */ "class_member ::= access_specifier variable_declaration SEMICOLON",
 /* 123 */ "class_member ::= access_specifier function_declaration",
 /* 124 */ "class_member ::= access_specifier COLON",
 /* 125 */ "access_specifier ::= PRIVATE",
 /* 126 */ "access_specifier ::= PROTECTED",
 /* 127 */ "access_specifier ::= PUBLIC",
 /* 128 */ "class_members ::= class_member",
 /* 129 */ "class_members ::= class_members class_member",
 /* 130 */ "function_declaration ::= FUNCTION IDENTIFIER LPAREN parameter_list RPAREN function_body",
 /* 131 */ "parameter ::= type IDENTIFIER",
 /* 132 */ "parameter ::= IDENTIFIER",
 /* 133 */ "opt_parameter ::= type IDENTIFIER ASSIGN expression",
 /* 134 */ "opt_parameter ::= IDENTIFIER ASSIGN expression",
 /* 135 */ "parameter_list ::=",
 /* 136 */ "parameter_list ::= parameters",
 /* 137 */ "parameter_list ::= opt_parameters",
 /* 138 */ "parameter_list ::= parameters COMMA opt_parameters",
 /* 139 */ "parameters ::= parameter",
 /* 140 */ "parameters ::= parameters COMMA parameter",
 /* 141 */ "opt_parameters ::= opt_parameter",
 /* 142 */ "opt_parameters ::= opt_parameters COMMA opt_parameter",
 /* 143 */ "function_body ::= statement",
 /* 144 */ "extern_declaration ::= EXTERN LIT_STRING type IDENTIFIER LPAREN parameter_list RPAREN SEMICOLON",
 /* 145 */ "for_statement ::= FOR LPAREN for_init_statement expression SEMICOLON expression_opt RPAREN statement",
 /* 146 */ "for_init_statement ::= expression_statement",
 /* 147 */ "for_init_statement ::= variable_declaration SEMICOLON",
 /* 148 */ "foreach_statement ::= FOR LPAREN id_expression IN expression RPAREN statement",
 /* 149 */ "foreach_statement ::= FOR LPAREN foreach_decl IN expression RPAREN statement",
 /* 150 */ "foreach_decl ::= VAR IDENTIFIER",
 /* 151 */ "if_statement ::= IF LPAREN expression RPAREN statement",
 /* 152 */ "if_statement ::= IF LPAREN expression RPAREN statement ELSE statement",
 /* 153 */ "while_statement ::= WHILE LPAREN expression RPAREN statement",
 /* 154 */ "switch_statement ::= SWITCH LPAREN expression RPAREN LBRACE switch_body RBRACE",
 /* 155 */ "switch_body ::=",
 /* 156 */ "switch_body ::= switch_body switch_case",
 /* 157 */ "switch_body ::= switch_body default_case",
 /* 158 */ "switch_case ::= CASE literal COLON case_statements",
 /* 159 */ "default_case ::= DEFAULT COLON case_statements",
 /* 160 */ "case_statements ::= statement_sequence",
 /* 161 */ "try_statement ::= TRY compound_statement CATCH LPAREN IDENTIFIER RPAREN compound_statement",
 /* 162 */ "throw_statement ::= THROW expression SEMICOLON",
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
  { 78, 1 },
  { 79, 1 },
  { 81, 1 },
  { 81, 2 },
  { 80, 0 },
  { 80, 1 },
  { 82, 1 },
  { 82, 1 },
  { 82, 1 },
  { 82, 1 },
  { 82, 1 },
  { 82, 1 },
  { 82, 1 },
  { 82, 1 },
  { 82, 1 },
  { 82, 1 },
  { 82, 1 },
  { 82, 1 },
  { 82, 1 },
  { 82, 1 },
  { 82, 1 },
  { 82, 1 },
  { 98, 1 },
  { 100, 0 },
  { 100, 1 },
  { 99, 1 },
  { 99, 3 },
  { 103, 1 },
  { 103, 1 },
  { 103, 1 },
  { 103, 1 },
  { 103, 1 },
  { 103, 1 },
  { 101, 1 },
  { 101, 5 },
  { 102, 1 },
  { 102, 3 },
  { 102, 3 },
  { 102, 3 },
  { 102, 3 },
  { 102, 3 },
  { 102, 3 },
  { 102, 3 },
  { 102, 3 },
  { 102, 3 },
  { 102, 3 },
  { 102, 3 },
  { 102, 3 },
  { 102, 3 },
  { 102, 3 },
  { 102, 3 },
  { 102, 3 },
  { 102, 3 },
  { 102, 3 },
  { 104, 1 },
  { 104, 2 },
  { 104, 2 },
  { 104, 2 },
  { 104, 2 },
  { 104, 1 },
  { 105, 1 },
  { 105, 2 },
  { 105, 2 },
  { 105, 1 },
  { 105, 3 },
  { 105, 3 },
  { 105, 4 },
  { 107, 1 },
  { 107, 1 },
  { 107, 3 },
  { 107, 1 },
  { 107, 1 },
  { 109, 1 },
  { 109, 1 },
  { 109, 1 },
  { 109, 1 },
  { 109, 1 },
  { 109, 1 },
  { 109, 1 },
  { 109, 1 },
  { 109, 1 },
  { 110, 1 },
  { 111, 3 },
  { 112, 1 },
  { 112, 3 },
  { 113, 1 },
  { 106, 2 },
  { 114, 1 },
  { 114, 1 },
  { 114, 1 },
  { 114, 1 },
  { 108, 4 },
  { 116, 1 },
  { 117, 1 },
  { 117, 3 },
  { 118, 3 },
  { 119, 1 },
  { 119, 3 },
  { 115, 0 },
  { 115, 1 },
  { 115, 1 },
  { 84, 1 },
  { 84, 2 },
  { 87, 2 },
  { 87, 3 },
  { 83, 3 },
  { 91, 3 },
  { 91, 2 },
  { 93, 2 },
  { 94, 2 },
  { 85, 1 },
  { 85, 2 },
  { 85, 2 },
  { 122, 2 },
  { 124, 1 },
  { 124, 3 },
  { 123, 1 },
  { 123, 3 },
  { 121, 4 },
  { 121, 5 },
  { 126, 2 },
  { 126, 1 },
  { 126, 3 },
  { 126, 2 },
  { 126, 2 },
  { 127, 1 },
  { 127, 1 },
  { 127, 1 },
  { 125, 1 },
  { 125, 2 },
  { 120, 6 },
  { 130, 2 },
  { 130, 1 },
  { 131, 4 },
  { 131, 3 },
  { 128, 0 },
  { 128, 1 },
  { 128, 1 },
  { 128, 3 },
  { 132, 1 },
  { 132, 3 },
  { 133, 1 },
  { 133, 3 },
  { 129, 1 },
  { 95, 8 },
  { 86, 8 },
  { 134, 1 },
  { 134, 2 },
  { 90, 7 },
  { 90, 7 },
  { 135, 2 },
  { 88, 5 },
  { 88, 7 },
  { 89, 5 },
  { 92, 7 },
  { 136, 0 },
  { 136, 2 },
  { 136, 2 },
  { 137, 4 },
  { 138, 3 },
  { 139, 1 },
  { 96, 7 },
  { 97, 3 },
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
#line 75 "cscript.in"
{ p->SetRoot(yymsp[0].minor.yy151); }
#line 1301 "cscript.c"
        break;
      case 1:
#line 78 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(translation_unit, yymsp[0].minor.yy151); }
#line 1306 "cscript.c"
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
      case 19:
      case 20:
      case 22:
      case 24:
      case 25:
      case 33:
      case 35:
      case 54:
      case 59:
      case 60:
      case 63:
      case 67:
      case 68:
      case 70:
      case 92:
      case 110:
      case 113:
      case 116:
      case 143:
      case 146:
      case 160:
#line 81 "cscript.in"
{ yygotominor.yy151 = yymsp[0].minor.yy151; }
#line 1346 "cscript.c"
        break;
      case 3:
#line 82 "cscript.in"
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
#line 1360 "cscript.c"
        break;
      case 4:
      case 23:
#line 95 "cscript.in"
{ yygotominor.yy151 = 0; }
#line 1366 "cscript.c"
        break;
      case 21:
      case 101:
#line 116 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(empty_statement); }
#line 1372 "cscript.c"
        break;
      case 26:
#line 132 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(assignment_expression, yymsp[-1].minor.yy54, yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1377 "cscript.c"
        break;
      case 27:
#line 136 "cscript.in"
{ yygotominor.yy54 = op_assign; }
#line 1382 "cscript.c"
        break;
      case 28:
#line 137 "cscript.in"
{ yygotominor.yy54 = op_assadd; }
#line 1387 "cscript.c"
        break;
      case 29:
#line 138 "cscript.in"
{ yygotominor.yy54 = op_asssub; }
#line 1392 "cscript.c"
        break;
      case 30:
#line 139 "cscript.in"
{ yygotominor.yy54 = op_assmul; }
#line 1397 "cscript.c"
        break;
      case 31:
#line 140 "cscript.in"
{ yygotominor.yy54 = op_assdiv; }
#line 1402 "cscript.c"
        break;
      case 32:
#line 141 "cscript.in"
{ yygotominor.yy54 = op_assmod; }
#line 1407 "cscript.c"
        break;
      case 34:
#line 145 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(ternary_expression, yymsp[-4].minor.yy151, yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1412 "cscript.c"
        break;
      case 36:
#line 149 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(binary_expression, op_logor,   yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1417 "cscript.c"
        break;
      case 37:
#line 150 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(binary_expression, op_logand,  yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1422 "cscript.c"
        break;
      case 38:
#line 151 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(binary_expression, op_bitor,   yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1427 "cscript.c"
        break;
      case 39:
#line 152 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(binary_expression, op_bitxor,  yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1432 "cscript.c"
        break;
      case 40:
#line 153 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(binary_expression, op_bitand,  yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1437 "cscript.c"
        break;
      case 41:
#line 154 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(binary_expression, op_eq,   yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1442 "cscript.c"
        break;
      case 42:
#line 155 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(binary_expression, op_ne,   yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1447 "cscript.c"
        break;
      case 43:
#line 156 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(binary_expression, op_lt,   yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1452 "cscript.c"
        break;
      case 44:
#line 157 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(binary_expression, op_le,   yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1457 "cscript.c"
        break;
      case 45:
#line 158 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(binary_expression, op_gt,   yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1462 "cscript.c"
        break;
      case 46:
#line 159 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(binary_expression, op_ge,   yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1467 "cscript.c"
        break;
      case 47:
#line 160 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(binary_expression, op_add,  yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1472 "cscript.c"
        break;
      case 48:
#line 161 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(binary_expression, op_sub,  yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1477 "cscript.c"
        break;
      case 49:
#line 162 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(binary_expression, op_mul,  yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1482 "cscript.c"
        break;
      case 50:
#line 163 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(binary_expression, op_div,  yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1487 "cscript.c"
        break;
      case 51:
#line 164 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(binary_expression, op_mod,  yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1492 "cscript.c"
        break;
      case 52:
#line 165 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(binary_expression, op_seq,  yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1497 "cscript.c"
        break;
      case 53:
#line 166 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(binary_expression, op_sne,  yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1502 "cscript.c"
        break;
      case 55:
#line 170 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(prefix_expression, op_negate, yymsp[0].minor.yy151); }
#line 1507 "cscript.c"
        break;
      case 56:
#line 171 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(prefix_expression, op_preinc, yymsp[0].minor.yy151); }
#line 1512 "cscript.c"
        break;
      case 57:
#line 172 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(prefix_expression, op_predec, yymsp[0].minor.yy151); }
#line 1517 "cscript.c"
        break;
      case 58:
#line 173 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(prefix_expression, op_not,    yymsp[0].minor.yy151); }
#line 1522 "cscript.c"
        break;
      case 61:
#line 178 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(postfix_expression, op_postinc, yymsp[-1].minor.yy151); }
#line 1527 "cscript.c"
        break;
      case 62:
#line 179 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(postfix_expression, op_postdec, yymsp[-1].minor.yy151); }
#line 1532 "cscript.c"
        break;
      case 64:
#line 181 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(member_expression, yymsp[-2].minor.yy151, String(yymsp[0].minor.yy0)); }
#line 1537 "cscript.c"
        break;
      case 65:
#line 182 "cscript.in"
{ yygotominor.yy151 = yymsp[0].minor.yy151; yymsp[0].minor.yy151->m_a3 = yymsp[-2].minor.yy151; }
#line 1542 "cscript.c"
        break;
      case 66:
#line 183 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(index_expression, yymsp[-3].minor.yy151, yymsp[-1].minor.yy151); }
#line 1547 "cscript.c"
        break;
      case 69:
      case 111:
      case 112:
      case 147:
#line 188 "cscript.in"
{ yygotominor.yy151 = yymsp[-1].minor.yy151; }
#line 1555 "cscript.c"
        break;
      case 71:
#line 190 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(this_expression); }
#line 1560 "cscript.c"
        break;
      case 72:
#line 193 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(literal_value, Variant(String(yymsp[0].minor.yy0), Variant::stInt)); }
#line 1565 "cscript.c"
        break;
      case 73:
#line 194 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(literal_value, Variant(hex2dec(String(yymsp[0].minor.yy0).c_str()))); }
#line 1570 "cscript.c"
        break;
      case 74:
#line 195 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(literal_value, Variant(bin2dec(String(yymsp[0].minor.yy0).c_str()))); }
#line 1575 "cscript.c"
        break;
      case 75:
#line 196 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(literal_value, Variant(rom2dec(String(yymsp[0].minor.yy0).c_str()))); }
#line 1580 "cscript.c"
        break;
      case 76:
#line 197 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(literal_value, Variant(String(yymsp[0].minor.yy0), Variant::stReal)); }
#line 1585 "cscript.c"
        break;
      case 77:
#line 198 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(literal_value, Variant(String(yymsp[0].minor.yy0), Variant::stString)); }
#line 1590 "cscript.c"
        break;
      case 78:
#line 199 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(literal_value, Variant(true));    }
#line 1595 "cscript.c"
        break;
      case 79:
#line 200 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(literal_value, Variant(false));   }
#line 1600 "cscript.c"
        break;
      case 80:
#line 201 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(literal_value, Variant());        }
#line 1605 "cscript.c"
        break;
      case 81:
#line 204 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(lvalue, String(yymsp[0].minor.yy0)); }
#line 1610 "cscript.c"
        break;
      case 82:
#line 207 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(list_literal, yymsp[-1].minor.yy151); }
#line 1615 "cscript.c"
        break;
      case 83:
#line 208 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(list_content, yymsp[0].minor.yy151); }
#line 1620 "cscript.c"
        break;
      case 84:
#line 209 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(list_content, yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1625 "cscript.c"
        break;
      case 85:
#line 211 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(list_entry, yymsp[0].minor.yy151); }
#line 1630 "cscript.c"
        break;
      case 86:
#line 214 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(new_expression, String(yymsp[0].minor.yy0)); }
#line 1635 "cscript.c"
        break;
      case 87:
#line 221 "cscript.in"
{ yygotominor.yy151 = new Ast(builtin_type, Variant::stBool);    }
#line 1640 "cscript.c"
        break;
      case 88:
#line 222 "cscript.in"
{ yygotominor.yy151 = new Ast(builtin_type, Variant::stInt);     }
#line 1645 "cscript.c"
        break;
      case 89:
#line 223 "cscript.in"
{ yygotominor.yy151 = new Ast(builtin_type, Variant::stString);  }
#line 1650 "cscript.c"
        break;
      case 90:
#line 224 "cscript.in"
{ yygotominor.yy151 = new Ast(class_type, String(yymsp[0].minor.yy0));            }
#line 1655 "cscript.c"
        break;
      case 91:
#line 232 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(function_call, String(yymsp[-3].minor.yy0), yymsp[-1].minor.yy151); }
#line 1660 "cscript.c"
        break;
      case 93:
      case 96:
      case 139:
      case 141:
#line 239 "cscript.in"
{ yygotominor.yy15 = new AstList; yygotominor.yy15->push_back(yymsp[0].minor.yy151); }
#line 1668 "cscript.c"
        break;
      case 94:
      case 97:
      case 140:
      case 142:
#line 240 "cscript.in"
{ yygotominor.yy15 = yymsp[-2].minor.yy15; yymsp[-2].minor.yy15->push_back(yymsp[0].minor.yy151); }
#line 1676 "cscript.c"
        break;
      case 95:
#line 243 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(named_argument, String(yymsp[-2].minor.yy0), yymsp[0].minor.yy151); }
#line 1681 "cscript.c"
        break;
      case 98:
#line 251 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(positional_arguments, new AstList); }
#line 1686 "cscript.c"
        break;
      case 99:
#line 252 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(positional_arguments, yymsp[0].minor.yy15); }
#line 1691 "cscript.c"
        break;
      case 100:
#line 253 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(named_arguments,      yymsp[0].minor.yy15); }
#line 1696 "cscript.c"
        break;
      case 102:
#line 262 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(expression_statement, yymsp[-1].minor.yy151); }
#line 1701 "cscript.c"
        break;
      case 103:
#line 265 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(compound_statement); }
#line 1706 "cscript.c"
        break;
      case 104:
#line 266 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(compound_statement, yymsp[-1].minor.yy151); }
#line 1711 "cscript.c"
        break;
      case 105:
#line 269 "cscript.in"
{ p->Parse(yymsp[-1].minor.yy0); yygotominor.yy151 = p->GetRoot(); }
#line 1716 "cscript.c"
        break;
      case 106:
#line 272 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(return_statement, yymsp[-1].minor.yy151); }
#line 1721 "cscript.c"
        break;
      case 107:
#line 273 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(return_statement);    }
#line 1726 "cscript.c"
        break;
      case 108:
#line 276 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(break_statement); }
#line 1731 "cscript.c"
        break;
      case 109:
#line 277 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(continue_statement); }
#line 1736 "cscript.c"
        break;
      case 114:
#line 291 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(variable_declaration, String(yymsp[0].minor.yy0));    }
#line 1741 "cscript.c"
        break;
      case 115:
#line 292 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(variable_declaration, String(yymsp[-2].minor.yy0), yymsp[0].minor.yy151); }
#line 1746 "cscript.c"
        break;
      case 117:
#line 295 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(declaration_sequence, yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1751 "cscript.c"
        break;
      case 118:
#line 302 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(class_declaration, String(yymsp[-2].minor.yy0)); }
#line 1756 "cscript.c"
        break;
      case 119:
#line 303 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(class_declaration, String(yymsp[-3].minor.yy0), yymsp[-1].minor.yy15); }
#line 1761 "cscript.c"
        break;
      case 120:
#line 306 "cscript.in"
{ yygotominor.yy151 = yymsp[-1].minor.yy151; yymsp[-1].minor.yy151->m_props["access"] = accessDefault; }
#line 1766 "cscript.c"
        break;
      case 121:
#line 307 "cscript.in"
{ yygotominor.yy151 = yymsp[0].minor.yy151; yymsp[0].minor.yy151->m_props["access"] = accessDefault; }
#line 1771 "cscript.c"
        break;
      case 122:
#line 308 "cscript.in"
{ yygotominor.yy151 = yymsp[-1].minor.yy151; yymsp[-1].minor.yy151->m_props["access"] = yymsp[-2].minor.yy68; }
#line 1776 "cscript.c"
        break;
      case 123:
#line 309 "cscript.in"
{ yygotominor.yy151 = yymsp[0].minor.yy151; yymsp[0].minor.yy151->m_props["access"] = yymsp[-1].minor.yy68; }
#line 1781 "cscript.c"
        break;
      case 124:
#line 310 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(access_specifier, yymsp[-1].minor.yy68); }
#line 1786 "cscript.c"
        break;
      case 125:
#line 314 "cscript.in"
{ yygotominor.yy68 = accessPrivate;   }
#line 1791 "cscript.c"
        break;
      case 126:
#line 315 "cscript.in"
{ yygotominor.yy68 = accessProtected; }
#line 1796 "cscript.c"
        break;
      case 127:
#line 316 "cscript.in"
{ yygotominor.yy68 = accessPublic;    }
#line 1801 "cscript.c"
        break;
      case 128:
#line 320 "cscript.in"
{ 
  yygotominor.yy15 = new AstList;
  yygotominor.yy15->push_back(yymsp[0].minor.yy151);
}
#line 1809 "cscript.c"
        break;
      case 129:
#line 324 "cscript.in"
{ 
  yygotominor.yy15 = yymsp[-1].minor.yy15;
  yygotominor.yy15->push_back(yymsp[0].minor.yy151);
}
#line 1817 "cscript.c"
        break;
      case 130:
#line 335 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(function_declaration, String(yymsp[-4].minor.yy0), yymsp[-2].minor.yy15, yymsp[0].minor.yy151); }
#line 1822 "cscript.c"
        break;
      case 131:
#line 338 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(parameter, String(yymsp[0].minor.yy0), yymsp[-1].minor.yy151); }
#line 1827 "cscript.c"
        break;
      case 132:
#line 339 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(parameter, String(yymsp[0].minor.yy0)); }
#line 1832 "cscript.c"
        break;
      case 133:
#line 342 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(parameter, String(yymsp[-2].minor.yy0), yymsp[-3].minor.yy151,         yymsp[0].minor.yy151); }
#line 1837 "cscript.c"
        break;
      case 134:
#line 343 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(parameter, String(yymsp[-2].minor.yy0), AstData(), yymsp[0].minor.yy151); }
#line 1842 "cscript.c"
        break;
      case 135:
      case 155:
#line 347 "cscript.in"
{ yygotominor.yy15 = new AstList; }
#line 1848 "cscript.c"
        break;
      case 136:
      case 137:
#line 348 "cscript.in"
{ yygotominor.yy15 = yymsp[0].minor.yy15; }
#line 1854 "cscript.c"
        break;
      case 138:
#line 350 "cscript.in"
{ yygotominor.yy15 = yymsp[-2].minor.yy15; yymsp[-2].minor.yy15->adopt(*yymsp[0].minor.yy15); }
#line 1859 "cscript.c"
        break;
      case 144:
#line 373 "cscript.in"
{ 
  yygotominor.yy151 = new Ast(extern_declaration, String(yymsp[-4].minor.yy0), yymsp[-2].minor.yy15, String(yymsp[-6].minor.yy0), yymsp[-5].minor.yy151); 
}
#line 1866 "cscript.c"
        break;
      case 145:
#line 383 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(for_statement, yymsp[-5].minor.yy151, yymsp[-4].minor.yy151, yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1871 "cscript.c"
        break;
      case 148:
      case 149:
#line 394 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(foreach_statement, yymsp[-4].minor.yy151, yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1877 "cscript.c"
        break;
      case 150:
#line 396 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(variable_declaration, String(yymsp[0].minor.yy0)); }
#line 1882 "cscript.c"
        break;
      case 151:
#line 407 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(if_statement, yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1887 "cscript.c"
        break;
      case 152:
#line 408 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(if_statement, yymsp[-4].minor.yy151, yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1892 "cscript.c"
        break;
      case 153:
#line 416 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(while_statement, yymsp[-2].minor.yy151,  yymsp[0].minor.yy151); }
#line 1897 "cscript.c"
        break;
      case 154:
#line 424 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(switch_statement, yymsp[-4].minor.yy151, yymsp[-1].minor.yy15); }
#line 1902 "cscript.c"
        break;
      case 156:
      case 157:
#line 429 "cscript.in"
{ yygotominor.yy15 = yymsp[-1].minor.yy15; yygotominor.yy15->push_back(yymsp[0].minor.yy151); }
#line 1908 "cscript.c"
        break;
      case 158:
#line 433 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(switch_case, yymsp[-2].minor.yy151, yymsp[0].minor.yy151); }
#line 1913 "cscript.c"
        break;
      case 159:
#line 436 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(default_case, yymsp[0].minor.yy151); }
#line 1918 "cscript.c"
        break;
      case 161:
#line 446 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(try_statement, yymsp[-5].minor.yy151, String(yymsp[-2].minor.yy0), yymsp[0].minor.yy151); }
#line 1923 "cscript.c"
        break;
      case 162:
#line 448 "cscript.in"
{ yygotominor.yy151 = p->AllocAst(throw_statement, yymsp[-1].minor.yy151); }
#line 1928 "cscript.c"
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
#line 54 "cscript.in"

  p->OnParseFailure();
#line 1976 "cscript.c"
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
#line 57 "cscript.in"

  p->OnSyntaxError();
#line 1994 "cscript.c"
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

