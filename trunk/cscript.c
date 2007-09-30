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
#define YYNOCODE 146
#define YYACTIONTYPE unsigned short int
#define CScriptParseTOKENTYPE  Token 
typedef union {
  CScriptParseTOKENTYPE yy0;
  AccessTypes yy68;
  Ast* yy71;
  AstList* yy155;
  opcodes yy264;
  int yy291;
} YYMINORTYPE;
#ifndef YYSTACKDEPTH
#define YYSTACKDEPTH 100
#endif
#define CScriptParseARG_SDECL  Parser* p ;
#define CScriptParseARG_PDECL , Parser* p 
#define CScriptParseARG_FETCH  Parser* p  = yypParser->p 
#define CScriptParseARG_STORE yypParser->p  = p 
#define YYNSTATE 299
#define YYNRULE 172
#define YYERRORSYMBOL 79
#define YYERRSYMDT yy291
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
 /*     0 */   264,  472,  291,  290,   11,  289,  286,  283,  282,  280,
 /*    10 */   279,  278,  277,  276,  274,  273,  270,  269,  267,  266,
 /*    20 */   265,  137,  261,   65,  259,   70,  167,  257,   95,  247,
 /*    30 */   246,  245,  244,  242,  240,   55,   50,   51,   52,   53,
 /*    40 */    54,  159,  166,  175,  154,  156,   75,   56,   57,   58,
 /*    50 */   161,  113,   17,  260,   24,  125,  239,  237,  236,  235,
 /*    60 */   234,  233,  232,  230,  229,  228,  164,  130,   52,   53,
 /*    70 */    54,  191,    3,  298,  142,   20,  149,  151,   68,  165,
 /*    80 */    12,  121,  174,  150,  155,  131,  133,   55,  296,   66,
 /*    90 */   135,  104,  117,   32,   26,   74,   78,  263,   29,   56,
 /*   100 */    57,   58,   14,  113,   17,  178,   24,  152,  239,  237,
 /*   110 */   236,  235,  234,  233,  232,  230,  229,  228,   77,  130,
 /*   120 */   124,   77,  179,  191,    3,  187,  142,   20,  149,  151,
 /*   130 */    68,  165,  251,  252,   32,  150,  262,  131,  133,   55,
 /*   140 */    62,  454,  135,  104,  117,    3,   79,   74,  144,  209,
 /*   150 */    29,   56,   57,   58,   68,  113,   17,  108,   24,  150,
 /*   160 */   239,  237,  236,  235,  234,  233,  232,  230,  229,  228,
 /*   170 */   227,  130,   34,  210,   94,  191,    3,  103,  142,   20,
 /*   180 */   149,  151,   68,  165,  170,  173,  110,  150,  105,  131,
 /*   190 */   133,  183,  184,  185,  135,  104,  117,   67,  172,   74,
 /*   200 */    16,  268,   29,  215,  264,  183,  184,  185,   13,  289,
 /*   210 */   286,  283,  282,  280,  279,  278,  277,  276,  274,  273,
 /*   220 */   270,  269,  267,  266,  265,  137,  261,  177,  259,   70,
 /*   230 */   176,  257,   95,  247,  246,  245,  244,  242,  240,  174,
 /*   240 */   148,  155,  106,  127,   63,  297,   66,  175,  154,  156,
 /*   250 */    38,   41,   47,   42,   43,   37,   39,   44,   45,   46,
 /*   260 */    48,   49,   50,   51,   52,   53,   54,  275,  180,  264,
 /*   270 */    36,   59,  100,   13,  289,  286,  283,  282,  280,  279,
 /*   280 */   278,  277,  276,  274,  273,  270,  269,  267,  266,  265,
 /*   290 */   137,  261,   31,  259,   70,  182,  257,   95,  247,  246,
 /*   300 */   245,  244,  242,  240,  256,  255,   61,   76,    8,   22,
 /*   310 */   287,  207,  175,  154,  156,  198,   64,  259,   70,  186,
 /*   320 */   257,   95,  247,  246,  245,  244,  242,  240,  264,   26,
 /*   330 */   143,   25,  272,  193,  286,  283,  282,  280,  279,  278,
 /*   340 */   277,  276,  274,  273,  270,  269,  267,  266,  265,  137,
 /*   350 */   261,    6,  259,   70,  250,  257,   95,  247,  246,  245,
 /*   360 */   244,  242,  240,  254,   95,  247,  246,  245,  244,  242,
 /*   370 */   240,  175,  154,  156,   71,  134,   21,  264,   19,    1,
 /*   380 */   192,    5,  289,  286,  283,  282,  280,  279,  278,  277,
 /*   390 */   276,  274,  273,  270,  269,  267,  266,  265,  137,  261,
 /*   400 */    80,  259,   70,    7,  257,   95,  247,  246,  245,  244,
 /*   410 */   242,  240,    4,   14,  190,   18,  111,    9,  264,   23,
 /*   420 */   175,  154,  156,  284,  286,  283,  282,  280,  279,  278,
 /*   430 */   277,  276,  274,  273,  270,  269,  267,  266,  265,  137,
 /*   440 */   261,   27,  259,   70,    2,  257,   95,  247,  246,  245,
 /*   450 */   244,  242,  240,   35,   15,  271,   72,  223,  139,  219,
 /*   460 */   264,  175,  154,  156,  224,  107,  286,  283,  282,  280,
 /*   470 */   279,  278,  277,  276,  274,  273,  270,  269,  267,  266,
 /*   480 */   265,  137,  261,   28,  259,   70,   73,  257,   95,  247,
 /*   490 */   246,  245,  244,  242,  240,  129,  122,  208,  128,  205,
 /*   500 */   126,  264,  473,  175,  154,  156,  288,  286,  283,  282,
 /*   510 */   280,  279,  278,  277,  276,  274,  273,  270,  269,  267,
 /*   520 */   266,  265,  137,  261,   10,  259,   70,   60,  257,   95,
 /*   530 */   247,  246,  245,  244,  242,  240,  241,  473,  473,  473,
 /*   540 */   473,  473,  264,  473,  175,  154,  156,  231,  286,  283,
 /*   550 */   282,  280,  279,  278,  277,  276,  274,  273,  270,  269,
 /*   560 */   267,  266,  265,  137,  261,  473,  259,   70,  473,  257,
 /*   570 */    95,  247,  246,  245,  244,  242,  240,  473,  473,  473,
 /*   580 */   473,  473,  473,  264,  473,  175,  154,  156,  238,  286,
 /*   590 */   283,  282,  280,  279,  278,  277,  276,  274,  273,  270,
 /*   600 */   269,  267,  266,  265,  137,  261,  473,  259,   70,  473,
 /*   610 */   257,   95,  247,  246,  245,  244,  242,  240,  473,  473,
 /*   620 */   473,  473,  473,  473,  264,  473,  175,  154,  156,  292,
 /*   630 */   286,  283,  282,  280,  279,  278,  277,  276,  274,  273,
 /*   640 */   270,  269,  267,  266,  265,  137,  261,  473,  259,   70,
 /*   650 */   473,  257,   95,  247,  246,  245,  244,  242,  240,  473,
 /*   660 */   473,  473,  473,  473,  473,  264,  473,  175,  154,  156,
 /*   670 */   243,  286,  283,  282,  280,  279,  278,  277,  276,  274,
 /*   680 */   273,  270,  269,  267,  266,  265,  137,  261,  473,  259,
 /*   690 */    70,  473,  257,   95,  247,  246,  245,  244,  242,  240,
 /*   700 */   253,   95,  247,  246,  245,  244,  242,  240,  175,  154,
 /*   710 */   156,   40,   38,   41,   47,   42,   43,   37,   39,   44,
 /*   720 */    45,   46,   48,   49,   50,   51,   52,   53,   54,  206,
 /*   730 */   204,  202,  201,  200,  199,   30,   55,   39,   44,   45,
 /*   740 */    46,   48,   49,   50,   51,   52,   53,   54,   56,   57,
 /*   750 */    58,  473,  113,   17,  473,   24,  196,  239,  237,  236,
 /*   760 */   235,  234,  233,  232,  230,  229,  228,  473,  130,   55,
 /*   770 */   473,  473,  191,  183,  184,  185,  473,  473,  473,   69,
 /*   780 */   473,   56,   57,   58,  473,  113,   17,  473,   24,  473,
 /*   790 */   239,  237,  236,  235,  234,  233,  232,  230,  229,  228,
 /*   800 */   473,  130,   55,  473,   96,  181,  257,   95,  247,  246,
 /*   810 */   245,  244,  242,  240,   56,   57,   58,  473,  113,   17,
 /*   820 */   473,   24,  473,  239,  237,  236,  235,  234,  233,  232,
 /*   830 */   230,  229,  228,  473,  130,   55,  197,  473,  257,   95,
 /*   840 */   247,  246,  245,  244,  242,  240,  473,   56,   57,   58,
 /*   850 */   473,   99,   17,  473,   24,  473,  239,  237,  236,  235,
 /*   860 */   234,  233,  232,  230,  229,  228,  473,  130,   21,  214,
 /*   870 */   261,  473,  259,   70,  473,  257,   95,  247,  246,  245,
 /*   880 */   244,  242,  240,  285,  431,  473,  116,  213,  109,  211,
 /*   890 */   163,  473,  473,  473,  473,  431,  473,  137,  261,  473,
 /*   900 */   259,   70,  473,  257,   95,  247,  246,  245,  244,  157,
 /*   910 */   240,  473,  473,  189,  473,  473,  225,  261,  473,  259,
 /*   920 */    70,  145,  257,   95,  247,  246,  245,  244,  242,  240,
 /*   930 */   226,  153,  473,  473,   33,  147,  473,  473,   41,   47,
 /*   940 */    42,   43,   37,   39,   44,   45,   46,   48,   49,   50,
 /*   950 */    51,   52,   53,   54,   47,   42,   43,   37,   39,   44,
 /*   960 */    45,   46,   48,   49,   50,   51,   52,   53,   54,   42,
 /*   970 */    43,   37,   39,   44,   45,   46,   48,   49,   50,   51,
 /*   980 */    52,   53,   54,  225,  261,  473,  259,   70,  473,  257,
 /*   990 */    95,  247,  246,  245,  244,  242,  240,  158,  153,   43,
 /*  1000 */    37,   39,   44,   45,   46,   48,   49,   50,   51,   52,
 /*  1010 */    53,   54,  473,  473,  473,  473,  473,  281,  261,  141,
 /*  1020 */   259,   70,  473,  257,   95,  247,  246,  245,  244,  242,
 /*  1030 */   240,  214,  261,  473,  259,   70,  473,  257,   95,  247,
 /*  1040 */   246,  245,  244,  242,  240,  473,  473,  203,  261,  212,
 /*  1050 */   259,   70,  473,  257,   95,  247,  246,  245,  244,  242,
 /*  1060 */   240,  112,  261,  473,  259,   70,  473,  257,   95,  247,
 /*  1070 */   246,  245,  244,  242,  240,  146,  261,  473,  259,   70,
 /*  1080 */   473,  257,   95,  247,  246,  245,  244,  242,  240,  473,
 /*  1090 */   132,  261,  473,  259,   70,  473,  257,   95,  247,  246,
 /*  1100 */   245,  244,  242,  240,  188,  261,  473,  259,   70,  473,
 /*  1110 */   257,   95,  247,  246,  245,  244,  242,  240,  115,  261,
 /*  1120 */   473,  259,   70,  473,  257,   95,  247,  246,  245,  244,
 /*  1130 */   242,  240,  168,  261,  473,  259,   70,  473,  257,   95,
 /*  1140 */   247,  246,  245,  244,  242,  240,  473,  473,  120,  261,
 /*  1150 */   473,  259,   70,  473,  257,   95,  247,  246,  245,  244,
 /*  1160 */   242,  240,  140,  261,  473,  259,   70,  473,  257,   95,
 /*  1170 */   247,  246,  245,  244,  242,  240,  222,  261,  473,  259,
 /*  1180 */    70,  473,  257,   95,  247,  246,  245,  244,  242,  240,
 /*  1190 */   473,  136,  261,  473,  259,   70,  473,  257,   95,  247,
 /*  1200 */   246,  245,  244,  242,  240,  101,  261,  473,  259,   70,
 /*  1210 */   473,  257,   95,  247,  246,  245,  244,  242,  240,  162,
 /*  1220 */   261,  473,  259,   70,  473,  257,   95,  247,  246,  245,
 /*  1230 */   244,  242,  240,  123,  261,  473,  259,   70,  473,  257,
 /*  1240 */    95,  247,  246,  245,  244,  242,  240,  473,  473,  119,
 /*  1250 */   261,  473,  259,   70,  473,  257,   95,  247,  246,  245,
 /*  1260 */   244,  242,  240,  258,  473,  259,   70,  473,  257,   95,
 /*  1270 */   247,  246,  245,  244,  242,  240,   97,  473,  257,   95,
 /*  1280 */   247,  246,  245,  244,  242,  240,  473,  195,  473,  257,
 /*  1290 */    95,  247,  246,  245,  244,  242,  240,   93,  473,  257,
 /*  1300 */    95,  247,  246,  245,  244,  242,  240,  194,  473,  257,
 /*  1310 */    95,  247,  246,  245,  244,  242,  240,   92,  473,  257,
 /*  1320 */    95,  247,  246,  245,  244,  242,  240,  237,  236,  235,
 /*  1330 */   234,  233,  232,  230,  229,  228,   84,  473,  257,   95,
 /*  1340 */   247,  246,  245,  244,  242,  240,   45,   46,   48,   49,
 /*  1350 */    50,   51,   52,   53,   54,   91,  473,  257,   95,  247,
 /*  1360 */   246,  245,  244,  242,  240,   90,  473,  257,   95,  247,
 /*  1370 */   246,  245,  244,  242,  240,   89,  473,  257,   95,  247,
 /*  1380 */   246,  245,  244,  242,  240,   86,  473,  257,   95,  247,
 /*  1390 */   246,  245,  244,  242,  240,   82,  473,  257,   95,  247,
 /*  1400 */   246,  245,  244,  242,  240,   88,  473,  257,   95,  247,
 /*  1410 */   246,  245,  244,  242,  240,   81,  473,  257,   95,  247,
 /*  1420 */   246,  245,  244,  242,  240,   83,  473,  257,   95,  247,
 /*  1430 */   246,  245,  244,  242,  240,   85,  473,  257,   95,  247,
 /*  1440 */   246,  245,  244,  242,  240,   87,  473,  257,   95,  247,
 /*  1450 */   246,  245,  244,  242,  240,  249,   95,  247,  246,  245,
 /*  1460 */   244,  242,  240,   98,  248,   95,  247,  246,  245,  244,
 /*  1470 */   242,  240,   98,  473,  473,  473,  473,  118,  473,  221,
 /*  1480 */   218,  220,  160,  114,  473,  473,  138,  473,  221,  218,
 /*  1490 */   220,  160,  114,  171,   98,  473,  473,  473,   68,  169,
 /*  1500 */   293,  294,  295,  150,   68,  473,  293,  294,  295,  150,
 /*  1510 */   217,  218,  216,  473,  102,
};
static const YYCODETYPE yy_lookahead[] = {
 /*     0 */    79,   80,   81,   82,   83,   84,   85,   86,   87,   88,
 /*    10 */    89,   90,   91,   92,   93,   94,   95,   96,   97,   98,
 /*    20 */    99,  100,  101,  139,  103,  104,  126,  106,  107,  108,
 /*    30 */   109,  110,  111,  112,  113,   15,   14,   15,   16,   17,
 /*    40 */    18,  125,  126,  122,  123,  124,   46,   27,   28,   29,
 /*    50 */    31,   31,   32,  144,   34,    5,   36,   37,   38,   39,
 /*    60 */    40,   41,   42,   43,   44,   45,   31,   47,   16,   17,
 /*    70 */    18,   51,   52,   53,   54,   55,   56,   57,   58,   59,
 /*    80 */    35,   31,  122,   63,  124,   65,   66,   15,  128,  129,
 /*    90 */    70,   71,   72,   19,   26,   75,  143,  144,   78,   27,
 /*   100 */    28,   29,   34,   31,   32,  122,   34,  124,   36,   37,
 /*   110 */    38,   39,   40,   41,   42,   43,   44,   45,   77,   47,
 /*   120 */    76,   77,   26,   51,   52,   53,   54,   55,   56,   57,
 /*   130 */    58,   59,  140,  141,   19,   63,   89,   65,   66,   15,
 /*   140 */    46,   67,   70,   71,   72,   52,   46,   75,    5,  110,
 /*   150 */    78,   27,   28,   29,   58,   31,   32,   31,   34,   63,
 /*   160 */    36,   37,   38,   39,   40,   41,   42,   43,   44,   45,
 /*   170 */    33,   47,   67,  120,   31,   51,   52,   31,   54,   55,
 /*   180 */    56,   57,   58,   59,   51,   51,  116,   63,   31,   65,
 /*   190 */    66,   48,   49,   50,   70,   71,   72,   89,   51,   75,
 /*   200 */    46,   89,   78,  133,   79,   48,   49,   50,   83,   84,
 /*   210 */    85,   86,   87,   88,   89,   90,   91,   92,   93,   94,
 /*   220 */    95,   96,   97,   98,   99,  100,  101,   51,  103,  104,
 /*   230 */    51,  106,  107,  108,  109,  110,  111,  112,  113,  122,
 /*   240 */    31,  124,  111,  116,  127,  128,  129,  122,  123,  124,
 /*   250 */     2,    3,    4,    5,    6,    7,    8,    9,   10,   11,
 /*   260 */    12,   13,   14,   15,   16,   17,   18,  142,   51,   79,
 /*   270 */   105,   34,   31,   83,   84,   85,   86,   87,   88,   89,
 /*   280 */    90,   91,   92,   93,   94,   95,   96,   97,   98,   99,
 /*   290 */   100,  101,   67,  103,  104,   51,  106,  107,  108,  109,
 /*   300 */   110,  111,  112,  113,   27,   28,   52,   30,   35,   32,
 /*   310 */    51,   31,  122,  123,  124,  101,   46,  103,  104,   51,
 /*   320 */   106,  107,  108,  109,  110,  111,  112,  113,   79,   26,
 /*   330 */    42,   34,  142,   84,   85,   86,   87,   88,   89,   90,
 /*   340 */    91,   92,   93,   94,   95,   96,   97,   98,   99,  100,
 /*   350 */   101,   35,  103,  104,   53,  106,  107,  108,  109,  110,
 /*   360 */   111,  112,  113,  106,  107,  108,  109,  110,  111,  112,
 /*   370 */   113,  122,  123,  124,   73,   74,   19,   79,   51,   26,
 /*   380 */   131,   83,   84,   85,   86,   87,   88,   89,   90,   91,
 /*   390 */    92,   93,   94,   95,   96,   97,   98,   99,  100,  101,
 /*   400 */    52,  103,  104,   69,  106,  107,  108,  109,  110,  111,
 /*   410 */   112,  113,   35,   34,   51,   46,   31,   35,   79,   19,
 /*   420 */   122,  123,  124,   84,   85,   86,   87,   88,   89,   90,
 /*   430 */    91,   92,   93,   94,   95,   96,   97,   98,   99,  100,
 /*   440 */   101,   34,  103,  104,   26,  106,  107,  108,  109,  110,
 /*   450 */   111,  112,  113,   26,   34,   51,   42,   31,   35,   35,
 /*   460 */    79,  122,  123,  124,   51,   84,   85,   86,   87,   88,
 /*   470 */    89,   90,   91,   92,   93,   94,   95,   96,   97,   98,
 /*   480 */    99,  100,  101,   34,  103,  104,   35,  106,  107,  108,
 /*   490 */   109,  110,  111,  112,  113,   35,   31,   33,   31,   31,
 /*   500 */    34,   79,  145,  122,  123,  124,   84,   85,   86,   87,
 /*   510 */    88,   89,   90,   91,   92,   93,   94,   95,   96,   97,
 /*   520 */    98,   99,  100,  101,   35,  103,  104,   34,  106,  107,
 /*   530 */   108,  109,  110,  111,  112,  113,   35,  145,  145,  145,
 /*   540 */   145,  145,   79,  145,  122,  123,  124,   84,   85,   86,
 /*   550 */    87,   88,   89,   90,   91,   92,   93,   94,   95,   96,
 /*   560 */    97,   98,   99,  100,  101,  145,  103,  104,  145,  106,
 /*   570 */   107,  108,  109,  110,  111,  112,  113,  145,  145,  145,
 /*   580 */   145,  145,  145,   79,  145,  122,  123,  124,   84,   85,
 /*   590 */    86,   87,   88,   89,   90,   91,   92,   93,   94,   95,
 /*   600 */    96,   97,   98,   99,  100,  101,  145,  103,  104,  145,
 /*   610 */   106,  107,  108,  109,  110,  111,  112,  113,  145,  145,
 /*   620 */   145,  145,  145,  145,   79,  145,  122,  123,  124,   84,
 /*   630 */    85,   86,   87,   88,   89,   90,   91,   92,   93,   94,
 /*   640 */    95,   96,   97,   98,   99,  100,  101,  145,  103,  104,
 /*   650 */   145,  106,  107,  108,  109,  110,  111,  112,  113,  145,
 /*   660 */   145,  145,  145,  145,  145,   79,  145,  122,  123,  124,
 /*   670 */    84,   85,   86,   87,   88,   89,   90,   91,   92,   93,
 /*   680 */    94,   95,   96,   97,   98,   99,  100,  101,  145,  103,
 /*   690 */   104,  145,  106,  107,  108,  109,  110,  111,  112,  113,
 /*   700 */   106,  107,  108,  109,  110,  111,  112,  113,  122,  123,
 /*   710 */   124,    1,    2,    3,    4,    5,    6,    7,    8,    9,
 /*   720 */    10,   11,   12,   13,   14,   15,   16,   17,   18,   19,
 /*   730 */    20,   21,   22,   23,   24,   25,   15,    8,    9,   10,
 /*   740 */    11,   12,   13,   14,   15,   16,   17,   18,   27,   28,
 /*   750 */    29,  145,   31,   32,  145,   34,   31,   36,   37,   38,
 /*   760 */    39,   40,   41,   42,   43,   44,   45,  145,   47,   15,
 /*   770 */   145,  145,   51,   48,   49,   50,  145,  145,  145,   58,
 /*   780 */   145,   27,   28,   29,  145,   31,   32,  145,   34,  145,
 /*   790 */    36,   37,   38,   39,   40,   41,   42,   43,   44,   45,
 /*   800 */   145,   47,   15,  145,  104,   51,  106,  107,  108,  109,
 /*   810 */   110,  111,  112,  113,   27,   28,   29,  145,   31,   32,
 /*   820 */   145,   34,  145,   36,   37,   38,   39,   40,   41,   42,
 /*   830 */    43,   44,   45,  145,   47,   15,  104,  145,  106,  107,
 /*   840 */   108,  109,  110,  111,  112,  113,  145,   27,   28,   29,
 /*   850 */   145,   31,   32,  145,   34,  145,   36,   37,   38,   39,
 /*   860 */    40,   41,   42,   43,   44,   45,  145,   47,   19,  100,
 /*   870 */   101,  145,  103,  104,  145,  106,  107,  108,  109,  110,
 /*   880 */   111,  112,  113,   86,   35,  145,  117,  118,  119,  120,
 /*   890 */   121,  145,  145,  145,  145,   46,  145,  100,  101,  145,
 /*   900 */   103,  104,  145,  106,  107,  108,  109,  110,  111,  112,
 /*   910 */   113,  145,  145,   64,  145,  145,  100,  101,  145,  103,
 /*   920 */   104,  124,  106,  107,  108,  109,  110,  111,  112,  113,
 /*   930 */   114,  115,  145,  145,  137,  138,  145,  145,    3,    4,
 /*   940 */     5,    6,    7,    8,    9,   10,   11,   12,   13,   14,
 /*   950 */    15,   16,   17,   18,    4,    5,    6,    7,    8,    9,
 /*   960 */    10,   11,   12,   13,   14,   15,   16,   17,   18,    5,
 /*   970 */     6,    7,    8,    9,   10,   11,   12,   13,   14,   15,
 /*   980 */    16,   17,   18,  100,  101,  145,  103,  104,  145,  106,
 /*   990 */   107,  108,  109,  110,  111,  112,  113,  114,  115,    6,
 /*  1000 */     7,    8,    9,   10,   11,   12,   13,   14,   15,   16,
 /*  1010 */    17,   18,  145,  145,  145,  145,  145,  100,  101,  102,
 /*  1020 */   103,  104,  145,  106,  107,  108,  109,  110,  111,  112,
 /*  1030 */   113,  100,  101,  145,  103,  104,  145,  106,  107,  108,
 /*  1040 */   109,  110,  111,  112,  113,  145,  145,  100,  101,  118,
 /*  1050 */   103,  104,  145,  106,  107,  108,  109,  110,  111,  112,
 /*  1060 */   113,  100,  101,  145,  103,  104,  145,  106,  107,  108,
 /*  1070 */   109,  110,  111,  112,  113,  100,  101,  145,  103,  104,
 /*  1080 */   145,  106,  107,  108,  109,  110,  111,  112,  113,  145,
 /*  1090 */   100,  101,  145,  103,  104,  145,  106,  107,  108,  109,
 /*  1100 */   110,  111,  112,  113,  100,  101,  145,  103,  104,  145,
 /*  1110 */   106,  107,  108,  109,  110,  111,  112,  113,  100,  101,
 /*  1120 */   145,  103,  104,  145,  106,  107,  108,  109,  110,  111,
 /*  1130 */   112,  113,  100,  101,  145,  103,  104,  145,  106,  107,
 /*  1140 */   108,  109,  110,  111,  112,  113,  145,  145,  100,  101,
 /*  1150 */   145,  103,  104,  145,  106,  107,  108,  109,  110,  111,
 /*  1160 */   112,  113,  100,  101,  145,  103,  104,  145,  106,  107,
 /*  1170 */   108,  109,  110,  111,  112,  113,  100,  101,  145,  103,
 /*  1180 */   104,  145,  106,  107,  108,  109,  110,  111,  112,  113,
 /*  1190 */   145,  100,  101,  145,  103,  104,  145,  106,  107,  108,
 /*  1200 */   109,  110,  111,  112,  113,  100,  101,  145,  103,  104,
 /*  1210 */   145,  106,  107,  108,  109,  110,  111,  112,  113,  100,
 /*  1220 */   101,  145,  103,  104,  145,  106,  107,  108,  109,  110,
 /*  1230 */   111,  112,  113,  100,  101,  145,  103,  104,  145,  106,
 /*  1240 */   107,  108,  109,  110,  111,  112,  113,  145,  145,  100,
 /*  1250 */   101,  145,  103,  104,  145,  106,  107,  108,  109,  110,
 /*  1260 */   111,  112,  113,  101,  145,  103,  104,  145,  106,  107,
 /*  1270 */   108,  109,  110,  111,  112,  113,  104,  145,  106,  107,
 /*  1280 */   108,  109,  110,  111,  112,  113,  145,  104,  145,  106,
 /*  1290 */   107,  108,  109,  110,  111,  112,  113,  104,  145,  106,
 /*  1300 */   107,  108,  109,  110,  111,  112,  113,  104,  145,  106,
 /*  1310 */   107,  108,  109,  110,  111,  112,  113,  104,  145,  106,
 /*  1320 */   107,  108,  109,  110,  111,  112,  113,   37,   38,   39,
 /*  1330 */    40,   41,   42,   43,   44,   45,  104,  145,  106,  107,
 /*  1340 */   108,  109,  110,  111,  112,  113,   10,   11,   12,   13,
 /*  1350 */    14,   15,   16,   17,   18,  104,  145,  106,  107,  108,
 /*  1360 */   109,  110,  111,  112,  113,  104,  145,  106,  107,  108,
 /*  1370 */   109,  110,  111,  112,  113,  104,  145,  106,  107,  108,
 /*  1380 */   109,  110,  111,  112,  113,  104,  145,  106,  107,  108,
 /*  1390 */   109,  110,  111,  112,  113,  104,  145,  106,  107,  108,
 /*  1400 */   109,  110,  111,  112,  113,  104,  145,  106,  107,  108,
 /*  1410 */   109,  110,  111,  112,  113,  104,  145,  106,  107,  108,
 /*  1420 */   109,  110,  111,  112,  113,  104,  145,  106,  107,  108,
 /*  1430 */   109,  110,  111,  112,  113,  104,  145,  106,  107,  108,
 /*  1440 */   109,  110,  111,  112,  113,  104,  145,  106,  107,  108,
 /*  1450 */   109,  110,  111,  112,  113,  106,  107,  108,  109,  110,
 /*  1460 */   111,  112,  113,  116,  106,  107,  108,  109,  110,  111,
 /*  1470 */   112,  113,  116,  145,  145,  145,  145,  130,  145,  132,
 /*  1480 */   133,  134,  135,  136,  145,  145,  130,  145,  132,  133,
 /*  1490 */   134,  135,  136,   53,  116,  145,  145,  145,   58,   53,
 /*  1500 */    60,   61,   62,   63,   58,  145,   60,   61,   62,   63,
 /*  1510 */   132,  133,  134,  145,  136,
};
#define YY_SHIFT_USE_DFLT (-1)
#define YY_SHIFT_MAX 165
static const short yy_shift_ofst[] = {
 /*     0 */   124,  124,  124,   20,  124,   72,  124,  124,  124,  124,
 /*    10 */   124,  124,  124,  124,  820,  721,  787,  787,  787,  787,
 /*    20 */   754,  787,  787,  787,  787,  787,  787,  787,  787,  787,
 /*    30 */   787,  787,  787,  787,  787,  787,  787,  787,  787,  787,
 /*    40 */   787,  787,  787,  787,  787,  787,  787,  787,  787,  787,
 /*    50 */   787,  787,  787,  787,  787,  787,  787,  787,  787,  143,
 /*    60 */   143, 1446,  143, 1440,  157,  301,   96,   44,   19,  241,
 /*    70 */   710, 1290,  725,   93,   93,  146,  126,   93,   41,   19,
 /*    80 */    -1,  248,  935,  950,  964,  993,  729,  729, 1336, 1336,
 /*    90 */    22,   22,   22,   22,  849,  277,   52,   52,   50,   68,
 /*   100 */    74,  273,  270,  303,  297,  357,  353,  334,  379,  369,
 /*   110 */   385,  400,  427,  379,  270,  423,  424,  449,  460,  464,
 /*   120 */   489,  400,  493,  501,  466,  468,  467,  465,  451,  413,
 /*   130 */   426,  414,  404,  420,  418,  407,  382,  363,  377,  348,
 /*   140 */   327,  316,  288,  268,  280,  259,  244,  225,  237,  217,
 /*   150 */   209,  179,  176,  154,  147,  134,  133,  105,  137,  100,
 /*   160 */    94,  115,   45,    0,  254,   35,
};
#define YY_REDUCE_USE_DFLT (-117)
#define YY_REDUCE_MAX 80
static const short yy_reduce_ofst[] = {
 /*     0 */   -79,  190,  125,  298,  249,  422,  339,  504,  586,  463,
 /*    10 */   381,  422,  545,  422,  769,  797,  816,  883,  931,  917,
 /*    20 */   975, 1004, 1149,  947, 1133, 1105, 1076, 1048, 1018,  990,
 /*    30 */   961, 1119, 1032, 1062, 1091,  214, 1162, 1281, 1291, 1301,
 /*    40 */  1311, 1321, 1331, 1341, 1271, 1261, 1251, 1232, 1213, 1193,
 /*    50 */  1172,  700,  732, 1183, 1203,  257,  594, 1349, 1358, 1356,
 /*    60 */  1347,  117, 1378,  -40,   70,   -8,  -17,  -47,  -84,  -84,
 /*    70 */   165,  131,  127,  112,  108,   53,   39,   47,  -91, -100,
 /*    80 */  -116,
};
static const YYACTIONTYPE yy_default[] = {
 /*     0 */   303,  471,  471,  471,  471,  471,  471,  471,  471,  471,
 /*    10 */   471,  304,  471,  464,  397,  471,  471,  471,  471,  322,
 /*    20 */   471,  471,  471,  471,  471,  471,  471,  471,  471,  471,
 /*    30 */   471,  471,  471,  471,  471,  471,  471,  471,  471,  471,
 /*    40 */   471,  471,  471,  471,  471,  471,  471,  471,  471,  471,
 /*    50 */   471,  471,  471,  471,  471,  471,  471,  471,  471,  437,
 /*    60 */   437,  471,  471,  471,  471,  471,  471,  471,  471,  471,
 /*    70 */   332,  471,  471,  471,  471,  471,  471,  471,  465,  471,
 /*    80 */   459,  335,  336,  337,  338,  339,  352,  351,  340,  341,
 /*    90 */   342,  343,  344,  345,  389,  353,  347,  346,  471,  380,
 /*   100 */   413,  471,  441,  471,  471,  389,  471,  455,  363,  398,
 /*   110 */   471,  471,  471,  380,  439,  471,  471,  471,  471,  471,
 /*   120 */   471,  430,  471,  471,  471,  471,  471,  471,  471,  471,
 /*   130 */   471,  471,  471,  471,  471,  471,  471,  471,  471,  471,
 /*   140 */   471,  471,  471,  471,  471,  471,  471,  471,  471,  471,
 /*   150 */   471,  471,  471,  382,  471,  471,  471,  367,  471,  412,
 /*   160 */   438,  413,  471,  399,  471,  471,  415,  416,  414,  417,
 /*   170 */   411,  418,  410,  419,  420,  409,  408,  421,  422,  423,
 /*   180 */   407,  406,  405,  386,  387,  388,  404,  403,  435,  436,
 /*   190 */   401,  400,  429,  447,  350,  349,  389,  348,  333,  331,
 /*   200 */   330,  329,  328,  434,  327,  432,  326,  433,  365,  364,
 /*   210 */   396,  395,  393,  392,  391,  446,  442,  444,  445,  390,
 /*   220 */   440,  443,  394,  385,  448,  384,  383,  381,  379,  378,
 /*   230 */   377,  452,  376,  375,  374,  373,  372,  371,  456,  370,
 /*   240 */   369,  368,  367,  457,  366,  362,  359,  358,  357,  356,
 /*   250 */   458,  460,  461,  355,  354,  361,  360,  334,  325,  324,
 /*   260 */   467,  321,  469,  466,  320,  319,  318,  317,  468,  316,
 /*   270 */   315,  470,  462,  314,  313,  463,  312,  311,  310,  309,
 /*   280 */   308,  323,  307,  306,  449,  450,  305,  451,  302,  301,
 /*   290 */   300,  299,  453,  424,  425,  426,  428,  427,  402,
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
  "VARIADIC",      "EXTERN",        "FOR",           "IN",          
  "LOWER_THAN_ELSE",  "ELSE",          "IF",            "WHILE",       
  "SWITCH",        "CASE",          "DEFAULT",       "TRY",         
  "CATCH",         "FINALLY",       "THROW",         "error",       
  "main",          "translation_unit",  "statement_sequence_opt",  "statement_sequence",
  "statement",     "include_statement",  "expression_statement",  "declaration_statement",
  "for_statement",  "compound_statement",  "if_statement",  "while_statement",
  "foreach_statement",  "return_statement",  "switch_statement",  "break_statement",
  "continue_statement",  "extern_declaration",  "try_statement",  "throw_statement",
  "expression",    "assignment_expression",  "expression_opt",  "conditional_expression",
  "binary_expression",  "assignment_operator",  "unary_expression",  "postfix_expression",
  "new_expression",  "primary_expression",  "function_call",  "literal",     
  "id_expression",  "list_literal",  "list_content",  "list_entry",  
  "type",          "argument_list",  "positional_argument",  "positional_argument_list",
  "named_argument",  "named_argument_list",  "function_declaration",  "class_declaration",
  "variable_declaration",  "declarator_sequence",  "declarator",    "class_members",
  "class_member",  "access_specifier",  "parameter_list",  "function_body",
  "parameter",     "opt_parameter",  "variadic_parameter",  "parameters",  
  "opt_parameters",  "for_init_statement",  "foreach_decl",  "switch_body", 
  "switch_case",   "default_case",  "case_statements",  "catch_block", 
  "finally_block",
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
 /* 133 */ "parameter ::= type BITAND IDENTIFIER",
 /* 134 */ "parameter ::= BITAND IDENTIFIER",
 /* 135 */ "opt_parameter ::= type IDENTIFIER ASSIGN expression",
 /* 136 */ "opt_parameter ::= IDENTIFIER ASSIGN expression",
 /* 137 */ "variadic_parameter ::= IDENTIFIER VARIADIC",
 /* 138 */ "parameter_list ::=",
 /* 139 */ "parameter_list ::= parameters",
 /* 140 */ "parameter_list ::= opt_parameters",
 /* 141 */ "parameter_list ::= variadic_parameter",
 /* 142 */ "parameter_list ::= parameters COMMA opt_parameters",
 /* 143 */ "parameter_list ::= parameters COMMA variadic_parameter",
 /* 144 */ "parameters ::= parameter",
 /* 145 */ "parameters ::= parameters COMMA parameter",
 /* 146 */ "opt_parameters ::= opt_parameter",
 /* 147 */ "opt_parameters ::= opt_parameters COMMA opt_parameter",
 /* 148 */ "function_body ::= statement",
 /* 149 */ "extern_declaration ::= EXTERN LIT_STRING type IDENTIFIER LPAREN parameter_list RPAREN SEMICOLON",
 /* 150 */ "for_statement ::= FOR LPAREN for_init_statement expression SEMICOLON expression_opt RPAREN statement",
 /* 151 */ "for_init_statement ::= expression_statement",
 /* 152 */ "for_init_statement ::= variable_declaration SEMICOLON",
 /* 153 */ "foreach_statement ::= FOR LPAREN id_expression IN expression RPAREN statement",
 /* 154 */ "foreach_statement ::= FOR LPAREN foreach_decl IN expression RPAREN statement",
 /* 155 */ "foreach_decl ::= VAR IDENTIFIER",
 /* 156 */ "if_statement ::= IF LPAREN expression RPAREN statement",
 /* 157 */ "if_statement ::= IF LPAREN expression RPAREN statement ELSE statement",
 /* 158 */ "while_statement ::= WHILE LPAREN expression RPAREN statement",
 /* 159 */ "switch_statement ::= SWITCH LPAREN expression RPAREN LBRACE switch_body RBRACE",
 /* 160 */ "switch_body ::=",
 /* 161 */ "switch_body ::= switch_body switch_case",
 /* 162 */ "switch_body ::= switch_body default_case",
 /* 163 */ "switch_case ::= CASE literal COLON case_statements",
 /* 164 */ "default_case ::= DEFAULT COLON case_statements",
 /* 165 */ "case_statements ::= statement_sequence",
 /* 166 */ "try_statement ::= TRY compound_statement catch_block",
 /* 167 */ "try_statement ::= TRY compound_statement finally_block",
 /* 168 */ "try_statement ::= TRY compound_statement catch_block finally_block",
 /* 169 */ "catch_block ::= CATCH LPAREN IDENTIFIER RPAREN compound_statement",
 /* 170 */ "finally_block ::= FINALLY compound_statement",
 /* 171 */ "throw_statement ::= THROW expression SEMICOLON",
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
  { 80, 1 },
  { 81, 1 },
  { 83, 1 },
  { 83, 2 },
  { 82, 0 },
  { 82, 1 },
  { 84, 1 },
  { 84, 1 },
  { 84, 1 },
  { 84, 1 },
  { 84, 1 },
  { 84, 1 },
  { 84, 1 },
  { 84, 1 },
  { 84, 1 },
  { 84, 1 },
  { 84, 1 },
  { 84, 1 },
  { 84, 1 },
  { 84, 1 },
  { 84, 1 },
  { 84, 1 },
  { 100, 1 },
  { 102, 0 },
  { 102, 1 },
  { 101, 1 },
  { 101, 3 },
  { 105, 1 },
  { 105, 1 },
  { 105, 1 },
  { 105, 1 },
  { 105, 1 },
  { 105, 1 },
  { 103, 1 },
  { 103, 5 },
  { 104, 1 },
  { 104, 3 },
  { 104, 3 },
  { 104, 3 },
  { 104, 3 },
  { 104, 3 },
  { 104, 3 },
  { 104, 3 },
  { 104, 3 },
  { 104, 3 },
  { 104, 3 },
  { 104, 3 },
  { 104, 3 },
  { 104, 3 },
  { 104, 3 },
  { 104, 3 },
  { 104, 3 },
  { 104, 3 },
  { 104, 3 },
  { 106, 1 },
  { 106, 2 },
  { 106, 2 },
  { 106, 2 },
  { 106, 2 },
  { 106, 1 },
  { 107, 1 },
  { 107, 2 },
  { 107, 2 },
  { 107, 1 },
  { 107, 3 },
  { 107, 3 },
  { 107, 4 },
  { 109, 1 },
  { 109, 1 },
  { 109, 3 },
  { 109, 1 },
  { 109, 1 },
  { 111, 1 },
  { 111, 1 },
  { 111, 1 },
  { 111, 1 },
  { 111, 1 },
  { 111, 1 },
  { 111, 1 },
  { 111, 1 },
  { 111, 1 },
  { 112, 1 },
  { 113, 3 },
  { 114, 1 },
  { 114, 3 },
  { 115, 1 },
  { 108, 2 },
  { 116, 1 },
  { 116, 1 },
  { 116, 1 },
  { 116, 1 },
  { 110, 4 },
  { 118, 1 },
  { 119, 1 },
  { 119, 3 },
  { 120, 3 },
  { 121, 1 },
  { 121, 3 },
  { 117, 0 },
  { 117, 1 },
  { 117, 1 },
  { 86, 1 },
  { 86, 2 },
  { 89, 2 },
  { 89, 3 },
  { 85, 3 },
  { 93, 3 },
  { 93, 2 },
  { 95, 2 },
  { 96, 2 },
  { 87, 1 },
  { 87, 2 },
  { 87, 2 },
  { 124, 2 },
  { 126, 1 },
  { 126, 3 },
  { 125, 1 },
  { 125, 3 },
  { 123, 4 },
  { 123, 5 },
  { 128, 2 },
  { 128, 1 },
  { 128, 3 },
  { 128, 2 },
  { 128, 2 },
  { 129, 1 },
  { 129, 1 },
  { 129, 1 },
  { 127, 1 },
  { 127, 2 },
  { 122, 6 },
  { 132, 2 },
  { 132, 1 },
  { 132, 3 },
  { 132, 2 },
  { 133, 4 },
  { 133, 3 },
  { 134, 2 },
  { 130, 0 },
  { 130, 1 },
  { 130, 1 },
  { 130, 1 },
  { 130, 3 },
  { 130, 3 },
  { 135, 1 },
  { 135, 3 },
  { 136, 1 },
  { 136, 3 },
  { 131, 1 },
  { 97, 8 },
  { 88, 8 },
  { 137, 1 },
  { 137, 2 },
  { 92, 7 },
  { 92, 7 },
  { 138, 2 },
  { 90, 5 },
  { 90, 7 },
  { 91, 5 },
  { 94, 7 },
  { 139, 0 },
  { 139, 2 },
  { 139, 2 },
  { 140, 4 },
  { 141, 3 },
  { 142, 1 },
  { 98, 3 },
  { 98, 3 },
  { 98, 4 },
  { 143, 5 },
  { 144, 2 },
  { 99, 3 },
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
{ p->SetRoot(yymsp[0].minor.yy71); }
#line 1327 "cscript.c"
        break;
      case 1:
#line 78 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(translation_unit, yymsp[0].minor.yy71); }
#line 1332 "cscript.c"
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
      case 148:
      case 151:
      case 165:
#line 81 "cscript.in"
{ yygotominor.yy71 = yymsp[0].minor.yy71; }
#line 1372 "cscript.c"
        break;
      case 3:
#line 82 "cscript.in"
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
#line 1386 "cscript.c"
        break;
      case 4:
      case 23:
#line 95 "cscript.in"
{ yygotominor.yy71 = 0; }
#line 1392 "cscript.c"
        break;
      case 21:
      case 101:
#line 116 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(empty_statement); }
#line 1398 "cscript.c"
        break;
      case 26:
#line 132 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(assignment_expression, yymsp[-1].minor.yy264, yymsp[-2].minor.yy71, yymsp[0].minor.yy71); }
#line 1403 "cscript.c"
        break;
      case 27:
#line 136 "cscript.in"
{ yygotominor.yy264 = op_assign; }
#line 1408 "cscript.c"
        break;
      case 28:
#line 137 "cscript.in"
{ yygotominor.yy264 = op_assadd; }
#line 1413 "cscript.c"
        break;
      case 29:
#line 138 "cscript.in"
{ yygotominor.yy264 = op_asssub; }
#line 1418 "cscript.c"
        break;
      case 30:
#line 139 "cscript.in"
{ yygotominor.yy264 = op_assmul; }
#line 1423 "cscript.c"
        break;
      case 31:
#line 140 "cscript.in"
{ yygotominor.yy264 = op_assdiv; }
#line 1428 "cscript.c"
        break;
      case 32:
#line 141 "cscript.in"
{ yygotominor.yy264 = op_assmod; }
#line 1433 "cscript.c"
        break;
      case 34:
#line 145 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(ternary_expression, yymsp[-4].minor.yy71, yymsp[-2].minor.yy71, yymsp[0].minor.yy71); }
#line 1438 "cscript.c"
        break;
      case 36:
#line 149 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(binary_expression, op_logor,   yymsp[-2].minor.yy71, yymsp[0].minor.yy71); }
#line 1443 "cscript.c"
        break;
      case 37:
#line 150 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(binary_expression, op_logand,  yymsp[-2].minor.yy71, yymsp[0].minor.yy71); }
#line 1448 "cscript.c"
        break;
      case 38:
#line 151 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(binary_expression, op_bitor,   yymsp[-2].minor.yy71, yymsp[0].minor.yy71); }
#line 1453 "cscript.c"
        break;
      case 39:
#line 152 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(binary_expression, op_bitxor,  yymsp[-2].minor.yy71, yymsp[0].minor.yy71); }
#line 1458 "cscript.c"
        break;
      case 40:
#line 153 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(binary_expression, op_bitand,  yymsp[-2].minor.yy71, yymsp[0].minor.yy71); }
#line 1463 "cscript.c"
        break;
      case 41:
#line 154 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(binary_expression, op_eq,   yymsp[-2].minor.yy71, yymsp[0].minor.yy71); }
#line 1468 "cscript.c"
        break;
      case 42:
#line 155 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(binary_expression, op_ne,   yymsp[-2].minor.yy71, yymsp[0].minor.yy71); }
#line 1473 "cscript.c"
        break;
      case 43:
#line 156 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(binary_expression, op_lt,   yymsp[-2].minor.yy71, yymsp[0].minor.yy71); }
#line 1478 "cscript.c"
        break;
      case 44:
#line 157 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(binary_expression, op_le,   yymsp[-2].minor.yy71, yymsp[0].minor.yy71); }
#line 1483 "cscript.c"
        break;
      case 45:
#line 158 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(binary_expression, op_gt,   yymsp[-2].minor.yy71, yymsp[0].minor.yy71); }
#line 1488 "cscript.c"
        break;
      case 46:
#line 159 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(binary_expression, op_ge,   yymsp[-2].minor.yy71, yymsp[0].minor.yy71); }
#line 1493 "cscript.c"
        break;
      case 47:
#line 160 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(binary_expression, op_add,  yymsp[-2].minor.yy71, yymsp[0].minor.yy71); }
#line 1498 "cscript.c"
        break;
      case 48:
#line 161 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(binary_expression, op_sub,  yymsp[-2].minor.yy71, yymsp[0].minor.yy71); }
#line 1503 "cscript.c"
        break;
      case 49:
#line 162 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(binary_expression, op_mul,  yymsp[-2].minor.yy71, yymsp[0].minor.yy71); }
#line 1508 "cscript.c"
        break;
      case 50:
#line 163 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(binary_expression, op_div,  yymsp[-2].minor.yy71, yymsp[0].minor.yy71); }
#line 1513 "cscript.c"
        break;
      case 51:
#line 164 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(binary_expression, op_mod,  yymsp[-2].minor.yy71, yymsp[0].minor.yy71); }
#line 1518 "cscript.c"
        break;
      case 52:
#line 165 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(binary_expression, op_seq,  yymsp[-2].minor.yy71, yymsp[0].minor.yy71); }
#line 1523 "cscript.c"
        break;
      case 53:
#line 166 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(binary_expression, op_sne,  yymsp[-2].minor.yy71, yymsp[0].minor.yy71); }
#line 1528 "cscript.c"
        break;
      case 55:
#line 170 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(prefix_expression, op_negate, yymsp[0].minor.yy71); }
#line 1533 "cscript.c"
        break;
      case 56:
#line 171 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(prefix_expression, op_preinc, yymsp[0].minor.yy71); }
#line 1538 "cscript.c"
        break;
      case 57:
#line 172 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(prefix_expression, op_predec, yymsp[0].minor.yy71); }
#line 1543 "cscript.c"
        break;
      case 58:
#line 173 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(prefix_expression, op_not,    yymsp[0].minor.yy71); }
#line 1548 "cscript.c"
        break;
      case 61:
#line 178 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(postfix_expression, op_postinc, yymsp[-1].minor.yy71); }
#line 1553 "cscript.c"
        break;
      case 62:
#line 179 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(postfix_expression, op_postdec, yymsp[-1].minor.yy71); }
#line 1558 "cscript.c"
        break;
      case 64:
#line 181 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(member_expression, yymsp[-2].minor.yy71, String(yymsp[0].minor.yy0)); }
#line 1563 "cscript.c"
        break;
      case 65:
#line 182 "cscript.in"
{ yygotominor.yy71 = yymsp[0].minor.yy71; yymsp[0].minor.yy71->m_a3 = yymsp[-2].minor.yy71; }
#line 1568 "cscript.c"
        break;
      case 66:
#line 183 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(index_expression, yymsp[-3].minor.yy71, yymsp[-1].minor.yy71); }
#line 1573 "cscript.c"
        break;
      case 69:
      case 111:
      case 112:
      case 152:
#line 188 "cscript.in"
{ yygotominor.yy71 = yymsp[-1].minor.yy71; }
#line 1581 "cscript.c"
        break;
      case 71:
#line 190 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(this_expression); }
#line 1586 "cscript.c"
        break;
      case 72:
#line 193 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(literal_value, Variant(String(yymsp[0].minor.yy0), Variant::stInt)); }
#line 1591 "cscript.c"
        break;
      case 73:
#line 194 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(literal_value, Variant(hex2dec(String(yymsp[0].minor.yy0).c_str()))); }
#line 1596 "cscript.c"
        break;
      case 74:
#line 195 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(literal_value, Variant(bin2dec(String(yymsp[0].minor.yy0).c_str()))); }
#line 1601 "cscript.c"
        break;
      case 75:
#line 196 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(literal_value, Variant(rom2dec(String(yymsp[0].minor.yy0).c_str()))); }
#line 1606 "cscript.c"
        break;
      case 76:
#line 197 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(literal_value, Variant(String(yymsp[0].minor.yy0), Variant::stReal)); }
#line 1611 "cscript.c"
        break;
      case 77:
#line 198 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(literal_value, Variant(String(yymsp[0].minor.yy0), Variant::stString)); }
#line 1616 "cscript.c"
        break;
      case 78:
#line 199 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(literal_value, Variant(true));    }
#line 1621 "cscript.c"
        break;
      case 79:
#line 200 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(literal_value, Variant(false));   }
#line 1626 "cscript.c"
        break;
      case 80:
#line 201 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(literal_value, Variant());        }
#line 1631 "cscript.c"
        break;
      case 81:
#line 204 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(lvalue, String(yymsp[0].minor.yy0)); }
#line 1636 "cscript.c"
        break;
      case 82:
#line 207 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(list_literal, yymsp[-1].minor.yy71); }
#line 1641 "cscript.c"
        break;
      case 83:
#line 208 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(list_content, yymsp[0].minor.yy71); }
#line 1646 "cscript.c"
        break;
      case 84:
#line 209 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(list_content, yymsp[-2].minor.yy71, yymsp[0].minor.yy71); }
#line 1651 "cscript.c"
        break;
      case 85:
#line 211 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(list_entry, yymsp[0].minor.yy71); }
#line 1656 "cscript.c"
        break;
      case 86:
#line 214 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(new_expression, String(yymsp[0].minor.yy0)); }
#line 1661 "cscript.c"
        break;
      case 87:
#line 221 "cscript.in"
{ yygotominor.yy71 = new Ast(builtin_type, Variant::stBool);    }
#line 1666 "cscript.c"
        break;
      case 88:
#line 222 "cscript.in"
{ yygotominor.yy71 = new Ast(builtin_type, Variant::stInt);     }
#line 1671 "cscript.c"
        break;
      case 89:
#line 223 "cscript.in"
{ yygotominor.yy71 = new Ast(builtin_type, Variant::stString);  }
#line 1676 "cscript.c"
        break;
      case 90:
#line 224 "cscript.in"
{ yygotominor.yy71 = new Ast(class_type, String(yymsp[0].minor.yy0));            }
#line 1681 "cscript.c"
        break;
      case 91:
#line 232 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(function_call, String(yymsp[-3].minor.yy0), yymsp[-1].minor.yy71); }
#line 1686 "cscript.c"
        break;
      case 93:
      case 96:
      case 141:
      case 144:
      case 146:
#line 239 "cscript.in"
{ yygotominor.yy155 = new AstList; yygotominor.yy155->push_back(yymsp[0].minor.yy71); }
#line 1695 "cscript.c"
        break;
      case 94:
      case 97:
      case 143:
      case 145:
      case 147:
#line 240 "cscript.in"
{ yygotominor.yy155 = yymsp[-2].minor.yy155; yymsp[-2].minor.yy155->push_back(yymsp[0].minor.yy71); }
#line 1704 "cscript.c"
        break;
      case 95:
#line 243 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(named_argument, String(yymsp[-2].minor.yy0), yymsp[0].minor.yy71); }
#line 1709 "cscript.c"
        break;
      case 98:
#line 251 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(positional_arguments, new AstList); }
#line 1714 "cscript.c"
        break;
      case 99:
#line 252 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(positional_arguments, yymsp[0].minor.yy155); }
#line 1719 "cscript.c"
        break;
      case 100:
#line 253 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(named_arguments,      yymsp[0].minor.yy155); }
#line 1724 "cscript.c"
        break;
      case 102:
#line 262 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(expression_statement, yymsp[-1].minor.yy71); }
#line 1729 "cscript.c"
        break;
      case 103:
#line 265 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(compound_statement); }
#line 1734 "cscript.c"
        break;
      case 104:
#line 266 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(compound_statement, yymsp[-1].minor.yy71); }
#line 1739 "cscript.c"
        break;
      case 105:
#line 269 "cscript.in"
{ p->Parse(yymsp[-1].minor.yy0); yygotominor.yy71 = p->GetRoot(); }
#line 1744 "cscript.c"
        break;
      case 106:
#line 272 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(return_statement, yymsp[-1].minor.yy71); }
#line 1749 "cscript.c"
        break;
      case 107:
#line 273 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(return_statement);    }
#line 1754 "cscript.c"
        break;
      case 108:
#line 276 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(break_statement); }
#line 1759 "cscript.c"
        break;
      case 109:
#line 277 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(continue_statement); }
#line 1764 "cscript.c"
        break;
      case 114:
#line 291 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(variable_declaration, String(yymsp[0].minor.yy0));    }
#line 1769 "cscript.c"
        break;
      case 115:
#line 292 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(variable_declaration, String(yymsp[-2].minor.yy0), yymsp[0].minor.yy71); }
#line 1774 "cscript.c"
        break;
      case 117:
#line 295 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(declaration_sequence, yymsp[-2].minor.yy71, yymsp[0].minor.yy71); }
#line 1779 "cscript.c"
        break;
      case 118:
#line 302 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(class_declaration, String(yymsp[-2].minor.yy0)); }
#line 1784 "cscript.c"
        break;
      case 119:
#line 303 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(class_declaration, String(yymsp[-3].minor.yy0), yymsp[-1].minor.yy155); }
#line 1789 "cscript.c"
        break;
      case 120:
#line 306 "cscript.in"
{ yygotominor.yy71 = yymsp[-1].minor.yy71; yymsp[-1].minor.yy71->m_props["access"] = accessDefault; }
#line 1794 "cscript.c"
        break;
      case 121:
#line 307 "cscript.in"
{ yygotominor.yy71 = yymsp[0].minor.yy71; yymsp[0].minor.yy71->m_props["access"] = accessDefault; }
#line 1799 "cscript.c"
        break;
      case 122:
#line 308 "cscript.in"
{ yygotominor.yy71 = yymsp[-1].minor.yy71; yymsp[-1].minor.yy71->m_props["access"] = yymsp[-2].minor.yy68; }
#line 1804 "cscript.c"
        break;
      case 123:
#line 309 "cscript.in"
{ yygotominor.yy71 = yymsp[0].minor.yy71; yymsp[0].minor.yy71->m_props["access"] = yymsp[-1].minor.yy68; }
#line 1809 "cscript.c"
        break;
      case 124:
#line 310 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(access_specifier, yymsp[-1].minor.yy68); }
#line 1814 "cscript.c"
        break;
      case 125:
#line 314 "cscript.in"
{ yygotominor.yy68 = accessPrivate;   }
#line 1819 "cscript.c"
        break;
      case 126:
#line 315 "cscript.in"
{ yygotominor.yy68 = accessProtected; }
#line 1824 "cscript.c"
        break;
      case 127:
#line 316 "cscript.in"
{ yygotominor.yy68 = accessPublic;    }
#line 1829 "cscript.c"
        break;
      case 128:
#line 320 "cscript.in"
{ 
  yygotominor.yy155 = new AstList;
  yygotominor.yy155->push_back(yymsp[0].minor.yy71);
}
#line 1837 "cscript.c"
        break;
      case 129:
#line 324 "cscript.in"
{ 
  yygotominor.yy155 = yymsp[-1].minor.yy155;
  yygotominor.yy155->push_back(yymsp[0].minor.yy71);
}
#line 1845 "cscript.c"
        break;
      case 130:
#line 335 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(function_declaration, String(yymsp[-4].minor.yy0), yymsp[-2].minor.yy155, yymsp[0].minor.yy71); }
#line 1850 "cscript.c"
        break;
      case 131:
#line 338 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(parameter, String(yymsp[0].minor.yy0), ptByVal, yymsp[-1].minor.yy71); }
#line 1855 "cscript.c"
        break;
      case 132:
#line 339 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(parameter, String(yymsp[0].minor.yy0), ptByVal);    }
#line 1860 "cscript.c"
        break;
      case 133:
#line 340 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(parameter, String(yymsp[0].minor.yy0), ptByRef, yymsp[-2].minor.yy71); }
#line 1865 "cscript.c"
        break;
      case 134:
#line 341 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(parameter, String(yymsp[0].minor.yy0), ptByRef);    }
#line 1870 "cscript.c"
        break;
      case 135:
#line 344 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(parameter, String(yymsp[-2].minor.yy0), ptByVal, yymsp[-3].minor.yy71,         yymsp[0].minor.yy71); }
#line 1875 "cscript.c"
        break;
      case 136:
#line 345 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(parameter, String(yymsp[-2].minor.yy0), ptByVal, AstData(), yymsp[0].minor.yy71); }
#line 1880 "cscript.c"
        break;
      case 137:
#line 348 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(parameter, String(yymsp[-1].minor.yy0), ptVariadic); }
#line 1885 "cscript.c"
        break;
      case 138:
      case 160:
#line 352 "cscript.in"
{ yygotominor.yy155 = new AstList; }
#line 1891 "cscript.c"
        break;
      case 139:
      case 140:
#line 353 "cscript.in"
{ yygotominor.yy155 = yymsp[0].minor.yy155; }
#line 1897 "cscript.c"
        break;
      case 142:
#line 356 "cscript.in"
{ yygotominor.yy155 = yymsp[-2].minor.yy155; yymsp[-2].minor.yy155->adopt(*yymsp[0].minor.yy155); }
#line 1902 "cscript.c"
        break;
      case 149:
#line 380 "cscript.in"
{ 
  yygotominor.yy71 = new Ast(extern_declaration, String(yymsp[-4].minor.yy0), yymsp[-2].minor.yy155, String(yymsp[-6].minor.yy0), yymsp[-5].minor.yy71); 
}
#line 1909 "cscript.c"
        break;
      case 150:
#line 390 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(for_statement, yymsp[-5].minor.yy71, yymsp[-4].minor.yy71, yymsp[-2].minor.yy71, yymsp[0].minor.yy71); }
#line 1914 "cscript.c"
        break;
      case 153:
      case 154:
#line 401 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(foreach_statement, yymsp[-4].minor.yy71, yymsp[-2].minor.yy71, yymsp[0].minor.yy71); }
#line 1920 "cscript.c"
        break;
      case 155:
#line 403 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(variable_declaration, String(yymsp[0].minor.yy0)); }
#line 1925 "cscript.c"
        break;
      case 156:
#line 414 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(if_statement, yymsp[-2].minor.yy71, yymsp[0].minor.yy71); }
#line 1930 "cscript.c"
        break;
      case 157:
#line 415 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(if_statement, yymsp[-4].minor.yy71, yymsp[-2].minor.yy71, yymsp[0].minor.yy71); }
#line 1935 "cscript.c"
        break;
      case 158:
#line 423 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(while_statement, yymsp[-2].minor.yy71,  yymsp[0].minor.yy71); }
#line 1940 "cscript.c"
        break;
      case 159:
#line 431 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(switch_statement, yymsp[-4].minor.yy71, yymsp[-1].minor.yy155); }
#line 1945 "cscript.c"
        break;
      case 161:
      case 162:
#line 436 "cscript.in"
{ yygotominor.yy155 = yymsp[-1].minor.yy155; yygotominor.yy155->push_back(yymsp[0].minor.yy71); }
#line 1951 "cscript.c"
        break;
      case 163:
#line 440 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(switch_case, yymsp[-2].minor.yy71, yymsp[0].minor.yy71); }
#line 1956 "cscript.c"
        break;
      case 164:
#line 443 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(default_case, yymsp[0].minor.yy71); }
#line 1961 "cscript.c"
        break;
      case 166:
#line 453 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(try_statement, yymsp[-1].minor.yy71, yymsp[0].minor.yy71); }
#line 1966 "cscript.c"
        break;
      case 167:
#line 454 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(try_statement, yymsp[-1].minor.yy71, AstData(), yymsp[0].minor.yy71); }
#line 1971 "cscript.c"
        break;
      case 168:
#line 455 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(try_statement, yymsp[-2].minor.yy71, yymsp[-1].minor.yy71, yymsp[0].minor.yy71); }
#line 1976 "cscript.c"
        break;
      case 169:
#line 457 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(catch_block, String(yymsp[-2].minor.yy0), yymsp[0].minor.yy71); }
#line 1981 "cscript.c"
        break;
      case 170:
#line 459 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(finally_block, yymsp[0].minor.yy71); }
#line 1986 "cscript.c"
        break;
      case 171:
#line 461 "cscript.in"
{ yygotominor.yy71 = p->AllocAst(throw_statement, yymsp[-1].minor.yy71); }
#line 1991 "cscript.c"
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
#line 2039 "cscript.c"
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
#line 2057 "cscript.c"
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

