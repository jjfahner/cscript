/* Driver template for the LEMON parser generator.
** The author disclaims copyright to this source code.
*/
/* First off, code is include which follows the "include" declaration
** in the input file. */
#include <stdio.h>
#line 25 "cscript.in"


#include "tokens.h"
#include "parser.h"

#pragma warning(disable:4065)

//
// Some defines to make the code a bit cleaner
//
#define INS(arg)      (p->PushByte(TOK_##arg))
#define PUSHB(arg)    (p->PushByte(arg))
#define PUSHW(arg)    (p->PushWord(arg))
#define PUSHQ(arg)    (p->PushQuad(arg))
#define PUSHFRAME()   (p->PushFrame())
#define POPFRAME()    (p->PopFrame())
#define PATCH(arg)    (p->SetQuad(p->PopOffset(arg), p->GetPos()))
#define PUSHCALL(arg) (p->PushCall(arg))
#define POPCALL()     (p->PopCall())
#define PUSHARG()     (p->PushArg())

#line 31 "cscript.c"
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
#define YYNOCODE 130
#define YYACTIONTYPE unsigned short int
#define CScriptParseTOKENTYPE  Token 
typedef union {
  CScriptParseTOKENTYPE yy0;
  int yy259;
} YYMINORTYPE;
#ifndef YYSTACKDEPTH
#define YYSTACKDEPTH 100
#endif
#define CScriptParseARG_SDECL  Parser* p ;
#define CScriptParseARG_PDECL , Parser* p 
#define CScriptParseARG_FETCH  Parser* p  = yypParser->p 
#define CScriptParseARG_STORE yypParser->p  = p 
#define YYNSTATE 215
#define YYNRULE 137
#define YYERRORSYMBOL 54
#define YYERRSYMDT yy259
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
 /*     0 */   353,  108,    7,  198,  191,  185,  183,  181,  176,  175,
 /*    10 */   172,  170,   83,  169,  127,  168,   67,   88,   99,  193,
 /*    20 */   141,   79,  113,  107,  111,  109,  122,   11,  209,  199,
 /*    30 */   169,   92,  168,   67,   88,  213,  148,  141,   79,  113,
 /*    40 */   107,  111,  109,  122,   11,  119,  120,  121,   92,   93,
 /*    50 */   116,  195,  100,   75,    2,  146,   85,  141,   79,  113,
 /*    60 */   107,  111,  109,  122,   11,   14,    1,   58,   92,  200,
 /*    70 */    94,  136,  191,  185,  183,  181,  176,  175,  172,  170,
 /*    80 */    83,  169,   95,  168,   67,   88,  190,   84,  141,   79,
 /*    90 */   113,  107,  111,  109,  122,   11,   50,   64,  117,   92,
 /*   100 */   210,   89,   48,   47,  148,  155,  123,   24,   61,  128,
 /*   110 */   130,  131,  134,  135,  137,   87,  138,  211,  144,  171,
 /*   120 */     3,  139,    2,  187,   85,   45,   44,   43,   42,   41,
 /*   130 */    40,   38,   37,   36,   96,   58,  196,  212,   94,  192,
 /*   140 */    60,    7,  198,  191,  185,  183,  181,  176,  175,  172,
 /*   150 */   170,   83,  169,   21,  168,   67,   88,  151,   17,  141,
 /*   160 */    79,  113,  107,  111,  109,  122,   11,   38,   37,   36,
 /*   170 */    92,  132,  129,   57,   23,  148,  152,   52,   31,   33,
 /*   180 */    34,   35,   39,   46,   45,   44,   43,   42,   41,   40,
 /*   190 */    38,   37,   36,    2,    8,   85,    6,  118,   79,  113,
 /*   200 */   107,  111,  109,  122,   11,   19,   58,  182,   92,   94,
 /*   210 */   179,  191,  185,  183,  181,  176,  175,  172,  170,   83,
 /*   220 */   169,    5,  168,   67,   88,   16,   22,  141,   79,  113,
 /*   230 */   107,  111,  109,  122,   11,   51,  126,   53,   92,   48,
 /*   240 */    47,  123,  155,  148,   24,  142,  128,  130,  131,  134,
 /*   250 */   135,  137,   87,   66,   49,  145,   54,   48,   47,   10,
 /*   260 */   155,    2,   24,   85,  128,  130,  131,  134,  135,  137,
 /*   270 */    87,    4,  197,  214,   58,   13,  177,   94,  174,  191,
 /*   280 */   185,  183,  181,  176,  175,  172,  170,   83,  169,   62,
 /*   290 */   168,   67,   88,   15,  207,  141,   79,  113,  107,  111,
 /*   300 */   109,  122,   11,  101,  287,   82,   92,  125,  120,  121,
 /*   310 */    20,  148,  149,  147,  168,   67,   88,    9,  150,  141,
 /*   320 */    79,  113,  107,  111,  109,  122,   11,  354,   77,    2,
 /*   330 */    92,   85,  141,   79,  113,  107,  111,  109,  122,   11,
 /*   340 */   156,  173,   58,   92,  143,   94,  167,  191,  185,  183,
 /*   350 */   181,  176,  175,  172,  170,   83,  169,  354,  168,   67,
 /*   360 */    88,  354,  354,  141,   79,  113,  107,  111,  109,  122,
 /*   370 */    11,  133,  354,  354,   92,  354,  354,  208,   63,  148,
 /*   380 */   354,  354,  354,   33,   34,   35,   39,   46,   45,   44,
 /*   390 */    43,   42,   41,   40,   38,   37,   36,    2,  354,   85,
 /*   400 */   354,  354,  354,  165,   41,   40,   38,   37,   36,  354,
 /*   410 */    58,  354,  354,   94,   65,  191,  185,  183,  181,  176,
 /*   420 */   175,  172,  170,   83,  169,  354,  168,   67,   88,  354,
 /*   430 */   354,  141,   79,  113,  107,  111,  109,  122,   11,  354,
 /*   440 */   354,  354,   92,  354,  354,  354,  354,  148,   35,   39,
 /*   450 */    46,   45,   44,   43,   42,   41,   40,   38,   37,   36,
 /*   460 */   354,  354,  354,  354,   81,    2,  354,   85,  141,   79,
 /*   470 */   113,  107,  111,  109,  122,   11,  354,  354,   58,   92,
 /*   480 */   354,   94,  194,  191,  185,  183,  181,  176,  175,  172,
 /*   490 */   170,   83,  169,  354,  168,   67,   88,  354,  354,  141,
 /*   500 */    79,  113,  107,  111,  109,  122,   11,  354,  354,  354,
 /*   510 */    92,  354,  354,  354,  354,  148,  157,  354,  168,   67,
 /*   520 */    88,  354,  354,  141,   79,  113,  107,  111,  109,  122,
 /*   530 */    11,  354,  354,    2,   92,   85,  354,  354,  354,  354,
 /*   540 */   354,  354,  354,  354,  354,  354,   58,  354,  354,   94,
 /*   550 */    55,  191,  185,  183,  181,  176,  175,  172,  170,   83,
 /*   560 */   169,  354,  168,   67,   88,  354,  354,  141,   79,  113,
 /*   570 */   107,  111,  109,  122,   11,  354,  354,  354,   92,  354,
 /*   580 */   354,  354,  354,  148,  154,  354,  168,   67,   88,  354,
 /*   590 */   354,  141,   79,  113,  107,  111,  109,  122,   11,  354,
 /*   600 */    76,    2,   92,   85,  141,   79,  113,  107,  111,  109,
 /*   610 */   122,   11,  354,  354,   58,   92,  354,   94,  354,   32,
 /*   620 */    31,   33,   34,   35,   39,   46,   45,   44,   43,   42,
 /*   630 */    41,   40,   38,   37,   36,   25,   28,   30,   29,   26,
 /*   640 */    27,  240,  354,  354,  354,   48,   47,  354,  155,  354,
 /*   650 */    24,  354,  128,  130,  131,  134,  135,  137,   87,  354,
 /*   660 */    49,  145,   54,  153,  140,   18,  103,  166,  354,  354,
 /*   670 */    59,  178,  354,  188,   48,   47,  354,  155,  354,   24,
 /*   680 */   354,  128,  130,  131,  134,  135,  137,   87,  354,   49,
 /*   690 */   145,   54,  153,  354,  354,  103,  166,  205,  206,   59,
 /*   700 */   178,  354,  188,  354,   83,  169,  354,  168,   67,   88,
 /*   710 */   354,  354,  141,   79,  113,  107,  111,  109,  122,   11,
 /*   720 */   354,  354,  354,   92,  354,  354,  354,  354,  148,  162,
 /*   730 */   169,  201,  168,   67,   88,  354,  354,  141,   79,  113,
 /*   740 */   107,  111,  109,  122,   11,  354,  354,  354,   92,   12,
 /*   750 */   354,  203,  169,  204,  168,   67,   88,  354,  354,  141,
 /*   760 */    79,  113,  107,  111,  109,  122,   11,  354,  354,  354,
 /*   770 */    92,  354,  354,  354,  354,  354,   91,  158,  169,  354,
 /*   780 */   168,   67,   88,  354,  354,  141,   79,  113,  107,  111,
 /*   790 */   109,  122,   11,   56,  354,  184,   92,   90,  354,  199,
 /*   800 */   169,  202,  168,   67,   88,  354,  354,  141,   79,  113,
 /*   810 */   107,  111,  109,  122,   11,  354,  354,  354,   92,  180,
 /*   820 */   169,  186,  168,   67,   88,   86,  354,  141,   79,  113,
 /*   830 */   107,  111,  109,  122,   11,  354,  354,  354,   92,  354,
 /*   840 */   354,  354,  354,  354,  158,  169,  354,  168,   67,   88,
 /*   850 */   354,  354,  141,   79,  113,  107,  111,  109,  122,   11,
 /*   860 */   354,  354,  160,   92,  189,  169,  354,  168,   67,   88,
 /*   870 */   354,  354,  141,   79,  113,  107,  111,  109,  122,   11,
 /*   880 */   354,  102,  169,   92,  168,   67,   88,  354,  354,  141,
 /*   890 */    79,  113,  107,  111,  109,  122,   11,  354,   97,  169,
 /*   900 */    92,  168,   67,   88,  354,  354,  141,   79,  113,  107,
 /*   910 */   111,  109,  122,   11,  354,  354,  354,   92,  354,  354,
 /*   920 */   354,  354,  105,  169,   98,  168,   67,   88,  354,  354,
 /*   930 */   141,   79,  113,  107,  111,  109,  122,   11,  354,  354,
 /*   940 */   354,   92,  124,  169,  354,  168,   67,   88,  354,  354,
 /*   950 */   141,   79,  113,  107,  111,  109,  122,   11,  354,  104,
 /*   960 */   169,   92,  168,   67,   88,  354,  354,  141,   79,  113,
 /*   970 */   107,  111,  109,  122,   11,  354,  106,  169,   92,  168,
 /*   980 */    67,   88,  354,  354,  141,   79,  113,  107,  111,  109,
 /*   990 */   122,   11,  354,  354,  159,   92,  168,   67,   88,  354,
 /*  1000 */   354,  141,   79,  113,  107,  111,  109,  122,   11,  354,
 /*  1010 */   354,  163,   92,  168,   67,   88,  354,  354,  141,   79,
 /*  1020 */   113,  107,  111,  109,  122,   11,  354,  354,  354,   92,
 /*  1030 */    34,   35,   39,   46,   45,   44,   43,   42,   41,   40,
 /*  1040 */    38,   37,   36,  164,  354,  168,   67,   88,  354,  354,
 /*  1050 */   141,   79,  113,  107,  111,  109,  122,   11,  354,  354,
 /*  1060 */   161,   92,  168,   67,   88,  354,  354,  141,   79,  113,
 /*  1070 */   107,  111,  109,  122,   11,  354,  354,  354,   92,   39,
 /*  1080 */    46,   45,   44,   43,   42,   41,   40,   38,   37,   36,
 /*  1090 */    80,  354,  354,  354,  141,   79,  113,  107,  111,  109,
 /*  1100 */   122,   11,   78,  354,  354,   92,  141,   79,  113,  107,
 /*  1110 */   111,  109,  122,   11,   73,  354,  354,   92,  141,   79,
 /*  1120 */   113,  107,  111,  109,  122,   11,   74,  354,  354,   92,
 /*  1130 */   141,   79,  113,  107,  111,  109,  122,   11,  354,  110,
 /*  1140 */   354,   92,  354,  141,   79,  113,  107,  111,  109,  122,
 /*  1150 */    11,   69,  354,  354,   92,  141,   79,  113,  107,  111,
 /*  1160 */   109,  122,   11,  354,  354,  354,   92,  112,  354,  354,
 /*  1170 */   354,  141,   79,  113,  107,  111,  109,  122,   11,   68,
 /*  1180 */   354,  354,   92,  141,   79,  113,  107,  111,  109,  122,
 /*  1190 */    11,   70,  354,  354,   92,  141,   79,  113,  107,  111,
 /*  1200 */   109,  122,   11,   71,  354,  354,   92,  141,   79,  113,
 /*  1210 */   107,  111,  109,  122,   11,  114,  354,  354,   92,  141,
 /*  1220 */    79,  113,  107,  111,  109,  122,   11,   72,  354,  354,
 /*  1230 */    92,  141,   79,  113,  107,  111,  109,  122,   11,  354,
 /*  1240 */   354,  354,   92,  115,   79,  113,  107,  111,  109,  122,
 /*  1250 */    11,  354,  354,  354,   92,
};
static const YYCODETYPE yy_lookahead[] = {
 /*     0 */    55,   56,   57,   58,   59,   60,   61,   62,   63,   64,
 /*    10 */    65,   66,   67,   68,   93,   70,   71,   72,   97,   95,
 /*    20 */    75,   76,   77,   78,   79,   80,   81,   82,   38,   67,
 /*    30 */    68,   86,   70,   71,   72,  101,   91,   75,   76,   77,
 /*    40 */    78,   79,   80,   81,   82,   94,   95,   96,   86,   87,
 /*    50 */    31,   89,   90,   71,  109,   29,  111,   75,   76,   77,
 /*    60 */    78,   79,   80,   81,   82,   39,  105,  122,   86,   31,
 /*    70 */   125,   58,   59,   60,   61,   62,   63,   64,   65,   66,
 /*    80 */    67,   68,   99,   70,   71,   72,   78,   34,   75,   76,
 /*    90 */    77,   78,   79,   80,   81,   82,   39,   98,   41,   86,
 /*   100 */   101,  102,   25,   26,   91,   28,   38,   30,   40,   32,
 /*   110 */    33,   34,   35,   36,   37,   38,  103,  104,   41,  119,
 /*   120 */   120,  108,  109,  128,  111,    8,    9,   10,   11,   12,
 /*   130 */    13,   14,   15,   16,  126,  122,   31,   88,  125,   38,
 /*   140 */    56,   57,   58,   59,   60,   61,   62,   63,   64,   65,
 /*   150 */    66,   67,   68,   17,   70,   71,   72,  110,   39,   75,
 /*   160 */    76,   77,   78,   79,   80,   81,   82,   14,   15,   16,
 /*   170 */    86,   25,   26,   27,   28,   91,   44,   30,    2,    3,
 /*   180 */     4,    5,    6,    7,    8,    9,   10,   11,   12,   13,
 /*   190 */    14,   15,   16,  109,  118,  111,   31,   75,   76,   77,
 /*   200 */    78,   79,   80,   81,   82,   30,  122,   29,   86,  125,
 /*   210 */    58,   59,   60,   61,   62,   63,   64,   65,   66,   67,
 /*   220 */    68,  123,   70,   71,   72,   52,   30,   75,   76,   77,
 /*   230 */    78,   79,   80,   81,   82,   31,   78,   30,   86,   25,
 /*   240 */    26,   38,   28,   91,   30,   84,   32,   33,   34,   35,
 /*   250 */    36,   37,   38,   31,   40,   41,   42,   25,   26,   30,
 /*   260 */    28,  109,   30,  111,   32,   33,   34,   35,   36,   37,
 /*   270 */    38,   31,   49,   38,  122,   41,  124,  125,   58,   59,
 /*   280 */    60,   61,   62,   63,   64,   65,   66,   67,   68,   39,
 /*   290 */    70,   71,   72,   23,   43,   75,   76,   77,   78,   79,
 /*   300 */    80,   81,   82,   38,   30,   92,   86,   94,   95,   96,
 /*   310 */    24,   91,   68,   41,   70,   71,   72,   30,   74,   75,
 /*   320 */    76,   77,   78,   79,   80,   81,   82,  129,   71,  109,
 /*   330 */    86,  111,   75,   76,   77,   78,   79,   80,   81,   82,
 /*   340 */    41,  121,  122,   86,   41,  125,   58,   59,   60,   61,
 /*   350 */    62,   63,   64,   65,   66,   67,   68,  129,   70,   71,
 /*   360 */    72,  129,  129,   75,   76,   77,   78,   79,   80,   81,
 /*   370 */    82,  100,  129,  129,   86,  129,  129,  106,  107,   91,
 /*   380 */   129,  129,  129,    3,    4,    5,    6,    7,    8,    9,
 /*   390 */    10,   11,   12,   13,   14,   15,   16,  109,  129,  111,
 /*   400 */   129,  129,  129,  115,   12,   13,   14,   15,   16,  129,
 /*   410 */   122,  129,  129,  125,   58,   59,   60,   61,   62,   63,
 /*   420 */    64,   65,   66,   67,   68,  129,   70,   71,   72,  129,
 /*   430 */   129,   75,   76,   77,   78,   79,   80,   81,   82,  129,
 /*   440 */   129,  129,   86,  129,  129,  129,  129,   91,    5,    6,
 /*   450 */     7,    8,    9,   10,   11,   12,   13,   14,   15,   16,
 /*   460 */   129,  129,  129,  129,   71,  109,  129,  111,   75,   76,
 /*   470 */    77,   78,   79,   80,   81,   82,  129,  129,  122,   86,
 /*   480 */   129,  125,   58,   59,   60,   61,   62,   63,   64,   65,
 /*   490 */    66,   67,   68,  129,   70,   71,   72,  129,  129,   75,
 /*   500 */    76,   77,   78,   79,   80,   81,   82,  129,  129,  129,
 /*   510 */    86,  129,  129,  129,  129,   91,   68,  129,   70,   71,
 /*   520 */    72,  129,  129,   75,   76,   77,   78,   79,   80,   81,
 /*   530 */    82,  129,  129,  109,   86,  111,  129,  129,  129,  129,
 /*   540 */   129,  129,  129,  129,  129,  129,  122,  129,  129,  125,
 /*   550 */    58,   59,   60,   61,   62,   63,   64,   65,   66,   67,
 /*   560 */    68,  129,   70,   71,   72,  129,  129,   75,   76,   77,
 /*   570 */    78,   79,   80,   81,   82,  129,  129,  129,   86,  129,
 /*   580 */   129,  129,  129,   91,   68,  129,   70,   71,   72,  129,
 /*   590 */   129,   75,   76,   77,   78,   79,   80,   81,   82,  129,
 /*   600 */    71,  109,   86,  111,   75,   76,   77,   78,   79,   80,
 /*   610 */    81,   82,  129,  129,  122,   86,  129,  125,  129,    1,
 /*   620 */     2,    3,    4,    5,    6,    7,    8,    9,   10,   11,
 /*   630 */    12,   13,   14,   15,   16,   17,   18,   19,   20,   21,
 /*   640 */    22,   23,  129,  129,  129,   25,   26,  129,   28,  129,
 /*   650 */    30,  129,   32,   33,   34,   35,   36,   37,   38,  129,
 /*   660 */    40,   41,   42,   43,   44,   45,   46,   47,  129,  129,
 /*   670 */    50,   51,  129,   53,   25,   26,  129,   28,  129,   30,
 /*   680 */   129,   32,   33,   34,   35,   36,   37,   38,  129,   40,
 /*   690 */    41,   42,   43,  129,  129,   46,   47,   60,   61,   50,
 /*   700 */    51,  129,   53,  129,   67,   68,  129,   70,   71,   72,
 /*   710 */   129,  129,   75,   76,   77,   78,   79,   80,   81,   82,
 /*   720 */   129,  129,  129,   86,  129,  129,  129,  129,   91,   67,
 /*   730 */    68,   69,   70,   71,   72,  129,  129,   75,   76,   77,
 /*   740 */    78,   79,   80,   81,   82,  129,  129,  129,   86,  112,
 /*   750 */   129,   67,   68,  116,   70,   71,   72,  129,  129,   75,
 /*   760 */    76,   77,   78,   79,   80,   81,   82,  129,  129,  129,
 /*   770 */    86,  129,  129,  129,  129,  129,  114,   67,   68,  129,
 /*   780 */    70,   71,   72,  129,  129,   75,   76,   77,   78,   79,
 /*   790 */    80,   81,   82,   83,  129,   85,   86,  113,  129,   67,
 /*   800 */    68,  117,   70,   71,   72,  129,  129,   75,   76,   77,
 /*   810 */    78,   79,   80,   81,   82,  129,  129,  129,   86,   67,
 /*   820 */    68,   89,   70,   71,   72,   73,  129,   75,   76,   77,
 /*   830 */    78,   79,   80,   81,   82,  129,  129,  129,   86,  129,
 /*   840 */   129,  129,  129,  129,   67,   68,  129,   70,   71,   72,
 /*   850 */   129,  129,   75,   76,   77,   78,   79,   80,   81,   82,
 /*   860 */   129,  129,   85,   86,   67,   68,  129,   70,   71,   72,
 /*   870 */   129,  129,   75,   76,   77,   78,   79,   80,   81,   82,
 /*   880 */   129,   67,   68,   86,   70,   71,   72,  129,  129,   75,
 /*   890 */    76,   77,   78,   79,   80,   81,   82,  129,   67,   68,
 /*   900 */    86,   70,   71,   72,  129,  129,   75,   76,   77,   78,
 /*   910 */    79,   80,   81,   82,  129,  129,  129,   86,  129,  129,
 /*   920 */   129,  129,   67,   68,  127,   70,   71,   72,  129,  129,
 /*   930 */    75,   76,   77,   78,   79,   80,   81,   82,  129,  129,
 /*   940 */   129,   86,   67,   68,  129,   70,   71,   72,  129,  129,
 /*   950 */    75,   76,   77,   78,   79,   80,   81,   82,  129,   67,
 /*   960 */    68,   86,   70,   71,   72,  129,  129,   75,   76,   77,
 /*   970 */    78,   79,   80,   81,   82,  129,   67,   68,   86,   70,
 /*   980 */    71,   72,  129,  129,   75,   76,   77,   78,   79,   80,
 /*   990 */    81,   82,  129,  129,   68,   86,   70,   71,   72,  129,
 /*  1000 */   129,   75,   76,   77,   78,   79,   80,   81,   82,  129,
 /*  1010 */   129,   68,   86,   70,   71,   72,  129,  129,   75,   76,
 /*  1020 */    77,   78,   79,   80,   81,   82,  129,  129,  129,   86,
 /*  1030 */     4,    5,    6,    7,    8,    9,   10,   11,   12,   13,
 /*  1040 */    14,   15,   16,   68,  129,   70,   71,   72,  129,  129,
 /*  1050 */    75,   76,   77,   78,   79,   80,   81,   82,  129,  129,
 /*  1060 */    68,   86,   70,   71,   72,  129,  129,   75,   76,   77,
 /*  1070 */    78,   79,   80,   81,   82,  129,  129,  129,   86,    6,
 /*  1080 */     7,    8,    9,   10,   11,   12,   13,   14,   15,   16,
 /*  1090 */    71,  129,  129,  129,   75,   76,   77,   78,   79,   80,
 /*  1100 */    81,   82,   71,  129,  129,   86,   75,   76,   77,   78,
 /*  1110 */    79,   80,   81,   82,   71,  129,  129,   86,   75,   76,
 /*  1120 */    77,   78,   79,   80,   81,   82,   71,  129,  129,   86,
 /*  1130 */    75,   76,   77,   78,   79,   80,   81,   82,  129,   71,
 /*  1140 */   129,   86,  129,   75,   76,   77,   78,   79,   80,   81,
 /*  1150 */    82,   71,  129,  129,   86,   75,   76,   77,   78,   79,
 /*  1160 */    80,   81,   82,  129,  129,  129,   86,   71,  129,  129,
 /*  1170 */   129,   75,   76,   77,   78,   79,   80,   81,   82,   71,
 /*  1180 */   129,  129,   86,   75,   76,   77,   78,   79,   80,   81,
 /*  1190 */    82,   71,  129,  129,   86,   75,   76,   77,   78,   79,
 /*  1200 */    80,   81,   82,   71,  129,  129,   86,   75,   76,   77,
 /*  1210 */    78,   79,   80,   81,   82,   71,  129,  129,   86,   75,
 /*  1220 */    76,   77,   78,   79,   80,   81,   82,   71,  129,  129,
 /*  1230 */    86,   75,   76,   77,   78,   79,   80,   81,   82,  129,
 /*  1240 */   129,  129,   86,   75,   76,   77,   78,   79,   80,   81,
 /*  1250 */    82,  129,  129,  129,   86,
};
#define YY_SHIFT_USE_DFLT (-11)
#define YY_SHIFT_MAX 106
static const short yy_shift_ofst[] = {
 /*     0 */   649,  620,  649,  649,  649,  649,  649,  649,  649,  214,
 /*    10 */   232,  232,  232,  232,  232,  232,  232,  232,   77,  232,
 /*    20 */   232,  232,  232,  232,  232,  232,  232,  232,  232,  232,
 /*    30 */   232,  232,  232,  232,  232,  232,  232,  232,  232,  232,
 /*    40 */   232,  232,  232,  232,  232,  232,  232,  232,  232,  265,
 /*    50 */   265,  251,  -10,   68,  235,  223,   26,  203,  196,  175,
 /*    60 */   132,  101,  -10,  -11,  -11,  -11,  -11,  618,  176,  380,
 /*    70 */  1026,  443, 1073,  117,  117,  392,  392,  392,  392,  146,
 /*    80 */   153,  153,   57,  272,  299,  287,  286,  274,  270,  250,
 /*    90 */   234,  240,  229,  222,  207,  204,  173,  178,  165,  147,
 /*   100 */   119,  136,  105,   53,   38,   19,  303,
};
#define YY_REDUCE_USE_DFLT (-80)
#define YY_REDUCE_MAX 66
static const short yy_reduce_ofst[] = {
 /*     0 */   -55,   13,   84,  220,  288,  152,  356,  424,  492,  637,
 /*    10 */   -38,  710,  684,  662,  777,  752,  797,  732,  909,  892,
 /*    20 */   244,  875,  814,  831,  855,  992,  448,  516,  975,  926,
 /*    30 */   943, 1080, 1108, 1120, 1132, 1156, 1144, 1096, 1068, 1043,
 /*    40 */  1019,  393,  -18,  257,  529, 1031, 1055, 1168,  122,  213,
 /*    50 */   -49,  271,   -1,    8,  -79,    0,  161,  158,   98,   76,
 /*    60 */    47,  -76,  -66,  -39,  -17,   -5,   49,
};
static const YYACTIONTYPE yy_default[] = {
 /*     0 */   218,  352,  218,  352,  352,  352,  352,  219,  352,  352,
 /*    10 */   292,  352,  352,  229,  352,  352,  352,  352,  352,  352,
 /*    20 */   352,  352,  352,  352,  352,  352,  352,  352,  352,  352,
 /*    30 */   352,  352,  352,  352,  352,  352,  352,  352,  352,  352,
 /*    40 */   352,  352,  352,  352,  352,  352,  352,  352,  352,  352,
 /*    50 */   352,  352,  307,  352,  352,  338,  352,  352,  352,  352,
 /*    60 */   352,  352,  352,  312,  309,  351,  288,  238,  244,  245,
 /*    70 */   246,  247,  248,  249,  250,  254,  252,  253,  251,  260,
 /*    80 */   256,  255,  352,  352,  352,  352,  352,  279,  352,  308,
 /*    90 */   352,  352,  352,  352,  352,  352,  352,  352,  352,  352,
 /*   100 */   293,  300,  352,  352,  352,  352,  352,  270,  215,  269,
 /*   110 */   257,  268,  258,  263,  259,  262,  271,  294,  261,  297,
 /*   120 */   298,  299,  272,  279,  301,  296,  266,  295,  273,  265,
 /*   130 */   274,  275,  264,  302,  276,  277,  310,  278,  313,  314,
 /*   140 */   316,  243,  280,  318,  319,  320,  282,  321,  322,  242,
 /*   150 */   239,  323,  325,  324,  237,  281,  326,  236,  283,  235,
 /*   160 */   285,  232,  230,  234,  233,  327,  328,  332,  231,  228,
 /*   170 */   227,  336,  226,  339,  341,  225,  224,  342,  343,  345,
 /*   180 */   241,  223,  267,  222,  284,  221,  291,  346,  347,  350,
 /*   190 */   348,  220,  300,  349,  217,  290,  344,  340,  216,  289,
 /*   200 */   337,  331,  330,  333,  329,  334,  335,  315,  317,  304,
 /*   210 */   305,  311,  286,  306,  303,
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
  "LSBRACKET",     "RSBRACKET",     "LRBRACKET",     "RRBRACKET",   
  "INTEGER",       "REAL",          "STRING",        "TRUE",        
  "FALSE",         "NULL",          "IDENTIFIER",    "COMMA",       
  "VAR",           "SEMICOLON",     "FUN",           "LCBRACKET",   
  "RCBRACKET",     "RETURN",        "INCLUDE",       "FOR",         
  "LOWER_THAN_ELSE",  "ELSE",          "IF",            "WHILE",       
  "IN",            "FOREACH",       "error",         "translation_unit",
  "statement_sequence_opt",  "statement_sequence",  "statement",     "include_statement",
  "expression_statement",  "declaration_statement",  "for_statement",  "compound_statement",
  "if_statement",  "while_statement",  "foreach_statement",  "expression",  
  "assignment_expression",  "expression_opt",  "conditional_expression",  "binary_expression",
  "cond_part",     "cond_expr_true",  "cond_expr_false",  "unary_expression",
  "postfix_expression",  "primary_expression",  "id_expression",  "function_call",
  "literal",       "list_definition",  "list_start",    "list_content",
  "list_end",      "list_entry",    "function_call_start",  "argument_list",
  "function_call_end",  "argument",      "arguments",     "declaration", 
  "declaration_sequence",  "function_declaration",  "var_declaration",  "simple_declaration",
  "init_declaration",  "function_name",  "parameter_list",  "parameters_done",
  "function_body",  "parameter",     "parameters",    "function_statement",
  "return_statement",  "function_statements",  "function_block",  "function_block_start",
  "function_block_end",  "compound_start",  "compound_end",  "for_start",   
  "for_init",      "for_cond",      "for_expr",      "for_end",     
  "for_init_statement",  "for_condition",  "if_cond",       "if_else",     
  "if_else_1",     "if_else_2",     "while_start",   "while_condition",
  "while_content",  "foreach_init",  "for_lvalue",    "for_expression",
  "foreach_end", 
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
 /*  13 */ "expression ::= assignment_expression",
 /*  14 */ "expression_opt ::=",
 /*  15 */ "expression_opt ::= expression",
 /*  16 */ "assignment_expression ::= conditional_expression",
 /*  17 */ "assignment_expression ::= binary_expression ASSIGN assignment_expression",
 /*  18 */ "assignment_expression ::= binary_expression ASSADD assignment_expression",
 /*  19 */ "assignment_expression ::= binary_expression ASSSUB assignment_expression",
 /*  20 */ "assignment_expression ::= binary_expression ASSMUL assignment_expression",
 /*  21 */ "assignment_expression ::= binary_expression ASSDIV assignment_expression",
 /*  22 */ "assignment_expression ::= binary_expression ASSMOD assignment_expression",
 /*  23 */ "conditional_expression ::= binary_expression",
 /*  24 */ "conditional_expression ::= cond_part QUESTION cond_expr_true COLON cond_expr_false",
 /*  25 */ "cond_part ::= binary_expression",
 /*  26 */ "cond_expr_true ::= expression",
 /*  27 */ "cond_expr_false ::= assignment_expression",
 /*  28 */ "binary_expression ::= unary_expression",
 /*  29 */ "binary_expression ::= binary_expression LOGOR binary_expression",
 /*  30 */ "binary_expression ::= binary_expression LOGAND binary_expression",
 /*  31 */ "binary_expression ::= binary_expression BITOR binary_expression",
 /*  32 */ "binary_expression ::= binary_expression BITXOR binary_expression",
 /*  33 */ "binary_expression ::= binary_expression BITAND binary_expression",
 /*  34 */ "binary_expression ::= binary_expression EQUALS binary_expression",
 /*  35 */ "binary_expression ::= binary_expression NEQUALS binary_expression",
 /*  36 */ "binary_expression ::= binary_expression ST binary_expression",
 /*  37 */ "binary_expression ::= binary_expression SE binary_expression",
 /*  38 */ "binary_expression ::= binary_expression GT binary_expression",
 /*  39 */ "binary_expression ::= binary_expression GE binary_expression",
 /*  40 */ "binary_expression ::= binary_expression ADDOP binary_expression",
 /*  41 */ "binary_expression ::= binary_expression SUBOP binary_expression",
 /*  42 */ "binary_expression ::= binary_expression MULOP binary_expression",
 /*  43 */ "binary_expression ::= binary_expression DIVOP binary_expression",
 /*  44 */ "binary_expression ::= binary_expression MODOP binary_expression",
 /*  45 */ "unary_expression ::= postfix_expression",
 /*  46 */ "unary_expression ::= ADDADD unary_expression",
 /*  47 */ "unary_expression ::= SUBSUB unary_expression",
 /*  48 */ "postfix_expression ::= primary_expression",
 /*  49 */ "postfix_expression ::= postfix_expression ADDADD",
 /*  50 */ "postfix_expression ::= postfix_expression SUBSUB",
 /*  51 */ "postfix_expression ::= postfix_expression DOT id_expression",
 /*  52 */ "postfix_expression ::= postfix_expression LSBRACKET expression RSBRACKET",
 /*  53 */ "postfix_expression ::= function_call",
 /*  54 */ "primary_expression ::= literal",
 /*  55 */ "primary_expression ::= id_expression",
 /*  56 */ "primary_expression ::= LRBRACKET expression RRBRACKET",
 /*  57 */ "primary_expression ::= list_definition",
 /*  58 */ "literal ::= INTEGER",
 /*  59 */ "literal ::= REAL",
 /*  60 */ "literal ::= STRING",
 /*  61 */ "literal ::= TRUE",
 /*  62 */ "literal ::= FALSE",
 /*  63 */ "literal ::= NULL",
 /*  64 */ "id_expression ::= IDENTIFIER",
 /*  65 */ "list_definition ::= list_start list_content list_end",
 /*  66 */ "list_start ::= LSBRACKET",
 /*  67 */ "list_end ::= RSBRACKET",
 /*  68 */ "list_entry ::= expression",
 /*  69 */ "list_content ::= list_entry",
 /*  70 */ "list_content ::= list_content COMMA list_entry",
 /*  71 */ "function_call ::= function_call_start LRBRACKET argument_list RRBRACKET function_call_end",
 /*  72 */ "function_call_start ::= IDENTIFIER",
 /*  73 */ "function_call_end ::=",
 /*  74 */ "argument ::= expression",
 /*  75 */ "arguments ::= argument",
 /*  76 */ "arguments ::= arguments COMMA argument",
 /*  77 */ "argument_list ::=",
 /*  78 */ "argument_list ::= arguments",
 /*  79 */ "declaration ::= VAR declaration_sequence SEMICOLON",
 /*  80 */ "declaration ::= FUN function_declaration",
 /*  81 */ "declaration_sequence ::= var_declaration",
 /*  82 */ "declaration_sequence ::= declaration_sequence COMMA var_declaration",
 /*  83 */ "var_declaration ::= simple_declaration",
 /*  84 */ "var_declaration ::= init_declaration",
 /*  85 */ "simple_declaration ::= IDENTIFIER",
 /*  86 */ "init_declaration ::= IDENTIFIER ASSIGN expression",
 /*  87 */ "function_declaration ::= function_name LRBRACKET parameter_list parameters_done RRBRACKET function_body",
 /*  88 */ "function_name ::= IDENTIFIER",
 /*  89 */ "parameter ::= IDENTIFIER",
 /*  90 */ "parameters ::= parameter",
 /*  91 */ "parameters ::= parameters COMMA parameter",
 /*  92 */ "parameter_list ::=",
 /*  93 */ "parameter_list ::= parameters",
 /*  94 */ "parameters_done ::=",
 /*  95 */ "function_statement ::= statement",
 /*  96 */ "function_statement ::= return_statement",
 /*  97 */ "function_statements ::=",
 /*  98 */ "function_statements ::= function_statements function_statement",
 /*  99 */ "function_block ::= function_block_start function_statements function_block_end",
 /* 100 */ "function_block_start ::= LCBRACKET",
 /* 101 */ "function_block_end ::= RCBRACKET",
 /* 102 */ "function_body ::= function_block",
 /* 103 */ "return_statement ::= RETURN expression SEMICOLON",
 /* 104 */ "return_statement ::= RETURN SEMICOLON",
 /* 105 */ "expression_statement ::= SEMICOLON",
 /* 106 */ "expression_statement ::= expression SEMICOLON",
 /* 107 */ "declaration_statement ::= declaration",
 /* 108 */ "compound_statement ::= compound_start statement_sequence_opt compound_end",
 /* 109 */ "compound_start ::= LCBRACKET",
 /* 110 */ "compound_end ::= RCBRACKET",
 /* 111 */ "include_statement ::= INCLUDE STRING SEMICOLON",
 /* 112 */ "for_statement ::= for_start LRBRACKET for_init for_cond SEMICOLON for_expr RRBRACKET for_end",
 /* 113 */ "for_start ::= FOR",
 /* 114 */ "for_init ::= for_init_statement",
 /* 115 */ "for_cond ::= for_condition",
 /* 116 */ "for_expr ::= expression_opt",
 /* 117 */ "for_end ::= statement",
 /* 118 */ "for_condition ::= expression",
 /* 119 */ "for_init_statement ::= expression_statement",
 /* 120 */ "for_init_statement ::= declaration_statement",
 /* 121 */ "if_statement ::= IF if_cond statement if_else",
 /* 122 */ "if_cond ::= LRBRACKET expression RRBRACKET",
 /* 123 */ "if_else ::=",
 /* 124 */ "if_else ::= if_else_1 if_else_2",
 /* 125 */ "if_else_1 ::= ELSE",
 /* 126 */ "if_else_2 ::= statement",
 /* 127 */ "while_statement ::= while_start while_condition while_content",
 /* 128 */ "while_start ::= WHILE",
 /* 129 */ "while_condition ::= LRBRACKET expression RRBRACKET",
 /* 130 */ "while_content ::= statement",
 /* 131 */ "foreach_statement ::= foreach_init LRBRACKET for_lvalue IN for_expression RRBRACKET statement foreach_end",
 /* 132 */ "foreach_init ::= FOREACH",
 /* 133 */ "for_lvalue ::= id_expression",
 /* 134 */ "for_lvalue ::= VAR simple_declaration",
 /* 135 */ "for_expression ::= expression",
 /* 136 */ "foreach_end ::=",
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
  { 55, 1 },
  { 57, 1 },
  { 57, 2 },
  { 56, 0 },
  { 56, 1 },
  { 58, 1 },
  { 58, 1 },
  { 58, 1 },
  { 58, 1 },
  { 58, 1 },
  { 58, 1 },
  { 58, 1 },
  { 58, 1 },
  { 67, 1 },
  { 69, 0 },
  { 69, 1 },
  { 68, 1 },
  { 68, 3 },
  { 68, 3 },
  { 68, 3 },
  { 68, 3 },
  { 68, 3 },
  { 68, 3 },
  { 70, 1 },
  { 70, 5 },
  { 72, 1 },
  { 73, 1 },
  { 74, 1 },
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
  { 75, 1 },
  { 75, 2 },
  { 75, 2 },
  { 76, 1 },
  { 76, 2 },
  { 76, 2 },
  { 76, 3 },
  { 76, 4 },
  { 76, 1 },
  { 77, 1 },
  { 77, 1 },
  { 77, 3 },
  { 77, 1 },
  { 80, 1 },
  { 80, 1 },
  { 80, 1 },
  { 80, 1 },
  { 80, 1 },
  { 80, 1 },
  { 78, 1 },
  { 81, 3 },
  { 82, 1 },
  { 84, 1 },
  { 85, 1 },
  { 83, 1 },
  { 83, 3 },
  { 79, 5 },
  { 86, 1 },
  { 88, 0 },
  { 89, 1 },
  { 90, 1 },
  { 90, 3 },
  { 87, 0 },
  { 87, 1 },
  { 91, 3 },
  { 91, 2 },
  { 92, 1 },
  { 92, 3 },
  { 94, 1 },
  { 94, 1 },
  { 95, 1 },
  { 96, 3 },
  { 93, 6 },
  { 97, 1 },
  { 101, 1 },
  { 102, 1 },
  { 102, 3 },
  { 98, 0 },
  { 98, 1 },
  { 99, 0 },
  { 103, 1 },
  { 103, 1 },
  { 105, 0 },
  { 105, 2 },
  { 106, 3 },
  { 107, 1 },
  { 108, 1 },
  { 100, 1 },
  { 104, 3 },
  { 104, 2 },
  { 60, 1 },
  { 60, 2 },
  { 61, 1 },
  { 63, 3 },
  { 109, 1 },
  { 110, 1 },
  { 59, 3 },
  { 62, 8 },
  { 111, 1 },
  { 112, 1 },
  { 113, 1 },
  { 114, 1 },
  { 115, 1 },
  { 117, 1 },
  { 116, 1 },
  { 116, 1 },
  { 64, 4 },
  { 118, 3 },
  { 119, 0 },
  { 119, 2 },
  { 120, 1 },
  { 121, 1 },
  { 65, 3 },
  { 122, 1 },
  { 123, 3 },
  { 124, 1 },
  { 66, 8 },
  { 125, 1 },
  { 126, 1 },
  { 126, 2 },
  { 127, 1 },
  { 128, 0 },
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
      case 1:
      case 2:
      case 3:
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
      case 23:
      case 24:
      case 28:
      case 45:
      case 48:
      case 51:
      case 53:
      case 54:
      case 55:
      case 56:
      case 57:
      case 65:
      case 67:
      case 69:
      case 70:
      case 71:
      case 75:
      case 76:
      case 77:
      case 78:
      case 79:
      case 80:
      case 81:
      case 82:
      case 83:
      case 84:
      case 87:
      case 90:
      case 91:
      case 92:
      case 93:
      case 95:
      case 96:
      case 97:
      case 98:
      case 99:
      case 105:
      case 107:
      case 108:
      case 112:
      case 118:
      case 119:
      case 120:
      case 121:
      case 124:
      case 127:
      case 131:
      case 133:
      case 134:
#line 67 "cscript.in"
{
}
#line 1258 "cscript.c"
        break;
      case 17:
#line 96 "cscript.in"
{ INS(ASSIGN); }
#line 1263 "cscript.c"
        break;
      case 18:
#line 97 "cscript.in"
{ INS(ASSADD); }
#line 1268 "cscript.c"
        break;
      case 19:
#line 98 "cscript.in"
{ INS(ASSSUB); }
#line 1273 "cscript.c"
        break;
      case 20:
#line 99 "cscript.in"
{ INS(ASSMUL); }
#line 1278 "cscript.c"
        break;
      case 21:
#line 100 "cscript.in"
{ INS(ASSDIV); }
#line 1283 "cscript.c"
        break;
      case 22:
#line 101 "cscript.in"
{ INS(ASSMOD); }
#line 1288 "cscript.c"
        break;
      case 25:
#line 108 "cscript.in"
{
  INS(JZ);
  p->PushOffset("cond_exp_1");
  PUSHQ(0);
}
#line 1297 "cscript.c"
        break;
      case 26:
#line 113 "cscript.in"
{
  INS(JMP);
  p->PushOffset("cond_exp_2");
  PUSHQ(0);
  p->SetQuad(p->PopOffset("cond_exp_1"), p->GetPos());
}
#line 1307 "cscript.c"
        break;
      case 27:
#line 119 "cscript.in"
{
  p->SetQuad(p->PopOffset("cond_exp_2"), p->GetPos());
}
#line 1314 "cscript.c"
        break;
      case 29:
#line 125 "cscript.in"
{ INS(LOGOR);   }
#line 1319 "cscript.c"
        break;
      case 30:
#line 126 "cscript.in"
{ INS(LOGAND);  }
#line 1324 "cscript.c"
        break;
      case 31:
#line 127 "cscript.in"
{ INS(PREINC);  }
#line 1329 "cscript.c"
        break;
      case 32:
#line 128 "cscript.in"
{ INS(BITXOR);  }
#line 1334 "cscript.c"
        break;
      case 33:
#line 129 "cscript.in"
{ INS(BITAND);  }
#line 1339 "cscript.c"
        break;
      case 34:
#line 130 "cscript.in"
{ INS(EQUALS);  }
#line 1344 "cscript.c"
        break;
      case 35:
#line 131 "cscript.in"
{ INS(NEQUALS); }
#line 1349 "cscript.c"
        break;
      case 36:
#line 132 "cscript.in"
{ INS(ST);      }
#line 1354 "cscript.c"
        break;
      case 37:
#line 133 "cscript.in"
{ INS(SE);      }
#line 1359 "cscript.c"
        break;
      case 38:
#line 134 "cscript.in"
{ INS(GT);      }
#line 1364 "cscript.c"
        break;
      case 39:
#line 135 "cscript.in"
{ INS(GE);      }
#line 1369 "cscript.c"
        break;
      case 40:
#line 136 "cscript.in"
{ INS(ADDOP);   }
#line 1374 "cscript.c"
        break;
      case 41:
#line 137 "cscript.in"
{ INS(SUBOP);   }
#line 1379 "cscript.c"
        break;
      case 42:
#line 138 "cscript.in"
{ INS(MULOP);   }
#line 1384 "cscript.c"
        break;
      case 43:
#line 139 "cscript.in"
{ INS(DIVOP);   }
#line 1389 "cscript.c"
        break;
      case 44:
#line 140 "cscript.in"
{ INS(MODOP);   }
#line 1394 "cscript.c"
        break;
      case 46:
#line 144 "cscript.in"
{ INS(PREINC); }
#line 1399 "cscript.c"
        break;
      case 47:
#line 145 "cscript.in"
{ INS(PRESUB); }
#line 1404 "cscript.c"
        break;
      case 49:
#line 149 "cscript.in"
{ INS(POSTINC); }
#line 1409 "cscript.c"
        break;
      case 50:
#line 150 "cscript.in"
{ INS(POSTSUB); }
#line 1414 "cscript.c"
        break;
      case 52:
#line 152 "cscript.in"
{ INS(INDEX); }
#line 1419 "cscript.c"
        break;
      case 58:
      case 59:
#line 162 "cscript.in"
{ p->PushRVal(Variant(String(yymsp[0].minor.yy0), Variant::stInt)); }
#line 1425 "cscript.c"
        break;
      case 60:
#line 164 "cscript.in"
{ p->PushRVal(Variant(String(yymsp[0].minor.yy0), Variant::stString)); }
#line 1430 "cscript.c"
        break;
      case 61:
#line 165 "cscript.in"
{ p->PushRVal(Variant(true)); }
#line 1435 "cscript.c"
        break;
      case 62:
#line 166 "cscript.in"
{ p->PushRVal(Variant(false)); }
#line 1440 "cscript.c"
        break;
      case 63:
      case 66:
#line 167 "cscript.in"
{ p->PushRVal(Variant()); }
#line 1446 "cscript.c"
        break;
      case 64:
#line 170 "cscript.in"
{ INS(LVALUE); PUSHQ(p->GetVar(yymsp[0].minor.yy0)); }
#line 1451 "cscript.c"
        break;
      case 68:
#line 176 "cscript.in"
{ INS(ARRAY); }
#line 1456 "cscript.c"
        break;
      case 72:
#line 189 "cscript.in"
{ PUSHCALL(yymsp[0].minor.yy0); }
#line 1461 "cscript.c"
        break;
      case 73:
#line 190 "cscript.in"
{ POPCALL(); }
#line 1466 "cscript.c"
        break;
      case 74:
#line 194 "cscript.in"
{ PUSHARG(); }
#line 1471 "cscript.c"
        break;
      case 85:
#line 221 "cscript.in"
{ 
  INS(VAR);      
  PUSHQ(p->AddVar(yymsp[0].minor.yy0)); 
}
#line 1479 "cscript.c"
        break;
      case 86:
#line 227 "cscript.in"
{ 
  INS(VARINIT);  
  PUSHQ(p->AddVar(yymsp[-2].minor.yy0)); 
}
#line 1487 "cscript.c"
        break;
      case 88:
#line 236 "cscript.in"
{	p->PushFunction(yymsp[0].minor.yy0); }
#line 1492 "cscript.c"
        break;
      case 89:
#line 239 "cscript.in"
{ p->AddParam(yymsp[0].minor.yy0); }
#line 1497 "cscript.c"
        break;
      case 94:
#line 250 "cscript.in"
{ p->GenFunProlog(); }
#line 1502 "cscript.c"
        break;
      case 100:
      case 109:
      case 132:
#line 262 "cscript.in"
{ PUSHFRAME(); }
#line 1509 "cscript.c"
        break;
      case 101:
      case 110:
#line 263 "cscript.in"
{ POPFRAME();  }
#line 1515 "cscript.c"
        break;
      case 102:
#line 266 "cscript.in"
{ p->PopFunction(); }
#line 1520 "cscript.c"
        break;
      case 103:
#line 269 "cscript.in"
{ INS(RET); }
#line 1525 "cscript.c"
        break;
      case 104:
#line 270 "cscript.in"
{ p->PushRVal(Variant()); INS(RET); }
#line 1530 "cscript.c"
        break;
      case 106:
#line 280 "cscript.in"
{ INS(POP); }
#line 1535 "cscript.c"
        break;
      case 111:
#line 291 "cscript.in"
{ p->ParseFile(yymsp[-1].minor.yy0); }
#line 1540 "cscript.c"
        break;
      case 113:
#line 325 "cscript.in"
{ 
  PUSHFRAME(); 
}
#line 1547 "cscript.c"
        break;
      case 114:
#line 330 "cscript.in"
{ 
  p->PushOffset("for_label1"); 
}
#line 1554 "cscript.c"
        break;
      case 115:
#line 335 "cscript.in"
{ 
  INS(JZ);
  p->PushOffset("for_patch_1");
  PUSHQ(0);
  INS(JMP);
  p->PushOffset("for_patch_2");
  PUSHQ(0);
  p->PushOffset("for_label_2"); 
}
#line 1567 "cscript.c"
        break;
      case 116:
#line 346 "cscript.in"
{ 
  INS(JMP);
  PUSHQ(p->PopOffset("for_label1"));
  PATCH("for_patch_2");
}
#line 1576 "cscript.c"
        break;
      case 117:
#line 353 "cscript.in"
{
  INS(JMP); 
  PUSHQ(p->PopOffset("for_label_2"));
  PATCH("for_patch_1");
  POPFRAME();
}
#line 1586 "cscript.c"
        break;
      case 122:
#line 392 "cscript.in"
{
  INS(JZ);
  p->PushOffset("if_patch_1");
  PUSHQ(0);
}
#line 1595 "cscript.c"
        break;
      case 123:
#line 399 "cscript.in"
{
  PATCH("if_patch_1");
}
#line 1602 "cscript.c"
        break;
      case 125:
#line 407 "cscript.in"
{
  INS(JMP);
  p->PushOffset("if_patch_2");
  PUSHQ(0);
  PATCH("if_patch_1");
}
#line 1612 "cscript.c"
        break;
      case 126:
#line 415 "cscript.in"
{
  PATCH("if_patch_2");
}
#line 1619 "cscript.c"
        break;
      case 128:
#line 428 "cscript.in"
{
  p->PushOffset("switch_cond_1");
}
#line 1626 "cscript.c"
        break;
      case 129:
#line 433 "cscript.in"
{
  INS(JZ);
  p->PushOffset("switch_cond_2");
  PUSHQ(0);
}
#line 1635 "cscript.c"
        break;
      case 130:
#line 440 "cscript.in"
{
  INS(JMP);
  PUSHQ(p->PopOffset("switch_cond_1"));
  PATCH("switch_cond_2");
}
#line 1644 "cscript.c"
        break;
      case 135:
#line 458 "cscript.in"
{ }
#line 1649 "cscript.c"
        break;
      case 136:
#line 460 "cscript.in"
{ POPFRAME(); }
#line 1654 "cscript.c"
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
#line 14 "cscript.in"

  throw std::runtime_error("Parse failed");
#line 1702 "cscript.c"
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
#line 17 "cscript.in"

  throw std::runtime_error("Syntax error");
#line 1720 "cscript.c"
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

