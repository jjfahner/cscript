/* Driver template for the LEMON parser generator.
** The author disclaims copyright to this source code.
*/
/* First off, code is include which follows the "include" declaration
** in the input file. */
#include <stdio.h>
#line 44 "cscript.in"


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
 /*     0 */   353,  108,    7,  199,  190,  186,  185,  182,  180,  179,
 /*    10 */   178,  174,  172,  106,  170,  198,  165,   67,   91,  181,
 /*    20 */     3,  137,   79,  112,  117,  110,  107,  133,   13,  201,
 /*    30 */   162,  170,   99,  165,   67,   91,  153,  149,  137,   79,
 /*    40 */   112,  117,  110,  107,  133,   13,  120,  121,  122,   99,
 /*    50 */   100,  152,  138,   98,    2,   87,   92,   48,   47,   51,
 /*    60 */   171,  118,   19,   88,  136,  147,  155,  158,  161,  169,
 /*    70 */    86,   59,  140,  190,  186,  185,  182,  180,  179,  178,
 /*    80 */   174,  172,  106,  170,  123,  165,   67,   91,  211,  176,
 /*    90 */   137,   79,  112,  117,  110,  107,  133,   13,   15,  159,
 /*   100 */   170,   99,  165,   67,   91,  129,  149,  137,   79,  112,
 /*   110 */   117,  110,  107,  133,   13,   56,  134,  109,   99,  135,
 /*   120 */    64,  212,  141,    2,    1,   92,   40,   44,   38,   37,
 /*   130 */    36,   82,   88,  126,  121,  122,   38,   37,   36,  119,
 /*   140 */    59,   61,   62,    7,  199,  190,  186,  185,  182,  180,
 /*   150 */   179,  178,  174,  172,  106,  170,  128,  165,   67,   91,
 /*   160 */   102,   24,  137,   79,  112,  117,  110,  107,  133,   13,
 /*   170 */   101,   50,  175,   99,  130,  127,   57,   23,  149,  163,
 /*   180 */   170,  202,  165,   67,   91,  200,    8,  137,   79,  112,
 /*   190 */   117,  110,  107,  133,   13,    2,   65,   92,   99,  210,
 /*   200 */    96,   52,   66,   22,   88,   10,    4,   16,   20,  213,
 /*   210 */   144,   58,   59,  168,  190,  186,  185,  182,  180,  179,
 /*   220 */   178,  174,  172,  106,  170,   89,  165,   67,   91,  209,
 /*   230 */    21,  137,   79,  112,  117,  110,  107,  133,   13,  124,
 /*   240 */   119,   93,   99,  191,  157,    9,  214,  149,  132,  170,
 /*   250 */    14,  165,   67,   91,   95,  193,  137,   79,  112,  117,
 /*   260 */   110,  107,  133,   13,    2,  208,   92,   99,    5,   11,
 /*   270 */   166,  103,   53,   88,   17,  288,  148,  196,    6,  192,
 /*   280 */   354,   59,  184,  190,  186,  185,  182,  180,  179,  178,
 /*   290 */   174,  172,  106,  170,  354,  165,   67,   91,  354,  354,
 /*   300 */   137,   79,  112,  117,  110,  107,  133,   13,  354,  354,
 /*   310 */   354,   99,  354,  354,  354,  354,  149,  162,  170,  354,
 /*   320 */   165,   67,   91,  354,  354,  137,   79,  112,  117,  110,
 /*   330 */   107,  133,   13,    2,  354,   92,   99,  354,  354,  131,
 /*   340 */   354,  354,   88,  354,  354,  354,  354,  354,  354,  183,
 /*   350 */    59,  189,  190,  186,  185,  182,  180,  179,  178,  174,
 /*   360 */   172,  106,  170,  354,  165,   67,   91,  354,  354,  137,
 /*   370 */    79,  112,  117,  110,  107,  133,   13,  354,  354,  354,
 /*   380 */    99,  354,  354,  354,  354,  149,   84,  170,  354,  165,
 /*   390 */    67,   91,  354,  354,  137,   79,  112,  117,  110,  107,
 /*   400 */   133,   13,    2,  354,   92,   99,  354,  354,  354,  354,
 /*   410 */   354,   88,  354,  354,  354,  354,  354,  354,  354,   59,
 /*   420 */   354,  187,  354,   63,  190,  186,  185,  182,  180,  179,
 /*   430 */   178,  174,  172,  106,  170,  354,  165,   67,   91,  354,
 /*   440 */   354,  137,   79,  112,  117,  110,  107,  133,   13,  354,
 /*   450 */   354,  354,   99,  354,  354,  354,  354,  149,   83,  170,
 /*   460 */   354,  165,   67,   91,  354,  354,  137,   79,  112,  117,
 /*   470 */   110,  107,  133,   13,    2,  354,   92,   99,  354,  354,
 /*   480 */   354,  354,  354,   88,  354,  354,  354,  354,  354,  354,
 /*   490 */   354,   59,  194,  190,  186,  185,  182,  180,  179,  178,
 /*   500 */   174,  172,  106,  170,  354,  165,   67,   91,  354,  354,
 /*   510 */   137,   79,  112,  117,  110,  107,  133,   13,  354,  354,
 /*   520 */   354,   99,  354,  354,  354,  354,  149,  105,  170,  354,
 /*   530 */   165,   67,   91,  354,  354,  137,   79,  112,  117,  110,
 /*   540 */   107,  133,   13,    2,  354,   92,   99,  354,  354,  354,
 /*   550 */   354,  354,   88,  354,  354,  354,  354,  354,  354,  354,
 /*   560 */    59,   54,  190,  186,  185,  182,  180,  179,  178,  174,
 /*   570 */   172,  106,  170,  354,  165,   67,   91,  354,  354,  137,
 /*   580 */    79,  112,  117,  110,  107,  133,   13,  354,  354,  354,
 /*   590 */    99,   48,   47,  354,  171,  149,   19,  354,  136,  147,
 /*   600 */   155,  158,  161,  169,   86,  354,   49,  146,   55,  354,
 /*   610 */   354,  354,    2,  354,   92,  354,  354,  354,  354,  354,
 /*   620 */   354,   88,  354,  354,  354,  354,  354,  354,  354,   59,
 /*   630 */   354,   32,   33,   34,   35,   39,   41,   46,   45,   31,
 /*   640 */    43,   42,   40,   44,   38,   37,   36,   29,   25,   26,
 /*   650 */    27,   28,   30,  241,  354,  354,  354,   48,   47,  354,
 /*   660 */   171,  354,   19,  354,  136,  147,  155,  158,  161,  169,
 /*   670 */    86,  354,   49,  146,   55,  154,  139,   18,   94,  167,
 /*   680 */   354,  177,  354,  354,   60,  188,   48,   47,  354,  171,
 /*   690 */   354,   19,  354,  136,  147,  155,  158,  161,  169,   86,
 /*   700 */   354,   49,  146,   55,  154,  354,   18,   94,  167,  354,
 /*   710 */   177,  206,  207,   60,  188,  354,  354,  354,  354,  106,
 /*   720 */   170,  354,  165,   67,   91,  354,  354,  137,   79,  112,
 /*   730 */   117,  110,  107,  133,   13,  354,  204,  170,   99,  165,
 /*   740 */    67,   91,  354,  149,  137,   79,  112,  117,  110,  107,
 /*   750 */   133,   13,  354,  354,  354,   99,  354,  354,  354,  354,
 /*   760 */   354,  354,  354,   12,  354,  197,  170,  205,  165,   67,
 /*   770 */    91,  354,  354,  137,   79,  112,  117,  110,  107,  133,
 /*   780 */    13,   90,  354,  354,   99,  203,  354,  354,  159,  170,
 /*   790 */   354,  165,   67,   91,  354,  354,  137,   79,  112,  117,
 /*   800 */   110,  107,  133,   13,   76,  354,  151,   99,  137,   79,
 /*   810 */   112,  117,  110,  107,  133,   13,  354,   85,  354,   99,
 /*   820 */    33,   34,   35,   39,   41,   46,   45,   31,   43,   42,
 /*   830 */    40,   44,   38,   37,   36,   34,   35,   39,   41,   46,
 /*   840 */    45,   31,   43,   42,   40,   44,   38,   37,   36,  125,
 /*   850 */   170,  354,  165,   67,   91,  354,  354,  137,   79,  112,
 /*   860 */   117,  110,  107,  133,   13,  354,  354,  142,   99,  165,
 /*   870 */    67,   91,  354,  150,  137,   79,  112,  117,  110,  107,
 /*   880 */   133,   13,  354,  354,  354,   99,  354,  354,  354,  104,
 /*   890 */   170,  354,  165,   67,   91,  354,  354,  137,   79,  112,
 /*   900 */   117,  110,  107,  133,   13,  354,   97,  170,   99,  165,
 /*   910 */    67,   91,  354,  354,  137,   79,  112,  117,  110,  107,
 /*   920 */   133,   13,  354,  354,  156,   99,  165,   67,   91,  354,
 /*   930 */   354,  137,   79,  112,  117,  110,  107,  133,   13,  354,
 /*   940 */   354,  164,   99,  165,   67,   91,  354,  354,  137,   79,
 /*   950 */   112,  117,  110,  107,  133,   13,  354,  354,  354,   99,
 /*   960 */   160,  354,  165,   67,   91,  354,  354,  137,   79,  112,
 /*   970 */   117,  110,  107,  133,   13,  354,  354,  195,   99,  165,
 /*   980 */    67,   91,  354,  354,  137,   79,  112,  117,  110,  107,
 /*   990 */   133,   13,  354,  354,  354,   99,   35,   39,   41,   46,
 /*  1000 */    45,   31,   43,   42,   40,   44,   38,   37,   36,  354,
 /*  1010 */   354,  173,  354,  165,   67,   91,  354,  354,  137,   79,
 /*  1020 */   112,  117,  110,  107,  133,   13,  354,  354,  354,   99,
 /*  1030 */   143,  354,  165,   67,   91,  354,  354,  137,   79,  112,
 /*  1040 */   117,  110,  107,  133,   13,  354,  354,  354,   99,   39,
 /*  1050 */    41,   46,   45,   31,   43,   42,   40,   44,   38,   37,
 /*  1060 */    36,  354,  354,   48,   47,  354,  171,  354,   19,  354,
 /*  1070 */   136,  147,  155,  158,  161,  169,   86,  354,  354,  145,
 /*  1080 */    41,   46,   45,   31,   43,   42,   40,   44,   38,   37,
 /*  1090 */    36,   78,  354,  354,  354,  137,   79,  112,  117,  110,
 /*  1100 */   107,  133,   13,   73,  354,  354,   99,  137,   79,  112,
 /*  1110 */   117,  110,  107,  133,   13,   80,  354,  354,   99,  137,
 /*  1120 */    79,  112,  117,  110,  107,  133,   13,   81,  354,  354,
 /*  1130 */    99,  137,   79,  112,  117,  110,  107,  133,   13,   77,
 /*  1140 */   354,  354,   99,  137,   79,  112,  117,  110,  107,  133,
 /*  1150 */    13,  354,   72,  354,   99,  354,  137,   79,  112,  117,
 /*  1160 */   110,  107,  133,   13,   74,  354,  354,   99,  137,   79,
 /*  1170 */   112,  117,  110,  107,  133,   13,  111,  354,  354,   99,
 /*  1180 */   137,   79,  112,  117,  110,  107,  133,   13,   75,  354,
 /*  1190 */   354,   99,  137,   79,  112,  117,  110,  107,  133,   13,
 /*  1200 */   113,  354,  354,   99,  137,   79,  112,  117,  110,  107,
 /*  1210 */   133,   13,   68,  354,  354,   99,  137,   79,  112,  117,
 /*  1220 */   110,  107,  133,   13,  354,   69,  354,   99,  354,  137,
 /*  1230 */    79,  112,  117,  110,  107,  133,   13,   70,  354,  354,
 /*  1240 */    99,  137,   79,  112,  117,  110,  107,  133,   13,  115,
 /*  1250 */   354,  354,   99,  137,   79,  112,  117,  110,  107,  133,
 /*  1260 */    13,   71,  354,  354,   99,  137,   79,  112,  117,  110,
 /*  1270 */   107,  133,   13,  354,  354,  354,   99,   45,   31,   43,
 /*  1280 */    42,   40,   44,   38,   37,   36,  116,   79,  112,  117,
 /*  1290 */   110,  107,  133,   13,  354,  354,  354,   99,  114,   79,
 /*  1300 */   112,  117,  110,  107,  133,   13,  354,  354,  354,   99,
};
static const YYCODETYPE yy_lookahead[] = {
 /*     0 */    55,   56,   57,   58,   59,   60,   61,   62,   63,   64,
 /*    10 */    65,   66,   67,   68,   69,   79,   71,   72,   73,  123,
 /*    20 */   124,   76,   77,   78,   79,   80,   81,   82,   83,   96,
 /*    30 */    68,   69,   87,   71,   72,   73,   44,   92,   76,   77,
 /*    40 */    78,   79,   80,   81,   82,   83,   95,   96,   97,   87,
 /*    50 */    88,  110,   90,   91,  109,  119,  111,   25,   26,   39,
 /*    60 */    28,   41,   30,  118,   32,   33,   34,   35,   36,   37,
 /*    70 */    38,  126,   58,   59,   60,   61,   62,   63,   64,   65,
 /*    80 */    66,   67,   68,   69,   29,   71,   72,   73,   29,  121,
 /*    90 */    76,   77,   78,   79,   80,   81,   82,   83,   39,   68,
 /*   100 */    69,   87,   71,   72,   73,   31,   92,   76,   77,   78,
 /*   110 */    79,   80,   81,   82,   83,   84,  101,   86,   87,  104,
 /*   120 */   105,  107,  108,  109,  106,  111,   12,   13,   14,   15,
 /*   130 */    16,   93,  118,   95,   96,   97,   14,   15,   16,   38,
 /*   140 */   126,   40,   56,   57,   58,   59,   60,   61,   62,   63,
 /*   150 */    64,   65,   66,   67,   68,   69,   94,   71,   72,   73,
 /*   160 */    98,   17,   76,   77,   78,   79,   80,   81,   82,   83,
 /*   170 */   100,   30,   89,   87,   25,   26,   27,   28,   92,   68,
 /*   180 */    69,   70,   71,   72,   73,   38,  122,   76,   77,   78,
 /*   190 */    79,   80,   81,   82,   83,  109,   99,  111,   87,  102,
 /*   200 */   103,   31,   31,   30,  118,   30,  127,   39,   30,  102,
 /*   210 */    41,   39,  126,   58,   59,   60,   61,   62,   63,   64,
 /*   220 */    65,   66,   67,   68,   69,  114,   71,   72,   73,   38,
 /*   230 */    24,   76,   77,   78,   79,   80,   81,   82,   83,   79,
 /*   240 */    38,   34,   87,   85,   41,   30,   38,   92,   68,   69,
 /*   250 */    23,   71,   72,   73,   74,   51,   76,   77,   78,   79,
 /*   260 */    80,   81,   82,   83,  109,   43,  111,   87,   31,   41,
 /*   270 */   115,   38,   30,  118,   48,   30,   41,   31,   31,   31,
 /*   280 */   129,  126,   58,   59,   60,   61,   62,   63,   64,   65,
 /*   290 */    66,   67,   68,   69,  129,   71,   72,   73,  129,  129,
 /*   300 */    76,   77,   78,   79,   80,   81,   82,   83,  129,  129,
 /*   310 */   129,   87,  129,  129,  129,  129,   92,   68,   69,  129,
 /*   320 */    71,   72,   73,  129,  129,   76,   77,   78,   79,   80,
 /*   330 */    81,   82,   83,  109,  129,  111,   87,  129,  129,   90,
 /*   340 */   129,  129,  118,  129,  129,  129,  129,  129,  129,  125,
 /*   350 */   126,   58,   59,   60,   61,   62,   63,   64,   65,   66,
 /*   360 */    67,   68,   69,  129,   71,   72,   73,  129,  129,   76,
 /*   370 */    77,   78,   79,   80,   81,   82,   83,  129,  129,  129,
 /*   380 */    87,  129,  129,  129,  129,   92,   68,   69,  129,   71,
 /*   390 */    72,   73,  129,  129,   76,   77,   78,   79,   80,   81,
 /*   400 */    82,   83,  109,  129,  111,   87,  129,  129,  129,  129,
 /*   410 */   129,  118,  129,  129,  129,  129,  129,  129,  129,  126,
 /*   420 */   129,  128,  129,   58,   59,   60,   61,   62,   63,   64,
 /*   430 */    65,   66,   67,   68,   69,  129,   71,   72,   73,  129,
 /*   440 */   129,   76,   77,   78,   79,   80,   81,   82,   83,  129,
 /*   450 */   129,  129,   87,  129,  129,  129,  129,   92,   68,   69,
 /*   460 */   129,   71,   72,   73,  129,  129,   76,   77,   78,   79,
 /*   470 */    80,   81,   82,   83,  109,  129,  111,   87,  129,  129,
 /*   480 */   129,  129,  129,  118,  129,  129,  129,  129,  129,  129,
 /*   490 */   129,  126,   58,   59,   60,   61,   62,   63,   64,   65,
 /*   500 */    66,   67,   68,   69,  129,   71,   72,   73,  129,  129,
 /*   510 */    76,   77,   78,   79,   80,   81,   82,   83,  129,  129,
 /*   520 */   129,   87,  129,  129,  129,  129,   92,   68,   69,  129,
 /*   530 */    71,   72,   73,  129,  129,   76,   77,   78,   79,   80,
 /*   540 */    81,   82,   83,  109,  129,  111,   87,  129,  129,  129,
 /*   550 */   129,  129,  118,  129,  129,  129,  129,  129,  129,  129,
 /*   560 */   126,   58,   59,   60,   61,   62,   63,   64,   65,   66,
 /*   570 */    67,   68,   69,  129,   71,   72,   73,  129,  129,   76,
 /*   580 */    77,   78,   79,   80,   81,   82,   83,  129,  129,  129,
 /*   590 */    87,   25,   26,  129,   28,   92,   30,  129,   32,   33,
 /*   600 */    34,   35,   36,   37,   38,  129,   40,   41,   42,  129,
 /*   610 */   129,  129,  109,  129,  111,  129,  129,  129,  129,  129,
 /*   620 */   129,  118,  129,  129,  129,  129,  129,  129,  129,  126,
 /*   630 */   129,    1,    2,    3,    4,    5,    6,    7,    8,    9,
 /*   640 */    10,   11,   12,   13,   14,   15,   16,   17,   18,   19,
 /*   650 */    20,   21,   22,   23,  129,  129,  129,   25,   26,  129,
 /*   660 */    28,  129,   30,  129,   32,   33,   34,   35,   36,   37,
 /*   670 */    38,  129,   40,   41,   42,   43,   44,   45,   46,   47,
 /*   680 */   129,   49,  129,  129,   52,   53,   25,   26,  129,   28,
 /*   690 */   129,   30,  129,   32,   33,   34,   35,   36,   37,   38,
 /*   700 */   129,   40,   41,   42,   43,  129,   45,   46,   47,  129,
 /*   710 */    49,   60,   61,   52,   53,  129,  129,  129,  129,   68,
 /*   720 */    69,  129,   71,   72,   73,  129,  129,   76,   77,   78,
 /*   730 */    79,   80,   81,   82,   83,  129,   68,   69,   87,   71,
 /*   740 */    72,   73,  129,   92,   76,   77,   78,   79,   80,   81,
 /*   750 */    82,   83,  129,  129,  129,   87,  129,  129,  129,  129,
 /*   760 */   129,  129,  129,  112,  129,   68,   69,  116,   71,   72,
 /*   770 */    73,  129,  129,   76,   77,   78,   79,   80,   81,   82,
 /*   780 */    83,  113,  129,  129,   87,  117,  129,  129,   68,   69,
 /*   790 */   129,   71,   72,   73,  129,  129,   76,   77,   78,   79,
 /*   800 */    80,   81,   82,   83,   72,  129,   86,   87,   76,   77,
 /*   810 */    78,   79,   80,   81,   82,   83,  129,  120,  129,   87,
 /*   820 */     2,    3,    4,    5,    6,    7,    8,    9,   10,   11,
 /*   830 */    12,   13,   14,   15,   16,    3,    4,    5,    6,    7,
 /*   840 */     8,    9,   10,   11,   12,   13,   14,   15,   16,   68,
 /*   850 */    69,  129,   71,   72,   73,  129,  129,   76,   77,   78,
 /*   860 */    79,   80,   81,   82,   83,  129,  129,   69,   87,   71,
 /*   870 */    72,   73,  129,   75,   76,   77,   78,   79,   80,   81,
 /*   880 */    82,   83,  129,  129,  129,   87,  129,  129,  129,   68,
 /*   890 */    69,  129,   71,   72,   73,  129,  129,   76,   77,   78,
 /*   900 */    79,   80,   81,   82,   83,  129,   68,   69,   87,   71,
 /*   910 */    72,   73,  129,  129,   76,   77,   78,   79,   80,   81,
 /*   920 */    82,   83,  129,  129,   69,   87,   71,   72,   73,  129,
 /*   930 */   129,   76,   77,   78,   79,   80,   81,   82,   83,  129,
 /*   940 */   129,   69,   87,   71,   72,   73,  129,  129,   76,   77,
 /*   950 */    78,   79,   80,   81,   82,   83,  129,  129,  129,   87,
 /*   960 */    69,  129,   71,   72,   73,  129,  129,   76,   77,   78,
 /*   970 */    79,   80,   81,   82,   83,  129,  129,   69,   87,   71,
 /*   980 */    72,   73,  129,  129,   76,   77,   78,   79,   80,   81,
 /*   990 */    82,   83,  129,  129,  129,   87,    4,    5,    6,    7,
 /*  1000 */     8,    9,   10,   11,   12,   13,   14,   15,   16,  129,
 /*  1010 */   129,   69,  129,   71,   72,   73,  129,  129,   76,   77,
 /*  1020 */    78,   79,   80,   81,   82,   83,  129,  129,  129,   87,
 /*  1030 */    69,  129,   71,   72,   73,  129,  129,   76,   77,   78,
 /*  1040 */    79,   80,   81,   82,   83,  129,  129,  129,   87,    5,
 /*  1050 */     6,    7,    8,    9,   10,   11,   12,   13,   14,   15,
 /*  1060 */    16,  129,  129,   25,   26,  129,   28,  129,   30,  129,
 /*  1070 */    32,   33,   34,   35,   36,   37,   38,  129,  129,   41,
 /*  1080 */     6,    7,    8,    9,   10,   11,   12,   13,   14,   15,
 /*  1090 */    16,   72,  129,  129,  129,   76,   77,   78,   79,   80,
 /*  1100 */    81,   82,   83,   72,  129,  129,   87,   76,   77,   78,
 /*  1110 */    79,   80,   81,   82,   83,   72,  129,  129,   87,   76,
 /*  1120 */    77,   78,   79,   80,   81,   82,   83,   72,  129,  129,
 /*  1130 */    87,   76,   77,   78,   79,   80,   81,   82,   83,   72,
 /*  1140 */   129,  129,   87,   76,   77,   78,   79,   80,   81,   82,
 /*  1150 */    83,  129,   72,  129,   87,  129,   76,   77,   78,   79,
 /*  1160 */    80,   81,   82,   83,   72,  129,  129,   87,   76,   77,
 /*  1170 */    78,   79,   80,   81,   82,   83,   72,  129,  129,   87,
 /*  1180 */    76,   77,   78,   79,   80,   81,   82,   83,   72,  129,
 /*  1190 */   129,   87,   76,   77,   78,   79,   80,   81,   82,   83,
 /*  1200 */    72,  129,  129,   87,   76,   77,   78,   79,   80,   81,
 /*  1210 */    82,   83,   72,  129,  129,   87,   76,   77,   78,   79,
 /*  1220 */    80,   81,   82,   83,  129,   72,  129,   87,  129,   76,
 /*  1230 */    77,   78,   79,   80,   81,   82,   83,   72,  129,  129,
 /*  1240 */    87,   76,   77,   78,   79,   80,   81,   82,   83,   72,
 /*  1250 */   129,  129,   87,   76,   77,   78,   79,   80,   81,   82,
 /*  1260 */    83,   72,  129,  129,   87,   76,   77,   78,   79,   80,
 /*  1270 */    81,   82,   83,  129,  129,  129,   87,    8,    9,   10,
 /*  1280 */    11,   12,   13,   14,   15,   16,   76,   77,   78,   79,
 /*  1290 */    80,   81,   82,   83,  129,  129,  129,   87,   76,   77,
 /*  1300 */    78,   79,   80,   81,   82,   83,  129,  129,  129,   87,
};
#define YY_SHIFT_USE_DFLT (-9)
#define YY_SHIFT_MAX 106
static const short yy_shift_ofst[] = {
 /*     0 */   661,  632,  661,  661,  661,  661,  661,  661,  661,  566,
 /*    10 */    32,   32,   32,   32,   32,   32,   32,   32, 1038,   32,
 /*    20 */    32,   32,   32,   32,   32,   32,   32,   32,   32,   32,
 /*    30 */    32,   32,   32,   32,   32,   32,   32,   32,   32,   32,
 /*    40 */    32,   32,   32,   32,   32,   32,   32,   32,   32,  233,
 /*    50 */   191,  233,  222,  101,  204,  208,   59,  202,  191,  178,
 /*    60 */   173,  147,   -8,   -9,   -9,   -9,   -9,  630,  818,  832,
 /*    70 */   992, 1044, 1074, 1269, 1269,  114,  114,  114,  114,  149,
 /*    80 */   122,  122,   20,  246,  248,  247,  245,  226,  242,  237,
 /*    90 */   228,  227,  215,  203,  207,  206,  172,  169,  168,  175,
 /*   100 */   171,  170,  141,  144,   74,   55,  235,
};
#define YY_REDUCE_USE_DFLT (-105)
#define YY_REDUCE_MAX 66
static const short yy_reduce_ofst[] = {
 /*     0 */   -55,   14,   86,  224,  293,  155,  365,  434,  503,  651,
 /*    10 */   -38,  111,  668,   31,  180,  720,  249,  697,  838,  821,
 /*    20 */   318,  798,  390,  459,  781,  961,  855,  872,  942,  891,
 /*    30 */   908, 1116, 1140, 1153, 1165, 1189, 1177, 1128, 1104, 1080,
 /*    40 */  1055, 1031,  732, 1019, 1043, 1067, 1092, 1222, 1210,   38,
 /*    50 */    97,  -49,   15,  -64, -104,   62,  158,  160,  107,   79,
 /*    60 */    64,  -67,  -59,  -32,   18,   70,   83,
};
static const YYACTIONTYPE yy_default[] = {
 /*     0 */   218,  352,  218,  352,  352,  352,  352,  219,  352,  352,
 /*    10 */   293,  230,  352,  352,  352,  352,  352,  352,  352,  352,
 /*    20 */   352,  352,  352,  352,  352,  352,  352,  352,  352,  352,
 /*    30 */   352,  352,  352,  352,  352,  352,  352,  352,  352,  352,
 /*    40 */   352,  352,  352,  352,  352,  352,  352,  352,  352,  352,
 /*    50 */   308,  352,  352,  352,  344,  352,  352,  352,  352,  352,
 /*    60 */   352,  352,  352,  341,  316,  310,  289,  239,  245,  246,
 /*    70 */   247,  248,  249,  250,  251,  253,  255,  252,  254,  261,
 /*    80 */   257,  256,  352,  352,  352,  352,  280,  352,  352,  352,
 /*    90 */   352,  352,  352,  352,  352,  352,  309,  352,  294,  352,
 /*   100 */   352,  352,  352,  301,  352,  352,  352,  270,  215,  285,
 /*   110 */   269,  258,  264,  259,  263,  260,  262,  271,  295,  280,
 /*   120 */   298,  299,  300,  268,  267,  302,  297,  266,  296,  272,
 /*   130 */   265,  292,  242,  273,  303,  311,  274,  244,  291,  314,
 /*   140 */   315,  317,  243,  234,  318,  319,  320,  275,  321,  322,
 /*   150 */   240,  286,  323,  325,  324,  276,  235,  326,  277,  284,
 /*   160 */   233,  278,  290,  231,  236,  232,  327,  328,  332,  279,
 /*   170 */   229,  282,  228,  237,  227,  287,  336,  337,  226,  225,
 /*   180 */   224,  342,  223,  345,  347,  222,  221,  348,  349,  351,
 /*   190 */   220,  281,  350,  346,  217,  238,  343,  340,  338,  216,
 /*   200 */   301,  339,  331,  330,  333,  329,  334,  335,  313,  305,
 /*   210 */   306,  283,  312,  307,  304,
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
  "LBRACKET",      "RBRACKET",      "LPAREN",        "RPAREN",      
  "INTEGER",       "REAL",          "STRING",        "TRUE",        
  "FALSE",         "NULL",          "IDENTIFIER",    "COMMA",       
  "VAR",           "SEMICOLON",     "FUN",           "LBRACE",      
  "RBRACE",        "RETURN",        "INCLUDE",       "FOR",         
  "IN",            "FOREACH",       "LOWER_THAN_ELSE",  "ELSE",        
  "IF",            "WHILE",         "error",         "translation_unit",
  "statement_sequence_opt",  "statement_sequence",  "statement",     "include_statement",
  "expression_statement",  "declaration_statement",  "for_statement",  "compound_statement",
  "if_statement",  "while_statement",  "foreach_statement",  "return_statement",
  "expression",    "assignment_expression",  "expression_opt",  "conditional_expression",
  "binary_expression",  "cond_part",     "cond_expr_true",  "cond_expr_false",
  "unary_expression",  "postfix_expression",  "primary_expression",  "id_expression",
  "function_call",  "literal",       "list_definition",  "list_start",  
  "list_content",  "list_end",      "list_entry",    "function_call_start",
  "argument_list",  "function_call_end",  "argument",      "arguments",   
  "declaration",   "declaration_sequence",  "function_declaration",  "var_declaration",
  "simple_declaration",  "init_declaration",  "function_name",  "parameter_list",
  "parameters_done",  "function_body",  "parameter",     "parameters",  
  "function_block",  "function_block_start",  "function_statements",  "function_block_end",
  "function_statement",  "compound_start",  "compound_end",  "for_start",   
  "for_init",      "for_cond",      "for_expr",      "for_end",     
  "for_init_statement",  "for_condition",  "foreach_init",  "for_lvalue",  
  "for_expression",  "foreach_end",   "if_cond",       "if_else",     
  "if_else_1",     "if_else_2",     "while_start",   "while_condition",
  "while_content",
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
 /*  18 */ "assignment_expression ::= binary_expression ASSIGN assignment_expression",
 /*  19 */ "assignment_expression ::= binary_expression ASSADD assignment_expression",
 /*  20 */ "assignment_expression ::= binary_expression ASSSUB assignment_expression",
 /*  21 */ "assignment_expression ::= binary_expression ASSMUL assignment_expression",
 /*  22 */ "assignment_expression ::= binary_expression ASSDIV assignment_expression",
 /*  23 */ "assignment_expression ::= binary_expression ASSMOD assignment_expression",
 /*  24 */ "conditional_expression ::= binary_expression",
 /*  25 */ "conditional_expression ::= cond_part QUESTION cond_expr_true COLON cond_expr_false",
 /*  26 */ "cond_part ::= binary_expression",
 /*  27 */ "cond_expr_true ::= expression",
 /*  28 */ "cond_expr_false ::= assignment_expression",
 /*  29 */ "binary_expression ::= unary_expression",
 /*  30 */ "binary_expression ::= binary_expression LOGOR binary_expression",
 /*  31 */ "binary_expression ::= binary_expression LOGAND binary_expression",
 /*  32 */ "binary_expression ::= binary_expression BITOR binary_expression",
 /*  33 */ "binary_expression ::= binary_expression BITXOR binary_expression",
 /*  34 */ "binary_expression ::= binary_expression BITAND binary_expression",
 /*  35 */ "binary_expression ::= binary_expression EQUALS binary_expression",
 /*  36 */ "binary_expression ::= binary_expression NEQUALS binary_expression",
 /*  37 */ "binary_expression ::= binary_expression ST binary_expression",
 /*  38 */ "binary_expression ::= binary_expression SE binary_expression",
 /*  39 */ "binary_expression ::= binary_expression GT binary_expression",
 /*  40 */ "binary_expression ::= binary_expression GE binary_expression",
 /*  41 */ "binary_expression ::= binary_expression ADDOP binary_expression",
 /*  42 */ "binary_expression ::= binary_expression SUBOP binary_expression",
 /*  43 */ "binary_expression ::= binary_expression MULOP binary_expression",
 /*  44 */ "binary_expression ::= binary_expression DIVOP binary_expression",
 /*  45 */ "binary_expression ::= binary_expression MODOP binary_expression",
 /*  46 */ "unary_expression ::= postfix_expression",
 /*  47 */ "unary_expression ::= ADDADD unary_expression",
 /*  48 */ "unary_expression ::= SUBSUB unary_expression",
 /*  49 */ "postfix_expression ::= primary_expression",
 /*  50 */ "postfix_expression ::= postfix_expression ADDADD",
 /*  51 */ "postfix_expression ::= postfix_expression SUBSUB",
 /*  52 */ "postfix_expression ::= postfix_expression DOT id_expression",
 /*  53 */ "postfix_expression ::= postfix_expression LBRACKET expression RBRACKET",
 /*  54 */ "postfix_expression ::= function_call",
 /*  55 */ "primary_expression ::= literal",
 /*  56 */ "primary_expression ::= id_expression",
 /*  57 */ "primary_expression ::= LPAREN expression RPAREN",
 /*  58 */ "primary_expression ::= list_definition",
 /*  59 */ "literal ::= INTEGER",
 /*  60 */ "literal ::= REAL",
 /*  61 */ "literal ::= STRING",
 /*  62 */ "literal ::= TRUE",
 /*  63 */ "literal ::= FALSE",
 /*  64 */ "literal ::= NULL",
 /*  65 */ "id_expression ::= IDENTIFIER",
 /*  66 */ "list_definition ::= list_start list_content list_end",
 /*  67 */ "list_start ::= LBRACKET",
 /*  68 */ "list_end ::= RBRACKET",
 /*  69 */ "list_entry ::= expression",
 /*  70 */ "list_content ::= list_entry",
 /*  71 */ "list_content ::= list_content COMMA list_entry",
 /*  72 */ "function_call ::= function_call_start LPAREN argument_list RPAREN function_call_end",
 /*  73 */ "function_call_start ::= IDENTIFIER",
 /*  74 */ "function_call_end ::=",
 /*  75 */ "argument ::= expression",
 /*  76 */ "arguments ::= argument",
 /*  77 */ "arguments ::= arguments COMMA argument",
 /*  78 */ "argument_list ::=",
 /*  79 */ "argument_list ::= arguments",
 /*  80 */ "declaration ::= VAR declaration_sequence SEMICOLON",
 /*  81 */ "declaration ::= FUN function_declaration",
 /*  82 */ "declaration_sequence ::= var_declaration",
 /*  83 */ "declaration_sequence ::= declaration_sequence COMMA var_declaration",
 /*  84 */ "var_declaration ::= simple_declaration",
 /*  85 */ "var_declaration ::= init_declaration",
 /*  86 */ "simple_declaration ::= IDENTIFIER",
 /*  87 */ "init_declaration ::= IDENTIFIER ASSIGN expression",
 /*  88 */ "function_declaration ::= function_name LPAREN parameter_list parameters_done RPAREN function_body",
 /*  89 */ "function_name ::= IDENTIFIER",
 /*  90 */ "parameter ::= IDENTIFIER",
 /*  91 */ "parameters ::= parameter",
 /*  92 */ "parameters ::= parameters COMMA parameter",
 /*  93 */ "parameter_list ::=",
 /*  94 */ "parameter_list ::= parameters",
 /*  95 */ "parameters_done ::=",
 /*  96 */ "function_body ::= function_block",
 /*  97 */ "function_block ::= function_block_start function_statements function_block_end",
 /*  98 */ "function_block_start ::= LBRACE",
 /*  99 */ "function_block_end ::= RBRACE",
 /* 100 */ "function_statement ::= statement",
 /* 101 */ "function_statements ::=",
 /* 102 */ "function_statements ::= function_statements function_statement",
 /* 103 */ "return_statement ::= RETURN expression SEMICOLON",
 /* 104 */ "return_statement ::= RETURN SEMICOLON",
 /* 105 */ "expression_statement ::= SEMICOLON",
 /* 106 */ "expression_statement ::= expression SEMICOLON",
 /* 107 */ "declaration_statement ::= declaration",
 /* 108 */ "compound_statement ::= compound_start statement_sequence_opt compound_end",
 /* 109 */ "compound_start ::= LBRACE",
 /* 110 */ "compound_end ::= RBRACE",
 /* 111 */ "include_statement ::= INCLUDE STRING SEMICOLON",
 /* 112 */ "for_statement ::= for_start LPAREN for_init for_cond SEMICOLON for_expr RPAREN for_end",
 /* 113 */ "for_start ::= FOR",
 /* 114 */ "for_init ::= for_init_statement",
 /* 115 */ "for_cond ::= for_condition",
 /* 116 */ "for_expr ::= expression_opt",
 /* 117 */ "for_end ::= statement",
 /* 118 */ "for_condition ::= expression",
 /* 119 */ "for_init_statement ::= expression_statement",
 /* 120 */ "for_init_statement ::= declaration_statement",
 /* 121 */ "foreach_statement ::= foreach_init LPAREN for_lvalue IN for_expression RPAREN statement foreach_end",
 /* 122 */ "foreach_init ::= FOREACH",
 /* 123 */ "for_lvalue ::= id_expression",
 /* 124 */ "for_lvalue ::= VAR simple_declaration",
 /* 125 */ "for_expression ::= expression",
 /* 126 */ "foreach_end ::=",
 /* 127 */ "if_statement ::= IF if_cond statement if_else",
 /* 128 */ "if_cond ::= LPAREN expression RPAREN",
 /* 129 */ "if_else ::=",
 /* 130 */ "if_else ::= if_else_1 if_else_2",
 /* 131 */ "if_else_1 ::= ELSE",
 /* 132 */ "if_else_2 ::= statement",
 /* 133 */ "while_statement ::= while_start while_condition while_content",
 /* 134 */ "while_start ::= WHILE",
 /* 135 */ "while_condition ::= LPAREN expression RPAREN",
 /* 136 */ "while_content ::= statement",
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
  { 58, 1 },
  { 68, 1 },
  { 70, 0 },
  { 70, 1 },
  { 69, 1 },
  { 69, 3 },
  { 69, 3 },
  { 69, 3 },
  { 69, 3 },
  { 69, 3 },
  { 69, 3 },
  { 71, 1 },
  { 71, 5 },
  { 73, 1 },
  { 74, 1 },
  { 75, 1 },
  { 72, 1 },
  { 72, 3 },
  { 72, 3 },
  { 72, 3 },
  { 72, 3 },
  { 72, 3 },
  { 72, 3 },
  { 72, 3 },
  { 72, 3 },
  { 72, 3 },
  { 72, 3 },
  { 72, 3 },
  { 72, 3 },
  { 72, 3 },
  { 72, 3 },
  { 72, 3 },
  { 72, 3 },
  { 76, 1 },
  { 76, 2 },
  { 76, 2 },
  { 77, 1 },
  { 77, 2 },
  { 77, 2 },
  { 77, 3 },
  { 77, 4 },
  { 77, 1 },
  { 78, 1 },
  { 78, 1 },
  { 78, 3 },
  { 78, 1 },
  { 81, 1 },
  { 81, 1 },
  { 81, 1 },
  { 81, 1 },
  { 81, 1 },
  { 81, 1 },
  { 79, 1 },
  { 82, 3 },
  { 83, 1 },
  { 85, 1 },
  { 86, 1 },
  { 84, 1 },
  { 84, 3 },
  { 80, 5 },
  { 87, 1 },
  { 89, 0 },
  { 90, 1 },
  { 91, 1 },
  { 91, 3 },
  { 88, 0 },
  { 88, 1 },
  { 92, 3 },
  { 92, 2 },
  { 93, 1 },
  { 93, 3 },
  { 95, 1 },
  { 95, 1 },
  { 96, 1 },
  { 97, 3 },
  { 94, 6 },
  { 98, 1 },
  { 102, 1 },
  { 103, 1 },
  { 103, 3 },
  { 99, 0 },
  { 99, 1 },
  { 100, 0 },
  { 101, 1 },
  { 104, 3 },
  { 105, 1 },
  { 107, 1 },
  { 108, 1 },
  { 106, 0 },
  { 106, 2 },
  { 67, 3 },
  { 67, 2 },
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
  { 66, 8 },
  { 118, 1 },
  { 119, 1 },
  { 119, 2 },
  { 120, 1 },
  { 121, 0 },
  { 64, 4 },
  { 122, 3 },
  { 123, 0 },
  { 123, 2 },
  { 124, 1 },
  { 125, 1 },
  { 65, 3 },
  { 126, 1 },
  { 127, 3 },
  { 128, 1 },
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
      case 17:
      case 24:
      case 25:
      case 29:
      case 46:
      case 49:
      case 52:
      case 54:
      case 55:
      case 56:
      case 57:
      case 58:
      case 66:
      case 68:
      case 70:
      case 71:
      case 72:
      case 76:
      case 77:
      case 78:
      case 79:
      case 80:
      case 81:
      case 82:
      case 83:
      case 84:
      case 85:
      case 88:
      case 91:
      case 92:
      case 93:
      case 94:
      case 97:
      case 100:
      case 101:
      case 102:
      case 105:
      case 107:
      case 108:
      case 112:
      case 118:
      case 119:
      case 120:
      case 121:
      case 122:
      case 123:
      case 124:
      case 127:
      case 130:
      case 133:
#line 86 "cscript.in"
{
}
#line 1269 "cscript.c"
        break;
      case 18:
#line 116 "cscript.in"
{ INS(ASSIGN); }
#line 1274 "cscript.c"
        break;
      case 19:
#line 117 "cscript.in"
{ INS(ASSADD); }
#line 1279 "cscript.c"
        break;
      case 20:
#line 118 "cscript.in"
{ INS(ASSSUB); }
#line 1284 "cscript.c"
        break;
      case 21:
#line 119 "cscript.in"
{ INS(ASSMUL); }
#line 1289 "cscript.c"
        break;
      case 22:
#line 120 "cscript.in"
{ INS(ASSDIV); }
#line 1294 "cscript.c"
        break;
      case 23:
#line 121 "cscript.in"
{ INS(ASSMOD); }
#line 1299 "cscript.c"
        break;
      case 26:
#line 128 "cscript.in"
{
  INS(JZ);
  p->PushOffset("cond_exp_1");
  PUSHQ(0);
}
#line 1308 "cscript.c"
        break;
      case 27:
#line 133 "cscript.in"
{
  INS(JMP);
  p->PushOffset("cond_exp_2");
  PUSHQ(0);
  p->SetQuad(p->PopOffset("cond_exp_1"), p->GetPos());
}
#line 1318 "cscript.c"
        break;
      case 28:
#line 139 "cscript.in"
{
  p->SetQuad(p->PopOffset("cond_exp_2"), p->GetPos());
}
#line 1325 "cscript.c"
        break;
      case 30:
#line 145 "cscript.in"
{ INS(LOGOR);   }
#line 1330 "cscript.c"
        break;
      case 31:
#line 146 "cscript.in"
{ INS(LOGAND);  }
#line 1335 "cscript.c"
        break;
      case 32:
#line 147 "cscript.in"
{ INS(PREINC);  }
#line 1340 "cscript.c"
        break;
      case 33:
#line 148 "cscript.in"
{ INS(BITXOR);  }
#line 1345 "cscript.c"
        break;
      case 34:
#line 149 "cscript.in"
{ INS(BITAND);  }
#line 1350 "cscript.c"
        break;
      case 35:
#line 150 "cscript.in"
{ INS(EQUALS);  }
#line 1355 "cscript.c"
        break;
      case 36:
#line 151 "cscript.in"
{ INS(NEQUALS); }
#line 1360 "cscript.c"
        break;
      case 37:
#line 152 "cscript.in"
{ INS(ST);      }
#line 1365 "cscript.c"
        break;
      case 38:
#line 153 "cscript.in"
{ INS(SE);      }
#line 1370 "cscript.c"
        break;
      case 39:
#line 154 "cscript.in"
{ INS(GT);      }
#line 1375 "cscript.c"
        break;
      case 40:
#line 155 "cscript.in"
{ INS(GE);      }
#line 1380 "cscript.c"
        break;
      case 41:
#line 156 "cscript.in"
{ INS(ADDOP);   }
#line 1385 "cscript.c"
        break;
      case 42:
#line 157 "cscript.in"
{ INS(SUBOP);   }
#line 1390 "cscript.c"
        break;
      case 43:
#line 158 "cscript.in"
{ INS(MULOP);   }
#line 1395 "cscript.c"
        break;
      case 44:
#line 159 "cscript.in"
{ INS(DIVOP);   }
#line 1400 "cscript.c"
        break;
      case 45:
#line 160 "cscript.in"
{ INS(MODOP);   }
#line 1405 "cscript.c"
        break;
      case 47:
#line 164 "cscript.in"
{ INS(PREINC); }
#line 1410 "cscript.c"
        break;
      case 48:
#line 165 "cscript.in"
{ INS(PRESUB); }
#line 1415 "cscript.c"
        break;
      case 50:
#line 169 "cscript.in"
{ INS(POSTINC); }
#line 1420 "cscript.c"
        break;
      case 51:
#line 170 "cscript.in"
{ INS(POSTSUB); }
#line 1425 "cscript.c"
        break;
      case 53:
#line 172 "cscript.in"
{ INS(INDEX); }
#line 1430 "cscript.c"
        break;
      case 59:
      case 60:
#line 182 "cscript.in"
{ p->PushRVal(Variant(String(yymsp[0].minor.yy0), Variant::stInt)); }
#line 1436 "cscript.c"
        break;
      case 61:
#line 184 "cscript.in"
{ p->PushRVal(Variant(String(yymsp[0].minor.yy0), Variant::stString)); }
#line 1441 "cscript.c"
        break;
      case 62:
#line 185 "cscript.in"
{ p->PushRVal(Variant(true)); }
#line 1446 "cscript.c"
        break;
      case 63:
#line 186 "cscript.in"
{ p->PushRVal(Variant(false)); }
#line 1451 "cscript.c"
        break;
      case 64:
      case 67:
#line 187 "cscript.in"
{ p->PushRVal(Variant()); }
#line 1457 "cscript.c"
        break;
      case 65:
#line 190 "cscript.in"
{ INS(LVALUE); PUSHQ(p->GetVar(yymsp[0].minor.yy0)); }
#line 1462 "cscript.c"
        break;
      case 69:
#line 196 "cscript.in"
{ INS(ARRAY); }
#line 1467 "cscript.c"
        break;
      case 73:
#line 209 "cscript.in"
{ PUSHCALL(yymsp[0].minor.yy0); }
#line 1472 "cscript.c"
        break;
      case 74:
#line 210 "cscript.in"
{ POPCALL(); }
#line 1477 "cscript.c"
        break;
      case 75:
#line 214 "cscript.in"
{ PUSHARG(); }
#line 1482 "cscript.c"
        break;
      case 86:
#line 241 "cscript.in"
{ 
  INS(VAR);      
  PUSHQ(p->AddVar(yymsp[0].minor.yy0)); 
}
#line 1490 "cscript.c"
        break;
      case 87:
#line 247 "cscript.in"
{ 
  INS(VARINIT);  
  PUSHQ(p->AddVar(yymsp[-2].minor.yy0)); 
}
#line 1498 "cscript.c"
        break;
      case 89:
#line 256 "cscript.in"
{	p->PushFunction(yymsp[0].minor.yy0); }
#line 1503 "cscript.c"
        break;
      case 90:
#line 259 "cscript.in"
{ p->AddParam(yymsp[0].minor.yy0); }
#line 1508 "cscript.c"
        break;
      case 95:
#line 270 "cscript.in"
{ p->GenFunProlog(); }
#line 1513 "cscript.c"
        break;
      case 96:
#line 273 "cscript.in"
{ p->PopFunction(); }
#line 1518 "cscript.c"
        break;
      case 98:
      case 109:
#line 277 "cscript.in"
{ PUSHFRAME(); }
#line 1524 "cscript.c"
        break;
      case 99:
      case 110:
#line 278 "cscript.in"
{ POPFRAME();  }
#line 1530 "cscript.c"
        break;
      case 103:
#line 288 "cscript.in"
{ INS(RET); }
#line 1535 "cscript.c"
        break;
      case 104:
#line 289 "cscript.in"
{ p->PushRVal(Variant()); INS(RET); }
#line 1540 "cscript.c"
        break;
      case 106:
#line 299 "cscript.in"
{ INS(POP); }
#line 1545 "cscript.c"
        break;
      case 111:
#line 310 "cscript.in"
{ p->ParseFile(yymsp[-1].minor.yy0); }
#line 1550 "cscript.c"
        break;
      case 113:
#line 341 "cscript.in"
{ 
  PUSHFRAME(); 
}
#line 1557 "cscript.c"
        break;
      case 114:
#line 346 "cscript.in"
{ 
  p->PushOffset("for_label1"); 
}
#line 1564 "cscript.c"
        break;
      case 115:
#line 351 "cscript.in"
{ 
  INS(JZ);
  p->PushOffset("for_patch_1");
  PUSHQ(0);
  INS(JMP);
  p->PushOffset("for_patch_2");
  PUSHQ(0);
  p->PushOffset("for_label_2"); 
}
#line 1577 "cscript.c"
        break;
      case 116:
#line 362 "cscript.in"
{ 
  INS(JMP);
  PUSHQ(p->PopOffset("for_label1"));
  PATCH("for_patch_2");
}
#line 1586 "cscript.c"
        break;
      case 117:
#line 369 "cscript.in"
{
  INS(JMP); 
  PUSHQ(p->PopOffset("for_label_2"));
  PATCH("for_patch_1");
  POPFRAME();
}
#line 1596 "cscript.c"
        break;
      case 125:
#line 396 "cscript.in"
{ }
#line 1601 "cscript.c"
        break;
      case 126:
#line 398 "cscript.in"
{ POPFRAME(); }
#line 1606 "cscript.c"
        break;
      case 128:
#line 426 "cscript.in"
{
  INS(JZ);
  p->PushOffset("if_patch_1");
  PUSHQ(0);
}
#line 1615 "cscript.c"
        break;
      case 129:
#line 433 "cscript.in"
{
  PATCH("if_patch_1");
}
#line 1622 "cscript.c"
        break;
      case 131:
#line 441 "cscript.in"
{
  INS(JMP);
  p->PushOffset("if_patch_2");
  PUSHQ(0);
  PATCH("if_patch_1");
}
#line 1632 "cscript.c"
        break;
      case 132:
#line 449 "cscript.in"
{
  PATCH("if_patch_2");
}
#line 1639 "cscript.c"
        break;
      case 134:
#line 462 "cscript.in"
{
  p->PushOffset("switch_cond_1");
}
#line 1646 "cscript.c"
        break;
      case 135:
#line 467 "cscript.in"
{
  INS(JZ);
  p->PushOffset("switch_cond_2");
  PUSHQ(0);
}
#line 1655 "cscript.c"
        break;
      case 136:
#line 474 "cscript.in"
{
  INS(JMP);
  PUSHQ(p->PopOffset("switch_cond_1"));
  PATCH("switch_cond_2");
}
#line 1664 "cscript.c"
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
#line 33 "cscript.in"

  p->OnParseFailure();
#line 1712 "cscript.c"
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
#line 36 "cscript.in"

  p->OnSyntaxError();
#line 1730 "cscript.c"
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

