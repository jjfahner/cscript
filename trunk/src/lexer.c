/* Generated by re2c 0.12.2 */
//////////////////////////////////////////////////////////////////////////
//
// This file is � 2007 - 2009 JJ Fahner <jan-jaap@jan-jaap.net>
// This file is part of the cscript interpreter.
// CScript can be found at http://svn.jan-jaap.net/
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
// 
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
// 
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//
//////////////////////////////////////////////////////////////////////////

int parseNextToken(Char*& start, Char*& end)
{
  Char* qptr = 0;
  
  if(*start == 0)
  {
    return 0;
  }
  
  	{
		static const unsigned char yybm[] = {
			 16,  16,  16,  16,  16,  16,  16,  16, 
			 16,  17,  16,  16,  16,  18,  16,  16, 
			 16,  16,  16,  16,  16,  16,  16,  16, 
			 16,  16,  16,  16,  16,  16,  16,  16, 
			 17,  16,  16,  16,  16,  16,  16,  16, 
			 16,  16,  16,  16,  16,  16,  16,  16, 
			124, 124,  60,  60,  60,  60,  60,  60, 
			 60,  60,  16,  16,  16,  16,  16,  16, 
			 16,  52,  52, 180, 180,  52,  52,  20, 
			 20, 148,  20,  20, 148, 148,  20, 148, 
			 20,  20,  20,  20,  20,  20, 148,  20, 
			148,  20,  20,  16,  16,  16,  16,  20, 
			  0,  52,  52,  52,  52,  52,  52,  20, 
			 20,  20,  20,  20,  20,  20,  20,  20, 
			 20,  20,  20,  20,  20,  20,  20,  20, 
			 20,  20,  20,  16,  16,  16,  16,  16, 
			 16,  16,  16,  16,  16,  16,  16,  16, 
			 16,  16,  16,  16,  16,  16,  16,  16, 
			 16,  16,  16,  16,  16,  16,  16,  16, 
			 16,  16,  16,  16,  16,  16,  16,  16, 
			 16,  16,  16,  16,  16,  16,  16,  16, 
			 16,  16,  16,  16,  16,  16,  16,  16, 
			 16,  16,  16,  16,  16,  16,  16,  16, 
			 16,  16,  16,  16,  16,  16,  16,  16, 
			 16,  16,  16,  16,  16,  16,  16,  16, 
			 16,  16,  16,  16,  16,  16,  16,  16, 
			 16,  16,  16,  16,  16,  16,  16,  16, 
			 16,  16,  16,  16,  16,  16,  16,  16, 
			 16,  16,  16,  16,  16,  16,  16,  16, 
			 16,  16,  16,  16,  16,  16,  16,  16, 
			 16,  16,  16,  16,  16,  16,  16,  16, 
			 16,  16,  16,  16,  16,  16,  16,  16, 
		};

		{
			Char yych;
			unsigned int yyaccept = 0;

			yych = (Char)*end;
			if(yych & ~0xFF) {
			} else if(yybm[0+yych] & 1) {
				goto yy3;
			}
			switch(yych) {
			case 0x000A:	goto yy8;
			case 0x000D:	goto yy6;
			case '!':	goto yy41;
			case '"':	goto yy34;
			case '%':	goto yy53;
			case '&':	goto yy57;
			case '(':	goto yy72;
			case ')':	goto yy74;
			case '*':	goto yy51;
			case '+':	goto yy47;
			case ',':	goto yy64;
			case '-':	goto yy49;
			case '.':	goto yy37;
			case '/':	goto yy10;
			case '0':	goto yy30;
			case '1':
			case '2':
			case '3':
			case '4':
			case '5':
			case '6':
			case '7':
			case '8':
			case '9':	goto yy32;
			case ':':	goto yy60;
			case ';':	goto yy62;
			case '<':	goto yy43;
			case '=':	goto yy39;
			case '>':	goto yy45;
			case '?':	goto yy59;
			case 'A':
			case 'B':
			case 'C':
			case 'D':
			case 'E':
			case 'F':
			case 'G':
			case 'H':
			case 'I':
			case 'J':
			case 'K':
			case 'L':
			case 'M':
			case 'N':
			case 'O':
			case 'P':
			case 'Q':
			case 'R':
			case 'S':
			case 'T':
			case 'U':
			case 'V':
			case 'W':
			case 'X':
			case 'Y':
			case 'Z':
			case 'a':
			case 'g':
			case 'h':
			case 'j':
			case 'k':
			case 'l':
			case 'm':
			case 'p':
			case 'q':
			case 'x':
			case 'y':
			case 'z':	goto yy28;
			case '[':	goto yy68;
			case ']':	goto yy70;
			case '^':	goto yy66;
			case '_':	goto yy12;
			case '`':	goto yy36;
			case 'b':	goto yy14;
			case 'c':	goto yy15;
			case 'd':	goto yy16;
			case 'e':	goto yy17;
			case 'f':	goto yy18;
			case 'i':	goto yy19;
			case 'n':	goto yy20;
			case 'o':	goto yy21;
			case 'r':	goto yy22;
			case 's':	goto yy23;
			case 't':	goto yy24;
			case 'u':	goto yy25;
			case 'v':	goto yy26;
			case 'w':	goto yy27;
			case '{':	goto yy76;
			case '|':	goto yy55;
			case '}':	goto yy78;
			default:	goto yy2;
			}
yy2:
			end = qptr;
			if(yyaccept <= 0) {
				goto yy31;
			} else {
				goto yy38;
			}
yy3:
			++end;
			yych = (Char)*end;
			if(yych & ~0xFF) {
			} else if(yybm[0+yych] & 1) {
				goto yy3;
			}
			{ return TOK_WHITESPACE; }
yy6:
			++end;
			yych = (Char)*end;
			if(yych & ~0xFF) {
				goto yy2;
			} else if(yybm[0+yych] & 2) {
				goto yy6;
			}
			if(yych != 0x000A) goto yy2;
yy8:
			++end;
			{ return TOK_NEWLINE; }
yy10:
			++end;
			if((yych = (Char)*end) <= '.') {
				if(yych == '*') goto yy293;
			} else {
				if(yych <= '/') goto yy295;
				if(yych == '=') goto yy291;
			}
			{ return TOK_DIVOP; }
yy12:
			++end;
			if((yych = (Char)*end) == '_') goto yy283;
			goto yy29;
yy13:
			{ return TOK_IDENTIFIER; }
yy14:
			yych = (Char)*++end;
			if(yych == 'o') goto yy274;
			if(yych == 'r') goto yy275;
			goto yy29;
yy15:
			yych = (Char)*++end;
			if(yych == 'a') goto yy258;
			if(yych == 'o') goto yy259;
			goto yy29;
yy16:
			yych = (Char)*++end;
			if(yych == 'e') goto yy251;
			goto yy29;
yy17:
			yych = (Char)*++end;
			if(yych == 'l') goto yy241;
			if(yych == 'x') goto yy242;
			goto yy29;
yy18:
			yych = (Char)*++end;
			if(yych <= 'i') {
				if(yych == 'a') goto yy218;
				if(yych <= 'h') goto yy29;
				goto yy219;
			} else {
				if(yych <= 'o') {
					if(yych <= 'n') goto yy29;
					goto yy220;
				} else {
					if(yych == 'u') goto yy221;
					goto yy29;
				}
			}
yy19:
			yych = (Char)*++end;
			if(yych == 'f') goto yy206;
			if(yych == 'n') goto yy208;
			goto yy29;
yy20:
			yych = (Char)*++end;
			if(yych <= 'd') {
				if(yych == 'a') goto yy190;
				goto yy29;
			} else {
				if(yych <= 'e') goto yy191;
				if(yych == 'u') goto yy192;
				goto yy29;
			}
yy21:
			yych = (Char)*++end;
			if(yych == 'p') goto yy182;
			goto yy29;
yy22:
			yych = (Char)*++end;
			if(yych == 'e') goto yy176;
			goto yy29;
yy23:
			yych = (Char)*++end;
			if(yych == 't') goto yy164;
			if(yych == 'w') goto yy165;
			goto yy29;
yy24:
			yych = (Char)*++end;
			if(yych == 'h') goto yy150;
			if(yych == 'r') goto yy151;
			goto yy29;
yy25:
			yych = (Char)*++end;
			if(yych == 'n') goto yy145;
			goto yy29;
yy26:
			yych = (Char)*++end;
			if(yych == 'a') goto yy142;
			goto yy29;
yy27:
			yych = (Char)*++end;
			if(yych == 'h') goto yy137;
			goto yy29;
yy28:
			++end;
			yych = (Char)*end;
yy29:
			if(yych & ~0xFF) {
				goto yy13;
			} else if(yybm[0+yych] & 4) {
				goto yy28;
			}
			goto yy13;
yy30:
			yyaccept = 0;
			yych = (Char)*(qptr = ++end);
			if(yych <= 'q') {
				if(yych == 'b') goto yy126;
				goto yy33;
			} else {
				if(yych <= 'r') goto yy125;
				if(yych == 'x') goto yy127;
				goto yy33;
			}
yy31:
			{ return TOK_LIT_INTEGER; }
yy32:
			yyaccept = 0;
			qptr = ++end;
			yych = (Char)*end;
yy33:
			if(yych & ~0xFF) {
				goto yy31;
			} else if(yybm[0+yych] & 8) {
				goto yy32;
			}
			if(yych == '.') goto yy121;
			goto yy31;
yy34:
			++end;
			{ return TOK_LIT_STRING; }
yy36:
			yych = (Char)*++end;
			if(yych & ~0xFF) {
				goto yy117;
			} else if(yybm[0+yych] & 16) {
				goto yy117;
			}
			goto yy2;
yy37:
			yyaccept = 1;
			yych = (Char)*(qptr = ++end);
			if(yych == '.') goto yy114;
yy38:
			{ return TOK_DOT; }
yy39:
			++end;
			if((yych = (Char)*end) == '=') goto yy110;
			{ return TOK_ASSIGN; }
yy41:
			++end;
			if((yych = (Char)*end) == '=') goto yy106;
			{ return TOK_NOT; }
yy43:
			++end;
			if((yych = (Char)*end) == '=') goto yy104;
			if(yych == '?') goto yy102;
			{ return TOK_LT; }
yy45:
			++end;
			if((yych = (Char)*end) == '=') goto yy100;
			{ return TOK_GT; }
yy47:
			++end;
			if((yych = (Char)*end) == '+') goto yy98;
			if(yych == '=') goto yy96;
			{ return TOK_ADDOP; }
yy49:
			++end;
			if((yych = (Char)*end) == '-') goto yy94;
			if(yych == '=') goto yy92;
			{ return TOK_SUBOP; }
yy51:
			++end;
			if((yych = (Char)*end) == '=') goto yy90;
			{ return TOK_MULOP; }
yy53:
			++end;
			if((yych = (Char)*end) == '=') goto yy88;
			{ return TOK_MODOP; }
yy55:
			++end;
			if((yych = (Char)*end) == '|') goto yy86;
			{ return TOK_BITOR; }
yy57:
			++end;
			if((yych = (Char)*end) == '&') goto yy84;
			{ return TOK_BITAND; }
yy59:
			yych = (Char)*++end;
			if(yych == '>') goto yy82;
			goto yy2;
yy60:
			++end;
			if((yych = (Char)*end) == ':') goto yy80;
			{ return TOK_COLON; }
yy62:
			++end;
			{ return TOK_SEMICOLON; }
yy64:
			++end;
			{ return TOK_COMMA; }
yy66:
			++end;
			{ return TOK_BITXOR; }
yy68:
			++end;
			{ return TOK_LBRACKET; }
yy70:
			++end;
			{ return TOK_RBRACKET; }
yy72:
			++end;
			{ return TOK_LPAREN; }
yy74:
			++end;
			{ return TOK_RPAREN; }
yy76:
			++end;
			{ return TOK_LBRACE; }
yy78:
			++end;
			{ return TOK_RBRACE; }
yy80:
			++end;
			{ return TOK_NS_SEP; }
yy82:
			++end;
			{ return TOK_XMLPRC; }
yy84:
			++end;
			{ return TOK_LOGAND; }
yy86:
			++end;
			{ return TOK_LOGOR; }
yy88:
			++end;
			{ return TOK_ASSMOD; }
yy90:
			++end;
			{ return TOK_ASSMUL; }
yy92:
			++end;
			{ return TOK_ASSSUB; }
yy94:
			++end;
			{ return TOK_SUBSUB; }
yy96:
			++end;
			{ return TOK_ASSADD; }
yy98:
			++end;
			{ return TOK_ADDADD; }
yy100:
			++end;
			{ return TOK_GE; }
yy102:
			++end;
			{ return TOK_XMLPRO; }
yy104:
			++end;
			{ return TOK_LE; }
yy106:
			++end;
			if((yych = (Char)*end) == '=') goto yy108;
			{ return TOK_NEQUALS; }
yy108:
			++end;
			{ return TOK_SNE; }
yy110:
			++end;
			if((yych = (Char)*end) == '=') goto yy112;
			{ return TOK_EQUALS; }
yy112:
			++end;
			{ return TOK_SEQ; }
yy114:
			yych = (Char)*++end;
			if(yych != '.') goto yy2;
			++end;
			{ return TOK_VARIADIC; }
yy117:
			++end;
			yych = (Char)*end;
			if(yych & ~0xFF) {
			} else if(yybm[0+yych] & 16) {
				goto yy117;
			}
			++end;
			{ return TOK_SHELLCMD; }
yy121:
			yych = (Char)*++end;
			if(yych <= '/') goto yy2;
			if(yych >= ':') goto yy2;
yy122:
			++end;
			yych = (Char)*end;
			if(yych <= '/') goto yy124;
			if(yych <= '9') goto yy122;
yy124:
			{ return TOK_LIT_REAL; }
yy125:
			yych = (Char)*++end;
			if(yych & ~0xFF) {
				goto yy2;
			} else if(yybm[0+yych] & 128) {
				goto yy134;
			}
			goto yy2;
yy126:
			yych = (Char)*++end;
			if(yych & ~0xFF) {
				goto yy2;
			} else if(yybm[0+yych] & 64) {
				goto yy131;
			}
			goto yy2;
yy127:
			yych = (Char)*++end;
			if(yych & ~0xFF) {
			} else if(yybm[0+yych] & 32) {
				goto yy128;
			}
			goto yy2;
yy128:
			++end;
			yych = (Char)*end;
			if(yych & ~0xFF) {
			} else if(yybm[0+yych] & 32) {
				goto yy128;
			}
			{ return TOK_LIT_HEX; }
yy131:
			++end;
			yych = (Char)*end;
			if(yych & ~0xFF) {
			} else if(yybm[0+yych] & 64) {
				goto yy131;
			}
			{ return TOK_LIT_BIN; }
yy134:
			++end;
			yych = (Char)*end;
			if(yych & ~0xFF) {
			} else if(yybm[0+yych] & 128) {
				goto yy134;
			}
			{ return TOK_LIT_ROM; }
yy137:
			yych = (Char)*++end;
			if(yych != 'i') goto yy29;
			yych = (Char)*++end;
			if(yych != 'l') goto yy29;
			yych = (Char)*++end;
			if(yych != 'e') goto yy29;
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 4) {
				goto yy28;
			}
			{ return TOK_WHILE; }
yy142:
			yych = (Char)*++end;
			if(yych != 'r') goto yy29;
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 4) {
				goto yy28;
			}
			{ return TOK_VAR; }
yy145:
			yych = (Char)*++end;
			if(yych != 's') goto yy29;
			yych = (Char)*++end;
			if(yych != 'e') goto yy29;
			yych = (Char)*++end;
			if(yych != 't') goto yy29;
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 4) {
				goto yy28;
			}
			{ return TOK_UNSET; }
yy150:
			yych = (Char)*++end;
			if(yych == 'i') goto yy157;
			if(yych == 'r') goto yy158;
			goto yy29;
yy151:
			yych = (Char)*++end;
			if(yych == 'u') goto yy152;
			if(yych == 'y') goto yy153;
			goto yy29;
yy152:
			yych = (Char)*++end;
			if(yych == 'e') goto yy155;
			goto yy29;
yy153:
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 4) {
				goto yy28;
			}
			{ return TOK_TRY; }
yy155:
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 4) {
				goto yy28;
			}
			{ return TOK_TRUE; }
yy157:
			yych = (Char)*++end;
			if(yych == 's') goto yy162;
			goto yy29;
yy158:
			yych = (Char)*++end;
			if(yych != 'o') goto yy29;
			yych = (Char)*++end;
			if(yych != 'w') goto yy29;
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 4) {
				goto yy28;
			}
			{ return TOK_THROW; }
yy162:
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 4) {
				goto yy28;
			}
			{ return TOK_THIS; }
yy164:
			yych = (Char)*++end;
			if(yych == 'r') goto yy171;
			goto yy29;
yy165:
			yych = (Char)*++end;
			if(yych != 'i') goto yy29;
			yych = (Char)*++end;
			if(yych != 't') goto yy29;
			yych = (Char)*++end;
			if(yych != 'c') goto yy29;
			yych = (Char)*++end;
			if(yych != 'h') goto yy29;
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 4) {
				goto yy28;
			}
			{ return TOK_SWITCH; }
yy171:
			yych = (Char)*++end;
			if(yych != 'i') goto yy29;
			yych = (Char)*++end;
			if(yych != 'n') goto yy29;
			yych = (Char)*++end;
			if(yych != 'g') goto yy29;
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 4) {
				goto yy28;
			}
			{ return TOK_STRING; }
yy176:
			yych = (Char)*++end;
			if(yych != 't') goto yy29;
			yych = (Char)*++end;
			if(yych != 'u') goto yy29;
			yych = (Char)*++end;
			if(yych != 'r') goto yy29;
			yych = (Char)*++end;
			if(yych != 'n') goto yy29;
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 4) {
				goto yy28;
			}
			{ return TOK_RETURN; }
yy182:
			yych = (Char)*++end;
			if(yych != 'e') goto yy29;
			yych = (Char)*++end;
			if(yych != 'r') goto yy29;
			yych = (Char)*++end;
			if(yych != 'a') goto yy29;
			yych = (Char)*++end;
			if(yych != 't') goto yy29;
			yych = (Char)*++end;
			if(yych != 'o') goto yy29;
			yych = (Char)*++end;
			if(yych != 'r') goto yy29;
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 4) {
				goto yy28;
			}
			{ return TOK_OPERATOR; }
yy190:
			yych = (Char)*++end;
			if(yych == 'm') goto yy198;
			goto yy29;
yy191:
			yych = (Char)*++end;
			if(yych == 'w') goto yy196;
			goto yy29;
yy192:
			yych = (Char)*++end;
			if(yych != 'l') goto yy29;
			yych = (Char)*++end;
			if(yych != 'l') goto yy29;
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 4) {
				goto yy28;
			}
			{ return TOK_NULL; }
yy196:
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 4) {
				goto yy28;
			}
			{ return TOK_NEW; }
yy198:
			yych = (Char)*++end;
			if(yych != 'e') goto yy29;
			yych = (Char)*++end;
			if(yych != 's') goto yy29;
			yych = (Char)*++end;
			if(yych != 'p') goto yy29;
			yych = (Char)*++end;
			if(yych != 'a') goto yy29;
			yych = (Char)*++end;
			if(yych != 'c') goto yy29;
			yych = (Char)*++end;
			if(yych != 'e') goto yy29;
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 4) {
				goto yy28;
			}
			{ return TOK_NAMESPACE; }
yy206:
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 4) {
				goto yy28;
			}
			{ return TOK_IF; }
yy208:
			++end;
			if((yych = (Char)*end) <= '_') {
				if(yych <= '@') {
					if(yych <= '/') goto yy209;
					if(yych <= '9') goto yy28;
				} else {
					if(yych <= 'Z') goto yy28;
					if(yych >= '_') goto yy28;
				}
			} else {
				if(yych <= 'c') {
					if(yych <= '`') goto yy209;
					if(yych <= 'b') goto yy28;
					goto yy210;
				} else {
					if(yych == 't') goto yy211;
					if(yych <= 'z') goto yy28;
				}
			}
yy209:
			{ return TOK_IN; }
yy210:
			yych = (Char)*++end;
			if(yych == 'l') goto yy213;
			goto yy29;
yy211:
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 4) {
				goto yy28;
			}
			{ return TOK_INT; }
yy213:
			yych = (Char)*++end;
			if(yych != 'u') goto yy29;
			yych = (Char)*++end;
			if(yych != 'd') goto yy29;
			yych = (Char)*++end;
			if(yych != 'e') goto yy29;
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 4) {
				goto yy28;
			}
			{ return TOK_INCLUDE; }
yy218:
			yych = (Char)*++end;
			if(yych == 'l') goto yy237;
			goto yy29;
yy219:
			yych = (Char)*++end;
			if(yych == 'n') goto yy231;
			goto yy29;
yy220:
			yych = (Char)*++end;
			if(yych == 'r') goto yy229;
			goto yy29;
yy221:
			yych = (Char)*++end;
			if(yych != 'n') goto yy29;
			yych = (Char)*++end;
			if(yych != 'c') goto yy29;
			yych = (Char)*++end;
			if(yych != 't') goto yy29;
			yych = (Char)*++end;
			if(yych != 'i') goto yy29;
			yych = (Char)*++end;
			if(yych != 'o') goto yy29;
			yych = (Char)*++end;
			if(yych != 'n') goto yy29;
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 4) {
				goto yy28;
			}
			{ return TOK_FUNCTION; }
yy229:
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 4) {
				goto yy28;
			}
			{ return TOK_FOR; }
yy231:
			yych = (Char)*++end;
			if(yych != 'a') goto yy29;
			yych = (Char)*++end;
			if(yych != 'l') goto yy29;
			yych = (Char)*++end;
			if(yych != 'l') goto yy29;
			yych = (Char)*++end;
			if(yych != 'y') goto yy29;
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 4) {
				goto yy28;
			}
			{ return TOK_FINALLY; }
yy237:
			yych = (Char)*++end;
			if(yych != 's') goto yy29;
			yych = (Char)*++end;
			if(yych != 'e') goto yy29;
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 4) {
				goto yy28;
			}
			{ return TOK_FALSE; }
yy241:
			yych = (Char)*++end;
			if(yych == 's') goto yy248;
			goto yy29;
yy242:
			yych = (Char)*++end;
			if(yych != 't') goto yy29;
			yych = (Char)*++end;
			if(yych != 'e') goto yy29;
			yych = (Char)*++end;
			if(yych != 'r') goto yy29;
			yych = (Char)*++end;
			if(yych != 'n') goto yy29;
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 4) {
				goto yy28;
			}
			{ return TOK_EXTERN; }
yy248:
			yych = (Char)*++end;
			if(yych != 'e') goto yy29;
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 4) {
				goto yy28;
			}
			{ return TOK_ELSE; }
yy251:
			yych = (Char)*++end;
			if(yych != 'f') goto yy29;
			yych = (Char)*++end;
			if(yych != 'a') goto yy29;
			yych = (Char)*++end;
			if(yych != 'u') goto yy29;
			yych = (Char)*++end;
			if(yych != 'l') goto yy29;
			yych = (Char)*++end;
			if(yych != 't') goto yy29;
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 4) {
				goto yy28;
			}
			{ return TOK_DEFAULT; }
yy258:
			yych = (Char)*++end;
			if(yych <= 'r') goto yy29;
			if(yych <= 's') goto yy267;
			if(yych <= 't') goto yy268;
			goto yy29;
yy259:
			yych = (Char)*++end;
			if(yych != 'n') goto yy29;
			yych = (Char)*++end;
			if(yych != 't') goto yy29;
			yych = (Char)*++end;
			if(yych != 'i') goto yy29;
			yych = (Char)*++end;
			if(yych != 'n') goto yy29;
			yych = (Char)*++end;
			if(yych != 'u') goto yy29;
			yych = (Char)*++end;
			if(yych != 'e') goto yy29;
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 4) {
				goto yy28;
			}
			{ return TOK_CONTINUE; }
yy267:
			yych = (Char)*++end;
			if(yych == 'e') goto yy272;
			goto yy29;
yy268:
			yych = (Char)*++end;
			if(yych != 'c') goto yy29;
			yych = (Char)*++end;
			if(yych != 'h') goto yy29;
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 4) {
				goto yy28;
			}
			{ return TOK_CATCH; }
yy272:
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 4) {
				goto yy28;
			}
			{ return TOK_CASE; }
yy274:
			yych = (Char)*++end;
			if(yych == 'o') goto yy280;
			goto yy29;
yy275:
			yych = (Char)*++end;
			if(yych != 'e') goto yy29;
			yych = (Char)*++end;
			if(yych != 'a') goto yy29;
			yych = (Char)*++end;
			if(yych != 'k') goto yy29;
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 4) {
				goto yy28;
			}
			{ return TOK_BREAK; }
yy280:
			yych = (Char)*++end;
			if(yych != 'l') goto yy29;
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 4) {
				goto yy28;
			}
			{ return TOK_BOOL; }
yy283:
			yych = (Char)*++end;
			if(yych != 'n') goto yy29;
			yych = (Char)*++end;
			if(yych != 'a') goto yy29;
			yych = (Char)*++end;
			if(yych != 't') goto yy29;
			yych = (Char)*++end;
			if(yych != 'i') goto yy29;
			yych = (Char)*++end;
			if(yych != 'v') goto yy29;
			yych = (Char)*++end;
			if(yych != 'e') goto yy29;
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 4) {
				goto yy28;
			}
			{ return TOK_NATIVE; }
yy291:
			++end;
			{ return TOK_ASSDIV; }
yy293:
			++end;
			{ return TOK_COMMENT; }
yy295:
			++end;
			{ return TOK_COMMENT; }
		}
	}

}
