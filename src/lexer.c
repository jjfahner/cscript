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
			  0,   0,   0,   0,   0,   0,   0,   0, 
			  0,   2,   0,   0,   0,   4,   0,   0, 
			  0,   0,   0,   0,   0,   0,   0,   0, 
			  0,   0,   0,   0,   0,   0,   0,   0, 
			  2,   0,   0,   0,   0,   0,   0,   0, 
			  0,   0,   0,   0,   0,   0,   0,   0, 
			120, 120,  56,  56,  56,  56,  56,  56, 
			 56,  56,   0,   0,   0,   0,   0,   0, 
			  0,  40,  40, 168, 168,  40,  40,   8, 
			  8, 136,   8,   8, 136, 136,   8, 136, 
			  8,   8,   8,   8,   8,   8, 136,   8, 
			136,   8,   8,   0,   0,   0,   0,   8, 
			  0,  40,  40,  40,  40,  40,  40,   8, 
			  8,   8,   8,   8,   8,   8,   8,   8, 
			  8,   8,   8,   8,   8,   8,   8,   8, 
			  8,   8,   8,   0,   0,   0,   0,   0, 
			  0,   0,   0,   0,   0,   0,   0,   0, 
			  0,   0,   0,   0,   0,   0,   0,   0, 
			  0,   0,   0,   0,   0,   0,   0,   0, 
			  0,   0,   0,   0,   0,   0,   0,   0, 
			  0,   0,   0,   0,   0,   0,   0,   0, 
			  0,   0,   0,   0,   0,   0,   0,   0, 
			  0,   0,   0,   0,   0,   0,   0,   0, 
			  0,   0,   0,   0,   0,   0,   0,   0, 
			  0,   0,   0,   0,   0,   0,   0,   0, 
			  0,   0,   0,   0,   0,   0,   0,   0, 
			  0,   0,   0,   0,   0,   0,   0,   0, 
			  0,   0,   0,   0,   0,   0,   0,   0, 
			  0,   0,   0,   0,   0,   0,   0,   0, 
			  0,   0,   0,   0,   0,   0,   0,   0, 
			  0,   0,   0,   0,   0,   0,   0,   0, 
			  0,   0,   0,   0,   0,   0,   0,   0, 
		};

		{
			Char yych;
			unsigned int yyaccept = 0;

			yych = (Char)*end;
			if(yych & ~0xFF) {
			} else if(yybm[0+yych] & 2) {
				goto yy3;
			}
			switch(yych) {
			case 0x000A:	goto yy8;
			case 0x000D:	goto yy6;
			case '!':	goto yy38;
			case '"':	goto yy32;
			case '%':	goto yy50;
			case '&':	goto yy54;
			case '(':	goto yy70;
			case ')':	goto yy72;
			case '*':	goto yy48;
			case '+':	goto yy44;
			case ',':	goto yy62;
			case '-':	goto yy46;
			case '.':	goto yy34;
			case '/':	goto yy10;
			case '0':	goto yy28;
			case '1':
			case '2':
			case '3':
			case '4':
			case '5':
			case '6':
			case '7':
			case '8':
			case '9':	goto yy30;
			case ':':	goto yy58;
			case ';':	goto yy60;
			case '<':	goto yy40;
			case '=':	goto yy36;
			case '>':	goto yy42;
			case '?':	goto yy56;
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
			case 'o':
			case 'p':
			case 'q':
			case 'u':
			case 'x':
			case 'y':
			case 'z':	goto yy26;
			case '[':	goto yy66;
			case ']':	goto yy68;
			case '^':	goto yy64;
			case '_':	goto yy12;
			case 'b':	goto yy14;
			case 'c':	goto yy15;
			case 'd':	goto yy16;
			case 'e':	goto yy17;
			case 'f':	goto yy18;
			case 'i':	goto yy19;
			case 'n':	goto yy20;
			case 'r':	goto yy21;
			case 's':	goto yy22;
			case 't':	goto yy23;
			case 'v':	goto yy24;
			case 'w':	goto yy25;
			case '{':	goto yy74;
			case '|':	goto yy52;
			case '}':	goto yy76;
			default:	goto yy2;
			}
yy2:
			end = qptr;
			if(yyaccept <= 0) {
				goto yy29;
			} else {
				goto yy35;
			}
yy3:
			++end;
			yych = (Char)*end;
			if(yych & ~0xFF) {
			} else if(yybm[0+yych] & 2) {
				goto yy3;
			}
			{ return TOK_WHITESPACE; }
yy6:
			++end;
			yych = (Char)*end;
			if(yych & ~0xFF) {
				goto yy2;
			} else if(yybm[0+yych] & 4) {
				goto yy6;
			}
			if(yych != 0x000A) goto yy2;
yy8:
			++end;
			{ return TOK_NEWLINE; }
yy10:
			++end;
			if((yych = (Char)*end) <= '.') {
				if(yych == '*') goto yy274;
			} else {
				if(yych <= '/') goto yy276;
				if(yych == '=') goto yy272;
			}
			{ return TOK_DIVOP; }
yy12:
			++end;
			if((yych = (Char)*end) == '_') goto yy264;
			goto yy27;
yy13:
			{ return TOK_IDENTIFIER; }
yy14:
			yych = (Char)*++end;
			if(yych == 'o') goto yy255;
			if(yych == 'r') goto yy256;
			goto yy27;
yy15:
			yych = (Char)*++end;
			if(yych == 'a') goto yy239;
			if(yych == 'o') goto yy240;
			goto yy27;
yy16:
			yych = (Char)*++end;
			if(yych == 'e') goto yy232;
			goto yy27;
yy17:
			yych = (Char)*++end;
			if(yych == 'l') goto yy222;
			if(yych == 'x') goto yy223;
			goto yy27;
yy18:
			yych = (Char)*++end;
			if(yych <= 'i') {
				if(yych == 'a') goto yy199;
				if(yych <= 'h') goto yy27;
				goto yy200;
			} else {
				if(yych <= 'o') {
					if(yych <= 'n') goto yy27;
					goto yy201;
				} else {
					if(yych == 'u') goto yy202;
					goto yy27;
				}
			}
yy19:
			yych = (Char)*++end;
			if(yych == 'f') goto yy187;
			if(yych == 'n') goto yy189;
			goto yy27;
yy20:
			yych = (Char)*++end;
			if(yych <= 'd') {
				if(yych == 'a') goto yy171;
				goto yy27;
			} else {
				if(yych <= 'e') goto yy172;
				if(yych == 'u') goto yy173;
				goto yy27;
			}
yy21:
			yych = (Char)*++end;
			if(yych == 'e') goto yy165;
			goto yy27;
yy22:
			yych = (Char)*++end;
			if(yych == 't') goto yy153;
			if(yych == 'w') goto yy154;
			goto yy27;
yy23:
			yych = (Char)*++end;
			if(yych == 'h') goto yy139;
			if(yych == 'r') goto yy140;
			goto yy27;
yy24:
			yych = (Char)*++end;
			if(yych == 'a') goto yy136;
			goto yy27;
yy25:
			yych = (Char)*++end;
			if(yych == 'h') goto yy131;
			goto yy27;
yy26:
			++end;
			yych = (Char)*end;
yy27:
			if(yych & ~0xFF) {
				goto yy13;
			} else if(yybm[0+yych] & 8) {
				goto yy26;
			}
			goto yy13;
yy28:
			yyaccept = 0;
			yych = (Char)*(qptr = ++end);
			if(yych <= 'q') {
				if(yych == 'b') goto yy120;
				goto yy31;
			} else {
				if(yych <= 'r') goto yy119;
				if(yych == 'x') goto yy121;
				goto yy31;
			}
yy29:
			{ return TOK_LIT_INTEGER; }
yy30:
			yyaccept = 0;
			qptr = ++end;
			yych = (Char)*end;
yy31:
			if(yych & ~0xFF) {
				goto yy29;
			} else if(yybm[0+yych] & 16) {
				goto yy30;
			}
			if(yych == '.') goto yy115;
			goto yy29;
yy32:
			++end;
			{ return TOK_LIT_STRING; }
yy34:
			yyaccept = 1;
			yych = (Char)*(qptr = ++end);
			if(yych == '.') goto yy112;
yy35:
			{ return TOK_DOT; }
yy36:
			++end;
			if((yych = (Char)*end) == '=') goto yy108;
			{ return TOK_ASSIGN; }
yy38:
			++end;
			if((yych = (Char)*end) == '=') goto yy104;
			{ return TOK_NOT; }
yy40:
			++end;
			if((yych = (Char)*end) == '=') goto yy102;
			if(yych == '?') goto yy100;
			{ return TOK_ST; }
yy42:
			++end;
			if((yych = (Char)*end) == '=') goto yy98;
			{ return TOK_GT; }
yy44:
			++end;
			if((yych = (Char)*end) == '+') goto yy96;
			if(yych == '=') goto yy94;
			{ return TOK_ADDOP; }
yy46:
			++end;
			if((yych = (Char)*end) == '-') goto yy92;
			if(yych == '=') goto yy90;
			{ return TOK_SUBOP; }
yy48:
			++end;
			if((yych = (Char)*end) == '=') goto yy88;
			{ return TOK_MULOP; }
yy50:
			++end;
			if((yych = (Char)*end) == '=') goto yy86;
			{ return TOK_MODOP; }
yy52:
			++end;
			if((yych = (Char)*end) == '|') goto yy84;
			{ return TOK_BITOR; }
yy54:
			++end;
			if((yych = (Char)*end) == '&') goto yy82;
			{ return TOK_BITAND; }
yy56:
			++end;
			if((yych = (Char)*end) == '>') goto yy80;
			{ return TOK_QUESTION; }
yy58:
			++end;
			if((yych = (Char)*end) == ':') goto yy78;
			{ return TOK_COLON; }
yy60:
			++end;
			{ return TOK_SEMICOLON; }
yy62:
			++end;
			{ return TOK_COMMA; }
yy64:
			++end;
			{ return TOK_BITXOR; }
yy66:
			++end;
			{ return TOK_LBRACKET; }
yy68:
			++end;
			{ return TOK_RBRACKET; }
yy70:
			++end;
			{ return TOK_LPAREN; }
yy72:
			++end;
			{ return TOK_RPAREN; }
yy74:
			++end;
			{ return TOK_LBRACE; }
yy76:
			++end;
			{ return TOK_RBRACE; }
yy78:
			++end;
			{ return TOK_NS_SEP; }
yy80:
			++end;
			{ return TOK_XMLPRC; }
yy82:
			++end;
			{ return TOK_LOGAND; }
yy84:
			++end;
			{ return TOK_LOGOR; }
yy86:
			++end;
			{ return TOK_ASSMOD; }
yy88:
			++end;
			{ return TOK_ASSMUL; }
yy90:
			++end;
			{ return TOK_ASSSUB; }
yy92:
			++end;
			{ return TOK_SUBSUB; }
yy94:
			++end;
			{ return TOK_ASSADD; }
yy96:
			++end;
			{ return TOK_ADDADD; }
yy98:
			++end;
			{ return TOK_GE; }
yy100:
			++end;
			{ return TOK_XMLPRO; }
yy102:
			++end;
			{ return TOK_SE; }
yy104:
			++end;
			if((yych = (Char)*end) == '=') goto yy106;
			{ return TOK_NEQUALS; }
yy106:
			++end;
			{ return TOK_SNE; }
yy108:
			++end;
			if((yych = (Char)*end) == '=') goto yy110;
			{ return TOK_EQUALS; }
yy110:
			++end;
			{ return TOK_SEQ; }
yy112:
			yych = (Char)*++end;
			if(yych != '.') goto yy2;
			++end;
			{ return TOK_VARIADIC; }
yy115:
			yych = (Char)*++end;
			if(yych <= '/') goto yy2;
			if(yych >= ':') goto yy2;
yy116:
			++end;
			yych = (Char)*end;
			if(yych <= '/') goto yy118;
			if(yych <= '9') goto yy116;
yy118:
			{ return TOK_LIT_REAL; }
yy119:
			yych = (Char)*++end;
			if(yych & ~0xFF) {
				goto yy2;
			} else if(yybm[0+yych] & 128) {
				goto yy128;
			}
			goto yy2;
yy120:
			yych = (Char)*++end;
			if(yych & ~0xFF) {
				goto yy2;
			} else if(yybm[0+yych] & 64) {
				goto yy125;
			}
			goto yy2;
yy121:
			yych = (Char)*++end;
			if(yych & ~0xFF) {
			} else if(yybm[0+yych] & 32) {
				goto yy122;
			}
			goto yy2;
yy122:
			++end;
			yych = (Char)*end;
			if(yych & ~0xFF) {
			} else if(yybm[0+yych] & 32) {
				goto yy122;
			}
			{ return TOK_LIT_HEX; }
yy125:
			++end;
			yych = (Char)*end;
			if(yych & ~0xFF) {
			} else if(yybm[0+yych] & 64) {
				goto yy125;
			}
			{ return TOK_LIT_BIN; }
yy128:
			++end;
			yych = (Char)*end;
			if(yych & ~0xFF) {
			} else if(yybm[0+yych] & 128) {
				goto yy128;
			}
			{ return TOK_LIT_ROM; }
yy131:
			yych = (Char)*++end;
			if(yych != 'i') goto yy27;
			yych = (Char)*++end;
			if(yych != 'l') goto yy27;
			yych = (Char)*++end;
			if(yych != 'e') goto yy27;
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 8) {
				goto yy26;
			}
			{ return TOK_WHILE; }
yy136:
			yych = (Char)*++end;
			if(yych != 'r') goto yy27;
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 8) {
				goto yy26;
			}
			{ return TOK_VAR; }
yy139:
			yych = (Char)*++end;
			if(yych == 'i') goto yy146;
			if(yych == 'r') goto yy147;
			goto yy27;
yy140:
			yych = (Char)*++end;
			if(yych == 'u') goto yy141;
			if(yych == 'y') goto yy142;
			goto yy27;
yy141:
			yych = (Char)*++end;
			if(yych == 'e') goto yy144;
			goto yy27;
yy142:
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 8) {
				goto yy26;
			}
			{ return TOK_TRY; }
yy144:
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 8) {
				goto yy26;
			}
			{ return TOK_TRUE; }
yy146:
			yych = (Char)*++end;
			if(yych == 's') goto yy151;
			goto yy27;
yy147:
			yych = (Char)*++end;
			if(yych != 'o') goto yy27;
			yych = (Char)*++end;
			if(yych != 'w') goto yy27;
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 8) {
				goto yy26;
			}
			{ return TOK_THROW; }
yy151:
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 8) {
				goto yy26;
			}
			{ return TOK_THIS; }
yy153:
			yych = (Char)*++end;
			if(yych == 'r') goto yy160;
			goto yy27;
yy154:
			yych = (Char)*++end;
			if(yych != 'i') goto yy27;
			yych = (Char)*++end;
			if(yych != 't') goto yy27;
			yych = (Char)*++end;
			if(yych != 'c') goto yy27;
			yych = (Char)*++end;
			if(yych != 'h') goto yy27;
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 8) {
				goto yy26;
			}
			{ return TOK_SWITCH; }
yy160:
			yych = (Char)*++end;
			if(yych != 'i') goto yy27;
			yych = (Char)*++end;
			if(yych != 'n') goto yy27;
			yych = (Char)*++end;
			if(yych != 'g') goto yy27;
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 8) {
				goto yy26;
			}
			{ return TOK_STRING; }
yy165:
			yych = (Char)*++end;
			if(yych != 't') goto yy27;
			yych = (Char)*++end;
			if(yych != 'u') goto yy27;
			yych = (Char)*++end;
			if(yych != 'r') goto yy27;
			yych = (Char)*++end;
			if(yych != 'n') goto yy27;
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 8) {
				goto yy26;
			}
			{ return TOK_RETURN; }
yy171:
			yych = (Char)*++end;
			if(yych == 'm') goto yy179;
			goto yy27;
yy172:
			yych = (Char)*++end;
			if(yych == 'w') goto yy177;
			goto yy27;
yy173:
			yych = (Char)*++end;
			if(yych != 'l') goto yy27;
			yych = (Char)*++end;
			if(yych != 'l') goto yy27;
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 8) {
				goto yy26;
			}
			{ return TOK_NULL; }
yy177:
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 8) {
				goto yy26;
			}
			{ return TOK_NEW; }
yy179:
			yych = (Char)*++end;
			if(yych != 'e') goto yy27;
			yych = (Char)*++end;
			if(yych != 's') goto yy27;
			yych = (Char)*++end;
			if(yych != 'p') goto yy27;
			yych = (Char)*++end;
			if(yych != 'a') goto yy27;
			yych = (Char)*++end;
			if(yych != 'c') goto yy27;
			yych = (Char)*++end;
			if(yych != 'e') goto yy27;
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 8) {
				goto yy26;
			}
			{ return TOK_NAMESPACE; }
yy187:
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 8) {
				goto yy26;
			}
			{ return TOK_IF; }
yy189:
			++end;
			if((yych = (Char)*end) <= '_') {
				if(yych <= '@') {
					if(yych <= '/') goto yy190;
					if(yych <= '9') goto yy26;
				} else {
					if(yych <= 'Z') goto yy26;
					if(yych >= '_') goto yy26;
				}
			} else {
				if(yych <= 'c') {
					if(yych <= '`') goto yy190;
					if(yych <= 'b') goto yy26;
					goto yy191;
				} else {
					if(yych == 't') goto yy192;
					if(yych <= 'z') goto yy26;
				}
			}
yy190:
			{ return TOK_IN; }
yy191:
			yych = (Char)*++end;
			if(yych == 'l') goto yy194;
			goto yy27;
yy192:
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 8) {
				goto yy26;
			}
			{ return TOK_INT; }
yy194:
			yych = (Char)*++end;
			if(yych != 'u') goto yy27;
			yych = (Char)*++end;
			if(yych != 'd') goto yy27;
			yych = (Char)*++end;
			if(yych != 'e') goto yy27;
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 8) {
				goto yy26;
			}
			{ return TOK_INCLUDE; }
yy199:
			yych = (Char)*++end;
			if(yych == 'l') goto yy218;
			goto yy27;
yy200:
			yych = (Char)*++end;
			if(yych == 'n') goto yy212;
			goto yy27;
yy201:
			yych = (Char)*++end;
			if(yych == 'r') goto yy210;
			goto yy27;
yy202:
			yych = (Char)*++end;
			if(yych != 'n') goto yy27;
			yych = (Char)*++end;
			if(yych != 'c') goto yy27;
			yych = (Char)*++end;
			if(yych != 't') goto yy27;
			yych = (Char)*++end;
			if(yych != 'i') goto yy27;
			yych = (Char)*++end;
			if(yych != 'o') goto yy27;
			yych = (Char)*++end;
			if(yych != 'n') goto yy27;
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 8) {
				goto yy26;
			}
			{ return TOK_FUNCTION; }
yy210:
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 8) {
				goto yy26;
			}
			{ return TOK_FOR; }
yy212:
			yych = (Char)*++end;
			if(yych != 'a') goto yy27;
			yych = (Char)*++end;
			if(yych != 'l') goto yy27;
			yych = (Char)*++end;
			if(yych != 'l') goto yy27;
			yych = (Char)*++end;
			if(yych != 'y') goto yy27;
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 8) {
				goto yy26;
			}
			{ return TOK_FINALLY; }
yy218:
			yych = (Char)*++end;
			if(yych != 's') goto yy27;
			yych = (Char)*++end;
			if(yych != 'e') goto yy27;
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 8) {
				goto yy26;
			}
			{ return TOK_FALSE; }
yy222:
			yych = (Char)*++end;
			if(yych == 's') goto yy229;
			goto yy27;
yy223:
			yych = (Char)*++end;
			if(yych != 't') goto yy27;
			yych = (Char)*++end;
			if(yych != 'e') goto yy27;
			yych = (Char)*++end;
			if(yych != 'r') goto yy27;
			yych = (Char)*++end;
			if(yych != 'n') goto yy27;
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 8) {
				goto yy26;
			}
			{ return TOK_EXTERN; }
yy229:
			yych = (Char)*++end;
			if(yych != 'e') goto yy27;
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 8) {
				goto yy26;
			}
			{ return TOK_ELSE; }
yy232:
			yych = (Char)*++end;
			if(yych != 'f') goto yy27;
			yych = (Char)*++end;
			if(yych != 'a') goto yy27;
			yych = (Char)*++end;
			if(yych != 'u') goto yy27;
			yych = (Char)*++end;
			if(yych != 'l') goto yy27;
			yych = (Char)*++end;
			if(yych != 't') goto yy27;
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 8) {
				goto yy26;
			}
			{ return TOK_DEFAULT; }
yy239:
			yych = (Char)*++end;
			if(yych <= 'r') goto yy27;
			if(yych <= 's') goto yy248;
			if(yych <= 't') goto yy249;
			goto yy27;
yy240:
			yych = (Char)*++end;
			if(yych != 'n') goto yy27;
			yych = (Char)*++end;
			if(yych != 't') goto yy27;
			yych = (Char)*++end;
			if(yych != 'i') goto yy27;
			yych = (Char)*++end;
			if(yych != 'n') goto yy27;
			yych = (Char)*++end;
			if(yych != 'u') goto yy27;
			yych = (Char)*++end;
			if(yych != 'e') goto yy27;
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 8) {
				goto yy26;
			}
			{ return TOK_CONTINUE; }
yy248:
			yych = (Char)*++end;
			if(yych == 'e') goto yy253;
			goto yy27;
yy249:
			yych = (Char)*++end;
			if(yych != 'c') goto yy27;
			yych = (Char)*++end;
			if(yych != 'h') goto yy27;
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 8) {
				goto yy26;
			}
			{ return TOK_CATCH; }
yy253:
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 8) {
				goto yy26;
			}
			{ return TOK_CASE; }
yy255:
			yych = (Char)*++end;
			if(yych == 'o') goto yy261;
			goto yy27;
yy256:
			yych = (Char)*++end;
			if(yych != 'e') goto yy27;
			yych = (Char)*++end;
			if(yych != 'a') goto yy27;
			yych = (Char)*++end;
			if(yych != 'k') goto yy27;
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 8) {
				goto yy26;
			}
			{ return TOK_BREAK; }
yy261:
			yych = (Char)*++end;
			if(yych != 'l') goto yy27;
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 8) {
				goto yy26;
			}
			{ return TOK_BOOL; }
yy264:
			yych = (Char)*++end;
			if(yych != 'n') goto yy27;
			yych = (Char)*++end;
			if(yych != 'a') goto yy27;
			yych = (Char)*++end;
			if(yych != 't') goto yy27;
			yych = (Char)*++end;
			if(yych != 'i') goto yy27;
			yych = (Char)*++end;
			if(yych != 'v') goto yy27;
			yych = (Char)*++end;
			if(yych != 'e') goto yy27;
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 8) {
				goto yy26;
			}
			{ return TOK_NATIVE; }
yy272:
			++end;
			{ return TOK_ASSDIV; }
yy274:
			++end;
			{ return TOK_COMMENT; }
yy276:
			++end;
			{ return TOK_COMMENT; }
		}
	}

}
