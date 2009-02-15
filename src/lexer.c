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
			case ',':	goto yy60;
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
			case ':':	goto yy62;
			case ';':	goto yy58;
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
				if(yych == '*') goto yy263;
			} else {
				if(yych <= '/') goto yy265;
				if(yych == '=') goto yy261;
			}
			{ return TOK_DIVOP; }
yy12:
			++end;
			if((yych = (Char)*end) == '_') goto yy253;
			goto yy27;
yy13:
			{ return TOK_IDENTIFIER; }
yy14:
			yych = (Char)*++end;
			if(yych == 'o') goto yy244;
			if(yych == 'r') goto yy245;
			goto yy27;
yy15:
			yych = (Char)*++end;
			if(yych == 'a') goto yy228;
			if(yych == 'o') goto yy229;
			goto yy27;
yy16:
			yych = (Char)*++end;
			if(yych == 'e') goto yy221;
			goto yy27;
yy17:
			yych = (Char)*++end;
			if(yych == 'l') goto yy211;
			if(yych == 'x') goto yy212;
			goto yy27;
yy18:
			yych = (Char)*++end;
			if(yych <= 'i') {
				if(yych == 'a') goto yy188;
				if(yych <= 'h') goto yy27;
				goto yy189;
			} else {
				if(yych <= 'o') {
					if(yych <= 'n') goto yy27;
					goto yy190;
				} else {
					if(yych == 'u') goto yy191;
					goto yy27;
				}
			}
yy19:
			yych = (Char)*++end;
			if(yych == 'f') goto yy176;
			if(yych == 'n') goto yy178;
			goto yy27;
yy20:
			yych = (Char)*++end;
			if(yych == 'e') goto yy169;
			if(yych == 'u') goto yy170;
			goto yy27;
yy21:
			yych = (Char)*++end;
			if(yych == 'e') goto yy163;
			goto yy27;
yy22:
			yych = (Char)*++end;
			if(yych == 't') goto yy151;
			if(yych == 'w') goto yy152;
			goto yy27;
yy23:
			yych = (Char)*++end;
			if(yych == 'h') goto yy137;
			if(yych == 'r') goto yy138;
			goto yy27;
yy24:
			yych = (Char)*++end;
			if(yych == 'a') goto yy134;
			goto yy27;
yy25:
			yych = (Char)*++end;
			if(yych == 'h') goto yy129;
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
				if(yych == 'b') goto yy118;
				goto yy31;
			} else {
				if(yych <= 'r') goto yy117;
				if(yych == 'x') goto yy119;
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
			if(yych == '.') goto yy113;
			goto yy29;
yy32:
			++end;
			{ return TOK_LIT_STRING; }
yy34:
			yyaccept = 1;
			yych = (Char)*(qptr = ++end);
			if(yych == '.') goto yy110;
yy35:
			{ return TOK_DOT; }
yy36:
			++end;
			if((yych = (Char)*end) == '=') goto yy106;
			{ return TOK_ASSIGN; }
yy38:
			++end;
			if((yych = (Char)*end) == '=') goto yy102;
			{ return TOK_NOT; }
yy40:
			++end;
			if((yych = (Char)*end) == '=') goto yy100;
			if(yych == '?') goto yy98;
			{ return TOK_ST; }
yy42:
			++end;
			if((yych = (Char)*end) == '=') goto yy96;
			{ return TOK_GT; }
yy44:
			++end;
			if((yych = (Char)*end) == '+') goto yy94;
			if(yych == '=') goto yy92;
			{ return TOK_ADDOP; }
yy46:
			++end;
			if((yych = (Char)*end) == '-') goto yy90;
			if(yych == '=') goto yy88;
			{ return TOK_SUBOP; }
yy48:
			++end;
			if((yych = (Char)*end) == '=') goto yy86;
			{ return TOK_MULOP; }
yy50:
			++end;
			if((yych = (Char)*end) == '=') goto yy84;
			{ return TOK_MODOP; }
yy52:
			++end;
			if((yych = (Char)*end) == '|') goto yy82;
			{ return TOK_BITOR; }
yy54:
			++end;
			if((yych = (Char)*end) == '&') goto yy80;
			{ return TOK_BITAND; }
yy56:
			++end;
			if((yych = (Char)*end) == '>') goto yy78;
			{ return TOK_QUESTION; }
yy58:
			++end;
			{ return TOK_SEMICOLON; }
yy60:
			++end;
			{ return TOK_COMMA; }
yy62:
			++end;
			{ return TOK_COLON; }
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
			{ return TOK_XMLPRC; }
yy80:
			++end;
			{ return TOK_LOGAND; }
yy82:
			++end;
			{ return TOK_LOGOR; }
yy84:
			++end;
			{ return TOK_ASSMOD; }
yy86:
			++end;
			{ return TOK_ASSMUL; }
yy88:
			++end;
			{ return TOK_ASSSUB; }
yy90:
			++end;
			{ return TOK_SUBSUB; }
yy92:
			++end;
			{ return TOK_ASSADD; }
yy94:
			++end;
			{ return TOK_ADDADD; }
yy96:
			++end;
			{ return TOK_GE; }
yy98:
			++end;
			{ return TOK_XMLPRO; }
yy100:
			++end;
			{ return TOK_SE; }
yy102:
			++end;
			if((yych = (Char)*end) == '=') goto yy104;
			{ return TOK_NEQUALS; }
yy104:
			++end;
			{ return TOK_SNE; }
yy106:
			++end;
			if((yych = (Char)*end) == '=') goto yy108;
			{ return TOK_EQUALS; }
yy108:
			++end;
			{ return TOK_SEQ; }
yy110:
			yych = (Char)*++end;
			if(yych != '.') goto yy2;
			++end;
			{ return TOK_VARIADIC; }
yy113:
			yych = (Char)*++end;
			if(yych <= '/') goto yy2;
			if(yych >= ':') goto yy2;
yy114:
			++end;
			yych = (Char)*end;
			if(yych <= '/') goto yy116;
			if(yych <= '9') goto yy114;
yy116:
			{ return TOK_LIT_REAL; }
yy117:
			yych = (Char)*++end;
			if(yych & ~0xFF) {
				goto yy2;
			} else if(yybm[0+yych] & 128) {
				goto yy126;
			}
			goto yy2;
yy118:
			yych = (Char)*++end;
			if(yych & ~0xFF) {
				goto yy2;
			} else if(yybm[0+yych] & 64) {
				goto yy123;
			}
			goto yy2;
yy119:
			yych = (Char)*++end;
			if(yych & ~0xFF) {
			} else if(yybm[0+yych] & 32) {
				goto yy120;
			}
			goto yy2;
yy120:
			++end;
			yych = (Char)*end;
			if(yych & ~0xFF) {
			} else if(yybm[0+yych] & 32) {
				goto yy120;
			}
			{ return TOK_LIT_HEX; }
yy123:
			++end;
			yych = (Char)*end;
			if(yych & ~0xFF) {
			} else if(yybm[0+yych] & 64) {
				goto yy123;
			}
			{ return TOK_LIT_BIN; }
yy126:
			++end;
			yych = (Char)*end;
			if(yych & ~0xFF) {
			} else if(yybm[0+yych] & 128) {
				goto yy126;
			}
			{ return TOK_LIT_ROM; }
yy129:
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
yy134:
			yych = (Char)*++end;
			if(yych != 'r') goto yy27;
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 8) {
				goto yy26;
			}
			{ return TOK_VAR; }
yy137:
			yych = (Char)*++end;
			if(yych == 'i') goto yy144;
			if(yych == 'r') goto yy145;
			goto yy27;
yy138:
			yych = (Char)*++end;
			if(yych == 'u') goto yy139;
			if(yych == 'y') goto yy140;
			goto yy27;
yy139:
			yych = (Char)*++end;
			if(yych == 'e') goto yy142;
			goto yy27;
yy140:
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 8) {
				goto yy26;
			}
			{ return TOK_TRY; }
yy142:
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 8) {
				goto yy26;
			}
			{ return TOK_TRUE; }
yy144:
			yych = (Char)*++end;
			if(yych == 's') goto yy149;
			goto yy27;
yy145:
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
yy149:
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 8) {
				goto yy26;
			}
			{ return TOK_THIS; }
yy151:
			yych = (Char)*++end;
			if(yych == 'r') goto yy158;
			goto yy27;
yy152:
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
yy158:
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
yy163:
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
yy169:
			yych = (Char)*++end;
			if(yych == 'w') goto yy174;
			goto yy27;
yy170:
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
yy174:
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 8) {
				goto yy26;
			}
			{ return TOK_NEW; }
yy176:
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 8) {
				goto yy26;
			}
			{ return TOK_IF; }
yy178:
			++end;
			if((yych = (Char)*end) <= '_') {
				if(yych <= '@') {
					if(yych <= '/') goto yy179;
					if(yych <= '9') goto yy26;
				} else {
					if(yych <= 'Z') goto yy26;
					if(yych >= '_') goto yy26;
				}
			} else {
				if(yych <= 'c') {
					if(yych <= '`') goto yy179;
					if(yych <= 'b') goto yy26;
					goto yy180;
				} else {
					if(yych == 't') goto yy181;
					if(yych <= 'z') goto yy26;
				}
			}
yy179:
			{ return TOK_IN; }
yy180:
			yych = (Char)*++end;
			if(yych == 'l') goto yy183;
			goto yy27;
yy181:
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 8) {
				goto yy26;
			}
			{ return TOK_INT; }
yy183:
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
yy188:
			yych = (Char)*++end;
			if(yych == 'l') goto yy207;
			goto yy27;
yy189:
			yych = (Char)*++end;
			if(yych == 'n') goto yy201;
			goto yy27;
yy190:
			yych = (Char)*++end;
			if(yych == 'r') goto yy199;
			goto yy27;
yy191:
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
yy199:
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 8) {
				goto yy26;
			}
			{ return TOK_FOR; }
yy201:
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
yy207:
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
yy211:
			yych = (Char)*++end;
			if(yych == 's') goto yy218;
			goto yy27;
yy212:
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
yy218:
			yych = (Char)*++end;
			if(yych != 'e') goto yy27;
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 8) {
				goto yy26;
			}
			{ return TOK_ELSE; }
yy221:
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
yy228:
			yych = (Char)*++end;
			if(yych <= 'r') goto yy27;
			if(yych <= 's') goto yy237;
			if(yych <= 't') goto yy238;
			goto yy27;
yy229:
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
yy237:
			yych = (Char)*++end;
			if(yych == 'e') goto yy242;
			goto yy27;
yy238:
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
yy242:
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 8) {
				goto yy26;
			}
			{ return TOK_CASE; }
yy244:
			yych = (Char)*++end;
			if(yych == 'o') goto yy250;
			goto yy27;
yy245:
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
yy250:
			yych = (Char)*++end;
			if(yych != 'l') goto yy27;
			++end;
			if((yych = (Char)*end) & ~0xFF) {
			} else if(yybm[0+yych] & 8) {
				goto yy26;
			}
			{ return TOK_BOOL; }
yy253:
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
yy261:
			++end;
			{ return TOK_ASSDIV; }
yy263:
			++end;
			{ return TOK_COMMENT; }
yy265:
			++end;
			{ return TOK_COMMENT; }
		}
	}

}
