#include <stdio.h>
#include "util.h"
#include "errormsg.h"
#include "tokens.h"

YYSTYPE yylval;

int yylex(void); /* prototype for the lexing function */



string toknames[] = {
"ID", "STRING", "INT", "COMMA", "COLON", "SEMICOLON", "LPAREN",
"RPAREN", "LBRACK", "RBRACK", "LBRACE", "RBRACE", "DOT", "ARRAY", "BREAK",
"NIL", "FUNCTION", "VAR", "TYPE", "LET", "IN", "END", "IF",
"WHILE", "FOR", "TO", "THEN", "DO", "ELSE", "ASSIGN", "AND", "OR",
"EQ", "NEQ", "LT", "LE", "GT", "GE", "PLUS", "MINUS", "TIMES", "DIVIDE", 
"OF"};


string tokname(tok) {
	return tok < 258 || tok>300 ? "BAD_TOKEN" : toknames[tok - 258];
}

void analyze()
{
	int tok;
	for (;;) {
		tok = yylex();
		if (tok == 0) break;
		switch (tok) {
		case ID: case STRING:
			printf("%10s %4d %s\n", tokname(tok), EM_tokPos, yylval.sval);
			break;
		case INT:
			printf("%10s %4d %d\n", tokname(tok), EM_tokPos, yylval.ival);
			break;
		default:
			printf("%10s %4d\n", tokname(tok), EM_tokPos);
		}
	}
}

int main(int argc, char **argv) {
	//string fname; int tok;
	//if (argc!=2) {fprintf(stderr,"usage: a.out filename\n"); exit(1);}
	//fname=argv[1];
	int i;
	char buf[100];
	int file = 1;
	for (i = 1; i < 50; ++i)
	{
		printf("%d:", i);
		snprintf(buf, 100, "%s%d%s", "..\\1_Tests\\Official\\test", i, ".tig");
		EM_reset(buf);
		analyze();
	}

	return 0;
}


