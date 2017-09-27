%{
#include <math.h>
#include <string.h>
#include "util.h"
#include "tokens.h"
#include "errormsg.h"

int charPos=1;

int yywrap(void)
{
 charPos=1;
 return 1;
}


void adjust(void)
{
 EM_tokPos=charPos;
 charPos+=yyleng;
}

%}


/* Lex Definitions: */
digits [0-9]+

%x comment
%x str

%option stack

%%

        #define MAX_STR_CONST 10000
        char string_buf[MAX_STR_CONST];
        char *string_buf_ptr = string_buf;

"/*"					{adjust(); yy_push_state(comment);}
<comment>{
	"/*"				{adjust(); yy_push_state(comment);}
	"*"+                {adjust(); continue;}
	[^/*]+              {adjust(); continue;}
	[\n]				{adjust(); EM_newline(); continue;}
	"/"					{adjust(); continue;}
	"*/"				{adjust(); yy_pop_state();}
	<<EOF>>				{adjust(); EM_error(EM_tokPos,"Unclosed comment"); BEGIN(INITIAL); yyterminate();}
}


\"						{adjust(); string_buf_ptr = string_buf; BEGIN(str);}

<str>\"					{
							/* saw closing quote - all done */
							adjust();
							*string_buf_ptr = '\0';
							yylval.sval=String(string_buf);
							BEGIN(INITIAL);
							return STRING;
							/* return string constant token type and
							* value to parser
							*/
						}

<str>\n					{
							/* error - unterminated string constant */
							/* generate error message */
							adjust();
						}

<str>\\[0-7]{1,3}		{
							/* octal escape sequence */
							adjust();
							int result;
							(void) sscanf( yytext + 1, "%o", &result );
							if ( result > 0xff );
								/* error, constant is out-of-bounds */
							*string_buf_ptr++ = result;
						}

<str>\\[0-9]+			{
							/* generate error - bad escape sequence; something
							* like '\48' or '\0777777'
							*/
							adjust();
						}

<str>\\n				{adjust(); *string_buf_ptr++ = '\n';}
<str>\\t				{adjust(); *string_buf_ptr++ = '\t';}
<str>\\r				{adjust(); *string_buf_ptr++ = '\r';}
<str>\\b				{adjust(); *string_buf_ptr++ = '\b';}
<str>\\f				{adjust(); *string_buf_ptr++ = '\f';}
<str><<EOF>>			{adjust(); EM_error(EM_tokPos,"Unclosed string"); BEGIN(INITIAL); yyterminate();}

<str>\\(.|\n)			{adjust(); *string_buf_ptr++ = yytext[1];}

<str>[^\\\n\"]+			{
							adjust();
							char *yptr = yytext;
							while ( *yptr )
								*string_buf_ptr++ = *yptr++;
						}


	/* Keywords */
array					{adjust(); return ARRAY;}
if						{adjust(); return IF;}
then					{adjust(); return THEN;}
else					{adjust(); return ELSE;}
while					{adjust(); return WHILE;}
for						{adjust(); return FOR;}
to						{adjust(); return TO;}
do						{adjust(); return DO;}
let						{adjust(); return LET;}
in						{adjust(); return IN;}
end						{adjust(); return END;}
of						{adjust(); return OF;}
break					{adjust(); return BREAK;}
nil						{adjust(); return NIL;}
function				{adjust(); return FUNCTION;}
var						{adjust(); return VAR;}
type					{adjust(); return TYPE;}


	/* punctuations */
" "						{adjust(); continue;}
"\t"					{adjust(); continue;}
"\r"					{adjust(); continue;}
","						{adjust(); return COMMA;}
":"						{adjust(); return COLON;}
";"						{adjust(); return SEMICOLON;}
"("						{adjust(); return LPAREN;}
")"						{adjust(); return RPAREN;}
"["						{adjust(); return LBRACK;}
"]"						{adjust(); return RBRACK;}
"{"						{adjust(); return LBRACE;}
"}"						{adjust(); return RBRACE;}
"\n"					{adjust(); EM_newline(); continue;}


	/* Operators */
"."						{adjust(); return DOT;}
"+"						{adjust(); return PLUS;}
"-"						{adjust(); return MINUS;}
"*"						{adjust(); return TIMES;}
"/"						{adjust(); return DIVIDE;}
"="						{adjust(); return EQ;}
"<>"					{adjust(); return NEQ;}
"<"						{adjust(); return LT;}
"<="					{adjust(); return LE;}
">"						{adjust(); return GT;}
">="					{adjust(); return GE;}
"&"						{adjust(); return AND;}
"|"						{adjust(); return OR;}
":="					{adjust(); return ASSIGN;}


	/* Data types */
[a-zA-Z][a-zA-Z0-9_]*	{adjust(); yylval.sval=String(yytext); return ID;}
{digits}				{adjust(); yylval.ival=atoi(yytext); return INT;}
<<EOF>>					{adjust(); BEGIN(INITIAL); yyterminate();}
.						{adjust(); EM_error(EM_tokPos,"illegal token");}


