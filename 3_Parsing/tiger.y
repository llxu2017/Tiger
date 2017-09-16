%{
#include <stdio.h>
#include "util.h"
#include "errormsg.h"

#define YYDEBUG 1

yydebug = 0;

int yylex(void); /* function prototype */

void yyerror(char *s)
{
 EM_error(EM_tokPos, "%s", s);
}
%}


%union {
	int pos;
	int ival;
	string sval;
	}

%token <sval> ID STRING
%token <ival> INT



%token 
  COMMA COLON SEMICOLON
  LPAREN RPAREN LBRACK RBRACK LBRACE RBRACE
  DOT
  ARRAY
  BREAK NIL
  FUNCTION VAR TYPE
  LET IN END
  IF WHILE FOR TO

%nonassoc THEN DO
%right ELSE
%right ASSIGN
%left AND OR
%nonassoc EQ NEQ LT LE GT GE
%left PLUS MINUS
%left TIMES DIVIDE
%nonassoc OF


%start program

%%

program
       : exp
	   ;

declist
       : %empty
	   | dec
	   | declist dec
	   ;

dec
   : typedec
   | vardec
   | funcdeclist
   ;

funcdeclist
           : funcdec
		   | funcdeclist funcdec
		   ;

nametylist
          : namety
		  | nametylist namety
		  ;

typedec
       : nametylist
	   ;

ty
  : ID
  | recordty 
  | arrayty
  ;

namety
       : TYPE ID EQ ty
	   ;

recordty
        : LBRACE fieldlist RBRACE
		;

arrayty
       : ARRAY OF ID
	   ;

efieldlist
          : %empty
		  | efield
		  | efieldlist COMMA efield
		  ;

efield
      : ID EQ exp
	  ;

fieldlist
         : %empty
		 | field
		 | fieldlist COMMA field
		 ;

field
     : ID COLON ID
     ;

var
   : simplevar
   | fieldvar
   | subscriptvar
   ;

simplevar
         : ID
		 ;

fieldvar
        : var DOT ID
		;

subscriptvar
            : ID LBRACK exp RBRACK
			| var LBRACK exp RBRACK
			;

vardec
      : VAR ID ASSIGN exp
	  | VAR ID COLON ID ASSIGN exp
	  ;

funcdec
       : FUNCTION ID LPAREN fieldlist RPAREN EQ exp
	   | FUNCTION ID LPAREN fieldlist RPAREN COLON ID EQ exp
	   ;

explist
       : %empty
	   | exp
	   | explist SEMICOLON exp
	   | explist COMMA exp
	   ;

opexp
     : exp PLUS exp
     | exp MINUS exp
	 | MINUS exp
     | exp TIMES exp
     | exp DIVIDE exp
     | exp EQ exp
	 | exp NEQ exp
     | exp LT exp
     | exp LE exp
     | exp GT exp
     | exp GE exp
     | exp AND exp
	 | exp OR exp
     ;

ifexp
     : IF exp THEN exp ELSE exp
	 | IF exp THEN exp
	 ;

whileexp
        : WHILE exp DO exp
	    ;

forexp
      : FOR ID ASSIGN exp TO exp DO exp
      ;

arrayexp
        : ID LBRACK exp RBRACK OF exp
		;

recordexp
         : ID LBRACE efieldlist RBRACE
		 ;

varexp
      : var
	  ;

nilexp
      : NIL
	  ;

intexp
      : INT
	  ;

stringexp
         : STRING
		 ;

callexp
       : ID LPAREN explist RPAREN
	   ;

seqexp
      : LPAREN explist RPAREN
	  ;

assignexp
         : var ASSIGN exp
		 ;

breakexp
        : BREAK
		;

letexp
      : LET declist IN explist END
	  ;

exp
   : varexp
   | nilexp
   | intexp
   | stringexp
   | callexp
   | opexp
   | recordexp
   | seqexp
   | assignexp
   | ifexp
   | whileexp
   | forexp
   | breakexp
   | letexp
   | arrayexp
   ;


//Fail as expected
//49