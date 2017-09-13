%code top{
#include <stdio.h>
#include "util.h"
#include "symbol.h" 
#include "errormsg.h"


int yylex(void); /* function prototype */



void yyerror(char *s)
{
 EM_error(EM_tokPos, "%s", s);
}
}


%union {
	int pos;
	int ival;
	string sval;
	A_var var;
	A_exp exp;
	A_expList explist;
	A_dec dec;
	A_decList declist;
	A_ty ty;
	A_field field;
	A_fieldList fieldlist;
	A_fundec funcdec;
	A_fundecList funcdeclist;
	A_namety namety;
	A_nametyList nametylist;
	A_efield efield;
	A_efieldList efieldlist;
	}

%code requires{
	#include "absyn.h"
	A_exp absyn_root;
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

%type <exp> program exp varexp seqexp
%type <var> var simplevar fieldvar subscriptvar
%type <exp> intexp stringexp nilexp arrayexp recordexp assignexp breakexp callexp opexp forexp whileexp ifexp letexp
%type <dec> dec typedec vardec
%type <ty> ty arrayty recordty
%type <field> field
%type <fieldlist> fieldlist
%type <explist> explist
%type <funcdec> funcdec
%type <funcdeclist> funcdeclist
%type <declist> declist
%type <namety> namety
%type <nametylist> nametylist
%type <efield> efield
%type <efieldlist> efieldlist


%start program

%%
   
program
       : exp		{absyn_root = $1;}
	   ;

declist
       : %empty		{$$ = NULL;}
	   //| dec		{$$ = A_DecList($1, NULL);}
	   | dec declist		{$$ = A_DecList($1, $2);}
	   ;

dec
   : typedec		{$$ = $1;}
   | vardec		{$$ = $1;}
   | funcdeclist		{$$ = A_FunctionDec(EM_tokPos, $1);}
   ;

funcdeclist
           : funcdec		{$$ = A_FundecList($1, NULL);}
		   | funcdec funcdeclist		{$$ = A_FundecList($1, $2);}
		   ;

nametylist
          : namety		{$$ = A_NametyList($1, NULL);}
		  | namety nametylist		{$$ = A_NametyList($1, $2);}
		  ;

typedec
       : nametylist		{$$ = A_TypeDec(EM_tokPos, $1);}
	   ;

ty
  : ID		{$$ = A_NameTy(EM_tokPos, S_Symbol($1));}
  | recordty		{$$ = $1;}
  | arrayty		{$$ = $1;}
  ;

namety
      : TYPE ID EQ ty		{$$ = A_Namety(S_Symbol($2), $4);}
	  ;

recordty
        : LBRACE fieldlist RBRACE		{$$ = A_RecordTy(EM_tokPos, $2);}
		;

arrayty
       : ARRAY OF ID		{$$ = A_ArrayTy(EM_tokPos, S_Symbol($3));}
	   ;

efieldlist
          : %empty		{$$ = NULL;}
		  | efield		{$$ = A_EfieldList($1, NULL);}
		  | efield COMMA efieldlist		{$$ = A_EfieldList($1, $3);}
		  ;

efield
      : ID EQ exp		{$$ = A_Efield(S_Symbol($1), $3);}
	  ;

fieldlist
         : %empty		{$$ = NULL;}
		 | field		{$$ = A_FieldList($1, NULL);}
		 | field COMMA fieldlist		{$$ = A_FieldList($1, $3);}
		 ;

field
     : ID COLON ID		{$$ = A_Field(EM_tokPos, S_Symbol($1), S_Symbol($3));}
     ;

var
   : simplevar		{$$ = $1;}
   | fieldvar		{$$ = $1;}
   | subscriptvar		{$$ = $1;}
   | var DOT var
   ;

simplevar
         : ID		{$$ = A_SimpleVar(EM_tokPos, S_Symbol($1));}
		 ;

fieldvar
        : VAR ID COLON ID		{$$ = A_FieldVar(EM_tokPos, A_SimpleVar(EM_tokPos, S_Symbol($2)), S_Symbol($4));}
		;

subscriptvar
            : ID LBRACK exp RBRACK		{$$ = A_SubscriptVar(EM_tokPos, A_SimpleVar(EM_tokPos, S_Symbol($1)), $3);}
			| subscriptvar LBRACK exp RBRACK		{$$ = A_SubscriptVar(EM_tokPos, $1, $3);}
			;

vardec
      : VAR ID ASSIGN exp		{$$ = A_VarDec(EM_tokPos, S_Symbol($2), S_Symbol("NULL"), $4);}
	  | VAR ID COLON ID ASSIGN exp		{$$ = A_VarDec(EM_tokPos, S_Symbol($2), S_Symbol($4), $6);}
	  ;

funcdec
       : FUNCTION ID LPAREN fieldlist RPAREN EQ exp		{$$ = A_Fundec(EM_tokPos, S_Symbol($2), $4, S_Symbol("NULL"), $7);}
	   | FUNCTION ID LPAREN fieldlist RPAREN COLON ID EQ exp		{$$ = A_Fundec(EM_tokPos, S_Symbol($2), $4, S_Symbol($7), $9);}
	   ;

explist
       : %empty		{$$ = NULL;}
	   | exp		{$$ = A_ExpList($1, NULL);}
	   | exp SEMICOLON explist		{$$ = A_ExpList($1, $3);}
	   | exp COMMA explist		{$$ = A_ExpList($1, $3);}
	   ;

opexp
     : exp PLUS exp		{$$ = A_OpExp(EM_tokPos, A_plusOp, $1, $3);}
     | exp MINUS exp		{$$ = A_OpExp(EM_tokPos, A_minusOp, $1, $3);}
	 | MINUS exp		{$$ = A_OpExp(EM_tokPos, A_minusOp, A_IntExp(EM_tokPos, 0), $2);}
     | exp TIMES exp		{$$ = A_OpExp(EM_tokPos, A_timesOp, $1, $3);}
     | exp DIVIDE exp		{$$ = A_OpExp(EM_tokPos, A_divideOp, $1, $3);}
     | exp EQ exp		{$$ = A_OpExp(EM_tokPos, A_eqOp, $1, $3);}
	 | exp NEQ exp		{$$ = A_OpExp(EM_tokPos, A_neqOp, $1, $3);}
     | exp LT exp		{$$ = A_OpExp(EM_tokPos, A_ltOp, $1, $3);}
     | exp LE exp		{$$ = A_OpExp(EM_tokPos, A_leOp, $1, $3);}
     | exp GT exp		{$$ = A_OpExp(EM_tokPos, A_gtOp, $1, $3);}
     | exp GE exp		{$$ = A_OpExp(EM_tokPos, A_geOp, $1, $3);}
     | exp AND exp		{$$ = A_IfExp(EM_tokPos, $1, $3, A_IntExp(EM_tokPos, 0));}
	 | exp OR exp		{$$ = A_IfExp(EM_tokPos, $1, A_IntExp(EM_tokPos, 1), $3);}
     ;

ifexp
     : IF exp THEN exp ELSE exp		{$$ = A_IfExp(EM_tokPos, $2, $4, $6);}
	 | IF exp THEN exp		{$$ = A_IfExp(EM_tokPos, $2, $4, NULL);}
	 ;

whileexp
        : WHILE exp DO exp		{$$ = A_WhileExp(EM_tokPos, $2, $4);}
	    ;

forexp
      : FOR var ASSIGN exp TO exp DO exp		{$$ = A_ForExp(EM_tokPos, S_Symbol($2), $4, $6, $8);}
      ;

arrayexp
        : ID LBRACK exp RBRACK OF exp		{$$ = A_ArrayExp(EM_tokPos, S_Symbol($1), $3, $6);}
		;

recordexp
         : ID LBRACE efieldlist RBRACE		{$$ = A_RecordExp(EM_tokPos, S_Symbol($1), $3);}
		 ;

varexp
      : var		{$$ = A_VarExp(EM_tokPos, $1);}
	  ;

nilexp
      : NIL		{$$ = A_NilExp(EM_tokPos);}
	  ;

intexp
      : INT		{$$ = A_IntExp(EM_tokPos, $1);}
	  ;

stringexp
         : STRING		{$$ = A_StringExp(EM_tokPos, $1);}
		 ;

callexp
       : ID LPAREN explist RPAREN		{$$ = A_CallExp(EM_tokPos, S_Symbol($1), $3);}
	   ;

seqexp
      : explist		{$$ = A_SeqExp(EM_tokPos, $1);}
	  ;

assignexp
         : var ASSIGN exp		{$$ = A_AssignExp(EM_tokPos, $1, $3);}
		 ;

breakexp
        : BREAK		{$$ = A_BreakExp(EM_tokPos);}
		;

letexp
      : LET declist IN seqexp END		{$$ = A_LetExp(EM_tokPos, $2, $4);}
	  | LET declist IN exp END		{$$ = A_LetExp(EM_tokPos, $2, $4);}
	  ;

exp
   : varexp		{$$ = $1;}
   | nilexp		{$$ = $1;}
   | intexp		{$$ = $1;}
   | stringexp		{$$ = $1;}
   | callexp		{$$ = $1;}
   | opexp		{$$ = $1;}
   | recordexp		{$$ = $1;}
   | LPAREN seqexp RPAREN		{$$ = $2;}
   | assignexp		{$$ = $1;}
   | ifexp		{$$ = $1;}
   | whileexp		{$$ = $1;}
   | forexp		{$$ = $1;}
   | breakexp		{$$ = $1;}
   | letexp		{$$ = $1;}
   | arrayexp		{$$ = $1;}
   ;


	
