/* A Bison parser, made by GNU Bison 3.0.4.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

#ifndef YY_YY_TIGER_TAB_H_INCLUDED
# define YY_YY_TIGER_TAB_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif
/* "%code requires" blocks.  */
#line 39 "tiger.y" /* yacc.c:1909  */

	#include "absyn.h"
	A_exp absyn_root;

#line 49 "tiger.tab.h" /* yacc.c:1909  */

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    ID = 258,
    STRING = 259,
    INT = 260,
    COMMA = 261,
    COLON = 262,
    SEMICOLON = 263,
    LPAREN = 264,
    RPAREN = 265,
    LBRACK = 266,
    RBRACK = 267,
    LBRACE = 268,
    RBRACE = 269,
    DOT = 270,
    ARRAY = 271,
    BREAK = 272,
    NIL = 273,
    FUNCTION = 274,
    VAR = 275,
    TYPE = 276,
    LET = 277,
    IN = 278,
    END = 279,
    IF = 280,
    WHILE = 281,
    FOR = 282,
    TO = 283,
    THEN = 284,
    DO = 285,
    ELSE = 286,
    ASSIGN = 287,
    AND = 288,
    OR = 289,
    EQ = 290,
    NEQ = 291,
    LT = 292,
    LE = 293,
    GT = 294,
    GE = 295,
    PLUS = 296,
    MINUS = 297,
    TIMES = 298,
    DIVIDE = 299,
    OF = 300
  };
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED

union YYSTYPE
{
#line 19 "tiger.y" /* yacc.c:1909  */

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
	

#line 128 "tiger.tab.h" /* yacc.c:1909  */
};

typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_TIGER_TAB_H_INCLUDED  */
