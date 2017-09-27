/*
 * parse.c - Parse source file.
 */

#include <stdio.h>
#include "util.h"
#include "symbol.h"
#include "absyn.h"
#include "errormsg.h"
#include "parse.h"

extern int yyparse(void);
extern A_exp absyn_root;

/* parse source file fname; 
   return abstract syntax data structure */
A_exp parse(string fname) 
{EM_reset(fname);
 if (yyparse() == 0) /* parsing worked */
   return absyn_root;
 else return NULL;
}

int main()
{
	int i;
	char buf[100];
	int file = 1;
	FILE *fp = fopen("tree.txt", "w");
	for (i = 0; i < 49; ++i)
	{
		fprintf(fp, "%d:\n", i);
		snprintf(buf, 100, "%s%d%s", "..\\1_Tests\\Official\\test", i, ".tig");
		absyn_root = parse(buf);
		pr_exp(fp, absyn_root, 0);
		fprintf(fp, "\n");
	}
	{
		fprintf(fp, "%s:", "merge");
		snprintf(buf, 100, "%s", "..\\1_Tests\\Official\\merge.tig");
		absyn_root = parse(buf);
		pr_exp(fp, absyn_root, 0);
		fprintf(fp, "\n");
	}
	{
		fprintf(fp, "%s:", "queens");
		snprintf(buf, 100, "%s", "..\\1_Tests\\Official\\queens.tig");
		absyn_root = parse(buf);
		pr_exp(fp, absyn_root, 0);
		fprintf(fp, "\n");
	}

	fclose(fp);
	return 0;

}
