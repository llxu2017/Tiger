/*
 * parse.c - Parse source file.
 */

#include <stdio.h>
#include "absyn.h"
#include "errormsg.h"
#include "parse.h"
#include "prabsyn.h"
#include "semant.h"

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
	const int j = 49;
	for (i = 1; i < j + 1; ++i)
	{
		printf("%d\n", i);
		fprintf(fp, "%d:\n", i);
		snprintf(buf, 100, "%s%d%s", "D:\\tiger\\tiger\\testcases\\test", i, ".tig");
		//snprintf(buf, 100, "%s%d%s", "D:\\tiger\\Testcases\\Good\\", i, ".tig");
		absyn_root = parse(buf);
		if (absyn_root) {
			pr_exp(fp, absyn_root, 0);
			fprintf(fp, "\n");
			SEM_transProg(absyn_root);
		}

	}
	//{
	//	fprintf(fp, "%s:", "merge");
	//	snprintf(buf, 100, "%s", "D:\\tiger\\tiger\\testcases\\merge.tig");
	//	absyn_root = parse(buf);
	//	pr_exp(fp, absyn_root, 0);
	//	fprintf(fp, "\n");
	//}
	//{
	//	fprintf(fp, "%s:", "queens");
	//	snprintf(buf, 100, "%s", "D:\\tiger\\tiger\\testcases\\queens.tig");
	//	absyn_root = parse(buf);
	//	pr_exp(fp, absyn_root, 0);
	//	fprintf(fp, "\n");
	//}

	fclose(fp);
	return 0;
}
