#include <stdio.h>
#include <string.h>
#include "util.h"
#include "errormsg.h"
#include "tiger.tab.h"

extern int yyparse(void);

void parse(string fname)
{
	EM_reset(fname);
	if (yyparse() == 0) /* parsing worked */
		fprintf(stderr, "Parsing successful!\n");
	else fprintf(stderr, "Parsing failed\n");
}


int main(int argc, char **argv) {
	//if (argc!=2) {fprintf(stderr,"usage: a.out filename\n"); exit(1);}
	//parse(argv[1]);
	int i;
	char buf[100];
	for (i = 1; i < 50; ++i)
	{
		printf("%d:", i);
		snprintf(buf, 100, "%s%d%s", "..\\1_Tests\\Official\\test", i, ".tig");
		parse(buf);
	}
	parse("..\\1_Tests\\Official\\merge.tig");
	parse("..\\1_Tests\\Official\\queens.tig");
	return 0;
}
