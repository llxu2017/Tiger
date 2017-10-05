#ifndef ESCAPE_H
#define ESCAPE_H

#include "absyn.h"

void Esc_findEscape(A_exp exp);

typedef struct E_escapeentry_ *E_escapeentry;

struct E_escapeentry_ {
	int depth;
	bool escape;
};

E_escapeentry E_EscapeEntry(int depth, bool escape);

#endif
