#include "translate.h"
#include "frame.h"


static Tr_level tr_level = 0;

struct Tr_access_ { Tr_level level; F_access access; };

Tr_accessList Tr_AccessList(Tr_access head, Tr_accessList tail)
{
	Tr_accessList list = checked_malloc(sizeof(*list));
	list->head = head;
	list->tail = tail;
	return list;
}

Tr_level Tr_outermost(void)
{

}

Tr_level Tr_newLevel(Tr_level parent, Temp_label name, U_boolList formals)
{

}

Tr_accessList Tr_formals(Tr_level level)
{

}

Tr_access Tr_allocLocal(Tr_level level, bool escape)
{

}