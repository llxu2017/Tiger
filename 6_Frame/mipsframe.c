#include "frame.h"


struct F_access_ {
	enum {inFrame, inReg} kind;
	union {
		int offset; /* InFrame */
		Temp_temp reg; /* InReg */
	}u;
};

static F_access InFrame(int offset)
{
	F_access p = checked_malloc(sizeof(*p));
	p->kind = inFrame;
	p->u.offset = offset;
	return p;
}

static F_access InReg(Temp_temp reg)
{
	F_access p = checked_malloc(sizeof(*p));
	p->kind = inReg;
	p->u.reg = reg;
	return p;
}

F_frame F_newFrame(Temp_label name, U_boolList formals)
{

}

Temp_label F_name(F_frame f)
{

}

F_accessList F_formals(F_frame f)
{

}

F_access F_allocLocal(F_frame f, bool escape)
{

}