#include "env.h"

S_table E_base_tenv(void)
{
	S_table tenv = S_empty();
	S_enter(tenv, S_Symbol("int"), Ty_Int());
	S_enter(tenv, S_Symbol("string"), Ty_String());
	return tenv;
}

S_table E_base_venv(void)
{
	S_table venv = S_empty();
	return venv;
}