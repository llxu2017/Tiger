#include "env.h"

S_table E_base_tenv(void)
{
	S_table tenv = S_empty();
	S_enter(tenv, S_Symbol("int"), Ty_Int());
	S_enter(tenv, S_Symbol("string"), Ty_String());
	S_enter(tenv, S_Symbol("nil"), Ty_Nil());
	return tenv;
}

S_table E_base_venv(void)
{
	S_table venv = S_empty();
	S_enter(venv, S_Symbol("getchar"), E_FunEntry(NULL, Ty_String()));
	S_enter(venv, S_Symbol("print"), E_FunEntry(Ty_TyList(Ty_String(), NULL), Ty_Void()));
	S_enter(venv, S_Symbol("printi"), E_FunEntry(Ty_TyList(Ty_Int(), NULL), Ty_Void()));
	S_enter(venv, S_Symbol("chr"), E_FunEntry(Ty_TyList(Ty_Int(), NULL), Ty_String()));
	S_enter(venv, S_Symbol("ord"), E_FunEntry(Ty_TyList(Ty_String(), NULL), Ty_Int()));
	S_enter(venv, S_Symbol("size"), E_FunEntry(Ty_TyList(Ty_String(), NULL), Ty_Int()));
	S_enter(venv, S_Symbol("concat"), E_FunEntry(Ty_TyList(Ty_String(), Ty_TyList(Ty_String(), NULL)), Ty_String()));
	S_enter(venv, S_Symbol("substring"), E_FunEntry(Ty_TyList(Ty_String(), Ty_TyList(Ty_Int(), Ty_TyList(Ty_Int(), NULL))), Ty_String()));
	return venv;
}


E_enventry E_VarEntry(Ty_ty ty)
{
	E_enventry p = checked_malloc(sizeof(*p));
	p->kind = E_varEntry;
	p->u.var.ty = ty;
	return p;
}

E_enventry E_FunEntry(Ty_tyList formals, Ty_ty result)
{
	E_enventry p = checked_malloc(sizeof(*p));
	p->kind = E_funEntry;
	p->u.fun.formals = formals;
	p->u.fun.result = result;
	return p;
}