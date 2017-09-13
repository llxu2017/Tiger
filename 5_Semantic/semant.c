#include <stdio.h>
#include "semant.h"
#include "errormsg.h"
#include "env.h"

struct expty expTy(Tr_exp exp, Ty_ty ty)
{
	struct expty e;
	e.exp = exp;
	e.ty = ty;
	return e;
}

E_enventry E_VarEntry(Ty_ty ty)
{
	E_enventry p = checked_malloc(sizeof(*p));
	p->kind = E_varEntry;
	p->u.var.ty = ty;
	return p;
}

// TO DO
Ty_ty actual_ty(Ty_ty ty)
{
	return ty;
}

struct expty handle_intExp(S_table venv, S_table tenv, A_exp a)
{
	static struct expty expty_int = { NULL };
	expty_int.ty = Ty_Int();
	return expty_int;
}

struct expty handle_stringExp(S_table venv, S_table tenv, A_exp a)
{
	static struct expty expty_string = { NULL };
	expty_string.ty = Ty_String();
	return expty_string;
}

struct expty handle_opExp(S_table venv, S_table tenv, A_exp a)
{
	A_oper oper = a->u.op.oper;
	struct expty left = transExp(venv, tenv, a->u.op.left);
	struct expty right = transExp(venv, tenv, a->u.op.right);
	switch (oper) {
		case A_plusOp: 
		case A_minusOp:
		case A_timesOp:
		case A_divideOp:{
			if (left.ty->kind != Ty_int)
				EM_error(a->u.op.left->pos, "integer required.");
			if (right.ty->kind != Ty_int)
				EM_error(a->u.op.right->pos, "integer required.");
			return expTy(NULL, Ty_Int());
		}
		default:
			assert(0);
	}
}

struct expty handle_letExp(S_table venv, S_table tenv, A_exp a)
{
	struct expty exp;
	A_decList d;
	S_beginScope(venv);
	S_beginScope(tenv);
	for (d = a->u.let.decs; d; d = d->tail)
		transDec(venv, tenv, d->head);
	exp = transExp(venv, tenv, a->u.let.body);
	S_endScope(tenv);
	S_endScope(venv);
	return exp;
}

struct expty handle_arrayExp(S_table venv, S_table tenv, A_exp a)
{
	struct expty exp = { NULL };
	Ty_ty x = S_look(tenv, a->u.array.typ);
	if (!x)
		assert(0);
	struct expty size = transExp(venv, tenv, a->u.array.size);
	if (size.ty->kind != Ty_int)
		EM_error(a->pos, "array index must be integers.");
	struct expty init = transExp(venv, tenv, a->u.array.init);
	if (init.ty->kind != x->u.array->kind)
		EM_error(a->pos, "array initializer must be of the same type as array.");
	exp.ty = Ty_Array(x);
	return exp;
}

struct expty transExp(S_table venv, S_table tenv, A_exp a){
	switch (a->kind){
		case A_varExp: return transVar(venv, tenv, a->u.var);
		case A_nilExp: assert(0);
		case A_intExp: return handle_intExp(venv, tenv, a);
		case A_stringExp: return handle_stringExp(venv, tenv, a);
		case A_callExp: assert(0);
		case A_opExp: return handle_opExp(venv, tenv, a);
		case A_recordExp:
		case A_seqExp:
		case A_assignExp:
		case A_ifExp:
		case A_whileExp:
		case A_forExp:
		case A_breakExp:
			assert(0);
		case A_letExp: return handle_letExp(venv, tenv, a);
		case A_arrayExp: return handle_arrayExp(venv, tenv, a);
		default:
			assert(0);
	}
}

struct expty transVar(S_table venv, S_table tenv, A_var v)
{
	switch (v->kind){
		case A_simpleVar:{
			E_enventry x = S_look(venv, v->u.simple);
			if (x && x->kind == E_varEntry)
				return expTy(NULL, actual_ty(x->u.var.ty));
			else{
				EM_error(v->pos, "undefined variable \"%s\".\n", S_name(v->u.simple));
				return expTy(NULL, Ty_Int());
		}
		case A_fieldVar:
		case A_subscriptVar:
		default:
			assert(0);
		}

	}
}

void transDec(S_table venv, S_table tenv, A_dec d)
{
	switch (d->kind) {
		case A_varDec: {
			Ty_ty x = S_look(tenv, d->u.var.typ);
			if (x)
				fprintf(stderr, "Type \"%d\" defined.\n", x->u.array->kind);
			struct expty e = transExp(venv, tenv, d->u.var.init);
			if (d->u.var.typ != S_Symbol("NULL"))
				fprintf(stderr, "%s", "Type is explicitly initialized.\n");
			S_enter(venv, d->u.var.var, E_VarEntry(e.ty));
			break;
		}
		case A_typeDec: {
			S_enter(tenv, d->u.type->head->name, transTy(tenv, d->u.type->head->ty));
			break;
		}
		case A_functionDec:
		default:
			assert(0);
	}
}

Ty_ty transTy(S_table tenv, A_ty a)
{
	switch (a->kind) {
		case A_nameTy: {
			Ty_ty x = S_look(tenv, a->u.name);
			return Ty_Name(a->u.name, x);
		}
		case A_recordTy: assert(0);
		case A_arrayTy: {
			Ty_ty x = S_look(tenv, a->u.array);
			if (x)
				fprintf(stderr, "Type \"%s\" defined.\n", S_name(a->u.array));
			return Ty_Array(x);
		}
		default: assert(0);
	}
}

void SEM_transProg(A_exp exp)
{
	S_table venv = E_base_venv();
	S_table tenv = E_base_tenv();
	transExp(venv, tenv, exp);
}
