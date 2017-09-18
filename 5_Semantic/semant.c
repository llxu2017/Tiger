#include <stdio.h>
#include "semant.h"
#include "errormsg.h"
#include "env.h"

#define MAX_PARAM 256	// Maximum number of fields length

static int rescan = 0;
static int infor = 0;
S_symbol for_idx;

struct expty expTy(Tr_exp exp, Ty_ty ty)
{
	struct expty e;
	e.exp = exp;
	e.ty = ty;
	return e;
}

static Ty_ty actual_ty(Ty_ty ty)
{
	if (rescan && !ty) return ty;
	// TO DO: detect cyclic declaration
	while (ty->kind == Ty_name)
		ty = ty->u.name.ty;
	return ty;
}

static struct expty handle_intExp(S_table venv, S_table tenv, A_exp a)
{
	static struct expty expty_int = { NULL };
	expty_int.ty = Ty_Int();
	return expty_int;
}

static struct expty handle_stringExp(S_table venv, S_table tenv, A_exp a)
{
	static struct expty expty_string = { NULL };
	expty_string.ty = Ty_String();
	return expty_string;
}

static struct expty handle_callExp(S_table venv, S_table tenv, A_exp a)
{
	static struct expty exp;
	if (rescan) return exp;
	A_expList e;
	E_enventry x = S_look(venv, a->u.call.func);
	Ty_tyList t;
	if (!x)
		EM_error(a->pos, "function \"%s\" not defined.\n", S_name(a->u.call.func));
	for (e = a->u.call.args, t = x->u.fun.formals; e && t; e = e->tail, t = t->tail) {
		exp = transExp(venv, tenv, e->head);
		if (actual_ty(exp.ty)->kind != actual_ty(t->head)->kind)
			EM_error(a->pos, "function arguments type mismatch.\n");
	}
	if (e || t)
		EM_error(a->pos, "number of function arguments do not match.\n");
	exp.exp = NULL;
	exp.ty = x->u.fun.result;
	return exp;
}

static struct expty handle_opExp(S_table venv, S_table tenv, A_exp a)
{
	A_oper oper = a->u.op.oper;
	struct expty left = transExp(venv, tenv, a->u.op.left);
	struct expty right = transExp(venv, tenv, a->u.op.right);
	switch (oper) {
		case A_plusOp:
		case A_minusOp:
		case A_timesOp:
		case A_divideOp: {
			if (actual_ty(left.ty)->kind != Ty_int)
				EM_error(a->u.op.left->pos, "integer required.");
			if (actual_ty(right.ty)->kind != Ty_int)
				EM_error(a->u.op.right->pos, "integer required.");
			return expTy(NULL, Ty_Int());
		}
		case A_eqOp:
		case A_neqOp:
		case A_ltOp:
		case A_gtOp:
		case A_geOp:
		case A_leOp: {
			if (actual_ty(left.ty)->kind != actual_ty(right.ty)->kind)
				EM_error(a->u.op.left->pos, "can not compare different types.");
			return expTy(NULL, Ty_Int());
		}
		default: assert(0);
	}
}

static struct expty handle_recordExp(S_table venv, S_table tenv, A_exp a)
{
	struct expty exp = { NULL };
	if (rescan) return exp;
	Ty_ty x = S_look(tenv, a->u.record.typ);
	if (!x)
		EM_error(a->pos, "undefined record type \"%s\".\n", S_name(a->u.record.typ));
	if (!x->u.record)
		EM_error(a->pos, "a record type expected.\n");
	Ty_fieldList f;
	A_efieldList e;
	for (e = a->u.record.fields, f = x->u.record; e && f; e = e->tail, f = f->tail) {
		if (e->head->name != f->head->name)
			EM_error(a->pos, "record field name mismatch.\n");
		struct expty val = transExp(venv, tenv, e->head->exp);
		if (actual_ty(val.ty)->kind != Ty_nil &&
			actual_ty(val.ty)->kind != actual_ty(f->head->ty)->kind)
			EM_error(a->pos, "record field type mismatch.\n");
		if (actual_ty(val.ty)->kind == Ty_nil &&
			actual_ty(f->head->ty)->kind != Ty_record)
			EM_error(a->pos, "nil can only be assigned to record type.\n");
	}
	if (f || e)
		EM_error(a->pos, "record length mismatch.\n");
	exp.ty = x;
	return exp;
}

static struct expty handle_seqExp(S_table venv, S_table tenv, A_exp a)
{
	struct expty exp = { NULL };
	A_expList e;
	if (!a->u.seq) {
		exp.ty = Ty_Void();
		return exp;
	}
	for (e = a->u.seq; e; e = e->tail)
		exp = transExp(venv, tenv, e->head);
	return exp;
}

static struct expty handle_assignExp(S_table venv, S_table tenv, A_exp a)
{
	if (infor && a->u.assign.var->u.simple == for_idx)
		EM_error(a->u.assign.var->pos, "Cannot modify for loop index.\n");
	struct expty exp = { NULL };
	struct expty tmp = { NULL };
	tmp = transExp(venv, tenv, a->u.assign.exp);
	exp = transVar(venv, tenv, a->u.assign.var);
	if (actual_ty(exp.ty)->kind != actual_ty(tmp.ty)->kind)
		EM_error(a->pos, "assignment type mismatch.\n");
	return exp;
}

static struct expty handle_ifExp(S_table venv, S_table tenv, A_exp a)
{
	struct expty condition = { NULL };
	struct expty then = { NULL };
	struct expty elsee = { NULL };
	condition = transExp(venv, tenv, a->u.iff.test);
	if (actual_ty(condition.ty)->kind != Ty_int)
		EM_error(a->u.iff.test->pos, "if confition must be integer type.\n");
	then = transExp(venv, tenv, a->u.iff.then);
	if (a->u.iff.elsee) {
		elsee = transExp(venv, tenv, a->u.iff.elsee);
		if (actual_ty(then.ty)->kind == Ty_Void && actual_ty(elsee.ty)->kind == Ty_Void)
			return then;
		if (actual_ty(then.ty)->kind != actual_ty(elsee.ty)->kind)
			EM_error(a->u.iff.elsee->pos, "if branches type mismatch.\n");
	}
	else {
		if (actual_ty(then.ty)->kind != Ty_Void)
			EM_error(a->u.iff.then->pos, "No value should be returned.\n");
	}
	return then;
}

static struct expty handle_whileExp(S_table venv, S_table tenv, A_exp a)
{
	struct expty condition = { NULL };
	struct expty doo = { NULL };
	condition = transExp(venv, tenv, a->u.whilee.test);
	if (actual_ty(condition.ty)->kind != Ty_int)
		EM_error(a->u.whilee.test->pos, "while confition must be integer type.\n");
	doo = transExp(venv, tenv, a->u.whilee.body);
	if (actual_ty(doo.ty)->kind != Ty_void)
		EM_error(a->u.whilee.body->pos, "while loop cannot return value.\n");
	return doo;
}

static struct expty handle_forExp(S_table venv, S_table tenv, A_exp a)
{
	struct expty low = { NULL };
	struct expty high = { NULL };
	struct expty body = { NULL };
	low = transExp(venv, tenv, a->u.forr.lo);
	high = transExp(venv, tenv, a->u.forr.hi);
	if (actual_ty(low.ty)->kind != Ty_int)
		EM_error(a->u.forr.lo->pos, "for loop low range must be integer type.\n");
	if (actual_ty(high.ty)->kind != Ty_int)
		EM_error(a->u.forr.hi->pos, "for loop high range must be integer type.\n");
	S_beginScope(venv);
	S_enter(venv, a->u.forr.var, E_VarEntry(Ty_Int()));
	infor = 1;
	for_idx = a->u.forr.var;
	body = transExp(venv, tenv, a->u.forr.body);
	infor = 0;
	if (actual_ty(body.ty)->kind != Ty_void)
		EM_error(a->u.forr.body->pos, "for loop cannot return value.\n");
	S_endScope(venv);
	return body;
}

static struct expty handle_letExp(S_table venv, S_table tenv, A_exp a)
{
	struct expty exp;
	A_decList d;
	S_beginScope(venv);
	S_beginScope(tenv);
	rescan = 1;
	for (d = a->u.let.decs; d; d = d->tail)
		transDec(venv, tenv, d->head);
	rescan = 0;
	for (d = a->u.let.decs; d; d = d->tail)
		transDec(venv, tenv, d->head);
	A_expList e;
	for(e = a->u.let.body; e; e = e->tail)
	    exp = transExp(venv, tenv, e->head);
	S_endScope(tenv);
	S_endScope(venv);
	return exp;
}

static struct expty handle_arrayExp(S_table venv, S_table tenv, A_exp a)
{
	struct expty exp = { NULL };
	if (rescan) return exp;
	Ty_ty x = S_look(tenv, a->u.array.typ);
	if (!x)
		EM_error(a->pos, "undefined type \"%s\".\n", S_name(a->u.array.typ));
	if (!x->u.array)
		EM_error(a->pos, "an array type expected.\n");
	struct expty size = transExp(venv, tenv, a->u.array.size);
	if (actual_ty(size.ty)->kind != Ty_int)
		EM_error(a->pos, "array index must be integers.");
	struct expty init = transExp(venv, tenv, a->u.array.init);
	if (actual_ty(init.ty)->kind != actual_ty(x->u.array)->kind)
		EM_error(a->pos, "array initializer must be of the same type as array.");
	exp.ty = x;
	return exp;
}

static struct expty transExp(S_table venv, S_table tenv, A_exp a) {
	switch (a->kind) {
		case A_varExp: return transVar(venv, tenv, a->u.var);
		case A_nilExp: return (struct expty) { NULL, Ty_Nil() };
		case A_intExp: return handle_intExp(venv, tenv, a);
		case A_stringExp: return handle_stringExp(venv, tenv, a);
		case A_callExp: return handle_callExp(venv, tenv, a);
		case A_opExp: return handle_opExp(venv, tenv, a);
		case A_recordExp: return handle_recordExp(venv, tenv, a);
		case A_seqExp: return handle_seqExp(venv, tenv, a);
		case A_assignExp: return handle_assignExp(venv, tenv, a);
		case A_ifExp: return handle_ifExp(venv, tenv, a);
		case A_whileExp: return handle_whileExp(venv, tenv, a);
		case A_forExp: return handle_forExp(venv, tenv, a);
		case A_breakExp: assert(0); break;
		case A_letExp: return handle_letExp(venv, tenv, a);
		case A_arrayExp: return handle_arrayExp(venv, tenv, a);
		default: assert(0);
	}
}

static struct expty transSimpleVar(S_table venv, S_table tenv, A_var v)
{
	E_enventry x = S_look(venv, v->u.simple);
	if (!x)
		EM_error(v->pos, "undefined variable \"%s\".\n", S_name(v->u.simple));
	if (x && x->kind == E_varEntry)
		return expTy(NULL, actual_ty(x->u.var.ty));
	else
		EM_error(v->pos, "undefined variable \"%s\".\n", S_name(v->u.simple));
		//return expTy(NULL, Ty_Int());
}

static struct expty transSubscriptVar(S_table venv, S_table tenv, A_var v)
{
	assert(0);
}

static struct expty transFieldVar(S_table venv, S_table tenv, A_var v)
{
	E_enventry x = S_look(venv, v->u.field.var->u.simple);
	if (!x)
		EM_error(v->pos, "undefined variable \"%s\".\n", S_name(v->u.field.var->u.simple));
	S_symbol sym = v->u.field.sym;
	Ty_fieldList f;
	for (f = x->u.var.ty->u.record; f; f = f->tail) {
		if (f->head->name != sym) continue;
		break;
	}
	if (!f)
		EM_error(v->pos, "undefined field name \"%s\".\n", S_name(sym));
	return expTy(NULL, actual_ty(f->head->ty));
}

static struct expty transVar(S_table venv, S_table tenv, A_var v)
{
	switch (v->kind) {
		case A_simpleVar: transSimpleVar(venv, tenv, v); break;
		case A_fieldVar: transFieldVar(venv, tenv, v); break;
		case A_subscriptVar:transSubscriptVar(venv, tenv, v); break;
		default: assert(0);
	}
}

static void transVarDec(S_table venv, S_table tenv, A_dec d)
{
	if (rescan) return;
	Ty_ty x = S_look(tenv, d->u.var.typ);
	if (S_name(d->u.var.typ) != "NULL" && !x)
		EM_error(d->pos, "undefined type \"%s\".\n", S_name(d->u.var.typ));
	struct expty e = transExp(venv, tenv, d->u.var.init);
	if (S_name(d->u.var.typ) != "NULL" && (
		actual_ty(x)->kind != actual_ty(e.ty)->kind ||
		actual_ty(x)->kind == Ty_record &&
		d->u.var.typ != d->u.var.init->u.record.typ))
		EM_error(d->u.var.init->pos, "var declaration type mismatch.\n");
	S_enter(venv, d->u.var.var, E_VarEntry(e.ty));
}

static void transTypeDec(S_table venv, S_table tenv, A_dec d)
{
	A_nametyList a;
	for (a = d->u.type; a; a = a->tail) {
		S_enter(tenv, a->head->name, transTy(tenv, a->head->ty));
		if (!rescan)
			actual_ty(S_look(tenv, a->head->name));
	}
}

static Ty_tyList makeFormals(S_table tenv, A_fieldList p)
{
	A_fieldList f;
	A_fieldList stk[MAX_PARAM]; // TO DO: a generic stack
	Ty_ty x;
	Ty_ty stkx[MAX_PARAM]; // TO DO: a generic stack
	Ty_tyList tytylist = NULL;
	int i = 0, j;
	for (f = p; f; f = f->tail) {
		x = S_look(tenv, f->head->typ);
		if (!x)
			EM_error(f->head->pos, "undefined type \"%s\" in function parameters.\n", S_name(f->head->typ));
		stkx[i] = x;
		stk[i++] = f;
	}
	for (j = i - 1; j >= 0; --j)
		tytylist = Ty_TyList(stkx[j], tytylist);
	return tytylist;
}

static void transFormalDec(S_table venv, S_table tenv, A_fieldList params)
{
	A_fieldList f;
	for (f = params; f; f = f->tail) {
		Ty_ty x = S_look(tenv, f->head->typ);
		if (!x)
			EM_error(params->head->pos, "undefined type \"%s\".\n", S_name(f->head->typ));
		S_enter(venv, f->head->name, E_VarEntry(x));
	}
}

static void transFunctionDec(S_table venv, S_table tenv, A_dec d)
{
	A_fundecList f;
	struct expty exp;
	for (f = d->u.function; f; f = f->tail) {
		Ty_ty x = NULL;
		if (S_name(f->head->result) != "NULL") {
			x = S_look(tenv, f->head->result);
			if (!x)
				EM_error(f->head->pos, "undefined function return type \"%s\".\n", S_name(f->head->result));
		}
		else
			x = Ty_Void();
		S_enter(venv, 
			    f->head->name, 
			    E_FunEntry(makeFormals(tenv, f->head->params), x)
		);
		S_beginScope(venv);
		transFormalDec(venv, tenv, f->head->params);
		if (!rescan) {
			exp = transExp(venv, tenv, f->head->body);
			if (actual_ty(exp.ty)->kind != actual_ty(x)->kind)
				EM_error(f->head->body->pos, "function return type mismatch.\n");
		}
		S_endScope(venv);
	}
}

static void transDec(S_table venv, S_table tenv, A_dec d)
{
	switch (d->kind) {
		case A_varDec: transVarDec(venv, tenv, d); break;
		case A_typeDec: transTypeDec(venv, tenv, d); break;
		case A_functionDec: transFunctionDec(venv, tenv, d); break;
		default: assert(0);
	}
}

static Ty_ty transNameTy(S_table tenv, A_ty a)
{
	Ty_ty x = S_look(tenv, a->u.name);
	if (!rescan && !x)
		EM_error(a->pos, "undefined type \"%s\".\n", S_name(a->u.name));
	return Ty_Name(a->u.name, x);
}

static Ty_ty transRecordTy(S_table tenv, A_ty a)
{
	A_fieldList f;
	A_fieldList stk[MAX_PARAM]; // TO DO: a generic stack
	Ty_ty x;
	Ty_ty stkx[MAX_PARAM]; // TO DO: a generic stack
	Ty_fieldList tyfdlist = NULL;
	int i = 0, j;
	for (f = a->u.record; f; f = f->tail) {
		x = S_look(tenv, f->head->typ);
		if (!rescan && !x)
			EM_error(a->pos, "undefined type \"%s\".\n", S_name(f->head->typ));
		stkx[i] = x;
		stk[i++] = f;
	}
	for (j = i - 1; j >= 0; --j)
		tyfdlist = Ty_FieldList(Ty_Field(stk[j]->head->name, stkx[j]), tyfdlist);
	return Ty_Record(tyfdlist);
}

static Ty_ty transArrayTy(S_table tenv, A_ty a)
{
	Ty_ty x = S_look(tenv, a->u.array);
	if (!x)
		EM_error(a->pos, "undefined type \"%s\".\n", S_name(a->u.array));
	return Ty_Array(x);
}

static Ty_ty transTy(S_table tenv, A_ty a)
{
	switch (a->kind) {
		case A_nameTy: transNameTy(tenv, a); break;
		case A_recordTy: transRecordTy(tenv, a); break;
		case A_arrayTy: transArrayTy(tenv, a); break;
		default: assert(0);
	}
}

void SEM_transProg(A_exp exp)
{
	S_table venv = E_base_venv();
	S_table tenv = E_base_tenv();
	transExp(venv, tenv, exp);
}