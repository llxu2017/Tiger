#include <stdio.h>
#include "semant.h"
#include "errormsg.h"
#include "env.h"

#define MAX_PARAM 256	// Maximum number of fields length, function arguments.

static int infor = 0;
static int loop_depth = 0;
S_symbol for_idx;

struct expty expTy(Tr_exp exp, Ty_ty ty)
{
	struct expty e;
	e.exp = exp;
	e.ty = ty;
	return e;
}

static void check_cyclic_ty(int pos, S_symbol sym, Ty_ty ty)
{
	while (ty->kind == Ty_name) {
		ty = ty->u.name.ty;
		if (!ty) {
			EM_error(pos, "cyclic type declarations.");
			break;
		}
	}
}

static Ty_ty actual_ty(S_table tenv, Ty_ty ty)
{
	if (ty->kind == Ty_name && !ty->u.name.ty)
		ty = S_look(tenv, ty->u.name.sym);
	if (!ty) return ty;
	while (ty->kind == Ty_name)
		ty = ty->u.name.ty;
	return ty;
}

static bool validate_ty(S_table tenv, Ty_ty ty1, Ty_ty ty2)
{
	if (ty1 && ty2->kind == Ty_nil ||
		ty2 && ty1->kind == Ty_nil)
		return TRUE;
	return actual_ty(tenv, ty1) == actual_ty(tenv, ty2);
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
	struct expty exp = { NULL, NULL };
	A_expList e = NULL;
	E_enventry x = S_look(venv, a->u.call.func);
	if (!x) {
		EM_error(a->pos, "function \"%s\" not defined.", S_name(a->u.call.func));
		return exp;
	}
	if (x->kind != E_funEntry) {
		EM_error(a->pos, "variable not bound to a function.");
		return exp;
	}
	Ty_tyList t = NULL;
	for (e = a->u.call.args, t = x->u.fun.formals; e && t; e = e->tail, t = t->tail) {
		exp = transExp(venv, tenv, e->head);
		if (!validate_ty(tenv, exp.ty, t->head))
			EM_error(a->pos, "function arguments type mismatch.");
	}
	if (e || t)
		EM_error(a->pos, "number of function arguments do not match.");
	exp.ty = x->u.fun.result;
	return exp;
}

static struct expty handle_opExp(S_table venv, S_table tenv, A_exp a)
{
	A_oper oper = a->u.op.oper;
	struct expty left = transExp(venv, tenv, a->u.op.left);
	struct expty right = transExp(venv, tenv, a->u.op.right);
	if (actual_ty(tenv, left.ty)->kind == Ty_nil && actual_ty(tenv, left.ty)->kind == Ty_nil)
		EM_error(a->u.op.right->pos, "two operands with unknown types.");
	switch (oper) {
	case A_plusOp:
	case A_minusOp:
	case A_timesOp:
	case A_divideOp: {
		if (!validate_ty(tenv, left.ty, Ty_Int()))
			EM_error(a->u.op.left->pos, "integer required.");
		if (!validate_ty(tenv, right.ty, Ty_Int()))
			EM_error(a->u.op.right->pos, "integer required.");
		return expTy(NULL, Ty_Int());
	}
	case A_eqOp:
	case A_neqOp:
	case A_ltOp:
	case A_gtOp:
	case A_geOp:
	case A_leOp: {
		if (!validate_ty(tenv, left.ty, right.ty))
			EM_error(a->u.op.left->pos, "can not compare different types.");
		return expTy(NULL, Ty_Int());
	}
	default: assert(0);
	}
}

static struct expty handle_recordExp(S_table venv, S_table tenv, A_exp a)
{
	struct expty exp = { NULL };
	Ty_ty x = S_look(tenv, a->u.record.typ);
	if (x) x = actual_ty(tenv, x);
	exp.ty = x;
	if (!x) {
		EM_error(a->pos, "undefined record type \"%s\".", S_name(a->u.record.typ));
		exp.ty = Ty_Void(); // Error recovery only
		return exp;
	}
	if (!x->u.record) {
		EM_error(a->pos, "a record type expected.");
		return exp;
	}
	Ty_fieldList f = NULL;
	A_efieldList e = NULL;
	for (e = a->u.record.fields, f = x->u.record; e && f; e = e->tail, f = f->tail) {
		if (e->head->name != f->head->name)
			EM_error(a->pos, "record field name mismatch.");
		struct expty val = transExp(venv, tenv, e->head->exp);
		if (!validate_ty(tenv, val.ty, Ty_Nil()) && !validate_ty(tenv, val.ty, f->head->ty))
			EM_error(a->pos, "record field type mismatch.");
		if (validate_ty(tenv, val.ty, Ty_Nil()) && actual_ty(tenv, f->head->ty)->kind != Ty_record)
			EM_error(a->pos, "nil can only be assigned to record type.");
	}
	if (f || e)
		EM_error(a->pos, "record length mismatch.");
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
		EM_error(a->u.assign.var->pos, "cannot modify for loop index.");
	struct expty exp = { NULL };
	struct expty tmp = { NULL };
	tmp = transExp(venv, tenv, a->u.assign.exp);
	exp = transVar(venv, tenv, a->u.assign.var);
	if (exp.ty && !validate_ty(tenv, exp.ty, tmp.ty))
		EM_error(a->pos, "assignment type mismatch.");
	exp.ty = Ty_Void(); // Assignment produce no values
	return exp;
}

static struct expty handle_ifExp(S_table venv, S_table tenv, A_exp a)
{
	struct expty condition = { NULL };
	struct expty then = { NULL };
	struct expty elsee = { NULL };
	condition = transExp(venv, tenv, a->u.iff.test);
	if (!validate_ty(tenv, condition.ty, Ty_Int()))
		EM_error(a->u.iff.test->pos, "if test must be integer type.");
	then = transExp(venv, tenv, a->u.iff.then);
	if (a->u.iff.elsee) {
		elsee = transExp(venv, tenv, a->u.iff.elsee);
		if (validate_ty(tenv, then.ty, Ty_Void()) && validate_ty(tenv, elsee.ty, Ty_Void()))
			return then;
		if (!validate_ty(tenv, then.ty, elsee.ty))
			EM_error(a->u.iff.elsee->pos, "if branches type mismatch.");
	}
	else {
		if (!validate_ty(tenv, then.ty, Ty_Void()))
			EM_error(a->u.iff.then->pos, "no value should be returned.");
	}
	return then;
}

static struct expty handle_whileExp(S_table venv, S_table tenv, A_exp a)
{
	struct expty condition = { NULL };
	struct expty doo = { NULL };
	condition = transExp(venv, tenv, a->u.whilee.test);
	if (!validate_ty(tenv, condition.ty, Ty_Int()))
		EM_error(a->u.whilee.test->pos, "while test must be integer type.");
	++loop_depth;
	doo = transExp(venv, tenv, a->u.whilee.body);
	--loop_depth;
	if (!validate_ty(tenv, doo.ty, Ty_Void()))
		EM_error(a->u.whilee.body->pos, "while loop cannot return value.");
	return doo;
}

static struct expty handle_forExp(S_table venv, S_table tenv, A_exp a)
{
	struct expty low = { NULL };
	struct expty high = { NULL };
	struct expty body = { NULL };
	low = transExp(venv, tenv, a->u.forr.lo);
	high = transExp(venv, tenv, a->u.forr.hi);
	if (!validate_ty(tenv, low.ty, Ty_Int()))
		EM_error(a->u.forr.lo->pos, "for loop low range must be integer type.");
	if (!validate_ty(tenv, high.ty, Ty_Int()))
		EM_error(a->u.forr.hi->pos, "for loop high range must be integer type.");
	S_beginScope(venv);
	S_enter(venv, a->u.forr.var, E_VarEntry(Ty_Int()));
	infor = 1;
	for_idx = a->u.forr.var;
	++loop_depth;
	body = transExp(venv, tenv, a->u.forr.body);
	--loop_depth;
	infor = 0;
	if (!validate_ty(tenv, body.ty, Ty_Void()))
		EM_error(a->u.forr.body->pos, "for loop cannot return value.");
	S_endScope(venv);
	return body;
}

static struct expty handle_breakExp(S_table venv, S_table tenv, A_exp a)
{
	assert(loop_depth >= 0);
	if (!loop_depth)
		EM_error(a->pos, "break can only be used in while or for loops.");
	return (struct expty) { NULL, Ty_Void() };
}

static struct expty handle_letExp(S_table venv, S_table tenv, A_exp a)
{
	struct expty exp = { NULL, NULL };
	A_decList d;
	S_beginScope(venv);
	S_beginScope(tenv);
	for (d = a->u.let.decs; d; d = d->tail)
		transDec(venv, tenv, d->head);
	A_expList e;
	for (e = a->u.let.body; e; e = e->tail)
		exp = transExp(venv, tenv, e->head);
	S_endScope(tenv);
	S_endScope(venv);
	return exp;
}

static struct expty handle_arrayExp(S_table venv, S_table tenv, A_exp a)
{
	struct expty exp = { NULL };
	Ty_ty x = S_look(tenv, a->u.array.typ);
	if (!x)
		EM_error(a->pos, "undefined type \"%s\".", S_name(a->u.array.typ));
	if (!actual_ty(tenv, x)->u.array)
		EM_error(a->pos, "an array type expected.");
	struct expty size = transExp(venv, tenv, a->u.array.size);
	if (!validate_ty(tenv, size.ty, Ty_Int()))
		EM_error(a->pos, "array index must be integers.");
	struct expty init = transExp(venv, tenv, a->u.array.init);
	if (!validate_ty(tenv, init.ty, actual_ty(tenv, x)->u.array))
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
	case A_breakExp: return handle_breakExp(venv, tenv, a);
	case A_letExp: return handle_letExp(venv, tenv, a);
	case A_arrayExp: return handle_arrayExp(venv, tenv, a);
	default: assert(0);
	}
}

static struct expty transSimpleVar(S_table venv, S_table tenv, A_var v)
{
	E_enventry x = S_look(venv, v->u.simple);
	if (!x)
		EM_error(v->pos, "undefined variable \"%s\".", S_name(v->u.simple));
	if (x && x->kind == E_varEntry)
		return expTy(NULL, actual_ty(tenv, x->u.var.ty));
	return expTy(NULL, Ty_Void); // Error recovery only
}

static struct expty transSubscriptVar(S_table venv, S_table tenv, A_var v)
{
	struct expty exp = { NULL };
	exp = transExp(venv, tenv, v->u.subscript.exp);
	if (!validate_ty(tenv, exp.ty, Ty_Int())) {
		EM_error(v->pos, "array index must be integer type.");
		return expTy(NULL, NULL);
	}
	exp = transVar(venv, tenv, v->u.subscript.var);
	if (actual_ty(tenv, exp.ty)->kind != Ty_array) {
		EM_error(v->pos, "not an array.");
		return expTy(NULL, NULL);
	}
	exp.ty = exp.ty->u.array;
	return exp;
}

static struct expty transFieldVar(S_table venv, S_table tenv, A_var v)
{
	struct expty exp = { NULL };
	exp = transVar(venv, tenv, v->u.field.var);
	if (actual_ty(tenv, exp.ty)->kind != Ty_record) {
		EM_error(v->pos, "not a record.");
		return expTy(NULL, NULL);
	}
	S_symbol sym = v->u.field.sym;
	Ty_fieldList f;
	for (f = exp.ty->u.record; f; f = f->tail) {
		if (f->head->name != sym) continue;
		break;
	}
	if (!f) {
		EM_error(v->pos, "undefined field name \"%s\".", S_name(sym));
		return expTy(NULL, NULL);
	}
	return expTy(NULL, actual_ty(tenv, f->head->ty));
}

static struct expty transVar(S_table venv, S_table tenv, A_var v)
{
	switch (v->kind) {
	case A_simpleVar: return transSimpleVar(venv, tenv, v);
	case A_fieldVar: return transFieldVar(venv, tenv, v);
	case A_subscriptVar: return transSubscriptVar(venv, tenv, v);
	default: assert(0);
	}
}

static void transVarDec(S_table venv, S_table tenv, A_dec d)
{
	Ty_ty x = S_look(tenv, d->u.var.typ);
	if (S_name(d->u.var.typ) != "NULL" && !x)
		EM_error(d->pos, "undefined type \"%s\".", S_name(d->u.var.typ));
	struct expty e = transExp(venv, tenv, d->u.var.init);
	if (S_name(d->u.var.typ) != "NULL" && !validate_ty(tenv, x, e.ty))
		EM_error(d->u.var.init->pos, "var declaration type mismatch.");
	if (S_name(d->u.var.typ) == "NULL" && e.ty->kind == Ty_nil)
		EM_error(d->u.var.init->pos, "nil must has explicit type.");
	if (S_name(d->u.var.typ) != "NULL" && e.ty->kind == Ty_nil)
		S_enter(venv, d->u.var.var, E_VarEntry(x));
	else
		S_enter(venv, d->u.var.var, E_VarEntry(e.ty));
}

static void transTypeDec(S_table venv, S_table tenv, A_dec d)
{
	A_nametyList a;
	S_table tmpenv = S_empty();
	for (a = d->u.type; a; a = a->tail)
		S_enter(tenv, a->head->name, Ty_Name(a->head->name, NULL));
	for (a = d->u.type; a; a = a->tail) {
		if (S_look(tmpenv, a->head->name))
			EM_error(d->pos, "type alias already exists in this declaration batch.");
		S_enter(tenv, a->head->name, transTy(tenv, a->head->ty));
		S_enter(tmpenv, a->head->name, S_look(tenv, a->head->name));
	}
	for (a = d->u.type; a; a = a->tail)
		check_cyclic_ty(d->pos, a->head->name, S_look(tenv, a->head->name));
}

static Ty_tyList makeFormals(S_table tenv, A_fieldList p)
{
	//if (!p) return Ty_TyList(Ty_Void(), NULL);
	A_fieldList f, stk[MAX_PARAM]; // TO DO: a generic stack
	Ty_ty x, stkx[MAX_PARAM]; // TO DO: a generic stack
	Ty_tyList tytylist = NULL;
	int i = 0, j;
	for (f = p; f; f = f->tail) {
		x = S_look(tenv, f->head->typ);
		if (!x) EM_error(f->head->pos, "undefined type \"%s\" in function parameters.", S_name(f->head->typ));
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
			EM_error(params->head->pos, "undefined type \"%s\".", S_name(f->head->typ));
		S_enter(venv, f->head->name, E_VarEntry(x));
	}
}

static void transFunctionDec(S_table venv, S_table tenv, A_dec d)
{
	A_fundecList f;
	struct expty exp;
	S_table tmpenv = S_empty();
	for (f = d->u.function; f; f = f->tail) {
		Ty_ty x = NULL;
		if (S_name(f->head->result) != "NULL") {
			x = S_look(tenv, f->head->result);
			if (!x)
				EM_error(f->head->pos, "undefined function return type \"%s\".", S_name(f->head->result));
		}
		else
			x = Ty_Void();
		S_enter(venv,
			f->head->name,
			E_FunEntry(makeFormals(tenv, f->head->params), x)
		);
	}
	for (f = d->u.function; f; f = f->tail) {
		Ty_ty x = NULL;
		if (S_name(f->head->result) != "NULL") {
			x = S_look(tenv, f->head->result);
			if (!x)
				EM_error(f->head->pos, "undefined function return type \"%s\".", S_name(f->head->result));
		}
		else
			x = Ty_Void();
		if (S_look(tmpenv, f->head->name))
			EM_error(d->pos, "function name already exits.");
		S_enter(venv,
			f->head->name,
			E_FunEntry(makeFormals(tenv, f->head->params), x)
		);
		S_enter(tmpenv,
			f->head->name,
			S_look(venv, f->head->name)
		);
		S_beginScope(venv);
		transFormalDec(venv, tenv, f->head->params);
		exp = transExp(venv, tenv, f->head->body);
		if (!validate_ty(tenv, exp.ty, x))
			EM_error(f->head->body->pos, "function return type mismatch.");
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
	if (!x)
		EM_error(a->pos, "undefined type \"%s\".", S_name(a->u.name));
	return Ty_Name(a->u.name, x);
}

static Ty_ty transRecordTy(S_table tenv, A_ty a)
{
	A_fieldList f, stk[MAX_PARAM]; // TO DO: a generic stack
	Ty_ty x, stkx[MAX_PARAM]; // TO DO: a generic stack
	Ty_fieldList tyfdlist = NULL;
	int i = 0, j;
	for (f = a->u.record; f; f = f->tail) {
		x = S_look(tenv, f->head->typ);
		if (!x) EM_error(a->pos, "undefined type \"%s\".", S_name(f->head->typ));
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
		EM_error(a->pos, "undefined type \"%s\".", S_name(a->u.array));
	return Ty_Array(x);
}

static Ty_ty transTy(S_table tenv, A_ty a)
{
	switch (a->kind) {
	case A_nameTy: return transNameTy(tenv, a);
	case A_recordTy: return transRecordTy(tenv, a);
	case A_arrayTy: return transArrayTy(tenv, a);
	default: assert(0); return NULL;
	}
}

void SEM_transProg(A_exp exp)
{
	S_table venv = E_base_venv();
	S_table tenv = E_base_tenv();
	transExp(venv, tenv, exp);
}