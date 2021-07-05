% Privacy module

:- module(priv,[
	       ]).

:- use_module(dctg).
:- use_module(s4p).
:- use_module(ui).
:- use_module(test).

:- include('TEST/priv_test').

:- op( 100,  yfx, user:(^^) ).
:- op( 1178, xfx, user:(::=) ).
:- op( 1179, xfx, user:(<:>) ).
:- op( 1178, yfx, user:(&&) ).
:- op( 1177, xfx, user:(::-) ).
:- op( 1176, xfx, user:(from) ).

:- dynamic priv_initialized/1.

priv_initialized(false).

%init :- param:initialized(true), !. % already initialized

init :- % priv_initialized(false), !,
	sp_tokens(Trules), sp_lex(Lrules), sp_constant(Crules),	sp_fact(Frules), sp_ac(ACrules),
	sp_constraint(ConstrRules), sp_conditional_facts(CFrules),
	%	sp_punct(Prules),%	sp_e(Erules),%	sp_identifier(Irules),
	append([Trules,Lrules,Crules,Frules,ConstrRules,CFrules,ACrules], Rules),
	dctg:dctg_list_reconsult(Rules),
	retractall( priv_initialized(_) ), assert( priv_initialized(true) ), !.
init.

