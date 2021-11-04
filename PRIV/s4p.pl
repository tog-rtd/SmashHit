% S4P Privacy Language

:- module(s4p,[id_token/2, sp_tokens/1, sp_lex/1, sp_constant/1, sp_fact/1, sp_ac/1, %sp_identifier/1,
	       sp_grammar/1, sp_constraint/1, sp_conditional_facts/1,
	       infix_ops/1
	       ]).

:- use_module(dctg).

:- dynamic s4p_initialized/1.

:- op( 100,  yfx, user:(^^) ).
:- op( 1178, xfx, user:(::=) ).
:- op( 1179, xfx, user:(<:>) ).
:- op( 1178, yfx, user:(&&) ).
:- op( 1177, xfx, user:(::-) ).
:- op( 1176, xfx, user:(from) ).

s4p_initialized(false).

init:- param:initialized(true), !. % already initialized
init :- % put other initialization actions here
	retractall( s4p_initialized(_) ), assert( s4p_initialized(true) ).

re_init :- un_init, init.
un_init :- retractall( s4p_initialized(_) ), assert( s4p_initialized(false) ).

% LEXICAL
% VOCABULARY

id_token(W,Token) :- name(X,W), token(X,Token).

token(X,Token) :- reserved(X,Token), !.
token(X,id(X)).

reserved(if, if).
reserved(can, can).
reserved(say_infinity, say_infinity).
reserved(say_inf, say_infinity).
reserved(say_zero, say_zero).
reserved(say_0, say_zero).
reserved(say, say).
reserved(says_k, says_k).
reserved(says, says).
reserved(is, is).
reserved(is_a, is_a).
reserved(act, act).
reserved(as, as).
reserved(inf, inf).
reserved(nul, nul).
reserved(file, file).
reserved(read, read).
reserved(has, has).
reserved(till, till).
reserved(access, access).
reserved(from, from).
reserved(where, where).
reserved(constr_placeholder, constr_placeholder).
reserved(cfact_placeholder, cfact_placeholder).

infix_ops( [says, can, say_zero, say_infinity, act, as, read, has, access, from] ).

sp_tokens([
    (	tLPAREN ::= ['('] ),
    (	tRPAREN ::= [')'] ),
    (	tLE ::= ['<='] ),
    (	tLT ::= ['<'] ),
    (	tGE ::= ['>='] ),
    (	tGT ::= ['>'] ),
    (	tPSEP ::= ['://'] ),
    (	tIF ::= [if] ),
    (	tCAN ::= [can] ),
    (	tSAY ::= [say] ),
    (	tACT ::= [act] ),
    (	tAS ::= [as] ),
    (	tINF ::= [inf] ),
    (	tNUL ::= [nul] ),
    (	tFILE ::= [file] ),
    (	tREAD ::= [read] ),
    (	tHAS ::= [has] ),
    (	tTILL ::= [till] ),
    (	tACCESS ::= [access] ),
    (	tFROM ::= [from] ),
    (	tCANSAY ::= tCAN, tSAY, delegation_flag ),
    (	tCANACTAS ::= tCAN, tACT, tAS ),
    (	tCANREAD ::= tCAN, tREAD ),
    (	tHASACCESSFROM ::= tHAS, tACCESS, tFROM ),
    (	tIDENT ::= [id(Id)] <:> prefix(Id) )
]).

%sp_punct([
%	( punctuation(';') ::= ";" )
%]).

sp_lex([
    (	uletter(L) ::= [L], {is_upper(L)}, !  <:> sp(L)    ),
    (	letter(L) ::= [L], {is_letter(L)}  <:> sp(L)    ),
    (	digit(D) ::= [D], {is_digit(D)}    <:> sp(D)    ),
    (	lparen(lparen) ::= [C], {char_code(C,X), X=:=40}    <:> sp(C)    ),
    (	rparen(rparen) ::= [C], {char_code(C,X), X=:=41}    <:> sp(C)    ),
    (	underscore(C) ::= [C], {char_code(C,X), X=:=95}    <:> sp(C)    ),
    (	semicolon(semic) ::= [C], {char_code(C,X), X=:=59}    <:> sp(C)    ),
    (	colon(colon) ::= [C], {char_code(C,X), X=:=58}    <:> sp(C)    ),
    (	fullstop(fstop) ::= [C], {char_code(C,X), X=:=46}    <:> sp(C)    ),
    (	comma(comma) ::= [C], {char_code(C,X), X=:=44}    <:> sp(C)    ),
    (	slash(slash) ::= [C], {char_code(C,X), X=:=47}    <:> sp(C)    ),
    (	lords([L|Ls]) ::= letter(L), lords(Ls)    <:> sp([])    ),
    (	lords([L|Ls]) ::= digit(L), lords(Ls)    <:> sp([])    ),
    (	lords([L|Ls]) ::= underscore(L), lords(Ls)    <:> sp([])    ),
    (	lords([]) ::= []    ),
    (	word([L|Ls]) ::= letter(L), lords(Ls)    ),
    (	spaces ::= space, spaces    ),
    (	spaces ::= []    ),
    (	space ::= [C], {char_code(C,X), X=:=32}    ),
    (	space ::= [C], {char_code(C,X), X=:=10}    ),
    (	space ::= [C], {char_code(C,X), X=:=9}    ),

    (	lexemes(L) ::= spaces, lexeme_list(L)  ),
    (	lexeme_list([L|Ls]) ::= lexeme(L), !, spaces, lexeme_list(Ls)    ),
    (	lexeme_list([]) ::= []    ),

    (	lexeme(Token) ::= word(W), {s4p:id_token(W,Token)} ),
    (	lexeme(fsep) ::= colon(_), slash(_), slash(_) ),
    (	lexeme(Con) ::= lconst(Con) ),
    (	lexeme(C) ::=  slash(C) ),
    (	lexeme(C) ::=  colon(C) ),
    (	lexeme(C) ::=  semicolon(C) ),
    (	lexeme(C) ::=  lparen(C) ),
    (	lexeme(C) ::=  rparen(C) ),
    (	lexeme(C) ::=  comma(C) ),
    (	lexeme(C) ::= fullstop(C) )
    %(	lexeme(Punct) ::= punctuation(Punct) )
    % ...
]).

sp_constant([
    (	lconst(C) ::= num(C)
    <:> sp(C)    ),
    (	variable(V) ::= [id(V)], { s4p:is_variable_name(V) } ),
    (	constant(A) ::= [id(A)] ),
    (	constant(N) ::= [num(N)] ),
    (	constant(F) ::= filename(F) ),

%    (	constant(C) ::= word(C)
%    <:> sp(C)    ),
%    (	constant(C) ::= tFILE
%    <:> sp(C)    ),

    (	num(num(N)) ::= number(Number), { name(N,Number) }
    <:> sp(N)    ),
    (	number([D|Ds]) ::= digit(D), digits(Ds)
    <:> sp(N)    ),
    (	digits([D|Ds]) ::= digit(D), digits(Ds)
    <:> sp(D)    ),
    (	digits([]) ::= []
    <:> sp(D)    ),

    (	filename(FN) ::= [file], [fsep], pathname(PN),
	{  atomic_list_concat(['file','://',PN],FN) }   ),
    (	pathname(Pn) ::= [id(C)], ['/'], pathname(Cs),
	{ atomic_list_concat([C,'/',Cs], Pn) }
    ),
    (	pathname(C) ::= [id(C)]
%    <:> sp(C)    ),
%    (	pathname([C|Cs]) ::= nonslashchar(C), nonslashchars(Cs)
%    <:> sp(C)    ),
%    (	nonslashchar(C) ::= [C], {is_letter(C)}
%    <:> sp(C)    ),
%    (	nonslashchars([C|Cs]) ::= nonslashchar(C), nonslashcars(Cs)
    <:> sp(C)    )
]).

sp_fact([
    (	e(V) ::= variable(V), !     ),
    (	e(C) ::= constant(C)     ),

    (	pred(P,A1,An) ::=  [id(P)], [lparen], predargs(A1,An), [rparen]    ),
    (	pred(P,A1,An) ::=  [id(P)], predarg(A), { A1=[A|An] } ),
    (	pred(P,A1,An) ::=  [id(P)], { A1=An, An=[] } ),

    (	pred(is_a,A1,An) ::=  [is_a], predarg(A), { A1=[A|An] } ),
    (	pred(is_a,A1,An) ::=  [is], [id(a)], predarg(A), { A1=[A|An] } ),

    (	pred(can_read,A1,An) ::=  [can], [read], predarg(A),
                                  { A1=[A|An] } ),

    (	pred(has_access_from_till,A1,An) ::=
                                  [has], [access],[from], predarg(From), [till], predarg(Till),
                                  { A1=[From, Till | An] } ),

    (	predarg(A) ::= e(A) ),

    (	predargs(A1,An) ::= predarg(A), [comma], predargs(A2,An), { A1=[A|A2] } ),
    (	predargs([A],[]) ::= predarg(A) ),

    (	verbphrase(F1,Fn,A1,An,K) ::= pred(P,A1,An), { F1=[P|Fn] } ),

    (	verbphrase(F1,Fn,A1,An,K) ::= [can], [act], [as], e(E),
	                              { F1=[can,act,as|Fn], A1=[E|An] } ),

    (	verbphrase(F1,Fn,A1,An,K) ::= [can], [say_infinity], fact(F2,Fn,A1,An,K),
                                      { F1=[can,say_infinity|F2] } ),

    (	verbphrase(F1,Fn,A1,An,K) ::= [can], [say_zero], fact(F2,Fn,A1,An,K),
	                              { F1=[can,say_zero|F2] } ),

    (	verbphrase(F1,Fn,A1,An,K) ::= [can], [say], fact(F2,Fn,A1,An,K),
	                              { F1=[can,say|F2] } ),

%    (	verbphrase(F1,Fn,A1,An,K) ::= [can], [read], filename(F),
%	                            { F1=[can,read|Fn], A1=[F|An] } ),

    (	fact(F1,Fn,A1,An,K) ::= e(E), { A1=[E|A2] }, verbphrase(F1,Fn,A2,An,K)  )
]).

sp_constraint([
    (	opt_constraint([C]) ::= [where], constraint_expression(C) ),
    (	opt_constraint([]) ::= [] ),

    (	constraint_expression(constr_placeholder) ::= [constr_placeholder] )
]).

sp_conditional_facts([
    (	opt_conditional_facts(CCFs) ::= [if], conditional_facts(CFs),
		       { findall(CodedCF,
				(   member(cfact(F1,_Fn,A1,_An,K),CFs),
				    atomic_list_concat(F1,'_',Cff),
				    CodedCF =.. [Cff|A1]
				),
				CCFs)
		       }
    ),
    (	opt_conditional_facts([]) ::= [] ),

    (	conditional_facts( [ cfact(F,[],A,[],_) | CFs ] ) ::=
			  conditional_fact(F,[],A,[],_), [comma], conditional_facts(CFs) ),
    (	conditional_facts( [ cfact(F,[],A,[],_) ] ) ::=	conditional_fact(F,[],A,[],_) ),

    (	conditional_fact(F1,[],[],[],_) ::= [cfact_placeholder], {F1=[cfact_placeholder] } ),
    (	conditional_fact(F1,[],A1,[],K) ::= fact(F1,[],A1,[],K),
		       { true
		       }
    )
]).

sp_ac([
    (	assertion_context([A|As]) ::= assertion(A,0), !, assertion_context(As) ),
    (	assertion_context([]) ::= [] ),

    (	assertion(CodedA,K) ::= { K1 is K+1 },
			issuer(Iss), [says], fact(F1,[],A1,[],K1),
	                opt_conditional_facts(CFs), opt_constraint(Cs), [fstop],
	                { F=[says|F1], HA=[Iss|A1], % gather functor bits and args
	                  atomic_list_concat(F,'_',HF),
	                  compound_name_arguments(CodedH,HF,HA),
			  append(CFs,Cs,Rhs),
			  (   Rhs \= []
			  ->  s4p:list_commas(Rhs,CFgoal),
			      CodedA = (CodedH:-CFgoal)
			  ;   CodedA = CodedH
			  )
			}
    ),
    (	assertion(CodedA,K) ::= { K1 is K+1 },
			issuer(Iss), [says_k], fact(F1,[],A1,[],K1),
	                opt_conditional_facts(CFs), opt_constraint(Cs), [fstop],
	                { F=[says_k|F1], HA=[Iss,K|A1], % gather functor bits and args
	                  atomic_list_concat(F,'_',HF),
	                  compound_name_arguments(CodedH,HF,HA),
			  append(CFs,Cs,Rhs),
			  (   Rhs \= []
			  ->  CodedA = (CodedH:-Rhs)
			  ;   CodedA = CodedH
			  )
			}
    ),

    (	issuer(Iss) ::= [id(Iss)] )
]).

%sp_identifier([
%    (	identifier('Alice') ::= ["Alice"]  <:> sp('Alice')    )
%]).

% convert from list to comma structure
%
list_commas([A], A) :- !.
list_commas([A,B], ','(A,B)) :- !.
list_commas([G|Gs], ','(G,CGs) ) :- list_commas(Gs,CGs).

%  is_variable_name(Name) :- atom_chars(Name,Chars), nth1(1,Chars,Initial), is_upper(Initial).
is_variable_name(Name) :- is_variable(Name).
:- dynamic is_variable/1.
is_variable(x).
is_variable(y).
is_variable(z).

% ========================================================================

sp_reserved('can read', can_read).
sp_reserved('has access from', has_access_from).

sp_assertion([
    (	assertion ::= issuer, ['says'], fact, ['if'], factlist, [','], constraint
        <:> sp(0)
    ),
    (	assertion ::= issuer, ['says'], fact
        <:> sp(0)
    )
]).

sp_grammar([
%    (	e ::= variable(V)
%        <:> sp(V)
%    ),
%    (	e ::= constant(C)
%        <:> sp(C)
%    ),
    (	e ::= identifier(I)
        <:> sp(I)
    ),
    (	e ::= filename(F)
        <:> sp(F)
    ),
%    (	variable('X') ::= ['X']
%        <:> sp('X')
%    ),
%    (	constant ::= ['9']
%        <:> sp('9')
%    ),
%    (	constant ::= ['Alice']
%        <:> sp('Alice')
%    ),
%    (	constant ::= ['researcher']
%        <:>  sp('researcher')
%    ),
    (	constant(C) ::= num(C)
        <:> sp(C)
    ),
    (	constant(C) ::= identifier(C)
        <:> sp(C)
    ),
    (	num(num(N)) ::= number(Number), { name(N,Number) }
        <:> sp(N)
    ),
    (	number([D|Ds]) ::= digit(D), digits(Ds)
        <:> sp(N)
    ),
    (	digit(D) ::= [D], {is_digit(D)}
        <:> sp(D)
    ),
    (	digits([D|Ds]) ::= digit(D), digits(Ds)
        <:> sp(D)
    ),
    (	digits([]) ::= []
        <:> sp(D)
    ),
    (	identifier('project') ::= ['project']
        <:> sp('project')
    ),
    (	identifier('researcher') ::= ['researcher']
        <:> sp('researcher')
    ),
    (	identifier('Alice') ::= ['Alice']
        <:> sp('Alice')
    ),
    (	filename(FN) ::= ['file'], ['://'], pathname(PN)
        <:> sp(FN) ::- atomic_list_concat(['file','://',PN],FN)
    ),
    (	pathname([C|Cs]) ::= nonslashchar(C), nonslashchars(Cs), ['/'], pathname(PN)
        <:> sp(C)
    ),
    (	pathname([C|Cs]) ::= nonslashchar(C), nonslashchars(Cs)
        <:> sp(C)
    ),
    (	nonslashchar(C) ::= [C], {is_letter(C)}
        <:> sp(C)
    ),
    (	nonslashchars([C|Cs]) ::= nonslashchar(C), nonslashcars(Cs)
        <:> sp(C)
    ),
    (	pred ::= ['can read'], hole^^HOLE
        <:> sp(PREDval) ::- HOLE^^sp(HOLEval), atomic_list_concat(['can read',HOLEval],' ',PREDval)
    ),
    (	pred ::= ['has access from'], hole^^H1, ['till'], hole^^H2
        <:> sp(PREDval) ::- H1^^sp(H1val),H2^^sp(H2val),atomic_list_concat(['has access from',H1val,'till',H2val],' ',PREDval)
    ),
    (	hole ::= ['-']
        <:> sp('-')
    ),
    (	delegation_flag ::= ['nul']
        <:> sp('0')
    ),
    (	delegation_flag ::= ['inf']
        <:> sp('inf')
    ),
    (	verbphrase::= pred^^PRED
        <:> sp(VPval) ::- PRED^^sp(VPval)
    ),
    (	verbphrase ::= ['can say'], delegation_flag^^DF, fact^^F
        <:> sp(VPval) ::- DF^^sp(DFval), F^^sp(Fval), atomic_list_concat(['can say',DFval,Fval],' ',VPval)
    ),
    (	verbphrase ::= ['can act as'], e
        <:> sp('can act as')
    ),
    (	pred ::= predid^^PID, predargs^^PAs
        <:> sp(PREDval) ::- PID^^sp(PIDval), PAs^^sp(PAsval), atomic_list_concat([PIDval,PAsval],' ',PREDval)
    ),
    (	predid ::= ['pred']
        <:> sp('pred')
    ),
    (	predid ::= ['is a']
        <:> sp('is a')
    ),
    (	predid ::= ['can read']
        <:> sp('can read')
    ),
    (	predargs ::= []
        <:> sp([])
    ),
    (	predargs ::= predarg^^PA, predargs^^PAs
        <:> sp(PARGSval) ::- PA^^sp(PAval), PAs^^sp(PAsval), atomic_list_concat([PAval,PAsval],' ',PARGSval)
    ),
    (	predarg ::= e^^E
        <:> sp(Eval) ::- E^^sp(Eval)
    ),
    (	fact::= e, verbphrase
        <:> sp(0)
    ),
    (	factlist::= fact, facts
        <:> sp(0)
    ),
    (	facts::= []
        <:> sp(0)
    ),
    (	facts::= [','], fact, facts
        <:> sp(0)
    ),
    (	assertion ::= issuer, ['says'], fact, ['if'], factlist, [','], constraint
        <:> sp(0)
    ),
    (	assertion ::= issuer, ['says'], fact
        <:> sp(0)
    ),
    (	constraint ::= ['$constraint$']
        <:> sp(0)
    ),
    (	issuer ::= ['STS']
        <:> sp(0)
    ),
    (	issuer ::= ['FileServer']
        <:> sp(0)
    )
]).

% S4P
%
% Const - constant symbols
%
% Pred - a set of predicates
%
% BehSymb - predicate symbold for PII-relevant service behaviours
%
% An atom a is a predicate symbol applied to an expression tuple.
% Atoms may be infix.
%   e.g. <Alice is a NicePerson>
%
% An atom constructed from predicates in BehSymb are behaviour atoms.
%   e.g. <delete Email within 1 yr>
%
% Constraint language
% A set of relation symbols disjoint from Pred.
%
% Assertions
%   <E says f0 if f1,...,fn where c>   the keyword if omited when n=0
% where E elt Const, fi are facts, c is a constraint on variables
% occurring in the assertion.
%
% x, y denote variables
% E, U, S are elts of Const
% e denotes an expression
% c denotes a constraint
% a denotes an atom
% b denotes a behaviour atom
% B is a ground behaviour atom
% BB is a set of ground behaviour atoms
% f is a fact
% F is a ground fact
% alpha an asseration
% Alpha a set of assertions
% Theta a variable substitution
% gamma a ground total variable substitution (map every var to a const)
%
% Facts and Queries
%
%   f ::= a | e can_say f | e may b | e will b
%   q ::= e says f? | c? | not q | q1 and q2 | q1 or q2 | exists x(q)
%



%s4p_grammar([
%    (	a ::= b
%        <:>
%    )
%]).
