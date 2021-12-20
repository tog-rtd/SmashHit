% DCTG - Definite Clause Translation Grammar translator / interpreter

:- module(dctg, ['^^'/2]).

%:- use_module(assess).

/* Sources: Harvey Abramson, Veronical Dahl, Jocelyn Paine  */
/* Adapted by Rance DeLong */

/*
This file defines a DCTG translator as described in Abramson/Dahl
The Jocelyn Paine version is further modified.
DeLong changed operator precedence for SWI prolog and to live
within a larger system, and also made some changes to semantic
attribute interpretation.

Paine's predicate dctg_reconsult, for loading
grammars from file, is adapted by DeLong to use grammars
from internal structures. The original is retained for
testing purposes. The spec of the original follows: */


/*  dctg_reconsult( File+ ):
        File: a filename (atom).
    Loads the DCTG grammar in File, converting it to internal
    form useable by the parser.

    dctg_reconsult expects the following terms in its file:
        ?- (X)                  Call X, and go on to the next term.
        :- (X)                  Call X, and go on to the next term.
        X::=Y                   Translate as a DCTG clause, and assert
                                the corresponding Prolog.
        X:-Y                    Assert.
        Any other term X.       Assert.

    dctg_reconsult acts like ordinary reconsult in its dealings with
    repeated clauses. If it meets a clause C with functor and arity
    F,A, then it will always assert the clause. It will delete
    all other clauses for the same functor and arity, provided that
    they did not immediately precede the current one C.

    This applies both to ordinary Prolog clauses, and to DCTG clauses.
*/

% :- op( 100, yfx, ^^ ).
% :- op( 254, xfx, ::= ).
% :- op( 255, xfx, <:> ).
% :- op( 255, xfx, && ).
% :- op( 254, xfx, ::- ).
% :- op( 253, xfx, from ).

:- op( 100,  yfx, user:(^^) ).
:- op( 1178, xfx, user:(::=) ).
:- op( 1179, xfx, user:(<:>) ).
:- op( 1178, yfx, user:(&&) ).
:- op( 1177, xfx, user:(::-) ).
:- op( 1176, xfx, user:(from) ).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% read grammar rules from file
% or file containing a structure containing the grammar rules
% or from a list of grammar rules
%

dctg_list_reconsult([],_).
dctg_list_reconsult(Glist,StartSym) :-
	Glist = [Rule|_],
	( Rule = (StartSym ::= _) ; Rule = ((StartSym ::= _) <:> _) ),
	dctg_list_reconsult(Glist).

dctg_list_reconsult(List) :-
	dctg_s_reconsult1(List,'$none').

dctg_s_reconsult( File ) :-
    seeing( CIS ),
    see( File ), seen,
    see( File ),
    read(S), write_canonical(S), nl,
    seen, see(CIS),
    S =.. [_,Rules],
    dctg_list_reconsult(Rules).

dctg_s_reconsult1([],_).
dctg_s_reconsult1([Rule|Rules],Previous) :-
	process_term(Rule,Previous,Next), !,
	dctg_s_reconsult1(Rules,Next).

dctg_reconsult( File ) :-
    seeing( CIS ),
    see( File ), seen,
    see( File ),
    dctg_reconsult_1( '$none' ),
    seen,
    see( CIS ).


dctg_reconsult_1( Previous ) :-
    read( Term ),
    (
        Term = end_of_file
    ->
        true
    ;
        process_term( Term, Previous, Next ),
        dctg_reconsult_1( Next )
    ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% translation
%

process_term( ?-(Term), _, '$none' ) :-
    call( Term ) -> true ; true.

process_term( :-(Term), _, '$none' ) :-
    call( Term ) -> true ; true.

process_term( DCTG, Previous, Next  ) :-
    ( DCTG = (_ ::= _) ; DCTG = (_ <:> _) ),
    !,
    translate_rule( DCTG, Prolog ),
    process_term( Prolog, Previous, Next ).

process_term( :-(Head,Tail), Previous, clause(Functor,Arity) ) :-
    !,
    functor( Head, Functor, Arity ),
    (
        Previous \= clause(Functor,Arity)
    ->
        abolish( Functor, Arity )
    ;
        true
    ),
    assert( (Head:-Tail) ).

process_term( Head, Previous, Next ) :-
    process_term( (Head:-true), Previous, Next ).


translate_rule( (LP::=[]<:>Sem), H ) :-
    !,
    subtree_name_usage_check([],LP,[],Sem),
    t_lp( LP, [], S, S, Sem, H ).

translate_rule( (LP::=[]), H ) :-
    !,
    t_lp( LP, [], S, S, [], H ).

translate_rule( (LP::=RP<:>Sem), (H:-B) ) :-
    !,
%	writeln('translating:'),
%	write('LP: '), writeln(LP),
%	write('RP: '), writeln(RP),
%	write('SEM: '), writeln(Sem),
    t_rp( RP, [], StL, S, SR, B1, [], Nst ),
%        write('Subtree Names: '), writeln(Nst),
    subtree_name_usage_check(Nst,LP,RP,Sem),
    reverse( StL, RStL ),
    t_lp( LP, RStL, S, SR, Sem, H ),
    tidy( B1, B ).

translate_rule( (LP::=RP), (H:-B) ) :-
    translate_rule( (LP::=RP<:>[]), (H:-B) ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% t_lp  translate left part
%

t_lp( (LP,List), StL, S, SR, Sem, H ) :-
    append( List, SR, List2 ),
    makelist( Sem, Semantics ),
    add_extra_args( [node(LP,StL,Semantics),S,List2], LP, H ).

t_lp( LP, StL, S, SR, Sem, H ) :-
    makelist( Sem, Semantics ),
    add_extra_args( [node(LP,StL,Semantics),S,SR], LP, H ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% t_rp  translate right part
%
%   augmented to collect variables used to name subtrees (RJD)
%   2 additional arguments: in and out lists of variables used
%   as subtree names introduced in _^^SubTreeName terms in RP
%

t_rp( !, St, St, S, S, !, Nst, Nst ) :- !.

t_rp( [], St, [ [] | St ], S, S1, S=S1, Nst, Nst ) :- !.

t_rp( [X], St, [ [X] | St ], S, SR, c(S,X,SR), Nst, Nst ) :- !.

t_rp( [X|R], St, [ [X|R] | St ], S, SR, (c(S,X,SR1),RB), Nst1, Nst ) :-
    !,
    t_rp( R, St, [ R | St ], SR1, SR, RB, Nst1, Nst ).

t_rp( {T}, St, St, S, S, T, Nst, Nst ) :- !.

t_rp( (T,R), St, StR, S, SR, (Tt,Rt), Nst1, Nst ) :-
    !,
    t_rp( T, St, St1, S, SR1, Tt, Nst1, Nst2 ),
    t_rp( R, St1, StR, SR1, SR, Rt, Nst2, Nst ).

t_rp( T^^N, St, [N|St], S, SR, Tt, Nst, [N|Nst] ) :-
    add_extra_args( [N,S,SR], T, Tt ).

t_rp( T, St, [St1|St], S, SR, Tt, Nst, Nst ) :-
    add_extra_args( [St1,S,SR], T, Tt ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% interpreter for semantic attributes
%
%   Cuts added by DeLong to remedy needless backtracking.
%   Clause 5, also added by DeLong, catches references to undefined
%   attributes (attribute reference and definition do not agree).
%   Another approach would be to statically analyze the grammar
%   rule at translation time, see "usage checks" below. The cuts
%   impose the restriction on grammar rules that each attribute
%   must be described by a single clause in the semantic part
%   of a rule. If this restriction is found to be inconvenient
%   then translation-time static analysis is an option. A cut
%   after Traverse rather than before would remove the restriction
%   in part, but to completely do so would require the removal
%   of the cut also from clause 3 (unit clause attributes).
%   These tradeoffs may need to be revisited after more experience
%   is gained in the writing of a variety of useful grammars.
%

node( _, _, Sem ) ^^ Args :- !,
    Sem ^^ Args.

[ (Args::-Traverse) | _Rules ] ^^ Args :- !,
    Traverse.

[ Args | _Rules ] ^^ Args :- !.

[ _ | Rules ] ^^ Args :- !,
    Rules ^^ Args.

[] ^^ Args :- !,
        format('\n***attribute reference error: ~w ***~n',Args),
	fail.  % without cuts, failure causes backtracking

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% usage checks
%
% it would be neat to do these checks using attributes in the
% grammar itself, perhaps added automatically, but that would
% require the DCTG itself to be described by a DCTG.

subtree_name_usage_check(Nst,LP,RP,Sem) :-
	makelist(Sem,Semantics),
	var_occurrences(Semantics,Vars,Strefs),
	T = (Nst,Vars,Strefs,LP,RP,Sem),
	copy_term(T,TC), % don't bind original vars
	numbervars(TC,0,_),
	TC = (NNst,NVars,NStrefs,NLP,NRP,NSem),
	(   subtree_name_usage_check1(NNst,NStrefs,NVars)
	;   subtree_name_usage_error(NLP,NRP,NSem)
	).

subtree_name_usage_check1(NNst,NStrefs,NVars) :-
	is_set(NNst), % no dups among subtree names in rhs
	subset(NStrefs,NNst), % all refs are defined in rhs
	list_to_set(NVars,NVarsS),
	intersection(NVarsS,NNst,[]), % no subtree names as vars
	!.

subtree_name_usage_error(LP,RP,Sem) :-
	writeln('\n*** subtree name usage error ***'),
        format('rule: ~w ::= ~w <:> ~w~n',[LP,RP,Sem]).

% create lists of variables and subtree references occurring in Sem
% this could have also been done in makelist with a major overhaul
%
var_occurrences([],[],[]) :- !.
var_occurrences(Semantics,Vars,SubTreeRefs) :-
	var_occurrences(Semantics,[],Vars,[],SubTreeRefs).

var_occurrences(X,V,[X|V],R,R) :- var(X), !.
var_occurrences(X,V,V,R,R) :- atomic(X), !.
var_occurrences([X|Rest],Vi,Vo,Ri,Ro) :- !,
	var_occurrences(X,Vi,Vi1,Ri,Ri1),
	var_occurrences(Rest,Vi1,Vo,Ri1,Ro).
var_occurrences( (X,Y), Vi, Vo, Ri, Ro ) :- !,
	var_occurrences(X,Vi,Vi1,Ri,Ri1),
	var_occurrences(Y,Vi1,Vo,Ri1,Ro).
var_occurrences( (A::-Traverse), Vi, Vo, Ri, Ro ) :- !,
	var_occurrences(Traverse,Vi,Vi1,Ri,Ri1),
	var_occurrences(A,Vi1,Vo,Ri1,Ro).
var_occurrences( X^^Y, Vi, Vo, Ri, [X|Ri1] ) :- var(X), !,
	var_occurrences(Y,Vi,Vo,Ri,Ri1).
var_occurrences( X, Vi, Vo, Ri, Ro ) :- compound(X), !,
	X =.. [_|Args], var_occurrences(Args,Vi,Vo,Ri,Ro).

% Another useful usage check would be to check that for terms of
% the form SubTreeName^^Attr the subtree referenced actually has
% the referenced attribute.
%
% The RP and the Semantics would need to be cross referenced
% and rules for the appropriate nonterminals checked for the
% definitions of referenced attributes. This may need to be
% done in a separate pass because attributes may be referenced
% before being defined. The predicate that performs the check
% would be invoked at a higher level, after all the rules
% of the grammar had been translated. It could be done, for
% example, in dctg_list_reconsult (and less conveniently in
% dctg_reconsult by re-reading the file).
% The predicate,
%     attribute_use_def_check(GrammarList)
% would be invoked in an environment in
% which the rules had already been translated. The previously
% asserted clauses could be consulted to retrieve the semantics
% associated with references to attributes of those nonterminals.
% Other one-pass strategies may be achievable by passing data
% structures during the translation process.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% auxiliary predicates
%

add_extra_args( L, T, T1 ) :-
    T =.. Tl,
    append( Tl, L, Tl1 ),
    T1 =.. Tl1.


tidy( ((P1,P2),P3), Q ) :-
    tidy( (P1,(P2,P3)), Q ).

tidy( (P1,P2), (Q1,Q2) ) :-
    !,
    tidy( P1, Q1 ),
    tidy( P2, Q2 ).

tidy( A, A ).


c( [X|S], X, S ).


makelist( Sem, [Sem] ) :-
    var( Sem ),
    !.

makelist( (Sem1&&Sem2), [Sem1_|List] ) :-
    !,
    makelist_1( Sem1, Sem1_ ),
%    makelist_1( Sem2, Sem2_ ),
    makelist( Sem2, List ).

%makelist( (Sem1&&Sem2), [Sem1_|List] ) :-
%    !,
%    makelist_1( Sem1, Sem1_ ),
%    makelist_1( Sem2, Sem2_ ),
%    makelist( Sem2_, List ).

makelist( [], [] ) :- !.

%makelist( Sem, [Sem] ). % see following clause

makelist( Sem1, [Sem] ) :- makelist_1(Sem1,Sem).


makelist_1( Var^^Args, Sem ) :-
    !,
    Sem = Var^^Args.

makelist_1( (Attr from Var), Sem ) :-
    !,
    Attr_V =.. [ Attr, _V ],
    Sem = (Attr_V ::- Var^^Attr_V).

makelist_1( Sem, Sem ).


append( [], L, L ) :- !.

append( [X|R], L, [X|R1] ) :-
    append( R, L, R1 ).


reverse( X, RX ) :-
    rev1( X, [], RX ).


rev1( [], R, R ) :- !.

rev1( [X|Y], Z, R ) :-
    rev1( Y, [X|Z], R ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% dctg module self-test
%

self_test :-
	bit_test_grammar(Rules),
	dctg_list_reconsult(Rules),
	test_bit.

dctg_test :- dctg_reconsult('dctgtest.pl'), test.

dctg_s_test :- dctg_s_reconsult('dctg_s_test.pl'), test.

test( L, V ) :-
    write( 'LIST ' ), write( L ), nl,
    number( Tree, L, [] ),
/*    write( 'TREE ' ), write( Tree ), nl, */
    Tree ^^ value( V ),
    write( 'VALUE ' ), write( V ), nl, nl.

bits_to_value( L, V ) :-
    number( Tree, L, [] ),
    Tree ^^ value(V), !.

test :-
    test( [0], _ ),
    test( [1], _ ),
    test( [1,0,1,1,1,0], _ ),
    test( [1,'.',1], _ ),
    test( [1,'.',0,1,1,0,1], _ ).

test_bit :-
    bits_to_value( [0], 0 ),
    bits_to_value( [1], 1 ),
    bits_to_value( [1,0,1,1,1,0], 46 ),
    bits_to_value( [1,'.',1], 1.5 ),
    bits_to_value( [1,'.',0,1,1,0,1], 1.40625 ).

:- dynamic bit/3, bitstring/3, number/3, fraction/3.

bit_test_grammar([
    (   bit ::= [0]
        <:>
        bitval( 0, _ )
    ),
    (   bit ::= [1]
        <:>
        bitval( V,Scale ) ::- V is **(2,Scale)
    ),
    (   bitstring ::= []
        <:>
        length(0)
        &&
        value(0,_)
    ),
    (   bitstring ::= bit^^B, bitstring^^B1
        <:>
        length( Length ) ::-
            B1 ^^ length(Length1),
            Length is Length1 + 1
        &&
        value( Value, ScaleB ) ::-
            B ^^ bitval( VB, ScaleB ),
            S1 is ScaleB - 1,
            B1 ^^ value( V1, S1 ),
            Value is VB + V1
    ),
    (   number ::= bitstring ^^ B, fraction ^^ F
        <:>
        value(V) ::-
            B ^^ length(Length),
            S is Length-1,
            B ^^ value( VB, S ),
            F ^^ fractional_value( VF ),
            V is VB + VF
    ),
    (   fraction ::= ['.'], bitstring ^^ B
        <:>
        fractional_value( V ) ::-
            S is -1,
            B ^^ value( V, S )
    ),
    (   fraction ::= []
        <:>
        fractional_value(0)
    )
]).


pgtree(node(Name,DList,_)) :- pgtree(node(Name,DList,_),0).
% ptree/2 - convert to ptree/3
pgtree(node(Name,DList,_),L) :- !,
	format('~*|<~q>~n',[L,Name]),
	L1 is L + 4,
	pgtree2(DList,L1),
	true.
pgtree(_E,_L). % :- format('~*|~w~n',[_L,_E]).

pgtree2([],_) :- !.
pgtree2([N|Ns],L) :- !, pgtree(N,L), pgtree2(Ns,L).

is_letter(L) :- (is_lower(L) ; is_upper(L)).
is_lower(L) :- char_code(L,C), C>96, C<123.
is_upper(L) :- char_code(L,C), C>64, C<91.

