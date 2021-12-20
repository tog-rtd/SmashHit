% S4P Translation

:- module(s4p_trans,[
	  ]).

% TRANSLATE AN ASSERTION CONTEXT TO CLAUSES

translate_assertion( Assertion, AClauses ) :-
	assertion_parts( Assertion, A, Fact0, CFacts, Constraint ),
	translate(A, Fact0, CFacts, Constraint, TClauses),
	expand( TClauses, DerivedClauses ),
	append( TClauses, DerivedClauses, AClauses).

translate(A,AHead,ABody,C,[T]) :- flat(AHead), !,
	cfacts_from_body(ABody, CondFacts),
	findall( TBodyAtom,
		(member(CondFact, CondFacts), trans(A, CondFact, TBodyAtom) ),
		TBodyAtoms),
	assemble_clause(A,AHead,TBodyAtoms,C,T),
	true.
translate(AHead,ABody,C,TClauses) :- % non-flat Ahead
	expand(AHead, AClauses),
	expand_facts(ABody, CondFacts),
	translate(A,AHead,ABody,C,TC1),
	findall( TClause,
		( member(CondFact, CondFacts), trans_clause(A, CondFact, TClause) ),
		TClausess),
	true.

trans(A, CF, TA) :-
	TA = says_k(K, A, CF), % K a fresh variable
	true.

trans_clause(A, CFact, TClause) :-
	TClause = ( FactHead :- BodyAtoms ),

	true.

flat(Fact) :-
	true.

assertion_parts( Assertion, A, Fact0, CFacts, Constraint) :-
	true.

expand( TClauses, TDerivedClauses ) :-
	true.

cfacts_from_body(ABody, CFacts) :-
	true.

assemble_clause(A, AHead, TBodyAtoms, C, TClause) :-
	true.

ttc01 :- f( 'A says B can say_inf y can say_0 C can read z if y can read Foo.', _).
