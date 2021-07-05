%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Privacy self test
%

priv_startup_tests([tc01,tc02,tc03,tc04,tc05,tc06,tc07,tc08,tc09,tc10,tc11,tc12,tc13,tc14,tc15, % lex
                    tc21,tc21a,tc21b,tc22,tc23,tc24,tc25,tc26,tc27,tc28,tc29,tc30, % parse - assertion context
		    tc41
                   ]).

priv_regression_tests([]).

self_test :- % priv:init,
	priv_startup_tests(Tests),
%	forall(member(T,Tests), test:report_test(priv:T)).
	forall(member(T,Tests), self_test(T)).

self_test(TC) :- priv:init, test:report_test(priv:TC).

regression_test :-  % priv:init,
	priv_startup_tests(Startup),
	priv_regression_tests(Regression),
	append(Startup,Regression,AllTests),
	forall(member(T,AllTests), self_test(T)).

lex_test( Rule, S, Result ) :-
	atom_chars(S,Chars),
	Lex =.. [Rule, LL, _LT, Chars, [] ],
	call(dctg:Lex), !, LL = Result.

parse_test( Rule, S, Result ) :-
	atom_chars(S,CL),
	dctg:lexeme_list( LL, _LT, CL, [] ),
	Parse =.. [Rule, PL, _PT, LL, [] ],
	call(dctg:Parse), !, PL = Result.

% LEX TESTS
tc01 :- lex_test( lexeme_list, '42', [num(42)] ).
tc02 :- lex_test( lexeme_list, 'word_1', [id(word_1)] ).
tc03 :- lex_test( lexeme_list, 'file', [file] ).
tc04 :- lex_test( lexeme_list, 'fx 42', [id(fx),num(42)] ).
tc05 :- lex_test( lexeme_list, 'Alice', [id('Alice')] ).
tc06 :- lex_test( lexeme_list, 'STS says Alice pred', [id('STS'),says,id('Alice'),id(pred)] ).
tc07 :- lex_test( lexeme_list, 'STS says Alice is a researcher',
                  [id('STS'), says, id('Alice'), is, id(a), id(researcher)] ).
tc08 :- lex_test( lexeme_list, 'STS says Alice is_a researcher',
                  [id('STS'), says, id('Alice'), is_a, id(researcher)] ).
tc09 :- lex_test( lexeme_list, 'A says x can say_inf y can say_0 B can act as z.',
                [id('A'),says,id(x),can,say_infinity,id(y),can,say_zero,id('B'),can,act,as,id(z),fstop] ).
tc10 :- lex_test( lexeme_list, 'A says_k x can say_inf y can say_0 B can act as z.',
                [id('A'),says_k,id(x),can,say_infinity,id(y),can,say_zero,id('B'),can,act,as,id(z),fstop] ).
tc11 :- lex_test( lexeme_list, 'FileServer says Alice can read project.',
                  [id('FileServer'), says, id('Alice'), can, read, id(project), fstop] ).
tc12 :- lex_test( lexeme_list, 'FileServer says Alice can read file://project.',
                  [id('FileServer'), says, id('Alice'), can, read, file, fsep, id(project), fstop] ).
tc13 :- lex_test( lexeme_list, 'FileServer says Alice can read file://project if cfact_placeholder where constr_placeholder.',
                  [id('FileServer'), says, id('Alice'), can, read, file, fsep, id(project), if, cfact_placeholder ,where, constr_placeholder, fstop] ).
tc14 :- lex_test( lexeme_list,  'A says B can say_inf y can say_0 C can read z if cfact_placeholder.',
		  [id('A'),says,id('B'),can,say_infinity,id(y),can,say_zero,id('C'),can,read,id(z),if,cfact_placeholder,fstop] ).
tc15 :- lex_test( lexeme_list,  'A says B can say_inf y can say_0 C can read z if y can read Foo.',
		  [id('A'),says,id('B'),can,say_infinity,id(y),can,say_zero,id('C'),can,read,id(z),if,id(y),can,read,id('Foo'),fstop] ).

% PARSE TESTS assertion context
tc21 :- parse_test( assertion_context, 'STS says Alice pred 5.', [says_pred('STS', 'Alice', 5)] ).

tc21a :- parse_test( assertion_context, 'STS says Alice is_a researcher.', [says_is_a('STS', 'Alice', researcher)] ).
tc21b :- parse_test( assertion_context, 'STS says Alice is a researcher.', [says_is_a('STS', 'Alice', researcher)] ).

tc22 :- parse_test( assertion_context, 'STS says Alice pred. Fileserver says Bob pred2(3,5).',
                    [says_pred('STS', 'Alice'), says_pred2('Fileserver', 'Bob', 3, 5)] ).
tc23 :- parse_test( assertion_context, 'A says x can say_inf y can say_0 B can act as z.',
                    [says_can_say_infinity_can_say_zero_can_act_as('A', x, y, 'B', z)] ).
tc24 :- parse_test( assertion_context, 'A says_k x can say_inf y can say_0 B can act as z.',
                    [says_k_can_say_infinity_can_say_zero_can_act_as('A', 0, x, y, 'B', z)] ).
tc25 :- parse_test( assertion_context,
                    'A says_k x can say_inf y can say_0 B can act as z.
                     C says x can say_inf y can say_0 D can act as z.',
                    [says_k_can_say_infinity_can_say_zero_can_act_as('A', 0, x, y, 'B', z),
                     says_can_say_infinity_can_say_zero_can_act_as('C', x, y, 'D', z)] ).
tc26 :- parse_test( assertion_context, 'FileServer says Alice can read file://project.',
                    [says_can_read('FileServer', 'Alice', 'file://project')] ).
tc27 :- parse_test( assertion_context,
		    'FileServer says Alice can read file://project
                     if cfact_placeholder.',
		    [(says_can_read('FileServer', 'Alice', 'file://project'):-cfact_placeholder)] ).
tc28 :- parse_test( assertion_context,
		    'FileServer says Alice can read file://project
                     if cfact_placeholder where constr_placeholder.',
		    [(says_can_read('FileServer', 'Alice', 'file://project'):-cfact_placeholder, constr_placeholder)] ).
tc29 :- parse_test( assertion_context,
		    'A says B can say_inf y can say_0 C can read z if cfact_placeholder.',
		    [says_can_say_infinity_can_say_zero_can_read('A','B',y,'C',z):-cfact_placeholder]).
tc30 :- parse_test( assertion_context,
		    'A says B can say_inf y can say_0 C can read z if y can read Foo, cfact_placeholder.',
		    [(says_can_say_infinity_can_say_zero_can_read('A','B',y,'C',z):-can_read(y,'Foo'), cfact_placeholder)]).

% PARSE TESTS query


% TRANSLATION TESTS
tc41 :- true.
