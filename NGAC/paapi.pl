% POLICY ADMINISTRATION API
:- module(paapi, []).

:- use_module('AUDIT/audit',[audit_gen/2]).
:- use_module('COM/param').
:- use_module(dpl).
%:- use_module(dpl_conditions).
:- use_module(policies).
:- use_module('COM/sessions').
:- use_module(pap).
:- use_module('COM/apiresp').

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_wrapper)).
:- use_module(library(http/http_header)).
:- use_module(library(http/http_parameters)).

% Policy Administration Web API
:- http_handler(root(paapi), root_apis(paapi), []).
:- http_handler(root('paapi/'), api_unimpl, [prefix]).
:- http_handler(root(paapi/add), paapi_add, [prefix]).
:- http_handler(root(paapi/addm), paapi_addm, [prefix]).
:- http_handler(root(paapi/delete), paapi_delete, [prefix]).
:- http_handler(root(paapi/deletem), paapi_deletem, [prefix]).
:- http_handler(root(paapi/getpol), paapi_getpol, [prefix]).
:- http_handler(root(paapi/setpol), paapi_setpol, [prefix]).
:- http_handler(root(paapi/combinepol), paapi_combinepol, [prefix]).
% :- http_handler(root(paapi/importpol), paapi_loadpol, [prefix]). % deprecated
:- http_handler(root(paapi/load), paapi_loadpol, [prefix]).
:- http_handler(root(paapi/loadi), paapi_loadpoli, [prefix]).
:- http_handler(root(paapi/readpol), paapi_readpol, [prefix]).
:- http_handler(root(paapi/purgepol), paapi_unloadpol, [prefix]).
:- http_handler(root(paapi/unload), paapi_unloadpol, [prefix]).
:- http_handler(root(paapi/loadcondi), paapi_loadcondi, [prefix]).
:- http_handler(root(paapi/unloadcondi), paapi_unloadcondi, [prefix]).
:- http_handler(root(paapi/readcond), paapi_readcond, [prefix]).
:- http_handler(root(paapi/reset), paapi_reset, [prefix]).
:- http_handler(root(paapi/resetcond), paapi_resetcond, [prefix]).
:- http_handler(root(paapi/initsession), paapi_initsession, [prefix]).
:- http_handler(root(paapi/endsession), paapi_endsession, [prefix]).
:- http_handler(root(paapi/getstatus), paapi_getstatus, [prefix]).

% Global Policy Admin API
:- http_handler(root(gpaapi), root_apis(gpaapi), []).
:- http_handler(root('gpaapi/'), api_unimpl, [prefix]).
:- http_handler(root(gpaapi/getgpol), gpaapi_getgpol, [prefix]).
:- http_handler(root(gpaapi/setgpol), gpaapi_setgpol, [prefix]).

% POLICY ADMIN APIs
paapi([add,addm,delete,deletem,getpol,setpol,combinepol,
		load,loadi,readpol,purgepol,unload,loadcondi,unloadcondi,
		readcond,reset,resetcond,initsession,endsession]).

:- include('paapi_dplp'). % meta-element predicates in separate file

% GLOBAL POLICY ADMIN APIs
gpaapi([getgpol,setgpol]).

%
% Policy Administration API
%

% add
paapi_add(Request) :-
	std_resp_prefix,
	parse_add_delete_arguments(Request, Policy, PElement, Token),
	(   authenticate(Token)
	->  add(Policy,PElement)
	;   true
	).
paapi_add(_) :- audit_gen(policy_admin, add(failure)).

% add(Policy,Consent) :- compound_name_arity(Consent,consent,10), !,
%	% consent is intercepted here just to enable consent-specific returns
%	% consent is also accepted in addm
%	(   add_consent(Policy, Consent,_Status)
%	->  std_resp_MS(success,'consent added',Consent),
%	    audit_gen(policy_admin, add_consent(Policy, Consent, success))
%	;   std_resp_MS(failure,'error adding consent', Consent),
%	    audit_gen(policy_admin, add_consent(Policy, Consent, failure))
%	).

add(Policy,PElement) :- isa_meta_element(PElement), !, policy(Policy,PC),
	dpl:unpack_policy_elements_with_meta_expansion(Policy:PC,[PElement],Errors),
	(	Errors == []
	->	std_resp_MS(success,'meta-element added',PElement),
		audit_gen(policy_admin, add(Policy, PElement, success))
	;   std_resp_MS(failure,'error adding meta-element',PElement:Errors),
		audit_gen(policy_admin, add(Policy, PElement, failure))
	).

add(Policy,PElement) :-
	(   %add_policy_element_restricted(Policy,PElement)
		add_policy_element(Policy,PElement)
	->  std_resp_MS(success,'element added',PElement),
	    audit_gen(policy_admin, add(Policy, PElement, success))
	;   std_resp_MS(failure,'error adding element',PElement),
	    audit_gen(policy_admin, add(Policy, PElement, failure))
	).

% delete
paapi_delete(Request) :-
	std_resp_prefix,
	parse_add_delete_arguments(Request, Policy, PElement, Token),
	(   authenticate(Token)
	->  delete(Policy,PElement), !
	;   true
	).
paapi_delete(_) :- audit_gen(policy_admin, delete(failure)).

delete(Policy,Consent) :- compound_name_arity(Consent,consent,1), !,
	(	%delete_consent(Policy, Consent)
		delete_PE(Policy:_,Consent,_)
	->	std_resp_MS(success,'consent deleted',Consent),
		audit_gen(policy_admin, delete_consent(Policy, Consent, success))
	;   std_resp_MS(failure,'error deleting consent',Consent),
		audit_gen(policy_admin, delete_consent(Policy, Consent, failure))
	).
delete(Policy,PElement) :-
	(   %delete_policy_element(Policy,PElement) % orig
		 delete_PE(Policy:_,PElement,chk)
		 %delete_PE(Policy:_,PElement)
	->  std_resp_MS(success,'element deleted',PElement),
	    audit_gen(policy_admin, delete(Policy, PElement, success))
	;   std_resp_MS(failure,'error deleting element',PElement),
	    audit_gen(policy_admin, delete(Policy, PElement, failure))
	).

parse_add_delete_arguments(Request, Policy, PElement, Token) :-
	catch(
	    http_parameters(Request,[policy(Policy,[atom]),
				     % accept either of the following for backward compat
				     policy_element(P_E,[atom,optional(true)]),
				     policyelement(PE,[atom,optional(true)]),
				     token(Token,[atom])
				   ]),
	    Err,
	    (	( Err = error(E,_) ; Err = E ), std_resp_MS(failure,'missing parameter',E), !, fail )
	), !,
	(   ( ( var(P_E), var(PE) ) ; ( nonvar(P_E), nonvar(PE) ) )
	->  std_resp_MS(failure,'error parsing request arguments',PElement),
	    !, fail
	;   P_E = PE
	),
	(
	    (
		read_term_from_atom(PE,PElement,[]),
		ground(PElement), PElement =.. [PEf|_PEargs],
		permitted_add_delete_policy_elements(Permitted),
		memberchk(PEf,Permitted)
	    )
	;
	    std_resp_MS(failure,'error in argument',''),
	    !, fail
	), !.

% addm
paapi_addm(Request) :-
	std_resp_prefix,
	catch(
	    http_parameters(Request,[policy(Policy,[atom]),
				     policy_elements(EltListAtom,[atom]),
				     name(Name,[atom,optional(true)]),
				     token(Token,[atom])]),
	    Err,
	    (	( Err = error(E,_) ; Err = E ), std_resp_MS(failure,'missing parameter',E), !, fail )
	), !,
	(   authenticate(Token)
	->  addm(Policy,EltListAtom,Name), !
	;   true
	).
paapi_addm(_) :- audit_gen(policy_admin, addm(failure)).

addm(Policy,EltListAtom,Name) :-
        ( ( read_term_from_atom(EltListAtom,EltList,[]), is_list(EltList),
	      add_named_policy_elements(Name,Policy,EltList) )
          ->  std_resp_MS(success,'elements added',EltList),
              audit_gen(policy_admin, addm(Policy, 'elements added'))
	  ;   std_resp_MS(failure,'error adding elements',EltListAtom),
              audit_gen(policy_admin, addm(Policy, 'error adding elements'))
	).

% deletem
paapi_deletem(Request) :-
	std_resp_prefix,
	catch(
	    (	http_parameters(Request,[
					policy(Policy,[atom]),
				    policy_elements(EltListAtom,[atom,optional(true)]),
				    name(Name,[atom,optional(true)]),
				    token(Token,[atom])]),
	        ( var(EltListAtom) ; var(Name) ) % one must be specified but not both
	    ),
	    Err,
	    (	( Err = error(E,_) ; Err = E ), std_resp_MS(failure,'missing parameter',E), !, fail )
	), !,
	(   authenticate(Token)
	->  deletem(Policy,EltListAtom,Name), !
	;   true
	).
paapi_deletem(_) :- audit_gen(policy_admin, deletem(failure)).

deletem(Policy,EltListAtom,Name) :- atom(EltListAtom), atom(Name), !,
	std_resp_MS(failure,'error: both name and policy_elements specified',EltListAtom),
    audit_gen(policy_admin, deletem(Policy, 'error: both name and policy_elements specified')).
deletem(Policy,EltListAtom,Name) :- ground(EltListAtom), var(Name), !,
        ( ( read_term_from_atom(EltListAtom,EltList,[]), is_list(EltList),
		%delete_named_policy_elements(Name,Policy,EltList) )
			delete_PEs(Policy:_, EltList) )
          ->  std_resp_MS(success,'elements deleted',EltList),
              audit_gen(policy_admin, deletem(Policy, 'elements deleted'))
	  ;   std_resp_MS(failure,'error deleting elements',EltListAtom),
              audit_gen(policy_admin, deletem(Policy, 'error deleting elements'))
	).
deletem(Policy,EltListAtom,Name) :- var(EltListAtom), ground(Name), !,
	(   %delete_named_policy_elements(Name,Policy,_)
		delete_named(Policy:_, Name)
	->  std_resp_MS(success,'elements deleted',Policy:Name),
		audit_gen(policy_admin, deletem(Policy, 'elements deleted'))
	;   std_resp_MS(failure,'error deleting elements',Policy:Name),
		audit_gen(policy_admin, deletem(Policy, 'error deleting elements'))
	).

% getpol
paapi_getpol(Request) :- % set current policy
	std_resp_prefix,
	catch(
	    http_parameters(Request,[token(Token,[atom])]),
	    Err,
	    (	( Err = error(E,_) ; Err = E ), std_resp_MS(failure,'missing parameter',E), !, fail )
	), !,
	(   authenticate(Token)
	->  getpol, !
	;   true
	).
paapi_getpol(_) :- audit_gen(policy_admin, getpol(failure)).

getpol :-
	get_current_policy(P),
	std_resp_BS(success,'current policy',P),
	audit_gen(policy_admin, getpol(success)).

% setpol
paapi_setpol(Request) :- % set current policy
	std_resp_prefix,
	catch(
	    http_parameters(Request,[policy(P,[atom]),token(Token,[atom])]),
	    Err,
	    (	( Err = error(E,_) ; Err = E ), std_resp_MS(failure,'missing parameter',E), !, fail )
	), !,
	(   authenticate(Token)
	->  setpol(P), !
	;   true
	).
paapi_setpol(_) :- audit_gen(policy_admin, setpol(failure)).

setpol(P) :-
	(   ( dpl:policy(P,_); P==all; P==allnc ; P==grant; P==deny; P==none )
	->  set_current_policy(P),
	    std_resp_BS(success,'policy set',P),
	    audit_gen(policy_admin, setpol(P,success))
	;   std_resp_MS(failure,'unknown policy',P),
	    audit_gen(policy_admin, setpol(P,failure))
	).

% loadpol
paapi_loadpol(Request) :- % load policy from a file
	std_resp_prefix,
	catch(
	    http_parameters(Request,[policyfile(Pfile,[atom]),token(Token,[atom])]),
	    Err,
	    (	( Err = error(E,_) ; Err = E ), std_resp_MS(failure,'missing parameter',E), !, fail )
	), !,
	(   authenticate(Token)
	->  loadpol(Pfile), !
	;   true
	).
paapi_loadpol(_) :- audit_gen(policy_admin, loadpol(failure)).

loadpol(Pfile) :-
	(   ( exists_file(Pfile), load_policy(Pfile,PolicyName) )
	->  % TODO add check for: all, none, grant, deny
	    std_resp_BS(success,'policy loaded',PolicyName),
	    audit_gen(policy_admin, load(Pfile,PolicyName,success))
	;   std_resp_MS(failure,'file or load error',Pfile),
	    audit_gen(policy_admin, load(Pfile,failure))
	).

% loadi
paapi_loadpoli(Request) :- % load policy immediate
	std_resp_prefix,
	catch(
	    http_parameters(Request,[policyspec(Pspec,[atom]),token(Token,[atom])]),
	    Err,
	    (	( Err = error(E,_) ; Err = E ), std_resp_MS(failure,'missing parameter',E), !, fail )
	), !,
	(   authenticate(Token)
	->  loadpoli(Pspec), !
	;   true
	).
paapi_loadpoli(_) :- audit_gen(policy_admin, loadpoli(failure)).

loadpoli(Pspec) :-
	(   ( ground(Pspec), load_policy_immediate(Pspec,PolicyName) )
	->  std_resp_BS(success,'policy loaded immediate',PolicyName),
	    audit_gen(policy_admin, loadi(Pspec,PolicyName,success))
	;   std_resp_MS(failure,'malformed policy or load error',Pspec),
	    audit_gen(policy_admin, loadi(Pspec,failure))
	).

% unloadpol
paapi_unloadpol(Request) :- % unload policy
	std_resp_prefix,
	catch(
	    http_parameters(Request,[policy(P,[atom]),token(Token,[atom])]),
	    Err,
	    (	( Err = error(E,_) ; Err = E ), std_resp_MS(failure,'missing parameter',E), !, fail )
	), !,
	(   authenticate(Token)
	->  unloadpol(P), !
	;   true
	).
paapi_unloadpol(_) :- audit_gen(policy_admin, unloadpol(failure)).


unloadpol(P) :-
	(   dpl:policy(P,_)
	->  unload_policy(P),
	    std_resp_MS(success,'policy unloaded',P),
	    audit_gen(policy_admin, unloadpol(P,success))
	;   std_resp_MS(failure,'unknown policy',P),
	    audit_gen(policy_admin, unloadpol(P,'unknown policy failure'))
	).

% readpol
paapi_readpol(Request) :-
	std_resp_prefix,
	catch(
	    http_parameters(Request,[policy(P,[atom,default(current_policy)]),
				     token(Token,[atom]),
				     part(Part,[atom,optional(true),default(meta)])
				    ]),
	    Err,
	    (	( Err = error(E,_) ; Err = E ), std_resp_MS(failure,'missing parameter',E), !, fail )
	), !,
	(   authenticate(Token)
	->  readpol(P,Part), !
	;   true
	).
paapi_readpol(_) :- audit_gen(policy_admin, readpol(failure)).

readpol(P,Part) :-
	(   ( P==current_policy, param:current_policy(PN), PN\==none ; policy(P,_), PN=P )
	->  policies:policy(PN,_PC,_PE,_PT), % PTerm = policy(PN,PC,PE,PT),
	    with_output_to( atom(PAtom), policyio:display_policy(PN,Part) ),
	    std_resp_BS(success,'read policy',PAtom)
	;   std_resp_MS(failure,'unknown policy',P),
	    audit_gen(policy_admin, readpol(P,failure))
	).

% combinepol
paapi_combinepol(Request) :-
	std_resp_prefix,
	catch(
	    http_parameters(Request,[policy1(P1,[atom]),
				     policy2(P2,[atom]),
				     combined(Pc,[atom]),
				     token(Token,[atom])
				    ]),
	    Err,
	    (	( Err = error(E,_) ; Err = E ), std_resp_MS(failure,'missing parameter',E), !, fail )
	), !,
	(   authenticate(Token)
	->  combinepol(P1,P2,Pc), !
	;   true
	).
paapi_combinepol(_) :- audit_gen(policy_admin, combinepol(failure)).

combinepol(P1,P2,Pc) :-
	(   pap:compose_policies(P1,P2,Pc)
	->  std_resp_BS(success,'policies combined',Pc),
	    audit_gen(policy_admin, combinepol(P1,P2,Pc,success))
	;   std_resp_MS(failure,'error combining policies',''),
	    audit_gen(policy_admin, combinepol(P1,P2,Pc,failure))
	).

% loadcondi - load condition variables and predicates
paapi_loadcondi(Request) :-
	std_resp_prefix,
	catch(
	    http_parameters(Request,[cond_name(Cname,[atom,default(dynamic)]),
				     cond_elements(EltListAtom,[atom]),
				     token(Token,[atom])]),
	    Err,
	    (	( Err = error(E,_) ; Err = E ), std_resp_MS(failure,'missing parameter',E), !, fail )
	), !,
	(   authenticate(Token)
	->  loadcondi(Cname,EltListAtom), !
	;   true
	).
paapi_loadcondi(_) :- audit_gen(policy_admin, loadcondi(failure)).

loadcondi(Cname,EltListAtom) :-
        ( ( read_term_from_atom(EltListAtom,EltList,[]), is_list(EltList),
	      dynamic_add_cond_elements(Cname,EltList) )
          ->  std_resp_MS(success,'cond elements added',Cname),
              audit_gen(policy_admin, loadcondi(Cname, 'cond elements added'))
	  ;   std_resp_MS(failure,'error adding cond elements',EltListAtom),
              audit_gen(policy_admin, loadcondi(Cname, 'error adding elements'))
	).

% unloadcondi
paapi_unloadcondi(Request) :-
	std_resp_prefix,
	catch(
	    (	http_parameters(Request,[cond_name(Cname,[atom,default(user_defined)]),
				     cond_elements(EltListAtom,[atom,optional(true)]),
				     token(Token,[atom])]),
		( ground(Cname) ; ground(EltListAtom) ) % at least one must be specified
	    ),
	    Err,
	    (	( Err = error(E,_) ; Err = E ), std_resp_MS(failure,'missing parameter',E), !, fail )
	), !,
	(   authenticate(Token)
	->  unloadcondi(Cname,EltListAtom), !
	;   true
	).
paapi_unloadcondi(_) :- audit_gen(policy_admin, unloadcondi(failure)).

unloadcondi(_Cname,EltListAtom) :- var(EltListAtom), !, fail. % for now
unloadcondi(Cname,EltListAtom) :- % Cname is either 'user_defined' or specified name
        ( ( read_term_from_atom(EltListAtom,EltList,[]), is_list(EltList),
	      dynamic_delete_cond_elements(Cname,EltList) )
          ->  std_resp_MS(success,'cond elements deleted',EltList),
              audit_gen(policy_admin, unloadcondi(Cname, 'cond elements unloaded'))
	  ;   std_resp_MS(failure,'error unloading cond elements',EltListAtom),
              audit_gen(policy_admin, unloadcondi(Cname, 'error unloading cond elements'))
	).

% readcond
paapi_readcond(Request) :-
	std_resp_prefix,
	catch(
	    http_parameters(Request,[cond_name(CN,[atom,default(dynamic)]),
				     token(Token,[atom])]),
	    Err,
	    (	( Err = error(E,_) ; Err = E ), std_resp_MS(failure,'missing parameter',E), !, fail )
	), !,
	(   authenticate(Token)
	->  readcond(CN), !
	;   true
	).
paapi_readcond(_) :- audit_gen(policy_admin, readcond(failure)).

readcond(CN) :-
	(   dpl_conditions:is_cond_name(CN)
	->
	    with_output_to( atom(CAtom), policyio:display_conditions(CN) ),
	    std_resp_BS(success,'read conditions',CAtom)
	;   std_resp_MS(failure,'unknown condition name',CN),
	    audit_gen(policy_admin, readcond(CN,failure))
	).

% reset
paapi_reset(Request) :-
	std_resp_prefix,
	catch(
	    http_parameters(Request,[
					domain(Dom,[atom,default(conditions)]),
				    name(Name,[atom,default('dynamic')]),
				    token(Token,[atom])]),
	    Err,
	    (	( Err = error(E,_) ; Err = E ), std_resp_MS(failure,'missing parameter',E), !, fail )
	), !,
	(   authenticate(Token)
	->  reset(Dom,Name), !
	;   true
	).
paapi_reset(_) :- audit_gen(policy_admin, reset(failure)).

reset(conditions,CN) :- !, % conditions domain
	(   dpl_conditions:is_cond_name(CN)
	->
	    preset(conditions,CN),
	    std_resp_BS(success,'reset conditions',CN)
	;   std_resp_MS(failure,'unknown condition name',CN),
	    audit_gen(policy_admin, reset(conditions,CN,failure))
	).
reset(policy,PN) :- !, % policies domain
	(   preset(policy,PN)
	->	std_resp_BS(success,'reset policy',PN),
		audit_gen(policy_admin, reset(policy,PN,success))
	;   std_resp_MS(failure,'unknown policy name',PN),
		audit_gen(policy_admin, reset(policy,PN,failure))
	).
reset(D,_) :- !,
	std_resp_MS(failure,'reset policies',unknown_domain(D)),
	audit_gen(policy_admin, reset(unknown_domain(D),failure)).

% resetcond - short-cut for conditions
%
% cond_name is the name of a condition set
% if cond_name is not supplied it defaults to 'dynamic'
% which will cause all dynamically loaded conditions to be unloaded
%
paapi_resetcond(Request) :-
	std_resp_prefix,
	catch(
	    http_parameters(Request,[cond_name(CN,[atom,default(all)]),
				     token(Token,[atom])]),
	    Err,
	    (	( Err = error(E,_) ; Err = E ), std_resp_MS(failure,'missing parameter',E), !, fail )
	), !,
	(   authenticate(Token)
	->  reset(conditions,CN), !
	;   true
	).
paapi_resetcond(_) :- audit_gen(policy_admin, resetcond(failure)).

% initsession
paapi_initsession(Request) :-
	std_resp_prefix,
	catch(
	    http_parameters(Request,[session(S,[atom]),
				    user(U,[atom]),token(Token,[atom])]),
	    Err,
	    (	( Err = error(E,_) ; Err = E ), std_resp_MS(failure,'missing parameter',E), !, fail )
	), !,
	(   authenticate(Token)
	->  initsession(S,U), !
	;   true
	).
paapi_initsession(_) :- audit_gen(policy_admin, initsession(failure)).

initsession(S,U) :-
	(   \+is_session(S,_)
	->  init_session(S,U),
	    std_resp_BS(success,'session initialized',S),
	    audit_gen(policy_admin, initsession(S,U,success))
	;   std_resp_MS(failure,'session already registered',S),
	    audit_gen(policy_admin, initsession(S,U,failure))
	).

% endsession
paapi_endsession(Request) :-
	std_resp_prefix,
	catch(
	    http_parameters(Request,[session(S,[atom]),
				     token(Token,[atom])]),
	    Err,
	    (	( Err = error(E,_) ; Err = E ), std_resp_MS(failure,'missing parameter',E), !, fail )
	), !,
	(   authenticate(Token)
	->  endsession(S), !
	;   true
	).
paapi_endsession(_) :- audit_gen(policy_admin, endsession(failure)).

endsession(S) :-
	(   is_session(S,_)
	->  end_session(S),
	    std_resp_MS(success,'session ended',S),
	    audit_gen(policy_admin, endsession(S,success))
	;   std_resp_MS(failure,'session unknown',S),
	    audit_gen(policy_admin, endsession(S,failure))
	).

% getstatus
paapi_getstatus(Request) :-
	std_resp_prefix,
	catch(
	    http_parameters(Request,[token(_Token,[atom,optional(true)])]),
	    Err,
	    (	( Err = error(E,_) ; Err = E ), std_resp_MS(failure,'missing parameter',E), !, fail )
	), !,
	getstatus(_),
   audit_gen(policy_admin, getstatus(success)), !.
paapi_getstatus(_) :- audit_gen(policy_query, getstatus(failure)).

getstatus(Status) :-
	param:build_version(ngac, CurrentVer),
	param:build_current_version_description(ngac, CurrentDesc),
	pap:get_current_policy(P),
	findall(policy(PN:PC, elements(Nelts), meta(Nmeta), assign(Nassign), assoc(Nassoc), cond(Ncond) ),
		(
			policy(PN,PC),
			aggregate_all(count, element(PN:PC,_), Nelts),
			aggregate_all(count, melement(PN:PC,_,true), Nmeta),
			aggregate_all(count, assign(PN:PC,_,_), Nassign),
			aggregate_all(count, associate(PN:PC,_,_,_), Nassoc),
			aggregate_all(count, cond(PN:PC,_,_), Ncond)
		),
		PolicyInfo),

	Status = status(
		current_version(CurrentVer,CurrentDesc),
		current_policy(P),
		PolicyInfo
	),
	std_resp_BS(success,getstatus,Status), !.
getstatus(_) :-
	    std_resp_MS(failure,'failure getting status',''),
	    audit_gen(policy_admin, getstatus(failure)).

%
% GLOBAL POLICY ADMIN
%

gpaapi_getgpol(Request) :- % set current policy
	std_resp_prefix,
	catch(
	    http_parameters(Request,[token(Token,[atom])]),
	    Err,
	    (	( Err = error(E,_) ; Err = E ), std_resp_MS(failure,'missing parameter',E), !, fail )
	), !,
	(   authenticate(Token)
	->  getgpol, !
	;   true
	).
gpaapi_getgpol(_) :-audit_gen(policy_admin, getgpol(failure)).

getgpol :-
	get_current_gpolicy(GP),
	std_resp_BS(success,'current global policy',GP),
	audit_gen(policy_admin, getgpol(success)).

gpaapi_setgpol(Request) :- % set current policy
	std_resp_prefix,
	catch(
	    http_parameters(Request,[policy(GP,[atom]),token(Token,[atom])]),
	    Err,
	    (	( Err = error(E,_) ; Err = E ), std_resp_MS(failure,'missing parameter',E), !, fail )
	), !,
	(   authenticate(Token)
	->  setgpol(GP), !
	;   true
	).
gpaapi_setgpol(_) :- audit_gen(policy_admin, setgpol(failure)).

setgpol(GP) :-
	(   dpl:policy(GP,_)
	->  set_current_gpolicy(GP),
	    std_resp_BS(success,'global policy set',GP),
	    audit_gen(policy_admin, setgpol(GP,success))
	;   std_resp_MS(failure,'unknown global policy',GP),
	    audit_gen(policy_admin, setgpol(GP,failure))
	).


authenticate(Token) :-
	(   authenticate_token(Token)
	->  true
	;   std_resp_M(failure,'authentication error',''),
	    audit_gen(policy_admin, 'authentication error'),
	    !, fail
	).

authenticate_token(Token) :- atom(Token), param:admin_token(Token), !.


read_term_from_atom_in_list([],[]).
read_term_from_atom_in_list([Elt|Elts],[TElt|TElts]) :-
	read_term_from_atom(Elt,TElt,[]),
	read_term_from_atom_in_list(Elts,TElts).

