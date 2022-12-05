% Policy Access/Administration Point

:- module(pap, [
		permitted_add_delete_policy_elements/1,
		add_policy_element_restricted/2,
		add_policy_element/2, add_policy_element/3,
		%add_consent/3, add_consent/4,
		%delete_consent/2, delete_consent/3,
		delete_policy_element/2, delete_policy_element/3,

		add_policy_elements/2, add_policy_elements/3,
		delete_policy_elements/2, delete_policy_elements/3,

		add_named_policy_elements/3,
		delete_named_policy_elements/3,

		compose_policies/3,
		get_current_policy/1, set_current_policy/1,
		get_current_gpolicy/1, set_current_gpolicy/1,
		load_policy/2, load_policy_immediate/2, unload_policy/1,
	    dynamic_add_cond_elements/2, dynamic_delete_cond_elements/2,
		preset/2]
	 ).

:- use_module('COM/param').
:- use_module(dpl).
:- use_module(dpl_conditions).
:- use_module('COM/sessions').
:- use_module('AUDIT/audit',[audit_gen/2]). % currently not used in this module

permitted_add_delete_policy_elements([user,user_attribute,object,object_attribute,assign,associate,consent]).

%
% Policy Administration Point commands
% called externally from paapi
% or internally through the pap exported predicates
%
% IN THE FOLLOWING NEED TO CHECK FOR EFFECTS OF CONDITIONAL RULES

%
% CURRENTLY ALL ADD REQUESTS USE DPL:UNPACK_POLICY_ELEMENTS
% NONE USE PAP:ADD_POLICY_ELEMENTS/3 or ADD_POLICY_ELEMENT/2,3
%

% add_policy_element/2
add_policy_element(P:PC,Element) :- !, atom(P), atom(PC), add_policy_element(P,PC,Element).
add_policy_element(P,Element) :- atom(P), policy(P,PC), !, add_policy_element(P,PC,Element).

% add_policy_element_restricted/2
add_policy_element_restricted(P:PC,Element) :- !, atom(P), atom(PC),
	permitted_add_delete_policy_elements(ADE), functor(Element,E,_), memberchk(E,ADE),
	unpack_policy_elements(P:PC,[Element]).
add_policy_element_restricted(P,Element) :- atom(P), policy(P,PC), !,
	permitted_add_delete_policy_elements(ADE), functor(Element,E,_), memberchk(E,ADE),
	unpack_policy_elements(P:PC,[Element]).

% From dpl.pl for reference:
%
% policy_elements([user,user_attribute,object,object_attribute,policy_class,
%		 operation,opset,composed_policy,assign,associate,connector,
%		 cond,conditions,external_attribute]).
%
% policy_elements_args([user(_),user_attribute(_),
%		      object(_),object(_,_,_,_,_,_,_),
%		      object_attribute(_),policy_class(_),operation(_),operation(_,_),
%		      opset(_,_),composed_policy(_,_,_),assign(_,_),associate(_,_,_),
%		      connector(_),cond(_,_),conditions(_),external_attribute(_)]).
%
% conditional_policy_elements_args([assign(_,_),associate(_,_,_)]).

% experimental consent meta-element - see initial handling in paapi and add_consent in this module
%add_policy_element(P,PC,Consent) :- compound_name_arity(Consent,consent,10), !,
%	add_consent(P,PC,Consent).


% add_policy_element(_,_,_). % silently ignore if conditions not met
% (see add_policy_elements)

% add_policy_elements/2
% this is the entry point from paapi for addm
%
% wanting addm to do more element kinds without further developing this,
% instead of calling add_policy_elements/3 lets just call
% unpack_policy_elements/2 from dpl
%
% we can distinguish "safe" add from "unsafe" add later
% So, it was:
% ----------
% add_policy_elements(P:PC,Elements) :- !, atom(P), atom(PC), add_policy_elements(P,PC,Elements).
% add_policy_elements(P,Elements) :- atom(P), policy(P,PC), add_policy_elements(P,PC,Elements).
% ----------
% for now we use dpl:unpack_policy_elements

% ADD WITH CONSTRAINT CHECKING
%

add_named_policy_elements(Name,Policy,Elements) :- var(Name), !,
	add_policy_elements(Policy,Elements).
add_named_policy_elements(Name,Policy,Elements) :- ground(Name), \+ named_policy_elements(Name,Policy,_), !,
	assert( named_policy_elements(Name,Policy,Elements) ),
	add_policy_elements(Policy,Elements).

% add_policy_elements/2   TODO - return Errors
add_policy_elements(P:PC,Elements) :- !, atom(P), atom(PC),
	dpl:unpack_policy_elements_with_meta_expansion(P:PC,Elements,Errors), Errors == [].
add_policy_elements(P,Elements) :- atom(P), policy(P,PC),
	dpl:unpack_policy_elements_with_meta_expansion(P:PC,Elements,Errors), Errors == [].

% add_policy_elements/3
add_policy_elements(_,_,[]).
add_policy_elements(P,PC,[Element|Elements]) :-
	( add_policy_element(P,PC,Element) ; true ), % silently ignore add failure (only add multiple)
	add_policy_elements(P,PC,Elements).

% add_policy_element/3
add_policy_element(P,PC,user(U)) :- \+element(P:PC,user(U)), !, p_assert( element(P:PC,user(U)) ).
add_policy_element(P,PC,object(O)) :- \+element(P:PC,object(O)), !, p_assert( element(P:PC,object(O)) ).
add_policy_element(P,PC,user_attribute(UA)) :- \+element(P:PC,user_attribute(UA)), !, p_assert( element(P:PC,user_attribute(UA)) ).
add_policy_element(P,PC,object_attribute(OA)) :- \+element(P:PC,object_attribute(OA)), !, p_assert( element(P:PC,object_attribute(OA)) ).
add_policy_element(P,PC,assign(E,Attr)) :-
	( ( element(P:PC,user(E)), element(P:PC,user_attribute(Attr)) ) % must be user to user_attribute
	;
	  ( element(P:PC,object(E)), element(P:PC,object_attribute(Attr)) ) % or object to object_attribute
	),
	\+assign(P:PC,E,Attr), % must be no current assignment
	!,
	p_assert( assign(P:PC,E,Attr) ).
add_policy_element(P,PC,associate(A,R,B)) :- atom(A), atom(B), ground(R), is_list(R),
	element(P:PC,user_attribute(A)), element(P:PC,object_attribute(B)),
	\+associate(P:PC,A,R,B),
	!,
	p_assert( associate(P:PC,A,R,B) ).
add_policy_element(P,PC,associate(A,R,P,B)) :- atom(A), atom(B), ground(R), is_list(R), atom(P),
	element(P:PC,user_attribute(A)), element(P:PC,object_attribute(B)),
	\+associate(P:PC,A,R,P,B),
	!,
	p_assert( associate(P:PC,A,R,P,B) ).

%%%%%%%%%%%%%%%%
% DELETE WITH DEPENDENCY CHECKING
%

% delete_named_policy_elements/3
delete_named_policy_elements(Name,Policy,Elements) :- var(Name), !, atom(Policy), ground(Elements),
	delete_policy_elements_no_chk(Policy,Elements).
delete_named_policy_elements(Name,Policy,Elements) :- atom(Name), atom(Policy), var(Elements),
	named_policy_elements(Name,Policy,NamedElements), !,
	retractall( named_policy_elements(Name,Policy,_) ),
	delete_policy_elements_no_chk(Policy,NamedElements).
delete_named_policy_elements(_,_,_) :- !.

% delete_policy_element/2 - normalize policy to P:PC
delete_policy_element(P:PC,Element) :- !, atom(P), atom(PC), delete_policy_element(P,PC,Element).
delete_policy_element(P,Element) :- atom(P), !, policy(P,PC), delete_policy_element(P,PC,Element).

% delete_policy_element/3
delete_policy_element(P,PC,user(U)) :- !, element(P:PC,user(U)),
	% there must be no current assignment of the user
	\+assign(P:PC,U,_), !,	p_retract( element(P:PC,user(U)) ).
delete_policy_element(P,PC,object(O)) :- !, element(P:PC,object(O)),
	% there must be no current assignment of the object
	\+assign(P:PC,O,_), !,	p_retract( element(P:PC,object(O)) ).
delete_policy_element(P,PC,assign(E,Attr)) :-  !, assign(P:PC,E,Attr), !, p_retract( assign(P:PC,E,Attr) ).
delete_policy_element(P,PC,associate(A,R,B)) :- !, atom(A), ground(R), is_list(R), atom(B),
	dpl:associate(P:PC,A,R,B),
	p_retract( associate(P:PC,A,R,B) ).
delete_policy_element(P,PC,associate(A,R,Pur,B)) :- !, atom(A), ground(R), is_list(R), atom(Pur), atom(B),
	dpl:associate(P:PC,A,R,Pur,B),
	p_retract( associate(P:PC,A,R,Pur,B) ).
% consent meta-element
delete_policy_element(P,PC,consent(ConsentID)) :- !,
	delete_consent(P,PC,consent(ConsentID)).
delete_policy_element(P,PC,Element) :- dpl:policy_elements_args(EltsArgs), memberchk(Element,EltsArgs), !,
	retractall( dpl:element(P:PC, Element) ).
delete_policy_element(_,_,_). % silently ignore if conditions not met
% (see delete_policy_elements)

% delete_policy_elements/2
delete_policy_elements(P:PC,Elements) :- !, atom(P), atom(PC), delete_policy_elements(P,PC,Elements).
delete_policy_elements(P,Elements) :- atom(P), policy(P,PC), delete_policy_elements(P,PC,Elements).

% delete_policy_elements/3
delete_policy_elements(_,_,[]).
delete_policy_elements(P,PC,[Element|Elements]) :-
	( delete_policy_element(P,PC,Element) ; true ), % silently ignore delete failure (only delete multiple)
	delete_policy_elements(P,PC,Elements).

%%%%%%%%%%%%%%%%
% DELETE WITHOUT DEPENDENCY CHECKING

% delete_policy_elements_no_chk/2
delete_policy_elements_no_chk(P:PC,Elements) :- !, atom(P), atom(PC), delete_policy_elements_no_chk(P,PC,Elements).
delete_policy_elements_no_chk(P,Elements) :- atom(P), policy(P,PC), delete_policy_elements_no_chk(P,PC,Elements).

% delete_policy_elements_no_chk/3
delete_policy_elements_no_chk(_,_,[]).
delete_policy_elements_no_chk(P,PC,[Element|Elements]) :-
	delete_policy_element_no_chk(P,PC,Element), % ; true ), % silently ignore delete failure (only delete multiple)
	delete_policy_elements_no_chk(P,PC,Elements).

% delete_policy_element_no_chk/3
delete_policy_element_no_chk(P,PC,assign(E,A)) :- !, p_retract(assign(P:PC,E,A)).
delete_policy_element_no_chk(P,PC,associate(A,R,B)) :- !, p_retract(associate(P:PC,A,R,B)).
delete_policy_element_no_chk(P,PC,associate(A,R,Pur,B)) :- !, p_retract(associate(P:PC,A,R,Pur,B)).
%delete_policy_element_no_chk(P,PC,cond(C,Es)) :- is_list(Es), !,
%	retractall( dpl:cond(P:PC,C,Es) ),
%	delete_policy_element_cond_no_chk(P,PC,Es,C).
delete_policy_element_no_chk(P,PC,cond(C,E)) :- !,
	retractall( dpl:cond(P:PC,C,E) ),
	delete_policy_element_cond_no_chk(P,PC,E,C).
%delete_policy_element_no_chk(P,PC,Meta) :- isa_meta_element(Meta), !, delete_meta_element(P:PC,Meta).
delete_policy_element_no_chk(P,PC,Element) :- isa_meta_element(Element), !, p_retract( melement(P:PC, Element, true) ).
delete_policy_element_no_chk(P,PC,Element) :- !, p_retract( element(P:PC, Element) ).
delete_policy_element_no_chk(_,_,_). % silently ignore if conditions not met

% delete_policy_element_cond_no_chk/4 
delete_policy_element_cond_no_chk(_,_,[],_) :- !.
delete_policy_element_cond_no_chk(P,PC,[E|Es],C) :- !,
	delete_policy_element_no_chk(P,PC,E),
	delete_policy_element_cond_no_chk(P,PC,Es,C).
delete_policy_element_cond_no_chk(P,PC,E,_) :- !,
	delete_policy_element_no_chk(P,PC,E),
	true.
%
%%%%%%%%%%%%%%

compose_policies(P1N,P2N,P3N) :-
	policies:policy(P1N, P1R, P1G, dpl),
	policies:policy(P2N, P2R, P2G, dpl),
	concat_atom([P1R,'+',P2R],P3R),
	% TODO add the new P3R as a policy class of the new policy
	sort(P1G,P1Gs), sort(P2G,P2Gs), merge_set(P1Gs,P2Gs,P3G),
	retractall(policies:policy(P3N,_,_,_)),
	assertz(policies:policy(P3N,P3R,P3G,dpl)),
	dpl:unpack_policy(policy(P3N,P3R,P3G,dpl)),
	true.

get_current_policy(P) :- param:current_policy(P).

set_current_policy(Pname) :- atom(Pname), !,
	(   Pname == all
	->  dpl:clear_policy,
	    Name=Pname
	;   ( Pname == allnc % no clear - used only for testing
	    ->  Name=all
	    ;	Name=Pname
	    )
	),
	(   memberchk(Name,[none,all,grant,deny]) ; policy(Name,_) ), !,
	param:setparam(current_policy,Name).
	% maybe should all a silent success if policy not defined?

get_current_gpolicy(GP) :- param:current_gpolicy(GP).

set_current_gpolicy(GPname) :- atom(GPname), !,
	% should check that the policy is defined
	param:setparam(current_gpolicy,GPname).

load_policy(Pfile,PolicyName) :-
	dpl:load_decl_policy(Pfile,PolicyName).

load_policy_immediate(Pspec,PolicyName) :-
	dpl:load_decl_policy_immediate(Pspec,PolicyName).

unload_policy(P) :-
	dpl:purge_policy(P,_),
	(   param:current_policy(P)
	->  param:setparam(current_policy,none)
	;   true
	).

% dynamic add/delete condition elements
%    called from paapi, calls dpl_conditions

dynamic_add_cond_elements(Cname,CElements) :-
    add_cond_elements(Cname,CElements).
dynamic_add_cond_elements(_,_). % for now ignore add fails

dynamic_delete_cond_elements(Cname,CElements) :-
    delete_cond_elements(Cname,CElements).
dynamic_delete_cond_elements(_,_). % for now ignore delete fails

%
% RESET
%    called from paapi

preset(conditions,Name) :- conditions_reset(Name).
preset(policy,Name) :- atom(Name), !,
	(	( Name == 'dynamic' ; Name == all )
	->	dpl:re_init
	;	%dpl:purge_policy(Name,_),
		policies:policy(Name,Pr,Pg,Pt),
		dpl:unpack_policy( policy(Name,Pr,Pg,Pt) )
	).
preset(policy,Name) :- var(Name), !, Name = all,
	dpl:re_init.
preset(_,_) :- !, fail.

% Instantiation of a general meta-element DPLP
%

add_meta_element(P:PC,Element,Status) :- !, atom(P), atom(PC), add_meta_element(P,PC,Element,Status).
add_meta_element(P,Element,Status) :- atom(P), policy(P,PC), add_meta_element(P,PC,Element,Status).

% add_meta_element(P,PC,ME,Status) :-
% 	true.

% Instantiation of a Consent meta-element DPLP
%

add_consent(P:PC,Consent,Status) :- !, atom(P), atom(PC), add_consent(P,PC,Consent,Status).
add_consent(P,Consent,Status) :- atom(P), policy(P,PC), add_consent(P,PC,Consent,Status).

% add_consent(P,PC,ConsentME,Status)
% delete_consent(P,PC,ConsentID)
%
add_consent(P,PC,ConsentME,Status) :-
	% compound_name_arity(ConsentME,consent,10), % already tested
	% ConsentME = consent(ConsentID,DC,DP,App,DPOs,Purpose,DS,PDitem,PDcategory,Constraint),
	ConsentME = consent(ConsentID,_,_,_,_,_,_,_,_,_),
	StoredC = element(P:PC, ConsentME),
	(	call(dpl:StoredC)
	->	true
	;	p_assert( StoredC )
	),
	 dpl:expand_meta_element(_,ConsentME,CC,CX,_PE),
	%consent(ConsentID,DC,DP,App,DPOs,Purpose,DS,PDitem,PDcategory,Constraint,CC,CX,_PE),
	(	ensure_existence(P:PC,CX)
	->	true
	;	fail % placeholder for action when not all prerequisite context elements exist TODO
	),
	% add_policy_elements(P,PC,CC),
	dpl:unpack_policy_elements(P:PC,CC),
	retractall( dpl:named_policy_elements(ConsentID,P,_) ),
	assert( named_policy_elements(ConsentID, P, [ConsentME|CC]) ),
	Status = success,
	true.

delete_consent(P:PC,Consent) :- !, atom(P), atom(PC), delete_consent(P,PC,Consent).
delete_consent(P,Consent) :- atom(P), policy(P,PC), delete_consent(P,PC,Consent).

delete_consent(P,_PC,ConsentShort) :-
	(	ConsentShort = consent(ConsentID)
	->	true
	;	ConsentShort = ConsentID
	),
	delete_named_policy_elements(ConsentID,P,_),
	true.
%delete_consent(_,_,_). % fail silently

/*

delete_dplp_policy_base(P:PC, PC) :- !, atom(P), atom(PC),
	delete_named_policy_elements(PC,P,_).

delete_data_controller(P:PC, DC_ID) :-
	% forall(
	% 	consent(ConsentID,DC_ID,DP_ID,App,DPOs,Purpose,DS_ID,PDI_ID,PDC_ID,Constraint),
	%	delete_consent(PPC,consent(ConsentID))
	% )
	forall(element(P:PC, consent(Cid,DC_ID,_,_,_,_,_,_,_,_)), delete_consent(P:PC,Cid)),
	(	named_policy_elements(DC_ID,P,_DCEs)
	->	delete_named_policy_elements(DC_ID,P,_)
	;	true
	),
	% following assumes that a data processor is only assigned to one data controller
	% if this assumption ever is no longer valid then reference counts will need to be added
	forall( (assign(P:PC, DP_ID, DC_ID), user(DP_ID)), delete_data_processor(P:PC, DP_ID) ),
	element(P:PC, data_controller(DC_ID, _DC_POLICY)),
	true.

delete_data_processor(P:PC, DP_ID) :-
	% forall(
	% 	consent(ConsentID,DC_ID,DP_ID,App,DPOs,Purpose,DS_ID,PDI_ID,PDC_ID,Constraint),
	%	delete_consent(PPC,consent(ConsentID))
	% )
	forall(element(P:PC, consent(Cid,_,DP_ID,_,_,_,_,_,_,_)), delete_consent(P:PC,Cid)),
	delete_named_policy_elements(DP_ID,P,_).

delete_data_subject(P:PC, DS_ID) :-
	% forall(
	% 	consent(ConsentID,DC_ID,DP_ID,App,DPOs,Purpose,DS_ID,PDI_ID,PDC_ID,Constraint),
	%	delete_consent(PPC,consent(ConsentID))
	% )
	forall(element(P:PC, consent(Cid,_,_,_,_,_,DS_ID,_,_,_)), delete_consent(P:PC,Cid)),
	forall( assign(PDI_ID,DS_ID), delete_data_item(P:PC, PDI_ID) ),
	delete_named_policy_elements(DS_ID,P,_).
	%findall(PDI_ID, assign(PDI_ID,DS_ID), PDIs),
	%forall( member(PDI,PDIs), delete_data_item(PPC, PDI) ).

delete_data_item(P:PC, PDI_ID) :-
	% delete any consents that the data item is directly involved in
	% forall(
	% 	consent(ConsentID,DC_ID,DP_ID,App,DPOs,Purpose,DS_ID,PDI_ID,PDC_ID,Constraint),
	%	delete_consent(PPr,consent(ConsentID))
	% )
	forall(element(P:PC, consent(Cid,_,_,_,_,_,_,PDI_ID,_,_)), delete_consent(P:PC,Cid)),
	p_retract( element(P:PC, data_item(PDI_ID,_,_))).

*/


% ensure_existence/2 - ensure_existence(P:PC, RequiredElts)
%
ensure_existence(P,E) :- atom(P), !, policy(P,PC), ensure_existence(P:PC,E).

ensure_existence(_,[]) :- !.
ensure_existence(P,[E|Es]) :- !, ensure_existence(P,E), ensure_existence(P,Es).
ensure_existence(P,assign(A,B)) :- dpl:assign(P,A,B), !.
ensure_existence(P,associate(A,B,C)) :- dpl:associate(P,A,B,C), !.
ensure_existence(P,associate(A,B,C,D)) :- dpl:associate(P,A,B,C,D), !.
ensure_existence(P,E) :- dpl:element(P,E), !.

% ensure_existence/3 - ensure_existence(P:PC, RequiredElts, MissingElts)
%
ensure_existence(_,[],[]) :- !.
ensure_existence(P,[E|Es],M) :- !,
	(	ensure_existence(P,E)
	->	ensure_existence(P,Es,M)
	;	ensure_existence(P,Es,Ms), M=[E|Ms]
	).
	