% Declarative Policy Language

:- module(dpl,[
		policy/2,
	    element/2, assign/3, associate/4,
	    associate/5, % DPLP
	    cond/3, conditions/2,
	    gg_policy/1, gg_element/2, gg_associate/4, gg_gateway/2,
	    cc_policy/1, cc_element/2, cc_associate/4, cc_assign/3,
	    cc_external_attribute/2, cc_local_cloud_gateway/3,
		policy_class/2,
		load_decl_policy/2, load_decl_policy_immediate/2, save_decl_policy/2,
		object_attribute/2, object_oattribute/2, object_oattribute_nd/2, object/2, object/8,
		user_attribute/2, user_uattribute/2, user/2,
	    purpose/2, operation/2, data_type/2, object_class/2, % DPLP
	    decl2imp/2, imp2decl/3,
		cmdTerms2policy/2,
	    get_id_operation_set/3,
		/*consent/11,*/ consent/13, named_policy_elements/3,
		p_assert/1, p_retract/1
	    ]).

:- use_module(dpl_conditions).
:- use_module(policies).
:- use_module(policyio).
:- use_module('COM/param').
:- use_module('EPP/epp_cpa').

% The lightweight policy model
%
% Cache of loaded policies as asserted clauses
% policy(PolicyName, PolicyRoot)
%
% element(PolicyName:PolicyRoot, Element)
%   where Element is: user(_), user_attribute(_)
%     object(_), object_attribute(_), policy_class(_),
%     opset(OpSetName, OpList), operation(OpName, OpInfo), operation(OpName),
%     composed_policy(P1,P2,Pcomposed),
%     connector('pm') (is an element of every policy)
%     assign(PolicyName:PolicyRoot, PolicyElement1, PolicyElement2)
%     associate(PolicyName:PolicyRoot, UserAttr, OpSetName, ObjectAttr)
%     associate(PolicyName:PolicyRoot, UserAttr, OpSetName, Purpose, ObjectAttr)
%     cond(Condition,Element)
%     conditions(ConditionPreds)
%     external_attribute(_)

:- dynamic policy/2.
:- dynamic element/2, assign/3, associate/4, cond/3, conditions/2.
:- dynamic associate/5.  % DPLP
:- dynamic consent/11.   % DPLP
:- dynamic gg_policy/1, gg_element/2, gg_associate/4, gg_gateway/2.
:- dynamic cc_policy/1, cc_element/2, cc_associate/4, cc_assign/3.
:- dynamic cc_external_attribute/2, cc_local_cloud_gateway/3.

:- dynamic named_policy_elements/3. % name, policy, elements

% policy types
%
policy_types([dpl,dplp,s4p]). % DPLP

% policy(PolicyName, PolicyClass)
% policy(PolicyName, PolicyClass, PolicyType) defined in module policies
%

policy(Pname,Pclass) :- policy(Pname,Pclass,_). % get just the Policy:PolicyClass
%
% Policy elements for privacy
%
% DPLP meta-elements
%	dplp_policy_base(PolicyClass,Defs)
%               [
%			definitions(onto),
%                   include(onto),
%			policy_class(cpol_ex),
%			assign(cpol_ex,'PM'),
%			user_attribute(data_controllers),
%			object_attribute(data_subjects),
%			assign(data_controllers,cpol_ex),
%			assign(data_subjects,cpol_ex),
%			connector('PM')
%               ]
%
%	data_controller(DC_ID, DC_POLICY)
%		[ user_attribute(DC_ID), assign(DC_ID, data_controllers), privacy_policy(DC_ID, DC_POLICY) ]
%
%	data_processor(DP_ID, DP_POLICY, DC_ID)
%		[ user(DP_ID), assign(DP_ID, DC_ID), privacy_policy(DP_ID, DP_POLICY) ]
%
%	data_subject(DS_ID, DS_PERSONAL_DATA_ITEMS, DS_PREFERENCE)
%		[ object_attribute(DS_ID), assign(DS_ID, data_subjects),
%        data_item(PDI_ID, PDC_ID, DS_ID),
%         . . .
%        privacy_preference(DS_ID, DS_PREFERENCE)
%      ]
%
%	data_item(PDI_ID, PDC_ID, DS_ID)
%		[ object(PDI_ID), assign(PDI_ID, DS_ID), assign(PDI_ID, PDC_ID) ]
%
%	consent(ConsentID,DC,DP,App,DPOs,Purpose,DS,PDitem,PDcategory,Constraint)
%     ConsentElements = [
%         user(DP),
%         user_attribute(DC),
%         user_attribute(data_controllers),
%
%         object_attribute(PDitem),
%         object_attribute(PDcategory),
%         object_attribute(DS),
%         object_attribute(data_subjects),
%
%         assign(DP,DC),
%         assign(DC,data_controllers),
%
%         assign(PDitem,PDcategory),
%         assign(PDitem,DS),
%         assign(DS,data_subjects),
%
%         associate(DP,DPOs,Purpose,Constraint,DS)
%     ],
%     ConditionElements = [
%         condition_pred(Predicate)
%     ],
%     Options = [
%         constraint(Constraint)
%     ]
%
% Consent Meta-element (add/delete a DC/DP privacy policy)
%     a set of PDCs and NPDCs (subset of PDC+NPDC from ontology)
%     a Purpose (from ontology)
%     a set of DPOs (from ontology) an Application?
%
%
% consent/13 predicate returns: ConsentCore, ContextElts, PolicyElts
%  superceded by expand_meta_element/5 for consent
consent(ConsentID,DC,DP,App,DPOs,Purpose,DS,PDitem,PDcategory,Constraint,
	ConsentCore, ContextElts, PolicyElts) :-
	( App = app(_AppID,_AppOps,_AppPurpose) ; true ),
	ContextElts = [
        user_attribute(data_controllers),
        object_attribute(data_subjects),

        user_attribute(DC),
        assign(DC,data_controllers),
        user(DP),
        assign(DP,DC),

        object_attribute(DS),
        assign(DS,data_subjects),

        object(PDitem),
        object_attribute(PDcategory),
        assign(PDitem,PDcategory),
        assign(PDitem,DS)
   ],
    ConsentCore = [
        user_attribute(CUA),
        object_attribute(COA),
        assign(DP,CUA),
        assign(CUA,DC),
        assign(PDitem,COA),
        assign(COA,DS),
		CAssoc
    ],
	(	is_list(DPOs)
	->	ADPOs = DPOs
	;	ADPOs = [DPOs]
	),
    %context_elts(PolicyElts,ConsentCore,ContextElts),
    append(ContextElts,ConsentCore,PolicyElts),
    atom_concat(cID_,IDnum,ConsentID),
    atom_concat(cUA_,IDnum,CUA),
    atom_concat(cOA_,IDnum,COA),
    atom_concat(cPred_,IDnum,CPred),
	CPred == CPred,
	%ConsentCond =.. [CPred,Constraint],
	%ConsentCond == ConsentCond,
	ConsentCond = Constraint,
	% TODO - can add here to emit associate without cond when Constraint == true
	CAssoc = cond( ConsentCond,	associate(CUA,ADPOs,Purpose,COA) ),
    %format('~nCUA=~w COA=~w ConsentCond=~w~n',[CUA,COA,ConsentCond]),
    %format('~nConsentCore: ~w~n',[ConsentCore]),
    %format('~nContextElts: ~q~n',[ContextElts]), nl,
	!.

% context_elts(PolicyElts (PE),ConsentCore (CC), ContextElts (CX))
% assumes that all members of ConsentCore are in PolicyElts
context_elts([],_,[]) :- !.
context_elts(PEs,[],PEs) :- !.
context_elts([PE|PEs],CCs,CXs) :- elt_member(PE, CCs), !,
	context_elts(PEs,CCs,CXs).
context_elts([PE|PEs],CCs,[PE|CXs]) :-
	context_elts(PEs,CCs,CXs), !.

elt_member(_,[]) :- !, fail.
elt_member(E,[Elt|_]) :- E == Elt, !. % check w/o unification!
elt_member(E,[_|Elts]) :- elt_member(E,Elts).

% consent(ConsentID,DC,DP,DPOs,Purpose,DS,PDitem,PDcategory,Constraint,PE) :- true.
% e.g. consent(cid_123,dc_1,dp_11,[op1,op3],purp_21,ds_7,ds_7_street,dcat_addr,true,CE).

% EXPAND META-ELEMENTS
%
%	data_controller(DC_ID, DC_POLICY)
%		[ user_attribute(DC_ID), assign(DC_ID, data_controllers), privacy_policy(DC_ID, DC_POLICY) ]
%
%	data_processor(DP_ID, DP_POLICY, DC_ID)
%		[ user(DP_ID), assign(DP_ID, DC_ID), privacy_policy(DP_ID, DP_POLICY) ]
%
%	data_subject(DS_ID, DS_PERSONAL_DATA_ITEMS, DS_PREFERENCE)
%		[ object_attribute(DS_ID), assign(DS_ID, data_subjects),
%        data_item(PDI_ID, PDC_ID, DS_ID),
%         . . .
%        privacy_preference(DS_ID, DS_PREFERENCE)
%      ]
%
%	data_item(PDI_ID, PDC_ID, DS_ID)
%		[ object(PDI_ID), assign(PDI_ID, DS_ID), assign(PDI_ID, PDC_ID) ]
%
%	consent(ConsentID,DC,DP,App,DPOs,Purpose,DS,PDitem,PDcategory,Constraint)
%     [ user_attribute(CUA), object_attribute(COA), assign(DP,CUA), assign(CUA,DC),
%       assign(PDitem,COA), assign(COA,DS), cond( ConsentCond, associate(CUA,ADPOs,Purpose,COA) ) ]

expand_meta_element(_, dplp_policy_base(PolicyClass, GlobalDefs), CoreElts, ContextElts, PolicyElts ) :-
	 ContextElts = [
        purpose('Purpose'),
        operation('DataProcessing'),
        data_type('PersonalDataCategory')
    ],
	 CoreElts1 = [
        %include(GlobalDefs),
		connector('PM'),
        policy_class(PolicyClass),
        assign(PolicyClass,'PM'),
        user_attribute(data_controllers),
        object_attribute(data_subjects),
        assign(data_controllers,PolicyClass),
        assign(data_subjects,PolicyClass),
        assign('Purpose',PolicyClass),
        assign('DataProcessing',PolicyClass),
        assign('PersonalDataCategory',PolicyClass)
	],
	(	atom(GlobalDefs), policy(GlobalDefs,_)
	->	CoreElts = [include(GlobalDefs)|CoreElts1]
	;	CoreElts = CoreElts1
	),
	append(ContextElts,CoreElts,PolicyElts).

expand_meta_element( _, data_controller(DC_ID, DC_POLICY), CoreElts, ContextElts, PolicyElts ) :-
	ContextElts = [
		user_attribute(data_controllers)
	],
	CoreElts = [
		user_attribute(DC_ID),
		assign(DC_ID, data_controllers),
		privacy_policy(DC_ID, DC_POLICY)
	],
	append(ContextElts,CoreElts,PolicyElts).

expand_meta_element( _, data_processor(DP_ID, DP_POLICY, DC_ID), CoreElts, ContextElts, PolicyElts ) :-
	ContextElts = [
		user_attribute(DC_ID)
	],
	CoreElts = [
		user(DP_ID),
		assign(DP_ID, DC_ID),
		privacy_policy(DP_ID, DP_POLICY)
	],
	append(ContextElts,CoreElts,PolicyElts).

expand_meta_element( _, data_subject(DS_ID, DS_PDIs, DS_PREFERENCE), CoreElts, ContextElts, PolicyElts ) :-
	ContextElts = [
		object_attribute(data_subjects)
	],
	CoreElts1 = [
		object_attribute(DS_ID),
		assign(DS_ID, data_subjects),
	    %data_item(PDI_ID, PDC_ID, DS_ID),  EXPANDED
	    %    . . .
	    privacy_preference(DS_ID, DS_PREFERENCE)
	],
	maplist(pdi_elt(DS_ID), DS_PDIs, PDIElts),
	findall(PDIC, (member(PDIE,PDIElts), expand_meta_element(_,PDIE,PDIC,_,_)), PDICs ),
	append([CoreElts1,PDICs],CoreElts2),
	flatten(CoreElts2,CoreElts),
	append(ContextElts,CoreElts,PolicyElts).

expand_meta_element( _, data_item(PDI_ID, PDC_ID, DS_ID), CoreElts, ContextElts, PolicyElts ) :-
	ContextElts = [
		object_attribute(PDC_ID), % should already exist if the categories are pre-defined
		object_attribute(DS_ID)
	],
	CoreElts = [
		object(PDI_ID),
		assign(PDI_ID, DS_ID),
		assign(PDI_ID, PDC_ID)
	],
	append(ContextElts,CoreElts,PolicyElts).

expand_meta_element(_, consent(ConsentID,DC,DP,App,DPOs,Purpose,DS,PDitem,PDcategory,Constraint), CoreElts, ContextElts, PolicyElts ) :-
	ContextElts = [
        user_attribute(data_controllers),
        object_attribute(data_subjects),

        user_attribute(DC),
        assign(DC,data_controllers),
        user(DP),
        assign(DP,DC),

        object_attribute(DS),
        assign(DS,data_subjects),

        object(PDitem),
        object_attribute(PDcategory),
        assign(PDitem,PDcategory),
        assign(PDitem,DS)
	],
	CoreElts = [
        user_attribute(CUA),
        object_attribute(COA),
        assign(DP,CUA),
        assign(CUA,DC),
        assign(PDitem,COA),
        assign(COA,DS),
		CAssoc
	],
	( App = app(_AppID,_AppOps,_AppPurpose) ; true ),
	(	is_list(DPOs)
	->	ADPOs = DPOs
	;	ADPOs = [DPOs]
	),
	append(ContextElts,CoreElts,PolicyElts),
    atom_concat(cID_,IDnum,ConsentID),
    atom_concat(cUA_,IDnum,CUA),
    atom_concat(cOA_,IDnum,COA),
    atom_concat(cPred_,IDnum,CPred),
	CPred == CPred,
	%ConsentCond =.. [CPred,Constraint],
	%ConsentCond == ConsentCond,
	ConsentCond = Constraint,
	% TODO - can add here to emit associate without cond when Constraint == true
	CAssoc = cond( ConsentCond,	associate(CUA,ADPOs,Purpose,COA) ),
	!.

pdi_elt( DS_ID, PDI_ID:PDC, data_item(PDI_ID, PDC, DS_ID) ).

% ensure_existence/2 - ensure_existence(P:PC, RequiredElts)
%
ensure_existence(P,E) :- atom(P), !, policy(P,PC), ensure_existence(P:PC,E).

ensure_existence(_,[]) :- !.
ensure_existence(P,[E|Es]) :- !, ensure_existence(P,E), ensure_existence(P,Es).
ensure_existence(P,assign(A,B)) :- assign(P,A,B), !.
ensure_existence(P,associate(A,B,C)) :- associate(P,A,B,C), !.
ensure_existence(P,associate(A,B,C,D)) :- associate(P,A,B,C,D), !.
ensure_existence(P,E) :- element(P,E), !.

% ensure_existence/3 - ensure_existence(P:PC, RequiredElts, MissingElts)
%
ensure_existence(_,[],[]) :- !.
ensure_existence(P,[E|Es],M) :- !,
	(	ensure_existence(P,E)
	->	ensure_existence(P,Es,M)
	;	ensure_existence(P,Es,Ms), M=[E|Ms]
	).

% expand_meta_elements/3
expand_meta_elements(_,[],[]) :- !.
expand_meta_elements(P:PC,[ME|MEs],EMEs) :-
	expand_meta_element(P:PC,ME,EME,_,_),
	append(EME,EMEs1,EMEs),
	expand_meta_elements(P:PC,MEs,EMEs1).

% expand_meta_elements/2
expand_meta_elements(P:PC,Elements) :-
	findall( PBEs,
		(	element(P:PC,dplp_policy_base(PC, REFS)),
			expand_meta_element(P:PC,dplp_policy_base(PC, REFS),PBEs,_,_),
			retractall( named_policy_elements(PC,P,_) ),
			assert( named_policy_elements(PC,P,[dplp_policy_base(PC, REFS)|PBEs]) ),
			unpack_policy_elements(P:PC,PBEs)
		),
		PBEss),
	(	length(PBEss,PBcount), PBcount > 1
	->	true % TODO there shouldn't be more than one dplp policy base
	;	true
	),
	findall( DCEs,
		(	element(P:PC,data_controller(DC_ID, DC_POLICY)),
			expand_meta_element(P:PC,data_controller(DC_ID,DC_POLICY),DCEs,_,_),
			retractall( named_policy_elements(DC_ID,P,_) ),
			assert( named_policy_elements(DC_ID,P,[data_controller(DC_ID,DC_POLICY)|DCEs]) ),
			unpack_policy_elements(P:PC,DCEs)
		),
		DCEss),
	findall( DPEs,
		(	element(P:PC,data_processor(DP_ID, DP_POLICY, DC_ID)),
			expand_meta_element(P:PC,data_processor(DP_ID,DP_POLICY,DC_ID),DPEs,_,_),
			retractall( named_policy_elements(DP_ID,P,_) ),
			assert( named_policy_elements(DP_ID,P,[data_processor(DP_ID,DP_POLICY,DC_ID)|DPEs]) ),
			unpack_policy_elements(P:PC,DPEs)
		),
		DPEss),
	findall( DSEs,
		(	element(P:PC,data_subject(DS_ID, DS_PDIs, DS_PREFERENCE)),
			expand_meta_element(P:PC,data_subject(DS_ID,DS_PDIs,DS_PREFERENCE),DSEs,_,_),
			retractall( named_policy_elements(DS_ID,P,_) ),
			assert( named_policy_elements(DS_ID,P,[data_subject(DS_ID,DS_PDIs,DS_PREFERENCE)|DSEs]) ),
			unpack_policy_elements(P:PC,DSEs)
		),
		DSEss),
	findall( PDIEs,
		(	element(P:PC,data_item(PDI_ID, PDC_ID, DS_ID)),
			expand_meta_element(P:PC,data_item(PDI_ID,PDC_ID,DS_ID),PDIEs,_,_),
			retractall( named_policy_elements(PDI_ID,P,_) ),
			assert( named_policy_elements(PDI_ID,P,[data_item(PDI_ID,PDC_ID,DS_ID)|PDIEs]) ),
			unpack_policy_elements(P:PC,PDIEs)
		),
		PDIEss),
	findall( CEs,
		(	element(P:PC,consent(ConsentID,DC,DP,App,DPOs,Purpose,DS,PDitem,PDcategory,Constraint)),
			expand_meta_element(P:PC,consent(ConsentID,DC,DP,App,DPOs,Purpose,DS,PDitem,PDcategory,Constraint),CEs,CXs,_),
			(	ensure_existence(P:PC,CXs)
			->	true
			;	true % placeholder for action when not all prerequisite context elements exist TODO
			),
			retractall( named_policy_elements(ConsentID,P,_) ),
			assert(	named_policy_elements(ConsentID, P,
					       [consent(ConsentID,DC,DP,App,DPOs,Purpose,DS,PDitem,PDcategory,Constraint)|CEs]
					)
			),
			unpack_policy_elements(P:PC,CEs)
		),
		CEss),
	append( [DCEss,DPEss,DSEss,PDIEss,CEss], Elements1 ), flatten(Elements1,Elements).

%
% The core DPL and DPLP language
%
%

policy_elements([
		user,user_attribute,object,data_type,object_class,object_attribute,policy_class,
		operation,opset,composed_policy,assign,associate,connector,
		cond,conditions,external_attribute,
/* DPLP */	 purpose,retention,privacy_policy,privacy_preference,include,definitions,
/* DPLP */	 consent,data_subject,data_processor,data_controller /* meta-elements */
]).

policy_elements_args([user(_),user_attribute(_),
		      object(_),object(_,_),object(_,_,_),object(_,_,_,_,_,_,_),
			  object_class(_),object_class(_,_),data_type(_),data_type(_,_), % DPLP
		      object_attribute(_),policy_class(_),operation(_),operation(_,_),
		      opset(_,_),composed_policy(_,_,_),assign(_,_),associate(_,_,_),
/* DPLP */	  associate(_,_,_,_),
		      connector(_),cond(_,_),conditions(_),external_attribute(_),
/* DPLP */	  purpose(_),retention(_,_),privacy_policy(_,_),privacy_preference(_,_),
/* DPLP */    dplp_policy_base(_,_), include(_), definitions(_),
/* DPLP */	  data_controller(_,_), data_processor(_,_,_), data_subject(_,_,_), data_item(_,_,_),
/* DPLP */	  consent(_,_,_,_,_,_,_,_,_,_)]).

conditional_policy_elements_args([assign(_,_),associate(_,_,_),
/* DPLP */	      associate(_,_,_,_)]).

:- dynamic dpl_initialized/1.

dpl_initialized(false).

init:- param:initialized(true), !. % dpl_initialized(true), !. % already initialized
init :-
	%forall( policies:policy(Pn,Pr,Pg), unpack_policy( policy(Pn,Pr,Pg,dpl) ) ),
	forall( policies:policy(Pn,Pr,Pg,Pt), unpack_policy( policy(Pn,Pr,Pg,Pt) ) ), % DPLP
	forall( policies:gg_policy(Pn,Pg), unpack_policy( gg_policy(Pn,Pg) ) ),
	(   gg_policy(GPname)
	->  param:setparam(current_gpolicy,GPname)
	;   true
	),
	forall( policies:cc_policy(Pn,Pg), unpack_policy( cc_policy(Pn,Pg) ) ),
	(   cc_policy(CPname)
	->  param:setparam(current_cpolicy,CPname)
	;   true
	),
	dpl_conditions:init,
	retractall( dpl_initialized(_) ), assert( dpl_initialized(true) ).

re_init :- un_init, init.

un_init :-
	clear_policy,
	retractall( dpl_initialized(_) ), assert( dpl_initialized(false) ).

clear_policy :-
	purge_policy(_,_), % should have same effect as following 3 lines
	%retractall(policy(_,_)), retractall(element(_,_)),
	%retractall(assign(_,_,_)), retractall(associate(_,_,_,_)),
	%retractall(cond(_,_,_)), retractall(conditions(_,_)),
	param:setparam(current_policy,none),
	true.

check_valid_conditional_element(Rule) :-
	conditional_policy_elements_args(CEA),
	memberchk(Rule,CEA).


decl2imp(Dfile,Ifile) :-
	load_decl_policy(Dfile,PolicyName),
	save_as_cmds(PolicyName,Ifile).

save_as_cmds(PolicyName,CmdFile) :-
	policyio:policy_cmdstrs(PolicyName,CmdStrs),
	(   param:verbose(on)
	->  ui:display_listq(CmdStrs,1)
	;   true
	),
	policyio:save_cmdstrs_to_file(CmdFile,CmdStrs).

imp2decl(_Ifile,_Policy,_Dfile) :-
	% TODO
	true.

load_decl_policy(Pfile,PolicyName) :-
	policyio:load_term(Pfile,PolicyTerm),
	load_decl_policy_common(PolicyTerm,PolicyName).

load_decl_policy_immediate(PolicyAtom,PolicyName) :-
	read_term_from_atom(PolicyAtom,PolicyTerm,[]),
	load_decl_policy_common(PolicyTerm,PolicyName).

load_decl_policy_common(PolicyTerm,PolicyName) :-
	PolicyTerm = policy(PolicyName,PolicyRoot,PolicyElements),
	atom(PolicyName), atom(PolicyRoot), is_list(PolicyElements), !,
	retractall(policies:policy(PolicyName,_,_,_)),
	PolicyTermT = policy(PolicyName,PolicyRoot,PolicyElements,dpl),
	assertz(policies:PolicyTermT),
	unpack_policy(PolicyTermT).
load_decl_policy_common(PolicyTerm,PolicyName) :-
	PolicyTerm = policy(PolicyName,PolicyRoot,PolicyElements,PolicyType),
	atom(PolicyType), policy_types(PTs), memberchk(PolicyType,PTs),
	atom(PolicyName), atom(PolicyRoot), is_list(PolicyElements), !,
	retractall(policies:policy(PolicyName,_,_,_)),
	assertz(policies:PolicyTerm),
	unpack_policy(PolicyTerm).

save_decl_policy(Pfile,PolicyTerm) :-
	PolicyTerm = policy(_PolicyName,_PolicyRoot,_PolicyGraph,_PolicyType),
	policyio:save_term(Pfile,PolicyTerm),
	true.

cmdTerms2policy(_CmdTerms,_Policy) :- true.
	% UNIMPLEMENTED compatibility with PM
%	cmdTerms_policyElts(CmdTerms,PolicyElements),
%	Policy = policy(PolicyName,PolicyRoot,PolicyElements),
%	true.

% cmdTerms_policyElts([],[]).
% cmdTerms_policyElts([Term|Terms],[Elt|Elts]) :-	true.

%

unpack_policy(policy(PolicyName,PolicyRoot,PolicyElements)) :- !,
	unpack_policy(policy(PolicyName,PolicyRoot,PolicyElements,dpl)).
% following will need to split-out s4p and maybe DPLP
unpack_policy(policy(PolicyName,PolicyRoot,PolicyElements,PolicyType)) :-
	purge_policy(PolicyName,PolicyType), % TODO - consider making second arg _ (global policy names)
	assertz( policy(PolicyName,PolicyRoot,PolicyType) ),
	unpack_policy_elements_with_meta_expansion(PolicyName:PolicyRoot,PolicyElements), !,
	%unpack_policy_elements(PolicyName:PolicyRoot,PolicyElements), !,
	perform_static_policy_checks(PolicyName:PolicyRoot,PolicyType).

unpack_policy( gg_policy(GGpolicyName,GGpolicyElements) ) :-
	purge_ggpolicy(GGpolicyName),
	assertz( gg_policy(GGpolicyName) ),
	unpack_gpolicy_elements(GGpolicyName,GGpolicyElements), !,
	perform_static_gpolicy_checks(GGpolicyName).

unpack_policy( cc_policy(CCpolicyName,CCpolicyElements) ) :-
	purge_ccpolicy(CCpolicyName),
	assertz( cc_policy(CCpolicyName) ),
	unpack_cpolicy_elements(CCpolicyName,CCpolicyElements), !,
	perform_static_cpolicy_checks(CCpolicyName).

% note that PName below is P:PC

% unpack_policy_elements_with_meta_expansion/2
unpack_policy_elements_with_meta_expansion(PName,PolicyElements) :-
	unpack_policy_elements(PName,PolicyElements),
	expand_meta_elements(PName,_ExpandedElements),
	%unpack_policy_elements(PName,ExpandedElements),
	true.

unpack_policy_elements(_,[]).
unpack_policy_elements(PName,[assign(I,A)|PolElts]) :- !, % ASSIGN
	p_assert( assign(PName,I,A) ),
	unpack_policy_elements(PName,PolElts).
unpack_policy_elements(PName,[associate(I,M,A)|PolElts]) :- !, % ASSOCIATE/3
	p_assert( associate(PName,I,M,A) ),
	unpack_policy_elements(PName,PolElts).
unpack_policy_elements(PName,[associate(I,M,P,A)|PolElts]) :- !, % DPLP ASSOCIATE/4
	p_assert( associate(PName,I,M,P,A) ),
	unpack_policy_elements(PName,PolElts).
unpack_policy_elements(PName,[cond(Cond,Rules)|PolElts]) :- is_list(Rules), !, % COND
	unpack_policy_elements_cond(PName,Rules,Cond),
	unpack_policy_elements(PName,PolElts).
unpack_policy_elements(PName,[cond(Cond,Rule)|PolElts]) :- !, % COND
	unpack_policy_element_cond(PName,Rule,Cond),
	unpack_policy_elements(PName,PolElts).
unpack_policy_elements(PName,[conditions(Conditions)|PolElts]) :- is_list(Conditions), !,
	(   conditions(PName,_)                               % CONDITIONS
	->  % only accept one conditions declaration per policy
	    format('Only one conditions declaration permitted per policy~n')
	;   p_assert( conditions(PName,Conditions) ),
	    request_conditions(Conditions)
	),
	unpack_policy_elements(PName,PolElts).
% unpack_policy_elements(PName,[PolElt|PolElts]) :- functor(PolElt,consent,10), !,
%	compound_name_arguments(PolElt, consent, Cargs),
%	compound_name_arguments(Cterm, consent, [PName|Cargs]),
%	assertz( Cterm ),
%	unpack_policy_elements(PName,PolElts).
unpack_policy_elements(PName,[definitions(DName)|PolElts]) :- !, % for now, noop, could be syn for include
	% policies:policy(DName,_,_DpolElts,_),
	% unpack_policy_elements(PName,DpolElts),
	p_assert( element(PName, definitions(DName))),
	unpack_policy_elements(PName,PolElts).
unpack_policy_elements(PName,[include(IName)|PolElts]) :- !,
	policies:policy(IName,_,IpolElts,_),
	unpack_policy_elements(PName,IpolElts),
	unpack_policy_elements(PName,PolElts).
unpack_policy_elements(PName,[PolElt|PolElts]) :-
	policy_elements_args(PolEltsArgs),
	memberchk(PolElt,PolEltsArgs), !,
	p_assert( element(PName,PolElt) ),
	unpack_policy_elements(PName,PolElts).
unpack_policy_elements(Pname,[PolElt|PolElts]) :- !,
	format('error: unpack-policy_elements, unknown: ~q~n',PolElt), % TODO - unknown element temporary for testing
	unpack_policy_elements(PName,PolElts).
unpack_policy_elements(PName,[_|PolElts]) :- % skip unknown element
	unpack_policy_elements(PName,PolElts).

unpack_policy_elements_cond(_,[],_) :- !.
unpack_policy_elements_cond(PName,[PolElt|PolElts],Cond) :-
	unpack_policy_element_cond(PName,PolElt,Cond),
	unpack_policy_elements_cond(PName,PolElts,Cond).

unpack_policy_element_cond(_,cond(_,_),_) :- !.
	% skip a nested cond, maybe generate a diagnostic message here
unpack_policy_element_cond(PName,Element,Cond) :-
	check_valid_conditional_element(Element), !,
	unpack_policy_elements(PName,[Element]),
	p_assert( cond(PName,Cond,Element) ).
unpack_policy_element_cond(_,_,_). % skip invalid conditional element


unpack_gpolicy_elements(_,[]).
unpack_gpolicy_elements(PName,[gateway(G)|PolElts]) :- !,
	p_assert( gg_gateway(PName,G) ),
	unpack_gpolicy_elements(PName,PolElts).
unpack_gpolicy_elements(PName,[gg_associate(I,M,A)|PolElts]) :- !,
	p_assert( gg_associate(PName,I,M,A) ),
	unpack_gpolicy_elements(PName,PolElts).
unpack_gpolicy_elements(PName,[_,PolElts]) :- % skip unknown element
	unpack_gpolicy_elements(PName,PolElts).


unpack_cpolicy_elements(_,[]).
unpack_cpolicy_elements(PName,[cc_assign(I,A)|PolElts]) :- !,
	p_assert( cc_assign(PName,I,A) ),
	unpack_cpolicy_elements(PName,PolElts).
unpack_cpolicy_elements(PName,[cc_associate(I,M,A)|PolElts]) :- !,
	p_assert( cc_associate(PName,I,M,A) ),
	unpack_cpolicy_elements(PName,PolElts).
unpack_cpolicy_elements(PName,[external_attribute(A)|PolElts]) :- !,
	p_assert( cc_external_attribute(PName,A) ),
	unpack_cpolicy_elements(PName,PolElts).
unpack_cpolicy_elements(PName,[local_cloud_gateway(LC,G)|PolElts]) :- !,
	p_assert( cc_local_cloud_gateway(PName,LC,G) ),
	unpack_cpolicy_elements(PName,PolElts).
unpack_cpolicy_elements(PName,[_,PolElts]) :- % skip unknown element
	unpack_cpolicy_elements(PName,PolElts).

purge_policy(PolicyName,PolicyType) :-
	retractall( dpl:named_policy_elements(_,PolicyName,_) ),
	retractall( consent(PolicyName:_,_,_,_,_,_,_,_,_,_,_)), % TODO - won't be needed
	retractall(policy(PolicyName,_,PolicyType)),
	retractall(element(PolicyName:_,_)),
	retractall(assign(PolicyName:_,_,_)),
	retractall(associate(PolicyName:_,_,_,_)),
	retractall(associate(PolicyName:_,_,_,_,_)),
	retractall(cond(PolicyName:_,_,_)),
	retractall(conditions(PolicyName:_,_)).

purge_ggpolicy(PolicyName) :-
	retractall(gg_policy(PolicyName)),
	retractall(gg_gateway(PolicyName,_)),
	retractall(gg_associate(PolicyName,_,_,_)).

purge_ccpolicy(PolicyName) :-
	retractall(cc_policy(PolicyName)),
	retractall(cc_assign(PolicyName,_,_)),
	retractall(cc_associate(PolicyName,_,_,_)),
	retractall(cc_external_attribute(PolicyName,_)),
	retractall(cc_local_cloud_gateway(PolicyName,_,_)).

% assert and retract policy fact
p_assert(PI) :-	%format('asserting ~q~n',[PI]),
	% TODO - Better to have this check enabled but test script 07c fails due to non-ground rule in cpolicy.
	% TODO - The issue is how to allow "don't care" arguments to a condition predicate in a cond. Maybe "-"?
	(	true % ground(PI)
	->	(	clause(PI, true) -> true ;	assertz( PI ) )
	;	true % silently ignore nonground elements
	).

p_retract(PI) :- %format('retracting ~q~n',[PI]),
	retractall(PI).

perform_static_policy_checks(PName,PType) :-
	% check that conditions used are declared
	(   perform_condition_check(PName,PType)
	->  true
	;   format('Condition check for policy ~q failed.~n',PName)
	),
	% check that operations used are valid for the object class
	perform_object_class_check(PName,PType),
	% check that assignment arguments are defined
	perform_assignments_check(PName,PType),
	% check that association arguments are defined
	perform_associations_check(PName,PType),
	% check that the graph is connected
	perform_connectedness_check(PName,PType),
	% check that given policy root occurs as a PC in the policy
	perform_policy_root_check(PName,PType),
	% check that purpose graph is connected
	perform_purpose_graph_check(PName,PType),
	true.

perform_static_gpolicy_checks(_).

perform_static_cpolicy_checks(_).

% individual checks
%
perform_condition_check(PName,_PType) :-
	conditions(PName,DeclaredConditions), !,
	% could be made more precise
	% currently ignores the number and type of arguments to the declared and used predicates
	findall( Pred, (cond(PName,Condition,_), functor(Condition,Pred,_)), UsedPreds),
	forall( member(P,UsedPreds),
		( condition_predicate_check(P,DeclaredConditions); built_in_binary_relation_name(P) )
	      ).
perform_condition_check(_,_).

condition_predicate_check(PredName,DeclaredPredicates) :-
	member(C,DeclaredPredicates), functor(C,PredName,_).

% check that operations are defined and not used in places they shouldn't be used
perform_object_class_check(_PName,_PType).

% check that the arguments of an assignment are defined and compatible
perform_assignments_check(_PName,_PType).

% check that the arguments of an association are defined and compatible
perform_associations_check(_PName,_PType).

% check that the graph does not have disconnected components
perform_connectedness_check(_PName,_PType).

% check that given policy root occurs as a PC in the policy
perform_policy_root_check(_PName,_PType).

% check that purpose graph is connected
perform_purpose_graph_check(_PName,_PType).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
object_attribute(P,OA) :- element(P,object_attribute(OA)).

object_oattribute(P,O) :- var(O), !, object_oattribute_nd(P,O).

object_oattribute(P,O) :- object(P,O), !.
object_oattribute(P,O) :- element(P,object_attribute(O)).

object_oattribute_nd(P,O) :- object(P,O).
object_oattribute_nd(P,O) :- element(P,object_attribute(O)).

object(P,O) :- element(P,object(O)).
object(P,O) :- element(P,object(O,_)).
object(P,O) :- element(P,object(O,_,_)).
object(P,O) :- element(P,object(O,_,_,_,_,_,_)).

object(P,O,Oclass,Inh,Host,Path,BaseType,BaseName) :-
	element(P,object(O,Oclass,Inh,Host,Path,BaseType,BaseName)), !.
% default path for object element of the form object(<name>) :
object(P,O,file,no,localhost,Path,object_attribute,BaseName) :-
	element(P,object(O)), assign(P,O,BaseName),
	param:files_directory(FD), atomic_list_concat([FD,'/',O],Path).

user_attribute(P,U) :- element(P,user_attribute(U)).

user_uattribute(P,U) :- user(P,U).
user_uattribute(P,U) :- element(P,user_attribute(U)).

user(P,U) :- element(P,user(U)).

policy_class(P,PC) :- element(P,policy_class(PC)).

get_id_operation_set(PolicyClass,OpSetID, OpSet) :-
	element(PolicyName:PolicyClass, policy_class(PolicyClass)),
	element(PolicyName:PolicyClass, opset(OpSetID,OpSet)).

purpose(P,Purpose) :- element(P,purpose(Purpose)).

operation(P,Op) :- element(P,operation(Op)) ; element(P,operation(Op,_)).

data_type(P,T) :- element(P,data_type(T,_)) ; element(P,object_class(T,_)).

object_class(P,T) :- element(P,data_type(T,_)) ; element(P,object_class(T,_)).
