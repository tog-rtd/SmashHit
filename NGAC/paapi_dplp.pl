% META-ELEMENT ADMINISTRATION API

% DPLP meta-element APIs
:- http_handler(root(dplp), root_apis(dplp), []).
:- http_handler(root('dplp/'), api_unimpl, [prefix]).
:- http_handler(root(dplp/add_dplp_policy_base), dplp_add_dplp_policy_base, [prefix]).

:- http_handler(root(dplp/add_data_controller), dplp_add_data_controller, [prefix]).
:- http_handler(root(dplp/delete_data_controller), dplp_delete_data_controller, [prefix]).

:- http_handler(root(dplp/add_data_processor), dplp_add_data_processor, [prefix]).
:- http_handler(root(dplp/delete_data_processor), dplp_delete_data_processor, [prefix]).

:- http_handler(root(dplp/add_data_subject), dplp_add_data_subject, [prefix]).
:- http_handler(root(dplp/delete_data_subject), dplp_delete_data_subject, [prefix]).

:- http_handler(root(dplp/add_data_item), dplp_add_data_item, [prefix]).
:- http_handler(root(dplp/delete_data_item), dplp_delete_data_item, [prefix]).

:- http_handler(root(dplp/add_application), dplp_add_application, [prefix]).
:- http_handler(root(dplp/delete_application), dplp_delete_application, [prefix]).

:- http_handler(root(dplp/add_consent), dplp_add_consent, [prefix]).
:- http_handler(root(dplp/delete_consent), dplp_delete_consent, [prefix]).

dplp([add_dplp_policy_base,add_data_controller,delete_data_controller,
      add_data_processor,delete_data_processor,add_data_subject,delete_data_subject,
      add_data_item,delete_data_item,add_application,delete_application,
      add_consent,delete_consent]).

%
% DPLP META-ELEMENT ADMIN
%

% add_dplp_policy_base
dplp_add_dplp_policy_base(Request):-
	std_resp_prefix,
	catch(
	    http_parameters(Request,[
					policy(Policy,[atom,optional(true)]),
					policy_class(PolicyClass,[atom]),
					definitions(Definitions,[atom]),
				    token(Token,[atom])]),
	    _,
	    (	std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	(   authenticate(Token)
	->  add_dplp_policy_base(Policy, PolicyClass, Definitions), !
	;   true
	).
dplp_add_dplp_policy_base(_) :- audit_gen(dplp_admin, add_dplp_policy_base(failure)).

add_dplp_policy_base(Policy, PolicyClass, GlobalDefs) :-
	(	var(Policy)
	->	param:current_policy(Policy)
	;	true
	),
	policy(Policy,PC,dplp),
	PolicyBase = dplp_policy_base(PolicyClass, GlobalDefs),
	dpl:unpack_policy_elements_with_meta_expansion(Policy:PC,[PolicyBase],Errors),
	(	Errors == []
	->	std_resp_MS(success,'policy base added',PolicyBase),
		audit_gen(dplp_admin, add_dplp_policy_base(Policy, PolicyBase, success))
	;   std_resp_MBS(failure,'error adding policy base',PolicyBase:Errors),
		audit_gen(dplp_admin, add_dplp_policy_base(Policy, PolicyBase:Errors, failure))
	).

% add_data_controller
dplp_add_data_controller(Request) :-
	std_resp_prefix,
	catch(
	    http_parameters(Request,[
					policy(Policy,[atom,optional(true)]),
					data_controller(DC_ID,[atom]),
					privacy_policy(DC_POLICYatom,[atom,default('[]')]),
				    token(Token,[atom])]),
	    _,
	    (	std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	(   authenticate(Token)
	->  add_data_controller(Policy, DC_ID, DC_POLICYatom), !
	;   true
	).
dplp_add_data_controller(_) :- audit_gen(dplp_admin, add_data_controller(failure)).

add_data_controller(Policy, DC_ID, DC_POLICYatom) :-
	(	var(Policy)
	->	param:current_policy(Policy)
	;	true
	),
	policy(Policy,PC,dplp),
	(	atom(DC_POLICYatom)
	->	read_term_from_atom(DC_POLICYatom,DC_POLICY,[])
	;	DC_POLICY = DC_POLICYatom
	),
	DataController = data_controller(DC_ID, DC_POLICY),
	dpl:unpack_policy_elements_with_meta_expansion(Policy:PC,[DataController],Errors),
	(	Errors == []
	->	std_resp_MS(success,'data controller added',DataController),
		audit_gen(dplp_admin, add_data_controller(Policy, DataController, success))
	;   std_resp_MBS(failure,'error adding data controller',DataController:Errors),
		audit_gen(dplp_admin, add_data_controller(Policy, DataController:Errors, failure))
	).

% delete_data_controller
dplp_delete_data_controller(Request) :-
	std_resp_prefix,
	catch(
	    http_parameters(Request,[
					policy(Policy,[atom,optional(true)]),
					data_controller(DC_ID,[atom]),
				    token(Token,[atom])]),
	    _,
	    (	std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	(   authenticate(Token)
	->  delete_data_controller(Policy, DC_ID), !
	;   true
	).
dplp_delete_data_controller(_) :- audit_gen(dplp_admin, delete_data_controller(failure)).

delete_data_controller(Policy, DC_ID) :-
	(	var(Policy)
	->	param:current_policy(Policy)
	;	true
	),
	policy(Policy,PC,dplp),
	(	dpl:delete_ME(Policy:PC,data_controller(DC_ID,_))
	->	std_resp_MS(success,'data controller deleted',DC_ID),
		audit_gen(dplp_admin, delete_data_controller(Policy, DC_ID, success))
	;   std_resp_MS(failure,'error deleting data controller',DC_ID),
		audit_gen(dplp_admin, delete_data_controller(Policy, DC_ID, failure))
	).

% add_data_processor
dplp_add_data_processor(Request) :-
	std_resp_prefix,
	catch(
	    http_parameters(Request,[
					policy(Policy,[atom,optional(true)]),
					data_processor(DP_ID,[atom]),
					privacy_policy(DP_POLICYatom,[atom]),
					data_controller(DC_ID,[atom]),
				    token(Token,[atom])]),
	    _,
	    (	std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	(   authenticate(Token)
	->  add_data_processor(Policy, DP_ID, DP_POLICYatom, DC_ID), !
	;   true
	).
dplp_add_data_processor(_) :- audit_gen(dplp_admin, add_data_processor(failure)).

add_data_processor(Policy, DP_ID, DP_POLICYatom, DC_ID) :-
	(	var(Policy)
	->	param:current_policy(Policy)
	;	true
	),
	policy(Policy,PC,dplp),
	(	atom(DP_POLICYatom)
	->	read_term_from_atom(DP_POLICYatom,DP_POLICY,[])
	;	DP_POLICY = DP_POLICYatom
	),
	DataProcessor = data_processor(DP_ID, DP_POLICY, DC_ID),
	dpl:unpack_policy_elements_with_meta_expansion(Policy:PC,[DataProcessor],Errors),
	(	Errors == []
	->	std_resp_MS(success,'data processor added',DataProcessor),
		audit_gen(dplp_admin, add_data_processor(Policy, DataProcessor, success))
	;   std_resp_MBS(failure,'error adding data processor',DataProcessor:Errors),
		audit_gen(dplp_admin, add_data_processor(Policy, DataProcessor:Errors, failure))
	).

% delete_data_processor
dplp_delete_data_processor(Request) :-
	std_resp_prefix,
	catch(
	    http_parameters(Request,[
					policy(Policy,[atom,optional(true)]),
					data_processor(DP_ID,[atom]),
				    token(Token,[atom])]),
	    _,
	    (	std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	(   authenticate(Token)
	->  delete_data_processor(Policy, DP_ID), !
	;   true
	).
dplp_delete_data_processor(_) :- audit_gen(dplp_admin, delete_data_processor(failure)).

delete_data_processor(Policy, DP_ID) :-
	(	var(Policy)
	->	param:current_policy(Policy)
	;	true
	),
	policy(Policy,PC,dplp),
	(	dpl:delete_ME(Policy:PC,data_processor(DP_ID,_,_))
	->	std_resp_MS(success,'data processor deleted',DP_ID),
		audit_gen(dplp_admin, delete_data_processor(Policy, DP_ID, success))
	;   std_resp_MS(failure,'error deleting data processor',DP_ID),
		audit_gen(dplp_admin, delete_data_processor(Policy, DP_ID, failure))
	).

% add_application
dplp_add_application(Request) :-
	std_resp_prefix,
	catch(
	    http_parameters(Request,[
					policy(Policy,[atom,optional(true)]),
					application(APP_ID,[atom]),
					operations(DPOatom,[atom]),
					data_processor(DP_ID,[atom]),
				    token(Token,[atom])]),
	    _,
	    (	std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	(   authenticate(Token)
	->  add_application(Policy, APP_ID, DPOatom, DP_ID), !
	;   true
	).
dplp_add_application(_) :- audit_gen(dplp_admin, add_application(failure)).

add_application(Policy, APP_ID, DPOatom, DP_ID) :-
	(	var(Policy)
	->	param:current_policy(Policy)
	;	true
	),
	policy(Policy,PC,dplp),
	(	atom(DPOatom)
	->	read_term_from_atom(DPOatom,DPOs,[])
	;	DPOs = DPOatom
	),
	Application = application(APP_ID, DPOs, DP_ID),
	dpl:unpack_policy_elements_with_meta_expansion(Policy:PC,[Application],Errors),
	(	Errors == []
	->	std_resp_MS(success,'application added',Application),
		audit_gen(dplp_admin, add_application(Policy, Application, success))
	;   std_resp_MBS(failure,'error adding application',Application:Errors),
		audit_gen(dplp_admin, add_application(Policy, Application:Errors, failure))
	).

% delete_application
dplp_delete_application(Request) :-
	std_resp_prefix,
	catch(
	    http_parameters(Request,[
					policy(Policy,[atom,optional(true)]),
					application(APP_ID,[atom]),
				    token(Token,[atom])]),
	    _,
	    (	std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	(   authenticate(Token)
	->  delete_application(Policy, APP_ID), !
	;   true
	).
dplp_delete_application(_) :- audit_gen(dplp_admin, delete_application(failure)).

delete_application(Policy, APP_ID) :-
	(	var(Policy)
	->	param:current_policy(Policy)
	;	true
	),
	policy(Policy,PC,dplp),
	(	dpl:delete_ME(Policy:PC,application(APP_ID,_,_))
	->	std_resp_MS(success,'application deleted',APP_ID),
		audit_gen(dplp_admin, delete_application(Policy, APP_ID, success))
	;   std_resp_MS(failure,'error deleting application',APP_ID),
		audit_gen(dplp_admin, delete_application(Policy, APP_ID, failure))
	).

% add_data_subject
dplp_add_data_subject(Request) :-
	std_resp_prefix,
	catch(
	    http_parameters(Request,[
					policy(Policy,[atom,optional(true)]),
					data_subject(DS_ID,[atom]),
					data_items(DS_PDIatom,[atom]),
					privacy_preference(DS_PREFERENCEatom,[atom]),
				    token(Token,[atom])]),
	    _,
	    (	std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	(   authenticate(Token)
	->  add_data_subject(Policy, DS_ID, DS_PDIatom, DS_PREFERENCEatom), !
	;   true
	).
dplp_add_data_subject(_) :- audit_gen(dplp_admin, add_data_subject(failure)).

add_data_subject(Policy, DS_ID, DS_PDIatom, DS_PREFERENCEatom) :-
	(	var(Policy)
	->	param:current_policy(Policy)
	;	true
	),
	policy(Policy,PC,dplp),
	(	atom(DS_PDIatom)
	->	read_term_from_atom(DS_PDIatom,DS_PDIs,[])
	;	DS_PDIs = DS_PDIatom
	),
	(	atom(DS_PREFERENCEatom)
	->	read_term_from_atom(DS_PREFERENCEatom,DS_PREFERENCE,[])
	;	DS_PREFERENCE = DS_PREFERENCEatom
	),
	DataSubject = data_subject(DS_ID, DS_PDIs, DS_PREFERENCE),
	dpl:unpack_policy_elements_with_meta_expansion(Policy:PC,[DataSubject],Errors),
	(	Errors == []
	->	std_resp_MS(success,'data subject added',DataSubject),
		audit_gen(dplp_admin, add_data_subject(Policy, DataSubject, success))
	;   std_resp_MBS(failure,'error adding data subject',DataSubject:Errors),
		audit_gen(dplp_admin, add_data_subject(Policy, DataSubject:Errors, failure))
	).

% delete_data_subject
dplp_delete_data_subject(Request) :-
	std_resp_prefix,
	catch(
	    http_parameters(Request,[
					policy(Policy,[atom,optional(true)]),
					data_subject(DS_ID,[atom]),
				    token(Token,[atom])]),
	    _,
	    (	std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	(   authenticate(Token)
	->	delete_data_subject(Policy, DS_ID), !
	;   true
	).
dplp_delete_data_subject(_) :- audit_gen(dplp_admin, delete_data_subject(failure)).

delete_data_subject(Policy, DS_ID) :-
	(	var(Policy)
	->	param:current_policy(Policy)
	;	true
	),
	policy(Policy,PC,dplp),
	(	dpl:delete_ME(Policy:PC,data_subject(DS_ID,_,_))
	->	std_resp_MS(success,'data subject deleted',DS_ID),
		audit_gen(dplp_admin, delete_data_subject(Policy, DS_ID, success))
	;   std_resp_MS(failure,'error deleting data subject',DS_ID),
		audit_gen(dplp_admin, delete_data_subject(Policy, DS_ID, failure))
	).

% add_data_item
dplp_add_data_item(Request) :-
	std_resp_prefix,
	catch(
	    http_parameters(Request,[
					policy(Policy,[atom,optional(true)]),
					data_item(PDI_ID,[atom]),
					data_category(PDC_ID,[atom]),
					data_subject(DS_ID,[atom]),
				    token(Token,[atom])]),
	    _,
	    (	std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	(   authenticate(Token)
	->  add_data_item(Policy, PDI_ID, PDC_ID, DS_ID), !
	;   true
	).
dplp_add_data_item(_) :- audit_gen(dplp_admin, add_data_item(failure)).

add_data_item(Policy, PDI_ID, PDC_ID, DS_ID) :-
	(	var(Policy)
	->	param:current_policy(Policy)
	;	true
	),
	policy(Policy,PC,dplp),
	DataItem = data_item(PDI_ID, PDC_ID, DS_ID),
	dpl:unpack_policy_elements_with_meta_expansion(Policy:PC,[DataItem],Errors),
	(	Errors == []
	->	std_resp_MS(success,'data item added',DataItem),
		audit_gen(dplp_admin, add_data_item(Policy, DataItem, success))
	;   std_resp_MBS(failure,'error adding data item',DataItem:Errors),
		audit_gen(dplp_admin, add_data_item(Policy, DataItem:Errors, failure))
	).

% delete_data_item
dplp_delete_data_item(Request) :-
	std_resp_prefix,
	catch(
	    http_parameters(Request,[
					policy(Policy,[atom,optional(true)]),
					data_item(PDI_ID,[atom]),
				    token(Token,[atom])]),
	    _,
	    (	std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	(   authenticate(Token)
	->  delete_data_item(Policy, PDI_ID), !
	;   true
	).
dplp_delete_data_item(_) :- audit_gen(dplp_admin, delete_data_item(failure)).

delete_data_item(Policy, PDI_ID) :-
	(	var(Policy)
	->	param:current_policy(Policy)
	;	true
	),
	policy(Policy,PC,dplp),
	(	dpl:delete_ME(Policy:PC,data_item(PDI_ID,_,_))
	->	std_resp_MS(success,'data item deleted',PDI_ID),
		audit_gen(dplp_admin, delete_data_item(Policy, PDI_ID, success))
	;   std_resp_MS(failure,'error deleting data item',PDI_ID),
		audit_gen(dplp_admin, delete_data_item(Policy, PDI_ID, failure))
).

% add_consent
dplp_add_consent(Request) :-
	std_resp_prefix,
	catch(
	    http_parameters(Request,[
					policy(Policy,[atom,optional(true)]),
					consent(ConsentAtom,[atom,optional(true)]),
					consent_id(ConsentID,[atom,optional(true)]),
					data_controller(DC,[atom,optional(true)]),
					data_processor(DP,[atom,optional(true)]),
					application(App,[atom,optional(true)]),
					operations(DPOAtom,[atom,optional(true)]),
					purpose(Purpose,[atom,optional(true)]),
					data_subject(DS,[atom,optional(true)]),
					data_item(PDitem,[atom,optional(true)]),
					data_category(PDcategory,[atom,optional(true)]),
					% constraint(ConstraintAtom,[atom,default(true)]),
					constraint(ConstraintAtom,[atom,optional(true)]),
				    token(Token,[atom])]),
	    _,
	    (	std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	(   authenticate(Token)
	->	add_consent(Policy,ConsentAtom,ConsentID,DC,DP,App,DPOAtom,Purpose,DS,PDitem,PDcategory,ConstraintAtom), !
	;   true
	).
dplp_add_consent(_) :- audit_gen(dplp_admin, add_consent(failure)).

add_consent(Policy,ConsentAtom,ConsentID,DC,DP,App,DPOAtom,Purpose,DS,PDitem,PDcategory,ConstraintAtom) :-
	(	var(Policy)
	->	param:current_policy(Policy)
	;	true
	),
	policy(Policy,PC,dplp),
	(	var(ConsentAtom)
	->	atom(ConsentID),atom(DC),atom(DP),atom(App),ground(DPOAtom),atom(Purpose),
		atom(DS),atom(PDitem),atom(PDcategory),ground(ConstraintAtom),
		(	atom(DPOAtom)
		->	read_term_from_atom(DPOAtom,DPOs,[])
		;	DPOs = DPOAtom
			),
		(	atom(ConstraintAtom)
		->	read_term_from_atom(ConstraintAtom,Constraint,[])
		;	Constraint = ConstraintAtom
		),
		Consent=consent(ConsentID,DC,DP,App,DPOs,Purpose,DS,PDitem,PDcategory,Constraint)
	;	read_term_from_atom(ConsentAtom,Consent,[]),
		functor(Consent,consent,10)
	),
	dpl:unpack_policy_elements_with_meta_expansion(Policy:PC,[Consent],Errors),
	(	Errors == []
	->	std_resp_MS(success,'consent added',Consent),
		audit_gen(dplp_admin, add_consent(Policy, Consent, success))
	;   std_resp_MBS(failure,'error adding consent',Consent:Errors),
		audit_gen(dplp_admin, add_consent(Policy, Consent:Errors, failure))
	).

% delete_consent
dplp_delete_consent(Request) :-
	std_resp_prefix,
	catch(
	    http_parameters(Request,[
					policy(Policy,[atom,optional(true)]),
					consent_id(ConsentID,[atom]),
				    token(Token,[atom])]),
	    _,
	    (	std_resp_MS(failure,'missing parameter',''), !, fail )
	), !,
	(   authenticate(Token)
	->  delete_consent(Policy,ConsentID), !
	;   true
	).
dplp_delete_consent(_) :- audit_gen(dplp_admin, delete_consent(failure)).

delete_consent(Policy,ConsentID) :-
	(	var(Policy)
	->	param:current_policy(Policy)
	;	true
	),
	policy(Policy,PC,dplp),
	(	dpl:delete_ME(Policy:PC,consent(ConsentID))
	->	std_resp_MS(success,'consent deleted',ConsentID),
		audit_gen(dplp_admin, delete_consent(Policy, ConsentID, success))
	;   std_resp_MS(failure,'error deleting consent',ConsentID),
		audit_gen(dplp_admin, delete_consent(Policy, ConsentID, failure))
	).
