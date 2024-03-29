:- use_module(policyio).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% definition of the ngac tool interactive commands syntax
% syntax( Signature, CommandSet ).
%
syntax(access(policy,(user,mode,object)),           ngac).
syntax(access(policy,(user,mode,object),condition), ngac).
syntax(access(policy,(user,mode,purpose,object)),   ngac). % DPLP
syntax(add(element),                                ngac).
syntax(add(policy,element),                         ngac).
syntax(add_dplp_policy_base(policy_class,definitions),ngac). % DPLP
syntax(add_dplp_policy_base(policy,policy_class,definitions),    ngac). % DPLP
syntax(add_data_controller(dc_id, dc_policy),ngac). % DPLP
syntax(add_data_controller(policy, dc_id, dc_policy),ngac). % DPLP
syntax(add_data_processor(dp_id, dp_policy, dc_id),ngac). % DPLP
syntax(add_data_processor(policy, dp_id, dp_policy, dc_id),ngac). % DPLP
syntax(add_data_subject(ds_id, ds_pdis, ds_preference), ngac). % DPLP
syntax(add_data_subject(policy, ds_id, ds_pdis, ds_preference),ngac). % DPLP
syntax(add_data_item(pdi_id,pdc_id,ds_id),       ngac). % DPLP
syntax(add_data_item(policy,pdi_id,pdc_id,ds_id),ngac). % DPLP
syntax(add_application(app_id, dpos, dp_id),   ngac). % DPLP
syntax(add_application(policy, app_id, dpos, dp_id),ngac). % DPLP
syntax(add_consent(consent_id,dc_id,dp_id,app_id,dpos,purpose,ds_id,pdi_id,pdc_id,constraint),ngac). % DPLP
syntax(add_consent(policy,consent_id,dc_id,dp_id,app_id,dpos,purpose,ds_id,pdi_id,pdc_id,constraint),ngac). % DPLP
syntax(addm(policy,elements),                       ngac).
syntax(addm(policy,elements,name),                  ngac).
syntax(aoa(user),				    ngac).
syntax(aua(object),				    ngac).
syntax(combine(p1,p2,p3),			    ngac).
syntax(decl2imp(decl_file,imp_file),		                                  obsolete).
syntax(delete(policy,element),                      ngac).
syntax(deletem(policy,elements),                    ngac).
syntax(delete_name(policy,name),                    ngac).
%syntax(delete_dplp_policy_base(dplp_policy_base_meta_element),ngac). % DPLP
%syntax(delete_dplp_policy_base(policy, dplp_policy_base_meta_element),    ngac). % DPLP
syntax(delete_data_controller(dc_id),ngac). % DPLP
syntax(delete_data_controller(policy, dc_id),ngac). % DPLP
syntax(delete_data_processor(dp_id),ngac). % DPLP
syntax(delete_data_processor(policy,dp_id),ngac). % DPLP
syntax(delete_data_subject(ds_id), ngac). % DPLP
syntax(delete_data_subject(policy,ds_id),ngac). % DPLP
syntax(delete_data_item(pdi_id),       ngac). % DPLP
syntax(delete_data_item(policy,pdi_id),ngac). % DPLP
syntax(delete_application(app_id),   ngac). % DPLP
syntax(delete_application(policy, app_id),ngac). % DPLP
syntax(delete_consent(consent_id),                  ngac). % DPLP
syntax(delete_consent(policy,consent_id),           ngac). % DPLP
syntax(deletem(policy,elements),                    ngac).
syntax(deletem(policy,elements,name),               ngac).
syntax(dpl_reinit,                                  ngac).
syntax(dps(policy),                                                               obsolete).
syntax(export_commands(imp_file),                                                 obsolete).
syntax(getpol,				            ngac).
syntax(getstatus,                    ngac).
syntax(import(file_spec),			    ngac).
syntax(import_policy(policy_file),		    ngac).
syntax(los(policy),                                                               obsolete).
syntax(minaoa(user),				                                  			  obsolete).
syntax(newpol(policyid),                            ngac).
syntax(ngac,				 basic).
syntax(pmcmd,                                                                     obsolete).
syntax(policy_graph,				    ngac).
syntax(policy_graph(policy),			    ngac).
syntax(policy_graph(policy,graph_file),	            ngac).
syntax(policy_graph(policy,graph_file,graphics)    ,ngac).
syntax(policy_graph(policy,graph_file,graphics,dpi),ngac).
syntax(policy_meta,                                 ngac). % DPLP
syntax(policy_meta(policy),                         ngac). % DPLP
syntax(policy_sat(privpol,privpref),                ngac). % DPLP
syntax(policy_sat(policy,privpol,privpref),         ngac). % DPLP
syntax(policy_spec,                                 ngac).
syntax(policy_spec(policy),                         ngac).
syntax(policy_spec(policy,policy_file),	            ngac).
syntax(policy_spec(policy,policy_file,silent),      ngac).
syntax(policy_spec_v(var),                          ngac).
syntax(policy_spec_v(var,compare),                  ngac).
syntax(policy_spec_v(policy,var),                   ngac).
syntax(policy_spec_v(policy,var,compare),           ngac).
syntax(server,				            ngac).
syntax(server(port),				    ngac).
syntax(server(port,atoken),			    ngac).
syntax(server(port,atoken,etoken),		    ngac).
syntax(setpol(policyid),                            ngac).
%syntax(store(pol_id),				    ngac).
syntax(userlos(policy,user),                        ngac).
syntax(users(object),                               ngac).
syntax(users(object,mode),                          ngac).
syntax(users(object,mode,condition),                ngac).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% NGAC tool command semantics
% semantics(<signature with formal params>) :- <constraints>.
%
% optional static semantics entry, e.g., used to check command arguments
% distinct from syntax so syntax can be called separately
%
semantics(access(P,(U,M,O))) :- !, ground(P), ground(U), ground(M), ground(O).
semantics(access(P,(U,M,Pur,O))) :- !, ground(P), ground(U), ground(M), ground(O), ground(Pur). % DPLP
semantics(access(P,(U,M,O),C)) :- !, ground(P), ground(U), ground(M), ground(O),
	(   C==true ; compound(C) ; is_list(C) ), !.
semantics(add(E)) :- !, ground(E).
semantics(add(P,E)) :- !, ground(P), ground(E).
semantics(add_dplp_policy_base(PC,Defs)) :- atom(PC), atom(Defs). % DPLP
semantics(add_dplp_policy_base(P,PC,Defs)) :- atom(P), atom(PC), atom(Defs). % DPLP
semantics(add_data_controller(DC_ID,DC_POLICY)) :- atom(DC_ID), is_list(DC_POLICY). % DPLP
semantics(add_data_controller(P,DC_ID,DC_POLICY)) :- atom(P), atom(DC_ID), is_list(DC_POLICY). % DPLP
semantics(add_data_processor(DP_ID,DP_POLICY,DC_ID)) :- atom(DP_ID), is_list(DP_POLICY), atom(DC_ID). % DPLP
semantics(add_data_processor(P,DP_ID,DP_POLICY,DC_ID)) :- atom(P), atom(DP_ID), is_list(DP_POLICY), atom(DC_ID). % DPLP
semantics(add_data_subject(DS_ID, DS_PDIs, DS_PREFERENCE)) :- atom(DS_ID), is_list(DS_PDIs), is_list(DS_PREFERENCE). % DPLP
semantics(add_data_subject(P, DS_ID, DS_PDIs, DS_PREFERENCE)) :- atom(P), atom(DS_ID), is_list(DS_PDIs), is_list(DS_PREFERENCE). % DPLP
semantics(add_data_item(PDI_ID, PDC_ID, DS_ID)) :- atom(PDI_ID), atom(PDC_ID), atom(DS_ID). % DPLP
semantics(add_data_item(P,PDI_ID, PDC_ID, DS_ID)) :- atom(P), atom(PDI_ID), atom(PDC_ID), atom(DS_ID). % DPLP
semantics(add_application(APP_ID,DPOs,DP_ID)) :-  atom(APP_ID), is_list(DPOs), atom(DP_ID). % DPLP
semantics(add_application(P,APP_ID,DPOs,DP_ID)) :- atom(P), atom(APP_ID), is_list(DPOs), atom(DP_ID). % DPLP
semantics(add_consent(ConsentID,DC,DP,App,DPOs,Purpose,DS,PDitem,PDcategory,Constraint)) :- !,
	atom(ConsentID),atom(DC),atom(DP),atom(App),is_list(DPOs),atom(Purpose),atom(DS),
	atom(PDitem),atom(PDcategory),ground(Constraint).
semantics(add_consent(P,ConsentID,DC,DP,App,DPOs,Purpose,DS,PDitem,PDcategory,Constraint)) :- !,
	atom(P),atom(ConsentID),atom(DC),atom(DP),atom(App),is_list(DPOs),atom(Purpose),atom(DS),
	atom(PDitem),atom(PDcategory),ground(Constraint).
semantics(addm(P,Es)) :- !, ground(P), ground(Es), is_list(Es).
semantics(addm(P,Es,Nam)) :- !, ground(P), ground(Es), is_list(Es), ground(Nam).
semantics(aoa(U)) :- !, ground(U).
semantics(aua(O)) :- !, ground(O).
semantics(combine(P1,P2,P3)) :- !, atom(P1), atom(P2), atom(P3).
semantics(decl2imp(Dfile,Ifile)) :- !, atom(Dfile), atom(Ifile).
semantics(delete(P,E)) :- !, ground(P), ground(E).
%semantics(deletem(P,Es)) :- !, ground(P), ground(Es).
semantics(delete_name(P,N)) :- !, ground(P), atom(N).
%semantics(delete_dplp_policy_base(PB)) :- ground(PB). % DPLP
%semantics(delete_dplp_policy_base(P,PB)) :- atom(P), ground(PB). % DPLP
semantics(delete_data_controller(DC)) :- ground(DC). % DPLP
semantics(delete_data_controller(P,DC)) :- atom(P), ground(DC). % DPLP
semantics(delete_data_processor(DP)) :- ground(DP). % DPLP
semantics(delete_data_processor(P,DP)) :- atom(P), ground(DP). % DPLP
semantics(delete_data_subject(DS)) :- ground(DS). % DPLP
semantics(delete_data_subject(P,DS)) :- atom(P), ground(DS). % DPLP
semantics(delete_data_item(DI)) :- atom(DI). % DPLP
semantics(delete_data_item(P,DI)) :- atom(P), atom(DI). % DPLP
semantics(delete_application(APP)) :- ground(APP). % DPLP
semantics(delete_application(P,APP)) :- atom(P), ground(APP). % DPLP
semantics(delete_consent(Cid)) :- !, atom(Cid).
semantics(delete_consent(P,Cid)) :- !, ground(P), atom(Cid).
semantics(deletem(P,Es)) :- !, ground(P), ground(Es), is_list(Es).
semantics(deletem(P,Es,Nam)) :- !, ground(P), ground(Es), is_list(Es), ground(Nam).
semantics(dps(P)) :- !, ground(P).
semantics(export_commands(C)) :- atom(C).
semantics(getpol_v(P)) :- !, var(P).
semantics(import(FS)) :- !, functor(FS,F,1), (F==policy ; (F==model ; (F==pm ; F==database ; F==erp))).
semantics(import_policy(P)) :- atom(P).
semantics(los(P)) :- !, ground(P).
semantics(minaoa(U)) :- !, ground(U).
semantics(newpol(ID)) :- !, ground(ID).
semantics(newpol(T,ID)) :- !, ground(ID), ground(T). % synonym for setpol
semantics(policy_meta(P)) :-  !, atom(P).
semantics(policy_graph(P)) :- !, atom(P).
semantics(policy_graph(P,F)) :- !, atom(P), atom(F).
semantics(policy_graph(P,F,G)) :- !, atom(P), atom(F), (G==pdf;G==png).
semantics(policy_graph(P,F,G,R)) :- !, atom(P), atom(F), (G==pdf;G==png), integer(R).
% semantics(policy_sat(PP,PR)) :- !,
% 	(atom(PP) ; PP=privacy_policy(DP,DPEs),atom(DP),is_list(DPEs)),
% 	(atom(PR) ; PR=privacy_preference(DS,DSEs),atom(DS),is_list(DSEs)).
semantics(policy_sat(PP,PR)) :- !,
	(atom(PP) ; is_list(PP) ; (PP=privacy_policy(DP,PPL), atom(DP), is_list(PPL))),
	(atom(PR) ; is_list(PR) ; (PR=privacy_preference(DS,PRL), atom(DS), is_list(PRL))).
semantics(policy_spec(P)) :- !, (atom(P) ; compound_name_arity(P,policy,4)).
semantics(policy_spec(P,F)) :- !, atom(P), atom(F).
semantics(policy_spec(P,F,S)) :- !, atom(P), atom(F), S == silent.
semantics(policy_spec_v(V)) :- !, var(V).
semantics(policy_spec_v(V,C)) :- var(V), !, ground(C).
semantics(policy_spec_v(P,V)) :- atom(P), !, var(V).
semantics(policy_spec_v(P,V,C)) :- !, atom(P), var(V), ground(C).
semantics(server(Port)) :- !, integer(Port).
semantics(server(Port,AToken)) :- !, integer(Port), atom(AToken).
semantics(server(Port,AToken,EToken)) :- !, integer(Port), atom(AToken), atom(EToken).
semantics(setpol(ID)) :- !, ground(ID).
%semantics(store(P)) :- !, atom(P).
semantics(userlos(P,U)) :- !, ground(P), ground(U).
semantics(users(O)) :- !, atom(O).
semantics(users(O,M)) :- !, atom(O), atom(M).
semantics(users(O,M,C)) :- !, atom(O), atom(M),(atom(C);compound(C);is_list(C)),!.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% command help strings
%   help(Key,    HelpString).
%
%   all strings for a given key are displayed when key is given as an
%   argument to the help command, e.g., "help(access)"
%
help(access,    'Under current policy, user can access with mode the object.').
help(access,	'Arg1 is a policy name.').
help(access,    'Arg2 is and access triple, "(User, Mode, Object)".').
help(access,    'Optionally, Arg2 is an access 4-tuple, "(User, Mode, Purpose, Object)".'). % DPLP
help(access,    'Arg3 (opt) a condition predicate for conditional rules.').

help(add,       'add element to policy.').
help(add,	'Arg1 (opt) is a policy name. Default is current policy.').
help(add,	'Arg2 is a policy element.').

help(add_consent,'add consent meta-element').
help(add_consent,'Arg1 (opt) is a policy name.').
help(add_consent,'Arg2 is a consent meta-element.').

help(addm,       'add elements to policy.').
help(addm,	 'Arg1 is a policy name.').
help(addm,	 'Arg2 is a list of policy elements.').
help(addm,       'Arg3 (opt) is a name to associate with the policy elements.').

help(aoa,	'all object attributes for user in current policy and policy class').
help(aoa,       'Arg is user identifier.').

help(aua,	'all user attributes for object in current policy and policy class').
help(aua,       'Arg is object identifier.').

help(combine,	'Arg1 and Arg2 are the names of currently loaded declarative policy specs.').
help(combine,	'Arg3 is name of a new policy spec that is the combination of the first two.').

help(decl2imp,	'Arg1 is name of input file containing declarative policy spec.').
help(decl2imp,	'Arg2 is name of output file to contain imperative policy spec.').

help(delete,    'delete element from policy.').
help(delete,	'Arg1 is a policy name.').
help(delete,	'Arg2 is a policy element.').

help(deletem,   'delete list of elements.').

help(delete_name,'delete named list of elements.').

help(delete_consent,'delete consent meta-element.').
help(delete_consent,'Arg1 (opt) is a policy name.').
help(delete_consent,'Arg2 is a consent ID.').

help(deletem,    'delete elements from policy.').
help(deletem,    'Arg1 is a policy name.').
help(deletem,    'Arg2 is a list of policy elements.').
help(deletem,    'Arg3 (opt) is a name associated with a list of stored policy elements.').

help(dpl_reinit,'Reinitialize declarative policy language module.').

help(dps,       'Show derived privileges of the specified policy').
help(dps,	'Arg is a policy name').

help(export,    'export a specified model(model_id), policy(type,attrs) or database(db_id)').

help(export_commands, '"export" a policy in PM commands').

% help(export_policy, '"export" a policy for consumption by external tools').
% help(export_model, '"export" a model for comsumption by external tools').

help(getpol,    'Show the name of the current policy.').
help(getpol_v,  'Return the name of the current policy in the variable given as Arg1.').

help(getstatus, 'Show the server and policy information status.').

help(import,    'import a specified policy policy(file), pm(file), erp(file).').

help(import_policy, '"import" a declarative policy from a file.').

help(los,       'Show logical object system of the specified policy').
help(los,	'Arg is a policy name').

help(newpol,	'Create a new policy as the "current policy".').
help(newpol,	'If two arguments (currently unimplemented) the first is policy type.').
help(newpol,	'Last argument is the policy ID.').
help(newpol,	'This is a deprecated synonym for setpol.').

help(ngac,      'Switch to ngac user mode.').

help(pmcmd,	'Enter PM server command mode.').

help(policy_graph, 'Display graph of the current or named policy,').
help(policy_graph, 'Arg1 (opt) names a currently loaded policy (or "current_policy") to graph,').
help(policy_graph, 'Arg2 (opt) file name root for dot and png files.').
help(policy_graph, 'Arg3 (opt) graphics output file type (pdf/png).').
help(policy_graph, 'Arg4 (opt with Arg3) dots-per-inch setting, e.g. 300.').

help(policy_meta,  'Display meta-elements of policy.').
help(policy_meta,  'Arg1 (opt) names a currently loaded policy to display,').

help(policy_sat,   'Privacy policy satisfies privacy preference.').
help(policy_sat,   'Arg1 (opt) names a currently loaded policy (or "current_policy") to use,').
help(policy_sat,   'Arg2 is a DC/DP identifier or privacy policy,').
help(policy_sat,   'Arg3 is a DS identifier or privacy preference.').

help(policy_spec, 'Display the current or named policy,').
help(policy_spec, 'Arg1 (opt) names a currently loaded policy (or "current_policy") to display,').
help(policy_spec, 'Arg2 (opt) file name root to save policy file,').
help(policy_spec, 'Arg3 (opt) if present, must be "silent" to create file without console display.').

help(server,	'Start the policy server.').
help(server,    'Arg1 (optional) is the port number.').
help(server,    'Arg2 (optional after Arg1) is a Server admin token.').
help(server,    'Arg3 (optional after Arg2) is an EPP token.').

help(setpol,	'Create a new policy as the "current policy".').
help(setpol,	'If two arguments (currently unimplemented) the first is policy type.').
help(setpol,	'Last argument is the policy ID.').

help(userlos,   'Show the logical object system under policy for user.').
help(userlos,	'Arg 1 is policy name.').
help(userlos,   'Arg 2 is user name.').

help(users,     'List users with access to object.').
help(users,     'Arg1 is an object.').
help(users,     'Arg2 (optional) is an access mode to the object.').
help(users,     'Arg3 (optional after Arg2) is a condition predicate.').

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% commands with an entry in do that fail are reported
% as unimplemented commands.
%
do(access(P,(U,M,Pur,O))) :- !, % DPLP
	(   pdp:access_check(P,(U,M,Pur,O))
	->  writeln(grant)
	;   writeln(deny)
	).
do(access(P,(U,M,O))) :- !,
	(   pdp:access_check(P,(U,M,O))
	->  writeln(grant)
	;   writeln(deny)
	).
do(access(P,(U,M,O),C)) :- !,
	(   pdp:access_check(P,(U,M,O),C)
	->  writeln(grant)
	;   writeln(deny)
	).
do(add(Elts)) :- is_list(Elts), !, do(addm(Elts)).
do(add(Elt)) :- !, param:current_policy(P), pap:add_named_policy_elements(_,P,[Elt]).
do(add(P,Elts)) :- is_list(Elts), !, do(addm(P,Elts)).
do(add(P,Elt)) :- !, pap:add_named_policy_elements(_,P,[Elt]).
do(add_dplp_policy_base(PC,Defs)) :- !, param:current_policy(P),
	do(add_dplp_policy_base(P,PC,Defs)).
do(add_dplp_policy_base(P, PC, Defs)) :- !,
	paapi:add_dplp_policy_base(P,PC,Defs).
do(add_data_controller(DC_ID,DC_POLICY)) :- !, param:current_policy(P),
	do(add_data_controller(P, DC_ID, DC_POLICY)).
do(add_data_controller(P, DC_ID, DC_POLICY)) :- !,
	paapi:add_data_controller(P,DC_ID,DC_POLICY).
do(add_data_processor(DP_ID, DP_POLICY, DC_ID)) :- !, param:current_policy(P),
	do(add_data_processor(P, DP_ID, DP_POLICY, DC_ID)).
do(add_data_processor(P, DP_ID, DP_POLICY, DC_ID)) :- !,
	paapi:add_data_processor(P,DP_ID,DP_POLICY,DC_ID).
do(add_data_subject(DS_ID, DS_PDIs, DS_PREFERENCE)) :- !, param:current_policy(P),
	do(add_data_subject(P, DS_ID, DS_PDIs, DS_PREFERENCE)).
do(add_data_subject(P, DS_ID, DS_PDIs, DS_PREFERENCE)) :- !,
	paapi:add_data_subject(P, DS_ID, DS_PDIs, DS_PREFERENCE).
do(add_data_item(PDI_ID, PDC_ID, DS_ID)) :- !, param:current_policy(P),
	do(add_data_item(P, PDI_ID, PDC_ID, DS_ID)).
do(add_data_item(P,PDI_ID, PDC_ID, DS_ID)) :- !,
	paapi:add_data_item(P, PDI_ID, PDC_ID, DS_ID).
do(add_application(APP_ID,DPOs,DP_ID)) :- !, param:current_policy(P),
	do(add_application(P,APP_ID,DPOs,DP_ID)).
do(add_application(P,APP_ID,DPOs,DP_ID)) :- !,
	paapi:add_application(P,APP_ID,DPOs,DP_ID).
do(add_consent(ConsentID,DC,DP,App,DPOs,Purpose,DS,PDitem,PDcategory,Constraint)) :- !,
	param:current_policy(P),
	do(add_consent(P,ConsentID,DC,DP,App,DPOs,Purpose,DS,PDitem,PDcategory,Constraint)).
do(add_consent(P,ConsentID,DC,DP,App,DPOs,Purpose,DS,PDitem,PDcategory,Constraint)) :- !,
	paapi:add_consent(P,_,ConsentID,DC,DP,App,DPOs,Purpose,DS,PDitem,PDcategory,Constraint).
doo(addm(Elts)) :- !, param:current_policy(P), pap:add_named_policy_elements(_,P,Elts).
do(addm(P,Elts)) :- !, pap:add_named_policy_elements(_,P,Elts).
do(addm(P,Elts,Name)) :- !, pap:add_named_policy_elements(Name,P,Elts).
do(aoa(U)) :- !, param:current_policy(P), dpl:policy(P,PC),
	pdp:aoa(P,U,PC,AOA), ui:display_list(AOA).
do(aua(O)) :- !, param:current_policy(P), dpl:policy(P,PC),
	pdp:aua(P,O,PC,AUA), ui:display_list(AUA).
do(combine(P1,P2,Presult)) :- !,
	pap:compose_policies(P1,P2,Presult),
	true.
do(decl2imp(D,I)) :- !,
	 % same as import_policy+export_commands w/o making current policy
	dpl:decl2imp(D,I).
do(delete(P,Elt)) :- !, dpl:delete_PE(P:_,Elt).
%do(deletem(P,_Elts,Name)) :- !, pap:delete_named_policy_elements(Name,P,_).
do(deletem(P,Elts)) :- !, dpl:delete_PEs(P:_,Elts).
do(delete_name(P,Name)) :- !, dpl:delete_named(P:_,Name).
/*
do(delete_dplp_policy_base(PB)) :- !, param:current_policy(P), true.
do(delete_dplp_policy_base(P,PB)) :- !, true.
*/
do(delete_data_controller(DC)) :- !, param:current_policy(P), do(delete_data_controller(P,DC)).
do(delete_data_controller(P,DC)) :- !, paapi:delete_data_controller(P,DC).
do(delete_data_processor(DP)) :- !, param:current_policy(P), do(delete_data_processor(P,DP)).
do(delete_data_processor(P,DP)) :- !, paapi:delete_data_processor(P,DP).
do(delete_data_subject(DS)) :- !, param:current_policy(P), do(delete_data_subject(P,DS)).
do(delete_data_subject(P,DS)) :- !, paapi:delete_data_subject(P,DS).
do(delete_data_item(DI)) :- !, param:current_policy(P), do(delete_data_item(P,DI)).
do(delete_data_item(P,DI)) :- !, paapi:delete_data_item(P,DI).
do(delete_application(APP)) :- !, param:current_policy(P), do(delete_application(P,APP)).
do(delete_application(P,APP)) :- !, paapi:delete_application(P,APP).
do(delete_consent(Cid)) :- !, param:current_policy(P), do(delete_consent(P,Cid)).
do(delete_consent(P,Cid)) :- !, paapi:delete_consent(P,Cid).
do(dpl_reinit) :- !, dpl:reinit.
do(dps(P)) :- !, %param:current_policy(P), % dpl:policy(P,PC),
	pdp:policy_dps(P,DPS), ui:display_list(DPS).
do(export(commands,CmdFile)) :- !,
	param:current_policy(PolicyName),
	do( export_commands(PolicyName,CmdFile) ).
do(export_commands(PolicyName,CmdFile)) :-
	dpl:save_as_cmds(PolicyName,CmdFile).
do(getpol) :- !, param:current_policy(P), writeq(P), nl.
do(getpol_v(P)) :- !, param:current_policy(P).
do(getstatus) :- !, paapi:getstatus(Status),
        Status = status(
                current_version(CurrentVer,CurrentDesc),
                current_policy(CurrP),
                PolicyInfo
        ),
	format('Version: ~a, ~q~n',[CurrentVer,CurrentDesc]),
	format('Current policy: ~q~n',CurrP),
	forall(member(P,PolicyInfo), 
		( P = policy(PN:PC,elements(Nelt),meta(Nmeta),assign(Nassign),assoc(Nassoc),cond(Ncond)),
		  format('~q:~q  elts:~d   meta:~d  assigns:~d  assocs:~d  cond:~d',
			[PN,PC,Nelt,Nmeta, Nassign,Nassoc,Ncond]),
		  nl
		)
	),
	true.
do(import(pm(PM))) :- do(import_pm(PM)).
do(import(policy(P))) :- do(import_policy(P)).
do(import_pm(PM)) :- % import a PM imperative command file
	policyio:load_CmdTerms_from_CmdStrFile(PM,CmdTerms),
	% create policy form
	dpl:cmdTerms2policy(CmdTerms,Policy),
	% make it a queryable policy
	dpl:unpack_policy(Policy).
do(import_policy(Pfile)) :-  % import declarative policy
	dpl:load_decl_policy(Pfile,PolicyName),
	ui:notify('Policy loaded',PolicyName),
	do( newpol(PolicyName) ).
do(los(P)) :- !, pdp:los(P,LOS), ui:display_list(LOS).
do(minaoa(U)) :- !, param:current_policy(P), dpl:policy(P,PC),
	pdp:min_aoa(P,U,PC,MAOA), ui:display_list(MAOA).
do(newpol(P)) :- !, do(setpol(P)).
do(ngac) :- user_mode(ngac), !, writeln('Already in ngac mode').
do(ngac) :- !, user_mode(M), retractall(user_mode(_)), assert(user_mode(ngac)),
	param:prompt_string(ngac,Prompt), param:setparam(prompt_string,Prompt),
	rem_commands(M), add_commands(ngac), banner(ngac).
do(pmcmd) :- !, (interactive(true) -> tl(pmcmd) ; true).
do(policy_meta) :- !, do(policy_meta(current_policy)).
do(policy_meta(current_policy)) :- !, param:current_policy(P), do(policy_meta(P)).
do(policy_meta(P)) :- atom(P), !, policyio:display_meta(P:_).
do(policy_graph) :- !, do(policy_graph(current_policy)).
do(policy_graph(current_policy)) :- !, param:current_policy(P), do(policy_graph(P)).
do(policy_graph(current_policy,Fileroot)) :- !, param:current_policy(P), do(policy_graph(P,Fileroot)).
do(policy_graph(P)) :- !, param:graph_tmp_file(T), do(policy_graph(P,T)).
do(policy_graph(P,T)) :- !, do(policy_graph(P,T,png)). % default to png
do(policy_graph(P,Fileroot,Gfiletype)) :- !, do(policy_graph(P,Fileroot,Gfiletype,_)).
do(policy_graph(P,Fileroot,Gfiletype,Res)) :- !, dpl:policy(P,_), % must specify file type to specify res
	param:graph_directory_name(GraphD),
	(   Gfiletype == pdf, DISPfile=PDFfile ; Gfiletype == png, DISPfile=PNGfile ),
	(   exists_directory(GraphD)
	->  true
	;   make_directory(GraphD)
	),
	atomic_list_concat([GraphD,'/',Fileroot,'.dot'],DOTfile),
	atomic_list_concat([GraphD,'/',Fileroot,'.png'],PNGfile),
	atomic_list_concat([GraphD,'/',Fileroot,'.pdf'],PDFfile),
	current_output(Old),
	open(DOTfile,write,DOTstream,[create([default])]),
	set_output(DOTstream),
	policyio:graph_policy(P),
	close(DOTstream,[force(true)]),
	set_output(Old),
	(   var(Res) ->  Gdpi_arg = ''  % no res given use default resolution
	;   atomic_list_concat(['-Gdpi=',Res,' '], Gdpi_arg) ), % if a Res is given use -Gdpi argument
	param:local_dot_render(_,DOTcmd),
	param:local_open_file(_,OPEN),
	atomic_list_concat([DOTcmd,' -T', Gfiletype, ' ', Gdpi_arg, DOTfile , ' >', DISPfile], DotCommand),
	atomic_list_concat([OPEN,' ', DISPfile], OpenCommand),
	shell(DotCommand,Stat1),
	(	Stat1 =\= 0
	->	writeln(/*user_error,*/'error in dot file')
	;	shell(OpenCommand,_)
	),
	(   param:graph_tmp_file(Fileroot)
	->  sleep(2), delete_file(DOTfile), delete_file(DISPfile)
	;   true
	).
do(policy_sat(PPol,PPref)) :- !, do(policy_sat(current_policy,PPol,PPref)).
do(policy_sat(current_policy,PPol,PPref)) :- !, param:current_policy(P), do(policy_sat(P,PPol,PPref)).
do(policy_sat(P,PPol,PPref)) :- !,
	pdp:privacy_sat(P,PPol,PPref,NonSat),
	(	NonSat = (_,_):[]
	->	writeln('Satisfied')
	;	format('Non-satisfying: ~q~n',[NonSat])
	).
do(policy_spec) :- !, do(policy_spec(current_policy)).
do(policy_spec(current_policy)) :- !, param:current_policy(P), do(policy_spec(P,no_file_output,false)).
do(policy_spec(current_policy,Fileroot)) :- !, param:current_policy(P), do(policy_spec(P,Fileroot,false)).
do(policy_spec(current_policy,Fileroot,Silent)) :- !, param:current_policy(P), do(policy_spec(P,Fileroot,Silent)).
do(policy_spec(P)) :- atom(P), !, do(policy_spec(P,no_file_output,false)).
do(policy_spec(P)) :- compound_name_arity(P,policy,4), !, policyio:display_canonical(P).
do(policy_spec(P,Fileroot)) :- !, do(policy_spec(P,Fileroot,false)).
do(policy_spec(P,Fileroot,Silent)) :- !, dpl:policy(P,_),
	(   Fileroot \== no_file_output
	->  param:policy_directory_name(PolicyD),
	    (   exists_directory(PolicyD)
	    ->  true
	    ;   make_directory(PolicyD)
	    ),
	    atomic_list_concat([PolicyD,'/',Fileroot,'.pl'],PLfile),
	    current_output(Old),
	    open(PLfile,write,PL,[create([default])]),
	    set_output(PL),
	    policyio:display_policy(P),
	    close(PL,[force(true)]),
	    set_output(Old)
	;   true
	),
	(   Silent == false
	->  policyio:display_policy(P,meta)
	;   true
	).
do(policy_spec_v(V)) :- !, var(V), param:current_policy(P), do(policy_spec_v(P,V)).
do(policy_spec_v(V,C)) :- var(V), !, ground(C), param:current_policy(P), do(policy_spec_v(P,V)), V==C.
do(policy_spec_v(P,V)) :- atom(P), !, var(V), policyio:canonical_policy(P,V).
do(policy_spec_v(P,V,C)) :- !, atom(P), var(V), ground(C), policyio:canonical_policy(P,V), V==C.
do(server) :- !, param:setparam(sleep_after_server_start,off), server:server.
do(server(Port)) :- !, param:setparam(sleep_after_server_start,off), server:server(Port).
do(server(Port,AToken)) :- !, param:setparam(sleep_after_server_start,off), server:server(Port,AToken).
do(server(Port,AToken,EToken)) :- !, param:setparam(sleep_after_server_start,off), server:server(Port,AToken,EToken).
do(setpol(P)) :- !, pap:set_current_policy(P).
do(userlos(P,U)) :- pdp:user_los(P,U,V,E),
	write('V='), ui:display_list(V,''), write('E='), ui:display_list(E,'').
do(users(O)) :-  !,
	param:current_policy(P), pdp:aua_users(P,O,_PC,Users), writeln(Users). %ui:display_list(Users).
do(users(O,M)) :- !,
	param:current_policy(P), pdp:aua_users(P,(M,O),Users), writeln(Users). %ui:display_list(Users).
do(users(O,M,true)) :-  do(users(O,M)).
do(users(O,M,C)) :- is_list(C), !, dpl_conditions:is_cond_var_def_list(C),
	param:current_policy(P),
	pdp:aua_users(P,O,_,M,C,Users), writeln(Users). %ui:display_list(Users).
do(users(O,M,C)) :- dpl_conditions:validate_condition_predicate(C,_), !,
	param:current_policy(P),
	writeln('condition predicate ignored'), % HERE more to do
	pdp:aua_users(P,(M,O),Users), writeln(Users). %ui:display_list(Users).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% command support procedures
%
