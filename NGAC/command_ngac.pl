%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% definition of the ngac tool interactive commands syntax
% syntax( Signature, CommandSet ).
%
syntax(access(policy,(user,mode,object)),           ngac).
syntax(access(policy,(user,mode,object),condition), ngac).
syntax(access(policy,(user,mode,object,purpose)),   ngac). % DPLP
syntax(aoa(user),				    ngac).
syntax(aua(object),				    ngac).
syntax(combine(p1,p2,p3),			    ngac).
syntax(decl2imp(decl_file,imp_file),		                                  obsolete).
syntax(dpl_reinit,                                  ngac).
syntax(dps(policy),                                                               obsolete).
syntax(export_commands(imp_file),                                                 obsolete).
syntax(getpol,				            ngac).
syntax(import(file_spec),			    ngac).
syntax(import_policy(policy_file),		    ngac).
syntax(los(policy),                                                               obsolete).
syntax(minaoa(user),				                                  obsolete).
syntax(newpol(policyid),                            ngac).
syntax(ngac,				 basic).
syntax(pmcmd,                                                                     obsolete).
syntax(policy_graph,				    ngac).
syntax(policy_graph(policy),			    ngac).
syntax(policy_graph(policy,graph_file),	            ngac).
syntax(policy_graph(policy,graph_file,graphics)    ,ngac).
syntax(policy_graph(policy,graph_file,graphics,dpi),ngac).
syntax(policy_spec,                                 ngac).
syntax(policy_spec(policy),                         ngac).
syntax(policy_spec(policy,policy_file),	            ngac).
syntax(policy_spec(policy,policy_file,silent),      ngac).
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
semantics(access(P,(U,M,O,Pur))) :- !, ground(P), ground(U), ground(M), ground(O), ground(Pur). % DPLP
semantics(access(P,(U,M,O),C)) :- !, ground(P), ground(U), ground(M), ground(O),
	(   C==true ; compound(C) ; is_list(C) ), !.
semantics(aoa(U)) :- !, ground(U).
semantics(aua(O)) :- !, ground(O).
semantics(combine(P1,P2,P3)) :- !, atom(P1), atom(P2), atom(P3).
semantics(decl2imp(Dfile,Ifile)) :- !, atom(Dfile), atom(Ifile).
semantics(dps(P)) :- !, ground(P).
semantics(export_commands(C)) :- atom(C).
semantics(import(FS)) :- !, functor(FS,F,1), (F==policy ; (F==model ; (F==pm ; F==database ; F==erp))).
semantics(import_policy(P)) :- atom(P).
semantics(los(P)) :- !, ground(P).
semantics(minaoa(U)) :- !, ground(U).
semantics(newpol(ID)) :- !, ground(ID).
semantics(newpol(T,ID)) :- !, ground(ID), ground(T). % synonym for setpol
semantics(policy_graph(P)) :- !, atom(P).
semantics(policy_graph(P,F)) :- !, atom(P), atom(F).
semantics(policy_graph(P,F,G)) :- !, atom(P), atom(F), (G==pdf;G==png).
semantics(policy_graph(P,F,G,R)) :- !, atom(P), atom(F), (G==pdf;G==png), integer(R).
semantics(policy_spec(P)) :- !, atom(P).
semantics(policy_spec(P,F)) :- !, atom(P), atom(F).
semantics(policy_spec(P,F,S)) :- !, atom(P), atom(F), S == silent.
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
help(access,    'Optionally, Arg2 is an access 4-tuple, "(User, Mode, Object, Purpose)".'). % DPLP
help(access,    'Arg3 (opt) a condition predicate for conditional rules.').

help(aoa,	'all object attributes for user in current policy and policy class').
help(aoa,       'Arg is user identifier.').

help(aua,	'all user attributes for object in current policy and policy class').
help(aua,       'Arg is object identifier.').

help(combine,	'Arg1 and Arg2 are the names of currently loaded declarative policy specs.').
help(combine,	'Arg3 is name of a new policy spec that is the combination of the first two.').

help(decl2imp,	'Arg1 is name of input file containing declarative policy spec.').
help(decl2imp,	'Arg2 is name of output file to contain imperative policy spec.').

help(dpl_reinit,'Reinitialize declarative policy language module.').

help(dps,       'Show derived privileges of the specified policy').
help(dps,	'Arg is a policy name').

help(export,    'export a specified model(model_id), policy(type,attrs) or database(db_id)').

help(export_commands, '"export" a policy in PM commands').

% help(export_policy, '"export" a policy for consumption by external tools').
% help(export_model, '"export" a model for comsumption by external tools').

help(getpol,    'Show the name of the current policy.').

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
do(access(P,(U,M,O,Pur))) :- !, % DPLP
	(   pdp:access_check(P,(U,M,O,Pur))
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
do(dpl_reinit) :- !, dpl:reinit.
do(dps(P)) :- !, %param:current_policy(P), % dpl:policy(P,PC),
	pdp:policy_dps(P,DPS), ui:display_list(DPS).
do(export(commands,CmdFile)) :- !,
	param:current_policy(PolicyName),
	do( export_commands(PolicyName,CmdFile) ).
do(export_commands(PolicyName,CmdFile)) :-
	dpl:save_as_cmds(PolicyName,CmdFile).
do(getpol) :- !, param:current_policy(P), writeq(P), nl.
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
	shell(DotCommand,_), shell(OpenCommand,_),
	(   param:graph_tmp_file(Fileroot)
	->  sleep(2), delete_file(DOTfile), delete_file(DISPfile)
	;   true
	).
do(policy_spec) :- !, do(policy_spec(current_policy)).
do(policy_spec(current_policy)) :- !, param:current_policy(P), do(policy_spec(P,no_file_output,false)).
do(policy_spec(current_policy,Fileroot)) :- !, param:current_policy(P), do(policy_spec(P,Fileroot,false)).
do(policy_spec(current_policy,Fileroot,Silent)) :- !, param:current_policy(P), do(policy_spec(P,Fileroot,Silent)).
do(policy_spec(P)) :- !, do(policy_spec(P,no_file_output,false)).
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
	(   Silent \== silent
	->  policyio:display_policy(P)
	;   true
	).
do(server) :- !, server:server.
do(server(Port)) :- !, server:server(Port).
do(server(Port,AToken)) :- !, server:server(Port,AToken).
do(server(Port,AToken,EToken)) :- !, server:server(Port,AToken,EToken).
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
