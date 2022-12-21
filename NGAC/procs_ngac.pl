% stored "built-in" procedures

%%	NGAC Command Procs

proc(consent_ex1, [ % Tek's example with previous core_ontology
	reset,
	reset(policy,dplp_min),
	setpol(dplp_min),
	add_dplp_policy_base(pc,core_ontology),
	add_data_subject('ds[123BVC112]',['pdi(123BVC112)[123BVC112]':'Address'],[]),
	add_data_controller('dc[tTEK1235121]',[]),
	add_data_processor('dp[tEST123411][tTEK1235121]',[],'dc[tTEK1235121]'),
	add_application(dplp_min,'dp[tEST123411][tTEK1235121]_appl',['Collect','Use'], 'dp[tEST123411][tTEK1235121]'),
	add_consent(cID_33313,'dc[tTEK1235121]','dp[tEST123411][tTEK1235121]','dp[tEST123411][tTEK1235121]_appl',['dp[tEST123411][tTEK1235121]_appl'],'CommercialInterest','ds[123BVC112]','pdi(123BVC112)[123BVC112]','Address',true),
	policy_spec,
	access(dplp_min,('dp[tEST123411][tTEK1235121]','Use','SellProductsToDataSubject','pdi(123BVC112)[123BVC112]')),
	delete_consent(cID_33313),
	access(dplp_min,('dp[tEST123411][tTEK1235121]','Use','SellProductsToDataSubject','pdi(123BVC112)[123BVC112]'))
	]).

proc(consent_ex2, [ % use new ontology definitions policy smashHitCore202210
	reset,
	reset(policy,dplp_min),
	setpol(dplp_min),
	add_dplp_policy_base(pc,smashHitCore202210),
	add_data_subject('ds[123BVC112]',['pdi(123BVC112)[123BVC112]':'Birth Date'],[]),
	add_data_controller('dc[tTEK1235121]',[]),
	add_data_processor('dp[tEST123411][tTEK1235121]',[],'dc[tTEK1235121]'),
	add_application(dplp_min,'dp[tEST123411][tTEK1235121]_appl',['Collect','Store','Use'], 'dp[tEST123411][tTEK1235121]'),
	add_consent('C33313','dc[tTEK1235121]','dp[tEST123411][tTEK1235121]','dp[tEST123411][tTEK1235121]_appl',['dp[tEST123411][tTEK1235121]_appl'],'Commercial Interest','ds[123BVC112]','pdi(123BVC112)[123BVC112]','Birth Date',true),
	policy_spec,
	access(dplp_min,('dp[tEST123411][tTEK1235121]','Use','Sell Products To Data Subject','pdi(123BVC112)[123BVC112]')),
	delete_consent('C33313'),
	access(dplp_min,('dp[tEST123411][tTEK1235121]','Use','Sell Products To Data Subject','pdi(123BVC112)[123BVC112]'))
	]).

proc(consent_example_full, [
	reset,
	reset(policy,dplp_min),
	setpol(dplp_min),
	add_dplp_policy_base(pc,testdefs2),
	add_data_controller('dc[x]',[]),
	add_data_processor('dp[y][x]',[],'dc[x]'),
	add_data_subject('ds[1]',['pdi(1)[1]':'pdc{1}'],[]),
	add_data_item('pdi(2)[1]','pdc{2}','ds[1]'),
	add_application('dp[y][x]_app1',['dpo(w)','dpo(z)'],'dp[y][x]'),
	add_consent(cID_234,'dc[x]','dp[y][x]','dp[y][x]_app1',['dp[y][x]_app1'],'p(v)','ds[1]','pdi(1)[1]','pdc{1}',true),
	policy_spec,
	access(dplp_min,('dp[y][x]','dpo(z)','p(v)','pdi(1)[1]')),
	delete_consent(cID_234),
	access(dplp_min,('dp[y][x]','dpo(z)','p(v)','pdi(1)[1]')),
	echo(done)
	]).

proc(meta_delete, [ % check policy consistency after deleting meta-elements
	% Start with a policy that has many meta-elements (dplp3).
	% First, perform a sequence of meta-element bottom-up deletes
	% and capture the resulting policy as C3_1.
	% Then, perform top-down deletes (DCs and DSs) and capture as C3_2.
	% Compare C3_1 to C3_2. Policy starts as:
	% 
	% policy(dplp3, cpol, [
	%   dplp_policy_base(cpol, testdef_ex),
	%   opset( 'dp[y][x]_app1', ['dpo(w)','dpo(z)'] ),
	%   data_controller('dc[x]', []),
	%   data_processor('dp[y][x]',[],'dc[x]'),
	%   data_subject('ds[1]', ['pdi(1)[1]':'pdc{1}'], []),
	%   data_item('pdi(2)[1]','pdc{2}','ds[1]'),
	%   consent(cID_234,'dc[x]','dp[y][x]','app(a,y,x)',['dpo(z)'],'p(v)','ds[1]','pdi(1)[1]','pdc{1}',true),
	%   data_subject('ds[2]', ['pdi(1)[2]':'pdc{1}'], []),
	%   consent(cID_567,'dc[x]','dp[y][x]','app(a,y,x)',['dp[y][x]_app1'],'p(v)','ds[2]','pdi(1)[2]','pdc{1}',true)
	% ], dplp).

	reset,
	reset(policy,dplp3),
	echo('initial state'),
	policy_spec_v(dplp3, C0a),
	policy_spec(C0a),
	echo('performing deletions'),
	delete_name(dplp3, 'pdi(1)[1]'),
	delete_name(dplp3, 'pdi(1)[2]'),
	delete_name(dplp3, 'ds[2]'),
	delete_name(dplp3, 'ds[1]'),
	delete_name(dplp3, 'dp[y][x]'),
	delete_name(dplp3, 'dc[x]'),
	echo('saving result policy'),
	policy_spec_v(dplp3, C1a), % capture the resulting policy
	policy_spec(C1a),

	% restore the policy
	reset(policy,dplp3),
	policy_spec_v(dplp3, C0b),
	%policy_spec(C0b),
	echo('comparing restored to initial policy, expect equal'),
	compare_v(C0a,C0b,R0), echo(R0),

	% perform a different sequence of meta-element deletes (top-down)
	echo('performing different deletions'),
	delete_name(dplp3, 'ds[2]'),
	delete_name(dplp3, 'ds[1]'),
	delete_name(dplp3, 'dc[x]'),
	echo('save new result'),
	policy_spec_v(dplp3, C1b), % capture the resulting policy
	policy_spec(C1b),

	% compare the last resulting policy to the earlier result
	echo('comparing last result to first result, expect equal'),
	compare_v(C1a, C1b, R1), echo(R1)
	]).

proc(meta_add, [
		reset,
		reset(policy,dplp3),
		addm(dplp3,[data_controller('dc[x]',[])]),
		addm(dplp3,[data_processor('dp[y][x]', [], 'dc[x]')]),
		addm(dplp3,[data_subject('ds[1]',['pdi(1)[1]':'pdc{1}'],[])]),
		access(dplp3, ('dp[y][x]', 'dpo(z)', 'p(v)', 'pdi(1)[1]') ),
		addm(dplp3,[consent(cID_234,'dc[x]','dp[y][x]','app(a,y,x)',
			['dpo(z)'],'p(v)','ds[1]','pdi(1)[1]','pdc{1}',true)]),
		access(dplp3, ('dp[y][x]', 'dpo(z)', 'p(v)', 'pdi(1)[1]') ),
		addm(dplp3, [data_subject('ds[2]',[],[])]),
		addm(dplp3, [ data_item('pdi(1)[2]', 'pdc{1}', 'ds[2]') ]),
		access(dplp3, ('dp[y][x]', 'dpo(z)', 'p(v)', 'pdi(1)[2]') ),
		addm(dplp3, [consent(cID_567,'dc[x]','dp[y][x]','app(a,y,x)',
			['dpo(z)'],'p(v)','ds[2]','pdi(1)[2]','pdc{1}',true)]),
		access(dplp3, ('dp[y][x]', 'dpo(z)', 'p(v)', 'pdi(1)[1]') ),
		access(dplp3, ('dp[y][x]', 'dpo(z)', 'p(v)', 'pdi(1)[2]') ),
		delete_consent(cID_234),
		access(dplp3, ('dp[y][x]', 'dpo(z)', 'p(v)', 'pdi(1)[1]') ),
		access(dplp3, ('dp[y][x]', 'dpo(z)', 'p(v)', 'pdi(1)[2]') ),
		delete_consent(cID_567),
		access(dplp3, ('dp[y][x]', 'dpo(z)', 'p(v)', 'pdi(1)[1]') ),
		access(dplp3, ('dp[y][x]', 'dpo(z)', 'p(v)', 'pdi(1)[2]') )
	]).

proc(build_consent_ex, [
	]).

proc(consent_ex, [
	% policy(consent_ex1, cpol_ex, [
	% 	policy_class(cpol_ex),
	% 	assign(cpol_ex,'PM'),
	% 	user_attribute(data_controllers),
	% 	object_attribute(data_subjects),
	% 	assign(data_controllers,cpol_ex),
	% 	assign(data_subjects,cpol_ex),
	  
	% 	user_attribute('dc_[x]'),
	% 	assign('dc_[x]', data_controllers),
	  
	% 	user('dp_[y][x]'),
	% 	assign('dp_[y][x]', 'dc_[x]'),
	  
	% 	object_attribute('ds_[1]'),
	% 	assign('ds_[1]', data_subjects),  
	  
	% 	object_attribute('ds_[2]'),
	% 	assign('ds_[2]', data_subjects),  
	  
	% 	object('pdi_(1)[1]'),
	% 	assign('pdi_(1)[1]', 'ds_[1]'),
	% 	assign('pdi_(1)[1]', 'pdc_{1}'),
	  
	% 	object('pdi_(1)[2]'),
	% 	assign('pdi_(1)[2]', 'ds_[2]'),
	% 	assign('pdi_(1)[2]', 'pdc_{1}'),
	  
	% 	object_attribute('pdc_{1}'), % should not need this if defined in definitions
	% 	assign('pdc_{1}',cpol_ex), % should not need this if defined in definitions
	  
	% 	consent(cID_567,'dc_[x]','dp_[y][x]','app(a,y,x)',['dpo_(z)'],'p_(v)','ds_[2]','pdi_(1)[2]','pdc_{1}',true),
	  
	% 	connector('PM')
	% 	], dplp).
		reset,
		reset(policy,consent_ex),
		policy_spec(consent_ex),
		deletem(consent2,[],cID_234),
		policy_spec(consent_ex),
		deletem(consent2,[],cID_567),
		policy_spec(consent_ex),
		deletem(consent2,[],'pdi_(2)[1]'),
		policy_spec(consent_ex),
		deletem(consent2,[],'ds_[2]'),
		policy_spec(consent_ex),
		deletem(consent2,[],'ds_[1]'),
		policy_spec(consent_ex),
		deletem(consent2,[],'dp_[y][x]'),
		policy_spec(consent_ex),
		deletem(consent2,[],'dc_[x]'),
	  	policy_spec(consent_ex)
	]).

proc(vartest, [getpol_v(X),echo(X),policy_spec_v('Policy (a)',Y), echo(Y)]).
proc(vartest1, [getpol_v(X),echo(X),setpol('Policy (a)'),getpol_v(Y),echo(Y)]).
proc(vartest2, [policy_spec_v('Policy (a)',Y), policy_spec_v('Policy (a)',X,Y), echo(X)]).
proc(vartest3, [policy_spec_v('Policy (a)',X), echo(X),
				policy_spec_v('Policy (b)',Y), echo(Y), compare_v(X,Y,R), echo(R)]).

proc(meta_demo, [
	reset(policy,consent2),
	setpol(consent2),
	policy_spec,
	access(consent2, ('dp[y][x]', 'dpo(z)', 'p(v)', 'pdi(1)[1]') ),
	access(consent2, ('dp[y][x]', 'dpo(z)', 'p(v)', 'pdi(1)[2]') ),
	delete_consent(consent2, cID_234),
	policy_spec,
	access(consent2, ('dp[y][x]', 'dpo(z)', 'p(v)', 'pdi(1)[1]') ),
	access(consent2, ('dp[y][x]', 'dpo(z)', 'p(v)', 'pdi(1)[2]') ),
	delete_consent(consent2, cID_567),
	policy_spec,
	access(consent2, ('dp[y][x]', 'dpo(z)', 'p(v)', 'pdi(1)[1]') ),
	access(consent2, ('dp[y][x]', 'dpo(z)', 'p(v)', 'pdi(1)[2]') )
]).

proc(consent_me_demo, [
		reset(policy,consent1),
		setpol(consent1),
		policy_spec,
		access(consent1, ('dp[y][x]', 'dpo(z)', 'p(v)', 'pdi(1)[1]') ),
		add_consent(consent1,
			consent(cID_234,'dc[x]','dp[y][x]','app(a,y,x)',['dpo(z)'],'p(v)','ds[1]','pdi(1)[1]','pdc{1}',true)),
		policy_spec,
		access(consent1, ('dp[y][x]', 'dpo(z)', 'p(v)', 'pdi(1)[1]') ),
		delete_consent(consent1, cID_234),
		policy_spec,
		access(consent1, ('dp[y][x]', 'dpo(z)', 'p(v)', 'pdi(1)[1]') )
	]).

proc(policy_sat_demo, [
		reset(policy,dplp_sat_test),
		setpol(dplp_sat_test),
		echo('Expect: sat unsat sat unsat sat unsat sat sat unsat unsat'),
		policy_sat(dp3,ds3),
		policy_sat(dp4,ds4),
		policy_sat(dp5,ds5),
		policy_sat(dp6,ds6),
		policy_sat(dp7,ds7),
		policy_sat(dp8,ds8),
		policy_sat(dp9,ds9),
		policy_sat(dp10,ds10),
		policy_sat(dp11,ds11),
		policy_sat(dp12,ds12)
	]).

proc(guiserver, [
         set(guiserver,on),
         guitracer,
         set(jsonresp_server,on),
         set(jsonresp,on),
         set(no_sleep,on),
         traceone,
         server(8001),
         echo(ready)
     ]).

proc(review, [ %
         proc(queryA),
         proc(queryB),
         proc(autoCombined),
	 selftest,
         server(8001)
     ]).

proc(queryA, [ % query the example Policy (a)
	 newpol('Policy (a)'),
         policy_spec,
         echo('access queries, expect G G G D D D D D G D G G G G D D'),
	 access('Policy (a)',(u1,r,o1)),
	 access('Policy (a)',(u1,w,o1)),
	 access('Policy (a)',(u1,r,o2)),
	 access('Policy (a)',(u1,w,o2)),
	 access('Policy (a)',(u1,r,o3)),
	 access('Policy (a)',(u1,w,o3)),
	 access('Policy (a)',(u1,r,o4)),
	 access('Policy (a)',(u1,w,o4)),
	 access('Policy (a)',(u2,r,o1)),
	 access('Policy (a)',(u2,w,o1)),
	 access('Policy (a)',(u2,r,o2)),
	 access('Policy (a)',(u2,w,o2)),
	 access('Policy (a)',(u2,r,o3)),
	 access('Policy (a)',(u2,w,o3)),
	 access('Policy (a)',(u2,r,o4)),
	 access('Policy (a)',(u2,w,o4)),
         echo('Displaying policy graph'),
         policy_graph
     ]).

proc(queryB, [ % query the example Policy (b)
	 newpol('Policy (b)'),
         policy_spec,
         echo('access queries, expect D D G G D D D D D D G G G G G G'),
	 access('Policy (b)',(u1,r,o1)),
	 access('Policy (b)',(u1,w,o1)),
	 access('Policy (b)',(u1,r,o2)),
	 access('Policy (b)',(u1,w,o2)),
	 access('Policy (b)',(u1,r,o3)),
	 access('Policy (b)',(u1,w,o3)),
	 access('Policy (b)',(u1,r,o4)),
	 access('Policy (b)',(u1,w,o4)),
	 access('Policy (b)',(u2,r,o1)),
	 access('Policy (b)',(u2,w,o1)),
	 access('Policy (b)',(u2,r,o2)),
	 access('Policy (b)',(u2,w,o2)),
	 access('Policy (b)',(u2,r,o3)),
	 access('Policy (b)',(u2,w,o3)),
	 access('Policy (b)',(u2,r,o4)),
	 access('Policy (b)',(u2,w,o4)),
         echo('Displaying policy graph'),
         policy_graph
     ]).

proc(autoCombined, [
	 import(policy('EXAMPLES/policy_signals_access.pl')),
         echo('access queries, expect G G D D D D D D'),
	 access('Signals Access Policy', ('Ana',r,'VIN-1001 Door Signals')),
	 access('Signals Access Policy', ('Ana',r,'VIN-3001 Shift Signals')),
	 access('Signals Access Policy', ('Ana',r,'VIN-1001 Trip Signals')),
	 access('Signals Access Policy', ('Ana',r,'VIN-3001 Trip Signals')),
	 access('Signals Access Policy', ('Ana',w,'VIN-1001 Door Signals')),
	 access('Signals Access Policy', ('Ana',w,'VIN-3001 Shift Signals')),
	 access('Signals Access Policy', ('Ana',w,'VIN-1001 Trip Signals')),
	 access('Signals Access Policy', ('Ana',w,'VIN-3001 Trip Signals')),
	 import(policy('EXAMPLES/policy_vehicle_ownership.pl')),
         echo('access queries, expect D G D G D D D D'),
	 access('Vehicle Ownership Policy', ('Ana',r,'VIN-1001 Door Signals')),
	 access('Vehicle Ownership Policy', ('Ana',r,'VIN-3001 Shift Signals')),
	 access('Vehicle Ownership Policy', ('Ana',r,'VIN-1001 Trip Signals')),
	 access('Vehicle Ownership Policy', ('Ana',r,'VIN-3001 Trip Signals')),
	 access('Vehicle Ownership Policy', ('Ana',w,'VIN-1001 Door Signals')),
	 access('Vehicle Ownership Policy', ('Ana',w,'VIN-3001 Shift Signals')),
	 access('Vehicle Ownership Policy', ('Ana',w,'VIN-1001 Trip Signals')),
	 access('Vehicle Ownership Policy', ('Ana',w,'VIN-3001 Trip Signals')),
	 combine('Signals Access Policy','Vehicle Ownership Policy','Combined Policy'),
	 newpol('Combined Policy'),
         echo('Using Combined Policy'),
         echo('access queries, expect D G G D D D D D D'),
	 access('Combined Policy', ('Ana',r,'VIN-1001 Door Signals')),
	 access('Combined Policy', ('Sebastian',r,'VIN-1001 Door Signals')),
	 access('Combined Policy', ('Ana',r,'VIN-3001 Shift Signals')),
	 access('Combined Policy', ('Ana',r,'VIN-1001 Trip Signals')),
	 access('Combined Policy', ('Ana',r,'VIN-3001 Trip Signals')),
	 access('Combined Policy', ('Ana',w,'VIN-1001 Door Signals')),
	 access('Combined Policy', ('Ana',w,'VIN-3001 Shift Signals')),
	 access('Combined Policy', ('Ana',w,'VIN-1001 Trip Signals')),
	 access('Combined Policy', ('Ana',w,'VIN-3001 Trip Signals')),
         policy_graph
     ]).

proc(modelTestA, [
	 newpol('Policy (a)'),
	 los('Policy (a)'),
	 dps('Policy (a)'),
	 aoa(u1),
	 minaoa(u1),
	 userlos('Policy (a)',u1),
	 aoa(u2),
	 minaoa(u2),
	 userlos('Policy (a)',u2)
     ]).

proc(modelTestB, [
	 newpol('Policy (b)'),
	 los('Policy (b)'),
	 dps('Policy (b)'),
	 aoa(u1),
	 minaoa(u1),
	 userlos('Policy (b)',u1),
	 aoa(u2),
	 minaoa(u2),
	 userlos('Policy (b)',u2)
     ]).

proc(demo1, [ %
	 import(policy('EXAMPLES/policy3.pl')),
	 newpol('Policy3'),
	 access('Policy3',(jones,read,mrec1)),
	 access('Policy3',(jones,write,mrec1)),
	 access('Policy3',(smith,read,mrec1)),
	 access('Policy3',(smith,write,mrec1)),
	 access('Policy3',(smith,read,'Medical Records')) % OA not Object
     ]).

proc(demo2, [ % convert a declarative policy file to a PM command file
	 import(policy('EXAMPLES/Policy3.pl')),
	 newpol('Policy3'),
	 decl2imp('EXAMPLES/policy3.pl','EXAMPLES/policy3.pm')
     ]).

proc(demo3, [ %
	 combine('Policy (a)','Policy (b)','Policy (ab)'),
	 dps('Policy (ab)')
     ]).

proc(marketdemo, [ % demo the market policy
         setpol(mpolicy1),
         load_cond('EXAMPLES/market_cond.pl'),
         users('device_95b40cf9-a9fc-4bd8-b695-99773b6f25e4',r,
               [devid='95b40cf9-a9fc-4bd8-b695-99773b6f25e4', mchan=2,
                tstart='2020-09-08T08:00:00Z', tstop='2020-09-09T08:00:00Z', tsubmit='2020-09-09T08:03:22.350069Z',
                loMin=3.419216, loMax=3.519216, laMin=40.062069, laMax=40.072069]
              ),
         noop
     ]).

proc(test_echo, [
	 echo('hello world')
     ]).

proc(auto1, [
	 import(policy('EXAMPLES/policy_signals_access.pl')),
         echo('access queries, expect G G D D D D D D'),
	 access('Signals Access Policy', ('Ana',r,'VIN-1001 Door Signals')),
	 access('Signals Access Policy', ('Ana',r,'VIN-3001 Shift Signals')),
	 access('Signals Access Policy', ('Ana',r,'VIN-1001 Trip Signals')),
	 access('Signals Access Policy', ('Ana',r,'VIN-3001 Trip Signals')),
	 access('Signals Access Policy', ('Ana',w,'VIN-1001 Door Signals')),
	 access('Signals Access Policy', ('Ana',w,'VIN-3001 Shift Signals')),
	 access('Signals Access Policy', ('Ana',w,'VIN-1001 Trip Signals')),
	 access('Signals Access Policy', ('Ana',w,'VIN-3001 Trip Signals'))
     ]).

proc(auto2, [
	 import(policy('EXAMPLES/policy_vehicle_ownership.pl')),
         echo('access queries, expect D G D G D D D D'),
	 access('Vehicle Ownership Policy', ('Ana',r,'VIN-1001 Door Signals')),
	 access('Vehicle Ownership Policy', ('Ana',r,'VIN-3001 Shift Signals')),
	 access('Vehicle Ownership Policy', ('Ana',r,'VIN-1001 Trip Signals')),
	 access('Vehicle Ownership Policy', ('Ana',r,'VIN-3001 Trip Signals')),
	 access('Vehicle Ownership Policy', ('Ana',w,'VIN-1001 Door Signals')),
	 access('Vehicle Ownership Policy', ('Ana',w,'VIN-3001 Shift Signals')),
	 access('Vehicle Ownership Policy', ('Ana',w,'VIN-1001 Trip Signals')),
	 access('Vehicle Ownership Policy', ('Ana',w,'VIN-3001 Trip Signals'))
     ]).

proc(autocomb, [
	 combine('Signals Access Policy','Vehicle Ownership Policy','Combined Policy'),
	 newpol('Combined Policy'),
         echo('access queries, expect D G G D D D D D D'),
	 access('Combined Policy', ('Ana',r,'VIN-1001 Door Signals')),
	 access('Combined Policy', ('Sebastian',r,'VIN-1001 Door Signals')),
	 access('Combined Policy', ('Ana',r,'VIN-3001 Shift Signals')),
	 access('Combined Policy', ('Ana',r,'VIN-1001 Trip Signals')),
	 access('Combined Policy', ('Ana',r,'VIN-3001 Trip Signals')), % <==
	 access('Combined Policy', ('Ana',w,'VIN-1001 Door Signals')),
	 access('Combined Policy', ('Ana',w,'VIN-3001 Shift Signals')),
	 access('Combined Policy', ('Ana',w,'VIN-1001 Trip Signals')),
	 access('Combined Policy', ('Ana',w,'VIN-3001 Trip Signals'))
     ]).

