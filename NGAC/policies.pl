:- module(policies, [policy/3,policy/4,gg_policy/2,cc_policy/2]).
% Example policies used for built-in self-test
%   All policies defined in this module will be automatically included
%   in the initialization of the policy database in dpl/pip.
%   'Policy (a)', 'Policy (b)', 'Signals Access Policy' and 'Vehicle Ownership Policy'
%   and others that may be added up to the comment END OF TEST POLICIES
%   are assumed to be available to the self tests for pdp/pip.
%   DO NOT REMOVE THEM!
%   It is OK to add others.
%
% policy(PolicyName, PolicyRoot, PolicyElements, PolicyType)
% gg_policy(PolicyName, PolicyRoot, PolicyElements)
% cc_policy(PolicyName, PolicyRoot, PolicyElements)

:- dynamic policy/3, policy/4, gg_policy/2, cc_policy/2.
:- discontiguous policy/3, policy/4, gg_policy/2, cc_policy/2.

% Do not modify the following policies down to the % test policies marker

policy(dplp_sat_test, pc, [
	purpose(p12),
	purpose(p112),
	purpose(p111),
	purpose(p11),
	purpose(p1),
	purpose(p221),
	purpose(p22),
	purpose(p21),
	purpose(p2),
	purpose('Purpose'),
	operation(dpo12),
	operation(dpo11),
	operation(dpo1),
	operation(dpo22),
	operation(dpo21),
	operation(dpo2),
	operation('Processing'),
	user_attribute(data_controllers),
	object_attribute(data_subjects),
	data_type('Personal Data Category'),
	data_type(dt1),
	data_type(dt2),
	assign(dt1,'Personal Data Category'),
	assign(dt2,'Personal Data Category'),
	assign(dpo1,'Processing'),
	assign(dpo11,dpo1),
	assign(dpo12,dpo1),
	assign(dpo2,'Processing'),
	assign(dpo21,dpo2),
	assign(dpo22,dpo2),
	assign(p1,'Purpose'),
	assign(p11,p1),
	assign(p12,p1),
	assign(p111,p11),
	assign(p112,p11),
	assign(p2,'Purpose'),
	assign(p21,p2),
	assign(p22,p2),
	assign(p221,p22),
	assign(data_subjects,pc),
	assign(data_controllers,pc),
	data_controller(dc1,[]),
  data_processor(dp3,[(p112,dpo22,dt1)],dc1),
  data_processor(dp4,[(p1,dpo22,dt1)],dc1),
  data_processor(dp5,[(p112,dpo2,dt1)],dc1),
  data_processor(dp6,[(p112,dpo1,dt1)],dc1),
  data_processor(dp7,[(p11,dpo22,dt1),(p112,dpo21,dt1)],dc1),
  data_processor(dp8,[(p1,dpo22,dt1),(p112,dpo21,dt1)],dc1),
  data_processor(dp9,[(p1,dpo22,dt1),(p112,dpo21,dt1)],dc1),
  data_processor(dp10,[(p112,dpo1,dt1),(p112,dpo21,dt1)],dc1),
  data_processor(dp11,[(p112,dpo1,dt1),(p112,dpo21,dt1),(p111,dpo12,dt1)],dc1),
  data_processor(dp12,[(p112,dpo1,dt1),(p112,dpo21,dt1),(p111,dpo12,dt1)],dc1),
  data_subject(ds3,[di3_1:dt1],[(p11,dpo2,di3_1)]),
  data_subject(ds4,[di4_1:dt1],[(p11,dpo2,di4_1)]),
  data_subject(ds5,[di5_1:dt1],[(p11,dpo2,di5_1)]),
  data_subject(ds6,[di6_1:dt1],[(p11,dpo2,di6_1)]),
  data_subject(ds7,[di7_1:dt1],[(p1,dpo2,di7_1)]),
  data_subject(ds8,[di8_1:dt1],[(p11,dpo2,di8_1)]),
  data_subject(ds9,[di9_1:dt1],[(p1,dpo2,di9_1),(p1,dpo2,di9_1)]),
  data_subject(ds10,[di10_1:dt1],[(p11,dpo2,di10_1),(p112,dpo1,di10_1)]),
  data_subject(ds11,[di11_1:dt1],[(p11,dpo2,di11_1),(p112,dpo1,di11_1)]),
  data_subject(ds12,[di12_1:dt1],[(p11,dpo22,di12_1),(p112,dpo1,di12_1)]),
  application(app1,[dpo21,dpo22],dp1),
  consent(cID_01,dc1,dp1,none,[dpo1],p1,ds1,di1_1,dt1,true),
	policy_class(pc),
	assign(pc,'PM'),
	connector('PM')], dplp).

policy(testdefs2, testdefs2, [
  data_type('pdc{1}'), % object_attribute('pdc{1}'), 
  data_type('pdc{2}'), % object_attribute('pdc{2}'), 
  data_type('Personal Data Category'), %  object_attribute('PersonalDataCategory'),
  
  assign('pdc{1}','Personal Data Category'),
  assign('pdc{2}','Personal Data Category'),

  operation('dpo(u)'),
  operation('dpo(w)'),
  operation('dpo(z)'),
  operation('Processing'),
  assign('dpo(u)','Processing'),
  assign('dpo(w)','Processing'),
  assign('dpo(z)','Processing'),

  purpose('p(v)'),
  purpose('p(r)'),
  purpose('p(s)'),
  purpose('Purpose'),

  assign('p(v)','Purpose'),
  assign('p(r)','Purpose'),
  assign('p(s)','Purpose')
], dplp).

policy(testdefs1, testdefs1, [
  user_attribute(data_controllers),
  object_attribute(data_subjects),
  data_type('Personal Data Category'),

  data_type(dt1),
  data_type(dt2),

  assign(dt1,'Personal Data Category'),
  assign(dt2,'Personal Data Category'),

  % object(di1,dt1), % data item
  % object(di2,dt2),
  % object(di3,dt2),
  % data_item(di1,dt1,ds1),
  % data_item(di2,dt2,ds1),
  % data_item(di3,dt2,ds2),

  % object_attribute(ds1), % data subject
  %data_subject(ds1, [di1:dt1], [(p11,dpo2,di1), (p112,dpo1,di1)]),

  % user_attribute(dc1), % data controller
  % user_attribute(dc2), % data controller
  %data_controller(dc1, [(p112,dpo1,dt1), (p112,dpo21,dt1)]),
  %data_controller(dc2, [], []),

  operation(dpo12),
  operation(dpo11),
  operation(dpo1),
  operation(dpo22),
  operation(dpo21),
  operation(dpo2),
  operation('Processing'),

  assign(dpo1,'Processing'),
  assign(dpo11,dpo1),
  assign(dpo12,dpo1),
  assign(dpo2,'Processing'),
  assign(dpo21,dpo2),
  assign(dpo22,dpo2),

  purpose(p12),
  purpose(p112),
  purpose(p111),
  purpose(p11),
  purpose(p1),
  purpose(p221),
  purpose(p22),
  purpose(p21),
  purpose(p2),
  purpose('Purpose'),

  assign(p1,'Purpose'),
  assign(p11,p1),
  assign(p12,p1),
  assign(p111,p11),
  assign(p112,p11),
  assign(p2,'Purpose'),
  assign(p21,p2),
  assign(p22,p2),
  assign(p221,p22)
], dplp).

policy(testdefs1a, testdefs1a, [
  data_type(dt1,[]),
  data_type(dt2,[]),

  object(di1,dt1), % data item
  object(di2,dt2),
  object(di3,dt2),

  % assign(di1,dt1),
  % assign(di2,dt2),
  % assign(di3,dt2),

  object_attribute(data_subjects),

  object_attribute(ds1), % data subject
  object_attribute(ds2), % data subject

  user_attribute(dc1), % data controller

  user_attribute(dp1), % data processor
  user_attribute(dp2), % data processor

  user_attribute(data_controllers),

  assign(dc1,data_controllers),
  assign(dp1,dc1),
  assign(dp2,dc1),

  operation(dpo12),
  operation(dpo11),
  operation(dpo1),
  operation(dpo22),
  operation(dpo21),
  operation(dpo2),
  operation(all_operations),

  assign(dpo1,all_operations),
  assign(dpo11,dpo1),
  assign(dpo12,dpo1),
  assign(dpo2,all_operations),
  assign(dpo21,dpo2),
  assign(dpo22,dpo2),

  purpose(p12),
  purpose(p112),
  purpose(p111),
  purpose(p11),
  purpose(p1),
  purpose(p221),
  purpose(p22),
  purpose(p21),
  purpose(p2),
  purpose(all_purposes),

  assign(p1,all_purposes),
  assign(p11,p1),
  assign(p12,p1),
  assign(p111,p11),
  assign(p112,p11),
  assign(p2,all_purposes),
  assign(p21,p2),
  assign(p22,p2),
  assign(p221,p22)
  ], dplp).

policy(testdefs, testdefs, [
  data_type(dt1),
  data_type(dt2),
  data_type('Personal Data Category'),
  assign(dt1,'Personal Data Category'),
  assign(dt2,'Personal Data Category'),

  object(di1,dt1), % data item
  object(di2,dt2),
  object(di3,dt2),

  % assign(di1,dt1),
  % assign(di2,dt2),
  % assign(di3,dt2),

  %object_attribute(ds1), % data subject
  %user_attribute(dc1), % data controller
  %user_attribute(dc2), % data controller

  operation(dpo12),
  operation(dpo11),
  operation(dpo1),
  operation(dpo22),
  operation(dpo21),
  operation(dpo2),
  operation('Processing'),

  assign(dpo1,'Processing'),
  assign(dpo11,dpo1),
  assign(dpo12,dpo1),
  assign(dpo2,'Processing'),
  assign(dpo21,dpo2),
  assign(dpo22,dpo2),

  purpose(p12),
  purpose(p112),
  purpose(p111),
  purpose(p11),
  purpose(p1),
  purpose(p221),
  purpose(p22),
  purpose(p21),
  purpose(p2),
  purpose('Purpose'),

  assign(p1,'Purpose'),
  assign(p11,p1),
  assign(p12,p1),
  assign(p111,p11),
  assign(p112,p11),
  assign(p2,'Purpose'),
  assign(p21,p2),
  assign(p22,p2),
  assign(p221,p22)

  ], dplp).

policy(dplp_min, pc, [], dplp).

policy(dplp_base, cpol, [
  dplp_policy_base(cpol, testdefs2)
], dplp).

policy(dplp4, cpol, [
  dplp_policy_base(cpol, testdefs2),
  application( 'dp[y][x]_app1', ['dpo(w)','dpo(z)'], 'dp[y][x]'),
  data_controller('dc[x]', []),
  data_processor('dp[y][x]',[],'dc[x]'),
  data_subject('ds[1]', ['pdi(1)[1]':'pdc{1}'], []),
  data_item('pdi(2)[1]','pdc{2}','ds[1]'),
  consent(cID_234,'dc[x]','dp[y][x]','dp[y][x]_app1',['dpo(z)'],'p(v)','ds[1]','pdi(1)[1]','pdc{1}',true),
  data_subject('ds[2]', ['pdi(1)[2]':'pdc{1}'], []),
  consent(cID_567,'dc[x]','dp[y][x]','dp[y][x]_app1',['dp[y][x]_app1'],'p(v)','ds[2]','pdi(1)[2]','pdc{1}',true)
], dplp).

policy(dplp3, cpol, [
  dplp_policy_base(cpol, testdefs2),
  opset( 'dp[y][x]_app1', ['dpo(w)','dpo(z)'] ),
  data_controller('dc[x]', []),
  data_processor('dp[y][x]',[],'dc[x]'),
  data_subject('ds[1]', ['pdi(1)[1]':'pdc{1}'], []),
  data_item('pdi(2)[1]','pdc{2}','ds[1]'),
  consent(cID_234,'dc[x]','dp[y][x]','app(a,y,x)',['dpo(z)'],'p(v)','ds[1]','pdi(1)[1]','pdc{1}',true),
  data_subject('ds[2]', ['pdi(1)[2]':'pdc{1}'], []),
  consent(cID_567,'dc[x]','dp[y][x]','app(a,y,x)',['dp[y][x]_app1'],'p(v)','ds[2]','pdi(1)[2]','pdc{1}',true)
], dplp).

policy(dplp2, cpol_ex, [
  policy_class(cpol_ex),
  assign(cpol_ex,'PM'),
  user_attribute(data_controllers),
  object_attribute(data_subjects),
  assign(data_controllers,cpol_ex),
  assign(data_subjects,cpol_ex),
  object_attribute('pdc{1}'),
  object_attribute('pdc{2}'),
  object_attribute('PersonalDataCategory'),
  assign('pdc{1}','PersonalDataCategory'),
  assign('pdc{2}','PersonalDataCategory'),
  assign('PersonalDataCategory',cpol_ex),
  operation('dpo(u)'),
  operation('dpo(w)'),
  operation('dpo(z)'),
  opset( 'dp[y][x]_app1', ['dpo(w)','dpo(z)'] ),

  data_controller('dc[x]', []),
  data_processor('dp[y][x]',[],'dc[x]'),
  data_subject('ds[1]', ['pdi(1)[1]':'pdc{1}'], []),
  data_item('pdi(2)[1]','pdc{2}','ds[1]'),
  consent(cID_234,'dc[x]','dp[y][x]','app(a,y,x)',['dpo(z)'],'p(v)','ds[1]','pdi(1)[1]','pdc{1}',true),

  data_subject('ds[2]', ['pdi(1)[2]':'pdc{1}'], []),
  consent(cID_567,'dc[x]','dp[y][x]','app(a,y,x)',['dp[y][x]_app1'],'p(v)','ds[2]','pdi(1)[2]','pdc{1}',true)
],dplp).

policy(consent_ex1, cpol_ex, [
  policy_class(cpol_ex),
  assign(cpol_ex,'PM'),
  user_attribute(data_controllers),
  object_attribute(data_subjects),
  assign(data_controllers,cpol_ex),
  assign(data_subjects,cpol_ex),

  user_attribute('dc[x]'),
  assign('dc[x]', data_controllers),

  user('dp[y][x]'),
  assign('dp[y][x]', 'dc[x]'),

  object_attribute('ds[1]'),
  assign('ds[1]', data_subjects),  

  object_attribute('ds[2]'),
  assign('ds[2]', data_subjects),  

  object('pdi(1)[1]'),
  assign('pdi{1)[1]', 'ds[1]'),
  assign('pdi(1)[1]', 'pdc{1}'),

  object('pdi(1)[2]'),
  assign('pdi(1)[2]', 'ds[2]'),
  assign('pdi(1)[2]', 'pdc{1}'),

  object_attribute('pdc{1}'), % should not need this if defined in definitions
  assign('pdc{1}',cpol_ex), % should not need this if defined in definitions

  consent(cID_567,'dc[x]','dp[y][x]','app(a,y,x)',['dpo(z)'],'p(v)','ds[2]','pdi(1)[2]','pdc{1}',true),

  connector('PM')
  ], dplp).

% consent1 is used by the consent_me_demo in procs_ngac
policy(consent1,cpol1,[
  definitions(core_ontology),
  opset( 'dp[y][x]_app1', ['dpo(w)','dpo(z)'] ),
  
  user_attribute(data_controllers),
  object_attribute(data_subjects),

  user_attribute('dc[x]'),
  assign('dc[x]',data_controllers),
  user('dp[y][x]'),
  assign('dp[y][x]','dc[x]'),

  object_attribute('ds[1]'),
  assign('ds[1]',data_subjects),

  object('pdi(1)[1]'),
  %object_attribute('pdc{1}'),
  data_type('pdc{1}'),
  assign('pdc{1}',cpol1),
  assign('pdi(1)[1]','pdc{1}'),
  assign('pdi(1)[1]','ds[1]'),

  assign(data_controllers,cpol1),
  assign(data_subjects,cpol1),
  policy_class(cpol1),
  assign(cpol1,'PM'),
  connector('PM')
], dplp).

policy(cons1,cpol1,[
  user_attribute(data_controllers),
  object_attribute(data_subjects),
  user_attribute('dc[x]'),
  assign('dc[x]',data_controllers),
  user_attribute('dp[y][x]'),
  assign('dp[y][x]','dc[x]'),
  user('app_(a)[y][x]'),
  assign('app_(a)[y][x]','dp[y][x]'),
  object_attribute('ds[1]'),
  assign('ds[1]',data_subjects),
  object('pdi(1)[1]'),
  object_attribute('pdc{1}'),
  assign('pdi(1)[1]','pdc{1}'),
  assign('pdi(1)[1]','ds[1]'),
  assign(data_controllers,cpol1),
  assign(data_subjects,cpol1),
  policy_class(cpol1),
  assign(cpol1,'PM')%,
  %connector('PM')
], dplp).

policy(priv1,priv_pol1,[
	policy_class(priv_pol1),
	assign(priv_pol1,'PM'),

        user(s1), % interpretation: service(s1)
        user_attribute(dc1), % interpretation: data_controller(dc1)
        user_attribute(data_controllers),
        object(pii1), % interpretation: PII#1 of DS ds1
        object_attribute(ds1), % interpretation: data_subject(ds1)
        object_attribute(data_subjects),
        operation(r), % interpretation: data processing operation r
        assign(s1,dc1), % interpretation: u1 is offered by ua1
        assign(pii1,ds1), % interpretation: pii1 is PII item of ds1
        assign(ds1,data_subjects),
        assign(dc1,data_controllers),
        assign(data_subjects,priv_pol1),
        assign(data_controllers,priv_pol1),

        % interpretation: DS allows asc(dc1) to use asc(ds1) for [r] ops for purpose p11
        associate(dc1, [r], p11, ds1),

        purpose(p12),
        purpose(p112),
        purpose(p111),
        purpose(p11),
        purpose(p1),
        purpose(p221),
        purpose(p22),
        purpose(p21),
        purpose(p2),
        purpose(all_purposes),

        assign(p1,all_purposes),
        assign(p11,p1),
        assign(p12,p1),
        assign(p111,p11),
        assign(p112,p11),
        assign(p2,all_purposes),
        assign(p21,p2),
        assign(p22,p2),
        assign(p221,p22),
        assign(all_purposes,'PM'),

        % access(priv1,(s1,r,pii1,p112))=grant %interpretation: service s1 wants to r op on pii1 for purpose p112
        % Proof:
        %   associate(dc1, [r], p11, ds1)
        %   s1<=dc1, r<=[r], pii1<=ds1, p112<=p11
        % QED

	connector('PM')

        % privacy policy for DC
        %
        ],dplp).

policy('Policy (ap)','Privacy Access', [
	user('u1'),
	user('u2'),
	user_attribute('Group1'),
        user_attribute('Group2'),
        user_attribute('Division'),
	object('o1'),
        object('o2'),
        object('o3'),
        object_attribute('Project1'),
        object_attribute('Project2'),
        object_attribute('Gr2-Secret'),
        object_attribute('Projects'),
	assign('u1','Group1'),
	assign('u2','Group2'),
	assign('Group1','Division'),
	assign('Group2','Division'),
	assign('o1','Project1'),
	assign('o2','Project2'),
	assign('o3','Gr2-Secret'),
	assign('Project1','Projects'),
	assign('Project2','Projects'),
	assign('Division','Privacy Access'),
	assign('Projects','Privacy Access'),

	assign('Gr2-Secret','Privacy Access'),
	operation(r,'File'),
	operation(w,'File'),
	associate('Group1',[w],'Project1'),
	associate('Group2',[w],'Project2'),
	associate('Group2',[r,w], p11,'Gr2-Secret'),
	associate('Division',[r], p22,'Projects'),

        purpose(p12),
        purpose(p112),
        purpose(p111),
        purpose(p11),
        purpose(p1),
        purpose(p221),
        purpose(p22),
        purpose(p21),
        purpose(p2),
        purpose(all_purposes),

        assign(p1,all_purposes),
        assign(p11,p1),
        assign(p12,p1),
        assign(p111,p11),
        assign(p112,p11),
        assign(p2,all_purposes),
        assign(p21,p2),
        assign(p22,p2),
        assign(p221,p22),
        % assign(all_purposes,'PM'),

	policy_class('Privacy Access'),
        assign('Privacy Access','PM'),
	connector('PM')
	],dpl).

policy('Policy (a)','Project Access', [
	user('u1'),
	user('u2'),
	user_attribute('Group1'),
        user_attribute('Group2'),
        user_attribute('Division'),
	object('o1'),
        object('o2'),
        object('o3'),
        object_attribute('Project1'),
        object_attribute('Project2'),
        object_attribute('Gr2-Secret'),
        object_attribute('Projects'),
	policy_class('Project Access'),
	connector('PM'),
	assign('u1','Group1'),
	assign('u2','Group2'),
	assign('Group1','Division'),
	assign('Group2','Division'),
	assign('o1','Project1'),
	assign('o2','Project2'),
	assign('o3','Gr2-Secret'),
	assign('Project1','Projects'),
	assign('Project2','Projects'),
	assign('Division','Project Access'),
	assign('Projects','Project Access'),
	assign('Gr2-Secret','Project Access'),
        assign('Project Access','PM'),
	operation(r,'File'),
	operation(w,'File'),
	associate('Group1',[w],'Project1'),
	associate('Group2',[w],'Project2'),
	associate('Group2',[r,w],'Gr2-Secret'),
	associate('Division',[r],'Projects')
	],dpl).

policy('Policy (b)','File Management', [
	user('u1'),
	user('u2'),
	user_attribute('Alice'),
	user_attribute('Bob'),
	user_attribute('Users'),
	object('o2'),
	object('o3'),
	object('o4'),
	object_attribute('Proposals'),
	object_attribute('Reports'),
	object_attribute('Bob Home'),
	policy_class('File Management'),
	connector('PM'),
	assign('u1','Alice'),
	assign('u2','Bob'),
	assign('Alice','Users'),
	assign('Bob','Users'),
	assign('o2','Proposals'),
	assign('o3','Reports'),
	assign('o4','Reports'),
	assign('Proposals','Bob Home'),
	assign('Reports','Bob Home'),
	assign('Users','File Management'),
	assign('Bob Home','File Management'),
        assign('File Management','PM'),
	operation(r,'File'),
	operation(w,'File'),
	associate('Bob',[r,w],'Bob Home'),
	associate('Alice',[r,w],'o2')
	],dpl).

policy('Signals Access Policy','Signals Access', [
        user('Sebastian'),
        user('Ana'),
        user('OEM employee 1'),

        user_attribute('Vehicle Owners'),
        user_attribute('Vehicle OEM'),

        object('VIN-1001 Shift Signals'),
        object('VIN-1001 Window Signals'),
        object('VIN-1001 Door Signals'),
        object('VIN-1001 Trip Signals'),

        object('VIN-1002 Shift Signals'),
        object('VIN-1002 Window Signals'),
        object('VIN-1002 Door Signals'),
        object('VIN-1002 Trip Signals'),

        object('VIN-2001 Shift Signals'),
        object('VIN-2001 Window Signals'),
        object('VIN-2001 Door Signals'),
        object('VIN-2001 Trip Signals'),

        object('VIN-3001 Shift Signals'),
        object('VIN-3001 Window Signals'),
        object('VIN-3001 Door Signals'),
        object('VIN-3001 Trip Signals'),

        object_attribute('Trip Signals'),
        object_attribute('Window Signals'),
        object_attribute('Door Signals'),
        object_attribute('Shift Signals'),

        object_attribute('Owner Accessible Signals'),
        object_attribute('OEM Accessible Signals'),

        policy_class('Signals Access'),

        connector('PM'),

        assign('Sebastian', 'Vehicle Owners'),
        assign('Ana', 'Vehicle Owners'),

        assign('OEM employee 1', 'Vehicle OEM'),

        assign('Vehicle OEM', 'Vehicle Owners'),

        assign('VIN-1001 Shift Signals', 'Shift Signals'),
        assign('VIN-1001 Window Signals', 'Window Signals'),
        assign('VIN-1001 Door Signals', 'Door Signals'),
        assign('VIN-1001 Trip Signals', 'Trip Signals'),

        assign('VIN-1002 Shift Signals', 'Shift Signals'),
        assign('VIN-1002 Window Signals', 'Window Signals'),
        assign('VIN-1002 Door Signals', 'Door Signals'),
        assign('VIN-1002 Trip Signals', 'Trip Signals'),

        assign('VIN-2001 Shift Signals', 'Shift Signals'),
        assign('VIN-2001 Window Signals', 'Window Signals'),
        assign('VIN-2001 Door Signals', 'Door Signals'),
        assign('VIN-2001 Trip Signals', 'Trip Signals'),

        assign('VIN-3001 Shift Signals', 'Shift Signals'),
        assign('VIN-3001 Window Signals', 'Window Signals'),
        assign('VIN-3001 Door Signals', 'Door Signals'),
        assign('VIN-3001 Trip Signals', 'Trip Signals'),

        assign('Trip Signals', 'OEM Accessible Signals'),

        assign('Window Signals', 'Owner Accessible Signals'),
        assign('Door Signals', 'Owner Accessible Signals'),
        assign('Shift Signals', 'Owner Accessible Signals'),

        assign('Owner Accessible Signals', 'OEM Accessible Signals'),

        assign('Vehicle Owners', 'Signals Access'),
        assign('OEM Accessible Signals', 'Signals Access'),
        assign('Owner Accessible Signals', 'Signals Access'),

        assign('Signals Access','PM'),

        associate('Vehicle Owners', [r], 'Owner Accessible Signals'),
        associate('Vehicle OEM', [r,w], 'OEM Accessible Signals')
        ],dpl).

policy('Vehicle Ownership Policy','Vehicle Ownership', [
	user('Sebastian'),
        user('Ana'),

	user_attribute('Scholze Family'),
        user_attribute('Correia Family'),

        user_attribute('Owners'),

	object('VIN-1001 Shift Signals'),
	object('VIN-1001 Window Signals'),
	object('VIN-1001 Door Signals'),
	object('VIN-1001 Trip Signals'),

	object('VIN-1002 Shift Signals'),
	object('VIN-1002 Window Signals'),
	object('VIN-1002 Door Signals'),
	object('VIN-1002 Trip Signals'),

	object('VIN-2001 Shift Signals'),
	object('VIN-2001 Window Signals'),
	object('VIN-2001 Door Signals'),
	object('VIN-2001 Trip Signals'),

	object('VIN-3001 Shift Signals'),
	object('VIN-3001 Window Signals'),
	object('VIN-3001 Door Signals'),
	object('VIN-3001 Trip Signals'),

	object_attribute('Vehicle VIN-1001'),
	object_attribute('Vehicle VIN-1002'),
	object_attribute('Vehicle VIN-2001'),
	object_attribute('Vehicle VIN-3001'),

	object_attribute('Scholze Family Vehicles'),
	object_attribute('Correia Family Vehicles'),

	object_attribute('Vehicles'),

	policy_class('Vehicle Ownership'),

	connector('PM'),

	assign('Sebastian', 'Scholze Family'),
	assign('Ana', 'Correia Family'),

	assign('Scholze Family', 'Owners'),
	assign('Correia Family', 'Owners'),

	assign('VIN-1001 Shift Signals', 'Vehicle VIN-1001'),
	assign('VIN-1001 Window Signals', 'Vehicle VIN-1001'),
	assign('VIN-1001 Door Signals', 'Vehicle VIN-1001'),
	assign('VIN-1001 Trip Signals', 'Vehicle VIN-1001'),

	assign('VIN-1002 Shift Signals', 'Vehicle VIN-1002'),
	assign('VIN-1002 Window Signals', 'Vehicle VIN-1002'),
	assign('VIN-1002 Door Signals', 'Vehicle VIN-1002'),
	assign('VIN-1002 Trip Signals', 'Vehicle VIN-1002'),

	assign('VIN-2001 Shift Signals', 'Vehicle VIN-2001'),
	assign('VIN-2001 Window Signals', 'Vehicle VIN-2001'),
	assign('VIN-2001 Door Signals', 'Vehicle VIN-2001'),
	assign('VIN-2001 Trip Signals', 'Vehicle VIN-2001'),

	assign('VIN-3001 Shift Signals', 'Vehicle VIN-3001'),
	assign('VIN-3001 Window Signals', 'Vehicle VIN-3001'),
	assign('VIN-3001 Door Signals', 'Vehicle VIN-3001'),
	assign('VIN-3001 Trip Signals', 'Vehicle VIN-3001'),

	assign('Vehicle VIN-1001', 'Scholze Family Vehicles'),
	assign('Vehicle VIN-1002', 'Correia Family Vehicles'),
	assign('Vehicle VIN-2001', 'Scholze Family Vehicles'),
	assign('Vehicle VIN-3001', 'Correia Family Vehicles'),

	assign('Scholze Family Vehicles', 'Vehicles'),
	assign('Correia Family Vehicles', 'Vehicles'),

	assign('Owners', 'Vehicle Ownership'),
	assign('Vehicles', 'Vehicle Ownership'),

	assign('Vehicle Ownership', 'PM'),

	associate('Scholze Family',[o,r],'Scholze Family Vehicles'),
	associate('Correia Family',[o,r],'Correia Family Vehicles')
	],dpl).

policy('CondPolicy1','Conditional Access', [
        conditions([is_weekday, current_day_is_one_of(list)]),

        user('u1'),
        user('u2'),

        user_attribute('GroupA'),
        user_attribute('GroupB'),
        user_attribute('Division'),

        object('o1'),
        object('o2'),
        object('o3'),

        object_attribute('ProjectA'),
        object_attribute('ProjectB'),
        object_attribute('GrB-Secret'),
        object_attribute('Projects'),

        policy_class('Conditional Access'),
        connector('PM'),

        assign('u1','GroupA'),
        assign('u2','GroupB'),
        assign('GroupA','Division'),
        assign('GroupB','Division'),
        assign('o1','ProjectA'),
        assign('o2','ProjectB'),
        assign('o3','GrB-Secret'),
        assign('ProjectA','Projects'),
        assign('ProjectB','Projects'),
        assign('Division','Conditional Access'),
        assign('Projects','Conditional Access'),
        assign('GrB-Secret','Conditional Access'),
        assign('Conditional Access','PM'),

        associate('GroupA',[w],'ProjectA'),
        associate('GroupB',[w],'ProjectB'),
        %cond( current_day_is_one_of(['Monday','Tuesday',/*'Wednesday',*/'Thursday','Friday']),
        %cond( is_member_of(local_day,['Monday','Tuesday','Wednesday','Thursday','Friday']),
        cond( is_weekday,
              associate('GroupB',[r,w],'GrB-Secret') ),
        associate('Division',[r],'Projects')
        ],dpl).

%
% test policies above should not be modified - built-in test depends on them
%

% Example and experimental policies below

policy(cpolicy,access,[
	user(u1),
	user_attribute(ua1),
	object(o1),
	object_attribute(oa1),
	policy_class(access),
	connector('PM'),
	assign(u1,ua1),
	assign(o1,oa1),
	assign(ua1,access),
	assign(oa1,access),
	assign(access,'PM'),
	cond( is_True(_), associate(ua1,[r,w],oa1) )
        ],dpl).

policy(cpolicy2,access,[
	user(u1),
	user_attribute(ua1),
	object(o1),
	object_attribute(oa1),
	policy_class(access),
	connector('PM'),
	assign(u1,ua1),
	assign(o1,oa1),
	assign(ua1,access),
	assign(oa1,access),
	assign(access,'PM'),
	cond( is_True(weekday), associate(ua1,[r,w],oa1) )
        ],dpl).

policy(mpolicy1,market_policy,[
	connector('PM'),
	policy_class(market_policy),
	assign(market_policy,'PM'),

	user_attribute(sp_5f1aa9f638189e22005d0f39),
	assign(sp_5f1aa9f638189e22005d0f39, market_policy),
	object_attribute(owner_1),
	assign(owner_1, market_policy),
	object('device_95b40cf9-a9fc-4bd8-b695-99773b6f25e4'),
	assign('device_95b40cf9-a9fc-4bd8-b695-99773b6f25e4', owner_1),
    % aeon channel for offer
	user(achnl_5f5a39f20463e50012bca2c3),
	assign(achnl_5f5a39f20463e50012bca2c3, sp_5f1aa9f638189e22005d0f39),
    % a contract based on the offer
	user_attribute(ua_cntr_5f3fa521b1782447069c2649),
	object_attribute(oa_cntr_5f3fa521b1782447069c2649),
	assign(achnl_5f5a39f20463e50012bca2c3, ua_cntr_5f3fa521b1782447069c2649),
	assign(ua_cntr_5f3fa521b1782447069c2649, sp_5f1aa9f638189e22005d0f39),
	assign('device_95b40cf9-a9fc-4bd8-b695-99773b6f25e4', oa_cntr_5f3fa521b1782447069c2649),
	assign(oa_cntr_5f3fa521b1782447069c2649, owner_1),
	cond(dr_offer_5f5a39f2b559dcf200f424d0(devid,mchan,tstart,tstop,tsubmit,loMin,loMax,laMin,laMax),
		associate(ua_cntr_5f3fa521b1782447069c2649, [r], oa_cntr_5f3fa521b1782447069c2649) )

        ],dpl).

% Cloud of Clouds example

policy(lc1,'Local Cloud 1',[
       user(c11), user(c12),
       object(p11), object(p12), object(p13),
       user_attribute(cgroup1), user_attribute(cgroup2),
       object_attribute(pgroup1), object_attribute(pgroup2),
       assign(c11,cgroup1),
       assign(c12,cgroup2),
       assign(p11,pgroup1),
       assign(p12,pgroup1),
       assign(p13,pgroup2),

       associate(cgroup1,[invoke],pgroup1),
       associate(cgroup2,[invoke],pgroup2),

       external_attribute(ea11), % outgoing references

       associate(cgroup1, [invoke], ea11)
       ],dpl).

policy(lc2,'Local Cloud 2',[
       user(c21), user(c22), user(c23),
       object(p21), object(p22),
       user_attribute(group2),
       object_attribute(resources2),

       assign(p21,resources2),
       assign(c22,group2), assign(c23,group2),
       % ...

       external_attribute(ea21), % outgoing references
       external_attribute(ea22), % incoming references

       associate(group2, [invoke], ea21),
       associate(ea22, [invoke], resources2)
       ],dpl).

policy(lc3,'Local Cloud 3',[
       user(c31), user(c32),
       object(p31), object(p32), object(p33),
       user_attribute(project3),
       object_attribute(resources3),
       % ...
       assign(p32,resources3),

       external_attribute(ea31), % outgoing references
       external_attribute(ea32), % incoming references

       associate(project3, [invoke], ea31),
       associate(ea32, [invoke], resources3)
       ],dpl).

gg_policy('Gateway Flows', [
        gateway(g1),
        gateway(g2),
        gateway(g3),
        gg_associate(g1, [invoke], g3),
        gg_associate(g1, [invoke], g2),
        gg_associate(g2, [invoke], g3),
        gg_associate(g3, [invoke], g2)
       ]).

cc_policy('Cloud-of-Clouds', [
        % cc_associate(ExternalAttribute1, OpSet, ExternalAttribute2)
        %local_cloud_policy(lc1,lc1_policy), % name policies lc1, etc for simplicity
        %local_cloud_policy(lc2,lc2_policy),
        %local_cloud_policy(lc3,lc3_policy),
        local_cloud_gateway(lc1,g1),
        local_cloud_gateway(lc2,g2),
        local_cloud_gateway(lc3,g3),
        external_attribute(ea11),
        external_attribute(ea21),
        external_attribute(ea22),
        external_attribute(ea31),
        external_attribute(ea32),
        cc_assign(ea11,g1),
        cc_assign(ea21,g2), cc_assign(ea22,g2),
        cc_assign(ea31,g3), cc_assign(ea32,g3),
        cc_associate( ea11, [invoke], ea32 ),
        cc_associate( ea12, [invoke], ea22 ),
        cc_associate( ea21, [invoke], ea32 ),
        cc_associate( ea31, [invoke], ea22 )
       ]).

% end of Cloud of Clouds example

% END OF TEST POLICIES
% DO NOT DELETE OR MODIFY THE PRECEDING POLICIES
%
% Other example or test policies may be added below
%

policy('Policy (aa)','Project Access 1', [
	user('u1'),
	user('u2'),
        user('u3'),
        user('u5'),
	user_attribute('Group1'),
        user_attribute('Group2'),
        user_attribute('Division'),
	object('o1'),
        object('o2'),
        object('o3'),
        object_attribute('Project1'),
        object_attribute('Project2'),
        object_attribute('Gr2-Secret'),
        object_attribute('Projects'),
	policy_class('Project Access 1'),
	connector('PM'),
	assign('u1','Group1'),
	assign('u2','Group2'),
	assign('Group1','Division'),
	assign('Group2','Division'),
	assign('o1','Project1'),
	assign('o2','Project2'),
	assign('o3','Gr2-Secret'),
	assign('Project1','Projects'),
	assign('Project2','Projects'),
	assign('Division','Project Access 1'),
	assign('Projects','Project Access 1'),
	assign('Gr2-Secret','Project Access 1'),
        assign('Project Access 1','PM'),
	operation(r,'File'),
	operation(w,'File'),
	associate('Group1',[w],'Project1'),
	associate('Group2',[w],'Project2'),
	associate('Group2',[r,w],'Gr2-Secret'),
	associate('Division',[r],'Projects')
	],dpl).

policy('Policy (bb)','File Management 1', [
	user('u1'),
	user('u2'),
        user('u4'),
        user('u5'),
	user_attribute('Alice'),
	user_attribute('Bob'),
	user_attribute('Users'),
	object('o2'),
	object('o3'),
	object('o4'),
	object_attribute('Proposals'),
	object_attribute('Reports'),
	object_attribute('Bob Home'),
	policy_class('File Management 1'),
	connector('PM'),
	assign('u1','Alice'),
	assign('u2','Bob'),
	assign('Alice','Users'),
	assign('Bob','Users'),
	assign('o2','Proposals'),
	assign('o3','Reports'),
	assign('o4','Reports'),
	assign('Proposals','Bob Home'),
	assign('Reports','Bob Home'),
	assign('Users','File Management 1'),
	assign('Bob Home','File Management 1'),
        assign('File Management 1','PM'),
	operation(r,'File'),
	operation(w,'File'),
	associate('Bob',[r,w],'Bob Home'),
	associate('Alice',[r,w],'o2')
	],dpl).

% experimental privacy policy in the expanded policy declaration form
%

policy(priv2,priv_pol2,[
	policy_class(priv_pol2),
	assign(priv_pol2,'PM'),

        object_attribute(ds1), % data subject
        object_attribute(data_subjects),
        assign(ds1,data_subjects),
        assign(data_subjects,priv_pol2),

        data_type(dt1,[]),
        data_type(dt2,[]),
        object(di1,dt1),
        object(di2,dt2),
        object(di3,dt2),

        assign(di1,ds1),
        assign(di2,ds1),
        assign(di3,ds1),

        user(s1), % service
        user(s2), % service
        user_attribute(dc1), % data controller
        user_attribute(dc2), % data controller
        assign(s1,dc1),
        assign(s2,dc2),
        user_attribute(data_controllers),
        assign(dc1,data_controllers),
        assign(dc2,data_controllers),
        assign(data_controllers,priv_pol2),

        associate(dc2, [dpo1], ds1, p11),

        privacy_preference(ds1, [(p11,dpo2,di1)]),
        privacy_policy(dc1, [(p112,dpo22,dt1)]),

        % operation and purpose hierarchies are global
        operation(dpo12),
        operation(dpo11),
        operation(dpo1),
        operation(dpo22),
        operation(dpo21),
        operation(dpo2),
        operation(all_operations),

        assign(dpo1,all_operations),
        assign(dpo11,dpo1),
        assign(dpo12,dpo1),
        assign(dpo2,all_operations),
        assign(dpo21,dpo2),
        assign(dpo22,dpo2),

        purpose(p12),
        purpose(p112),
        purpose(p111),
        purpose(p11),
        purpose(p1),
        purpose(p221),
        purpose(p22),
        purpose(p21),
        purpose(p2),
        purpose(all_purposes),

        assign(p1,all_purposes),
        assign(p11,p1),
        assign(p12,p1),
        assign(p111,p11),
        assign(p112,p11),
        assign(p2,all_purposes),
        assign(p21,p2),
        assign(p22,p2),
        assign(p221,p22),
        % assign(all_purposes,priv_pol2),
        % assign(all_operations,priv_pol2),

	connector('PM')
        ],dplp).

% smashHit Core Ontology 202210
policy(smashHitCore202210, smashHitCore202210, [
  % user_attribute('Role'),  % add Roles in future version of smashHitCore
  % user_attribute('Data Controller'), assign('Data Controller','Role'),
  % user_attribute('Data Processor'), assign('Data Processor','Role'),
  % object_attribute('Data Subject'), assign('Data Subject','Role'), % check this will not be a problem
  data_type('Information Entity'),

    data_type('Personal Data Category'), assign('Personal Data Category','Information Entity'),

      data_type('External'), assign('External','Personal Data Category'),
        data_type('Identifying'), assign('Identifying','External'),
          data_type('Birth Date'), assign('Birth Date','Identifying'),
          data_type('Official ID'), assign('Official ID','Identifying'),
          data_type('UID'), assign('UID','Identifying'),
          data_type('Username'), assign('Username','Identifying'),

        data_type('Financial'), assign('Financial','Personal Data Category'),
          data_type('Ownership'), assign('Ownership','Financial'),

        data_type('Internal'), assign('Internal','Personal Data Category'),
          data_type('Authenticating'), assign('Authenticating','Internal'),
            data_type('Password'), assign('Password','Authenticating'),
          data_type('Preference'), assign('Preference','Internal'),
            data_type('Privacy Preference'), assign('Privacy Preference','Preference'),

        data_type('Tracking'), assign('Tracking','Personal Data Category'),
          data_type('Contact'), assign('Contact','Tracking'),
            data_type('Email Address'), assign('Email Address','Contact'),
            data_type('Telephone Number'), assign('Telephone Number','Contact'),

  operation('Processing'),
  
    operation('Adapt'), assign('Adapt','Processing'),
    operation('Align'), assign('Align','Processing'),
    operation('Alter'), assign('Alter','Processing'),
    operation('Attack'), assign('Attack','Processing'),
    operation('Collect'), assign('Collect','Processing'),
    operation('Combine'), assign('Combine','Processing'),
    operation('Consult'), assign('Consult','Processing'),
    operation('Destruct'), assign('Destruct','Processing'),
    operation('Disclose by Transmission'), assign('Disclose by Transmission','Processing'),
    operation('Disseminate'), assign('Disseminate','Processing'),
    operation('Erase'), assign('Erase','Processing'),
    operation('Fingerprint'), assign('Fingerprint','Processing'),
    operation('Hash'), assign('Hash','Processing'),
    operation('Organise'), assign('Organise','Processing'),
    operation('Receive'), assign('Receive','Processing'),
    operation('Record'), assign('Record','Processing'),
    operation('Register'), assign('Register','Processing'),
    operation('Reidentificate'), assign('Reidentificate','Processing'),
    operation('Restrict'), assign('Restrict','Processing'),
    operation('Retrieve'), assign('Retrieve','Processing'),
    operation('Send'), assign('Send','Processing'),
    operation('Share'), assign('Share','Processing'),
    operation('Store'), assign('Store','Processing'),
    operation('Structure'), assign('Structure','Processing'),
    operation('Trace'), assign('Trace','Processing'),
    operation('Use'), assign('Use','Processing'),
    operation('Watermark'), assign('Watermark','Processing'),

  purpose('Purpose'),

    purpose('Commercial Interest'), assign('Commercial Interest','Purpose'),
      purpose('Sell Data To Third Parties'), assign('Sell Data To Third Parties','Commercial Interest'),
      purpose('Sell Insights From Data'), assign('Sell Insights From Data','Commercial Interest'),
      purpose('Sell Products To Data Subject'), assign('Sell Products To Data Subject','Commercial Interest'),
      purpose('Sell Targetted Advertisements'), assign('Sell Targetted Advertisements','Commercial Interest'),

    purpose('Research And Development'), assign('Research And Development','Purpose'),
      purpose('Academic Research'), assign('Academic Research','Research And Development'),
      purpose('Commercial Research'), assign('Commercial Research','Research And Development'),
      purpose('Non-Commercial Research'), assign('Non-Commercial Research','Research And Development'),

    purpose('Security'), assign('Security','Purpose'),
      purpose('Access Control'), assign('Access Control','Security'),
      purpose('Fraud Prevention And Detection'), assign('Fraud Prevention And Detection','Security'),
      purpose('Identity Verification'), assign('Identity Verification','Security'),

    purpose('Service Optimization'), assign('Service Optimization','Purpose'),
      purpose('Optimisation For Consumer'), assign('Optimisation For Consumer','Service Optimization'),
        purpose('Optimise User Interface'), assign('Optimise User Interface','Optimisation For Consumer'),
      purpose('Optimisation For Controller'), assign('OptimisationForController','ServiceOptimization'),
        purpose('Improve Existing Products and Services'), assign('Improve Existing Product and Services','Optimisation For Controller'),
        purpose('Improve Internal CRM Processes'), assign('Improve Internal CRM Processes','Optimisation For Controller'),
        purpose('Increase Service Robustness'), assign('Increase Service Robustness','Optimisation For Controller'),
        purpose('Internal Resource Optimisation'), assign('Internal Resource Optimisation','Optimisation For Controller'),

    purpose('Service Personalization'), assign('Service Personalization','Purpose'),
      purpose('Create Personalized Recommendations'), assign('Create Personalized Recommendations','Service Personalization'),
        purpose('Create Event Recommendations'), assign('Create Event Recommendations','Create Personalized Recommendations'),
        purpose('Create Product Recommendations'), assign('Create Product Recommendations','Create Personalized Recommendations'),
      purpose('Personalised Benefits'), assign('Personalised Benefits','Service Personalization'),
      purpose('User Interface Personalisation'), assign('User Interface Personalisation','Service Personalization'),

    purpose('Service Provision'), assign('Service Provision','Purpose'),
      purpose('Customer Care'), assign('Customer Care','Service Provision'),
      purpose('Delivery of Goods'), assign('Deliver of Goods','Service Provision')

  ], dplp).

% previous ontology definitions
policy(core_ontology, core_ontology, [
        data_type('PersonalDataCategory'),
          data_type('External'), assign('External','PersonalDataCategory'),
            data_type('Identifying'), assign('Identifying','External'),
              data_type('Address'), assign('Address','Identifying'),
                data_type('PhysicalAddress'), assign('PhysicalAddress','Address'),
                  data_type('ConventionalStreetAddress'), assign('ConventionalStreetAddress','PhysicalAddress'),
                data_type('StreetAddress'), assign('StreetAddress','Address'),
              data_type('Name'), assign('Name','Identifying'),
              data_type('OfficialID'), assign('OfficialID','Identifying'),
              data_type('UID'), assign('UID','Identifying'),
              data_type('Username'), assign('Username','Identifying'),
              data_type('BirthDate'), assign('BirthDate','Identifying'),
          data_type('Financial'), assign('Financial','PersonalDataCategory'),
            data_type('Ownership'), assign('Ownership','Financial'),
          data_type('Internal'), assign('Internal','PersonalDataCategory'),
            data_type('Authenticating'), assign('Authenticating','Internal'),
              data_type('Password'), assign('Password','Authenticating'),
            data_type('Preference'), assign('Preference','Internal'),
              data_type('Privacy Preference'), assign('Privacy Preference','Preference'),
          data_type('Tracking'), assign('Tracking','PersonalDataCategory'),
            data_type('Contact'), assign('Contact','Tracking'),
              data_type('Email Address'), assign('Email Address','Contact'),
              data_type('Telephone Number'), assign('Telephone Number','Contact'),
            data_type('Location'), assign('Location','Tracking'),

        operation('DataProcessing'),
          operation('Adapt'), assign('Adapt','DataProcessing'),
          operation('Align'), assign('Align','DataProcessing'),
          operation('Alter'), assign('Alter','DataProcessing'),
          operation('Collect'), assign('Collect','DataProcessing'),
          operation('Combine'), assign('Combine','DataProcessing'),
          operation('Consult'), assign('Consult','DataProcessing'),
          operation('Destruct'), assign('Destruct','DataProcessing'),
          operation('DiscloseByTransmission'), assign('DiscloseByTransmission','DataProcessing'),
          operation('Disseminate'), assign('Disseminate','DataProcessing'),
          operation('Erase'), assign('Erase','DataProcessing'),
          operation('Organise'), assign('Organise','DataProcessing'),
          operation('Record'), assign('Record','DataProcessing'),
          operation('Restrict'), assign('Restrict','DataProcessing'),
          operation('Retrieve'), assign('Retrieve','DataProcessing'),
          operation('Share'), assign('Share','DataProcessing'),
          operation('Store'), assign('Store','DataProcessing'),
          operation('Structure'), assign('Structure','DataProcessing'),
          operation('Use'), assign('Use','DataProcessing'),

        purpose('Purpose'),
        purpose('CommercialInterest'), assign('CommercialInterest','Purpose'),
          purpose('SellDataToThirdParties'), assign('SellDataToThirdParties','CommercialInterest'),
          purpose('SellInsightsFromData'), assign('SellInsightsFromData','CommercialInterest'),
          purpose('SellProductsToDataSubject'), assign('SellProductsToDataSubject','CommercialInterest'),
          purpose('SellTargettedAdvertisements'), assign('SellTargettedAdvertisements','CommercialInterest'),
        purpose('ResearchAndDevelopment'), assign('ResearchAndDevelopment','Purpose'),
          purpose('AcademicResearch'), assign('AcademicResearch','ResearchAndDevelopment'),
          purpose('CommercialResearch'), assign('CommercialResearch','ResearchAndDevelopment'),
          purpose('NonCommercialResearch'), assign('NonCommercialResearch','ResearchAndDevelopment'),
        purpose('Security'), assign('Security','Purpose'),
          purpose('AccessControl'), assign('AccessControl','Security'),
          purpose('FraudPreventionAndDetection'), assign('FraudPreventionAndDetection','Security'),
          purpose('IdentityVerification'), assign('IdentityVerification','Security'),
        purpose('ServiceOptimization'), assign('ServiceOptimization','Purpose'),
          purpose('OptimisationForConsumer'), assign('OptimisationForConsumer','ServiceOptimization'),
            purpose('OptimiseUserInterface'), assign('OptimiseUserInterface','OptimisationForConsumer'),
          purpose('OptimisationForController'), assign('OptimisationForController','ServiceOptimization'),
            purpose('ImproveExistingProductsAndServices'), assign('ImproveExistingProductsAndServices','OptimisationForController'),
            purpose('ImproveInternalCRMProcesses'), assign('ImproveInternalCRMProcesses','OptimisationForController'),
            purpose('IncreaseServiceRobustness'), assign('IncreaseServiceRobustness','OptimisationForController'),
            purpose('InternalResourceOptimisation'), assign('InternalResourceOptimisation','OptimisationForController'),
        purpose('ServicePersonalization'), assign('ServicePersonalization','Purpose'),
          purpose('CreatePersonalizedRecommendations'), assign('CreatePersonalizedRecommendations','ServicePersonalization'),
            purpose('CreateEventRecommendations'), assign('CreateEventRecommendations','CreatePersonalizedRecommendations'),
            purpose('CreateProductRecommendations'), assign('CreateProductRecommendations','CreatePersonalizedRecommendations'),
          purpose('PersonalisedBenefits'), assign('PersonalisedBenefits','ServicePersonalization'),
          purpose('UserInterfacePersonalisation'), assign('UserInterfacePersonalisation','ServicePersonalization'),
        purpose('ServiceProvision'), assign('ServiceProvision','Purpose'),
          purpose('CustomerCare'), assign('CustomerCare','ServiceProvision'),
          purpose('DeliveryOfGoods'), assign('DeliveryOfGoods','ServiceProvision')

        ], dplp).

policy(data_core_ontology, core_ontology, [
        data_type('PersonalDataCategory'),
          data_type('External'), assign('External','PersonalDataCategory'),
            data_type('Identifying'), assign('Identifying','External'),
              data_type('Address'), assign('Address','Identifying'),
                data_type('PhysicalAddress'), assign('PhysicalAddress','Address'),
                  data_type('ConventionalStreetAddress'), assign('ConventionalStreetAddress','PhysicalAddress'),
                data_type('StreetAddress'), assign('StreetAddress','Address'),
              data_type('Name'), assign('Name','Identifying'),
              data_type('OfficialID'), assign('OfficialID','Identifying'),
              data_type('UID'), assign('UID','Identifying'),
              data_type('Username'), assign('Username','Identifying'),
              data_type('BirthDate'), assign('BirthDate','Identifying'),
          data_type('Financial'), assign('Financial','PersonalDataCategory'),
            data_type('Ownership'), assign('Ownership','Financial'),
          data_type('Internal'), assign('Internal','PersonalDataCategory'),
            data_type('Authenticating'), assign('Authenticating','Internal'),
              data_type('Password'), assign('Password','Authenticating'),
            data_type('Preference'), assign('Preference','Internal'),
              data_type('PrivacyPreference'), assign('PrivacyPreference','Preference'),
          data_type('Tracking'), assign('Tracking','PersonalDataCategory'),
            data_type('Contact'), assign('Contact','Tracking'),
              data_type('TelephoneNumber'), assign('TelephoneNumber','Contact'),
              data_type('EmailAddress'), assign('EmailAddress','Contact'),
            data_type('Location'), assign('Location','Tracking')
        ], dplp).

policy(process_core_ontology, core_ontology, [
        operation('DataProcessing'),
          operation('Adapt'), assign('Adapt','DataProcessing'),
          operation('Align'), assign('Align','DataProcessing'),
          operation('Alter'), assign('Alter','DataProcessing'),
          operation('Collect'), assign('Collect','DataProcessing'),
          operation('Combine'), assign('Combine','DataProcessing'),
          operation('Consult'), assign('Consult','DataProcessing'),
          operation('Destruct'), assign('Destruct','DataProcessing'),
          operation('DiscloseByTransmission'), assign('DiscloseByTransmission','DataProcessing'),
          operation('Disseminate'), assign('Disseminate','DataProcessing'),
          operation('Erase'), assign('Erase','DataProcessing'),
          operation('Organise'), assign('Organise','DataProcessing'),
          operation('Record'), assign('Record','DataProcessing'),
          operation('Restrict'), assign('Restrict','DataProcessing'),
          operation('Retrieve'), assign('Retrieve','DataProcessing'),
          operation('Share'), assign('Share','DataProcessing'),
          operation('Store'), assign('Store','DataProcessing'),
          operation('Structure'), assign('Structure','DataProcessing'),
          operation('Use'), assign('Use','DataProcessing')
        ], dplp).

policy(purpose_core_ontology, core_ontology, [
      purpose('Purpose'),
        purpose('CommercialInterest'), assign('CommercialInterest','Purpose'),
          purpose('SellDataToThirdParties'), assign('SellDataToThirdParties','CommercialInterest'),
          purpose('SellInsightsFromData'), assign('SellInsightsFromData','CommercialInterest'),
          purpose('SellProductsToDataSubject'), assign('SellProductsToDataSubject','CommercialInterest'),
          purpose('SellTargettedAdvertisements'), assign('SellTargettedAdvertisements','CommercialInterest'),
        purpose('ResearchAndDevelopment'), assign('ResearchAndDevelopment','Purpose'),
          purpose('AcademicResearch'), assign('AcademicResearch','ResearchAndDevelopment'),
          purpose('CommercialResearch'), assign('CommercialResearch','ResearchAndDevelopment'),
          purpose('NonCommercialResearch'), assign('NonCommercialResearch','ResearchAndDevelopment'),
        purpose('Security'), assign('Security','Purpose'),
          purpose('AccessControl'), assign('AccessControl','Security'),
          purpose('FraudPreventionAndDetection'), assign('FraudPreventionAndDetection','Security'),
          purpose('IdentityVerification'), assign('IdentityVerification','Security'),
        purpose('ServiceOptimization'), assign('ServiceOptimization','Purpose'),
          purpose('OptimisationForConsumer'), assign('OptimisationForConsumer','ServiceOptimization'),
            purpose('OptimiseUserInterface'), assign('OptimiseUserInterface','OptimisationForConsumer'),
          purpose('OptimisationForController'), assign('OptimisationForController','ServiceOptimization'),
            purpose('ImproveExistingProductsAndServices'), assign('ImproveExistingProductsAndServices','OptimisationForController'),
            purpose('ImproveInternalCRMProcesses'), assign('ImproveInternalCRMProcesses','OptimisationForController'),
            purpose('IncreaseServiceRobustness'), assign('IncreaseServiceRobustness','OptimisationForController'),
            purpose('InternalResourceOptimisation'), assign('InternalResourceOptimisation','OptimisationForController'),
        purpose('ServicePersonalization'), assign('ServicePersonalization','Purpose'),
          purpose('CreatePersonalizedRecommendations'), assign('CreatePersonalizedRecommendations','ServicePersonalization'),
            purpose('CreateEventRecommendations'), assign('CreateEventRecommendations','CreatePersonalizedRecommendations'),
            purpose('CreateProductRecommendations'), assign('CreateProductRecommendations','CreatePersonalizedRecommendations'),
          purpose('PersonalisedBenefits'), assign('PersonalisedBenefits','ServicePersonalization'),
          purpose('UserInterfacePersonalisation'), assign('UserInterfacePersonalisation','ServicePersonalization'),
        purpose('ServiceProvision'), assign('ServiceProvision','Purpose'),
          purpose('CustomerCare'), assign('CustomerCare','ServiceProvision'),
          purpose('DeliveryOfGoods'), assign('DeliveryOfGoods','ServiceProvision')
        ], dplp).
