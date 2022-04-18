#!/bin/sh
echo 'demo policy_sat examples of script 17 using policy built from dplp_min with meta-elements'

echo 'reset policy and build scenario before tests are run - dplp_min:pc + testdefs, policy/preference provided by each policy_sat call'
curl -s -G "http://127.0.0.1:8001/paapi/reset" --data-urlencode "token=admin_token" --data-urlencode "domain=policy" --data-urlencode "name=dplp_min"
curl -s -G "http://127.0.0.1:8001/paapi/setpol" --data-urlencode "policy=dplp_min" --data-urlencode "token=admin_token"
curl -s -G "http://127.0.0.1:8001/dplp/add_dplp_policy_base"  --data-urlencode "policy=dplp_min" --data-urlencode "policy_class=pc" --data-urlencode "definitions=testdefs" --data-urlencode "token=admin_token"

echo 'fetch the policy'
curl -s -G "http://127.0.0.1:8001/paapi/readpol" --data-urlencode "policy=dplp_min" --data-urlencode "token=admin_token"

echo 'policy_sat tests, expect: 1 sat 2 unsat 3 sat 4 unsat 5 sat 6 unsat 7 sat 8 sat 9 unsat 10 unsat'
echo case 1
curl -s -G "http://127.0.0.1:8001/pqapi/policy_sat" --data-urlencode "definitions=testdefs" \
	--data-urlencode "privacy_policy=privacy_policy(dc1, [(p112,dpo22,dt1)])" \
	--data-urlencode "privacy_preference=privacy_preference(ds1, [(p11,dpo2,di1)])"

echo case 2
curl -s -G "http://127.0.0.1:8001/pqapi/policy_sat" --data-urlencode "definitions=testdefs" \
	--data-urlencode "privacy_policy=privacy_policy(dc1, [(p1,dpo22,dt1)])" \
	--data-urlencode "privacy_preference=privacy_preference(ds1, [(p11,dpo2,di1)])"

echo case 3
curl -s -G "http://127.0.0.1:8001/pqapi/policy_sat" --data-urlencode "definitions=testdefs" \
	--data-urlencode "privacy_policy=privacy_policy(dc1, [(p112,dpo2,dt1)])" \
	--data-urlencode "privacy_preference=privacy_preference(ds1, [(p11,dpo2,di1)])"

echo case 4
curl -s -G "http://127.0.0.1:8001/pqapi/policy_sat" --data-urlencode "definitions=testdefs" \
	--data-urlencode "privacy_policy=privacy_policy(dc1, [(p112,dpo1,dt1)])" \
	--data-urlencode "privacy_preference=privacy_preference(ds1, [(p11,dpo2,di1)])"

echo case 5
curl -s -G "http://127.0.0.1:8001/pqapi/policy_sat" --data-urlencode "definitions=testdefs" \
	--data-urlencode "privacy_policy=privacy_policy(dc1, [(p11,dpo22,dt1),(p112,dpo21,dt1)])" \
	--data-urlencode "privacy_preference=privacy_preference(ds1, [(p1,dpo2,di1)])"

echo case 6
curl -s -G "http://127.0.0.1:8001/pqapi/policy_sat" --data-urlencode "definitions=testdefs" \
	--data-urlencode "privacy_policy=privacy_policy(dc1, [(p1,dpo22,dt1),(p112,dpo21,dt1)])" \
	--data-urlencode "privacy_preference=privacy_preference(ds1, [(p11,dpo2,di1)])"

echo case 7
curl -s -G "http://127.0.0.1:8001/pqapi/policy_sat" --data-urlencode "definitions=testdefs" \
	--data-urlencode "privacy_policy=privacy_policy(dc1,  [(p1,dpo22,dt1),(p112,dpo21,dt1)])" \
	--data-urlencode "privacy_preference=privacy_preference(ds1,  [(p1,dpo2,di1),(p1,dpo2,di1)])"

echo case 8
curl -s -G "http://127.0.0.1:8001/pqapi/policy_sat" --data-urlencode "definitions=testdefs" \
	--data-urlencode "privacy_policy=privacy_policy(dc1, [(p112,dpo1,dt1),(p112,dpo21,dt1)])"\
	--data-urlencode "privacy_preference=privacy_preference(ds1, [(p11,dpo2,di1),(p112,dpo1,di1)])"

echo case 9
curl -s -G "http://127.0.0.1:8001/pqapi/policy_sat" --data-urlencode "definitions=testdefs" \
	--data-urlencode "privacy_policy=privacy_policy(dc1, [(p112,dpo1,dt1),(p112,dpo21,dt1),(p111,dpo12,dt1)])"\
	--data-urlencode "privacy_preference=privacy_preference(ds1, [(p11,dpo2,di1),(p112,dpo1,di1)])"

echo case 10
curl -s -G "http://127.0.0.1:8001/pqapi/policy_sat" --data-urlencode "definitions=testdefs" \
	--data-urlencode "privacy_policy=privacy_policy(dc1, [(p112,dpo1,dt1),(p112,dpo21,dt1),(p111,dpo12,dt1)])"\
	--data-urlencode "privacy_preference=privacy_preference(ds1, [(p11,dpo22,di1),(p112,dpo1,di1)])"

echo 'reset policy and re-build scenario using meta-elements - dplp_min:pc + testdefs1, add policy/preference to each DC, DP and DS'
curl -s -G "http://127.0.0.1:8001/paapi/reset" --data-urlencode "token=admin_token" --data-urlencode "domain=policy" --data-urlencode "name=dplp_min"
curl -s -G "http://127.0.0.1:8001/dplp/add_dplp_policy_base"  --data-urlencode "policy=dplp_min" --data-urlencode "policy_class=pc" --data-urlencode "definitions=testdefs1" --data-urlencode "token=admin_token"
# add a data processor dp2 and add data subject ds2 to cover cases 8 and 9 above
curl -s -G "http://127.0.0.1:8001/dplp/add_data_controller" --data-urlencode "policy=dplp_min" --data-urlencode "data_controller=dc1" --Data-urlencode "privacy_policy=[]" --data-urlencode "token=admin_token"
curl -s -G "http://127.0.0.1:8001/dplp/add_data_processor" --data-urlencode "policy=dplp_min" --data-urlencode "data_processor=dp1" --data-urlencode "privacy_policy=[(p112,dpo1,dt1),(p112,dpo21,dt1)]"  --data-urlencode "data_controller=dc1" --data-urlencode "token=admin_token"
curl -s -G "http://127.0.0.1:8001/dplp/add_data_processor" --data-urlencode "policy=dplp_min" --data-urlencode "data_processor=dp2" --data-urlencode "privacy_policy=[(p112,dpo1,dt1),(p112,dpo21,dt1),(p111,dpo12,dt1)]"  --data-urlencode "data_controller=dc1" --data-urlencode "token=admin_token"
curl -s -G "http://127.0.0.1:8001/dplp/add_data_subject" --data-urlencode "policy=dplp_min" --data-urlencode "data_subject=ds1" --data-urlencode "privacy_preference=[(p11,dpo2,di1),(p112,dpo1,di1)]" --data-urlencode "data_items=[di1:dt1,di2:dt2]" --data-urlencode "token=admin_token"
curl -s -G "http://127.0.0.1:8001/dplp/add_data_subject" --data-urlencode "policy=dplp_min" --data-urlencode "data_subject=ds2" --data-urlencode "privacy_preference=[(p11,dpo2,di3),(p112,dpo1,di3)]" --data-urlencode "data_items=[di3:dt1,di4:dt2]" --data-urlencode "token=admin_token"
curl -s -G "http://127.0.0.1:8001/dplp/add_application" --data-urlencode "policy=dplp_min" --data-urlencode "application=app1" --data-urlencode "data_processor=dp1" --data-urlencode "operations=[dpo21,dpo22]" --data-urlencode "token=admin_token"

echo 'fetch the policy'
curl -s -G "http://127.0.0.1:8001/paapi/readpol" --data-urlencode "policy=dplp_min" --data-urlencode "token=admin_token"

echo 're-run policy_sat tests, expect: sat unsat sat unsat sat unsat sat sat unsat unsat'
curl -s -G "http://127.0.0.1:8001/pqapi/policy_sat" --data-urlencode "definitions=dplp_min" --data-urlencode "privacy_policy=dp1" --data-urlencode "privacy_preference=ds1"
curl -s -G "http://127.0.0.1:8001/pqapi/policy_sat" --data-urlencode "definitions=dplp_min" --data-urlencode "privacy_policy=dp2" --data-urlencode "privacy_preference=ds2"

exit

curl -s -G "http://127.0.0.1:8001/pqapi/policy_sat" --data-urlencode "definitions=dplp_min" --data-urlencode "privacy_policy=dp1" --data-urlencode "privacy_preference=ds1"
curl -s -G "http://127.0.0.1:8001/pqapi/policy_sat" --data-urlencode "definitions=dplp_min" --data-urlencode "privacy_policy=dp1" --data-urlencode "privacy_preference=ds1"
curl -s -G "http://127.0.0.1:8001/pqapi/policy_sat" --data-urlencode "definitions=dplp_min" --data-urlencode "privacy_policy=dp1" --data-urlencode "privacy_preference=ds1"
curl -s -G "http://127.0.0.1:8001/pqapi/policy_sat" --data-urlencode "definitions=dplp_min" --data-urlencode "privacy_policy=dp1" --data-urlencode "privacy_preference=ds1"
curl -s -G "http://127.0.0.1:8001/pqapi/policy_sat" --data-urlencode "definitions=dplp_min" --data-urlencode "privacy_policy=dp1" --data-urlencode "privacy_preference=ds1"
curl -s -G "http://127.0.0.1:8001/pqapi/policy_sat" --data-urlencode "definitions=dplp_min" --data-urlencode "privacy_policy=dp1" --data-urlencode "privacy_preference=ds1"
curl -s -G "http://127.0.0.1:8001/pqapi/policy_sat" --data-urlencode "definitions=dplp_min" --data-urlencode "privacy_policy=dp1" --data-urlencode "privacy_preference=ds2"
curl -s -G "http://127.0.0.1:8001/pqapi/policy_sat" --data-urlencode "definitions=dplp_min" --data-urlencode "privacy_policy=dp1" --data-urlencode "privacy_preference=ds1"

curl -s -G "http://127.0.0.1:8001/pqapi/policy_sat" --data-urlencode "definitions=dplp_min" \
	--data-urlencode "privacy_policy=privacy_policy(dc1, [(p112,dpo22,dt1)])" \
	--data-urlencode "privacy_preference=privacy_preference(ds1, [(p11,dpo2,di1)])"

curl -s -G "http://127.0.0.1:8001/pqapi/policy_sat" --data-urlencode "definitions=dplp_min" \
	--data-urlencode "privacy_policy=privacy_policy(dc1, [(p1,dpo22,dt1)])" \
	--data-urlencode "privacy_preference=privacy_preference(ds1, [(p11,dpo2,di1)])"

curl -s -G "http://127.0.0.1:8001/pqapi/policy_sat" --data-urlencode "definitions=dplp_min" \
	--data-urlencode "privacy_policy=privacy_policy(dc1, [(p112,dpo2,dt1)])" \
	--data-urlencode "privacy_preference=privacy_preference(ds1, [(p11,dpo2,di1)])"

curl -s -G "http://127.0.0.1:8001/pqapi/policy_sat" --data-urlencode "definitions=dplp_min" \
	--data-urlencode "privacy_policy=privacy_policy(dc1, [(p112,dpo1,dt1)])" \
	--data-urlencode "privacy_preference=privacy_preference(ds1, [(p11,dpo2,di1)])"

curl -s -G "http://127.0.0.1:8001/pqapi/policy_sat" --data-urlencode "definitions=dplp_min" \
	--data-urlencode "privacy_policy=privacy_policy(dc1, [(p11,dpo22,dt1),(p112,dpo21,dt1)])" \
	--data-urlencode "privacy_preference=privacy_preference(ds1, [(p1,dpo2,di1)])"

curl -s -G "http://127.0.0.1:8001/pqapi/policy_sat" --data-urlencode "definitions=dplp_min" \
	--data-urlencode "privacy_policy=privacy_policy(dc1, [(p1,dpo22,dt1),(p112,dpo21,dt1)])" \
	--data-urlencode "privacy_preference=privacy_preference(ds1, [(p11,dpo2,di1)])"

curl -s -G "http://127.0.0.1:8001/pqapi/policy_sat" --data-urlencode "definitions=dplp_min" \
	--data-urlencode "privacy_policy=privacy_policy(dc1,  [(p1,dpo22,dt1),(p112,dpo21,dt1)])" \
	--data-urlencode "privacy_preference=privacy_preference(ds1,  [(p1,dpo2,di1),(p1,dpo2,di1)])"

curl -s -G "http://127.0.0.1:8001/pqapi/policy_sat" --data-urlencode "definitions=dplp_min" \
	--data-urlencode "privacy_policy=privacy_policy(dc1, [(p112,dpo1,dt1),(p112,dpo21,dt1)])"\
	--data-urlencode "privacy_preference=privacy_preference(ds1, [(p11,dpo2,di1),(p112,dpo1,di1)])"

curl -s -G "http://127.0.0.1:8001/pqapi/policy_sat" --data-urlencode "definitions=dplp_min" \
	--data-urlencode "privacy_policy=privacy_policy(dc1, [(p112,dpo1,dt1),(p112,dpo21,dt1),(p111,dpo12,dt1)])"\
	--data-urlencode "privacy_preference=privacy_preference(ds1, [(p11,dpo2,di1),(p112,dpo1,di1)])"

curl -s -G "http://127.0.0.1:8001/pqapi/policy_sat" --data-urlencode "definitions=dplp_min" \
	--data-urlencode "privacy_policy=privacy_policy(dc1, [(p112,dpo1,dt1),(p112,dpo21,dt1),(p111,dpo12,dt1)])"\
	--data-urlencode "privacy_preference=privacy_preference(ds1, [(p11,dpo22,di1),(p112,dpo1,di1)])"

echo end of policy_sat tests
