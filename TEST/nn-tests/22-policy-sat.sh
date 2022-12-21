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

echo 'tun the test cases using the policy dplp_sat_test built from meta-elements'
curl -s -G "http://127.0.0.1:8001/paapi/setpol" --data-urlencode "policy=dplp_sat_test" --data-urlencode "token=admin_token"
curl -s -G "http://127.0.0.1:8001/paapi/reset" --data-urlencode "token=admin_token" --data-urlencode "domain=policy" --data-urlencode "name=dplp_sat_test"

curl -s -G "http://127.0.0.1:8001/pqapi/policy_sat" --data-urlencode "definitions=dplp_sat_test" \
	--data-urlencode "privacy_policy=dp3" --data-urlencode "privacy_preference=ds3"\

curl -s -G "http://127.0.0.1:8001/pqapi/policy_sat" --data-urlencode "definitions=dplp_sat_test" \
	--data-urlencode "privacy_policy=dp4" --data-urlencode "privacy_preference=ds4"\

curl -s -G "http://127.0.0.1:8001/pqapi/policy_sat" --data-urlencode "definitions=dplp_sat_test" \
	--data-urlencode "privacy_policy=dp5" --data-urlencode "privacy_preference=ds5"\

curl -s -G "http://127.0.0.1:8001/pqapi/policy_sat" --data-urlencode "definitions=dplp_sat_test" \
	--data-urlencode "privacy_policy=dp6" --data-urlencode "privacy_preference=ds6"\

curl -s -G "http://127.0.0.1:8001/pqapi/policy_sat" --data-urlencode "definitions=dplp_sat_test" \
	--data-urlencode "privacy_policy=dp7" --data-urlencode "privacy_preference=ds7"\

curl -s -G "http://127.0.0.1:8001/pqapi/policy_sat" --data-urlencode "definitions=dplp_sat_test" \
	--data-urlencode "privacy_policy=dp8" --data-urlencode "privacy_preference=ds8"\

curl -s -G "http://127.0.0.1:8001/pqapi/policy_sat" --data-urlencode "definitions=dplp_sat_test" \
	--data-urlencode "privacy_policy=dp9" --data-urlencode "privacy_preference=ds9"\

curl -s -G "http://127.0.0.1:8001/pqapi/policy_sat" --data-urlencode "definitions=dplp_sat_test" \
	--data-urlencode "privacy_policy=dp10" --data-urlencode "privacy_preference=ds10"\

curl -s -G "http://127.0.0.1:8001/pqapi/policy_sat" --data-urlencode "definitions=dplp_sat_test" \
	--data-urlencode "privacy_policy=dp11" --data-urlencode "privacy_preference=ds11"\

curl -s -G "http://127.0.0.1:8001/pqapi/policy_sat" --data-urlencode "definitions=dplp_sat_test" \
	--data-urlencode "privacy_policy=dp12" --data-urlencode "privacy_preference=ds12"\
