echo 'set policy to priv1'
curl -s -G "http://127.0.0.1:8001/paapi/setpol" --data-urlencode "policy=priv1" --data-urlencode "token=admin_token"
echo 'run test cases for priv1, expect grant grant deny deny grant deny'
curl -s 'http://127.0.0.1:8001/pqapi/access?user=s1&ar=r&object=pii1&purpose=p112'
curl -s 'http://127.0.0.1:8001/pqapi/access?user=s1&ar=r&object=pii1&purpose=p111'
curl -s 'http://127.0.0.1:8001/pqapi/access?user=s1&ar=r&object=pii1&purpose=p12'
curl -s 'http://127.0.0.1:8001/pqapi/access?user=s1&ar=w&object=pii1&purpose=p111'
curl -s 'http://127.0.0.1:8001/pqapi/access?user=s1&ar=r&object=pii1&purpose=p11'
curl -s 'http://127.0.0.1:8001/pqapi/access?user=s1&ar=r&object=pii1&purpose=p221'

echo 'set policy to Policy (ap)'
curl -s -G "http://127.0.0.1:8001/paapi/setpol" --data-urlencode "policy=Policy (ap)" --data-urlencode "token=admin_token"
echo 'run test cases for Policy (ap), expect grant deny deny'
curl -s 'http://127.0.0.1:8001/pqapi/access?user=u2&ar=w&object=o3&purpose=p112'
curl -s 'http://127.0.0.1:8001/pqapi/access?user=u2&ar=w&object=o3&purpose=p21'
curl -s 'http://127.0.0.1:8001/pqapi/access?user=u2&ar=w&object=o3&purpose=p1'

echo 'policy_sat tests, expect: sat unsat sat unsat sat unsat sat sat unsat'
curl -s -G "http://127.0.0.1:8001/pqapi/policy_sat" --data-urlencode "env=testdefs" \
	--data-urlencode "privpol=privacy_policy(dc1, [(p112,dpo22,dt1)])" \
	--data-urlencode "privpref=privacy_preference(ds1, [(p11,dpo2,di1)])"

curl -s -G "http://127.0.0.1:8001/pqapi/policy_sat" --data-urlencode "env=testdefs" \
	--data-urlencode "privpol=privacy_policy(dc1, [(p1,dpo22,dt1)])" \
	--data-urlencode "privpref=privacy_preference(ds1, [(p11,dpo2,di1)])"

curl -s -G "http://127.0.0.1:8001/pqapi/policy_sat" --data-urlencode "env=testdefs" \
	--data-urlencode "privpol=privacy_policy(dc1, [(p112,dpo2,dt1)])" \
	--data-urlencode "privpref=privacy_preference(ds1, [(p11,dpo2,di1)])"

curl -s -G "http://127.0.0.1:8001/pqapi/policy_sat" --data-urlencode "env=testdefs" \
	--data-urlencode "privpol=privacy_policy(dc1, [(p112,dpo1,dt1)])" \
	--data-urlencode "privpref=privacy_preference(ds1, [(p11,dpo2,di1)])"

curl -s -G "http://127.0.0.1:8001/pqapi/policy_sat" --data-urlencode "env=testdefs" \
	--data-urlencode "privpol=privacy_policy(dc1, [(p11,dpo22,dt1),(p112,dpo21,dt1)])" \
	--data-urlencode "privpref=privacy_preference(ds1, [(p1,dpo2,di1)])"

curl -s -G "http://127.0.0.1:8001/pqapi/policy_sat" --data-urlencode "env=testdefs" \
	--data-urlencode "privpol=privacy_policy(dc1, [(p1,dpo22,dt1),(p112,dpo21,dt1)])" \
	--data-urlencode "privpref=privacy_preference(ds1, [(p11,dpo2,di1)])"

curl -s -G "http://127.0.0.1:8001/pqapi/policy_sat" --data-urlencode "env=testdefs" \
	--data-urlencode "privpol=privacy_policy(dc1,  [(p1,dpo22,dt1),(p112,dpo21,dt1)])" \
	--data-urlencode "privpref=privacy_preference(ds1,  [(p1,dpo2,di1),(p1,dpo2,di1)])"

curl -s -G "http://127.0.0.1:8001/pqapi/policy_sat" --data-urlencode "env=testdefs" \
	--data-urlencode "privpol=privacy_policy(dc1, [(p112,dpo1,dt1),(p112,dpo21,dt1)])"\
	--data-urlencode "privpref=privacy_preference(ds1, [(p11,dpo2,di1),(p112,dpo1,di1)])"

curl -s -G "http://127.0.0.1:8001/pqapi/policy_sat" --data-urlencode "env=testdefs" \
	--data-urlencode "privpol=privacy_policy(dc1, [(p112,dpo1,dt1),(p112,dpo21,dt1),(p111,dpo12,dt1)])"\
	--data-urlencode "privpref=privacy_preference(ds1, [(p11,dpo2,di1),(p112,dpo1,di1)])"

curl -s -G "http://127.0.0.1:8001/pqapi/policy_sat" --data-urlencode "env=testdefs" \
	--data-urlencode "privpol=privacy_policy(dc1, [(p112,dpo1,dt1),(p112,dpo21,dt1),(p111,dpo12,dt1)])"\
	--data-urlencode "privpref=privacy_preference(ds1, [(p11,dpo22,di1),(p112,dpo1,di1)])"
echo end of dplp tests
