#!/bin/sh
# this script runs the expanded consent example of script 19 using all the meta elements
echo 'demo extended consent policy with incremental policy/condition build using meta-elements'

echo 'reset conditions and policy before tests are run'
curl -s -G "http://127.0.0.1:8001/paapi/resetcond" --data-urlencode "token=admin_token"
curl -s -G "http://127.0.0.1:8001/paapi/reset" --data-urlencode "token=admin_token" --data-urlencode "domain=policy" --data-urlencode "name=dplp_min"

echo 'set current policy to dplp_min'
curl -s -G "http://127.0.0.1:8001/paapi/setpol" --data-urlencode "policy=dplp_min" --data-urlencode "token=admin_token"

echo 'set up the policy root elements'
curl -s -G "http://127.0.0.1:8001/dplp/add_dplp_policy_base"  --data-urlencode "policy=dplp_min" --data-urlencode "policy_class=pc" --data-urlencode "definitions=testdefs2" --data-urlencode "token=admin_token"

echo 'fetch the policy'
curl -s -G "http://127.0.0.1:8001/paapi/readpol" --data-urlencode "policy=dplp_min" --data-urlencode "token=admin_token"

echo 'add a Data Controller (DC)'
curl -s -G "http://127.0.0.1:8001/dplp/add_data_controller" --data-urlencode "policy=dplp_min" --data-urlencode "data_controller=dc[x]" --data-urlencode "privacy_policy=[]" --data-urlencode "token=admin_token"

echo 'fetch the policy'
curl -s -G "http://127.0.0.1:8001/paapi/readpol" --data-urlencode "policy=dplp_min" --data-urlencode "token=admin_token"

echo 'add a Data Processor (DP)'
curl -s -G "http://127.0.0.1:8001/dplp/add_data_processor" --data-urlencode "policy=dplp_min" --data-urlencode "data_processor=dp[y][x]" --data-urlencode "privacy_policy=[]"  --data-urlencode "data_controller=dc[x]" --data-urlencode "token=admin_token"

echo 'fetch the policy'
curl -s -G "http://127.0.0.1:8001/paapi/readpol" --data-urlencode "policy=dplp_min" --data-urlencode "token=admin_token"

echo 'add a Data Subject (DS)'
curl -s -G "http://127.0.0.1:8001/dplp/add_data_subject" --data-urlencode "policy=dplp_min" --data-urlencode "data_subject=ds[1]" --data-urlencode "privacy_preference=[]" --data-urlencode "data_items=['pdi(1)[1]':'pdc{1}']" --data-urlencode "token=admin_token"

echo 'fetch the policy'
curl -s -G "http://127.0.0.1:8001/paapi/readpol" --data-urlencode "policy=dplp_min" --data-urlencode "token=admin_token"

echo 'add another personal data item (PDI)'
curl -s -G "http://127.0.0.1:8001/dplp/add_data_item" --data-urlencode "policy=dplp_min" --data-urlencode "data_item=pdi(2)[1]" --data-urlencode "data_category=pdc{2}" --data-urlencode "data_subject=ds[1]" --data-urlencode "token=admin_token"

echo 'fetch the policy'
curl -s -G "http://127.0.0.1:8001/paapi/readpol" --data-urlencode "policy=dplp_min" --data-urlencode "token=admin_token"

echo 'add an application'
curl -s -G "http://127.0.0.1:8001/dplp/add_application" --data-urlencode "policy=dplp_min" --data-urlencode "application=dp[y][x]_app1" --data-urlencode "data_processor=dp[y][x]" --data-urlencode "operations=['dpo(w)','dpo(z)']" --data-urlencode "token=admin_token"

echo 'now add the first consent to the policy'
curl -s -G "http://127.0.0.1:8001/dplp/add_consent" --data-urlencode "policy=dplp_min" --data-urlencode "consent_id=cID_234" --data-urlencode "data_controller=dc[x]" --data-urlencode "data_processor=dp[y][x]" --data-urlencode "application=dp[y][x]_app1" --data-urlencode "operations=['dp[y][x]_app1']" --data-urlencode "purpose=p(v)" --data-urlencode "data_item=pdi(1)[1]" --data-urlencode "data_subject=ds[1]"  --data-urlencode "data_category=pdc{1}" --data-urlencode "constraint=true"  --data-urlencode "token=admin_token"

echo 'fetch the policy'
curl -s -G "http://127.0.0.1:8001/paapi/readpol" --data-urlencode "policy=dplp_min" --data-urlencode "token=admin_token"

echo 'read the conditions'
curl -s -G "http://127.0.0.1:8001/paapi/readcond" --data-urlencode "token=admin_token"

echo 'query the policy for an access - expect grant'
curl -s "http://127.0.0.1:8001/pqapi/access" --data-urlencode "policy=dplp_min" --data-urlencode "user=dp[y][x]" --data-urlencode "ar=dpo(z)" --data-urlencode "purpose=p(v)" --data-urlencode "object=pdi(1)[1]"

echo 'delete the consent meta-element'
curl -s -G "http://127.0.0.1:8001/dplp/delete_consent" --data-urlencode "policy=dplp_min" --data-urlencode "token=admin_token" --data-urlencode "consent_id=cID_234"

echo 'fetch the policy'
curl -s -G "http://127.0.0.1:8001/paapi/readpol" --data-urlencode "policy=dplp_min" --data-urlencode "token=admin_token"

echo 'query the policy for an access - expect deny'
curl -s "http://127.0.0.1:8001/pqapi/access" --data-urlencode "policy=dplp_min" --data-urlencode "user=dp[y][x]" --data-urlencode "ar=dpo(z)" --data-urlencode "purpose=p(v)" --data-urlencode "object=pdi(1)[1]"

echo 'now add back the first consent to the policy'
curl -s -G "http://127.0.0.1:8001/dplp/add_consent" --data-urlencode "policy=dplp_min" --data-urlencode "consent_id=cID_234" --data-urlencode "data_controller=dc[x]" --data-urlencode "data_processor=dp[y][x]" --data-urlencode "application=dp[y][x]_app1" --data-urlencode "operations=['dp[y][x]_app1']" --data-urlencode "purpose=p(v)" --data-urlencode "data_item=pdi(1)[1]" --data-urlencode "data_subject=ds[1]"  --data-urlencode "data_category=pdc{1}" --data-urlencode "constraint=true"  --data-urlencode "token=admin_token"

echo 'add a second Data Subject (DS)'
curl -s -G "http://127.0.0.1:8001/dplp/add_data_subject" --data-urlencode "policy=dplp_min" --data-urlencode "data_subject=ds[2]" --data-urlencode "privacy_preference=[]" --data-urlencode "data_items=[]" --data-urlencode "token=admin_token"

echo 'add a personal data item to second subject(PDI)'
curl -s -G "http://127.0.0.1:8001/dplp/add_data_item" --data-urlencode "policy=dplp_min" --data-urlencode "data_item=pdi(1)[2]" --data-urlencode "data_category=pdc{1}" --data-urlencode "data_subject=ds[2]" --data-urlencode "token=admin_token"

echo 'add consent for second DS and data item'
curl -s -G "http://127.0.0.1:8001/dplp/add_consent" --data-urlencode "policy=dplp_min" --data-urlencode "consent_id=cID_567" --data-urlencode "data_controller=dc[x]" --data-urlencode "data_processor=dp[y][x]" --data-urlencode "application=dp[y][x]_app1" --data-urlencode "operations=['dpo(z)']" --data-urlencode "purpose=p(v)" --data-urlencode "data_item=pdi(1)[2]" --data-urlencode "data_subject=ds[2]"  --data-urlencode "data_category=pdc{1}" --data-urlencode "constraint=true"  --data-urlencode "token=admin_token"

echo 'fetch the policy'
curl -s -G "http://127.0.0.1:8001/paapi/readpol" --data-urlencode "policy=dplp_min" --data-urlencode "token=admin_token"

echo 'query the policy for an access for DS 1 - expect grant'
curl -s "http://127.0.0.1:8001/pqapi/access" --data-urlencode "policy=dplp_min" --data-urlencode "user=dp[y][x]" --data-urlencode "ar=dpo(z)" --data-urlencode "purpose=p(v)" --data-urlencode "object=pdi(1)[1]"

echo 'query the policy for an access for DS 2 - expect grant'
curl -s "http://127.0.0.1:8001/pqapi/access" --data-urlencode "policy=dplp_min" --data-urlencode "user=dp[y][x]" --data-urlencode "ar=dpo(z)" --data-urlencode "purpose=p(v)" --data-urlencode "object=pdi(1)[2]"
# here
echo 'delete the consent meta-element for DS 1'
curl -s -G "http://127.0.0.1:8001/dplp/delete_consent" --data-urlencode "policy=dplp_min" --data-urlencode "token=admin_token" --data-urlencode "consent_id=cID_234"

echo 'query the policy for an access for DS 1 - expect deny'
curl -s "http://127.0.0.1:8001/pqapi/access" --data-urlencode "policy=dplp_min" --data-urlencode "user=dp[y][x]" --data-urlencode "ar=dpo(z)" --data-urlencode "purpose=p(v)" --data-urlencode "object=pdi(1)[1]"

echo 'query the policy for an access for DS 2 - expect grant'
curl -s "http://127.0.0.1:8001/pqapi/access" --data-urlencode "policy=dplp_min" --data-urlencode "user=dp[y][x]" --data-urlencode "ar=dpo(z)" --data-urlencode "purpose=p(v)" --data-urlencode "object=pdi(1)[2]"

echo 'delete the consent meta-element for DS 2'
curl -s -G "http://127.0.0.1:8001/dplp/delete_consent" --data-urlencode "policy=dplp_min" --data-urlencode "token=admin_token" --data-urlencode "consent_id=cID_567"

echo 'fetch the policy'
curl -s -G "http://127.0.0.1:8001/paapi/readpol" --data-urlencode "policy=dplp_min" --data-urlencode "token=admin_token"

echo 'query the policy for an access for DS 1 - expect deny'
curl -s "http://127.0.0.1:8001/pqapi/access" --data-urlencode "policy=dplp_min" --data-urlencode "user=dp[y][x]" --data-urlencode "ar=dpo(z)" --data-urlencode "purpose=p(v)" --data-urlencode "object=pdi(1)[1]"

echo 'query the policy for an access for DS 2 - expect deny'
curl -s "http://127.0.0.1:8001/pqapi/access" --data-urlencode "policy=dplp_min" --data-urlencode "user=dp[y][x]" --data-urlencode "ar=dpo(z)" --data-urlencode "purpose=p(v)" --data-urlencode "object=pdi(1)[2]"
