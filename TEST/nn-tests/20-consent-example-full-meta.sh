#!/bin/sh
# this script runs the expanded consent example of script 19 using all the meta elements
echo 'demo extended consent policy with incremental policy/condition build using meta-elements'

echo 'reset conditions and policy before tests are run'
curl -s -G "http://127.0.0.1:8001/paapi/resetcond" --data-urlencode "token=admin_token"
#curl -s -G "http://127.0.0.1:8001/paapi/reset" --data-urlencode "token=admin_token" --data-urlencode "domain=policy" --data-urlencode "name=consent_ex"

Echo 'set up the consent_ex policy root elements'
curl -s -G "http://127.0.0.1:8001/paapi/loadi" --data-urlencode "policyspec=policy(consent_ex,cpol_ex,[
    purpose('Purpose'), % these 3 are to satisfy prerequisites of dplp_policy meta-element
    operation('DataProcessing'),
    object_attribute('PersonalDataCategory'),
    dplp_policy_base(cpol_ex, testdefs2)

    %Policy_class(cpol_ex),
    %assign(cpol_ex,'PM'),
    %user_attribute(data_controllers),
    %object_attribute(data_subjects),
    %assign(data_controllers,cpol_ex),
    %assign(data_subjects,cpol_ex),
    %connector('PM')
    ], dplp)" --data-urlencode "token=admin_token"

echo 'set current policy to consent_ex'
curl -s -G "http://127.0.0.1:8001/paapi/setpol" --data-urlencode "policy=consent_ex" --data-urlencode "token=admin_token"

echo 'fetch the policy'
curl -s -G "http://127.0.0.1:8001/paapi/readpol" --data-urlencode "policy=consent_ex" --data-urlencode "token=admin_token"

echo 'add a Data Controller (DC)'
curl -s -G "http://127.0.0.1:8001/paapi/addm" --data-urlencode "policy=consent_ex" --data-urlencode "policy_elements=[ data_controller('dc[x]',[]) ]" --Data-urlencode "token=admin_token"
#curl -s -G "http://127.0.0.1:8001/paapi/add" --data-urlencode "policy=consent_ex" --data-urlencode "policy_element=data_controller('dc[x]',[])" --data-urlencode "token=admin_token"

echo 'fetch the policy'
curl -s -G "http://127.0.0.1:8001/paapi/readpol" --data-urlencode "policy=consent_ex" --data-urlencode "token=admin_token"

echo 'add a Data Processor (DP)'
curl -s -G "http://127.0.0.1:8001/paapi/addm" --data-urlencode "policy=consent_ex" --data-urlencode "policy_elements=[ data_processor('dp[y][x]', [], 'dc[x]') ]" --Data-urlencode "token=admin_token"

echo 'fetch the policy'
curl -s -G "http://127.0.0.1:8001/paapi/readpol" --data-urlencode "policy=consent_ex" --data-urlencode "token=admin_token"

echo 'add a Data Subject (DS)'
curl -s -G "http://127.0.0.1:8001/paapi/addm" --data-urlencode "policy=consent_ex" --data-urlencode "policy_elements=[ data_subject('ds[1]',['pdi(1)[1]':'pdc{1}'],[]) ]" --Data-urlencode "token=admin_token"

echo 'fetch the policy'
curl -s -G "http://127.0.0.1:8001/paapi/readpol" --data-urlencode "policy=consent_ex" --data-urlencode "token=admin_token"

echo 'add another personal data item (PDI)'
curl -s -G "http://127.0.0.1:8001/paapi/addm" --data-urlencode "policy=consent_ex" --data-urlencode "policy_elements=[ data_item('pdi(2)[1]', 'pdc{2}', 'ds[1]') ]" --Data-urlencode "token=admin_token"

echo 'fetch the policy'
curl -s -G "http://127.0.0.1:8001/paapi/readpol" --data-urlencode "policy=consent_ex" --data-urlencode "token=admin_token"

echo 'now add the first consent to the policy'
curl -s -G "http://127.0.0.1:8001/paapi/addm" --data-urlencode "policy=consent_ex" --data-urlencode "policy_elements=[consent(cID_234,'dc[x]','dp[y][x]','app(a,y,x)',['dpo(z)'],'p(v)','ds[1]','pdi(1)[1]','pdc{1}',true)]" --Data-urlencode "token=admin_token"
#curl -s -G "http://127.0.0.1:8001/paapi/add" --data-urlencode "policy=consent_ex" --data-urlencode "policy_element=consent(cID_234,'dc[x]','dp[y][x]','app(a,y,x)',['dpo(z)'],'p(v)','ds[1]','pdi(1)[1]','pdc{1}',true)" --data-urlencode "token=admin_token"

echo 'fetch the policy'
curl -s -G "http://127.0.0.1:8001/paapi/readpol" --data-urlencode "policy=consent_ex" --data-urlencode "token=admin_token"

echo 'read the conditions'
curl -s -G "http://127.0.0.1:8001/paapi/readcond" --data-urlencode "token=admin_token"

echo 'query the policy for an access - expect grant'
curl -s "http://127.0.0.1:8001/pqapi/access" --data-urlencode "policy=consent_ex" --data-urlencode "user=dp[y][x]" --data-urlencode "ar=dpo(z)" --data-urlencode "purpose=p(v)" --data-urlencode "object=pdi(1)[1]"

echo 'delete the consent meta-element'
curl -s -G "http://127.0.0.1:8001/paapi/delete" --data-urlencode "policy=consent_ex" --data-urlencode "token=admin_token" --data-urlencode "policy_element=consent(cID_234)"

echo 'fetch the policy'
curl -s -G "http://127.0.0.1:8001/paapi/readpol" --data-urlencode "policy=consent_ex" --data-urlencode "token=admin_token"

echo 'query the policy for an access - expect deny'
curl -s "http://127.0.0.1:8001/pqapi/access" --data-urlencode "policy=consent_ex" --data-urlencode "user=dp[y][x]" --data-urlencode "ar=dpo(z)" --Data-urlencode "purpose=p(v)" --data-urlencode "object=pdi(1)[1]"

echo 'now add back the first consent to the policy'
curl -s -G "http://127.0.0.1:8001/paapi/addm" --data-urlencode "policy=consent_ex" --data-urlencode "policy_elements=[consent(cID_234,'dc[x]','dp[y][x]','app(a,y,x)',['dpo(z)'],'p(v)','ds[1]','pdi(1)[1]','pdc{1}',true)]" --data-urlencode "token=admin_token"

echo 'add a second Data Subject (DS)'
curl -s -G "http://127.0.0.1:8001/paapi/addm" --data-urlencode "policy=consent_ex" --data-urlencode "policy_elements=[ data_subject('ds[2]',[],[]) ]" --Data-urlencode "token=admin_token"

echo 'add a personal data item to second subject(PDI)'
curl -s -G "http://127.0.0.1:8001/paapi/addm" --data-urlencode "policy=consent_ex" --data-urlencode "policy_elements=[ data_item('pdi(1)[2]', 'pdc{1}', 'ds[2]') ]" --Data-urlencode "token=admin_token"

echo 'add consent for second DS and data item'
curl -s -G "http://127.0.0.1:8001/paapi/addm" --data-urlencode "policy=consent_ex" --data-urlencode "policy_elements=[consent(cID_567,'dc[x]','dp[y][x]','app(a,y,x)',['dpo(z)'],'p(v)','ds[2]','pdi(1)[2]','pdc{1}',true)]" --Data-urlencode "token=admin_token"

echo 'fetch the policy'
curl -s -G "http://127.0.0.1:8001/paapi/readpol" --data-urlencode "policy=consent_ex" --data-urlencode "token=admin_token"

echo 'query the policy for an access for DS 1 - expect grant'
curl -s "http://127.0.0.1:8001/pqapi/access" --data-urlencode "policy=consent_ex" --data-urlencode "user=dp[y][x]" --data-urlencode "ar=dpo(z)" --data-urlencode "purpose=p(v)" --data-urlencode "object=pdi(1)[1]"

echo 'query the policy for an access for DS 2 - expect grant'
curl -s "http://127.0.0.1:8001/pqapi/access" --data-urlencode "policy=consent_ex" --data-urlencode "user=dp[y][x]" --data-urlencode "ar=dpo(z)" --data-urlencode "purpose=p(v)" --data-urlencode "object=pdi(1)[2]"

echo 'delete the consent meta-element for DS 1'
curl -s -G "http://127.0.0.1:8001/paapi/delete" --data-urlencode "policy=consent_ex" --data-urlencode "token=admin_token" --data-urlencode "policy_element=consent(cID_234)"

echo 'query the policy for an access for DS 1 - expect deny'
curl -s "http://127.0.0.1:8001/pqapi/access" --data-urlencode "policy=consent_ex" --data-urlencode "user=dp[y][x]" --data-urlencode "ar=dpo(z)" --data-urlencode "purpose=p(v)" --data-urlencode "object=pdi(1)[1]"

echo 'query the policy for an access for DS 2 - expect grant'
curl -s "http://127.0.0.1:8001/pqapi/access" --data-urlencode "policy=consent_ex" --data-urlencode "user=dp[y][x]" --data-urlencode "ar=dpo(z)" --data-urlencode "purpose=p(v)" --data-urlencode "object=pdi(1)[2]"

echo 'delete the consent meta-element for DS 2'
# curl -s -G "http://127.0.0.1:8001/paapi/delete" --data-urlencode "policy=consent_ex" --data-urlencode "token=admin_token" --data-urlencode "policy_element=consent(cID_567)"
curl -s -G "http://127.0.0.1:8001/paapi/deletem" --data-urlencode "policy=consent_ex" --data-urlencode "token=admin_token" --data-urlencode "name=cID_567"

echo 'fetch the policy'
curl -s -G "http://127.0.0.1:8001/paapi/readpol" --data-urlencode "policy=consent_ex" --data-urlencode "token=admin_token"

echo 'query the policy for an access for DS 1 - expect deny'
curl -s "http://127.0.0.1:8001/pqapi/access" --data-urlencode "policy=consent_ex" --data-urlencode "user=dp[y][x]" --data-urlencode "ar=dpo(z)" --data-urlencode "purpose=p(v)" --data-urlencode "object=pdi(1)[1]"

echo 'query the policy for an access for DS 2 - expect deny'
curl -s "http://127.0.0.1:8001/pqapi/access" --data-urlencode "policy=consent_ex" --data-urlencode "user=dp[y][x]" --data-urlencode "ar=dpo(z)" --data-urlencode "purpose=p(v)" --data-urlencode "object=pdi(1)[2]"
