echo 'set policy to consent1'
curl -s -G "http://127.0.0.1:8001/paapi/setpol" --data-urlencode "policy=consent1" --data-urlencode "token=admin_token"

echo 'reset policy before tests are run'
curl -s -G "http://127.0.0.1:8001/paapi/reset" --data-urlencode "token=admin_token" --data-urlencode "domain=policy" --data-urlencode "name=consent1"

echo 'fetch the unmodified policy'
curl -s -G "http://127.0.0.1:8001/paapi/readpol" --data-urlencode "policy=consent1" --data-urlencode "token=admin_token"

Echo 'perform access check without consent - expect deny'
curl -s "http://127.0.0.1:8001/pqapi/access" --data-urlencode "policy=consent1" --data-urlencode "user=dp[y][x]" --data-urlencode "ar=dpo(z)" --data-urlencode "purpose=p(v)" --data-urlencode "object=pdi(1)[1]"

Echo 'add consent meta-element to policy'
curl -s -G "http://127.0.0.1:8001/paapi/add" --data-urlencode "policy=consent1" --data-urlencode "token=admin_token" --data-urlencode "policy_element=consent(cID_234,'dc[x]','dp[y][x]','app(a,y,x)',['dpo(z)'],'p(v)','ds[1]','pdi(1)[1]','pdc{1}',true)"

Echo 'fetch the modified policy'
curl -s -G "http://127.0.0.1:8001/paapi/readpol" --data-urlencode "policy=consent1" --data-urlencode "token=admin_token"

echo 'perform access check with consent - expect grant'
curl -s "http://127.0.0.1:8001/pqapi/access" --data-urlencode "policy=consent1" --data-urlencode "user=dp[y][x]" --data-urlencode "ar=dpo(z)" --data-urlencode "purpose=p(v)" --data-urlencode "object=pdi(1)[1]"

echo 'delete the consent meta-element'
curl -s -G "http://127.0.0.1:8001/paapi/delete" --data-urlencode "policy=consent1" --data-urlencode "token=admin_token" --data-urlencode "policy_element=consent(cID_234)"
#curl -s -G "http://127.0.0.1:8001/paapi/delete" --data-urlencode "policy=consent1" --data-urlencode "token=admin_token" --data-urlencode "name=cID_234"

Echo 'fetch the restored policy'
curl -s -G "http://127.0.0.1:8001/paapi/readpol" --data-urlencode "policy=consent1" --data-urlencode "token=admin_token"

echo 'perform access check without consent - expect deny'
curl -s "http://127.0.0.1:8001/pqapi/access" --data-urlencode "policy=consent1" --data-urlencode "user=dp[y][x]" --data-urlencode "ar=dpo(z)" --data-urlencode "purpose=p(v)" --data-urlencode "object=pdi(1)[1]"

echo end of consent-meta test
