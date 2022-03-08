echo 'set policy to consent1'
curl -s -G "http://127.0.0.1:8001/paapi/setpol" --data-urlencode "policy=consent1" --data-urlencode "token=admin_token"

echo 'fetch the unmodified policy'
curl -s -G "http://127.0.0.1:8001/paapi/readpol" --data-urlencode "policy=consent1" --data-urlencode "token=admin_token"

echo 'perform access check without consent'
curl -s "http://127.0.0.1:8001/pqapi/access" --data-urlencode "policy=consent1" --data-urlencode "user=dp_[y][x]" --data-urlencode "ar=dpo_(z)" --data-urlencode "purpose=p_(v)" --data-urlencode "object=pdi_(1)[1]"

echo 'add consent meta-element to policy'
curl -s -G "http://127.0.0.1:8001/paapi/add" --data-urlencode "policy=consent1" --data-urlencode "token=admin_token" --data-urlencode "policy_element=consent(cID_234,'dc_[x]','dp_[y][x]','app(a,y,x)',['dpo_(z)'],'p_(v)','ds_[1]','pdi_(1)[1]','pdc_{1}',true)"

echo 'fetch the modified policy'
curl -s -G "http://127.0.0.1:8001/paapi/readpol" --data-urlencode "policy=consent1" --data-urlencode "token=admin_token"

echo 'reset the policy'
curl -s -G "http://127.0.0.1:8001/paapi/reset" --data-urlencode "token=admin_token" --data-urlencode "domain=policies" --data-urlencode "name=consent1"

echo 'fetch the reset policy'
curl -s -G "http://127.0.0.1:8001/paapi/readpol" --data-urlencode "policy=consent1" --data-urlencode "token=admin_token"

echo 'add consent meta-element to policy'
curl -s -G "http://127.0.0.1:8001/paapi/add" --data-urlencode "policy=consent1" --data-urlencode "token=admin_token" --data-urlencode "policy_element=consent(cID_234,'dc_[x]','dp_[y][x]','app(a,y,x)',['dpo_(z)'],'p_(v)','ds_[1]','pdi_(1)[1]','pdc_{1}',true)"

echo 'perform access check with consent'
curl -s "http://127.0.0.1:8001/pqapi/access" --data-urlencode "policy=consent1" --data-urlencode "user=dp_[y][x]" --data-urlencode "ar=dpo_(z)" --data-urlencode "purpose=p_(v)" --data-urlencode "object=pdi_(1)[1]"

echo 'delete the consent meta-element'
curl -s -G "http://127.0.0.1:8001/paapi/delete" --data-urlencode "policy=consent1" --data-urlencode "token=admin_token" --data-urlencode "policy_element=consent(cID_234)"

echo 'fetch the restored policy'
curl -s -G "http://127.0.0.1:8001/paapi/readpol" --data-urlencode "policy=consent1" --data-urlencode "token=admin_token"

echo 'perform access check without consent'
curl -s "http://127.0.0.1:8001/pqapi/access" --data-urlencode "policy=consent1" --data-urlencode "user=dp_[y][x]" --data-urlencode "ar=dpo_(z)" --data-urlencode "purpose=p_(v)" --data-urlencode "object=pdi_(1)[1]"

echo end of consent-meta test
