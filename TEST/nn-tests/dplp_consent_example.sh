#!/bin/sh
# this script runs the consent example using the meta elements
# execute with an argument of "atb" to run remotely on the ngac-server hosted by ATB (otherwise it uses and instance at port 8001 on the local host

echo 'demo extended consent policy with incremental policy/condition build using meta-elements'

if [ $1x == atbx ]
then
url="ontologies.atb-bremen.de:8001"
else
url="http://127.0.0.1:8001"
fi
echo Using server at: $url

echo curl -s -G $url/paapi/setpol --data-urlencode "policy=dplp_min" --data-urlencode "token=admin_token"
curl -s -G $url/paapi/setpol --data-urlencode "policy=dplp_min" --data-urlencode "token=admin_token"

echo curl -s -G $url/paapi/getpol --data-urlencode "token=admin_token"
curl -s -G $url/paapi/getpol --data-urlencode "token=admin_token"

echo curl -s -G $url/paapi/reset --data-urlencode "token=admin_token" --data-urlencode "domain=policy" --data-urlencode "name=dplp_min"
curl -s -G $url/paapi/reset --data-urlencode "token=admin_token" --data-urlencode "domain=policy" --data-urlencode "name=dplp_min"

echo curl -s -G $url/paapi/readpol --data-urlencode "token=admin_token"
curl -s -G $url/paapi/readpol --data-urlencode "token=admin_token"

echo curl -s -G $url/dplp/add_dplp_policy_base  --data-urlencode "policy_class=pc" --data-urlencode "definitions=smashHitCore202210" --data-urlencode "token=admin_token"
curl -s -G $url/dplp/add_dplp_policy_base  --data-urlencode "policy_class=pc" --data-urlencode "definitions=smashHitCore202210" --data-urlencode "token=admin_token"


echo curl -s -G $url/dplp/add_data_subject --data-urlencode "data_subject=ds[123BVC112]" --data-urlencode "privacy_preference=[]" --data-urlencode "data_items=['pdi(123BVC112)[123BVC112]':'Birth Date']" --data-urlencode "token=admin_token"
curl -s -G $url/dplp/add_data_subject --data-urlencode "data_subject=ds[123BVC112]" --data-urlencode "privacy_preference=[]" --data-urlencode "data_items=['pdi(123BVC112)[123BVC112]':'Birth Date']" --data-urlencode "token=admin_token"

echo curl -s -G $url/dplp/add_data_controller --data-urlencode "data_controller=dc[tTEK1235121]" --data-urlencode "privacy_policy=[]" --data-urlencode "token=admin_token"
curl -s -G $url/dplp/add_data_controller --data-urlencode "data_controller=dc[tTEK1235121]" --data-urlencode "privacy_policy=[]" --data-urlencode "token=admin_token"

echo curl -s -G $url/dplp/add_data_processor --data-urlencode "data_processor=dp[tEST123411][tTEK1235121]" --data-urlencode "privacy_policy=[]"  --data-urlencode "data_controller=dc[tTEK1235121]" --data-urlencode "token=admin_token"
curl -s -G $url/dplp/add_data_processor --data-urlencode "data_processor=dp[tEST123411][tTEK1235121]" --data-urlencode "privacy_policy=[]"  --data-urlencode "data_controller=dc[tTEK1235121]" --data-urlencode "token=admin_token"

echo curl -s -G $url/dplp/add_application --data-urlencode "application=dp[tEST123411][tTEK1235121]_appl" --data-urlencode "data_processor=dp[tEST123411][tTEK1235121]" --data-urlencode "operations=['Collect','Store','Use']" --data-urlencode "token=admin_token"
curl -s -G $url/dplp/add_application --data-urlencode "application=dp[tEST123411][tTEK1235121]_appl" --data-urlencode "data_processor=dp[tEST123411][tTEK1235121]" --data-urlencode "operations=['Collect','Store','Use']" --data-urlencode "token=admin_token"

echo curl -s -G $url/dplp/add_consent --data-urlencode "consent_id=C33313" --data-urlencode "data_controller=dc[tTEK1235121]" --data-urlencode "data_processor=dp[tEST123411][tTEK1235121]" --data-urlencode "application=dp[tEST123411][tTEK1235121]_appl" --data-urlencode "operations=['dp[tEST123411][tTEK1235121]_appl']" --data-urlencode "purpose=Commercial Interest" --data-urlencode "data_item=pdi(123BVC112)[123BVC112]" --data-urlencode "data_subject=ds[123BVC112]"  --data-urlencode "data_category=Birth Date" --data-urlencode "constraint=true"  --data-urlencode "token=admin_token"
curl -s -G $url/dplp/add_consent --data-urlencode "consent_id=C33313" --data-urlencode "data_controller=dc[tTEK1235121]" --data-urlencode "data_processor=dp[tEST123411][tTEK1235121]" --data-urlencode "application=dp[tEST123411][tTEK1235121]_appl" --data-urlencode "operations=['dp[tEST123411][tTEK1235121]_appl']" --data-urlencode "purpose=Commercial Interest" --data-urlencode "data_item=pdi(123BVC112)[123BVC112]" --data-urlencode "data_subject=ds[123BVC112]"  --data-urlencode "data_category=Birth Date" --data-urlencode "constraint=true"  --data-urlencode "token=admin_token"

echo curl -s $url/pqapi/access --data-urlencode "user=dp[tEST123411][tTEK1235121]" --data-urlencode "ar=Use" --data-urlencode "purpose=Sell Products To Data Subject" --data-urlencode "object=pdi(123BVC112)[123BVC112]"
curl -s $url/pqapi/access --data-urlencode "user=dp[tEST123411][tTEK1235121]" --data-urlencode "ar=Use" --data-urlencode "purpose=Sell Products To Data Subject" --data-urlencode "object=pdi(123BVC112)[123BVC112]"

echo curl -s -G $url/dplp/delete_consent --data-urlencode "consent_id=C33313" --data-urlencode "token=admin_token" 
curl -s -G $url/dplp/delete_consent --data-urlencode "consent_id=C33313" --data-urlencode "token=admin_token" 

echo curl -s $url/pqapi/access --data-urlencode "user=dp[tEST123411][tTEK1235121]" --data-urlencode "ar=Use" --data-urlencode "purpose=Sell Products To Data Subject" --data-urlencode "object=pdi(123BVC112)[123BVC112]"
curl -s $url/pqapi/access --data-urlencode "user=dp[tEST123411][tTEK1235121]" --data-urlencode "ar=Use" --data-urlencode "purpose=Sell Products To Data Subject" --data-urlencode "object=pdi(123BVC112)[123BVC112]"

echo curl -s $url/paapi/readpol --data-urlencode "token=admin_token" 
curl -s $url/paapi/readpol --data-urlencode "token=admin_token" 
