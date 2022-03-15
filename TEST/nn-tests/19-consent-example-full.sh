#!/bin/sh
# this script runs the expanded consent example
# see EXAMPLES/SmashHit_CSSP_mech_use_04a.pptx for further description
#

echo 'demo extended consent policy with incremental policy/condition build'

echo 'reset conditions and policy before tests are run'
curl -s -G "http://127.0.0.1:8001/paapi/resetcond" --data-urlencode "token=admin_token"
curl -s -G "http://127.0.0.1:8001/paapi/reset" --data-urlencode "token=admin_token" --data-urlencode "domain=policies" --data-urlencode "name=consent_ex"

#
# POLICY ROOT -- root of the policy
# template: loadi( connector('PM'), policy_class(POLICY_CLASS), assign(POLICY_CLASS,'PM) )
#
echo 'set up the consent_ex policy root elements'
curl -s -G "http://127.0.0.1:8001/paapi/loadi" --data-urlencode "policyspec=policy(consent_ex,cpol_ex,[
    definitions(onto),
    policy_class(cpol_ex),
    assign(cpol_ex,'PM'),
    user_attribute(data_controllers),
    object_attribute(data_subjects),
    assign(data_controllers,cpol_ex),
    assign(data_subjects,cpol_ex),
    connector('PM')
    ], dplp)" --data-urlencode "token=admin_token"

echo 'set current policy to consent_ex'
curl -s -G "http://127.0.0.1:8001/paapi/setpol" --data-urlencode "policy=consent_ex" --data-urlencode "token=admin_token"

# DISPLAY
echo 'fetch the policy'
curl -s -G "http://127.0.0.1:8001/paapi/readpol" --data-urlencode "policy=consent_ex" --data-urlencode "token=admin_token"

#
# REGISTER DATA CONTROLLER (DC) -- (must happen before a consent involving the DC is added)
# template: addm( [user_attribute(DC_ID), assign(DC_ID, data_controllers)] )
#
echo 'add a Data Controller (DC)'
curl -s -G "http://127.0.0.1:8001/paapi/addm" --data-urlencode "policy=consent_ex" --data-urlencode "policy_elements=[
    user_attribute('dc_[x]'),
    assign('dc_[x]', data_controllers)
    ]" --data-urlencode "token=admin_token"

echo 'fetch the policy'
curl -s -G "http://127.0.0.1:8001/paapi/readpol" --data-urlencode "policy=consent_ex" --data-urlencode "token=admin_token"

#
# REGISTER DATA PROCESSOR (DP) -- (must happen before a consent involving the DP)
#               may happen after Data Controller is registered
# template: addm( [user(DP_ID), assign(DP_ID, DC_ID)] )
#
echo 'add a Data Processor (DP)'
curl -s -G "http://127.0.0.1:8001/paapi/addm" --data-urlencode "policy=consent_ex" --data-urlencode "policy_elements=[
    user('dp_[y][x]'),
    assign('dp_[y][x]', 'dc_[x]')
    ]" --data-urlencode "token=admin_token"

echo 'fetch the policy'
curl -s -G "http://127.0.0.1:8001/paapi/readpol" --data-urlencode "policy=consent_ex" --data-urlencode "token=admin_token"

#
# REGISTER DATA SUBJECT (DS) -- (must happen before a consent involving the DS)
# template: addm( [object_attribute(DS_ID), assign(DS_ID, data_subjects)] )
#
echo 'add a Data Subject (DS)'
curl -s -G "http://127.0.0.1:8001/paapi/addm" --data-urlencode "policy=consent_ex" --data-urlencode "policy_elements=[
    object_attribute('ds_[1]'),
    assign('ds_[1]', data_subjects)
    ]" --data-urlencode "token=admin_token"

echo 'fetch the policy'
curl -s -G "http://127.0.0.1:8001/paapi/readpol" --data-urlencode "policy=consent_ex" --data-urlencode "token=admin_token"

#
# REGISTER DATA ITEMS (must happen before a contract involving the data item)
# template: addm( [object(DATA_ITEM_ID), assign(DATA_ITEM_ID, DS_ID), assign(DATA_ITEM_ID, DATA_CATEGORY_ID] )
#
echo 'add a personal data item (PDI)'
curl -s -G "http://127.0.0.1:8001/paapi/addm" --data-urlencode "policy=consent_ex" --data-urlencode "policy_elements=[
    object('pdi_(1)[1]'),

    object_attribute('pdc_{1}'), 
    assign('pdc_{1}',cpol_ex),

    assign('pdi_(1)[1]', 'ds_[1]'),
    assign('pdi_(1)[1]', 'pdc_{1}')
    ]" --data-urlencode "token=admin_token"

# DISPLAY
echo 'fetch the policy'
curl -s -G "http://127.0.0.1:8001/paapi/readpol" --data-urlencode "policy=consent_ex" --data-urlencode "token=admin_token"

#
# CREATE CONSENT
#     Two steps:
#           1) install the condition declaration and predicate definitions corresponding to the offer
#           2) see below
# template: loadcondi( condition_variable(NAME:TYPE), ..., condition_predicate(COND_PRED_DECLARATION), (COND_PRED_DEFINITION) )
#
#echo 'add the conditions needed by the contract'
#curl -s -G "http://127.0.0.1:8001/paapi/loadcondi" --data-urlencode "cond_elements=[
#    condition_variable(loMax:number), condition_variable(laMin:number),
#    condition_predicate( pred_234, [name] ),
#    (pred_234(Name) :-
#        Name == x ) )
#    ]" --data-urlencode "token=admin_token"
#           2) install the consent meta-element
# template: addm( [ user_attribute(CONSENT_UA), object_attribute(CONSENT_OA),
#           assign('dp_[y][x]', CONSENTUA), assign(CONSENTUA, DC_ID),
#           assign(pdi_(1)[1]', CONSENTOA), assign(CONSENTOQ, DS_ID)),
#           cond( COND_PRED_INVOCATION, associate(CONSENTUA, [r], CONSENTOA) ) ] )
#
echo 'now add the first consent to the policy'
curl -s -G "http://127.0.0.1:8001/paapi/add" --data-urlencode "policy=consent_ex" --data-urlencode "policy_element=consent(cID_234,'dc_[x]','dp_[y][x]','app(a,y,x)',['dpo_(z)'],'p_(v)','ds_[1]','pdi_(1)[1]','pdc_{1}',true)" --data-urlencode "token=admin_token"

# read-out the policy
echo 'read the policy (explicitly named as consent_ex)'
curl -s -G "http://127.0.0.1:8001/paapi/readpol" --data-urlencode "policy=consent_ex" --data-urlencode "token=admin_token"

# read out the conditions
echo 'read the conditions'
curl -s -G "http://127.0.0.1:8001/paapi/readcond" --data-urlencode "token=admin_token"

echo 'query the policy for an access'
curl -s "http://127.0.0.1:8001/pqapi/access" --data-urlencode "policy=consent_ex" --data-urlencode "user=dp_[y][x]" --data-urlencode "ar=dpo_(z)" --data-urlencode "purpose=p_(v)" --data-urlencode "object=pdi_(1)[1]"

echo 'delete the consent meta-element'
curl -s -G "http://127.0.0.1:8001/paapi/delete" --data-urlencode "policy=consent_ex" --data-urlencode "token=admin_token" --data-urlencode "policy_element=consent(cID_234)"

echo 'fetch the policy'
curl -s -G "http://127.0.0.1:8001/paapi/readpol" --data-urlencode "policy=consent_ex" --data-urlencode "token=admin_token"

echo 'query the policy for an access'
curl -s "http://127.0.0.1:8001/pqapi/access" --data-urlencode "policy=consent_ex" --data-urlencode "user=dp_[y][x]" --data-urlencode "ar=dpo_(z)" --data-urlencode "purpose=p_(v)" --data-urlencode "object=pdi_(1)[1]"

echo 'end demo extended consent policy with incremental policy/condition build'

#########################################################################
# now extend the esample with another consent from a different DS
# but with the same Data Processor
#########################################################################
echo 'now add back the first consent to the policy'
curl -s -G "http://127.0.0.1:8001/paapi/add" --data-urlencode "policy=consent_ex" --data-urlencode "policy_element=consent(cID_234,'dc_[x]','dp_[y][x]','app(a,y,x)',['dpo_(z)'],'p_(v)','ds_[1]','pdi_(1)[1]','pdc_{1}',true)" --data-urlencode "token=admin_token"

echo 'add a second Data Subject (DS)'
curl -s -G "http://127.0.0.1:8001/paapi/addm" --data-urlencode "policy=consent_ex" --data-urlencode "policy_elements=[
    object_attribute('ds_[2]'),
    assign('ds_[2]', data_subjects)
    ]" --data-urlencode "token=admin_token"

echo 'add a personal data item to second subject(PDI)'
curl -s -G "http://127.0.0.1:8001/paapi/addm" --data-urlencode "policy=consent_ex" --data-urlencode "policy_elements=[
    object('pdi_(1)[2]'),

    object_attribute('pdc_{1}'), 
    assign('pdc_{1}',cpol_ex),

    assign('pdi_(1)[2]', 'ds_[2]'),
    assign('pdi_(1)[2]', 'pdc_{1}')
    ]" --data-urlencode "token=admin_token"

echo 'add consent for second DS and data item'
curl -s -G "http://127.0.0.1:8001/paapi/add" --data-urlencode "policy=consent_ex" --data-urlencode "policy_element=consent(cID_567,'dc_[x]','dp_[y][x]','app(a,y,x)',['dpo_(z)'],'p_(v)','ds_[2]','pdi_(1)[2]','pdc_{1}',true)" --data-urlencode "token=admin_token"

# read-out the policy
echo 'read the policy (explicitly named as consent_ex)'
curl -s -G "http://127.0.0.1:8001/paapi/readpol" --data-urlencode "policy=consent_ex" --data-urlencode "token=admin_token"

echo 'query the policy for an access for DS 1'
curl -s "http://127.0.0.1:8001/pqapi/access" --data-urlencode "policy=consent_ex" --data-urlencode "user=dp_[y][x]" --data-urlencode "ar=dpo_(z)" --data-urlencode "purpose=p_(v)" --data-urlencode "object=pdi_(1)[1]"

echo 'query the policy for an access for DS 2'
curl -s "http://127.0.0.1:8001/pqapi/access" --data-urlencode "policy=consent_ex" --data-urlencode "user=dp_[y][x]" --data-urlencode "ar=dpo_(z)" --data-urlencode "purpose=p_(v)" --data-urlencode "object=pdi_(1)[2]"

echo 'delete the consent meta-element for DS 1'
curl -s -G "http://127.0.0.1:8001/paapi/delete" --data-urlencode "policy=consent_ex" --data-urlencode "token=admin_token" --data-urlencode "policy_element=consent(cID_234)"

echo 'query the policy for an access for DS 1'
curl -s "http://127.0.0.1:8001/pqapi/access" --data-urlencode "policy=consent_ex" --data-urlencode "user=dp_[y][x]" --data-urlencode "ar=dpo_(z)" --data-urlencode "purpose=p_(v)" --data-urlencode "object=pdi_(1)[1]"

echo 'query the policy for an access for DS 2'
curl -s "http://127.0.0.1:8001/pqapi/access" --data-urlencode "policy=consent_ex" --data-urlencode "user=dp_[y][x]" --data-urlencode "ar=dpo_(z)" --data-urlencode "purpose=p_(v)" --data-urlencode "object=pdi_(1)[2]"

echo 'delete the consent meta-element for DS 2'
curl -s -G "http://127.0.0.1:8001/paapi/delete" --data-urlencode "policy=consent_ex" --data-urlencode "token=admin_token" --data-urlencode "policy_element=consent(cID_567)"

echo 'query the policy for an access for DS 1'
curl -s "http://127.0.0.1:8001/pqapi/access" --data-urlencode "policy=consent_ex" --data-urlencode "user=dp_[y][x]" --data-urlencode "ar=dpo_(z)" --data-urlencode "purpose=p_(v)" --data-urlencode "object=pdi_(1)[1]"

echo 'query the policy for an access for DS 2'
curl -s "http://127.0.0.1:8001/pqapi/access" --data-urlencode "policy=consent_ex" --data-urlencode "user=dp_[y][x]" --data-urlencode "ar=dpo_(z)" --data-urlencode "purpose=p_(v)" --data-urlencode "object=pdi_(1)[2]"

# read-out the policy
echo 'read the policy (explicitly named as consent_ex)'
curl -s -G "http://127.0.0.1:8001/paapi/readpol" --data-urlencode "policy=consent_ex" --data-urlencode "token=admin_token"
