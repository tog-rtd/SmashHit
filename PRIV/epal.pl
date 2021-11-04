% Enterprise Privacy Authorization Language (EPAL)

:- module(epal,[
	       ]).

:- use_module(dctg).

:- dynamic epal_initialized/1.

:- op( 100,  yfx, user:(^^) ).
:- op( 1178, xfx, user:(::=) ).
:- op( 1179, xfx, user:(<:>) ).
:- op( 1178, yfx, user:(&&) ).
:- op( 1177, xfx, user:(::-) ).
:- op( 1176, xfx, user:(from) ).

epal_initialized(false).

init:- param:initialized(true), !. % already initialized
init :-
	true.

re_init :- un_init, init.

un_init :-
	retractall( epal_initialized(_) ), assert( epal_initialized(false) ).


% EPAL (following is experimental and incomplete)
% Authorization Request <epal-query>
%   <epal-query> ::= [
%       <user-category>+
%       <data-category>+
%       <purpose>+
%       <action>+
%       <containers> ]
%   <user-category> ::= user_category( <epal:referringObjectType> )
%   <data-category> ::= data_category( <epal:referringObjectType> )
%   <purpose> ::= purpose( <epal:referringObjectType> )
%   <action> ::= action( <epal:referringObjectType> )
%   <containers> ::= <container>*
%   <container> ::= <refid> [ <attribute>* ]
%   <attribute> ::= <refid> [ <value>* ]
%   <value> ::= <epalSimpleType>
%   <refid> ::= <xs:NCName>
%
%   Example:
%       epal_query( [ user_category('SalesDepartment'),
%         data_category('CustomerRecord'),
%         purpose('OrderProcessing'),
%         action('Store'),
%	  container('CustomerRecord',[attribute(CustomerId,0123456789,'Alice')])
%	])
%
% Authorization Result <epal-ruling>
%   <epal-ruling> ::= [
%       <originating-rule>*
%       <obligation>* ]
%       <ruling>
%   <originating-rule> ::= originating_rule( <epal:referringObjectType>)
%   <obligation> ::= obligation( [ <originating-rule>+ <parameter>* ]
%   <parameter> ::= parameter( <refid> [ <value>* ] <attribute> )
%   <attribute> ::= attribute( simpleType, <epal:epalSimpleTypeURI> )
%   <ruling> ::= 'allow' | 'deny' | 'not-applicable'
%
%   Example:
%	epal_ruling('allow', [ originating_rule('rule1') ],
%         [ obligation( 'Retention', originating_rule('rule1'),
%                     parameter('Days', 2),
%                     parameter('Hours', 48) ) ]
%	)


epal_grammar([
    (	epal_request ::= epal_user_category, epal_action, epal_data_category, epal_purpose
        <:>
        epal(0)
    ),
    (	epal_ruling ::= ['ALLOW']
        <:>
        epal('ALLOW')
    ),
    (	epal_ruling ::= ['DENY']
        <:>
        epal('DENY')
    ),
    (	epal_user_category ::= ['Data User']
        <:>
        epal('Data User')
    ),
    (	epal_action ::= ['Action']
        <:>
        epal('Action')
    ),
    (	epal_data_category ::= ['Data Type']
        <:>
        epal('Data Type')
    ),
    (	epal_purpose ::= ['Purpose']
        <:>
        epal('Purpose')
    ),
    (	epal_condition ::= ['Condition']
        <:>
        epal('Condition')
    ),
    (	epal_obligation ::= ['Obligation']
        <:>
        epal('Obligation')
    ),
    (	epal_rule ::=
            epal_ruling^^ER, epal_user_category^^UC,
	    ['TO PERFORM'], epal_action^^EA,
	    ['ON'], epal_data_category^^DC,
	    ['FOR'], epal_purpose^^EP,
	    ['IF'], epal_condition^^EC,
	    ['AND CARRY OUT'], epal_obligation^^EO
        <:>
        epal(epal(Er,Uc,Ea,Dc,Ep,Ec,Eo)) ::- ER^^epal(Er), UC^^epal(Uc), EA^^epal(Ea),
	    DC^^epal(Dc), EP^^epal(Ep), EC^^epal(Ec), EO^^epal(Eo)
    ),
    (	epal_policy_information ::= []
        <:>
        epal(0)
    ),
    (	epal_definitions ::= []
        <:>
        epal(0)
    ),
    (	epal_rules ::= epal_rule^^ER
        <:>
        epal(E) ::- ER^^epal(E)
    ),
    (	epal_policy_doc ::= epal_policy_information, epal_definitions, epal_rules
        <:>
        epal(0)
    )
]).

epal_test :-
	epal_grammar(Rules),
	dctg:dctg_list_reconsult(Rules),
	test_epal,
	writeln('OK').

test_epal :-
    epal_tree(['ALLOW','Data User','TO PERFORM','Action','ON','Data Type','FOR','Purpose','IF','Condition','AND CARRY OUT','Obligation'], T ),
    T^^epal(E),writeln(E).

epal_tree( L, T ) :-
    dctg:epal_rules( T, L, [] ),
    !.
