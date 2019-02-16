(:
 : -------------------------------------------------------------------------
 :
 : rdfeTargetMatcher.xqm - functions checking if documents and nodes match target constraints of a semantic map
 :
 : -------------------------------------------------------------------------
 :)
 
module namespace f="http://www.ttools.org/shax/ns/xquery-functions";

import module namespace ref="http://www.rdfe.org/ns/xquery-functions" at 
    "rdfeConstants.xqm";    

import module namespace i="http://www.ttools.org/shax/ns/xquery-functions" at
    "constants.xqm",
    "util.xqm"
;
    
declare namespace xsd="http://www.w3.org/2001/XMLSchema";
declare namespace re="http://www.rdfe.org/ns/model";

(:~
 : Returns true if a given semantic map can be applied to a given document.
 :
 : @param semap a semantic map
 : @param doc a document
 : @return true if the semantic map complements the document, false otherwise
 :)
declare function f:semapAppliesToDocument($semap as element(), $doc as element())
        as xs:boolean {
    let $namespaceConstraint := $semap/@targetNamespace
    let $nameConstraint := $semap/@targetName
    let $assertions := $semap/
        (@targetAssertion, re:targetAssertion/(@expr, text())[string()][1])
    return
        f:matchesNameConstraints($doc, $namespaceConstraint, $nameConstraint) 
        and f:matchesAssertions($doc, $assertions)
};        

(:~
 : Returns true if a given resource model can be applied to a given XML node.
 :
 : @param rmodel the resource model
 : @param rnode the XML node
 : @return true or false
 :)
declare function f:rmodelAppliesToNode($rmodel as element(re:resource), $rnode as node())
        as xs:boolean {
    let $namespaceConstraint := $rmodel/@targetNodeNamespace
    let $nameConstraint := $rmodel/@targetNodeName
    let $assertions := $rmodel/
        (@targetNodeAssertion, re:targetNodeAssertion/(@expr, text())[string()][1])
    return
        f:matchesNameConstraints($rnode, $namespaceConstraint, $nameConstraint) 
        and f:matchesAssertions($rnode, $assertions)
};

(:~
 : Returns true if a node matches given patterns on namespace and name.
 : More precisely, for namespace and name at least one of the patterns must be matched.
 :)
declare function f:matchesNameConstraints($n as node(), $namespace as xs:string?,  $name as xs:string?)
        as xs:boolean {
    (: node name and namespace :)
    let $nname := $n/local-name(.)
    let $nnamespace := $n/namespace-uri(.)

    (: constraints on namespace and name :)
    let $namespaces := $namespace[string()] ! tokenize(.)
    let $names := $name[string()] ! tokenize(.)

    (: check namespace :)
    let $namespaceOk :=
        if (empty($namespaces)) then true()
        else if (not($nnamespace)) then
            if ($namespaces = '#null') then true()
            else false()
        else        
            let $nsFilters :=
                $namespaces !
                replace(., '\*', '.*')!
                replace(., '\?', '.') !
                concat('^', ., '$')
            return
                some $filter in $nsFilters satisfies matches($nnamespace, $filter, 'i')

    (: check name :)
    let $nameOk :=
        if (empty($names)) then true()
        else
            let $nFilters :=
                $names !
                replace(., '\*', '.*') !
                replace(., '\?', '.') !
                concat('^', ., '$')
            return
                some $filter in $nFilters satisfies matches($nname, $filter, 'i')
    return $namespaceOk and $nameOk                
};        

(:~
 : Returns true if a node satisfies all assertions.
 :
 : @param n a node
 : @param assertions nodes each one of which contains an assertion
 : @return true if the node satisfies all assertions
 :)
declare function f:matchesAssertions($n as node(), $assertions as node()*)
        as xs:boolean {
    every $ass in $assertions satisfies
        let $expr := concat('boolean(', $ass, ')')
        let $namespaceContext := $ass/ancestor-or-self::*[1]
        return f:xquery($expr, $namespaceContext, (), (), $n)
};        


