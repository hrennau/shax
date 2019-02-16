(:
 : -------------------------------------------------------------------------
 :
 : rdfeValidator.xqm - a module for validating rdfe documents
 :
 : -------------------------------------------------------------------------
 :)
 
module namespace f="http://www.ttools.org/shax/ns/xquery-functions";

import module namespace tt="http://www.ttools.org/xquery-functions" at 
    "tt/_request.xqm",
    "tt/_reportAssistent.xqm",
    "tt/_errorAssistent.xqm",
    "tt/_log.xqm",
    "tt/_nameFilter.xqm",
    "tt/_pcollection.xqm";    

import module namespace ref="http://www.rdfe.org/ns/xquery-functions" at 
    "rdfeConstants.xqm";    

import module namespace i="http://www.ttools.org/shax/ns/xquery-functions" at
    "constants.xqm",
    "rdfeTargetMatcher.xqm",
    "shaclWriter.xqm",
    "util.xqm";
    
declare namespace z="http://www.ttools.org/shax/ns/structure";
declare namespace shax="http://shax.org/ns/model";
declare namespace xsd="http://www.w3.org/2001/XMLSchema";
declare namespace re="http://www.rdfe.org/ns/model";

(:~
 : Validates semantic map documents.
 :
 : @param semaps semantic map documents
 : @return in case of errors a check report, empty sequence otherwise
 :)
declare function f:validateRdfe($semaps as element(re:semanticMap)+,
                                $docs as element()+)
        as element(errors)? {
    let $errors_xsd := f:validateRdfe_xsd($semaps)
    return
        if ($errors_xsd) then $errors_xsd else
        
    let $expressions := (
        $semaps/@targetAssertion,
        $semaps/re:targetAssertion/(@expr, text())[string()][1],
        $semaps//re:context/re:var/(@value, text[matches(., '\S')]),
        $semaps//re:resource/(
            @assertedTargetNodes[string(.)],
            @iri[not(. eq '#bnode')]
        ),
        $semaps//re:property/(
            @value
        ),
        $semaps//re:valueItemCase/(
            @test,
            @value
        ),
        ()
    )
    let $errors_invalid_expr :=    
        for $e in $expressions
        let $semap := $e/ancestor::re:semanticMap
        let $e_ := f:validateRdfe_augmentExpression($e, $semap)
        return (
            try {xquery:parse($e_)}
            catch * {
                let $uri := $e/ancestor::re:semanticMap/base-uri(.)
                let $etext := string($e)
                let $location := f:expressionLocation($e)       
                return
                    <error code="INVALID_XQUERY_EXPRESSION">{
                        <expression text="{$etext}"/>,
                        <file uri="{$uri}"/>,
                        <location path="{$location}"/>,
                        <errorDetails subCode="{$err:code}" description="{$err:description}"/>
                        , $e_
                    }</error>
            }
            )[self::error]
    let $error_nomatch :=            
        if (some $doc in $docs, $semap in $semaps satisfies f:semapAppliesToDocument($semap, $doc))
        then () else
            <error code="DOCS_NOT_TARGETS_OF_SEMAPS">{
                <errorDetails 
                      description="No input documents found which meet the target constraint of a semantic map."/>,
                <tip>Please check the target constraints: 
                semanticMap/(@targetName, @targetNamespace, @targetAssertion, targetAssertion)</tip> 
             }</error>
    let $errors := 
        ($errors_invalid_expr, $error_nomatch)             
    return
        if (empty($errors)) then ()
        else
            <errors count="{count($errors)}">{$errors}</errors>
            
};

(:~
 : Validates semantic map documents against XSDs.
 :
 : @param semaps semantic map documents
 : @return in case of errors a check report, empty sequence otherwise
 :)
declare function f:validateRdfe_xsd($semaps as element(re:semanticMap)+)
        as element(errors)? {
    let $xsd := $f:URI_XSD_RDFE ! doc(.)
    let $mapReports :=
        for $semap in $semaps
        let $raw := validate:xsd-info($semap, $xsd)
        return
            if (empty($raw)) then ()
            else
                <xsdErrors docUri="{$semap/base-uri(.)}" countErrors="{count($raw)}">{
                    $raw ! <msg>{.}</msg>
                }</xsdErrors>
    return
        if (empty($mapReports)) then ()
        else
            <errors>{$mapReports}</errors>
};

declare function f:validateRdfe_augmentExpression($expr as node(),
                                                  $semap as element(re:semanticMap))
        as xs:string {
    let $nsContext:= $expr/ancestor-or-self::*[1]        
    let $nsDecl := (
        'declare namespace rdfe="' || $ref:BUILTIN_NAMESPACE_BINDINGS?rdfe || '";',
        $nsContext/in-scope-prefixes(.)[not(. = ('xml', 'rdfe'))] 
            ! concat('declare namespace ', ., '="', namespace-uri-for-prefix(., $nsContext), '";')        
    )
    let $varNames := (
        'rdfe:docs',
        'rdfe:valueItem',
        'rdfe:self',
        'rdfe:nodeNumber',
        $semap//re:context/(re:var, re:function, re:fun)/@name
    )
    let $varDecls :=
        $varNames ! concat('declare variable $', ., ' external;')
    return
        string-join(($nsDecl, $varDecls, $expr), '&#xA;')
};

(:~
 : Returns a path-like string expressing the location of
 : a node containing an expression. Example:
 :    /semanticMap/context/var[name=tns]/@value
 :    /semanticMap/context/var[name=tns]/text() 
 :    /semanticMap/context/resource[modelID=element]/courvar[name=tns] 
 : attribute or a text node.
 :
 : @param exprNode a node containing an expression
 : @return location string
 :)
declare function f:expressionLocation($exprNode as node())
        as xs:string {
    let $exprNodeName :=
        typeswitch($exprNode)
        case attribute() return '@' || $exprNode/local-name(.)
        case text() return 'text()'
        case element() return $exprNode/local-name(.)
        default return error()  
    let $parentPath :=
        for $anc in $exprNode/ancestor::*
        let $post :=
            typeswitch($anc)
            case element(re:var) return $anc/@name/concat("name='",.,"'")
            case element(re:resource) return $anc/@modelID/concat("modelID=", ., "'")
            case element(re:property) return $anc/@iri/concat("iri='", ., "'")
            case element(re:valueItemCase) | element(re:targetAssertion) return
                1 + $anc/count(preceding-sibling::*[local-name(.) eq $anc/local-name(.)])
            default return ()
        return
            concat($anc/local-name(.), $post ! concat('[', ., ']'))
    return
        string-join(('', $parentPath, $exprNodeName), '/') 
        
};        

        
