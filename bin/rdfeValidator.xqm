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
    "rdfeLoaderOld.xqm",
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
declare function f:validateRdfe($semaps as element(re:semanticMap)+)
        as element(errors)? {
    let $expressions := (
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
    let $errors :=    
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
    return
        if (empty($errors)) then ()
        else
            <errors count="{count($errors)}">{
                $errors
            }</errors>
            
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

        
