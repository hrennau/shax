(:
 : -------------------------------------------------------------------------
 :
 : rdfeSepro.xqm - a module for processing semantic profiles
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

import module namespace i="http://www.ttools.org/shax/ns/xquery-functions" at
    "constants.xqm",
    "shaclWriter.xqm",
    "util.xqm";
    
declare namespace z="http://www.ttools.org/shax/ns/structure";
declare namespace shax="http://shax.org/ns/model";
declare namespace xsd="http://www.w3.org/2001/XMLSchema";
declare namespace re="http://www.rdfe.org/ns/model";

(:~
 : Compiles a semantic profile into a format which is convenient for
 : editing sementic maps accordingly.
 :
 : Example output:
 : <re:semanticProfile xmlns:re="http://www.rdfe.org/ns/model" 
 :                     format="compiled" 
 :                     assertedTargetNodesDefault="#none">
 :    <re:semapProfile iri="http://rote-liste.de/rdfe.gibook/1/" 
 :                     assertedTargetNodesDefault="#none">
 :       <re:rmodelProfile rmodelID="drug">
 :          <re:assertedTargetNodes expr="//gb:meta-daten"/>
 :       </re:rmodelProfile>
 :    </re:semapProfile>
 : </re:semanticProfile>
 :
 : @param sepro a semantic profile
 : @return compiled semantic profile
 :) 
declare function f:compileSemanticProfile($sepro as element(re:semanticProfile))
        as element(*) {

    let $semapProfiles :=    
        for $elemsForSemap in $sepro/(* except re:importSemanticMaps)
        group by $semapIri := $elemsForSemap/@semanticMapIri
        let $semapLevelSettings :=
            for $elem in $elemsForSemap[not(@rmodelID)]
            return
                typeswitch($elem)
                case element(re:assertedTargetNodesDefault) return
                    attribute assertedTargetNodesDefault {$elem/@value}
                default return error()
        let $rmodelProfiles := 
            for $elemsForRmodel in $elemsForSemap[@rmodelID]
            group by $modelId := $elemsForRmodel/@rmodelID
            let $rmodelSettings :=
                for $elem in $elemsForRmodel
                return
                    typeswitch($elem)
                    case element(re:assertedTargetNodes) return
                        <re:assertedTargetNodes expr="{$elem/@expr}"/>
                    default return error()
            let $rmodelSettingsAtts := $rmodelSettings/self::attribute()
            let $rmodelSettingsElems := $rmodelSettings except $rmodelSettingsAtts
            where string($modelId)
            return
                <re:rmodelProfile rmodelID="{$modelId}">{
                    $rmodelSettingsAtts,
                    $rmodelSettingsElems
                }</re:rmodelProfile>
        return
            <re:semapProfile iri="{$semapIri}">{
                $semapLevelSettings,
                $rmodelProfiles
            }</re:semapProfile>
    return
        <re:semanticProfile format="compiled">{
            $sepro/@assertedTargetNodesDefault,
            $semapProfiles
        }</re:semanticProfile>
};      

declare function f:applySemanticProfile($sepro as element(re:semanticProfile), $semaps as element(re:semanticMap)+)
        as element(re:semanticMap) {
    for $semap in $semaps
    return
        f:applySemanticProfileRC($semap, $sepro, $sepro)
};

(:
declare function f:applySemanticProfileRC($n as node(), $sepro as element(re:semanticProfile))
        as node()? {
    typeswitch($n)
    
    case document-node() return
        document {$n/node() ! f:applySemanticProfileRC(., $sepro)}
        
    case element() return
        element {node-name($n)} {
            in-scope-prefixes($n) ! namespace {.} {namespace-uri-for-prefix(., $n)},
            $n/@* ! f:applySemanticProfileRC(., $sepro),
            $n/node() ! f:applySemanticProfileRC(., $sepro)
        }

    case element(re:resource) return
        let $rmodelID := $n/ancestor::re:resource/@modelID
        let $semapIRI := $n/ancestor::re:semanticMap/@iri
        
        let $semapProfile := $sepro//re:semapProfile[@iri eq $semapIRI]
        let $rmodelProfile := $semapProfile/re:rmodelProfile[@rmodelID eq $rmodelID]

        let $newAssertedTargetNodes :=
            let $profile_assertedTargetNodes := $rmodelProfile/re:assertedTargetNodes
            return
                if ($profile_assertedTargetNodes) then 
                    let $exprAtt := $profile_assertedTargetNodes/@expr
                    let $exprText := $profile_assertedTargetNodes/text()
                    return
                        if ($exprAtt) then attribute {node-name($n)} {$exprAtt}
                        else if ($exprText) then <re:assertedTargetNodes>{$exprText}</re:assertedTargetNodes>
                else ()
        let $assertedTargetNodesDefault :=
            if ($newAssertedTargetNodes) then ()
            else
                ($rmodelProfile, $semapProfile)[1]
                /ancestor-or-self::*[@assertedTargetNodesDefault][1]/@assertedTargetNodesDefault
        
        let $forAtts := (
            $n/(@* except @assertedTargetNodes) ! f:applySemanticProfileRC(., $sepro),
            @assertedTargetNodes[not($newAssertedTargetNodes) and not(@assertedTargetNodesDefault eq '#null')],
            $newAssertedTargetNodes
        )
        let $forElems := $n/node() ! f:applySemanticProfileRC(., $sepro)
        let $atts := ($forAtts, $forElems)[self::attribute()]
        let $elems := ($forAtts, $forElems) except $atts
        return
            element {node-name($n)} {
                $atts,
                $elems
            }
            
(:            
    case attribute(assertedTargetNodes) return
        let $rmodelID := $n/ancestor::re:resource/@modelID
        let $semapIRI := $n/ancestor::re:semanticMap/@iri
        
        let $semapProfile := $sepro//re:semapProfile[@iri eq $semapIRI]
        let $rmodelProfile := $semapProfile/re:rmodelProfile[@rmodelID eq $rmodelID]
        let $profile_assertedTargetNodes := $rmodelProfile/re:assertedTargetNodes
        return
            if ($profile_assertedTargetNodes) then 
                let $exprAtt := $profile_assertedTargetNodes/@expr
                let $exprText := $profile_assertedTargetNodes/text()
                return
                    if ($exprAtt) then attribute {node-name($n)} {$exprAtt}
                    else if ($exprText) then <re:assertedTargetNodes>{$exprText}</re:assertedTargetNodes>

            else 
                let $default := ($rmodelProfile, $semapProfile)[1]/ancestor-or-self::*[@assertedTargetNodesDefault][1]/@assertedTargetNodesDefault]
                return
                    if ($default eq '#none') then () else $n
:)                    
    default return $n        
};
:)

declare function f:applySemanticProfileRC($n as node(), 
                                          $sepro as element(re:semanticProfile),
                                          $context as element())
        as node()? {
    typeswitch($n)
    
    case document-node() return
        document {$n/node() ! f:applySemanticProfileRC(., $sepro, $context)}
        
    case element(re:semanticMap) return
        let $semapIRI := $n/@iri
        let $context :=
            ($context/descendant-or-self::re:semapProfile[@iri eq $semapIRI], $context)[1]
        return f:applySemanticProfileRC_copyElem($n, $sepro, $context)
        
    case element(re:resource) return
        let $rmodelID := $n/@modelID
        let $rmodelProfile := $context/descendant-or-self::re:rmodelProfile[@rmodelID eq $rmodelID]
        let $context := ($rmodelProfile, $context)[1]
        
        let $newAssertedTargetNodes :=
            let $profile_assertedTargetNodes := $rmodelProfile/re:assertedTargetNodes
            return
                if ($profile_assertedTargetNodes) then 
                    let $exprAtt := $profile_assertedTargetNodes/@expr
                    let $exprText := $profile_assertedTargetNodes/text()
                    return
                        if ($exprAtt) then attribute assertedTargetNodes {$exprAtt}
                        else if ($exprText) then <re:assertedTargetNodes>{$exprText}</re:assertedTargetNodes>
                else ()
        let $assertedTargetNodesDefault := trace(
            if ($newAssertedTargetNodes) then ()
            else
                $context/ancestor-or-self::*[@assertedTargetNodesDefault][1]/@assertedTargetNodesDefault
        , 'ASSERTED_TARGET_NODES_DEFAULT: ')
        let $editAtts := (
            $n/(@* except @assertedTargetNodes) ! f:applySemanticProfileRC(., $sepro, $context),
            $n/@assertedTargetNodes[not($newAssertedTargetNodes) and not($assertedTargetNodesDefault eq '#none')],
            $newAssertedTargetNodes
        )
        let $editElems := $n/node() ! f:applySemanticProfileRC(., $sepro, $context)
        let $atts := ($editAtts, $editElems)[self::attribute()]
        let $elems := ($editAtts, $editElems) except $atts
        return
            element {node-name($n)} {$atts, $elems}

    case element() return 
        f:applySemanticProfileRC_copyElem($n, $sepro, $context)

    default return $n        
};

declare function f:applySemanticProfileRC_copyElem($e as element(), 
                                                   $sepro as element(re:semanticProfile),
                                                   $context as element())
        as element() {
    element {node-name($e)} {
        in-scope-prefixes($e) ! namespace {.} {namespace-uri-for-prefix(., $e)},    
        $e/@* ! f:applySemanticProfileRC(., $sepro, $context),
        $e/node() ! f:applySemanticProfileRC(., $sepro, $context)
    }
};    
