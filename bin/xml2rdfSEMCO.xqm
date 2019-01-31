(:
 : -------------------------------------------------------------------------
 :
 : xml2rdfSEMCO.xqm - functions for transforming XML into RDF, using a semantic context.
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
    "xml2rdfUtil.xqm",
    "shaclWriter.xqm",
    "shaxLoader.xqm",
    "schemaLoader.xqm",
    "targetNamespaceTools.xqm",
    "typeGlobalizer.xqm",
    "util.xqm",
    "xsdComponentManager.xqm";

    (: "typeGlobalizer.xqm", :) 
    
declare namespace z="http://www.ttools.org/shax/ns/structure";
declare namespace zz="http://www.ttools.org/structure";
declare namespace shax="http://shax.org/ns/model";
declare namespace shaxerr="http://shax.org/ns/error";
declare namespace nons="http://shax.org/ns/nonamespace";
declare namespace xsd="http://www.w3.org/2001/XMLSchema";
declare namespace sc="http://www.rdfe.org/ns/model";

(:
 : ============================================================================
 :
 :     p u b l i c    f u n c t i o n s
 :
 : ============================================================================
 :)

(:~
 : Maps XML documents to RDF triples, controlled by a semantic context document.
 :
 : @param docs the XML documents to be mapped
 : @param semContext a semantic context document
 : @param format controls the output format of the triples
 : @param options for future use
 : @return RDF triples
 :) 
declare function f:xml2rdfSEMCO($docs as element()*, 
                                $semContext as element(),
                                $format as xs:string, 
                                $options as element(options))
        as item()* {
    let $semContext := trace( f:augmentSemContext($semContext) , 'SEM_CONTEXT: ')        
    let $docs_ :=
        if ($semContext//sc:initialization/sc:action/@id = 'addNodeNumbers') then i:addNodeNumbers($docs)
        else $docs
    return if ($format eq 'xmlaug') then $docs_ else
    
    let $namespaces := f:semcoNamespaceMap($semContext)
    let $xtriples :=
        <shax:triples>{
            $namespaces,
            $semContext/sc:resource[@isTopLevel eq 'true']
            ! f:resource2Xtriples($docs_, ., $namespaces, $semContext)
        }</shax:triples>     
    return 
        if ($format eq 'xtriples0') then $xtriples else
        
    let $xtriples := f:editXtriples($xtriples, ())        
    return
        if ($format eq 'xtriples') then $xtriples 
        else i:xtriples2Triples($xtriples)
};

(:~
 : Evaluates a resource template and returns xtriples describing all matching
 : resources.
 :)
declare function f:resource2Xtriples($docs as element()*,
                                     $resource as element(sc:resource),
                                     $nsmap as element(shax:nsmap),
                                     $semContext as element())
        as element(shax:triple)* {
    let $matchExpr := $resource/@assertedTargetNodes
    for $doc in $docs
    let $varContext := f:getSemcoVarContext($semContext, $doc)
    let $propertyMatchMap := trace( f:getPropertyMatchMap($doc, $semContext, $varContext) , 'PROPERTY_MATCH_MAP: ')
    let $resourceNodes := xquery:eval($matchExpr, $varContext)
    let $xtriples := $resourceNodes/f:resourceNode2Xtriples(., $varContext, $propertyMatchMap, $resource, $nsmap, (), ())
    return $xtriples
};

(:~
 : Maps a resource node to xtriples.
 :)
declare function f:resourceNode2Xtriples($rnode as node(),
                                         $varContext as map(*),
                                         $propertyMatchMap as map(*),
                                         $resource as element(sc:resource),
                                         $nsmap as element(shax:nsmap),
                                         $propertyOf as xs:string?,
                                         $propertyName as xs:string?)
        as element(shax:triple)* {
    let $semanticMap := $resource/root()
    let $currentVarContext := map:put($varContext, '', $rnode)
    let $iriExpr := $resource/@iri/i:addContextToExpr(., .., $currentVarContext)
    let $iri := xquery:eval($iriExpr, $currentVarContext)
    let $iriRep := concat('<', $iri, '>')
    let $nodeType := $resource/@type
    
    (: write property treating this resource node's IRI as object :)
    let $xtriplePropertyOf :=
        if (not($propertyOf)) then () else
        <shax:triple s="{$propertyOf}" p="{$propertyName}" o="{$iriRep}"/>
        
    (: write rdf:type property :)
    let $xtripleType :=    
        if (not($nodeType)) then () else
            <shax:triple s="{$iriRep}" p="rdf:type" o="{$nodeType}"/>
        
    let $valueMap := f:getResourceNodeValueMap($rnode, $resource, $currentVarContext)
    let $propertiesElem := $resource/sc:properties
    let $xtriples :=
        for $property in $resource/sc:property
        let $id := $property/@sc:id
        let $type := $property/@type
        let $modelId := $property/@modelID
        let $objectResourceModel := trace(
            if (not($modelId)) then () 
            else $semanticMap//sc:resource[@modelID eq $modelId] , concat('type=', $type, ' ; OBJECT_RESOURCE_MODE: '))
        let $propertyIri := $property/@iri
(:        
        let $valueExpr := $property/@value/i:addContextToExpr(.,$rnode, $currentVarContext)
        let $valueItems := xquery:eval($valueExpr, $currentVarContext)
:)        
        let $valueItems := $valueMap($id)
        let $valueAtoms := $valueItems[not(. instance of node())]
        let $valueNodes := $valueItems[. instance of node()]
        let $valueNodesSimple := $valueNodes[not(*) and not(@* except @shax:nodeNr)]
        let $valueNodesComplex := $valueNodes except $valueNodesSimple
        let $xtriplesValues := 
            for $value in ($valueAtoms, $valueNodesSimple/string(.))
            return
                <shax:triple s="{$iriRep}" p="{$propertyIri}">{
                    if ($type eq '#iri') then 
                        let $ovalue := concat('<', $value, '>')
                        return attribute o {$ovalue}
                    else (
                        attribute v {$value},
                        if (not($type)) then () else attribute type {$type}
                    )
                }</shax:triple>

        let $xtriplesRnodes :=
            for $rnode in $valueNodesComplex
            return
                f:resourceNode2Xtriples($rnode, $varContext, $propertyMatchMap, $objectResourceModel, $nsmap, $iriRep, $propertyIri)
        return (
            $xtriplePropertyOf,
            $xtripleType,
            $xtriplesValues,
            $xtriplesRnodes
        )
    let $xtriples2 :=
        if (not($propertiesElem)) then () else
        let $id := $propertiesElem/@id
        let $nodesRaw := 
            let $valuesAttr := $propertiesElem/@values
            let $valuesExpr := $valuesAttr/i:addContextToExpr(.,$rnode, $currentVarContext)
            return xquery:eval($valuesExpr, $currentVarContext)
        let $nodesToSkip := map:keys($valueMap) ! $valueMap(.) [. instance of node()]
        let $nodes := $nodesRaw except $nodesToSkip
        
        for $node in $nodes
        let $propertyElem := trace( f:getPropertyMatch($node, $semanticMap, $propertyMatchMap) , 'PROPERTY_ELEM:')
        where $propertyElem
        return
            f:resourcePropertyNode2Xtriples($iriRep, 
                                            $node, 
                                            $currentVarContext, 
                                            $propertyMatchMap,
                                            $propertyElem, 
                                            $nsmap)
        
    return (
        $xtriples,
        $xtriples2
    )
};

declare function f:getPropertyMatch($node as node(),
                                    $semanticMap as element(sc:semanticMap))
        as element(sc:property)? {
    if ($node/self::attribute(id)) then $semanticMap//sc:property[@sc:id eq 'n20']
    else ()
};

(:~
 : Maps a resource node to xtriples.
 :)
declare function f:resourcePropertyNode2Xtriples($subject as xs:string,
                                                 $pnode as node(),
                                                 $currentVarContext as map(*),
                                                 $propertyMatchMap as map(*),
                                                 $property as element(sc:property),
                                                 $nsmap as element(shax:nsmap))
        as element(shax:triple)* {
    let $semanticMap := $property/ancestor::sc:semanticMap
    let $id := $property/@sc:id
    let $type := $property/@type
    let $modelId := trace($property/@modelID , 'MODEL_ID: ')
    let $objectResourceModel :=
        if (not($modelId)) then () 
        else $semanticMap//sc:resource[@modelID eq $modelId]
    let $propertyIri := $property/@iri
        
    let $valueItems :=
        let $valueAttr := $property/@value
        return
            if (not($valueAttr)) then $pnode
            else
                let $namespaceContext := if ($pnode/self::element()) then $pnode else $pnode/..
                let $currentVarContext := map:put($currentVarContext, '', $pnode)
                let $valueExpr := $property/@value/i:addContextToExpr(.,$namespaceContext, $currentVarContext)
                return
                    xquery:eval($valueExpr, $currentVarContext)
        
    let $valueAtoms := $valueItems[not(. instance of node())]
    let $valueNodes := $valueItems[. instance of node()]
    let $valueNodesSimple := $valueNodes[not(*) and not(@* except @shax:nodeNr)]
    let $valueNodesComplex := $valueNodes except $valueNodesSimple
    let $xtriplesValues := 
            for $value in ($valueAtoms, $valueNodesSimple/string(.))
            return
                <shax:triple s="{$subject}" p="{$propertyIri}">{
                    if ($type eq '#iri') then 
                        let $ovalue := concat('<', $value, '>')
                        return attribute o {$ovalue}
                    else (
                        attribute v {$value},
                        if (not($type)) then () else attribute type {$type}
                    )
                }</shax:triple>

    let $xtriplesRnodes :=
        for $rnode in $valueNodesComplex
        return
            f:resourceNode2Xtriples($rnode, $currentVarContext, $propertyMatchMap, $objectResourceModel, $nsmap, $subject, $propertyIri)
    return (
        $xtriplesValues,
        $xtriplesRnodes
    )
};

declare function f:getResourceNodeValueMap($rnode as node(), 
                                           $resource as element(sc:resource),
                                           $currentVarContext as map(*))
            as map(*) {
    let $valueMap :=
        map:merge(
            for $prop in $resource/sc:property
            let $id := $prop/@sc:id
            let $valueAttr := $prop/@value
            let $valueExpr := $valueAttr/i:addContextToExpr(.,$rnode, $currentVarContext)
            let $valueItems := xquery:eval($valueExpr, $currentVarContext)
            return map:entry($id, $valueItems)
        )
    return $valueMap
};

declare function f:semcoNamespaceMap($semContext as element())
        as element(shax:nsmap) {
    let $nsmap :=
        <shax:nsmap>{
            for $ns in $semContext/sc:namespace
            let $prefix := $ns/@prefix
            let $uri := $ns/@iri
            return
               <shax:ns uri="{$uri}" prefix="{$prefix}"/> 
        }</shax:nsmap>
    return $nsmap        
};

(:~
 : Augments a semantic context by adding information.
 :
 : Currently, only ID attributes are added.
 :
 : @param semContext the semantic context to be augmented
 : @return the augmented semantic context
 :) 
declare function f:augmentSemContext($semContext as element())
        as item()* {
    copy $semContext_ := $semContext
    modify
        for $elem at $pos in $semContext_//(sc:resource, sc:property, sc:properties) return
            insert node attribute sc:id {concat('n', $pos)} into $elem
    return $semContext_        
};

(:~
 : Constructs the context of variable bindings and function item bindings
 : used when evaluating XQuery expressions.
 :)
declare function f:getSemcoVarContext($semContext as element(), $doc as element())
        as map(*)? {
    map:merge((
        map:entry('', $doc),

        for $var in $semContext//sc:varContext/sc:var
        let $name := $var/@name
        let $value := $var/@value
        let $value := 
            if (exists($value)) then $value
            else
                let $valueExpr := $var/@valueExpr
                return xquery:eval($valueExpr, map{'': $doc})
        return
            map:entry($name, $value)
        ,
        for $func in $semContext//sc:functionsContext/sc:function
        let $name := $func/@name
        let $code := $func/string()
        let $functionItem := xquery:eval($code, map{'': $doc})
        return
            map:entry($name, $functionItem)
    ))
};

(:~
 : Constructs a map which associates property element IDs to
 : a map providing the property element, the nodes matched in
 : the context of $doc, the value of the @assertedTargetNodes attribute 
 : and the match propriety.
 :
 : @param doc the document in the context of which matches are analyzed
 : @param semanticMap the semantic map
 : @param varContexgt the variables context
 : @return the map
 :)
declare function f:getPropertyMatchMap($doc as element(),
                                       $semanticMap as element(sc:semanticMap),
                                       $varContext as map(*))
        as map(*) {
    let $currentVarContext := map:put($varContext, '', $doc)
    return
    
    map:merge(
        for $propertyElem in $semanticMap/sc:property[@match]
        let $propertyId := trace($propertyElem/@sc:id , 'PROPERTY_ID: ')
        let $match := $propertyElem/@match
        let $matchExpr :=
            if (starts-with($match, '/')) then $match else concat('//', $match)
            ! i:addContextToExpr(., $propertyElem, $currentVarContext)
        let $nodes := xquery:eval($matchExpr, $currentVarContext)
        let $refmap :=
            map:merge((
                map:entry('propertyElem', $propertyElem),
                map:entry('nodes', $nodes),
                map:entry('matchAtt', $match/string()),
                map:entry('priority', 0)
            ))
        return
            map:entry($propertyId, $refmap)
    )
            
};        

(:~
 : Determines the property element matching a given node.
 :
 : @param node the node for which a property element is sought
 : @param sementicContext the semantic context
 : @param propertyMatchMap a map providing data which enable the matching
 : @return a property element, if one is matched, otherwise the empty sequence
 :)
declare function f:getPropertyMatch($node as node(), 
                                    $semanticMap as element(sc:semanticMap),
                                    $propertyMatchMap as map(*))
        as element(sc:property)? {
    let $propertyIds := map:keys($propertyMatchMap)
    let $matchIds :=
        for $propertyId in $propertyIds
        let $nodes := $propertyMatchMap($propertyId)('nodes')
        where $nodes intersect ($node)
        return $propertyId
    let $matchId :=
        if (empty($matchIds)) then ()
        else if (count($matchIds) gt 1) then $matchIds[1] (: TO.DO - conflict resolution :)
        else $matchIds[1]
    return
        $matchId ! $propertyMatchMap(.)('propertyElem')
};        
        
