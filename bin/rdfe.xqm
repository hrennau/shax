(:
 : -------------------------------------------------------------------------
 :
 : rdfe.xqm - functions for transforming XML into RDF, using semantic maps.
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
    "rdfeLoader.xqm",
    "rdfeSepro.xqm",
    "rdfeValidator.xqm",
    "xml2rdfUtil.xqm",
    "shaclWriter.xqm",
    "shaxLoader.xqm",
    "schemaLoader.xqm",
    "rdfeTargetMatcher.xqm",
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
declare namespace re="http://www.rdfe.org/ns/model";
declare namespace rf="http://www.rdfe.org/ns/xquery-functions";

declare variable $f:SWITCH_USE_AGGREGATE_QUERY := 0;

(:
 : ============================================================================
 :
 :     p u b l i c    f u n c t i o n s
 :
 : ============================================================================
 :)

(:~
 : Maps XML documents to RDF triples, controlled by semantic map documents.
 :
 : @param docs the XML documents to be mapped
 : @param semap a semantic map document
 : @param format controls the output format of the triples
 : @param options for future use
 : @return RDF triples
 :) 
declare function f:rdfe($docs as element()*, 
                        $semaps as element(re:semanticMap)*,
                        $sepro as element(re:semanticProfile)?,
                        $format as xs:string, 
                        $options as element(options))
        as item()* {
(:        
    let $comp := $docs/(xs:simpleType, xs:complexType)[@name eq 'AgreementsType']
    let $nodeNumber := trace(f:nodeNumber($comp) , '###NODE_NUMBER: ')
:)    
    let $r1 := $options/@r1/xs:integer(.)        
    let $r2 := $options/@r2/xs:integer(.)
    let $semapsAndSepro := f:loadRdfe($semaps, $sepro)
    let $DUMMY := trace(count($semapsAndSepro) , 'COUNT_SEMAPS_SEPRO: ')
    let $seproRaw := $semapsAndSepro[self::re:semanticProfile]
    let $sepro := f:compileSemanticProfile($seproRaw)
    let $semaps := $semapsAndSepro[self::re:semanticMap]
    let $errors := f:validateRdfe($semaps, $docs)
    
    let $semaps := 
        if (not($sepro)) then $semaps
        else f:applySemanticProfile($sepro, $semaps)
    
    return if ($errors) then $errors else
        
    let $semaps := f:augmentSemap($semaps)  
    return if ($format eq 'xmlaug') then $docs else
    
    let $_INFO := trace((), 'Going to construct dynamic document context ...')
    let $collectedDynContext := f:getCollectedSemapDynContext($semaps, $docs, ())
    let $_INFO := trace((), 'Going to construct RNODE dict ...')
    let $rnodeDict := f:getRnodeDict($semaps, $docs, $collectedDynContext)  
    let $_INFO := trace(map:keys($rnodeDict) => count(), 'RNODE dict constructed, #entries: ')  
    let $namespaces := f:semapNamespaceMap($semaps)
    
    let $DUMMY := file:write('DEBUG_SEPRO.xml', $sepro)
    let $DUMMY := for $s at $pos in $semaps return file:write(concat('DEBUG_SEMAP', $pos, '.xml'), $s)
(:
    let $DUMMY := for $semap at $pos in $semaps return file:write(concat('DEBUG_SEMAP-', $pos, '.xml'), $semap)
:)    
(:
    let $DUMMY := trace($semaps/base-uri(.), 'SEM_BASE_URIS: ')
    let $DUMMY := trace($docs/base-uri(.), 'DOC_BASE_URIS: ')
    let $DUMMY := for $doc at $pos in $docs return file:write(concat('DEBUG_DOC-', $pos, '.xml'), $doc)    
    let $DUMMY := for $semap at $pos in $semaps return file:write(concat('DEBUG_SEMAP-', $pos, '.xml'), $semap)
    let $DUMMY := file:write('DEBUG_RNODE_DICT.xml', f:DEBUG_rnodeDict($rnodeDict))
    let $DUMMY := file:write('DEBUG_COLLECTED_DYN_CONTEXT.xml', f:DEBUG_collectedSemapDynContext($collectedDynContext))
:)    

    let $xtriplesAndTripleReqs :=
        for $key at $pos in map:keys($rnodeDict)
        return if ($pos lt $r1 or $pos gt $r2) then () else        
            let $_INFO := if ($pos mod 1000) then () else trace($pos, 'Process RESOURCE#')
            return f:constructResource($key, (), $rnodeDict, $semaps, $collectedDynContext)
    let $xtriples := $xtriplesAndTripleReqs[. instance of node()]
    let $tripleReqs := $xtriplesAndTripleReqs[. instance of map(*) or . instance of array(*)]
    let $_INFO := trace(count($tripleReqs), '#COUNT_RDESC_REQUESTS: ')
    (: let $_DEBUG := trace(f:DEBUG_reportTripleReqs($xtriplesAndTripleReqs), 'TRIPLE_REQS: ') :)
    let $furtherXtriples := f:processTripleRequests($tripleReqs, $rnodeDict, $semaps, $collectedDynContext)
    let $xtriples1 := ($xtriples, $furtherXtriples)
    let $xtriples2 := <shax:triples>{$namespaces, $xtriples1}</shax:triples>    
    return 
        if ($format eq 'xtriples0') then $xtriples2 
        else                
            let $_INFO := trace((), 'Going to edit xtriples ...')
            let $xtriples3 := f:editXtriples($xtriples2, ())   
            let $_INFO := trace(count($xtriples3/*), 'xtriples edited, #triples:')
            return
                if ($format eq 'xtriples') then $xtriples3 
                else i:xtriples2Triples($xtriples3)
};

(:~
 : Augments a set of semantic maps by adding information.
 : Modifications:
 : - root elements receive @iri and/or @xml:base, if they do not yet have these attributes
 : - property models receive an @re:id
 :
 : @param semap the semantic map to be augmented
 : @return the augmented semantic map
 :) 
declare function f:augmentSemap($semaps as element()+)
        as item()* {
    for $semap in $semaps    
    let $semap1 :=
        copy $semap_ := $semap
        modify (
            $semap_[not(@iri)]/(insert node attribute iri {$semap/base-uri(.)} into .),
            $semap_[not(@xml:base)]/(insert node attribute xml:base {$semap/base-uri(.)} into .),
            for $r at $posR in $semap_//re:resource
            return
                for $p at $posP in $r/re:property
                return insert node attribute re:id {'r' || $posR || 'p' || $posP} into $p
        )
        return $semap_
    let $semap2 :=
        copy $semap_ := $semap1
        modify 
            for $r in $semap_//re:resource
            let $aggregateQuery := f:getAggregateQueryForResourceModel($r)
            return
                insert node <re:aggregateQuery>{$aggregateQuery}</re:aggregateQuery> as first into $r
        return $semap_                
    return $semap2                
};

declare function f:getAggregateQueryForResourceModel($rmodel as element(re:resource))
        as xs:string {
(:        
    (
    'map:merge((',
    $rmodel/re:property ! ("map:entry('" || @re:id || "', " || @value ||"),"),
    '()))'
    ) => string-join('&#xA;')
:)    
    (
    'map{',
    let $countp := count($rmodel/re:property)
    for $p at $pos in $rmodel/re:property
    let $delim := if ($pos lt $countp) then ', ' else ()
    return "'" || $p/@re:id || "': (" || $p/@value || ')' || $delim,
    '}'
    ) => string-join('&#xA;')
};

(:
(:~
 : Augments input documents.
 : Currently, only @shax:nodeNr attributes are added.
 :
 : @param docs the documents to be augmented 
 : @param semap the semantic map in use
 : @return the augmented documents
 :) 
declare function f:augmentDocs($docs as element()*)
        as item()* {
    for $doc in $docs return i:addNodeNumbers($doc)
};
:)

(:~
 : Constructs a namespace map to be added to xtriples.
 :
 : @param semap a namespace map
 : @return a namespace map
 :)
declare function f:semapNamespaceMap($semaps as element()+)
        as element(shax:nsmap) {
            
    let $builtinPrefixes := map:keys($ref:BUILTIN_NAMESPACE_BINDINGS)
    return
    
    <shax:nsmap>{
        for $prefix in $builtinPrefixes
        let $iri := $ref:BUILTIN_NAMESPACE_BINDINGS($prefix)
        return <shax:ns uri="{$iri}" prefix="{$prefix}"/>
        ,
        for $ns in $semaps/re:namespace[not(@prefix = $builtinPrefixes)]
        group by $uri := $ns/@iri
        let $prefix := $ns[1]/@prefix
        return <shax:ns uri="{$uri}" prefix="{$prefix}"/> 
    }</shax:nsmap>
};

(:~
 : Creates a map providing a distinct evaluation context for each combination
 : of document URI and semap IRI. For each document, only those semantic
 : maps are considered which have a target constraint satisfied by the document.
 :
 : When there is not yet a current collected context, the documents
 : ($docs) are also stored in the map, using the empty string as keys.
 :
 : Map structure:
 :
 : '' => $docs
 : docUri
 : . semapIRI
 : . . varName => varValue
 :
 : Note that the semap IRI is not a file locator (like the document URI), but 
 : an abstract resource identifier.
 :)
declare function f:getCollectedSemapDynContext($semaps as element()+, 
                                               $docs as element()+, 
                                               $currentCollectedContext as map(*)?)
        as map(*)? {
    let $newCollectedContext :=    
        map:merge((   
            (: the set of initial input documents is stored with key '' :)
            if (exists($currentCollectedContext)) then () else map:entry('', $docs),
        
            for $doc in $docs
            let $docUri := $doc/base-uri(.)
            let $contextEntries :=
                map:merge(
                    for $semap in $semaps
                    let $semapIri := i:semapIriForNode($semap)
                    let $dynContext := f:getSemapDynContext($semap, $doc, $docs)
                    where f:semapAppliesToDocument($semap, $doc)
                    return
                        (: store context, using semap IRI as key :)
                        map:entry($semapIri, $dynContext)
                )
            return
                map:entry($docUri, $contextEntries)
        ))
    
    return
        map:merge(($newCollectedContext, $currentCollectedContext))
};

(:~
 : Constructs the context of variable bindings and function item bindings
 : used when evaluating XQuery expressions.
 :)
declare function f:getSemapDynContext($semap as element(), 
                                      $doc as element(), 
                                      $docs as element()+)
        as map(*)? {
    let $context := map{}
    let $contextConstructors := $semap//re:context/*
    return 
        if (empty($contextConstructors)) then $context
        else f:getSemapDynContextRC($context, $contextConstructors, $doc, $docs)
};

(:~
 : Recursive helper function of 'f:getSemapDynContext'.
 :)
declare function f:getSemapDynContextRC($context as map(*), 
                                        $contextConstructors as element()*, 
                                        $doc as element(),
                                        $docs as element()+)
        as map(*)? {
    let $head := head($contextConstructors)
    let $tail := tail($contextConstructors)
    let $name := $head/@name
    let $value :=
        if ($head/self::re:var) then
            let $valueExpr := $head/(@value, text())[1]
            return f:xquery($valueExpr, $head, $context, $docs, $doc)
        else if ($head/(self::re:fun, self::re:function)) then
            let $params := $head/@params/tokenize(normalize-space(.), ',\s?')
            let $returnType := $head/@as/string()
            let $paramsText := $params ! concat('$', .) => string-join(', ')
            let $returnTypeText := $returnType ! concat('as ', ., ' ')            
            let $code := $head/(@code, text() => string-join(''))[1]
            let $functionText := concat('function(', $paramsText, ') ', $returnTypeText, '{', $code, '}')
            return f:xquery($functionText, $head, $context, $docs, $doc)            
    let $entry := map:entry($name, $value)
    let $newContext := map:merge(($context, $entry))
    return
        if (not($tail)) then $newContext 
        else f:getSemapDynContextRC($newContext, $tail, $doc, $docs)
};

(:~
 : Builds the initial version of a resource node dictionary.
 : Structure:
 :
 : nodeID
 : . 'rnode' => node
 : . 'rmodels'
 : . . rmodelID
 : . . . 'rmodel' => rmodel element
 : . . . 'iri' => IRI string
 :
 :) 
declare function f:getRnodeDict($semaps as element()+,
                                $docs as element()+,
                                $collectedDynContext as map(*))
        as map(*) {
    map:merge(        
        for $semap in $semaps
        for $resource in $semap//re:resource[@assertedTargetNodes]
        let $rmodelId := $resource/@modelID
        let $iriExpr := $resource/@iri/string()        
        let $matchExpr := $resource/@assertedTargetNodes/replace(., '^([^/])', '//$1')
        
        for $doc in $docs[f:semapAppliesToDocument($semap, .)]        
        let $items := f:xqueryCDC($matchExpr, $resource, $collectedDynContext, $semap, $doc)
        let $nodes := $items[. instance of node()]
        return
            for $node at $pos in $nodes
            let $_DEBUG := if ($pos mod 100) then () else trace($pos, '#node: ')
            let $nodeId := f:getNodeId($node)
            let $iriValue := 
                if ($iriExpr eq '#bnode') then f:newBnodeIdentifier()
                else f:xqueryCDC($iriExpr, $resource, $collectedDynContext, $semap, $node)
            return
                map:entry(
                    $nodeId,                    
                    map{'rnode': $node,
                        'rmodels': map{
                            $rmodelId: map{
                                'rmodel': $resource, 
                                'iri': $iriValue
                             } 
                         }
                     }
                ) 
    )                
};

(:~
 : Constructs the triples describing a resource. The resource
 : is represented by an XML node with a given nodeID. The
 : XML node is retrieved from the resource dict. Also the 
 : resource model to be used is retrieved from the resource
 : dict.
 :
 : ISSUE: Currently, the resource model is retrieved from the
 : resource model dict by simple lookup: if the resource model ID
 : is found, the resource model is retrieved from the
 : referenced map using key 'rmodel'. This means that for
 : each nodeID only one resource model can be used. We need
 : a more flexible approach: a call parameter specifies the
 : resource model ID, and *that* resource model is retrieved.
 :
 : @return constructed triples, and maps describing object IRIs
 :    not found in the resource dictionary (if any)
 :)
declare function f:constructResource($nodeId as xs:string,
                                     $rmodelId as xs:string?,
                                     $rnodeDict as map(*),
                                     $semaps as element()+,
                                     $collectedDynContext as map(*))
        as item()* {
       
    let $rmodelAndIRI := f:rnodeDict_getRmodelAndIRI($rnodeDict, $nodeId, $rmodelId)
    let $rmodel := $rmodelAndIRI?rmodel
    let $rmodelId := $rmodelAndIRI?rmodelID
    let $subjectIri := $rmodelAndIRI?iri
    
    let $rnode := $rnodeDict($nodeId)?rnode
    let $rnodeRoot := $rnode/ancestor-or-self::*[last()]  
    let $xtripleType := 
        $rmodel/@type/tokenize(normalize-space(.), ' ') ! f:xtriple($subjectIri, 'rdf:type', ., '#iri', ())    
    
    (: for each property model: get triples of hybrid triples :)
    let $aggregateQueryResult := 
        if ($f:SWITCH_USE_AGGREGATE_QUERY) then    
            let $aggregateQuery := $rmodel/re:aggregateQuery/string()
            let $semap := f:semapForNode($rmodel)
            return
                f:xqueryCDC($aggregateQuery, $rnode, $collectedDynContext, $semap, $rnode)
        else ()            
    (: let $DUMMY := trace($aggregateQueryResult, 'AGG_QUERY_RESULT: ') :)
    
    let $xtriples1AndPendingXtriples := $rmodel/re:property ! 
        f:pmodel2Xtriples($rnode, ., $rmodelId, $subjectIri, (), $rnodeDict, $semaps, $collectedDynContext, $aggregateQueryResult)
    let $pendingXtriples := $xtriples1AndPendingXtriples[. instance of map(*) or . instance of array(*)]
    let $xtriples := $xtriples1AndPendingXtriples[. instance of node()]
    return ($xtripleType, $xtriples, $pendingXtriples)
};

(:
 : Constructs triples expressing a property of a resource represented
 : by a resource node.
 :
 : @rnode resource node
 : @param pmodel the property model
 : @param rmodelId identifies the resource model containing the property model
 : @param subjectIri the subject IRI
 : @param defaultPropertyValue default value of the property,
 :    used in the absence of a @value attribute
 : @param rnodeDict resource dict
 : @param semaps semantic maps
 : @param collectedDynContext dynamic context to be used when evaluating
 :    XPath expressions
 :
 : @return constructed triples, and maps describing object IRIs
 :    not found in the resource dictionary (if any)
 :)
declare function f:pmodel2Xtriples($rnode as node(),
                                   $pmodel as element(re:property),
                                   $rmodelId as xs:string,
                                   $subjectIri as xs:string, 
                                   $defaultPropertyValue as item()?, 
                                   $rnodeDict as map(*),
                                   $semaps as element()+,
                                   $collectedDynContext as map(*),
                                   $aggregateQueryResult as map(*)?)
        as item()* {
    let $docUris := map:keys($collectedDynContext)
    let $semap := f:semapForNode($pmodel)

    let $propertyIri := $pmodel/@iri
    let $propertyLangExpr := $pmodel/@lang    
    let $propertyType := $pmodel/@type
    let $propertyMinMaxOccurs := $pmodel/@card/f:getCardinalityRange(..)
    let $propertyMinOccurs := $propertyMinMaxOccurs[1]
    let $propertyMaxOccurs := $propertyMinMaxOccurs[2]
    
    let $isListProperty := $pmodel/@list eq 'true'
    let $reverse := $pmodel/@reverse eq 'true'   
    let $inversePropertyIri := $pmodel/@inverseIri
    let $objectRmodelId := $pmodel/@objectModelID    
    
    (: XDM items to be translated into RDF properties :)
    let $valueItems :=
        let $valueAttr := $pmodel/@value
        return
            if (not($valueAttr)) then $defaultPropertyValue
            else 
                if ($f:SWITCH_USE_AGGREGATE_QUERY) then 
                    $aggregateQueryResult($pmodel/@re:id)
                else 
                    $valueAttr/f:xqueryCDC(., .., $collectedDynContext, $semap, $rnode)
                    
    let $xtriplesAndPendingTriples :=
        for $item in $valueItems
        let $valueItemCase := 
            f:findMatchingValueItemCase($pmodel/re:valueItemCase, $item, $collectedDynContext, $semap, $rnode)

        let $propertyIri := ($valueItemCase/@iri, $propertyIri)[1]
        let $propertyLangExpr := ($valueItemCase/@lang, $propertyLangExpr)[1]        
        let $propertyType := ($valueItemCase/@type, $propertyType, 'xs:string')[1]
        let $propertyMinOccurs := ()
        let $propertyMaxOccurs := ()
        
        (: possibly map the item to item(s), continue with result of mapping :)
        let $itemUpdated :=
            if (not($valueItemCase/@value)) then $item
            else $valueItemCase/@value/
                f:xqueryCDC2(., .., $collectedDynContext, 
                    map{$ref:CLARC_PREFIX_RDFE || 'valueItem': $item}, $semap, $rnode)
        for $item in $itemUpdated
        let $propertyLang := $propertyLangExpr ! 
            f:xqueryCDC2(., .., $collectedDynContext, 
                         if (not($valueItemCase)) then () else
                            map{$ref:CLARC_PREFIX_RDFE || 'valueItem': $item},
                         $semap, $rnode)
        return
            (: process atomic item :)
            if ($propertyType ne '#resource' or $propertyLangExpr or $item instance of xs:anyAtomicType) then
                data($item) !
                f:xtriple($subjectIri, $propertyIri, ., $propertyType, $propertyLang, $reverse)
            (: process resource item :)                    
            else
                let $rnode := $item            
                let $objectRmodelId := ($valueItemCase/@objectModelID, $objectRmodelId)[1]            
                let $objectRmodel :=
                    if ($objectRmodelId) then f:getResourceModelForModelID($objectRmodelId, $semaps)
                    else f:getResourceModelForRnode($rnode, $semaps)
                return
                    if (not($objectRmodel)) then
                        error(QName((), 'INVALID_SEMAP'), 
                            concat('No resource model found for rnode, node name: ', name($rnode), ' ; property iri: ', $pmodel/@iri))
                    else
                    
                let $nodeId := f:getNodeId($rnode)
                let $docUri := i:docUriForNode($rnode)
                return
                    (: object IRI cannot be obtained, as the dynamic context for this 
                       document has not yet been determined; the map includes the docUri
                       as a signal to construct it; pending activities:
                          (a) extend the collected dynamic context
                          (b) add the IRI to the rnode dict;
                          (c) add the triples s-p-objectIRI ) :)
                    if (not($docUri = $docUris)) then
                        map{'taskType': 'hybridTriple',
                            'subjectIri': $subjectIri,
                            'propertyIri': $propertyIri,
                            'rnode': $rnode,  
                            'reverse': $reverse,
                            'inversePropertyIri': $inversePropertyIri,
                            'nodeId': $nodeId,
                            'docUri': $docUri,
                            'rmodel': $objectRmodel
                         }
                     else
                        (: update the object resource model :)
                        let $objectRmodelId := $objectRmodel/@modelID    
                        
                        (: try to retrieve the object resource IRI from the resource dict :)
                        let $objectIri_dict := f:rnodeDict_getRnodeIRI($rnodeDict, $nodeId, $objectRmodelId)
            
                        (: if object resource IRI is not found, evaluate it now, 
                           and write request to construct a resource description  :)
                        let $objectIriAndRdescReq :=
                            if ($objectIri_dict) then () else
    
                            let $objectIri := f:getResourceIRIForRnode($rnode, $objectRmodel, $collectedDynContext) 
                            return 
                                if (not($objectIri)) then () else
                                
                                let $rdescReq := 
                                    map{'taskType': 'rdescRequest',
                                        'rnode': $rnode,
                                        'rmodel': $objectRmodel, 
                                        'iri': $objectIri,
                                        'reverse': $reverse,
                                        'nodeId': $nodeId                                        
                                    }
                                return ($objectIri, $rdescReq)

                        let $objectIri_eval := $objectIriAndRdescReq[.instance of xs:anyAtomicType]
                        let $rdescReq := $objectIriAndRdescReq[.instance of map(*)]
                        return (
                            (: triples :)
                            if ($inversePropertyIri) then
                                ($objectIri_dict, $objectIri_eval) ! 
                                    f:xtriplePair($subjectIri, $propertyIri, ., $inversePropertyIri)
                            else
                                ($objectIri_dict, $objectIri_eval) ! 
                                    f:xtriple($subjectIri, $propertyIri, ., '#iri', (), $reverse),
                            (: request of a resource description :)
                            $rdescReq
                        )
    let $rawResult := $xtriplesAndPendingTriples
    
    (: cardinality check :)
    let $card := count($rawResult)    
    let $errorTriples :=
        let $errorDescriptors :=
            if (exists($propertyMinOccurs) and $card lt $propertyMinOccurs) then
                map{
                    'code': 'ERROR_MIN_OCCURS',
                    'msg': 'Number of property values less than min occurs',
                    'subjectIRI': $subjectIri,
                    'propertyIRI': $propertyIri,
                    'details': map{
                        'minOccurs': $propertyMinOccurs,
                        'actOccurs': count($valueItems),
                        'rmodelID': $rmodelId
                    }
                }
            else if ($propertyMaxOccurs ne -1 and $card gt $propertyMaxOccurs) then
                map{
                    'code': 'ERROR_MAX_OCCURS',
                    'msg': 'Number of property values greater than max occurs',
                    'subjectIRI': $subjectIri,
                    'propertyIRI': $propertyIri,
                    'details': map{
                        'maxOccurs': $propertyMaxOccurs,
                        'actOccurs': count($valueItems),
                        'rmodelID': $rmodelId
                    }
                }                        
            else ()            
        return
            if (empty($errorDescriptors)) then () else
                f:writeErrorTriples($errorDescriptors)
    return (
        $errorTriples,
        
        (: list=false => sequence containing xtriples, hybrid triples, rdesc requests :)
        if (not($isListProperty)) then $rawResult
        else
            let $xtriples := $rawResult[. instance of node()]
            let $maps := $rawResult[. instance of map(*)]
            let $hybridTriples := $maps[?taskType eq 'hybridTriple']
            let $rdescRequests := $maps[not(?taskType eq 'rdescRequest')]            
            return
                (: list=true and hybrid triples => (1) array of xtriples and hybrid triples; (2) rdesc requests :)
                if (exists($hybridTriples)) then (
                    (: array containing nodes and hybrid triples :)
                    array {
                        for $item in $rawResult
                        where not($item[. instance of map(*)]?taskType eq 'rdescRequest')
                        return $item
                    },
                    (: rdescRequests :)
                    $rdescRequests
                    
                (: list=true and no hybrid triples => (1) xtriple representing list; (2) rdesc requests :)
                ) else (
                    i:xtriples2List($xtriples),
                    $rdescRequests                    
                )
    )                
};    

declare function f:findMatchingValueItemCase($valueItemCases as element(re:valueItemCase)*,
                                             $item as item(),
                                             $collectedDynContext as map(*), 
                                             $semap as element(), 
                                             $rnode as node()) 
        as element(re:valueItemCase)? {
    if (empty($valueItemCases)) then () else
    
    let $head := head($valueItemCases)
    return
        let $exprValue :=
            $head/@test/f:xqueryCDC2(., .., $collectedDynContext, 
                map{$ref:CLARC_PREFIX_RDFE || 'valueItem': $item}, $semap, $rnode)
        return
            if (boolean($exprValue)) then $head else
                f:findMatchingValueItemCase(tail($valueItemCases), $item, $collectedDynContext, $semap, $rnode)        
};

(:~
 : 'notInDictIris' has these keys: nodeId, rnode, rmodel, iri.
 :)
declare function f:processTripleRequests(
                                     $tripleReqs as item()*, 
                                     $rnodeDict as map(*),
                                     $semaps as element()+,
                                     $collectedDynContext as map(*))
        as element(shax:triple)* {
    if (empty($tripleReqs)) then () else

    let $hybridLists := $tripleReqs[. instance of array(*)]
    let $tripleReqs_nolist := $tripleReqs[. instance of map(*)]
    let $rdescRequests_nolist := $tripleReqs_nolist[?taskType eq 'rdescRequest']    
    let $hybridTriples_nolist := $tripleReqs_nolist[?taskType eq 'hybridTriple']
    let $hybridTriples_list := array:flatten($hybridLists)[. instance of map(*)]
    
    (: extend the collected dynamic context by entries for new docment URIs :)
    let $collectedDynContext :=
        let $docUris_nolist := $hybridTriples_nolist?docUri
        let $docUris_list := $hybridTriples_list?docUri        
        let $newDocUris := ($docUris_nolist, $docUris_list)  => distinct-values() 
        let $newDocs := $newDocUris ! doc(.)/*
        let $_INFO := if (empty($newDocUris)) then () else trace('NEW_DOC_URIS: ')
        return
            if (empty($newDocs)) then $collectedDynContext
            else f:getCollectedSemapDynContext($semaps, $newDocs, $collectedDynContext)
    
    (: resolve hybrid triples (not in list);
          for each hybrid triple: xtriple + rdesc request 
     :)
    let $hybridTriples_nolist_resolved :=
        for $map in $hybridTriples_nolist
        return f:resolveHybridTriple($map, $collectedDynContext)
    let $xtriples_nolist_hybridTriples := $hybridTriples_nolist_resolved[. instance of node()]
    let $rdescRequests_nolist_hybridTriples := $hybridTriples_nolist_resolved[. instance of map(*)]
    
    (: expand hetero lists; 
          for each item: 
             if an xtriple: xtriple 
             if a hybrid triple: xtriple + rdesc request 
          return:
              (1) list xtriple (integrating xtriples)
              (2) rdesc requests
     :)
    let $hybridLists_expanded :=
        for $array in $hybridLists
        let $raw :=
            for $item in array:flatten($array)
            return
                typeswitch($item)                
                (: xtriple is retained :)
                case node() return $item                
                (: hybrid triple => xtriple + rdesc request :)
                case $hybridTriple as map(*) return 
                    f:resolveHybridTriple($hybridTriple, $collectedDynContext)                    
                default return error()
        let $xtriples := $raw[. instance of node()]
        let $rdescRequests := $raw[. instance of map(*)]
        return (
            i:xtriples2List($xtriples),
            $rdescRequests
        )            
    let $xtriples_hybridList := $hybridLists_expanded[. instance of node()]
    let $rdescRequests_hybridList := $hybridLists_expanded[. instance of map(*)]

    (: collect:
       - xtriples:
            (1) obtained for hybrid triples; 
            (2) extracted from hetero lists )
       - rdesc requests
            (1) rdesc requests received as such
            (2) rdesc requests implied by hybrid triples
            (3) rdesc requests extracted from hetero lists
     :)
    let $xtriples := (
        $xtriples_nolist_hybridTriples, 
        $xtriples_hybridList)     
    let $rdescRequests := (
        $rdescRequests_nolist, 
        $rdescRequests_nolist_hybridTriples, 
        $rdescRequests_hybridList)
        
    (: update rnode dict - for each rdesc request an entry nodeId/rmodelId/iri :)
    let $newRnodeDict := f:rnodeDict_processRdescRequests($rdescRequests, $rnodeDict)
    
    (: execute rdesc requests :)
    let $newXtriplesAndTripleReqs :=
        $rdescRequests !
        f:constructResource(?nodeId, ?rmodel/@modelID, $newRnodeDict, $semaps, $collectedDynContext)
            
    (: collect:
         new xtriples
         new rdesc requests
     :)
    let $newXtriples := $newXtriplesAndTripleReqs[. instance of node()]    
    let $newTripleReqs := $newXtriplesAndTripleReqs[. instance of map(*)]
    let $_INFO := trace(count($newTripleReqs), '#COUNT_RDESC_REQUESTS: ')
    let $_DEBUG := trace(f:DEBUG_reportTripleReqs($newTripleReqs), 'TRIPLE_REQS: ')
    
    (: return:
         xtriples
         new xtriples
         result of evaluating: new rdesc requests, new hybrid triples, new hybrid lists
     :)
    return (
        $xtriples,
        $newXtriples,
        f:processTripleRequests($newTripleReqs, $newRnodeDict, $semaps, $collectedDynContext)
    )        
};

(:~
 : Evaluates an rnode dictionary and returns for a resource representing
 : XML node a resource model and the IRI assigned to the resource by that
 : model. The input can specify a resource model ID, thus requesting that 
 : particular resource model and the IRI which it assigned to the resource. 
 : If no resource model ID is specified, the function returns a resource 
 : model arbitrarily selected from the resource models applicable to that 
 : XML node, according to the rnode dict. 
 :
 : The return value is a map with three entries: 'rmodelID' bound to
 : the resource model ID; 'rmodel' bount to the resource model itself;
 ' 'iri' bound to the IRI assigned to the resource by that model.
 :
 : @param rnodeDict an rnode dictionary
 : @param nodeId the node ID of an XML node representing a resource
 : @param rmodelID the resource model ID of the resource model to be
 :    retrieved; if not specified, an arbitrary selected resource model
 :    and the respective IRI are returned
 :)
declare function f:rnodeDict_getRmodelAndIRI($rnodeDict as map(*),
                                             $nodeId as xs:string, 
                                             $rmodelId as xs:string?)
        as map(*)? {
    if (not(map:contains($rnodeDict, $nodeId))) then () else
    
    let $rmodelId :=            
        if ($rmodelId) then $rmodelId
        else $rnodeDict($nodeId)?rmodels ! map:keys(.)[1]
    let $entry := $rnodeDict($nodeId)?rmodels($rmodelId)
    return
        if (empty($entry)) then () else
        map:merge((
            $entry,
            map:entry('rmodelID', $rmodelId)
        ))                      
};  

(:~
 : Returns the resource IRI associated with a given XML node by
 : a given resource model.
 :
 : @parm rnodeDict dictionary associating XML node IDs with resource models
 : @param nodeId identifies an XML node
 : @param remodelID identifies a resource model
 : @return the resource IRI 
 :) 
declare function f:rnodeDict_getRnodeIRI($rnodeDict as map(*),
                                         $nodeId as xs:string,
                                         $rmodelId as xs:string?)
        as xs:string? {
    if (not($rmodelId)) then () else        
    let $nodeIdEntry := $rnodeDict($nodeId)
    return
        if (empty($nodeIdEntry)) then ()
        else
            let $rmodelEntry := $nodeIdEntry?rmodels($rmodelId)
            return if (empty($rmodelEntry)) then () else $rmodelEntry?iri
};        

(:~
 : Updates the rnode dict, adding further combinations of rnode
 : and rmodel. 'notInDictIris' are maps each describing a combination
 : of rnode and rmodel not yet in the rnode dict. The maps have the
 : following keys: nodeId, rnode, rmodel, iri.
 :)
declare function f:rnodeDict_processRdescRequests(
                                     $rdescRequests as map(*)*, 
                                     $rnodeDict as map(*))
        as map(xs:string, map(*))* {
    let $nodeIds := $rdescRequests?nodeId
    return
        map:merge((
            (: all entries which are not updated :)
            map:keys($rnodeDict)[not(. = $nodeIds)] ! map:entry(., $rnodeDict(.)),
            
            (: new and updated entries :)
            for $map in $rdescRequests
            let $nodeId := $map?nodeId
            let $iri := $map?iri
            let $rmodel := $map?rmodel
            let $rmodelId := $rmodel/@modelID
            let $oldEntries := $rnodeDict($nodeId)?rmodels
            let $newEntry := map{$rmodelId: map{'rmodel': $rmodel, 'iri': $iri}}
            (: let $DUMMY := trace(concat('nodeId=', substring-after($nodeId, '#'), '; rmodelId=', $rmodelId, '; iri=', $iri), 'RDICT_EXT: ') :)
            return
                map{
                    $nodeId: map{
                        'rnode': $map?rnode,
                        'rmodels': map:merge(($oldEntries, $newEntry))
                    }
                }
        ))        
};

(:~
 : Resolves a hybrid triple to an xtriple and a resource
 : description request.
 :
 : @param hybrid a map describing the hybrid triple
 : @param collectedDynContext dynamic context
 : @return xtriple and resource description request,
 :     or the empty sequence if the XML node cannot be
 :     mapped to an IRI
 :)
declare function f:resolveHybridTriple($hybrid as map(*), $collectedDynContext as map(*))
        as item()* {
    let $rnode := $hybrid?rnode
    let $rmodel := $hybrid?rmodel
    let $objectIri := 
        f:getResourceIRIForRnode($rnode, $rmodel, $collectedDynContext)
    return
        if (not($objectIri)) then () else

        let $xtriple :=
            let $subjectIri := $hybrid?subjectIri
            let $propertyIri := $hybrid?propertyIri
            let $inversePropertyIri := $hybrid?inversePropertyIri
            let $reverse := $hybrid?reverse
            return
                if ($inversePropertyIri) then
                    f:xtriplePair($subjectIri, $propertyIri, $objectIri, $inversePropertyIri)
                else
                    f:xtriple($subjectIri, $propertyIri, $objectIri, '#iri', (), $reverse)
        let $rdescReq :=
            let $nodeId := $hybrid?nodeId
            return
                map{'taskType': 'rdescRequest', 'nodeId': $nodeId, 'rnode': $rnode, 'rmodel': $rmodel, 'iri': $objectIri}
        return ($xtriple, $rdescReq)
};


(:~
 : For debug purposes - transforms the rnode dictionary into
 : an XML representation.
 :
 : Map structure:
 : nodeID
 : . 'rnode' => node
 : . 'rmodels'
 : . . rmodelID
 : . . . 'rmodel' => rmodel element
 : . . . 'iri' => IRI string
:)
declare function f:DEBUG_rnodeDict($rnodeDict as map(*))
        as element() {
    <rnodeDict>{
        let $keys := map:keys($rnodeDict)
        for $key in $keys
        let $entry := $rnodeDict($key)
        let $rnode := $entry?rnode
        let $rmodelsMap := $entry?rmodels
        let $rmodelIds := map:keys($rmodelsMap)
        order by $key
        return
            <nodeId value="{$key}">{
                <rnode lname="{$rnode/name(.)}"/>,
                <rmodels>{
                    for $rmodelId in $rmodelIds
                    let $iri := $rmodelsMap($rmodelId)?iri 
                    return
                        <rmodel id="{$rmodelId}" iri="{$iri}"/>
                }</rmodels>
            }</nodeId>
    }</rnodeDict>        
};    

declare function f:DEBUG_rnode_newIris($rnodeDict1 as map(*), $rnodeDict2 as map(*))
        as element() {
    let $iris1 := $rnodeDict1?*?rmodels?*?iri
    let $iris2 := $rnodeDict2?*?rmodels?*?iri
    let $iris2New := $iris2[not(. = $iris1)]
    return
        <newIRIs countNew="{count($iris2New)}" count1="{count($iris1)}" count2="{count($iris2)}">{
            $iris2New ! <newIRI value="{.}"/>
        }</newIRIs>
    
};        

declare function f:DEBUG_rnode_countIris($rnodeDict as map(*))
        as xs:integer {
    let $iris := $rnodeDict?*?rmodels?*?iri
    return
        count($iris)
};        

declare function f:DEBUG_reportTripleReqs($xtriplesAndTripleReqs as item()*)
        as element() {
    let $hybridLists := $xtriplesAndTripleReqs[. instance of array(*)]        
    let $tripleReqs := $xtriplesAndTripleReqs[. instance of map(*)]
    let $rdescReqs := $tripleReqs[?iri ne '?']
    let $hybridTriples := $tripleReqs[not(?iri ne '?')]
    let $rdescReqFreqs :=
        for $rdescReq in $rdescReqs
        group by $lname := $rdescReq?rnode ! local-name(.)
        order by $lname
        return
            <nodes name="{$lname}" count="{count($rdescReq)}"/>
    let $hybridTripleFreqs :=
        for $hybridTriple in $hybridTriples
        group by $lname := $hybridTriple?rnode ! local-name(.)
        order by $lname
        return
            <nodes name="{$lname}" count="{count($hybridTriple)}"/>
    return
        <pendingTriplesReport>{
            <rdescReqs count="{count($rdescReqs)}">{
                $rdescReqFreqs
            }</rdescReqs>,
            <hybridTriples count="{count($hybridTriples)}">{
                $hybridTripleFreqs
            }</hybridTriples>,
            <hybridLists count="{count($hybridLists)}"/>
        }</pendingTriplesReport>
};        
