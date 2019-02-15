(:
 : -------------------------------------------------------------------------
 :
 : xml2rdfUtil.xqm - utility functions supporting the transformation of XML into RDF
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
    "util.xqm"
;

    (: "typeGlobalizer.xqm", :) 
    
declare namespace z="http://www.ttools.org/shax/ns/structure";
declare namespace zz="http://www.ttools.org/structure";
declare namespace shax="http://shax.org/ns/model";
declare namespace shaxerr="http://shax.org/ns/error";
declare namespace nons="http://shax.org/ns/nonamespace";
declare namespace xsd="http://www.w3.org/2001/XMLSchema";
declare namespace re="http://www.rdfe.org/ns/model";

declare variable $f:SWITCH_USE_GENERATE_ID := 0;
declare variable $f:SWITCH_DEBUG_XQUERY_CDC := 0;

(:~
 : Evaluates an XQuery expression.
 :)
declare function f:xqueryCDC($expr as xs:string,
                             $namespaceContext as element()?,
                             $collectedDynContext as map(*)?,
                             $semap as element(),
                             $contextItem as node())
        as item()* {
    if (not($expr)) then () else
    
    let $_DUMMY := if (not($f:SWITCH_DEBUG_XQUERY_CDC)) then () 
                   else file:append-text-lines('DEBUG_XQUERY_CDC.txt', $expr)
    
    let $docUri := f:docUriForNode($contextItem)     
    let $semapIri := $semap/@iri
    let $dynContext := $collectedDynContext($docUri)($semapIri)
    let $docs := $collectedDynContext('')
    return f:xquery($expr, $namespaceContext, $dynContext, $docs, $contextItem)        
};

(:~
 : Evaluates an XQuery expression. An optional additional dynamic context
 : augments the dynamic context obtained for the combination of document
 : (containing the context item) and semantic map.
 :)
declare function f:xqueryCDC2($expr as xs:string,
                              $namespaceContext as element()?,
                              $collectedDynContext as map(*)?,
                              $additionalContext as map(*)?,
                              $semap as element(),
                              $contextItem as node())
        as item()* {
    let $docUri := f:docUriForNode($contextItem)     
    let $semapIri := $semap/@iri
    let $dynContext := $collectedDynContext($docUri)($semapIri)
    let $dynContext2 := map:merge(($dynContext, $additionalContext), map{'duplicates': 'use-last'})
    let $docs := $collectedDynContext('')    
    return f:xquery($expr, $namespaceContext, $dynContext2, $docs, $contextItem)        
};

(:~
 : Evaluates an XQuery expression.
 :
 : Note that the dynamic context is augmented by variable $rdfe:nodeNumber which
 : can be used as an IRI identifying the context node.
 :)
declare function f:xquery($expr as xs:string,
                          $namespaceContext as element()?,
                          $dynContext as map(*)?,
                          $docs as node()*,
                          $contextItem as item())
        as item()* {
    let $dynContext_ := f:updateDynContext($dynContext, $docs, $contextItem)
    let $dynContext_ :=
        if ($contextItem instance of attribute() or $contextItem instance of element()) then
            map:put($dynContext_, $ref:CLARC_PREFIX_RDFE || 'nodeNumber', f:nodeNumber($contextItem))
        else $dynContext_
    return f:xquery($expr, $namespaceContext, $dynContext_)        
};


(:~
 : Evaluates an XQuery expression.
 :)
declare function f:xquery($expr as xs:string,
                          $namespaceContext as element()?,
                          $dynContext as map(*)?)
        as item()* {
    let $expr_ := f:addContextToExpr($expr, $namespaceContext, $dynContext)

(:
    let $expr2_ := unparsed-text('/products/drugbank/DEBUG_EXPR.xq')
    let $DUMMY := trace( xquery:eval($expr2_, map{}) , 'EVAL: ')
    let $DUMMY := file:write('DEBUG_EXPR.xq', $expr_)
:)    
    return 
        try {xquery:eval($expr_, $dynContext)}
        catch * {
            let $errorReport :=
                <errorReport>{
                    attribute code {$err:code},
                    attribute description {$err:description},
                    attribute expr {$expr},
                    <completeExpr>{$expr_}</completeExpr>
                }</errorReport>
            let $_INFO := trace($errorReport)
            return error(QName((), 'INVALID_CONFIG'), concat( 
                'Error in semap-defined expression - aborted. ',
                'Expression: ', $expr, ' ; extended expression: ', $expr_))
        }
};

(:~
 : Updates a dynamic context by setting the context item and further values.
 : Further values are:
 : - self = context Item
 : - docs = the set of all documents
 :
 : @param dynContext a map of variable bindings and/or function item bindings
 : @param contextItem an item to be used as context item
 : @return the updated dynamic context
 :)
declare function f:updateDynContext($dynContext as map(*)?, $docs as node()*, $contextItem as item())
        as map(*) {
    if (empty($dynContext)) then 
        map{'': $contextItem, 
            $ref:CLARC_PREFIX_RDFE || 'self': $contextItem, 
            $ref:CLARC_PREFIX_RDFE || 'docs': $docs}        
    else 
        map:merge((
            map:put($dynContext, '', $contextItem),
            map:entry($ref:CLARC_PREFIX_RDFE || 'self', $contextItem),
            map:entry($ref:CLARC_PREFIX_RDFE || 'docs', $docs)))
};        

(:~
 : Augments the text of an XQuery expression by adding a prolog providing
 : (a) the namespace bindings in scope at a specified element, (b) the
 : variable bindings supplied by a dynamic context.
 :
 : @param expr the expression text
 : @param namespaceContext this element provides required namespace bindings
 : @param dynContext a map of variable bindings and/or function item bindings
 : @return the augmented expression
 :)
declare function f:addContextToExpr($expr as xs:string, 
                                    $namespaceContext as element()?, 
                                    $dynContext as map(*)?)
        as xs:string {
    let $clarcRdfe := '\{' || $ref:URI_RDFE || '\}'
    let $namespaces := (
        'declare namespace rdfe="' || $ref:BUILTIN_NAMESPACE_BINDINGS?rdfe || '";',
        let $prefixes := $namespaceContext/in-scope-prefixes(.)[not(. eq 'xml')]
        for $prefix in $prefixes[. ne 'rdfe']
        let $uri := namespace-uri-for-prefix($prefix, $namespaceContext)
        return
            if (not(string($prefix))) then
                'declare default element namespace "' || $uri || '";'
            else
                'declare namespace ' || $prefix || '="' || $uri || '";'
    )                
    let $variables :=
        if (empty($dynContext)) then() else
        
        for $key in map:keys($dynContext)[string(.)]
        let $key := replace($key, $clarcRdfe, 'rdfe:')
        let $value := $dynContext($key)
        return
            (: concat('declare variable $', $key, ' := "', $value, '";') :)            
            concat('declare variable $', $key, ' external;')
    (: let $variables := ($variables, 'declare variable $rdfe:nodeNumber external := ();') :)            
    return
       string-join(($namespaces, $variables, $expr), '&#xA;')
};

(:~
 : Returns for a given node the document URI.
 :) 
declare function f:docUriForNode($node as node())
        as xs:string? {
    $node/ancestor-or-self::*[last()]/base-uri(.)        
};

(:~
 : Returns for a given semap node the semap IRI.
 :) 
declare function f:semapIriForNode($node as node())
        as xs:string? {
    (: $node/ancestor-or-self::*[last()]/@iri :)        
    f:semapForNode($node)/@iri
};

(:~
 : Returns for a given semap node the semap element.
 :) 
declare function f:semapForNode($node as node())
        as element(re:semanticMap) {
    $node/ancestor-or-self::*[last()]        
};

(:~
 : Maps an XML node to a node number expressing its identity by
 : concatenating the document uri with a node identifier which 
 : is unique among the identifiers used when processing this input.
 :)
declare function f:nodeNumber($node as node())
        as xs:string {
    let $baseUri := $node/ancestor-or-self::*[last()]/base-uri(.)
    let $numbers := $node/ancestor::*/(1 + count(preceding-sibling::*))
    let $lastStep :=
        typeswitch($node)
        case element() return 1 + count($node/preceding-sibling::*)
        case attribute() return 
            let $atts := $node/../@*/node-name(.) => sort()
            return
                concat('@',index-of($atts, node-name($node)))
        default return error((), "Invalid arg - 'getNodeNumber' requires input node which is an element or attribute")
    let $nodeNumber := concat($baseUri, '#location_', string-join(($numbers, $lastStep), '.'))         
    (: let $nodeNumber := concat('d1__', string-join(($numbers, $lastStep), '.')) :)
    return
        $nodeNumber
};

(:~
 : Transformse documents, augmenting each element node
 : with a @shax:nodeNumber attribute containing a
 : node identifier which is unique among the identifiers
 : used when processing this input.
 :)
declare function f:addNodeNumbers($docs as element()*)
        as element()* {
    for $doc at $pos in $docs
    let $docNumber := $pos
    let $rootNodeNumber := 'd' || $pos || '__1'
    return
        f:addNodeNumbersRC($doc, $rootNodeNumber)        
};

(:~
 : Recursive helper function of `addNodeNumbers`.
 :
 : @TO.DO - add support for mixed content
 :)
declare function f:addNodeNumbersRC($elem as element()+, $nodeNumber as xs:string)
        as element()+ {
    document {
    element {node-name($elem)} {
        f:namespaceNodes($elem),
        if ($elem/parent::*) then () else
            let $docUri := $elem/root()/document-uri(.)
            return (
                namespace rdf {$f:URI_RDF},
                attribute xml:base {$docUri},
                attribute shax:elemURI {$docUri || '#root-elem'}
            ),                
        attribute shax:nodeNr {$nodeNumber},
        $elem/@*,
        if (not($elem/*)) then $elem/node()
        else
            for $child at $pos in $elem/*
            let $childNodeNumber := $nodeNumber || '.' || $pos
            return
                f:addNodeNumbersRC($child, $childNodeNumber)
    }
    }/*
};

(:~
 : Returns for a given node a unique node ID string.
 :)
declare function f:getNodeId($node as node())
        as xs:string {
    (: generate-id($node) :)
    if ($f:SWITCH_USE_GENERATE_ID) then generate-id($node)
    
    else    
        let $docUri := $node/ancestor-or-self::*[last()]/base-uri(.)
        let $path := path($node)
        return
            concat($docUri, '#path:', $path)
};        

(:~
 : Returns the resource IRI of a resource represented by an XML node.
 : The IRI is obtained by evaluating the @IRI attribute on the
 : resource model. The resource model is either received as a
 : parameter, or it is now determined by scanning the semantic
 : maps for the best match for the resource node. If no single
 : best match is found, the function returns the empty sequence.
 :
 : @param rnode an XML node representing a resource
 : @param rmodel the resource model to be used for rnode
 : @param dynContext dynamic context to be used when evaluating XQuery
 :    expressions provided by the resource model
 : @return the resource IRI
 :)
declare function f:getResourceIRIForRnode($rnode as node(), 
                                          $rmodel as element()?, 
                                          $semaps as element(re:semanticMap)*,
                                          $collectedDynContext as map(*))
        as xs:string? {
    let $rmodel :=
        if ($rmodel) then $rmodel
        else f:getResourceModelForRnode($rnode, $semaps)
    return
        if (count($rmodel) ne 1) then ()
        else f:getResourceIRIForRnode($rnode, $rmodel, $collectedDynContext)
};

(:~
 : Returns the resource IRI of a resource represented by an XML node.
 : The IRI is obtained by evaluating the @IRI attribute on the
 : resource model. The resource model is received as a parameter.
 :
 : @param rnode an XML node representing a resource
 : @param rmodel the resource model to be used for rnode
 : @param collectedDynContext dynamic context to be used when evaluating XQuery
 :    expressions provided by the resource model
 : @return the resource IRI
 :)
declare function f:getResourceIRIForRnode($rnode as node(), 
                                          $rmodel as element(), 
                                          $collectedDynContext as map(*))
        as xs:string? {
    let $semap := f:semapForNode($rmodel)
    let $iriExpr := $rmodel/@iri
    return
        (: blank node - get a new identifier :)
        if ($iriExpr eq '#bnode') then f:newBnodeIdentifier()
        (: otherwise - evaluate the IRI expression :)        
        else
            $rmodel/@iri/f:xqueryCDC(., $rmodel, $collectedDynContext, $semap, $rnode)
};

(:~
 : Returns a resource model, given an attribute providing its ID.
 :
 : @param idAtt an attribute providing the model ID
 : @return the resource model
 :)
declare function f:getResourceModelForModelID($idAtt as attribute()?, $semaps as element()+)
        as element(re:resource)? {
    $idAtt/(let $id := . return $semaps//re:resource[@modelID eq $id])
};      


declare function f:getResourceModelForRnode($rnode as node(), $semaps as element(re:semanticMap)*)
        as element()? {
    let $rootElem := $rnode/ancestor-or-self::*[last()]
    let $mySemaps := $semaps[f:semapComplementsDoc(., $rootElem)]
    let $myRmodels := $mySemaps//re:resource[f:rmodelAppliesToNode(., $rnode)]
    let $errorMsg := if (count($myRmodels) le 1) then () else
        concat('Resource node targeted by multiple resource models; resource node name: ', 
                local-name($rnode), '; rmodel IDs: ', string-join($myRmodels/@modelID, ', '),
                ' ; consider disambiguating by using @objectModelID="my-modelID" on <re:property>.')
    return
        if ($errorMsg) then
            error(QName((), 'DYNAMIC_SEMAP_ERROR'), $errorMsg)
        else $myRmodels
};

(:~
 : Determines whether a given semap complements a given document.
 :
 : @param semap a semantic map
 : @param doc a document
 : @return true if the semantic map complements the document, false otherwise
 :)
declare function f:semapComplementsDoc($semap as element(), $doc as element())
        as xs:boolean {
    let $ns := $doc/namespace-uri(.)
    let $name := $doc/local-name(.)
    
    let $tns := $semap/@targetNamespace[string()] ! tokenize(.)
    let $tn := $semap/@targetName[string()] ! tokenize(.)
    let $tassertions := $semap/(@targetAssertion, 
                                re:targetAssertion/(@expr, text())[string()][1])
    let $tnsOk :=
        if (not($ns)) then
            if (empty($tns) or $tns = '#null') then true()
            else false()
        else        
            let $tnsFilters :=
                $tns !
                replace(., '\*', '.*')!
                replace(., '\?', '.') !
                concat('^', ., '$')
            return
                some $f in $tnsFilters satisfies matches($ns, $f, 'i')
    let $tnOk :=
        if (empty($tn)) then true()
        else
            let $tnFilters :=
                $tn !
                replace(., '\*', '.*') !
                replace(., '\?', '.') !
                concat('^', ., '$')
            return
                some $f in $tnFilters satisfies matches($name, $f, 'i')
    let $tassOk :=
        every $tass in $tassertions satisfies
            let $expr := concat('boolean(', $tass, ')')
            let $namespaceContext := $tass/ancestor-or-self::*[1]
            return f:xquery($expr, $namespaceContext, (), (), $doc)
    return $tnsOk and $tnOk and $tassOk            
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
    not($rmodel/@targetNodeNamespace ne namespace-uri($rnode)) and
    not($rmodel/@targetNodeName ne local-name($rnode))
};

declare function f:writeErrorTriples($errorDescriptors as map(*)*)
        as element(shax:triple)* {
    if (empty($errorDescriptors)) then () else
    for $err in $errorDescriptors
    let $s := f:newBnodeIdentifier()
    return (
        f:xtriple($s, 'rdf:type', 'rdfee:error', '#iri', ()),
        f:xtriple($s, 'rdfee:errorCode', $err?code, 'xs:string', ()),
        f:xtriple($s, 'rdfee:errorMsg', $err?msg, 'xs:string', ()),
        $err?subjectIRI ! f:xtriple($s, 'rdfee:subjectIRI', ., '#iri', ()),
        $err?propertyIRI ! f:xtriple($s, 'rdfee:predicateIRI', ., '#iri', ()),
        for $key in map:keys($err?details)
            return f:xtriple($s, concat('rdfee:', $key), $err?details($key))
    )            
};

declare function f:DEBUG_collectedSemapDynContext($collectedDynContext as map(*))
        as element() {
    <collectedDynContext>{
        for $docUri in map:keys($collectedDynContext)[string()]
        let $docMap := $collectedDynContext($docUri)
        return
            <doc uri="{$docUri}">{
                for $semapIRI in map:keys($docMap)
                let $varMap := $docMap($semapIRI)
                let $vars :=
                    for $varName in map:keys($varMap)
                    let $value := 
                        let $raw := $varMap($varName)
                        return
                            if ($raw instance of map(*)) then f:DEBUG_map2xml($raw, ())                        
                            else if ($raw instance of function(*)) then '#function-item'
                            else $raw
                    return
                        <var name="{$varName}">{
                            if ($value instance of xs:anyAtomicType*) then attribute value {$value}
                            else $value
                        }</var>                        
                return
                    <semap iri="{$semapIRI}">{$vars}</semap>
            }</doc>
    }</collectedDynContext>
};

declare function f:DEBUG_map2xml($map as map(*), $rootName as xs:string?)
        as element()* {
    let $entries :=
        let $keys  := map:keys($map)
        let $asElemName := every $key in $keys satisfies $key instance of xs:NCName
        for $key in $keys
        let $value := 
            let $raw := $map($key)
            return
                if ($raw instance of map(*)) then f:DEBUG_map2xml($raw, ())                        
                else if ($raw instance of function(*)) then '#function-item'
                else $raw
        let $content :=                
            if ($value instance of xs:anyAtomicType*) then attribute value {$value}
            else $value
        return
            if ($asElemName) then element {$key} {$content}
            else <entry key="{$key}">{$content}</entry>
    return
        if ($rootName) then element {$rootName} {$entries}
        else $entries
};


