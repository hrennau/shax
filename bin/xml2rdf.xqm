(:
 : -------------------------------------------------------------------------
 :
 : xml2rdf.xqm - functions for transforming XML into RDF.
 :
 : -------------------------------------------------------------------------
 :)
 
(:~@operations
   <operations>
      <operation name="rdfe" type="item()" func="rdfeOp">     
         <param name="dox" type="docFOX+" fct_minDocCount="1" sep="SC"/>    
         <param name="jox" type="jsonFOX*" fct_minDocCount="0" sep="SC"/>
         <param name="semap" type="docFOX?" fct_minDocCount="1"/>
         <param name="r1" type="xs:integer?" />
         <param name="r2" type="xs:integer?" />
         <param name="blankNodes" type="xs:boolean?" default="false"/>
         <param name="format" type="xs:string?" fct_values="xmlaug, xtriples0, xtriples, triples" default="triples"/>
         <param name="nons" type="xs:string?"/>         
         <param name="prefixNons" type="xs:string?"/>
      </operation>
    </operations>  
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
    "shaxLoader.xqm",
    "schemaLoader.xqm",
    "targetNamespaceTools.xqm",
    "triplesUtil.xqm",
    "typeGlobalizer.xqm",
    "util.xqm",
    "rdfe.xqm",
    "xml2rdfUtil.xqm",    
    "xsdComponentManager.xqm";

    (: "typeGlobalizer.xqm", :) 
    
declare namespace z="http://www.ttools.org/shax/ns/structure";
declare namespace zz="http://www.ttools.org/structure";
declare namespace shax="http://shax.org/ns/model";
declare namespace shaxerr="http://shax.org/ns/error";
declare namespace nons="http://shax.org/ns/nonamespace";
declare namespace xsd="http://www.w3.org/2001/XMLSchema";

(:
 : ============================================================================
 :
 :     o p e r a t i o n s
 :
 : ============================================================================
 :)

declare function f:rdfeOp($request as element())
        as item() {
    let $docs := (
        tt:getParam($request, 'dox')/*,
        tt:getParam($request, 'jox')/*
    )
    let $format := tt:getParam($request, 'format')
    let $r1 := tt:getParam($request, 'r1')
    let $r2 := tt:getParam($request, 'r2')
    let $useBlankNodes := tt:getParam($request, 'blankNodes')
    let $nons := tt:getParam($request, 'nons')
    let $prefixNons := tt:getParam($request, 'prefixNons')    
    let $semap := tt:getParam($request, 'semap')/*
    let $options :=
        <options>{
            $nons ! attribute nons {.},
            $prefixNons ! attribute prefixNons {.},
            $r1 ! attribute r1 {.},
            $r2 ! attribute r2 {.}
        }</options>
    let $rdf := 
        if ($semap) then f:rdfe($docs, $semap, $format, $options)
        else f:xml2rdf($docs, $useBlankNodes, $format, $options)
    return $rdf
};   

(:
 : ============================================================================
 :
 :     p u b l i c    f u n c t i o n s
 :
 : ============================================================================
 :)

(:~
 : Performs a transformation of XML documents into RDF triples which is
 : generic and does not use a semantic context.
 :
 : @param request the operation request
 : @return RDF triples
 :) 
declare function f:xml2rdf($docs as element()*, $useBlankNodes as xs:boolean, $format as xs:string, $options as element(options))
        as item()* {
    let $docs_ := i:addNodeNumbers($docs)
    return if ($format eq 'xmlaug') then $docs_ else
        
    let $xtriples :=
        let $individuals := $docs_/f:doc2Xtriples(., $useBlankNodes, $options)
        return        
            <shax:triples>{
                $individuals
            }</shax:triples>
    return if ($format eq 'xtriples0') then $xtriples else
    
    let $xtriples := f:editXtriples($xtriples, $options)
    return if ($format eq 'xtriples') then $xtriples else

    let $triples := i:xtriples2Triples($xtriples)
    
    return
        $triples
};   

(:
 : ============================================================================
 :
 :     p r i v a t e    f u n c t i o n s
 :
 : ============================================================================
 :)

(:
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
    element {node-name($elem)} {
        if ($elem/parent::*) then () else
            let $docUri := $elem/root()/document-uri(.)
            return (
                namespace rdf {$f:URI_RDF},
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
};
:)

(:~
 : Transforma a document augmented with @shax:nodeNr attributes
 : into a shax:triples document.
 :)
declare function f:doc2Xtriples($rootElem as element(), $useBlankNodes as xs:boolean, $options as element(options))
        as element(shax:triple)* {
    let $nons := $options/@nons        
    let $uriSubject := $f:URI_SHAX_WORLD
    let $uriSubjectRep := '<' || $uriSubject || '>'
    let $uriProperty := f:propertyUriForNode($rootElem, $nons)
    let $uriPropertyRep := '<' || $uriProperty || '>'
    let $uriObject := $rootElem/@shax:elemURI
    let $uriObjectRep := '<' || $uriObject || '>'

    let $rootTriple := 
        <shax:triple s="{$uriSubjectRep}" p="{$uriPropertyRep}" o="{$uriObjectRep}"/>
    let $otherTriples := f:doc2XtriplesRC($rootElem, $uriObject, $useBlankNodes, $nons)        
    return
        ($rootTriple, $otherTriples)
};        

declare function f:doc2XtriplesRC($elem as element(), $uri as xs:string, $useBlankNodes as xs:boolean, $nons as xs:string?)
        as element(shax:triple)* {
    (: let $uriRep := if (starts-with($uri, '_:')) then $uri else '<' || $uri || '>' :)
    let $uriRep := if (matches($uri, '^("|_:)')) then $uri else '<' || $uri || '>'
    return (
    
    (: a triple connecting the URI with an rdf:identifier :)
    <shax:triple s="{$uriRep}" p="rdf:label" v="{local-name($elem)}"/>,
    
    (: for each attribute a triple treating the attribute value as object :)
    for $att in $elem/(@* except (@shax:nodeNr, @shax:elemURI))
    let $uriProperty := f:propertyUriForNode($att, $nons)
    let $uriPropertyRep := '<' || $uriProperty || '>' 
    let $attValue := string($att)
    return
        <shax:triple s="{$uriRep}" p="{$uriPropertyRep}" v="{$attValue}"/>,

    (: complex with simple content: a triple treating the simple content as object :)
    if (not($elem/text())) then () else
        <shax:triple s="{$uriRep}" p="rdf:value" v="{$elem/string(.)}"/>,
    
    (: for each child element a triple using as object either the simple content or the child element URI :)    
    for $childElem in $elem/*
    let $uriProperty := f:propertyUriForNode($childElem, $nons)
    let $uriPropertyRep := '<' || $uriProperty || '>'
    let $isComplex :=  $childElem/f:isElemComplex(.)
    let $v := if ($isComplex) then () else string($childElem)
(:    
    let $o := if (not($isComplex)) then () else "_:n" || substring-after($childElem/@shax:nodeNr, '__')
    let $o := if (not($isComplex)) then () else replace($uri, '#.+', concat('#n', substring-after($childElem/@shax:nodeNr, '__')))
:)    
    let $o := 
        if (not($isComplex)) then () else 
            if ($useBlankNodes) then "_:n" || substring-after($childElem/@shax:nodeNr, '__')
            else replace($uri, '#.+', concat('#n', substring-after($childElem/@shax:nodeNr, '__')))
    let $orep := $o ! (if ($useBlankNodes) then . else concat('<', ., '>'))
    let $objectAtt := (
        $v ! attribute v {.},
        $orep ! attribute o {.}
    )
(:    
        else attribute o {"_:n" || substring-after($childElem/@shax:nodeNr, '__')}
:)        
    return (
        <shax:triple s="{$uriRep}" p="{$uriPropertyRep}">{$objectAtt}</shax:triple>,
        f:doc2XtriplesRC($childElem, $o, $useBlankNodes, $nons)[$isComplex]
    )
    )    
};

declare function f:propertyUriForNode($node as node(), $nons as xs:string?)
        as xs:anyURI {
    let $nodeUri := namespace-uri($node)[string()]
    let $uri := ($nodeUri, $nons, $f:URI_NONS_DEFAULT)[1]
    let $sep := if (matches($uri, '[#/]$')) then () else '#'
    return
        xs:anyURI($uri || $sep || local-name($node))
};

(:
(:~
 : Transforms an xtriples document into a Turtle representation of its
 : triples.
 :)
declare function f:xtriples2Triples($xtriples as element(shax:triples))
        as xs:string {
    let $prefixes := (
        $xtriples/shax:nsmap/shax:ns/concat('@prefix ', @prefix, ': <', @uri, '> .'),
        '@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .'
    )
    let $triples :=        
        for $xtriple in $xtriples/shax:triple
        group by $subject := $xtriple/@s
        let $indent := string-join(for $i in 1 to string-length($subject) return ' ', '')
        return (
            for $t at $pos in $xtriple
            let $s :=
                if ($pos eq 1) then $subject
                else $indent
            let $delimit := if ($pos eq count($xtriple)) then '.' else ';'    
            (: order by if (starts-with($t/@p, 'rdf:')) then 'aaa' else replace($t/@p, '.+:', ''), replace($t/@pm, ':.*', '') :)            
            return
                concat($s, ' ', $t/@p, ' ', $t/(@o, @v/concat('"', replace(., '"', '\\"'), '"')), ' ', $delimit) 
        ) 
    return
        string-join(($prefixes, ' ', $triples), '&#xA;')
};
:)

declare function f:isElemComplex($elem as element()?)
        as xs:boolean? {
    $elem/exists(((@* except @shax:nodeNr), *))        
};        