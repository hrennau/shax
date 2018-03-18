(:
 : -------------------------------------------------------------------------
 :
 : xsd.xqm - Document me!
 :
 : -------------------------------------------------------------------------
 :)
 
(:~@operations
   <operations>
      <operation name="xsd" type="item()*" func="xsdOp">     
         <param name="shax" type="docFOX+" sep="SC" fct_minDocCount="1" pgroup="input"/>        
         <param name="odir" type="directory?" fct_dirExists="true"/>
         <param name="ofile" type="xs:string?" default="#stdout"/>
         <param name="osuffixes" type="xs:string*"/>
         <pgroup name="input" minOccurs="1"/>   
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
    "namespaceTools.xqm",
    "shaclWriter.xqm",
    "util.xqm",
    "xsdUtil.xqm";
    
declare namespace z="http://www.ttools.org/shax/ns/structure";
declare namespace zz="http://www.ttools.org/structure";
declare namespace shax="http://shax.org/ns/model";
declare namespace xsd="http://www.w3.org/2001/XMLSchema";
declare namespace xs="http://www.w3.org/2001/XMLSchema";

(:~
 : Transforms a SHAX document into XSD documents.
 :
 : @param request the operation request
 : @return the RDF triples of a SHACL shape
 :) 
declare function f:xsdOp($request as element())
        as item()* {
    let $shax := tt:getParams($request, 'shax')/*   
    let $odir := tt:getParams($request, 'odir')
    let $ofile :=
        let $explicit := tt:getParams($request, 'ofile')
        return
            if ($explicit) then $explicit else ()
                (: $shax[1]/root()/replace(replace(document-uri(.), '\.[^.]+$', ''), '^.+/', '') :)
    let $osuffixes := tt:getParams($request, 'osuffixes')
    let $xsds := f:shax2xsd($shax)
    return
        f:writeXsds($xsds, $odir, $ofile, $osuffixes)
};   

declare function f:writeXsds($xsds as element(xs:schema)+,
                             $odir as xs:string?,
                             $ofile as xs:string?, 
                             $osuffixes as xs:string*)
        as element()? {
    if (not($odir)) then
        if (count($xsds) gt 1) then
            tt:createError('MISSING_INPUT_PARAM', 
                concat(
                    'As the SHAX model uses multiple namespaces (', count($xsds),
                    '), multiple XSDs (', count($xsds), ') must be written (one ',
                    'per target namespace) - please specify an output folder, ',
                    'using parameter $odir.'),
                ())
        else if ($ofile eq '#stdout') then $xsds
        else file:write($ofile, $xsds)
    else
    
    let $ofileBase := replace($ofile, '\.xsd$', '')
    let $odir := replace($odir, '([^/])$', '$1/')
    return
        if (count($xsds) eq 1) then
            let $fname := $odir || '/' || $ofileBase || '.xsd'
            return
                file:write($fname, $xsds)
        else
            let $xsdsSorted := sort($xsds, (), function($item){$item/@targetNamespace/lower-case(.)}) 
            let $xsdDescriptors :=
                for $xsd at $pos in $xsdsSorted
                let $tns := $xsd/@targetNamespace/string()
                let $importTns := i:schemaRequiredNamespaces($xsd)
                let $fname := 
                    let $suffix := ($osuffixes[$pos], $pos)[1]
                    return
                        $odir || $ofileBase || $suffix || '.xsd'
                return
                    <xsd fname="{$fname}" tns="{$tns}">{
                        for $uri in $importTns return <importNamespace uri="{$uri}"/>
                    }</xsd>
            let $xsdTable := trace(
                <xsds count="{count($xsdDescriptors)}">{
                    $xsdDescriptors
                }</xsds> , 'XSD_TABLE: ')
            return
                for $xsdInfo at $pos in $xsdTable/xsd
                let $fname := $xsdInfo/@fname
                let $imports :=
                    for $uri in $xsdInfo/importNamespace/@uri
                    let $importFname := $xsdTable/xsd[@tns eq $uri]/@fname
                    return
                        <xs:import namespace="{$uri}" schemaLocation="{$importFname}"/>
                let $xsdElem := $xsdsSorted[$pos]
                let $xsdWithImports :=
                    element {node-name($xsdElem)} {
                        f:namespaceNodes($xsdElem),
                        $xsdElem/@*,
                        $imports,
                        $xsdElem/node()
                    }
                return
                    file:write($fname, $xsdWithImports)
};

(:~
 : Maps a SHAX model to one or more XSD documents. For each
 : namespace used by a SHAX component, an XSD with the 
 : corresponding target namespace is constructed.
 :)
declare function f:shax2xsd($models as element()+)
        as item()+ {
    let $models := f:expandImports($models)        
    let $models := $models/f:addCardinalityAtts(.)  
    
    (: normalize namespace bindings :)
    let $nsmap := f:getShaxModelNamespaceMap($models)
    let $models := f:normalizeShaxModelNamespaceBindings($models, $nsmap)
    
    (: construct schema components :)
    let $comps := f:getXsdCompsRC($models, ())
    
    (: partition schema components by target namespace :)
    let $tnsComps :=
        map:merge(
            for $comp in $comps[self::xs:*]
            group by $tns := $comp/@shax:tns
            order by $tns
            return map:entry($tns, $comp)
        )
        
    (: construct schemas
          note - imports are added later when mappings 
                 namespace => schema location are available :)
    let $schemas :=
        for $tns in map:keys($tnsComps)
        let $myComps := $tnsComps($tns)
        let $tnsAtt :=
            if (not($tns)) then () else attribute targetNamespace {$tns}
        let $namespaceBindings := $nsmap/zz:ns/namespace {@prefix} {@uri}
        let $schema :=
            <xs:schema xmlns:shax="http://shax.org/ns/model">{
                $tnsAtt,
                $namespaceBindings,                
                attribute elementFormDefault {'qualified'},
                $myComps
            }</xs:schema>

        return f:finalizeSchema($schema)            
    return
        $schemas
};        

(:~
 : Recursive helper function of `getXsdComps`.
 :)
declare function f:getXsdCompsRC($n as node(), $tns as xs:string?)
        as node()* {
    typeswitch($n)
    case document-node() return document {for $c in $n/node() return f:getXsdCompsRC($c, $tns)}
    
    (: a global property definition corresponds to a global xs:element :)
    case element(shax:property) return
        let $qname := resolve-QName($n/@name, $n)
        let $lname := local-name-from-QName($qname)
        let $tns := namespace-uri-from-QName($qname)
        let $tnsAtt := attribute shax:tns {$tns}
        let $prefixAtt :=
            if (not($tns)) then () else attribute shax:prefix {prefix-from-QName($qname)}
            
        let $type :=
            if (not($n/@type)) then () else                
                 let $typeName := resolve-QName($n/@type, $n)
                 let $useTypeName := f:editXsdTypeReference($typeName)
                 return
                     attribute type {$useTypeName}
        return
            <xs:element name="{$lname}" shax:tns="{$tns}">{
                f:namespaceNodes($n),
                $prefixAtt,
                $type
            }</xs:element>
            
    case element(shax:objectType) return
        let $qname := resolve-QName($n/@name, $n)
        let $lname := local-name-from-QName($qname)
        let $tns := namespace-uri-from-QName($qname)
        let $tnsAtt := attribute shax:tns {$tns}
        let $prefixAtt :=
            if (not($tns)) then () else attribute shax:prefix {prefix-from-QName($qname)}
        let $baseType := $n/@extends

        let $itemName := $n/@itemName
        let $itemType := $n/@itemType     
        let $minSize := $n/@minSize
        let $maxSize := $n/@maxSize
        
        let $sequence :=
            if ($itemName) then
                <xs:sequence>{
                    <xs:element name="{$itemName}">{
                        $minSize/attribute minOccurs {.},
                        $maxSize/attribute maxOccurs {if (. eq '-1') then 'unbounded' else .},
                        $itemType/attribute type {.}
                    }</xs:element>
                }</xs:sequence>
            else
                <xs:sequence>{
                    for $c in $n/* return f:getXsdCompsRC($c, $tns)
                }</xs:sequence>
        return
            <xs:complexType name="{$lname}" shax:tns="{$tns}">{
                f:namespaceNodes($n),
                $prefixAtt,
                if ($baseType) then
                    <xs:complexContent>{
                        <xs:extension base="{$baseType}">{
                            $sequence
                        }</xs:extension>
                    }</xs:complexContent>
                else
                    $sequence
            }</xs:complexType>

    case element(shax:dataType) return
        let $qname := resolve-QName($n/@name, $n)
        let $lname := local-name-from-QName($qname)
        let $tns := namespace-uri-from-QName($qname)
        let $tnsAtt := attribute shax:tns {$tns}
        let $prefixAtt :=
            if (not($tns)) then () else attribute shax:prefix {prefix-from-QName($qname)}
        let $memberTypes := $n/@memberTypes
        let $itemDatatype := $n/@itemDatatype        
        let $container := $n/@container
        return
            <xs:simpleType>{
                if (exists($qname)) then attribute name {$lname} else (),
                attribute shax:tns {$tns},
                $prefixAtt,
                
                (: union type :)
                if ($memberTypes) then
                    <xs:union memberTypes="{$memberTypes}"/>
                
                (: list type with item type reference :)
                else if ($itemDatatype) then
                    <xs:list itemType="{$itemDatatype}"/>
                
                (: list type with local declaration of item type :)
                else if ($container) then
                    let $itemType :=
                        let $sourceDataType :=
                            element {node-name($n)} {
                                f:namespaceNodes($n),
                                $n/(@* except (@name, @container, @minSize, @maxSize, @size)),
                                $n/node()
                            }
                        return f:getXsdCompsRC($sourceDataType, $tns)
                    let $itemTypeFacets := (
                        $n/@minSize/<xs:minLength value="{.}"/>,
                        $n/@maxSize/<xs:maxLength value="{.}"/>,
                        $n/@size/(<xs:minLength value="{.}"/>, <xs:maxLength value="{.}"/>) 
                    )
                    return                        
                        if (not($itemTypeFacets)) then 
                            <xs:list>{$itemType}</xs:list>
                        else
                            <xs:restriction>{
                                <xs:simpleType>
                                   <xs:list>{$itemType}</xs:list>
                                </xs:simpleType>,
                                $itemTypeFacets
                            }</xs:restriction>
                (: none of the above :)
                else                    
                    $n/@base/f:getXsdCompsRC(., $tns)
            }</xs:simpleType>

    case element(shax:models) | element(shax:model) return
            for $c in $n/node() return f:getXsdCompsRC($c, $tns)

    case element(shax:choice) return
        let $cardAtts := f:getXsdCardinalityAtts($n)
        return
            <xs:choice>{
                $cardAtts,
                for $c in $n/* return f:getXsdCompsRC($c, $tns)
            }</xs:choice>
            
    case element(shax:pgroup) return
        let $cardAtts := f:getXsdCardinalityAtts($n)
        return
            <xs:sequence>{
                $cardAtts,
                for $c in $n/* return f:getXsdCompsRC($c, $tns)
            }</xs:sequence>
            
    case element() return
        if ($n/(parent::shax:objectType, parent::shax:choice, parent::shax:pgroup)) then
            let $qname := node-name($n)
            let $lname := local-name-from-QName($qname)
            let $ns := namespace-uri-from-QName($qname)
            let $cardAtts := f:getXsdCardinalityAtts($n)
            let $type :=
                if (not($n/@type)) then () else                
                    let $typeName := resolve-QName($n/@type, $n)
                    let $useTypeName := f:editXsdTypeReference($typeName)
                    return
                        attribute type {$useTypeName}
            let $ename :=
                if ($ns eq $tns) then QName((), $lname)
                else $qname
            return
            <xs:element name="{$ename}">{
                f:namespaceNodes($n),
                $cardAtts,
                $type,
                ()
            }</xs:element>
        else
            element {node-name($n)} {
                for $a in $n/@* return f:getXsdCompsRC($a, $tns),
                for $c in $n/node() return f:getXsdCompsRC($c, $tns)
            }
            
    case attribute(base) return
        let $baseName := resolve-QName($n, $n/..)
        let $useBaseName := f:editXsdTypeReference($baseName)
        return
            <xs:restriction base="{$useBaseName}">{
                let $content :=
                    for $a in $n/../(@* except (@name, @base)) 
                    return f:getXsdCompsRC($a, $tns)
                let $contentAtts := $content[. instance of attribute()]
                return (
                    $contentAtts,
                    $content except $contentAtts
                )
            }</xs:restriction>

    case attribute(pattern) return
        let $pattern := replace($n, '^\^|\$$', '')
        return
            <xs:pattern value="{$pattern}"/>

    case attribute(len) return
        <xs:length value="{$n}"/>

    case attribute(minLen) return
        <xs:minLength value="{$n}"/>

    case attribute(maxLen) return
        <xs:maxLength value="{$n}"/>

    case attribute(min) return
        <xs:minInclusive value="{$n}"/>

    case attribute(minEx) return
        <xs:minExclusive value="{$n}"/>

    case attribute(max) return
        <xs:maxInclusive value="{$n}"/>

    case attribute(maxEx) return
        <xs:maxExclusive value="{$n}"/>

    default return $n
};    

(:~
 : Finalizes a raw schema. Actions:
 : * remove @shax:tns, @shax:prefix
 : * add namespace nodes to xs:schema
 :)
declare function f:finalizeSchema($schema as element(xs:schema))
        as element(xs:schema) {
    f:finalizeSchemaRC($schema)        
};

(:~
 : Recursive helper function of `f:finalizeSchema`.
 :)
declare function f:finalizeSchemaRC($n as node())
        as node()? {
    typeswitch($n)
    case document-node() return document {for $c in $n/node() return f:finalizeSchemaRC($c)}
    case element(xs:schema) return
        element {node-name($n)} {
            f:namespaceNodes($n),
            for $a in $n/@* return f:finalizeSchemaRC($a),
            for $c in $n/node() return f:finalizeSchemaRC($c)
        }
    case element() return
        element {node-name($n)} {
            for $a in $n/@* return f:finalizeSchemaRC($a),
            for $c in $n/node() return f:finalizeSchemaRC($c)
        }
    case attribute (shax:tns) | attribute (shax:prefix) return ()
        
    default return $n
};

