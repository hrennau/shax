(:
 : -------------------------------------------------------------------------
 :
 : xsd.xqm - Document me!
 :
 : -------------------------------------------------------------------------
 :)
 
(:~@operations
   <operations>
      <operation name="xsd" type="item()+" func="xsdOp">     
         <param name="shax" type="docFOX*" sep="SC" fct_minDocCount="1" pgroup="input"/>        
         <pgroup name="input" minOccurs="1"/>   
         <param name="format" type="xs:string?" default="ttl" fct_values="xml, ttl"/>
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
    "util.xqm",
    "xsdUtil.xqm";
    
declare namespace z="http://www.ttools.org/shax/ns/structure";
declare namespace shax="http://shax.org/ns/model";
declare namespace xsd="http://www.w3.org/2001/XMLSchema";
declare namespace xs="http://www.w3.org/2001/XMLSchema";

(:~
 : Transforms a shax document into a XSD documents.
 :
 : @param request the operation request
 : @return the RDF triples of a SHACL shape
 :) 
declare function f:xsdOp($request as element())
        as item()+ {
    let $shax := tt:getParams($request, 'shax')/*   
    let $xsd := f:shax2xsd($shax)
    return
        $xsd
};   

(:~
 : Maps a shax model to one or more XSD documents.
 :)
declare function f:shax2xsd($shax as element())
        as item()+ {
    let $shax := $shax/f:addCardinalityAtts(.)        
    let $comps := f:getXsdCompsRC($shax, ())
    let $tnsComps :=
        map:merge(
            for $comp in $comps[self::xs:*]
            group by $tns := $comp/@shax:tns
            order by $tns
            return map:entry($tns, $comp)
        )
    let $schemas :=
        for $tns in map:keys($tnsComps)
        let $myComps := $tnsComps($tns)
        let $tnsAtt :=
            if (not($tns)) then () else attribute targetNamespace {$tns}
        let $nss :=
            if (not($tns)) then () else
            
            let $comp1 := $myComps[1]
            let $qname1 := $comp1/@name/resolve-QName(., ..)
            let $prefix1 := $comp1/@shax:prefix
            return
                namespace {$prefix1} {$tns}
            
        let $schema :=
            <xs:schema xmlns:shax="http://shax.org/ns/model">{
                $tnsAtt,
                $nss,                
                attribute elementFormDefault {'qualified'},
                <xs:import namespace="http://shax.org/ns/model" schemaLocation="shax.xsd"/>,
                $myComps
            }</xs:schema>
        return f:finalizeSchema($schema)            
    return
        (: TO.DO - deal with multiple xsds :)
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
                f:copyNamespaces($n),
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
        let $baseType := ($n/@base, 'shax:objectBaseType')[1] 
        return
            <xs:complexType name="{$lname}" shax:tns="{$tns}">{
                f:copyNamespaces($n),
                $prefixAtt,
                <xs:complexContent>{
                    <xs:extension base="{$baseType}">{
                        <xs:sequence>{
                            for $c in $n/* return f:getXsdCompsRC($c, $tns)
                        }</xs:sequence>
                    }</xs:extension>
                }</xs:complexContent>                    
            }</xs:complexType>

    case element(shax:dataType) return
        let $qname := resolve-QName($n/@name, $n)
        let $lname := local-name-from-QName($qname)
        let $tns := namespace-uri-from-QName($qname)
        let $tnsAtt := attribute shax:tns {$tns}
        let $prefixAtt :=
            if (not($tns)) then () else attribute shax:prefix {prefix-from-QName($qname)}
        return
            <xs:simpleType name="{$lname}" shax:tns="{$tns}">{
                $prefixAtt,
                $n/@base/f:getXsdCompsRC(., $tns)
            }</xs:simpleType>

    case element(shax:model) return
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
                f:copyNamespaces($n),
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
                for $a in $n/../(@* except (@name, @base)) return f:getXsdCompsRC($a, $tns)
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
            f:copyNamespaces($n),
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

