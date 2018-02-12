(:
 : -------------------------------------------------------------------------
 :
 : shaclWriter.xqm - Document me!
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
    "shaclWriter-deprecated.xqm",
    "util.xqm";
    
declare namespace z="http://www.ttools.org/shax/ns/structure";
declare namespace shax="http://shax.org/ns/model";
declare namespace stx="http://shax.org/ns/turtlexml";
declare namespace xsd="http://www.w3.org/2001/XMLSchema";

declare variable $f:NEW_TO_SHACL := true();

(:~
 : Transforms an expanded SHAX document into a SHACL graph.
 :)
declare function f:shaclFromShaxExpanded($shaxExpanded as element(shax:models),
                                         $deep as xs:boolean)
        as item() {
    let $prefixes := f:shaclFromShaxExpanded_prefixes($shaxExpanded)    
    let $shaclx := <stx:shacl>{f:shaclxFromShaxExpandedRC($shaxExpanded, 0, 3, $deep)}</stx:shacl>
    let $DUMMY := file:write('SHACLX.xml', $shaclx)
    let $shapes := (
        f:shaclFromShaxExpanded_topElements($shaxExpanded),
        if ($f:NEW_TO_SHACL) then 
            f:serializeShaclx($shaclx)
        else
            f:shaclFromShaxExpandedRC($shaxExpanded, 0, 3, $deep),
        f:shaclFromShaxExpanded_listType()[$shaclx//stx:node/@iri = 'shax:ListType']
    )
    return string-join(($prefixes, $shapes),'&#xA;')
};

(:~
 : Transforms the top-level elements of an expanded SHAX document
 : into node shapes with appropriate sh:datatype or sh:node.
 :)
declare function f:shaclFromShaxExpanded_topElements($shaxExpanded as element())
        as item()* {
    let $properties := $shaxExpanded//shax:model/shax:property
    return
        if (not($properties)) then () else
    
    string-join((        
    '#',
    '#top-level elements',
    '#==================',
    '#',        
    for $property at $pos in $properties
    let $type := $property/@datatype    
    let $typeQName := $type/resolve-QName(., ..)
    let $node := $property/@node
    let $typeConstraint :=
        if ($type) then concat('    sh:datatype xsd:', local-name-from-QName($typeQName))
        else if ($node) then concat('    sh:node ', $node)
        else ()
    where $typeConstraint
    return (
            (: concat('_e:_DocumentType', $pos), :)
            concat('_e:_RootResource___', $property/@name/replace(., ':', '___')),
            concat('    a sh:NodeShape ;', ''),
            concat('    sh:targetObjectsOf ', $property/@name, ' ;'),
            concat($typeConstraint, ' .'),
            ''
        ) 
    ), '&#xA;')
};

(:~
 : Returns the prefix declarations required by a SHACL graph.
 :
 : @param shaxExpanded expanded shax document
 : @return a fragment of a SHACL graph containing prefix declarations
 :)
declare function f:shaclFromShaxExpanded_prefixes($shaxExpanded as element(shax:models))
        as xs:string {
string-join((        
        
"@prefix sh: <http://www.w3.org/ns/shacl#> .
@prefix shax: <http://shax.org/ns/model#> .
@prefix nons: <http://shax.org/ns/nonamespace#> .
@prefix _e: <http://shax.org/ns/model/element-equivalent#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix xml: <http://www.w3.org/XML/1998/namespace#> .
@prefix xs:  <http://www.w3.org/2001/XMLSchema#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
"
,
distinct-values(
    for $model in $shaxExpanded/shax:model
    for $prefix in $model/in-scope-prefixes(.)[not(. = ('xml', 'xs', 'xsd', 'shax'))] 
    let $uri := namespace-uri-for-prefix($prefix, $model)
    let $uri := replace($uri, '[^#/]$', '$0#')    
    return concat('@prefix ', $prefix, ': <', $uri, '> .'))
,
'')
, '&#xA;')
};

declare function f:shaclFromShaxExpanded_listType()
        as item() {
"
shax:ListType
   a sh:NodeShape ;
   sh:xone (
      [
          a sh:NodeShape ;
          sh:in (rdf:nil) ;
      ]
      [
          a sh:NodeShape ;
          sh:property [
              sh:path rdf:first ;          
              sh:minCount 1;
              sh:maxCount 1;
          ] ;
          sh:property [
              sh:path rdf:rest ;          
              sh:minCount 1;
              sh:maxCount 1;
          ]
      ] 
   ) .
"
};

(:~ 
 : Recursive helper function of `shaclxFromShaxExpanded`.
 :
 : @param n a node of the expanded SHAX model
 : @param level the current level of indentation
 : @param indent the number of blanks per level of indentation
 : @return a SHACL representation of the input node
 :)
declare function f:shaclxFromShaxExpandedRC($n as node(),
                                            $level as xs:integer,
                                            $indent as xs:integer,
                                            $deep as xs:boolean)
        as node()* {
    let $nss := $n/self::*/i:copyNamespaces(.) return
    
    typeswitch($n)
    case comment() return 
        <stx:comment>{
            for $line in tokenize($n, '&#xA;') return 
                <stx:commentLine>{$line}</stx:commentLine>
        }</stx:comment>
        
    case element(shax:models) return 
        for $c in $n/shax:model return 
            f:shaclxFromShaxExpandedRC($c, $level, $indent, $deep)
    
    case element(shax:model) return 
        for $c in $n/node() return 
            f:shaclxFromShaxExpandedRC($c, $level, $indent, $deep)
    
    case element(shax:pshape) return
        let $path := $n/@path/<stx:path iri="{.}">{$nss}</stx:path>
        let $contentItems := (
            $n/@minCount[not(. eq '0')]/<stx:minCount number="{.}" />,             
            $n/@maxCount[not(. eq '-1')]/<stx:maxCount number="{.}"/>,             

            $n/@class/<stx:class iri="{.}">{$nss}</stx:class>,                
            $n/@node/<stx:node iri="{.}">{$nss}</stx:node>,
            $n/@nodeKind/<stx:nodeKind iri="{.}">{$nss}</stx:nodeKind>,
            
            f:shaclxFromShaxExpanded_facets($n),            
            for $c in $n/node() return 
                f:shaclxFromShaxExpandedRC($c, $level + 1, $indent, $deep)             
        )
        let $contentItems :=
            if (not($n/@ordered)) then $contentItems
            else (
                <stx:node iri="shax:ListType">{$nss}</stx:node>,
                <stx:property>
                    <stx:bnode>{
                        <stx:path string="([sh:zeroOrMorePath rdf:rest] rdf:first)"/>,
                        $contentItems
                    }</stx:bnode>
                </stx:property>
            )     
        let $content := ($path, $contentItems)    
        return (
            if ($n/parent::shax:shape) then (
                <stx:property>
                    <stx:bnode>{
                        $content
                    }</stx:bnode>
                </stx:property>
            ) else (
                <stx:bnode>{
                    $content
                }</stx:bnode>
            )
        )
            
            
    case element(shax:shape) return
        let $name := $n/@name/string()
        let $extends := $n/@extends/<stx:node iri="{string(.)}"/>
        let $contentItems    :=
        
          (: case: a union type :)
            if ($n/@memberTypes) then
                let $mtypes := $n/@memberTypes/tokenize(normalize-space(.), ' ')
                return (
                    <stx:type iri="sh:NodeShape">{$nss}</stx:type>,
                    <stx:or>{
                        $nss,
                        <stx:list>{
                            $mtypes ! <stx:memberType iri="{.}">{$nss}</stx:memberType>
                        }</stx:list>
                    }</stx:or>
                )
                
            (: case: a list type :)
            else if ($n/@container eq 'list' or $n/@itemDatatype or $n/@itemNode) then (   
                <stx:type iri="sh:NodeShape">{$nss}</stx:type>,
                <stx:node iri="shax:ListType">{$nss}</stx:node>,
                <stx:property>{
                    $nss,
                    <stx:bnode>{
                        <stx:path string="([sh:zeroOrMorePath rdf:rest] rdf:first)"/>,
                        $n/@minSize/<stx:minCount number="{.}"/>,
                        $n/@maxSize/<stx:maxCount number="{.}"/>,
                        if ($n/@itemDataType) then
                            $n/@itemDatatype/<stx:dataType iri="{.}">{$nss}</stx:dataType>
                        else if ($n/@itemNode) then
                            $n/@itemNode/<stx:node iri="{.}">{$nss}</stx:node>
                        else if ($n/@class) then (
                            $n/@class/<stx:class iri="{.}">{$nss}</stx:class>,
                            $extends
                        ) else f:shaclxFromShaxExpanded_facets($n),
      
                        for $c in $n/(node() except shax:value) return 
                            f:shaclxFromShaxExpandedRC($c, $level + 2, $indent, $deep)
                    }</stx:bnode>
                }</stx:property>
                    
            
            (: not a list type :)
            ) else (
                <stx:type iri="sh:NodeShape"/>,
                $n/@targetClass/<stx:targetClass iri="{.}">{$nss}</stx:targetClass>,             
                $n/@class/<stx:class iri="{.}">{$nss}</stx:class>,                
                $n/@node/<stx:node iri="{.}">{$nss}</stx:node>,                
                $extends,
                f:shaclxFromShaxExpanded_facets($n),
                for $c in $n/(node() except shax:value) return 
                    f:shaclxFromShaxExpandedRC($c, $level + 1, $indent, $deep)
            )

        let $contentItemsCount := count($contentItems)
        return (
            if ($name) then <stx:shape name="{$name}">{$nss, $contentItems}</stx:shape>
            else <stx:bnode>{$nss, $contentItems}</stx:bnode>
        )
        
    case element(shax:xone) return 
        let $contentItems := for $c in $n/node() return 
            f:shaclxFromShaxExpandedRC($c, $level + 1, $indent, $deep)
        return
            <stx:xone>{
                $nss,
                <stx:list>{
                    $contentItems
                }</stx:list>
            }</stx:xone>
        
    case element(shax:or) return 
        let $contentItems := for $c in $n/node() return 
            f:shaclxFromShaxExpandedRC($c, $level + 1, $indent, $deep)
        return
            <stx:or>{
                $nss,
                <stx:list>{
                    $contentItems
                }</stx:list>
            }</stx:or>

    case element(shax:and) return 
        let $contentItems := for $c in $n/node() return 
            f:shaclxFromShaxExpandedRC($c, $level + 1, $indent, $deep)
        return
            <stx:and>{
                $nss,
                <stx:list>{
                    $contentItems
                }</stx:list>
            }</stx:and>

    case element(shax:not) return 
        let $contentItems := for $c in $n/node() return 
            f:shaclxFromShaxExpandedRC($c, $level + 1, $indent, $deep)
        let $content := string-join($contentItems, '&#xA;')
        return 
            <stx:not>{
                $nss,
                $contentItems
            }</stx:not>
            
    case element(shax:property) return ()
    
    case element(shax:import) return ()
    
    default return <stx:UNKNOWN>{$n}</stx:UNKNOWN>
};

(:~
 : Transforms an element of expanded SHAX into shaclx elements
 : representing facets.
 :
 : @param elem elemenet from expanded SHAX
 : @return shclx representations of facets
 :)
declare function f:shaclxFromShaxExpanded_facets($elem as element())
        as element()* {
    let $nss := i:copyNamespaces($elem)        
    let $values :=
        let $items := $elem/shax:value/string()
        return
            if (empty($items)) then () 
            else            
                let $datatypeQName := $elem/@datatype/resolve-QName(., ..)
                for $item in $items
                return
                    <stx:value>{
                        i:copyNamespaces($elem),
                        if ($datatypeQName eq $i:QNAME_XSDTYPE_STRING) then attribute string {$item}
                        else if ($datatypeQName eq $i:QNAME_XSDTYPE_INTEGER) then attribute number {$item}                        
                        else (attribute literal {$item}, attribute type {$datatypeQName})
                    }</stx:value>
    return (
    
        $elem/@datatype/<stx:datatype iri="{.}">{$nss}</stx:datatype>,            
        $elem/@minInclusive/<stx:minInclusive number="{.}"/>,
        $elem/@maxInclusive/<stx:maxInclusive number="{.}"/>,             
        $elem/@minExclusive/<stx:minExclusive number="{.}"/>,
        $elem/@maxExclusive/<stx:maxExclusive number="{.}"/>,             
        $elem/@minLength/<stx:minLength number="{.}">{$nss}</stx:minLength>,             
        $elem/@maxLength/<stx:maxLength number="{.}">{$nss}</stx:maxLength>,             
        $elem/@pattern/<stx:pattern string="{.}"/>,             
        $elem/@flags/<stx:flags string="{.}"/>,  
        $values 
    )            
};   

declare function f:serializeShaclx($shacl as element(stx:shacl))
        as xs:string {
    f:serializeShaclxRC($shacl, 0, 3)        
};

declare function f:serializeShaclxRC($n as node(),
                                     $level as xs:integer,
                                     $indent as xs:integer)
        as xs:string? {
    let $prefix := string-join(for $i in 1 to $level * $indent return ' ', '')   
    let $prefixStep := string-join(for $i in 1 to $indent return ' ', '')
    let $lines :=
    
        typeswitch($n)
    
        case element(stx:shacl) return
            for $c in $n/node() return f:serializeShaclxRC($c, $level, $indent)
        
        case element(stx:comment) return 
            for $line in $n/stx:commentLine return concat('#', $line)

        case element(stx:shape) return (
            let $headLine := concat($prefix, $n/@name)        
            let $contentItems := (                            
                for $c in $n/(node() except stx:value) return 
                    f:serializeShaclxRC($c, $level + 1, $indent),
                    
                let $values := $n/stx:value
                return
                    if (not($values)) then () else

                    concat($prefix, $prefixStep, 'sh:in (', 
                        string-join(
                            for $value in $n/stx:value
                            return
                                if ($value/@string) then concat('"', $value/@string, '"')
                                else if ($value/@number) then string($value)
                                else if ($value/@type/resolve-QName(., ..) eq $i:QNAME_XSDTYPE_BOOLEAN) then string($value)
                                else $value/concat('"', $value/@literal, '"', '^^', @type)
                            , ' '),
                        ')')

            )                
            let $content := string-join($contentItems, ';&#xA;')
            return (
                $headLine, 
                $content || '.'
            )
        )
        case element(stx:path) return
            if ($n/@iri) then concat($prefix, 'sh:path ', $n/@iri)
            else if ($n/@string) then concat($prefix, 'sh:path ', $n/@string) 
            else error()
            
        case element(stx:minCount) return
            concat($prefix, 'sh:minCount ', $n/@number)
            
        case element(stx:maxCount) return
            concat($prefix, 'sh:maxCount ', $n/@number)
            
        case element(stx:minInclusive) return
            concat($prefix, 'sh:minInclusive ', $n/@number)
            
        case element(stx:maxInclusive) return
            concat($prefix, 'sh:maxInclusive ', $n/@number)
            
        case element(stx:minExclusive) return
            concat($prefix, 'sh:minInclusive ', $n/@number)
            
        case element(stx:maxExclusive) return
            concat($prefix, 'sh:maxInclusive ', $n/@number)
            
        case element(stx:minLength) return
            concat($prefix, 'sh:minLength ', $n/@number)
            
        case element(stx:maxLength) return
            concat($prefix, 'sh:maxLength ', $n/@number)
            
        case element(stx:pattern) return
            concat($prefix, 'sh:pattern "', $n/@string/replace(., '\\', '\\\\'), '"')
            
        case element(stx:flags) return
            concat($prefix, 'sh:flags "', $n/@string, '"')
            
        case element(stx:node) return
            concat($prefix, 'sh:node ', $n/@iri)
            
        case element(stx:datatype) return
            concat($prefix, 'sh:datatype ', $n/@iri)
            
        case element(stx:memberType) return
            concat($prefix, $n/@iri)
            
        case element(stx:type) return
            concat($prefix, 'a ', $n/@iri)
            
        case element(stx:targetClass) return
            concat($prefix, 'sh:targetClass ', $n/@iri)
            
        case element(stx:class) return
            concat($prefix, 'sh:class ', $n/@iri)
            
        case element(stx:nodeKind) return
            concat($prefix, 'sh:nodeKind ', $n/@iri)
            
        case element(stx:property) return
            if ($n/stx:bnode) then
                let $bnodeContentItems :=
                    for $c in $n/stx:bnode/* return
                        f:serializeShaclxRC($c, $level + 1, $indent) || ';'
                let $myLines := (
                    concat($prefix, 'sh:property ['),
                    $bnodeContentItems,
                    concat($prefix, ']')
                )
                return
                    string-join($myLines, '&#xA;')
            else
                concat($prefix, 'sh:property ', $n/@iri)
            
        case element(stx:bnode) return
            let $content := 
                string-join(
                    for $c in $n/node() return 
                        f:serializeShaclxRC($c, $level + 1, $indent) || ' ;'
                    , '&#xA;')
            return 
                concat($prefix, '[&#xA;', $content, '&#xA;', $prefix, ']')
                
        case element(stx:list) return
            let $content := 
                string-join(
                    for $c in $n/node() return 
                        f:serializeShaclxRC($c, $level + 1, $indent)
                    , '&#xA;')                        
            return 
                concat($prefix, '(&#xA;', $content, '&#xA;', $prefix, ')')
                
        case element(stx:xone) return (
            concat($prefix, 'sh:xone ('),
            string-join(
                $n/stx:list/*/f:serializeShaclxRC(., $level + 1, $indent)
                , '&#xA;'),
            concat($prefix, ')')
        )

        case element(stx:or) return (
            concat($prefix, 'sh:or ('),
            string-join(
                $n/stx:list/*/f:serializeShaclxRC(., $level + 1, $indent)
                , '&#xA;'),
            concat($prefix, ')')
        )

        case element(stx:not) return
            if ($n/stx:bnode) then (
                concat($prefix, 'sh:not ['),
                string-join(
                    for $c in $n/stx:bnode/* return f:serializeShaclxRC($c, $level + 1, $indent) || ';'
                    , '&#xA;'),
                concat($prefix, ']')
            ) else (
                concat($prefix, 'sh:not'),
                for $c in $n/node() return f:serializeShaclxRC(., $level + 1, $indent)
            )

        default return concat($prefix, 'UNKNOWN ELEM: ', name($n))
        
    return
        string-join($lines, '&#xA;')[string()]    
};
