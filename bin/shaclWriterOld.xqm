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
    "util.xqm";
    
declare namespace z="http://www.ttools.org/shax/ns/structure";
declare namespace shax="http://shax.org/ns/model";
declare namespace stx="http://shax.org/ns/turtlexml";
declare namespace xsd="http://www.w3.org/2001/XMLSchema";

(:~ 
 : Recursive helper function of `shaclFromShaxExpanded`.
 :
 : @param n a node of the expanded SHAX model
 : @param level the current level of indentation
 : @param indent the number of blanks per level of indentation
 : @return a SHACL representation of the input node
 :)
declare function f:shaclFromShaxExpandedRC($n as node(),
                                           $level as xs:integer,
                                           $indent as xs:integer,
                                           $deep as xs:boolean)
        as item()* {
    let $prefix0 := string-join(
        for $i in 1 to $indent return ' ', '')
        
    let $prefix := string-join(
        for $i in 1 to $level * $indent return ' ', '')
    let $prefix2 := $prefix || $prefix0
    let $prefix3 := $prefix2 || $prefix0
    let $prefix4 := $prefix3 || $prefix0    
    let $lines :=
    
    typeswitch($n)
    case comment() return 
        for $line in tokenize($n, '&#xA;') return concat($prefix, '#', $line)
    
    case element(shax:models) return 
        for $c in $n/shax:model return f:shaclFromShaxExpandedRC($c, $level, $indent, $deep)
    
    case element(shax:model) return 
        for $c in $n/node() return f:shaclFromShaxExpandedRC($c, $level, $indent, $deep)
    
    case element(shax:pshape) return
    
        let $datatype := $n/@datatype/string()
        let $datatypeQName := $n/@datatype/resolve-QName(., ..)
        let $datatypeLName := local-name-from-QName($datatypeQName)
        let $datatypeNS := namespace-uri-from-QName($datatypeQName)
        let $isXsdType := $datatypeNS eq $f:URI_XSD
    
        let $prefixNext :=
            if ($n/@ordered) then $prefix4 else $prefix2
            
        (: to be removed after refactoring the handling of nested type :)            
        let $nodeShape := trace(
            if (not($deep)) then () else
            
            let $nodeIRI := $n/@node/resolve-QName(., ..)
            return
                if (empty($nodeIRI)) then ()
                else
                    $n/root()//shax:model/shax:shape[@name/resolve-QName(., ..) eq $nodeIRI]            
                        , 'NODE_SHAPE: ')
            
        (: to be removed after refactoring the handling of nested type :)            
        let $nestedContent :=
            if (not($nodeShape)) then () else (
                (: the attributes are handled here, along with n's own atts :)
                for $c in $nodeShape/(node() except shax:value) return 
                    f:shaclFromShaxExpandedRC($c, $level + 1, $indent, $deep)
                || ' ;'
                    
            )
        
        (: to be removed after refactoring the handling of nested type :)        
        (: twisted way to add the atts of the nested type n's own atts :)
        let $attsFromNestedContent := trace(
            if (not($nodeShape)) then () else 
                <ELEM>{$nodeShape/(@* except (@name, @targetClass))}</ELEM>
         , 'ATTS_FROM_NESTED_CONTENT: ')
        let $nex := ($n, $attsFromNestedContent)
        
        let $values := (
            let $raw := $n/shax:value/string()
            return
                if (empty($raw)) then ()
                else if ($isXsdType and $datatypeLName = ('integer')) then $raw
                else if ($isXsdType and $datatypeLName eq 'string') then $raw ! concat('"', ., '"')
                else $raw ! concat('"', ., '"^^', $datatype)
            (: this second part will be removed soon :)
            ,
            let $raw := $nodeShape/shax:value/string()
            return
                if (empty($raw)) then ()
                else 
                    let $datatype := $nodeShape/@datatype/string()
                    let $datatypeQName := $nodeShape/@datatype/resolve-QName(., ..)
                    let $datatypeLName := local-name-from-QName($datatypeQName)
                    let $datatypeNS := namespace-uri-from-QName($datatypeQName)
                    let $isXsdType := $datatypeNS eq $f:URI_XSD                
                    return (
                        if ($isXsdType and $datatypeLName = ('integer')) then $raw
                        else if ($isXsdType and $datatypeLName eq 'string') then $raw ! concat('"', ., '"')
                        else $raw ! concat('"', ., '"^^', $datatype)
                    )          
        )
        
        (: to be removed after refactoring the handling of nested type :)
        let $nodeConstraintPrefix :=
            if (not($nodeShape)) then () else '# '
        
        let $contentItems := (
            $nex/@path/concat($prefix2, 'sh:path ', .),   
            
            if ($n/@ordered) then (
                concat($prefix2, 'sh:node shax:ListType '),
                concat($prefix2, 'sh:property ['),  
                concat($prefix3, 'sh:path ([sh:zeroOrMorePath rdf:rest] rdf:first)')            
            ) else (),
            
            $n/@minCount[not(. eq '0')]/concat($prefixNext, 'sh:minCount ', .),             
            $n/@maxCount[not(. eq '-1')]/concat($prefixNext, 'sh:maxCount ', .),             
            $nex/@datatype/concat($prefixNext, 'sh:datatype ', .),
            $n/@class/concat($prefixNext, 'sh:class ', .),                
            $n/@nodeKind/concat($prefixNext, 'sh:nodeKind ', ., ' '),
            $n/@node/concat($prefixNext, $nodeConstraintPrefix, 'sh:node ', .),             
            $nex/@minInclusive/concat($prefixNext, 'sh:minInclusive ', .),
            $nex/@maxInclusive/concat($prefixNext, 'sh:maxInclusive ', .),             
            $nex/@minExclusive/concat($prefixNext, 'sh:minExclusive ', .),
            $nex/@maxExclusive/concat($prefixNext, 'sh:maxExclusive ', .),             
            $nex/@minLength/concat($prefixNext, 'sh:minLength ', .),             
            $nex/@maxLength/concat($prefixNext, 'sh:maxLength ', .),             
            $nex/@pattern/concat($prefixNext, 'sh:pattern "', replace(., '\\', '\\\\'), '"'),             
            $nex/@flags/concat($prefixNext, 'sh:flags "', ., '"'),  
            if (empty($values)) then () else concat($prefixNext, 'sh:in (', string-join($values, ' '), ')'),            
            
            if (not($n/@ordered)) then () else
            concat($prefix2, ']'),
            
            for $c in $n/node() return 
                f:shaclFromShaxExpandedRC($c, $level + 1, $indent, $deep)             
        )
        let $content := for $item in $contentItems return concat($item, ' ;')
        
        let $content := (
            let $countContentItems := count($contentItems)
            for $item at $pos in $contentItems 
            return
                concat($item, ' ', 
                    if (ends-with($item, '[')) then ' '
                    else if (ends-with($item, ']')) then ';'
                    else ';')
            )

        return (
            if ($n/parent::shax:shape) then (
                concat($prefix, 'sh:property ['),
                $content,
                
                (: to be removed after refactoring the handling of nested type :)                
                $nestedContent,
                concat($prefix, ']')
            ) else (
                concat($prefix, '['),
                
                (: to be removed after refactoring the handling of nested type :)                
                $content,
                $nestedContent,
                concat($prefix, ']')
            )

        )
            
            
    case element(shax:shape) return
        let $name := $n/@name/string()
        let $datatype := $n/@datatype/string()
        let $datatypeQName := $n/@datatype/resolve-QName(., ..)
        let $datatypeLName := local-name-from-QName($datatypeQName)
        let $datatypeNS := namespace-uri-from-QName($datatypeQName)
        let $isXsdType := $datatypeNS eq $f:URI_XSD
        let $extends := $n/@extends/concat($prefix2, 'sh:node ', .)      
        let $values := 
            let $raw := $n/shax:value/string()
            return
                if (empty($raw)) then ()
                else if ($isXsdType and $datatypeLName = ('integer')) then $raw
                else if ($isXsdType and $datatypeLName eq 'string') then $raw ! concat('"', ., '"')
                else $raw ! concat('"', ., '"^^', $datatype)

        let $contentItems    :=
        
          (: case: a union type :)
            if ($n/@memberTypes) then
                let $mtypes := $n/@memberTypes/tokenize(normalize-space(.), ' ')
                return (
                    concat($prefix2, 'a sh:NodeShape'), 
                    string-join((
                        concat($prefix2, 'sh:or ('),
                        for $mtype in $mtypes return
                            concat($prefix3, $mtype),
                        concat($prefix2, ')')),
                        '&#xA;')
                )
            (: case: a list type :)
            else if ($n/@container eq 'list' or $n/@itemDatatype or $n/@itemNode) then (              
                    concat($prefix2, 'a sh:NodeShape'),
                    concat($prefix2, 'sh:node shax:ListType'),
                    concat($prefix2, 'sh:property ['),                
                    concat($prefix3, 'sh:path ([sh:zeroOrMorePath rdf:rest] rdf:first) '),
                    
                    (: list length constraints :)
                    $n/@minSize/concat($prefix3, 'sh:minCount ', .),
                    $n/@maxSize/concat($prefix3, 'sh:maxCount ', .),
                    
                    if ($n/@itemDataType) then $n/@itemDatatype/concat($prefix3, 'sh:datatype ', .)
                    else if ($n/@itemNode) then $n/@itemNode/concat($prefix3, 'sh:node ', .)
                    else if ($n/@class) then (
                        $n/@class/concat($prefix3, 'sh:class ', .),  
                        $extends
                    ) else (
                        $n/@datatype/concat($prefix3, 'sh:datatype ', .),                   
                        (: facets :)
                        $n/@minInclusive/concat($prefix3, 'sh:minInclusive ', .),
                        $n/@maxInclusive/concat($prefix3, 'sh:maxInclusive ', .),             
                        $n/@minExclusive/concat($prefix3, 'sh:minExclusive ', .),
                        $n/@maxExclusive/concat($prefix3, 'sh:maxExclusive ', .),             
                        $n/@minLength/concat($prefix3, 'sh:minLength ', .),             
                        $n/@maxLength/concat($prefix3, 'sh:maxLength ', .),     
                        $n/@pattern/concat($prefix3, 'sh:pattern "', replace(., '\\', '\\\\'), '"'),             
                        $n/@flags/concat($prefix3, 'sh:flags "', ., '"'),
                        if (empty($values)) then () else concat($prefix3, 'sh:in (', string-join($values, ' '), ')') 
                    ),        
                    for $c in $n/(node() except $values) return 
                        f:shaclFromShaxExpandedRC($c, $level + 2, $indent, $deep),
                    
                    concat($prefix2, ']')
            
            (: not a list type :)
            ) else (
                concat($prefix2, 'a sh:NodeShape'),
                $n/@targetClass/concat($prefix2, 'sh:targetClass ', .),             
                $n/@class/concat($prefix2, 'sh:class ', .),                
                $n/@datatype/concat($prefix2, 'sh:datatype ', .),
                $n/@node/concat($prefix2, 'sh:node ', .),                
                $extends,
                $n/@minInclusive/concat($prefix2, 'sh:minInclusive ', .),
                $n/@maxInclusive/concat($prefix2, 'sh:maxInclusive ', .),             
                $n/@minExclusive/concat($prefix2, 'sh:minExclusive ', .),
                $n/@maxExclusive/concat($prefix2, 'sh:maxExclusive ', .),             
                $n/@minLength/concat($prefix2, 'sh:minLength ', .),             
                $n/@maxLength/concat($prefix2, 'sh:maxLength ', .),             
                $n/@pattern/concat($prefix2, 'sh:pattern "', replace(., '\\', '\\\\'), '"'),             
                $n/@flags/concat($prefix2, 'sh:flags "', ., '"'),
                if (empty($values)) then () else concat($prefix2, 'sh:in (', string-join($values, ' '), ')'),

                for $c in $n/(node() except shax:value) return 
                    f:shaclFromShaxExpandedRC($c, $level + 1, $indent, $deep)
            )

        let $contentItemsCount := count($contentItems)
        let $content := (
            for $item at $pos in $contentItems 
            return
                concat($item, ' ', 
                    if (ends-with($item, '[')) then ' '
                    else if (not($name)) then ';'
                    else if ($pos eq $contentItemsCount) then '.'
                    else ';')
            )
        return (
            if ($name) then (
                concat($prefix, $name),
                $content,
                if (matches($content[last()], '[.;]$')) then '' else ()
            ) else (
                concat($prefix, '['),
                $content,
                concat($prefix, ']')
            )
        )
        
    case element(shax:xone) return 
        let $contentItems := for $c in $n/node() return 
            f:shaclFromShaxExpandedRC($c, $level + 1, $indent, $deep)
        let $content := string-join($contentItems, '&#xA;')
        return (
            concat($prefix, 'sh:xone ('),
            $content,
            concat($prefix, ')')
        )        
        
    case element(shax:or) return 
        let $contentItems := for $c in $n/node() return 
            f:shaclFromShaxExpandedRC($c, $level + 1, $indent, $deep)
        let $content := string-join($contentItems, '&#xA;')
        return (
            concat($prefix, 'sh:or ('),
            $content,
            concat($prefix, ')')
        )        

    case element(shax:and) return 
        let $contentItems := for $c in $n/node() return 
            f:shaclFromShaxExpandedRC($c, $level + 1, $indent, $deep)
        let $content := string-join($contentItems, '&#xA;')
        return (
            concat($prefix, 'sh:and ('),
            $content,
            concat($prefix, ')')
        )        

    case element(shax:not) return 
        let $contentItems := for $c in $n/node() return 
            f:shaclFromShaxExpandedRC($c, $level + 1, $indent, $deep)
        let $content := string-join($contentItems, '&#xA;')
        return (
            concat($prefix, 'sh:not'),
            $content
        )        
    case element(shax:property) return ()
    
    default return string($n)[normalize-space($n)]
    
    return string-join($lines, '&#xA;')[exists($lines)]
};
