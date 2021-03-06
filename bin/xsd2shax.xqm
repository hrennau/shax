(:
 : -------------------------------------------------------------------------
 :
 : xsd2shax.xqm - functions for transforming XSD into SHAX.
 :
 : -------------------------------------------------------------------------
 :)
 
(:~@operations
   <operations>
      <operation name="xsd2shax" type="element()" func="xsd2shaxOp">     
         <param name="xsd" type="docFOX+" sep="SC"/>    
         <param name="ignoreAnno" type="xs:boolean?" default="false"/>
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

(:~
 : Implements operation 'xsd2shax', which transforms XSD
 : documents into a SHAX model.
 :
 : @param request the operation request
 : @return a SHAX model capturing the model specified by the XSD documents
 :) 
declare function f:xsd2shaxOp($request as element())
        as item() {
    let $schemas := i:getSchemas($request) 
    let $ignoreAnno := tt:getParam($request, 'ignoreAnno')
    let $nsmap := i:getTnsPrefixMap($schemas)
    
    let $options := <options ignoreAnno="{$ignoreAnno}"/>
        
    return
        f:xsd2shax($options, $nsmap, $schemas)
};   

(:~
 : Transforms XSD documents into a SHAX model.
 :
 : @param options options controlling the processing
 : @param request the operation request
 : @return a SHAX model capturing the model specified by the XSD documents
 :) 
declare function f:xsd2shax($options as element(options)?,
                            $nsmap as element(zz:nsMap)?, 
                            $schemas as element(xs:schema)+)
        as element() {
        
    (: check if schemas meet constraints of current implementation :)
    let $check := f:xsd2shax_check($nsmap, $schemas)
    return
        if ($check) then $check else
    
    (: normalize schemas 
            to be considered - probably namespace normalization necessary  
            in order to exclude inconsistent prefix bindings :)
    let $schemas00 := $schemas
    
    (: globalize any local type definitions :)
    let $schemas01 :=      
        let $localTypes := $schemas00//(xs:simpleType, xs:complexType)[not(@name)]
        return
            if (not($localTypes)) then $schemas00
            else
            (:
                let $_DEBUG := trace(count($localTypes),
                    'Globalize local types, count: ') return
             :)
                f:schemasWithGlobalizedTypes($schemas00)

    (: properties :)
    let $properties := f:xsd2shax_properties($nsmap, $schemas01)

    (: object types :)
    let $otypes := f:xsd2shax_objectTypes($options, $nsmap, $schemas01)

    (: data types :)
    let $dtypes := f:xsd2shax_dataTypes($nsmap, $schemas01)

    (: content :)
    let $content := (
        comment {'*** properties ***'},
        $properties,
        comment {'*** data types ***'},
        $dtypes,
        comment {'*** object types ***'},
        $otypes
    )
    (: finalize shax model :)
    let $raw := <shax:model defaultCard="1" z:xsdCount="{count($schemas)}">{$content}</shax:model>
    let $final := f:xsd2shax_namespaceFixup($raw, $nsmap)
    return $final    
        
};        

(:~
 : Checks if the XSDs are compatible with current limitations of the implementation.
 :
 : @param request the operation request
 : @return a SHAX model capturing the model specified by the XSD documents
 :) 
declare function f:xsd2shax_check($nsmap as element(zz:nsMap)?, $schemas as element(xs:schema)+)
        as element(shaxerr:error)? {
        
    (: check if schemas meet constraints of current implementation :)
    let $violations :=    
        if ($schemas/xs:group) then
            <shaxerr:feature name="xsd2shax_group">
                <shaxerr:detail name="countGroups" value="{count($schemas/xs:group)}"/>
            </shaxerr:feature>
        else ()
    return
        if (empty($violations)) then ()
        else
            <shaxerr:error type="NOT_YET_IMPLEMENTED">{
                $violations
            }</shaxerr:error>
};

(:~
 : Returns the SHAX properties capturing the top-level element and
 : attribute declarations.
 :
 : @param nsmap a mapping of namespace URIs to prefixes
 : @param schemas the schema elements to be considered
 : @return SHAX properties
 :)
declare function f:xsd2shax_properties($nsmap as element(zz:nsMap)?, $schemas as element(xs:schema)+)
        as element(shax:property)* {
    let $elems := $schemas/xs:element
    let $atts := $schemas/xs:attribute    
    let $properties :=
        for $item in ($atts, $elems)
        let $name := $item/f:getComponentName(.) ! i:normalizeQNameNONS(., $nsmap)
        let $type := $item/@type/resolve-QName(., ..) ! i:normalizeQNameNONS(., $nsmap)
        let $sgroup := $item/self::xs:element/@substitutionGroup/resolve-QName(., ..) ! i:normalizeQNameNONS(., $nsmap)
        order by local-name-from-QName($name), prefix-from-QName($name)        
        return
            <shax:property name="{$name}">{
                if (not(exists($type))) then () else attribute type {$type},            
                if (not(exists($sgroup))) then () else attribute substitutes {$sgroup}                
            }</shax:property>
    return
        $properties
};        

(:~
 : Returns the SHAX object types capturing the complex type definitions found in
 : a set of schema documents.
 :
 : @param nsmap a mapping of namespace URIs to prefixes
 : @param schemas the schema elements to be considered
 : @return SHAX object types
 :)
declare function f:xsd2shax_objectTypes($options as element(options)?,
                                        $nsmap as element(zz:nsMap)?, 
                                        $schemas as element(xs:schema)+)
        as element(shax:objectType)* {
    let $ctypes := $schemas/xs:complexType
    let $ctypes_sc := $ctypes[.//xs:simpleContent]
    let $ctypes_cc := $ctypes except $ctypes_sc
    let $otypes_sc := f:xsd2shax_objectTypes_sc($ctypes_sc, $options, $nsmap, $schemas)
    let $otypes_cc := f:xsd2shax_objectTypes_cc($ctypes_cc, $options, $nsmap, $schemas)    
    let $otypes :=
        for $ot in ($otypes_sc, $otypes_cc)
        let $name := $ot/@name/tt:resolveNormalizedQName(., $nsmap)
        order by local-name-from-QName($name), prefix-from-QName($name)
        return $ot
    return
        $otypes        
};        

(:~
 : Returns the SHAX object types capturing the complex type definitions with simple
 : content found in a set of schema documents.
 :
 : @param ctypes complex type definitions (from XSD)
 : @param nsmap a mapping of namespace URIs to prefixes
 : @param schemas the schema elements to be considered
 : @return SHAX object types
 :)
declare function f:xsd2shax_objectTypes_sc($ctypes as element(xs:complexType)*,
                                           $options as element(options)?,
                                           $nsmap as element(zz:nsMap)?,
                                           $schemas as element(xs:schema)+)
        as element(shax:objectType)* {                                           
    for $ctype in $ctypes return f:xsd2shax_objectType_sc($ctype, $options, $nsmap, $schemas)
};

(:~
 : Returns the SHAX object types capturing the complex type definitions with complex
 : content found in a set of schema documents.
 :
 : @param ctypes complex type definitions with complex content (from XSD) 
 : @param nsmap a mapping of namespace URIs to prefixes
 : @param schemas the schema elements to be considered
 : @return SHAX object types
 :)
declare function f:xsd2shax_objectTypes_cc($ctypes as element(xs:complexType)*,
                                           $options as element(options)?,
                                           $nsmap as element(zz:nsMap)?,
                                           $schemas as element(xs:schema)+)
        as element(shax:objectType)* {
    for $ctype in $ctypes return f:xsd2shax_objectType_cc($ctype, $options, $nsmap, $schemas)        
};        

(:~
 : Returns the SHAX object type capturing a complex type definition with simple
 : content.
 :
 : NOTE: the case xs:restriction is not yet implemented.
 :
 : @param ctype a complex type definition (from XSD) 
 : @param nsmap a mapping of namespace URIs to prefixes
 : @param schemas the schema elements to be considered
 : @return SHAX object types
 :)
declare function f:xsd2shax_objectType_sc($ctype as element(xs:complexType), 
                                          $options as element(options)?,
                                          $nsmap as element(zz:nsMap)?,
                                          $schemas as element(xs:schema)*)
        as element(shax:objectType) {
    let $name := $ctype/i:getNormalizedComponentName(., $nsmap) ! i:normalizeQNameNONS(., $nsmap)
    
    let $restriction := $ctype/xs:simpleContent/xs:restriction
    return if ($restriction) then
        error(QName($i:URI_ERROR, 'NOT_YET_IMPLEMENTED'), concat('Not yet implemented: support ',
            'for complext type with simple content and xs:restriction; type name: ', $name))
        else
            
    let $extension := $ctype/xs:simpleContent/xs:extension
    let $base := $extension/@base/resolve-QName(., ..)
    let $baseUri := namespace-uri-from-QName($base)
    let $isBaseSimple :=
        if ($baseUri eq $i:URI_XSD) then true()
        else
            exists(
                $schemas[not($baseUri) and not(@targetNamespace) or
                         $baseUri and $baseUri eq @targetNamespace]
                /xs:simpleType)
    let $baseNorm := i:normalizeQNameNONS($base, $nsmap)                         
    let $extendsAtt := if ($isBaseSimple) then () else attribute extends {$baseNorm}    
    let $valueProperty :=
        if (not($isBaseSimple)) then () (: value property taken care of by a base type :)
        else <nons:value type="{$baseNorm}"/>
    
    let $properties := f:xsd2shax_typeContentItems($ctype, $options, $nsmap, $schemas)
    let $class := 
        if (ends-with(local-name-from-QName($name), 'Type')) then replace(string($name), 'Type$', '')
        else ()
    return
        <shax:objectType name="{$name}">{
            attribute targetClass {$class} [$class],
            $extendsAtt,
            $valueProperty,
            $properties
        }</shax:objectType>
};        

(:~
 : Returns the SHAX object type capturing a complex type definition with complex
 : content.
 :
 : @param ctype a complex type definition (from XSD) 
 : @param nsmap a mapping of namespace URIs to prefixes
 : @param schemas the schema elements to be considered
 : @return SHAX object types
 :)
declare function f:xsd2shax_objectType_cc($ctype as element(xs:complexType),
                                          $options as element(options)?,
                                          $nsmap as element(zz:nsMap)?,
                                          $schemas as element(xs:schema)*)
        as element(shax:objectType) {
    let $name := $ctype/i:getNormalizedComponentName(., $nsmap) ! i:normalizeQNameNONS(., $nsmap)
    
    let $extendsAtt :=
        let $base := $ctype/(xs:simpleContent, xs:complexContent)/xs:extension/@base
        return
            if (not($base)) then () 
            else
                let $tname := $base/resolve-QName(., ..) ! tt:normalizeQName(., $nsmap)
                return attribute extends {$tname}
    let $properties := f:xsd2shax_typeContentItems($ctype, $options, $nsmap, $schemas)
    let $class := 
        if (ends-with(local-name-from-QName($name), 'Type')) then replace(string($name), 'Type$', '')
        else ()
    return
        <shax:objectType name="{$name}">{
            attribute targetClass {$class} [$class],
            $extendsAtt,
            $properties
        }</shax:objectType>
};        

(:~
 : Returns the SHAX property declarations capturing the content items of a complex type 
 : definition.
 :
 : @param ctype a complex type definition (from XSD)
 : @param nsmap a mapping of namespace URIs to prefixes
 : @param schemas the schema elements to be considered
 : @return SHAX object types
 :)
declare function f:xsd2shax_typeContentItems($ctype as element(xs:complexType), 
                                             $options as element(options)?,
                                             $nsmap as element(zz:nsMap)?,
                                             $schemas as element(xs:schema)*)
        as node()* {
    f:xsd2shax_typeContentItemsRC($ctype, $options, $nsmap, $schemas)
};        

(:~
 : Recursive helper function of `xsd2shax_typeContentItems`.
 :
 : @param n the node to be processed
 : @param nsmap a mapping of namespace URIs to prefixes
 : @param schemas the schema elements to be considered
 : @return SHAX object types
 :)
declare function f:xsd2shax_typeContentItemsRC($n as node(),
                                               $options as element(options)?,
                                               $nsmap as element(zz:nsMap)?,
                                               $schemas as element(xs:schema)*)
        as node()* {
    typeswitch($n)
    case element(xs:annotation) return
        if ($options/@ignoreAnno[string()]/xs:boolean(.)) then ()
        else <shax:annotation source="xsd">{$n}</shax:annotation>
        
    case element(xs:complexType) | 
         element(xs:simpleContent) | 
         element(xs:complexContent) |
         element(xs:extension) | 
         element(xs:restriction) return
        for $c in $n/node() return
            f:xsd2shax_typeContentItemsRC($c, $options, $nsmap, $schemas)
            
    case element(xs:sequence) | element(xs:all) return
        (: @TO.DO - take minOccurs/maxOccurs into account ! :)
        let $items :=
            for $c in $n/node() return 
                f:xsd2shax_typeContentItemsRC($c, $options, $nsmap, $schemas)
            
        let $card := f:cardinalityDescForXsdComp($n) 
        let $cardAtt := attribute card {$card} [string($card)]
        let $parent := $n/parent::*
        return
            if ($card or $parent/(self::xs:choice, self::xs:sequence, self::xs:all)) then
                <shax:pgroup>{
                    $cardAtt,
                    $items
                }</shax:pgroup>
            else
                $items
            
    case element(xs:choice) return
        <shax:choice>{
            let $card := f:cardinalityDescForXsdComp($n) return
                attribute card {$card} [string($card)],
            for $a in $n/(@* except (@minOccurs, @maxOccurs)) return 
                f:xsd2shax_typeContentItemsRC($a, $options, $nsmap, $schemas), 
            for $c in $n/node() return 
                f:xsd2shax_typeContentItemsRC($c, $options, $nsmap, $schemas) 
        }</shax:choice>
        
    case element(xs:attributeGroup) return
        let $def := f:findAttributeGroup($n/@ref/resolve-QName(., ..), $nsmap, $schemas)
        return
            $def/*/f:xsd2shax_typeContentItemsRC(., $options, $nsmap, $schemas)
            
    case element(xs:element) | element(xs:attribute) return
        (: @TO.DO - rethink how to treat namespace-less components;
                    always put them into nons namespace?;
                    add prefix 'nons' to nsmap?
         :)
        let $name := f:getComponentName($n) ! i:normalizeQNameNONS(., $nsmap) 
        let $type := $n/@type/resolve-QName(., ..) ! i:normalizeQNameNONS(., $nsmap)
        let $typeAtt := attribute type {$type} [exists($type)]
        let $card := f:cardinalityDescForXsdComp($n)
        let $cardAtt := attribute card {$card} [string($card)]
        let $orderedAtt := attribute ordered {'true'} [$n/@shax:ordered eq 'true']   
        let $localStype := $n/xs:simpleType
        let $localDatatype :=
            $localStype/f:xsd2shax_dataType(., $nsmap, $schemas) 
        return
            element {$name} {
                $cardAtt,
                $typeAtt,
                $orderedAtt,
                $localDatatype/@*,
                $localDatatype/node()
            }
            
    case text() return if (not($n/matches(., '\S')) and $n/../*) then () else $n            
    default return $n            
};        

(:~
 : Returns the SHAX datatypes capturing the simple type definitions found in
 : a set of schema documents.
 :
 : @param nsmap a mapping of namespace URIs to prefixes
 : @param schemas the schema elements to be considered
 : @return SHAX object types
 :)
declare function f:xsd2shax_dataTypes($nsmap as element(zz:nsMap)?, 
                                      $schemas as element(xs:schema)+)
        as element(shax:dataType)* {
    let $stypes := $schemas/xs:simpleType
    let $dtypes :=
        for $stype in $stypes return
            f:xsd2shax_dataType($stype, $nsmap, $schemas)
    let $dtypes :=
        for $dt in $dtypes
        let $name := $dt/@name/tt:resolveNormalizedQName(., $nsmap)
        order by local-name-from-QName($name), prefix-from-QName($name)
        return $dt
    return
        $dtypes        
};        

(:~
 : Returns the SHAX datatype capturing a simple type definition.
 :
 : @param stype a simple type definition (from XSD) 
 : @param nsmap a mapping of namespace URIs to prefixes
 : @param schemas the schema elements to be considered
 : @return SHAX object types
 :)
declare function f:xsd2shax_dataType($stype as element(xs:simpleType), 
                                     $nsmap as element(zz:nsMap)?,
                                     $schemas as element(xs:schema)+)
                                     
        as element(shax:dataType) {
    let $name := $stype[@name]/i:getNormalizedComponentName(., $nsmap)        
    let $base := $stype/xs:restriction/@base
    let $memberTypes := $stype/xs:union/@memberTypes
    let $itemType := $stype/xs:list/@itemType
    
    let $content :=
        if ($base) then
            let $baseType := $base/resolve-QName(., ..) ! tt:normalizeQName(., $nsmap)
            
            let $values := $stype/xs:restriction/xs:enumeration/@value
            let $pattern := $stype/xs:restriction/xs:pattern/@value
                                  /replace(replace(., '^\^|\$$', ''), '(.+)', '^$1\$')
            let $min := $stype/xs:restriction/xs:minInclusive/@value    
            let $max := $stype/xs:restriction/xs:maxInclusive/@value    
            let $minEx := $stype/xs:restriction/xs:minExclusive/@value    
            let $maxEx := $stype/xs:restriction/xs:maxExclusive/@value    
            let $minLen := $stype/xs:restriction/xs:minLength/@value    
            let $maxLen := $stype/xs:restriction/xs:maxLength/@value    
            return (
                $base/attribute base {$baseType},
                if (not(exists($pattern))) then () else attribute pattern {$pattern}, 
                $min/attribute min {.},
                $max/attribute max {.},            
                $minEx/attribute minEx {.},
                $maxEx/attribute maxEx {.},            
                $minLen/attribute minLen {.},
                $maxLen/attribute maxLen {.},
                for $value in $values return 
                    <shax:value>{$value/string()}</shax:value>
            )
        else if ($memberTypes) then (
            let $tnames := 
                for $tname in $memberTypes/tokenize(normalize-space(.), ' ')
                let $normTname := $tname/resolve-QName($tname, $memberTypes/..) 
                                  ! tt:normalizeQName(., $nsmap)
                return $normTname
            return 
                attribute memberTypes {string-join($tnames, ' ')}
                
        ) else if ($itemType) then (
            let $tname := $itemType/resolve-QName(., ..) ! tt:normalizeQName(., $nsmap)
            return
                attribute itemType {$tname}
                
        ) else (
            trace($stype, 'UNEXPECTED_SIMPLE_TYPE: '),
            error()
        )
    return
        <shax:dataType>{
            if (empty($name)) then () else attribute name {$name},
            $content
        }</shax:dataType>
};  

declare function f:xsd2shax_namespaceFixup($model as element(shax:model),
                                           $nsmap as element(zz:nsMap)?)
        as element(shax:model) {
    let $raw := tt:addNSBs($model, $nsmap)        
    let $additionalNs :=
        if ($model//nons:*) then namespace nons {'http://shax.org/ns/nonamespace'}
        else ()
    return        
        element {node-name($raw)} {
            f:namespaceNodes($raw),
            $additionalNs,
            $raw/(@*, node())            
        }
};        


