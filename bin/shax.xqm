(:
 : -------------------------------------------------------------------------
 :
 : shax.xqm - Document me!
 :
 : -------------------------------------------------------------------------
 :)
 
(:~@operations
   <operations>
      <operation name="shacl" type="item()" func="shaclOp">     
         <param name="shax" type="docFOX" sep="SC" pgroup="input"/>        
         <pgroup name="input" minOccurs="1"/>   
         <param name="format" type="xs:string?" default="ttl" fct_values="xml, ttl"/>
         <param name="deep" type="xs:boolean?" default="false"/>
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
    "util.xqm";
    
declare namespace z="http://www.ttools.org/shax/ns/structure";
declare namespace shax="http://shax.org/ns/model";
declare namespace xsd="http://www.w3.org/2001/XMLSchema";

(:~
 : Transforms a shax document into a SHACL shape.
 :
 : @param request the operation request
 : @return the RDF triples of a SHACL shape
 :) 
declare function f:shaclOp($request as element())
        as item() {
    let $models := tt:getParams($request, 'shax')/*   
    let $format := tt:getParams($request, 'format')
    let $deep := tt:getParams($request, 'deep')    
    let $shacl := f:shacl($models, $format, $deep)
    return
        $shacl
};   

(:~
 : Transforms a shax document into a SHACL model. This is
 : accomplished by two chained transformations: (a) shax 
 : into expanded shax, (b) expanded shax into SHACL.
 :
 : @param models one or more shax documents
 : @format controls whether a SHACL model or an expanded
 :    shax document is returned
 : @return a SHACL model or an expanded shax document, depending
 :    on parameter $format
 :)
declare function f:shacl($models as element()+, 
                         $format as xs:string,
                         $deep as xs:boolean)
        as item() {
    let $modelsExpanded := f:expandShax($models)
    return
        if ($format eq 'xml') then $modelsExpanded
        else f:shaclFromShaxExpanded($modelsExpanded, $deep)
};        

(:~
 : Transformas SHAX documents into the expanded SHAX format.
 :
 : @param models one or more SHAX documents
 : @return an expanded SHAX document
 :)
declare function f:expandShax($models as element()+)
        as element() {
        
    (: *** recursively expand all imports :)
    let $trans1 := f:expandImports($models)
    
    (: *** add @card attributes (making default values explicit) :)
    let $trans2 := f:expandShax1($trans1)        
    
    (: *** replace references to substitution group heads by choice groups :)
    let $trans3 :=
        let $sgroups := f:sgroupsFromShax($trans2)
        return
            if (empty(map:keys($sgroups))) then $trans2
            else f:expandShax2RC($trans2, $sgroups)
   
    (: *** objectType => shape 
           pgroup     => shape
           choice     => xone 
           annotation => () :)   
    let $trans4 := f:expandShax3RC($trans3)
    
    (: *** property => pshape :)
    let $trans5 := f:expandShax4RC($trans4)
    
    (: *** datatype => shape :)    
    let $trans6 := f:expandShax5RC($trans5)

    (: *** pgroup => ...
           excludeProperties => ...
           @type, @base, @itemType => ... :)
    let $trans7 := 
        let $shapeNames := 
            $trans6//(shax:shape, shax:pshape)/@name/resolve-QName(., ..)
        return f:expandShax6RC($trans6, $shapeNames)
   
    let $DUMMY := file:write($i:DIR_DEBUG || 'DEBUG-trans1.xml', $trans1)   
    let $DUMMY := file:write($i:DIR_DEBUG || 'DEBUG-trans2.xml', $trans2)   
    let $DUMMY := file:write($i:DIR_DEBUG || 'DEBUG-trans3.xml', $trans3)   
    let $DUMMY := file:write($i:DIR_DEBUG || 'DEBUG-trans4.xml', $trans4)    
    let $DUMMY := file:write($i:DIR_DEBUG || 'DEBUG-trans5.xml', $trans5)    
    let $DUMMY := file:write($i:DIR_DEBUG || 'DEBUG-trans6.xml', $trans6)   
    let $DUMMY := file:write($i:DIR_DEBUG || 'DEBUG-trans7.xml', $trans7)    
    return $trans7
};        

(:~
 : Reports the substitution groups implied by a set of
 : SHAX documents. The result is a map representing each 
 : substitution group by an entry: the key is the qualified 
 : name of the group head, and the value is a sequence of 
 : the qualified names of all group members.
 :
 : @param SHAX a shax document
 : @return a map describing all substitution groups
 :)
declare function f:sgroupsFromShax($models as element(shax:models))
        as map(xs:QName, xs:QName+) {
    let $elems := $models/shax:model/shax:property
    let $elemsSGM := $elems[@substitutes]
    let $foldFunction :=
        function($accum as map(xs:QName, xs:QName+), $item as element(shax:property)) 
                as map(xs:QName, xs:QName+) {            
            let $groups := $item/@substitutes/tokenize(normalize-space(.), ' ') ! resolve-QName(., $item)
            return
                if (empty($groups)) then $accum
                else
                    let $itemName := resolve-QName($item/@name, $item)
                    return
                        map:merge((
                            $accum, $groups ! 
                                (map:entry(., ($accum(.), $itemName)))
                        ), map{"duplicates": "use-last"})
         }
    return
        fold-left($elems, map{}, $foldFunction)
};

(:~
 : Expansion of shax documents, step 1.
 :
 : Transformations:
 :    insert default values of @card
 :
 : The elements receiving default values are the elements representing
 : particles. These are the descendants of shax:objectType elements.
 :) 
declare function f:expandShax1($models as element(shax:models))
        as node()* {
    let $expanded1 :=
        for $model in $models/shax:model
        let $defaultCards := 
            map{"*": ($model/@defaultCard/string(), "1")[1], 
                "choice": "1",
                "pgroup": "1"
            }     
        return
            $model/f:expandShax1RC(., $defaultCards)
    return
        <shax:models count="{count($expanded1)}">{$expanded1}</shax:models>
};        

(:~
 : Recursive helper function of `expandShax1`.
 :) 
declare function f:expandShax1RC($n as node(), 
                                 $defaultCards as map(xs:string, xs:string))
        as node()* {
    typeswitch($n)
    
    case element() return
        let $card := $n/@card
        let $defaultCard :=
            if ($card) then ()
            else if (not($n/ancestor::shax:objectType)) then ()
            else 
                if ($n/self::shax:choice) then $defaultCards("choice")
                else if ($n/self::shax:pgroup) then $defaultCards("pgroup")
                else $defaultCards("*")
         
            
        return
            element {node-name($n)} {
                f:namespaceNodes($n),
                if ($card or not($defaultCard)) then () else attribute card {$defaultCard},
                for $a in $n/@* return f:expandShax1RC($a, $defaultCards),                
                for $c in $n/node() return f:expandShax1RC($c, $defaultCards)
            }
    default return $n                
};

(:~
 : Expansion of a shax document, step 2.
 :
 : Transformations:
 :    element uses (foo:bar) referencing an element declaration which is
 :    a substitution group head are replaced by a shax:choice element
 :    whose child elements represent the group members
 :
 : @TO.DO - take into account the @abstract annotation - abstract elements
 : are not included.
 : @TO.DO - if the group contains exactly one member which is not abstract,
 : replace the shax:choice by the child element representing that one member
 :) 
declare function f:expandShax2RC($n as node(), $sgroups as map(xs:QName, xs:QName+))
        as node()* {
    typeswitch($n)
    
    case element() return
        let $elemName := node-name($n)
        return
            (: case: element use references a substitution group head :)
            if (namespace-uri-from-QName($elemName) ne $f:URI_SHAX
                    and empty($n/parent::shax:model)
                    and $elemName = map:keys($sgroups)) then 
                let $groupMemberNames := ($elemName, $sgroups($elemName))
                let $cardAttribute := $n/@card
                let $models := $n/ancestor::shax:models/shax:model
                let $groupMembers := $models/shax:property[@name/resolve-QName(., ..) = $groupMemberNames]
                let $DUMMY := trace(count($groupMembers), 'COUNT_GROUP_MEMBERS: ')
                return
                    <shax:choice kind="substitutionGroup">{
                        f:namespaceNodes($n),
                        $cardAttribute,
                        for $groupMember in $groupMembers
                        let $name := $groupMember/@name/resolve-QName(., ..)
                        return
                            element {$name} {
                                for $a in $n/@* return f:expandShax2RC($a, $sgroups),
                                for $c in $n/node() return f:expandShax2RC($c, $sgroups)
                            }
                    }</shax:choice>
            else
                element {node-name($n)} {
                    f:namespaceNodes($n),
                    for $a in $n/@* return f:expandShax2RC($a, $sgroups),
                    for $c in $n/node() return f:expandShax2RC($c, $sgroups)
                }
    default return $n                
};

(:~
 : Expansion of a shax document, step 3.
 :
 : Transformations:
 :    objectType  => shape
 :    choice      => xone
 :    pgroup      => shape
 :                   (if pgroup child of shax:choice: 
 :                       also create: excludeProperties) 
 :) 
declare function f:expandShax3RC($n as node())
        as node()* {
    typeswitch($n)
    
    case element(shax:annotation) return ()
    
    (: shax:objectType => shax:shape :)
    case element(shax:objectType) return
        <shax:shape>{
            f:namespaceNodes($n),
            for $a in $n/@* return f:expandShax3RC($a),
            for $c in $n/node() return f:expandShax3RC($c)
        }</shax:shape>
        
    (: shax:choice - elaborate contents :)
    case element(shax:choice) return f:expandShax3RC_choice($n)
    
    (: shax:pgroup => shax:shape :)
    case element(shax:pgroup) return
        <shax:shape>{
            f:namespaceNodes($n),        
            let $atts := for $a in $n/@* return f:expandShax3RC($a)
            return (
                $atts,
                if ($n/parent::shax:choice and not($atts/self::attribute(docum))) then
                    attribute docum {"choice branch consisting of several properties"}
                else ()
            ),
            (: if 'pgroup' is child of 'choice' write 'excludeProperties' :)
            if (not($n/parent::shax:choice) or f:isParticleMultiple($n/..)) then 
                () 
            else
                let $myProps := $n//*[f:isPropertyNode(.)]/node-name()
                let $otherProps := $n/../(* except $n)
                    /descendant-or-self::*[f:isPropertyNode(.)]/node-name()
                return
                    <shax:excludeProperties iris="{$otherProps[not(. = $myProps)]}"/>,
             for $c in $n/node() return f:expandShax3RC($c)
        }</shax:shape>                    
        
    (: local property :)    
    case element() return
        let $elem :=
            element {node-name($n)} {
                f:namespaceNodes($n),            
                for $a in $n/@* return f:expandShax3RC($a),
                for $c in $n/node() return f:expandShax3RC($c)
            }
        return
            (: element not choid of 'choice': just copy :)        
            if (not($n/parent::shax:choice) or f:isParticleMultiple($n/..))then 
                $elem 
            (: element is child of 'choice': copy and add 'excludeProperties' :)                
            else
                let $otherProps := $n/../(* except $n)
                    /descendant-or-self::*[f:isPropertyNode(.)]/node-name()
                return
                    <shax:shape>{
                        f:namespaceNodes($n),
                        attribute docum {"choice branch consisting of a single property"},
                        <shax:excludeProperties iris="{$otherProps}"/>,
                        $elem
                    }</shax:shape>
            
    default return $n            
};        

declare function f:expandShax3RC_choice($n as node())
        as node()* {
    let $cardinalityRange := f:getCardinalityRange($n)
    let $contents := (
        for $a in $n/@* return f:expandShax3RC($a),
        for $c in $n/node() return f:expandShax3RC($c)
    )
    return
        (: case 1: maxOccurs of choice > 1 => branches not mutually exclusive
                                           => mapping to pgroup, not xone :)
        if ($cardinalityRange[2] > 1 or $cardinalityRange[2] < 0) then                                           
            let $contents_atts := $contents[self::attribute()]
            let $contents_children := $contents except $contents_atts
            
            (: unwrap the children of shax:shape children, 
                  which have been obtained for shax:pgroup elements :)
            let $contents_children_adapted1 :=
                for $c in $contents_children
                return
                    (: shax:shape => unwrap, multiplying multiplicities :)
                    if ($c/self::shax:shape) then
                        for $cc in $c/*
                        (: unwrapping requires multiplication of cardinality ranges :)                        
                        let $cardAdaptedAtt := 
                            (: let $cardAdapted := f:multiplyCardinalityRanges($n, $c) :) (: 20171025, hjr :)
                            let $cardAdapted := f:multiplyCardinalityRanges($c, $cc)                            
                            return attribute card {$cardAdapted}[$cardAdapted]
                        return
                            element {node-name($cc)} {
                                $cc/(@* except @card), 
                                $cardAdaptedAtt, 
                                $cc/node()
                            } 
                    (: not shax:shape => nothing to unwrap :)
                    else $c
                    
            (: adapt the cardinality by combining the cardinalites of choice and branches :)                    
            let $contents_children_adapted2 :=
                for $c in $contents_children_adapted1
                let $cardAdaptedAtt := 
                    let $cardAdapted := f:multiplyCardinalityRanges($n, $c)
                    return attribute card {$cardAdapted}[$cardAdapted]
                return
                    element {node-name($c)} {
                        $c/(@* except @card), 
                        $cardAdaptedAtt, 
                        $c/node()
                    }
            return (
                $contents_children_adapted2
                (: @TO.DO - clarify if $contents_atts can be dropped - where should they be attached? :)
            )
        else
        
        (: case 2: maxOccurs of choice = 1 => branches are mutually exclusive :)
        
    let $contents_atts := $contents[self::attribute()]
    let $contents_children := $contents except $contents_atts
    return
        <shax:xone docum="choice between properties and/or property groups">{
            f:namespaceNodes($n),        
            $contents_atts,
                
            (: express the possibility to NOT use the choice :)    
            if (not($cardinalityRange[1] eq 0)) then () 
            else
                let $choiceProps := $n/descendant-or-self::*[f:isPropertyNode(.)]/node-name()
                return
                    <shax:shape>{
                        f:namespaceNodes($n),
                        attribute docum {"choice branch representing NOT using the choice"},
                        <shax:excludeProperties iris="{$choiceProps}"/>
                    }</shax:shape>,
                
            (: the choice branches :)
            $contents_children
        }</shax:xone>
};

(:~
 : Expansion of a shax document, step 4.
 :
 :    foo:bar       => pshape
 :
 : Content:
 : - @name                              # the property name
 : - @type |                            # references a datatype or object type
 :     @base, @facet*, value-elem* |    # local definition of a datatype
 :     @class, property-elems           # local definition of an object type
 : - @kind                              # constraint - IRI, blank node, both?
 :
 : Note:
 : - @type should not be combined with @class, as @class should be part 
 :         of the type definition
 : - @kind can be used in order to allow blank nodes or to enforce IRIs
 :) 
declare function f:expandShax4RC($n as node())
        as node()* {
    typeswitch($n)
    case element(shax:shape) | 
         element(shax:xone) | 
         element(shax:pgroup) | 
         element(shax:dataType) 
    return
        element {node-name($n)} {
            f:namespaceNodes($n),        
            for $a in $n/@* return f:expandShax4RC($a),
            for $c in $n/node() return f:expandShax4RC($c)
        }
        
    case element() return
        if ($n/self::shax:*) then
            element {node-name($n)} {
                f:namespaceNodes($n),            
                for $a in $n/@* return f:expandShax4RC($a),
                for $c in $n/node() return f:expandShax4RC($c)
            }
            
        (: property element :)
        else
            let $propUse := $n
            (: property reference is implicit - absence of @type :)
            let $propRef :=
                if ($propUse/@type) then () 
                else
                    $propUse
                    /ancestor::shax:models/shax:model
                    /shax:property[resolve-QName(@name, .) eq node-name($n)]

            let $propDecl := ($propRef, $propUse)[1]
            
            let $kindAtt := $propDecl/@kind/attribute nodeKind {i:shaclNodeKind(.)}
            let $path := ($propRef/@name/resolve-QName(., ..), node-name($propUse))[1]
            let $baseAtt := $propDecl/@base/attribute base {.}            
            let $typeAtt := $propDecl/@type/attribute type {.}            
            let $classAtt := $propDecl/@class/attribute class {.}     
            let $orderedAtt := $propDecl/@ordered/attribute ordered {.}
            let $facetAtts := f:getShaclFacets_atts($propDecl)            
            let $facetElems := f:getShaclFacets_elems($propDecl)
            let $cardAtt := f:getPropertyCardinality($propUse)
            return                
                <shax:pshape path="{$path}">{
                    f:namespaceNodes($propDecl),   
                    $baseAtt,
                    $typeAtt,
                    $kindAtt,
                    $classAtt,
                    $cardAtt,
                    $orderedAtt,
                    $facetAtts,
                    $facetElems
                }</shax:pshape>
    case attribute(card) return
        f:getPropertyCardinality($n/..)
    default return $n                
};        

(:~
 : Expansion of a shax document, step 5.
 :
 :    *:dataType  => shape
 :
 : The shape has a type and an optional base attribute, as
 : well as facet attributes.
 :) 
declare function f:expandShax5RC($n as node())
        as node()? {
    typeswitch($n)
    case element(shax:dataType) return
        let $baseAtt := $n/@base/attribute base {.} 
        let $itemTypeAtt := $n/@itemType/attribute itemType {.}        
        let $containerAtt := $n/@container/attribute container {.}        
        let $memberTypesAtt := $n/@memberTypes/attribute memberTypes {.}        
        let $typeAtt := $n/@type/attribute type {.}
        let $minSizeAtt := $n/@minSize/attribute minSize {.}
        let $maxSizeAtt := $n/@maxSize/attribute maxSize {.}        
        let $facetAtts := f:getShaclFacets_atts($n)
        let $facetElems := f:getShaclFacets_elems($n)        
        return
            <shax:shape name="{$n/@name}">{
                f:namespaceNodes($n),
                $baseAtt,
                $itemTypeAtt, 
                $containerAtt,
                $memberTypesAtt,
                $minSizeAtt,
                $maxSizeAtt,
                $typeAtt,
                $facetAtts,
                $facetElems
            }</shax:shape>
        
    case element() return
        element {node-name($n)} {
            f:namespaceNodes($n),        
            for $a in $n/@* return f:expandShax5RC($a),
            for $c in $n/node() return f:expandShax5RC($c)
        }
    default return $n        
};        

(:~
 : Expansion of a shax document, step 6.
 :
 :    pgroup            => <and>...</and> | 
 :                         <shape><and>...</and></shape>
 
 :    excludeProperties => 
 :    <not><pshape/></not> 
 :    |
 :    <shape><not><pshape/></not></shape>
 :    | 
 :    <not>
 :       <shape>
 :          <or>
 :             <pshape/>...
 :          </or>
 :       </shape>
 :    </not> 
 :    |
 :    <shape>...</shape>
 :
 :    @type             => @datatype | @node
 :    @base             => @datatype | @node
 :    @itemType         => @itemDatatype | @itemNode 
 :
 : If the @type/@base/@itemType/@itemBase contains the name 
 : of a shape, it is translated into a @node (@itemNode) constraint, 
 : otherwise into a @datatype (@itemDatatype) constraint.
 :
 : @param n a node to be processed recursively
 : @param shapeNames the names of all shapes
 : @return the processed node
 :) 
declare function f:expandShax6RC($n as node(), $shapeNames as xs:QName*)
        as node()? {
    typeswitch($n)
    
    (: shax:pgroup => 
                      <shax:and>...</shax:and> | 
          <shax:shape><shax:and>...</shax:and></shax:shape> :)
    case element(shax:pgroup) return
        let $and :=
            <shax:and docum="choice branch comprising several properties">{
                for $a in $n/@* return f:expandShax6RC($n, $shapeNames),
                for $c in $n/node() return f:expandShax6RC($c, $shapeNames)            
            }</shax:and>
        return
            if ($n/(parent::shax:shape, parent::shax:pshape)) then $and
            else 
                <shax:shape docum="choice branch comprising a single property">{$and}</shax:shape>
                
    (: shax:excludeProperties => 
                      <shax:not><shax:pshape/></shax:not> |
          <shax:shape><shax:not><shax:pshape/></shax:not></shax:shape> |
           
                      <shax:not><shax:shape><shax:or>...</shax:or></shax:shape></shax:not> |
          <shax:shape><shax:not><shax:shape><shax:or>...</shax:or></shax:shape></shax:not></shax:shape>
     :)
    case element(shax:excludeProperties) return
        let $names := $n/@iris/tokenize(normalize-space(.), ' ') ! resolve-QName(., $n)
        let $not :=
            <shax:not docum="excludes the properties of alternative choice branches">{
                if (count($names) eq 1) then
                    <shax:pshape path="{$names}" minCount="1"/>
                else
                    <shax:shape>{                
                        <shax:or>{
                            $names ! <shax:pshape path="{.}" minCount="1"/>
                        }</shax:or>
                    }</shax:shape>
            }</shax:not>
        return
            if ($n/(parent::shax:shape, parent::shax:pshape)) then $not
            else <shax:shape>{$not}</shax:shape>
                
    case element() return
        element {node-name($n)} {
            f:namespaceNodes($n),        
            for $a in $n/@* return f:expandShax6RC($a, $shapeNames),
            for $c in $n/node() return f:expandShax6RC($c, $shapeNames)
        }
        
    (: @type/@base references a shape => @node constraint
       otherwise => @datatype constraint
     :)
    case attribute(type) | attribute(base) return
        let $qname := resolve-QName($n, $n/..)
        return
            if ($qname = $shapeNames) then attribute node {$n}
            else attribute datatype {$n}
            
    (: @itemType references a shape => @itemNode constraint
       otherwise => @itemDatatype constraint
     :)
    case attribute(itemType) return
        let $qname := resolve-QName($n, $n/..)
        return
            if ($qname = $shapeNames) then attribute itemNode {$n}
            else attribute itemDatatype {$n}
    default return $n        
};     

(:~
 : Constructs canonical shax attributes expressing type
 : facets.
 :
 : @param elem a shax element
 : @return shax attributes
 :)
declare function f:getShaclFacets_atts($elem as element())
        as attribute()* {
    $elem/(@minInclusive, @min)[1]/attribute minInclusive {.},
    $elem/(@maxInclusive, @max)[1]/attribute maxInclusive {.},       
    $elem/(@minExclusive, @minEx)[1]/attribute minExclusive {.},
    $elem/(@maxExclusive, @minEx)[1]/attribute maxExclusive {.},
    $elem/(@minLength, @minLen)[1]/attribute minLength {.},
    $elem/(@maxLength, @maxLen)[1]/attribute maxLength {.},       
    $elem/@pattern/attribute pattern {.},        
    $elem/@flags/attribute flags {.}        
};

(:~
 : Constructs canonical shax elements expressing type
 : facets.
 :
 : @param elem a shax element
 : @return shax elements
 :)
declare function f:getShaclFacets_elems($elem as element())
        as element()* {
    $elem/shax:value
};

(:~
 : Returns true if a given element is a shax property node.
 :
 : @param elem the node to be checked
 : @return true if the element is a property node
 :)
declare function f:isPropertyNode($elem as element())
        as xs:boolean {
    not($elem/self::shax:*)        
};

(:~
 : Returns SHACL attributes 'minCount' and/or 'maxCount' which
 : express the cardinality constraints of a property shape.
 :
 : @param p a shax property element
 : @return a 'minCount' and/or a 'maxCount' attribute
 :)
declare function f:getPropertyCardinality($p as element())
        as attribute()* {
    let $ecard := ($p/@card, $p/ancestor::*[@defaultCard][1]/@defaultCard)[1]
    return
        if (not($ecard)) then (
            attribute minCount {0},
            attribute maxCount {-1}
        )
        else if ($ecard eq '?') then (
            attribute minCount {0},
            attribute maxCount {1}
        )
        else if ($ecard eq '+') then (
            attribute minCount {1},
            attribute maxCount {-1}
        )
        else if ($ecard eq '*') then (
            attribute minCount {0},
            attribute maxCount {-1}
        )        
        else if ($ecard/matches(., '^\s*\d+\s*$')) then (
            attribute minCount {normalize-space($ecard)},
            attribute maxCount {normalize-space($ecard)}
        ) 
        else if ($ecard/matches(., '^\s*\d+\s*-\s*\d+\s*$')) then
            let $limits := 
                tokenize(replace($ecard, '\s*(\d+)\s*-\s*(\d+)\s*$', '$1#$2'), '#')
            return (                        
                attribute minCount {$limits[1]},
                attribute maxCount {$limits[2]}
            )
        else error(QName((), 'SYNTAX_ERROR'), concat('Invalid cardinality: ', $ecard))        
};        

