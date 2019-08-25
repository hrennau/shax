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
         <param name="shax" type="docFOX" sep="SC" fct_minDocCount="1" pgroup="input"/>        
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
    let $trans0 := f:loadShax($models)
    
    (: *** add @card attributes (making default values explicit) :)
    let $trans1 := f:expandShax1($trans0)        
    
    (: *** replace references to substitution group heads by choice groups :)
    let $trans2 := f:expandShax2($trans1)
   
    (: *** objectType => shape 
           pgroup     => shape
           choice     => xone 
           annotation => () :)   

    let $trans3 := f:expandShax31($trans2)
    
    (: *** property => pshape :)
    let $trans4 := f:expandShax4($trans3)
    
    (: *** datatype => shape :)    
    let $trans5 := f:expandShax5($trans4)

    (: *** pgroup => ...
           excludeProperties => ...
           @type, @base, @itemType => ... :)
    let $trans6 := f:expandShax6($trans5)

    let $DUMMY := i:writeDebugXml(1, 'DEBUG-trans0.xml', $trans0)   
    let $DUMMY := i:writeDebugXml(1, 'DEBUG-trans1.xml', $trans1)   
    let $DUMMY := i:writeDebugXml(1, 'DEBUG-trans2.xml', $trans2)   
    let $DUMMY := i:writeDebugXml(1, 'DEBUG-trans3.xml', $trans3)    
    let $DUMMY := i:writeDebugXml(1, 'DEBUG-trans4.xml', $trans4)    
    let $DUMMY := i:writeDebugXml(1, 'DEBUG-trans5.xml', $trans5)   
    let $DUMMY := i:writeDebugXml(1, 'DEBUG-trans6.xml', $trans6)    
    return $trans6
};        

(:~
 : Expansion of shax documents, step 1.
 :
 : Transformations:
 :    insert @card attributes with default cardinality constraint
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
                $n/@* ! f:expandShax1RC(., $defaultCards),                
                $n/node() ! f:expandShax1RC(., $defaultCards)
            }
    case text() return if (not($n/matches(., '\S')) and $n/../*) then () else $n            
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
declare function f:expandShax2($models as element(shax:models))
        as node()* {
    let $sgroups := f:sgroupsFromShax($models)
    return
        if (empty(map:keys($sgroups))) then $models
        else f:expandShax2RC($models, $sgroups)        
};

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
                let $cardAttributeChoice := $n/@card
                (: 20190310, hjr: bugfix - choice members must have cardinality 1.1 :)
                let $cardAttributeMember := attribute card {"1"}
                let $models := $n/ancestor::shax:models/shax:model
                let $groupMembers := $models/shax:property[@name/resolve-QName(., ..) = $groupMemberNames]
                let $_DEBUG := trace(count($groupMembers), 'COUNT_GROUP_MEMBERS: ')
                return
                    <shax:choice kind="substitutionGroup">{
                        f:namespaceNodes($n),
                        $cardAttributeChoice,
                        for $groupMember in $groupMembers
                        let $name := $groupMember/@name/resolve-QName(., ..)
                        return
                            element {$name} {
                                for $a in $n/(@* except @card) return f:expandShax2RC($a, $sgroups),
                                $cardAttributeMember,
                                for $c in $n/node() return f:expandShax2RC($c, $sgroups)
                            }
                    }</shax:choice>
            else
                element {node-name($n)} {
                    f:namespaceNodes($n),
                    for $a in $n/@* return f:expandShax2RC($a, $sgroups),
                    for $c in $n/node() return f:expandShax2RC($c, $sgroups)
                }
    case text() return if (not($n/matches(., '\S')) and $n/../*) then () else $n                
    default return $n                
};

(:~
 : Expansion of a shax document, step 3.
 :) 
declare function f:expandShax31($models as element(shax:models))
        as node()* {
    let $trans31 := $models        
    let $trans31a := f:expandShax31aRC($trans31)
    let $trans31b := f:expandShax31bRC($trans31a)
    let $trans31c := f:expandShax31cRC($trans31b)
    let $trans31d := f:expandShax31dRC($trans31c)
    let $trans31e := f:expandShax31eRC($trans31d)
    let $trans31f := f:expandShax31fRC($trans31e)
    let $trans31g := f:expandShax31gRC($trans31f)
    let $trans31h := f:expandShax31hRC($trans31g)    
    
    let $DUMMY := i:writeDebugXml(1, 'DEBUG-trans3a.xml', $trans31a) (: choices merged :)
    let $DUMMY := i:writeDebugXml(1, 'DEBUG-trans3b.xml', $trans31b) (: cbranches wrapped :)   
    let $DUMMY := i:writeDebugXml(1, 'DEBUG-trans3c.xml', $trans31c) (: multiple choices made pgroups :)    
    let $DUMMY := i:writeDebugXml(1, 'DEBUG-trans3d.xml', $trans31d) (: multiple pgroups made singular :)
    let $DUMMY := i:writeDebugXml(1, 'DEBUG-trans3e.xml', $trans31e) (: nested pgroups with card=1 are merged :)    
    let $DUMMY := i:writeDebugXml(1, 'DEBUG-trans3f.xml', $trans31f) (: add @excludeProperties :)
    let $DUMMY := i:writeDebugXml(1, 'DEBUG-trans3g.xml', $trans31g) (: shax:choice -> shax:xone, shax:alternative -> shax:shape :)
    let $DUMMY := i:writeDebugXml(1, 'DEBUG-trans3h.xml', $trans31h) (: shax:group unwrapped  :)    
    return
        $trans31h
};

(:~
 : Unwrap shax:choice children of shax:choice if they
 : have the same @card.
 :)
declare function f:expandShax31aRC($n as node())
        as node()* {
    typeswitch($n)
    
    case element(shax:choice) return
        let $expandedAtts := $n/@* ! f:expandShax31aRC(.)
        let $expandedChildren := $n/node() ! f:expandShax31aRC(.)        
        let $expandedItems := ($expandedAtts, $expandedChildren)
        let $atts := $expandedItems[self::attribute()]
        let $children := ($expandedItems except $atts)
        return
            element {node-name($n)} {
                f:namespaceNodes($n),
                $atts,
                for $child in $children
                return
                    if ($child/self::shax:choice and $child/@card eq $n/@card) then $child/*
                    else $child
            }
    case element() return
        element {node-name($n)} {
            f:namespaceNodes($n),
            $n/@* ! f:expandShax31aRC(.),
            $n/node() ! f:expandShax31aRC(.)
        }
    case text() return if (not($n/matches(., '\S')) and $n/../*) then () else $n        
    default return $n        
};        


(:~
 : Transformations:
 :    objectType         => <shape>...</shape>
 :    x[parent::choice]  => <alternative><x>...</x></alternative>
 :    annotation         => ()
 :    @class             => @class, @targetClass
 :) 
declare function f:expandShax31bRC($n as node())
        as node()* {
    typeswitch($n)
    
    case element(shax:annotation) return ()
    
    (: shax:objectType => shax:shape :)
    case element(shax:objectType) return
        <shax:shape>{
            f:namespaceNodes($n),
            $n/@* ! f:expandShax31bRC(.),
            $n/node() ! f:expandShax31bRC(.)
        }</shax:shape>
        
    (: shax:choice - wrap each branch in a shax:alternative :)
    case element(shax:choice) return 
        element {node-name($n)} {
            f:namespaceNodes($n),
            $n/@* ! f:expandShax31bRC(.),        
            $n/* ! <shax:alternative>{f:expandShax31bRC(.)}</shax:alternative>
        }
(:
    (: already now transform ? :)
    case element(shax:pgroup) return 
        <shax:shape> {
            f:namespaceNodes($n),
            $n/@* ! f:expandShax31bRC(.),
            $n/node() ! f:expandShax31bRC(.)
        }</shax:shape>            
:)
    case element() return 
        element {node-name($n)} {
            f:namespaceNodes($n),
            $n/@* ! f:expandShax31bRC(.),
            $n/node() ! f:expandShax31bRC(.)
        }            

    case attribute(class) return (
        $n,
        $n[parent::shax:objectType]/attribute targetClass {.}
    )
    
    case text() return if (not($n/matches(., '\S')) and $n/../*) then () else $n
    default return $n            
};        

(:~
 : Mapping choices with maxCard>1 to <pgroup>. Rules:
 : - each child of an alternative is transformed and its cardinality 
 :     is adapted (min=0, max=max(item) * max(choice)) 
 : - if a transformed child is a choice with a new maxCard>1, the
 :     transformed child is transformed again (choice -> pgroup)
 :)
declare function f:expandShax31cRC($n as node())
        as node()* {
    typeswitch($n)
    case $choice as element(shax:choice) return
        if ($choice/not(f:isParticleMultiple(.))) then f:expandShax31cRC_copy($choice)
        else
            let $ctrans := $choice/shax:alternative/*/f:expandShax31cRC(.)
            let $ctrans2 := 
                for $item in $ctrans
                (: new card: min=0, max=product :)
                let $newCard := f:multiplyMaxCardinality($choice, $item)
                return
                    if ($item/@card eq $newCard) then $item 
                    else
                        let $adapted :=
                            element {node-name($item)} {
                               $item/(@* except @card), 
                                attribute card {$newCard}, 
                                $item/node()
                            }
                        return
                            if ($adapted/self::shax:choice[f:isParticleMultiple(.)]) then 
                                f:expandShax31cRC($adapted)
                            else $adapted
            return <shax:pgroup card="1" fromChoiceWithCard="{$choice/@card}">{$ctrans2}</shax:pgroup>
        
    case element() return f:expandShax31cRC_copy($n)
        
    case text() return if (not($n/matches(., '\S')) and $n/../*) then () else $n        
    default return $n
};    

(:~
 : Transforms pgroups with maxCard>1 into pgroups with card=1.
 :)
declare function f:expandShax31dRC($n as node())
        as node()* {
    typeswitch($n)
    case $pgroup as element(shax:pgroup) return
        if ($pgroup/not(f:isParticleMultiple(.))) then f:expandShax31dRC_copy($pgroup)
        else
            let $ctrans := $pgroup/*/f:expandShax31dRC(.)
            let $ctrans2 := 
                for $item in $ctrans
                (: new card: min=0, max=product :)
                let $newCard := f:multiplyCardinalityRanges($pgroup, $item)
                return
                    if ($item/@card eq $newCard) then $item 
                    else
                        let $adapted :=
                            element {node-name($item)} {
                               $item/(@* except @card), 
                                attribute card {$newCard}, 
                                $item/node()
                            }
                        return
                            if ($adapted/self::shax:pgroup[f:isParticleMultiple(.)]) then 
                                f:expandShax31dRC($adapted)
                            else $adapted
            return <shax:pgroup card="1" fromPgroupWithCard="{$pgroup/@card}">{$ctrans2}</shax:pgroup>
        
    case element() return f:expandShax31dRC_copy($n)
    
    case text() return if (not($n/matches(., '\S')) and $n/../*) then () else $n    
    default return $n
};    

(:~
 : Merges nested pgroups.
 :
 : Note that the pgroups are known to have card=1.
 :)
declare function f:expandShax31eRC($n as node())
        as node()* {
    typeswitch($n)
    case $pgroup as element(shax:pgroup) return
        let $content := $pgroup/node() ! f:expandShax31eRC(.)
        return
            if ($pgroup/parent::shax:pgroup) then $content
            else
                element {node-name($pgroup)} {
                    f:namespaceNodes($n),
                    $pgroup/@* ! f:expandShax31eRC(.),
                    $content
                }
    case $elem as element() return
        element {node-name($elem)} {
            f:namespaceNodes($n),        
            $elem/@* ! f:expandShax31eRC(.),
            $elem/node() ! f:expandShax31eRC(.)
        }
        
    case text() return if (not($n/matches(., '\S')) and $n/../*) then () else $n        
    default return $n
};    

(:~
 : Augment <alternative>s with <excludeProperties>.
 :)
declare function f:expandShax31fRC($n as node())
        as node()* {
    typeswitch($n)
    case $alter as element(shax:alternative) return
        let $ownProperties := $alter//(* except shax:*)
        let $ownPropertyIris := $ownProperties/node-name(.) => distinct-values()
        let $alternativeProperties := $alter/../(shax:alternative except $alter)//(* except shax:*)
        let $alternativePropertyIris := $alternativeProperties/node-name(.) => distinct-values()
        let $excludedProperties := $alternativePropertyIris[not(. = $ownPropertyIris)]
        let $content := (
            $alter/@* ! f:expandShax31fRC(.),
            $alter/node() ! f:expandShax31fRC(.)
        )
        let $contentAtts := $content[self::attribute()]
        let $contentChildren := $content except $contentAtts
        return
            element {node-name($alter)} {
                f:namespaceNodes($n),
                $contentAtts,
                <shax:excludeProperties iris="{$excludedProperties}"/>,
                $contentChildren
            }
    case $elem as element() return
        element {node-name($elem)} {
            f:namespaceNodes($n),        
            $elem/@* ! f:expandShax31fRC(.),
            $elem/node() ! f:expandShax31fRC(.)
        }
        
    case text() return if (not($n/matches(., '\S')) and $n/../*) then () else $n        
    default return $n
};    

(:~
 : Mapping shax:choice to shax:xone, shax:alternative to shax:shape.
 :)
declare function f:expandShax31gRC($n as node())
        as node()* {
    typeswitch($n)
    case $choice as element(shax:choice) return
        <shax:xone docum="choice between properties and/or property groups">{
            f:namespaceNodes($choice),
            $choice/(@* except $choice/@docum) ! f:expandShax31gRC(.),
            $choice/node() ! f:expandShax31gRC(.)
        }</shax:xone>
        
    case $alter as element(shax:alternative) return
        <shax:shape shax:anno="alternative">{
            f:namespaceNodes($alter),
            $alter/@* ! f:expandShax31gRC(.),
            $alter/node() ! f:expandShax31gRC(.)
        }</shax:shape>
        
    case $elem as element() return
        element {node-name($elem)} {
            f:namespaceNodes($elem),
            $elem/@* ! f:expandShax31gRC(.),
            $elem/node() ! f:expandShax31gRC(.)
    }

    case text() return if (not($n/matches(., '\S')) and $n/../*) then () else $n
    default return $n
};    

(:~
 : Unwrapping shax:pgroup which is child of shax:pgroup or shax:shape.
 :)
declare function f:expandShax31hRC($n as node())
        as node()* {
    typeswitch($n)
    case $pgroup as element(shax:pgroup) return
        let $contents := $pgroup/node() ! f:expandShax31hRC(.)
        return        
            if ($pgroup/(parent::shax:shape, parent::shax:pgroup)) then
            (: if ($pgroup/(parent::shax:shape, parent::shax:pgroup) and $pgroup/@card eq '1') then :)            
                $contents
            else
                element {node-name($pgroup)} {
                    f:namespaceNodes($pgroup),
                    $pgroup/@* ! f:expandShax31hRC(.),                    
                    $contents
                }
        
    case $elem as element() return
        element {node-name($elem)} {
            f:namespaceNodes($elem),
            $elem/@* ! f:expandShax31hRC(.),
            $elem/node() ! f:expandShax31hRC(.)
    }

    case text() return if (not($n/matches(., '\S')) and $n/../*) then () else $n
    default return $n
};    

declare function f:expandShax31cRC_copy($e as element())
        as element() {
    element {node-name($e)} {
        f:namespaceNodes($e),
        $e/@* ! f:expandShax31cRC(.),
        $e/node() ! f:expandShax31cRC(.)
    }
};

declare function f:expandShax31dRC_copy($e as element())
        as element() {
    element {node-name($e)} {
        f:namespaceNodes($e),
        $e/@* ! f:expandShax31dRC(.),
        $e/node() ! f:expandShax31dRC(.)
    }
};

declare function f:expandShax4($models as element(shax:models))
        as node()* {
    f:expandShax4RC($models)        
};

(:~
 : Expansion of a shax document, step 4.
 :
 :    <foo:bar>  => <pshape path="foo:bar">
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
            
            (: @kind - SHACL node kind (e.g. sh:BlankNodeOrIRI :)
            let $kindAtt := $propDecl/@kind/attribute nodeKind {i:shaclNodeKind(.)}
            
            (: @path - the property IRI :)
            let $path := $propDecl/(@name/resolve-QName(., ..), node-name($propUse))[1]
            
            (: @base - base type :)
            let $baseAtt := $propDecl/@base/attribute base {.}

            (: @type - data type :)
            let $typeAtt := $propDecl/@type/attribute type {.}

            (: @class - rdf CLASS :)
            let $classAtt := $propDecl/@class/attribute class {.}    
            
            (: @ordered - SHACL constraint 'ordered' :)
            let $orderedAtt := $propDecl/@ordered/attribute ordered {.}
            
            (: facet attributes :)
            let $facetAtts := f:getShaclFacets_atts($propDecl)

            (: facet element :)
            let $facetElems := f:getShaclFacets_elems($propDecl)
            
            (: @minCount, @maxCount :)
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
                
    (: @card :)            
    case attribute(card) return f:getPropertyCardinality($n/..)
    
    case text() return if (not($n/matches(., '\S')) and $n/../*) then () else $n    
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
declare function f:expandShax5($models as element(shax:models))
        as node()* {
    f:expandShax5RC($models)        
};

(:~
 : Recursive helper function of f:expandShax5
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

declare function f:expandShax6($models as element(shax:models))
        as node()* {
    let $shapeNames := $models//(shax:shape, shax:pshape)/@name/resolve-QName(., ..)
    return f:expandShax6RC($models, $shapeNames)        
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
    return if (empty($elems/@substitutes)) then map{} else
    
    let $foldFunction :=
        function($accum as map(xs:QName, xs:QName+), $item as element(shax:property)) 
                as map(xs:QName, xs:QName+) {            
            let $groups := $item/@substitutes/tokenize(normalize-space(.), ' ') ! resolve-QName(., $item)
            return
                if (empty($groups)) then $accum else
                
                (: add this property name to all substituted groups :)
                let $itemName := resolve-QName($item/@name, $item)
                return
                    map:merge((
                        $accum, 
                        $groups ! (map:entry(., ($accum(.), $itemName)))
                    ), map{"duplicates": "use-last"})
         }
    return
        fold-left($elems, map{}, $foldFunction)
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

(:

(:
======================================================================================================================
:)
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
declare function f:expandShax3($models as element(shax:models))
        as node()* {
    let $models := f:expandShax3aRC($models)
    return
        f:expandShax3RC($models)
};

(:~
 : Unwrap shax:choice children of shax:choice if they
 : have the save @card.
 :)
declare function f:expandShax3aRC($n as node())
        as node()* {
    typeswitch($n)
    
    case element(shax:choice) return
        let $expandedAtts := $n/@* ! f:expandShax3aRC(.)
        let $expandedChildren := $n/node() ! f:expandShax3aRC(.)
        
        let $expanded := ($expandedAtts, $expandedChildren)
        let $atts := $expanded[self::attribute()]
        let $children := ($expanded except $atts)
        return
            element {node-name($n)} {
                f:namespaceNodes($n),
                $atts,
                for $child in $children
                return
                    if ($child/self::shax:choice and $child/@card eq $n/@card) then $child/*
                    else $child
            }
    case element() return
        element {node-name($n)} {
            f:namespaceNodes($n),
            $n/@* ! f:expandShax3aRC(.),
            $n/node() ! f:expandShax3aRC(.)
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
                let $otherProps := $n/parent::shax:choice/(* except $n)
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
            (: element not child of 'choice': just copy :)        
            if (not($n/parent::shax:choice) or f:isParticleMultiple($n/..))then 
                $elem 
            (: element is child of 'choice': copy and add 'excludeProperties' :)                
            else
                let $otherProps := $n/parent::shax:choice/(* except $n)
                    /descendant-or-self::*[f:isPropertyNode(.)]/node-name()
                return
                    <shax:shape>{
                        f:namespaceNodes($n),
                        attribute docum {"choice branch consisting of a single property"},
                        <shax:excludeProperties iris="{$otherProps}"/>,
                        $elem
                    }</shax:shape>
            
    case attribute(class) return (
        $n,
        $n[parent::shax:objectType]/attribute targetClass {.}
    )
    default return $n            
};        

(:~
 : A shax:choice is mapped to a shax:xone.
 :
 : Rules for the construction of the choice branches:
 : @TO.DO - ADD DOCUMENTATION
 :)
declare function f:expandShax3RC_choice($n as node())
        as node()* {
    let $cardinalityRange := f:getCardinalityRange($n)
    let $contents := (
        for $a in $n/@* return f:expandShax3RC($a),
        for $c in $n/node() return f:expandShax3RC($c)
    )
    return
        (: case 1: choice.maxOccurs > 1
           ==================================
           => branches not mutually exclusive
           => mapping to pgroup, not xone 
         :)
        if ($cardinalityRange[2] > 1 or $cardinalityRange[2] < 0) then        
            let $contents_atts := $contents[self::attribute()]
            let $cbranches := $contents except $contents_atts
            
            (: unwrap the children of shax:shape children, 
                  which have been obtained for shax:pgroup elements :)
            let $cbranches_adapted1 :=
                for $cbranch in $cbranches[self::element()]
                return
                    (: shax:shape => unwrap, multiplying multiplicities :)
                    if ($cbranch/self::shax:shape) then
                        let $_DEBUG := trace((), 'CHOICE WITH MAXOCCURS>1 - UNWRAP CHILD SHAPES')
                        for $particle in $cbranch/*
                        (: unwrapping requires multiplication of cardinality ranges :)                        
                        let $cardAdaptedAtt := 
                            let $cardAdapted := f:multiplyCardinalityRanges($cbranch, $particle)                            
                            return attribute card {$cardAdapted}[$cardAdapted]
                        return
                            element {node-name($particle)} {
                                $particle/(@* except @card), 
                                $cardAdaptedAtt, 
                                $particle/node()
                            } 
                    (: not shax:shape => nothing to unwrap :)
                    else $cbranch
                    
            (: adapt the cardinality by combining the cardinalites of choice and branches :)         
            
            (: PROBLEM:
               if the parent of $n is a shax:choice with maxCard=1: 
               $n must be mapped to a shape which excludes the contents of the other parent choice branches :)
            let $cbranches_adapted2 :=
                for $c in $cbranches_adapted1
                let $cardAdaptedAtt := 
                    let $cardAdapted := f:multiplyMaxCardinality($n, $c)
                    return attribute card {$cardAdapted}[$cardAdapted]
                return
                    element {node-name($c)} {
                        $c/(@* except @card), 
                        $cardAdaptedAtt, 
                        $c/node()
                    }
            return (
                (: case: this choice is child of a parent choice with maxOccurs=1 
                   => the properties must be wrapped in a shape which
                      excludes the properties of the parent-sibling choice branches
                 :)
                if ($n/parent::shax:choice/f:getCardinalityRange(.)[1] le 1) then
                    let $myProps := $n//*[f:isPropertyNode(.)]/node-name()
                    let $otherProps := $n/parent::shax:choice/(* except $n)
                        /descendant-or-self::*[f:isPropertyNode(.)]/node-name()
                    let $excludedProps := $otherProps[not(. = $myProps)]                        
                    return
                        <shax:shape shax:anno="repeatable-choice">{
                            <shax:excludeProperties iris="{$excludedProps}"/>,
                            $cbranches_adapted2
                        }</shax:shape>
                else
                    <shax:shape shax:anno="repeatable-choice">{
                        $cbranches_adapted2
                    }</shax:shape>                        
                (: @TO.DO - clarify if $contents_atts can be dropped - where should they be attached? :)
            )
        else
        
        (: case 2: choice.maxOccurs = 1
           =====================================
           => branches are mutually exclusive 
         :)
        
    let $contents_atts := $contents[self::attribute()]
    let $cbranches := $contents except $contents_atts
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
            $cbranches
        }</shax:xone>
};


(:
============================================================================================================
:)
(:
(:~
 : Edits multipls choices. Rules:
 :
 : (a) new cardinality = 
 :     current cardinality if one is true:
 :     - current cardinality not set
 :     - $multiplyMaxCardWith not set 
 :     (0 - $multiplyMaxCardWith) if all is true:
 :     - current name = shax:choice
 :     - current maxCard>1
 :     otherwise: (0 - current maxCard * $multiplyMaxCardWith)
 : (b) new element name =
 :     shax:pgroup if all is true:
 :     - current name = shax:choice
 :     - max(current maxCard, $multiplyMaxCardWith) > 1
 :     otherwise: current name
 : (c) new element content = :  
 :     current content recursively transformed ($multiplyMaxCardWith not set), if one is true:
 :     - current name != shax:choice
 :     - new maxCard = 1
 :     otherwise: shax:alternative/* recursively transformed ($multiplyMaxCardWith = $n/@card ! maxCard(.)) 
 :)
declare function f:expandShax31cRC($n as node(), $multiplyMaxCardWith as xs:integer?)
        as node()* {
    typeswitch($n)

    case element() return
        let $card :=
            if (not($n/@card) or empty($multiplyMaxCardWith)) then $n/@card
            else f:adaptChoiceBranchCardinality($n/@card, $multiplyMaxCardWith)
        let $maxCard := f:cardRangeFromCardDesc($card)[2]            
        let $name :=
            if (node-name($n) ne QName($f:URI_SHAX, 'choice') or $maxCard eq 1) then node-name($n)
            else QName($f:URI_SHAX, 'shax:pgroup')
        let $content :=
            if (node-name($n) ne QName($f:URI_SHAX, 'choice') or $maxCard eq 1) then (
                $n/(@* except @card) ! f:expandShax31cRC(., ()),
                $n/node() ! f:expandShax31cRC(., ())
            ) else (
                $n/shax:alternative ! f:expandShax31cRC(., $maxCard)    
            )
        let $contentAtts := ( 
            $content[self::attribute()][not(self::attribute(shax:card))], 
            $card ! attribute card {.})
        let $contentElems := $content except $contentAtts
        return
            element {$name} {$contentAtts, $contentElems}
            
     default return $n       
};    
:)

:)


