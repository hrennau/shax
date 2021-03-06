(:
 : -------------------------------------------------------------------------
 :
 : util.xqm - utility functions
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
    "namespaceTools.xqm";

declare namespace shax="http://shax.org/ns/model";
declare namespace z="http://www.ttools.org/shax/ns/structure";

declare function f:doesModelContainLists($shaxExpanded as element()+)
        as xs:boolean {
    exists($shaxExpanded//shax:shape/(@itemDatatype, @itemNode, @container[. eq 'list'])) or
    exists($shaxExpanded//shax:pshape/@ordered[. eq 'true'])
};

(:~
 : Maps a SHAX node kind value to a SHACL node kind value.
 :)
declare function f:shaclNodeKind($shaxNodeKind as xs:string)
        as xs:string {
    switch($shaxNodeKind)
    case 'iri' return 'sh:IRI'
    case 'bnode' return 'sh:BlankNode'
    case 'literal' return 'sh:Literal'
    case 'iri-bnode' return 'sh:BlankNodeOrIRI'
    case 'iri-literal' return 'sh:IRIOrLiteral'
    case 'bnode-literal' return 'sh:BlankNodeOrLiteral'
    default return error(QName((), 'MODEL_ERROR'), concat('Unknown node kind: ', $shaxNodeKind))
};

(:~
 : Returns the shape referenced by an attribute, if such a shape exists.
 : Note that the attribute might also reference a datatype, in which
 : case this function returns the empty sequence.
 :
 : @param ref an attribute containing a QName
 : @return the shape referenced, if it exists, the empty sequence otherwise
 :)
declare function f:getShape($ref as attribute())
        as element()? {
    $ref/ancestor::shax:model/shax:shape[@name/resolve-QName(., ..) eq $ref/resolve-QName(., ..)]        
};

(:~
 : Returns the range of cardinalities allowed by a SHAX descriptor. If the SHAX
 : descriptor does not have an @card attribute, the range of the default cardinality, 
 : if any exists, is returned.
 :
 : @param p a shax element (property or compositor)
 : @return two integers providing the minimum and maximum number of occurrences
 :)
declare function f:getCardinalityRange($p as element())
        as xs:integer+ {
    let $ecard := $p/@card
    return
        if (not($ecard)) then (0, -1)
        else if ($ecard eq '?') then (0, 1)
        else if ($ecard eq '+') then (1, -1)
        else if ($ecard eq '*') then (0, -1)        
        else if ($ecard/matches(., '^\s*\d+\s*$')) then 
            let $value := normalize-space($ecard) ! xs:integer(.)
            return ($value, $value)
        else if ($ecard/matches(., '^\s*\d+\s*-\s*\d+\s*$')) then
            let $limits := 
                tokenize(replace($ecard, '\s*(\d+)\s*-\s*(\d+)\s*$', '$1#$2'), '#')
            return $limits ! xs:integer(.)                        
        else error(QName((), 'SYNTAX_ERROR'), concat('Invalid cardinality: ', $ecard))
};     

declare function f:cardRangeFromCardDesc($desc as xs:string?)
        as xs:integer+ {
    if (not($desc)) then (0, -1)
    else if ($desc eq '?') then (0, 1)
    else if ($desc eq '+') then (1, -1)
    else if ($desc eq '*') then (0, -1)        
    else if (matches($desc, '^\s*\d+\s*$')) then 
        let $value := normalize-space($desc) ! xs:integer(.)
        return ($value, $value)
    else if (matches($desc, '^\s*\d+\s*-\s*\d+\s*$')) then
        let $limits := 
            tokenize(replace($desc, '\s*(\d+)\s*-\s*(\d+)\s*$', '$1#$2'), '#')
        return $limits ! xs:integer(.)                        
    else error(QName((), 'SYNTAX_ERROR'), concat('Invalid cardinality: ', $desc))
};     

(:
(:~
 : Returns the range of cardinalities allowed by a SHAX descriptor. If $considerDefault
 : is true and the SHAX descriptor does not have an @card attribute, the range of
 : the default cardinality, if any exists, is returned.
 :
 : @param p a shax element (property or compositor)
 : @return two integers providing the minimum and maximum number of occurrences
 :)
declare function f:getCardinalityRange($p as element(), $considerDefault as xs:boolean?)
        as xs:integer+ {
    let $DUMMY := trace(name($p), 'CARDINALITY_RANGE_FOR_ELEM: ')
    let $ecard := $p/@card
    let $ecard :=
        if ($ecard) then $ecard
        else if ($considerDefault) then $p/ancestor::*[@defaultCard][1]/@defaultCard
        else ()
    return
        if (not($ecard)) then (0, -1)
        else if ($ecard eq '?') then (0, 1)
        else if ($ecard eq '+') then (1, -1)
        else if ($ecard eq '*') then (0, -1)        
        else if ($ecard/matches(., '^\s*\d+\s*$')) then 
            let $value := normalize-space($ecard) ! xs:integer(.)
            return ($value, $value)
        else if ($ecard/matches(., '^\s*\d+\s*-\s*\d+\s*$')) then
            let $limits := 
                tokenize(replace($ecard, '\s*(\d+)\s*-\s*(\d+)\s*$', '$1#$2'), '#')
            return $limits ! xs:integer(.)                        
        else error(QName((), 'SYNTAX_ERROR'), concat('Invalid cardinality: ', $ecard))
};     
:)

(:~
 : Returns true if the max cardinality of a SHAX particle is greater than one.
 :
 : @param p a shax particle (property or compositor)
 : @return true if the cardinality range allows multiple values
 :)
declare function f:isParticleMultiple($p as element())
        as xs:boolean {
    let $maxCard := f:getCardinalityRange($p)[2]
    return $maxCard gt 1 or $maxCard lt 0
};        

(:~
 : Returns true if the min cardinality of a SHAX particle is zero.
 :
 : @param p a shax particle (property or compositor)
 : @return true if the cardinality range allows zero values
 :)
declare function f:isParticleOptional($p as element())
        as xs:boolean {
    let $minCard := f:getCardinalityRange($p)[1]
    return $minCard eq 0
};        


(:
(:~
 : Returns true if the cardinality of a SHAX particle is greater than one.
 :
 : @param p a shax particle (property or compositor)
 : @param considerDefault if true, a missing @card attribute is equivalent to
 :     a @card attribute with the value found in the nearest @defaultCard attribute
 :     found on ancestor or self
 : @return true if the cardinality range allows multiple values
 :)
declare function f:isParticleMultiple($p as element(), $considerDefault as xs:boolean?)
        as xs:boolean {
    let $maxCard := f:getCardinalityRange($p, $considerDefault)[2]
    return $maxCard gt 1 or $maxCard lt 0
};        
:)

(:~
 : Maps minimum and maximum numbers of occurrence to a succinct
 : occurrence descriptor as used in @z:occ (?, *, +, {i-j}).
 : An input maximum value of -1 is interpreted as infinity.
 :
 : @params minimum and maximum numbers of occurrence
 : @return a succinct occurrence descriptor, as used in @z:occ
 :)
declare function f:cardinalityDescFromCardinalityRange($minOccurs as xs:integer, 
                                                       $maxOccurs as xs:integer)
        as xs:string {
    switch($maxOccurs)
    case 0 return '{0-0}'
    case 1 return
        switch($minOccurs)
        case 0 return '?'
        case 1 return ''
        default return '{0-0}' 
    case -1 return
        switch($minOccurs)
        case 0 return '*'
        case 1 return '+'
        default return concat($minOccurs, '-*')
    default return concat($minOccurs, '-', $maxOccurs) 
};

(:~
 : Returns the occurrence descriptor capturing the result of 
 : "multiplying" two occurrence descriptors. The minOccurs
 : of the result is the product of the minOccurs values of the
 : input descriptors. Likewise, the maxOccurs of the result is
 : the product of the maxOccurs values of the input descriptors,
 : taking the special value "unbounded" into due account. 
 : 
 : Note. Multiplication is used in two contexts:
 : <ul>
 :   <li>replacing parent/child descriptors by a single descriptor</li>
 :   <li>referencing a definition</li> 
 : </ul>
 : 
 : An example for usecase 1 is the removal of pseudo groups consisting of
 : a single member. An example for usecase 2 is the replacement of
 : a group reference by the group contents.
 :
 : Rules:
 :     minOccurs(left)   = 0  => minOccurs = 0
 :     minOccurs(left)   = 1  => minOccurs = minOccurs(right)
 :     minOccurs(left)   > 1  => minOccurs = minOccurs(right) * minOccurs(left)
 :     maxOccurs(left)   = 0  => maxOccurs = 0
 :     maxOccurs(left)   = 1  => maxOccurs = maxOccurs(right)
 :     maxOccurs(left)   = *  => maxOccurs = *
 :     maxOccurs(right)  = *  => maxOccurs = *
 :     otherwise              => maxOccurs = maxOccurs(right) * maxOccurs(left) 
 :)
declare function f:multiplyCardinalityRanges($elem1 as element(), $elem2 as element())                                             
        as xs:string {
    let $lhsRange := f:getCardinalityRange($elem1)
    let $lhsMin := $lhsRange[1]
    let $lhsMax := $lhsRange[2]
    let $rhsRange := f:getCardinalityRange($elem2)    
    let $rhsMin := $rhsRange[1]
    let $rhsMax := $rhsRange[2]
    
    let $min := $lhsMin * $rhsMin
    let $max :=
        if (0 = ($lhsMax, $rhsMax)) then 0
        else if (-1 = ($lhsMax, $rhsMax)) then -1
        else $lhsMax * $rhsMax
    return
        f:cardinalityDescFromCardinalityRange($min, $max)
};

(:~
 : Returns the occurrence descriptor capturing the result of 
 : "multiplying" maxOccurs and setting minOccurs to 0. 
 :
 : This operation is needed when dealing with a choice which
 : has maxOccurs>1. Each branch obtains a maxOccurs which is
 * the product of choice.maxOccurs and branch.maxOccurs. The
 : minOccurs of the branch, on the other hand, is zero,
 : as the branch is possibly never used.
 : 
 : Rules:
 :     maxOccurs(left)   = 0  => maxOccurs = 0
 :     maxOccurs(left)   = 1  => maxOccurs = maxOccurs(right)
 :     maxOccurs(left)   = *  => maxOccurs = *
 :     maxOccurs(right)  = *  => maxOccurs = *
 :     otherwise              => maxOccurs = maxOccurs(right) * maxOccurs(left) 
 :)
declare function f:multiplyMaxCardinality($elem1 as element(), $elem2 as element())                                             
        as xs:string {
    let $lhsRange := f:getCardinalityRange($elem1)
    let $lhsMax := $lhsRange[2]
    let $rhsRange := f:getCardinalityRange($elem2)    
    let $rhsMax := $rhsRange[2]
    
    let $min := 0
    let $max :=
        if (0 = ($lhsMax, $rhsMax)) then 0
        else if (-1 = ($lhsMax, $rhsMax)) then -1
        else $lhsMax * $rhsMax
    return
        f:cardinalityDescFromCardinalityRange($min, $max)
};

declare function f:adaptChoiceBranchCardinality($branchCard as xs:string, $choiceMaxCard as xs:integer)                                             
        as xs:string {
    if ($choiceMaxCard eq 1) then $branchCard else
    
    let $max := f:cardRangeFromCardDesc($branchCard)[2]
    let $adaptedMin := 0
    let $adaptedMax :=
        if (0 = ($max, $choiceMaxCard)) then 0
        else if (-1 = ($max, $choiceMaxCard)) then -1
        else $max * $choiceMaxCard
    return
        f:cardinalityDescFromCardinalityRange($adaptedMin, $adaptedMax)
};


(:~
 : Edits a SHAX model, inserting cardinality attributes expressing the 
 : appropriate default cardinality.
 :
 : The input element is either a `shax:model` or a `shax:models` element.
 :
 : The elements receiving default values are the elements representing
 : particles. These are the descendants of shax:objectType elements.
 :
 :) 
declare function f:addCardinalityAtts($model as element())
        as node()* {
    f:addCardinalityAttsRC($model, ())        
};        

(:~
 : Recursive helper function of `resolveDefaultCardinalities`.
 :) 
declare function f:addCardinalityAttsRC($n as node(), 
                                        $defaultCards as map(xs:string, xs:string)?)
        as node()* {
    typeswitch($n)
    
    case element(shax:models) return
        element {node-name($n)} {
            f:namespaceNodes($n),        
            for $a in $n/@* return f:addCardinalityAttsRC($a, $defaultCards),
            for $c in $n/node() return f:addCardinalityAttsRC($c, $defaultCards)                
        }
            
    case element(shax:model) return
        let $defaultCards := 
            map{"*": ($n/@defaultCard/string(), "1")[1], 
                "choice": "1",
                "pgroup": "1"
            }
        return
            element {node-name($n)} {
                f:namespaceNodes($n),            
                for $a in $n/@* return f:addCardinalityAttsRC($a, $defaultCards),
                for $c in $n/node() return f:addCardinalityAttsRC($c, $defaultCards)                
            }
    
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
                for $a in $n/@* return f:addCardinalityAttsRC($a, $defaultCards),                
                for $c in $n/node() return f:addCardinalityAttsRC($c, $defaultCards)
            }
    default return $n                
};

(:~
 : Normalizes a URI. Returns it without changes, unless the URI is
 : a file URI. In this case, reduces multiple slashes after 'file:' to
 : a single slash, puts drive letter lower-case and adds drive letter 'c' in
 : case the uri does not contain a drive letter.
 :
 : @param uri the uri to be normalized
 : @return the normalized uri
 :)
declare function f:normalizeUri($uri as xs:string) {
    if (not(matches($uri, '^\s*file:'))) then $uri else
    
    let $u := replace($uri, 'file:/+', 'file:/')
    let $driveLetter := replace($u, '^file:/(.):.+', '$1')[not(. eq $u)]
    return
        if ($driveLetter) then
            replace($u, '(file:/).(:.+)', concat('$1', lower-case($driveLetter), '$2'))
        else
            replace($u, '^(file:/)(.+)', '$1c:/$2') 
};

declare function f:writeDebugXml($minDebugLevel as xs:integer,
                                 $fileName as xs:string,
                                 $xmlFile)
        as empty-sequence() {
    if ($minDebugLevel > $i:DEBUG_LEVEL) then () else
        let $filePath := concat($i:DIR_DEBUG, '/', $fileName)
        return file:write($filePath, $xmlFile)
};        
                                  
