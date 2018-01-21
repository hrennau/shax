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
    "util.xqm";
    
declare namespace shax="http://shax.org/ns/model";

(:~
 : Returns the XSD cardiality attributes reflecting the cardinality constraints of a 
 : SHAX particle.
 :
 : @param p a shax particle (property or compositor)
 : @return an optional @minOccurs and an optional @maxOccurs attribute
 :)

declare function f:getXsdCardinalityAtts($p as element())
        as attribute() * {
    let $cardRange := f:getCardinalityRange($p)
    let $minOccurs := 
        let $min := $cardRange[1]
        return
            if ($min eq 1) then ()
            else attribute minOccurs {$min}
    let $maxOccurs := 
        let $max := $cardRange[2]
        return
            if ($max eq 1) then () else 
                attribute maxOccurs {
                    if ($max eq -1) then 'unbounded'
                    else $max
                }
    return ($minOccurs, $maxOccurs)                
};        

declare function f:editXsdTypeReference($typeName as xs:QName)
        as xs:QName {
    if (namespace-uri-from-QName($typeName) eq $f:URI_XSD) then 
        QName($f:URI_XSD, concat('xs:', local-name-from-QName($typeName)))
    else $typeName
        
};        
