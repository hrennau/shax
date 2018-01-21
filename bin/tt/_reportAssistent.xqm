(: reportAssistent.xqm - provides report utilities
 :
 : @version 20130926-1 first version 
 : ===================================================================================
 :)

module namespace m="http://www.ttools.org/xquery-functions";

import module namespace i="http://www.ttools.org/xquery-functions" at
    "_request.xqm",
    "_constants.xqm",
    "_request_getters.xqm";

declare namespace z="http://www.ttools.org/structure";

(:~
 : Returns standard items to be added to a report. These are a timestamp and
 : an optional repetition of the request element.
 : 
 : @param request the request string
 : @return the standard items
 :)
declare function m:getStandardItems($request as element())
        as node()* {
    attribute t {current-dateTime()},        
    if (not(i:getParam($request, 'echo'))) then () else 
        <z:request>{$request}</z:request>
};

declare function m:finalEdit($report as element()?)
        as element()? {
    if (not($report)) then () else m:prettyPrint($report)        
};        

declare function m:_padRight($s as xs:string?, $width as xs:integer)
        as xs:string? {
    substring(concat($s, string-join(for $i in 1 to $width return ' ', '')), 1, $width)        
};

declare function m:_foldText($text as xs:string?, $width as xs:integer, $initialCol as xs:integer, $indent as xs:integer)
        as xs:string? {
    if (not($text)) then () else
    
    let $sep := concat('&#xA;', string-join(for $i in 1 to $indent return ' ', ''))
    let $len1 := $width - $initialCol
    let $len := string-length($text)
    return
        if ($len le $len1) then $text else
        
    let $lineRaw := substring($text, 1, $len1)
    let $line := replace($lineRaw, '^(.*\s).*', '$1')
    
    let $next := concat(replace(substring($lineRaw, 1 + string-length($line)), '^\s+', ''), substring($text, 1 + string-length($lineRaw))) 
    return 
        string-join(($line, m:_foldText($next, $width, $initialCol, $indent)), $sep)
};        

(:
 : Creates a copy of the input node with all element namespaces
 : removed.
 :
 : @param n the node to be transformed
 : @return a copy with element namespaces removed
 :)
declare function m:rmElemNamespaces($n as node())
        as node() {
    typeswitch($n)
    case document-node() return
        document {for $c in $n/node() return m:rmElemNamespaces($c)}
    case element() return
        element {local-name($n)} {
            for $a in $n/@* return m:rmElemNamespaces($a),
            for $c in $n/node() return m:rmElemNamespaces($c)            
        }
    default return $n        
};        

declare function m:prettyPrint($n as node())
        as node()? {
    typeswitch($n)
    case document-node() return 
        for $c in $n/node() return document {m:prettyPrint($c)}
    case element() return
        let $elem :=
            element {node-name($n)} {
                for $a in $n/@* return m:prettyPrint($a),
                for $c in $n/node() return m:prettyPrint($c)
            }                
        return
            if ($n/parent::*) then $elem
            else <_  xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance">{$elem}</_>/*       

    case text() return
        if ($n/../* and not(matches($n, '\S'))) then () else $n
    default return $n
};

(:~
 : Checks if all items of an item sequence are castable to a given type.
 :
 : @param items the items to be checked
 : $type the type against which to check
 : @return true if all items are castable to $type
 :)
declare function m:itemsCastable($items as item()*, $type as xs:string)
        as xs:boolean {
    every $item in $items satisfies
        if ($type eq 'xs:string') then true()    
        else if ($type eq 'xs:normalizedString') then $item castable as xs:normalizedString        
        else if ($type eq 'xs:token') then $item castable as xs:token
        else if ($type eq 'xs:language') then $item castable as xs:language
        else if ($type eq 'xs:NMTOKEN') then $item castable as xs:NMTOKEN        
        else if ($type eq 'xs:Name') then $item castable as xs:Name        
        else if ($type eq 'xs:NCName') then $item castable as xs:NCName
        else if ($type eq 'xs:ID') then $item castable as xs:ID
        else if ($type eq 'xs:IDREF') then $item castable as xs:IDREF        
        else if ($type eq 'xs:dateTime') then $item castable as xs:dateTime        
        else if ($type eq 'xs:date') then $item castable as xs:date        
        else if ($type eq 'xs:time') then $item castable as xs:time
        else if ($type eq 'xs:duration') then $item castable as xs:duration
        else if ($type eq 'xs:yearMonthDuration') then $item castable as xs:yearMonthDuration        
        else if ($type eq 'xs:dayTimeDuration') then $item castable as xs:dayTimeDuration        
        else if ($type eq 'xs:float') then $item castable as xs:float
        else if ($type eq 'xs:double') then $item castable as xs:double
        else if ($type eq 'xs:decimal') then $item castable as xs:decimal
        else if ($type eq 'xs:integer') then $item castable as xs:integer        
        else if ($type eq 'xs:nonPositiveInteger') then $item castable as xs:nonPositiveInteger        
        else if ($type eq 'xs:negativeInteger') then $item castable as xs:negativeInteger        
        else if ($type eq 'xs:long') then $item castable as xs:long
        else if ($type eq 'xs:int') then $item castable as xs:int        
        else if ($type eq 'xs:short') then $item castable as xs:short        
        else if ($type eq 'xs:byte') then $item castable as xs:byte        
        else if ($type eq 'xs:nonNegativeInteger') then $item castable as xs:nonNegativeInteger        
        else if ($type eq 'xs:unsignedLong') then $item castable as xs:unsignedLong
        else if ($type eq 'xs:unsignedInt') then $item castable as xs:unsignedInt        
        else if ($type eq 'xs:unsignedShort') then $item castable as xs:unsignedShort        
        else if ($type eq 'xs:unsignedByte') then $item castable as xs:unsignedByte
        else if ($type eq 'xs:positiveInteger') then $item castable as xs:positiveInteger        
        else if ($type eq 'xs:gYearMonth') then $item castable as xs:gYearMonth        
        else if ($type eq 'xs:gYear') then $item castable as xs:gYear        
        else if ($type eq 'xs:gMonthDay') then $item castable as xs:gMonthDay        
        else if ($type eq 'xs:gDay') then $item castable as xs:gDay        
        else if ($type eq 'xs:gMonth') then $item castable as xs:gMonth        
        else if ($type eq 'xs:boolean') then $item castable as xs:boolean        
        else if ($type eq 'xs:base64Binary') then $item castable as xs:base64Binary        
        else if ($type eq 'xs:hexBinary') then $item castable as xs:hexBinary        
        else if ($type eq 'xs:anyURI') then $item castable as xs:anyURI
        
        else if ($type eq 'directory') then true()        
        else if ($type eq 'docURI') then true()        
        else
            false()
};        


