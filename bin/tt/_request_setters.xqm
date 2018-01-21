(:~ 
 : _request_setters.xqm - setter functions setting parameter values
 :
 : Supported parameter item types:
 :
 : xs:boolean 
 : ...
 :
 : @version 20140908-1 first version 
 : ===================================================================================
 :)

module namespace m="http://www.ttools.org/xquery-functions";

import module namespace tt="http://www.ttools.org/xquery-functions" at
   "_constants.xqm",
   "_reportAssistent.xqm";   

(:~
 : Sets a parameter. If a request is supplied, the function
 : creates a copy extended by the desired parameter; otherwise,
 : the function creates a new request element.
 : 
 : @param request the request element
 : @return the operation name
 :)
declare function m:setParam($request as element()?, 
                            $name as xs:string, 
                            $value as xs:anyAtomicType*,
                            $type as xs:string)
      as item()* {

    if (not(tt:itemsCastable($value, $type))) then
        error(QName($tt:URI_ERROR, 'INVALID_ARG'), concat('Invalid arguments supplied ',
        'to ''setParam'' - item type: ', $type, '; items=', string-join($value, '; '))) else
    
    let $paramItem :=
        element {$name} {
            attribute itemType {$type},
            if (count($value) eq 1) then $value else
                for $item in $value return <item>{$item}</item>
            }                
    return
        if ($request) then
            element {node-name($request)} {
                $request/@*,
                $request/(node() except *[local-name(.) eq $name]),
                $paramItem
            }
        else <_request>{$paramItem}</_request>            
};

