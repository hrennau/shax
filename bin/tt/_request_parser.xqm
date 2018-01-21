(:~ 
 : _request_parser.xqm - parses a request string into a data structure
 :
 : @version 20131025-1 first version 
 : ===================================================================================
 :)

module namespace m="http://www.ttools.org/xquery-functions";

import module namespace i="http://www.ttools.org/xquery-functions" at
   "_constants.xqm",
   "_stringTools.xqm";   

(:~
 : Parses a request string into a data structure.
 : 
 : @param request the request string
  : @return an element representing the request
 :)
declare function m:_parseRequest($request as xs:string)
      as element() {
    let $request :=
        let $req := replace($request, '^\s+|\s+$', '')
        return
            if (starts-with($req, '?')) then
                concat('_help', $req[string-length(.) gt 1])
            else $req                
                
    let $operation := replace($request, '\s*\?.*', '', 's')
    let $params := 
        if (not(contains($request, '?'))) then () else
        replace($request, '^.*?\?\s*', '', 's')
    let $storeq := starts-with($params, '?')
    let $params := if ($storeq) then replace($params, '^\?\s*', '') else $params
    let $storeqAtt := if (not($storeq)) then () else attribute storeq {true()}
    (: let $items := m:_getParamItemRC($params) :)  
    let $items := m:_splitString($params, ',', ())
    let $items :=
        for $item in $items return
        if (not(contains($item, '='))) then
            if (starts-with($item, '~')) then <param name="{substring($item, 2)}" value="false"/>
            else <param name="{$item}" value="true"/>
        else
            let $name := replace($item, '^(.*?)\s*=.*', '$1', 's')
            let $value := replace($item, '^.*?=\s*', '', 's')
            return
                <param name="{$name}" value="{$value}"/>
    
    return
        <request operation="{$operation}" params="{$params}">{$storeqAtt, $items}</request>
};

(:
(:~
 : Recursive helper function of _parseRequest. Extracts the next
 : item, transforms it into an element representing item name
 : and value, and recursively calls itself for processing the
 : remainder of the parameter string, if there is a remainder.
 :
 : @param params the parameter string
 : @return the items, represented by "item" elements with a "name" and a
 :    "value" attribute providing item name and value
 :) 
declare function m:_getParamItemRCXXX($params as xs:string?)
        as element()* {
    if (empty($params)) then () else
    
    (: the next item preceding a comma or the string end :)
    let $item := replace($params, '^(.*?[^\\](\\\\)*)?,($|[^,].*)', '$1', 's')
    return if (not(normalize-space($item))) then () else
    
    let $next := if ($item eq $params) then () else
        replace(substring($params, string-length($item) + 2), '^\s+', '')
    let $itemElem :=
        if (not(contains($item, '='))) then
            if (starts-with($item, '~')) then <param name="{substring($item, 2)}" value="false"/>
            else <param name="{$item}" value="true"/>
        else
            let $name := replace($item, '^(.*?)\s*=.*', '$1', 's')
            let $value := replace($item, '^.*?=\s*', '', 's')
            return
                <param name="{$name}" value="{$value}"/>
    return (
        $itemElem,
        if (empty($next)) then () else m:_getParamItemRCXXX($next)
    )            
};        
:)