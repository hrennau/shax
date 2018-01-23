(:~ 
 : _request_getters.xqm - getter functions delivering parameter values
 :
 : Supported parameter item types:
 :
 : xs:integer 
 : xs:negativeInteger
 : xs:nonNegativeInteger
 : xs:nonPositiveInteger
 : xs:positiveInteger
 : xs:string
 :
 : @version 20140325-1 new item type 'directory'
 : @version 20140217-1 first version 
 : @version 20160118-1 new item type 'file' 
 : ===================================================================================
 :)

module namespace m="http://www.ttools.org/xquery-functions";

import module namespace tt="http://www.ttools.org/xquery-functions" at
   "_constants.xqm",  
   "_nameFilter_parser.xqm",   
   "_pcollection.xqm",   
   "_rcat.xqm",
   "_resourceAccess.xqm",   
   "_stringTools.xqm"
   ,
   "_csvParser.xqm"
   ;   

(:
 : ============================================================================
 :
 :     p u b l i c    f u n c t i o n s
 :
 : ============================================================================
 :)

(:~
 : Returns the operation name.
 : 
 : @param request the request element
 : @return the operation name
 :)
declare function m:getOperationName($request as element()?)
      as item()* {
    if (not($request)) then () else local-name($request)
};

(:~
 : Returns the names of available parameters, optionally filtered by
 : a name filter.
 :
 : @param request the request element
 : @param nameFilter a name filter
 : @return the naes of available parameters, optionally 
 :    filtered by the supplied namefilter
 :)
declare function m:getParamNames($request as element()?, $nameFilter as xs:string?)
        as xs:string* {
    let $allNames := $request/*/local-name(.)
    return
        if (not($nameFilter)) then $allNames 
        else
            let $filter := tt:parseNameFilter($nameFilter)
            return
                tt:filterNames($allNames, $filter)       
};

(:~
 : Returns true if a parameter exists, false otherwise.
 : 
 : @param request the request element
 : @param name the parameter name 
 : @return boolean value flagging parameter existence
 :)
declare function m:paramExists($request as element()?, $name as xs:string)
      as xs:boolean? {
    if (not($request)) then () else    
        boolean(m:_getParamElem($request, $name)) 
};

(:~
 : Returns a parameter value.
 : 
 : @param request the request element
 : @param name the parameter name 
 : @return the parameter value
 :)
declare function m:getParam($request as element()?, $name as xs:string)
      as item()* {
    if (not($request)) then () else
    
    let $paramElem := m:_getParamElem($request, $name) 
    return if (not($paramElem)) then () else m:_getParam($paramElem)
    
};

(:~
 : Returns a sequence of parameter values. The parameter names
 : can be specified by distinct items and/or as whitespace-separated
 : list of names. 
 :
 : Usage note. The function is for example useful when input documents
 : can be specified using several parameters of different type (e.g. 
 : document URI, directory filter descriptor, document catalog). 
 : 
 : @param request the request element
 : @param names the parameter names 
 : @return the sequence of parameter values
 :)
declare function m:getParams($request as element()?, $names as xs:string+)
      as item()* {
    if (not($request)) then () else
    
    for $name in $names
    for $n in tokenize(normalize-space($name), ' ')
    return m:getParam($request, $n)
};

(:~
 : Returns the string value of a parameter. If the parameter
 : is stored as separate items, the semicolon concatenated value 
 : is delivered (escaping any literal semicolon characters by a
 : preceding backslash).
 : 
 : @param request the request element
 : @param name the paramter name
 : @return the parameter value, or the default value
 :)
declare function m:getParamStringValue($request as element(), 
                                       $name as xs:string)
    as xs:string? {
    let $elem := m:_getParamElem($request, $name)
    return
        if (not($elem)) then ()
        else if ($elem/@valueText) then $elem/@valueText        
        else if (not($elem/*)) then string($elem) 
        else string-join($elem/*/replace((@text, .)[1], ';', '\\;'), ';')
};

(:~
 : Returns an element with attributes capturing the names 
 : and values of request parameters. The name and value
 : of each attribute are equal to the name and string
 : value of a parameter. If no name filter is specified, 
 : all parameters are used, otherwise only those matching 
 : the name filter. If $removePrefixes is no empty,
 : names are edited by removing the longest prefix
 : matching one of the prefixes specified.
 :
 : Usage note. Control elements are useful if request
 : parameters are used at a high frequence (e.g. during
 : the item processing within a recursion), as the
 : access to control attributes is faster than 
 : reading request parameters. Note, however, that the
 : control only represents the parameter string values.
 :
 : @param request a request element
 : @param nameFilter an optional name filter, using name filter syntax
 : @param removePrefixes an optional list of prefixes to be
 :    removed from the parameter names
 : @return the control element
 :)
declare function m:getControl($request as element(), 
                              $nameFilter as xs:string?, 
                              $removePrefixes as xs:string*)
        as element(control) {                              
    let $names := m:getParamNames($request, $nameFilter)
    let $useNames := tt:removeStringPrefixes($names, $removePrefixes, '.')
    return
        <control>{
            for $name in $useNames
            return
                attribute {$name} {m:getParamStringValue($request, $name)}
        }</control>   
};

(:
 : ============================================================================
 :
 :     p r i v a t e    f u n c t i o n s
 :
 : ============================================================================
 :)

declare function m:_getParam($paramElem as element()) 
        as item()* {
    let $itemType := $paramElem/@itemType/string()
    let $untypedItems := m:_getUntypedParamItems($paramElem)   
    return
        if ($itemType eq 'xs:string') then $untypedItems
        else if ($itemType eq 'xs:normalizedString') then
            for $item in $untypedItems return xs:normalizedString($item)
        else if ($itemType eq 'xs:token') then
            for $item in $untypedItems return xs:token($item)
        else if ($itemType eq 'xs:language') then
            for $item in $untypedItems return xs:language($item)
        else if ($itemType eq 'xs:NMTOKEN') then
            for $item in $untypedItems return xs:NMTOKEN($item)
        else if ($itemType eq 'xs:Name') then
            for $item in $untypedItems return xs:Name($item)
        else if ($itemType eq 'xs:NCName') then
            for $item in $untypedItems return xs:NCName($item)
        else if ($itemType eq 'xs:ID') then
            for $item in $untypedItems return xs:ID($item)
        else if ($itemType eq 'xs:IDREF') then
            for $item in $untypedItems return xs:IDREF($item)
        else if ($itemType eq 'xs:dateTime') then
            for $item in $untypedItems return xs:dateTime($item)
        else if ($itemType eq 'xs:date') then
            for $item in $untypedItems return xs:date($item)
        else if ($itemType eq 'xs:time') then
            for $item in $untypedItems return xs:time($item)
        else if ($itemType eq 'xs:duration') then
            for $item in $untypedItems return xs:duration($item)
        else if ($itemType eq 'xs:yearMonthDuration') then
            for $item in $untypedItems return xs:yearMonthDuration($item)
        else if ($itemType eq 'xs:dayTimeDuration') then
            for $item in $untypedItems return xs:dayTimeDuration($item)
        else if ($itemType eq 'xs:float') then
            for $item in $untypedItems return xs:float($item)
        else if ($itemType eq 'xs:double') then
            for $item in $untypedItems return xs:double($item)
        else if ($itemType eq 'xs:decimal') then
            for $item in $untypedItems return xs:decimal($item)
        else if ($itemType eq 'xs:integer') then
            for $item in $untypedItems return xs:integer($item)
        else if ($itemType eq 'xs:nonPositiveInteger') then
            for $item in $untypedItems return xs:nonPositiveInteger($item)
        else if ($itemType eq 'xs:negativeInteger') then
            for $item in $untypedItems return xs:negativeInteger($item)
        else if ($itemType eq 'xs:long') then
            for $item in $untypedItems return xs:long($item)
        else if ($itemType eq 'xs:int') then
            for $item in $untypedItems return xs:int($item)
        else if ($itemType eq 'xs:short') then
            for $item in $untypedItems return xs:short($item)
        else if ($itemType eq 'xs:byte') then
            for $item in $untypedItems return xs:byte($item)
        else if ($itemType eq 'xs:nonNegativeInteger') then
            for $item in $untypedItems return xs:nonNegativeInteger($item)
        else if ($itemType eq 'xs:unsignedLong') then
            for $item in $untypedItems return xs:unsignedLong($item)
        else if ($itemType eq 'xs:unsignedInt') then
            for $item in $untypedItems return xs:unsignedInt($item)
        else if ($itemType eq 'xs:unsignedShort') then
            for $item in $untypedItems return xs:unsignedShort($item)
        else if ($itemType eq 'xs:unsignedByte') then
            for $item in $untypedItems return xs:unsignedByte($item)
        else if ($itemType eq 'xs:positiveInteger') then
            for $item in $untypedItems return xs:positiveInteger($item)
        else if ($itemType eq 'xs:gYearMonth') then
            for $item in $untypedItems return xs:gYearMonth($item)
        else if ($itemType eq 'xs:gYear') then
            for $item in $untypedItems return xs:gYear($item)
        else if ($itemType eq 'xs:gMonthDay') then
            for $item in $untypedItems return xs:gMonthDay($item)
        else if ($itemType eq 'xs:gDay') then
            for $item in $untypedItems return xs:gDay($item)
        else if ($itemType eq 'xs:gMonth') then
            for $item in $untypedItems return xs:gMonth($item)
        else if ($itemType eq 'xs:boolean') then
            for $item in $untypedItems return xs:boolean($item)
        else if ($itemType eq 'xs:base64Binary') then
            for $item in $untypedItems return xs:base64Binary($item)
        else if ($itemType eq 'xs:hexBinary') then
            for $item in $untypedItems return xs:hexBinary($item)
        else if ($itemType eq 'xs:anyURI') then
            for $item in $untypedItems return xs:anyURI($item)
        else if ($itemType eq 'xs:QName') then
            for $item in $untypedItems return xs:QName($item)
        
        else if ($itemType eq 'nameFilter') then
            for $item in $untypedItems return $item  
        else if (matches($itemType, '^nameFilterMap(\(.*\))?$')) then
            for $item in $untypedItems return $item  
        else if ($itemType eq 'pathFilter') then
            for $item in $untypedItems return $item  
        else if ($itemType eq 'docURI') then
            for $item in $untypedItems return tt:doc($item)
        else if ($itemType eq 'docCAT') then
            for $item in $untypedItems
            let $uri := string($item)
            let $query := $item/@pquery
            return tt:resolveRcat(tt:doc($uri), $query)
        else if ($itemType eq 'docFLX') then
            for $item in $untypedItems
            let $doc := tt:doc($item)
            return 
                if ($doc/docs/doc) then tt:resolveRcat($doc)
                else $doc
        else if ($itemType eq 'catDFD') then
            for $item in $untypedItems return $item
        else if ($itemType eq 'catFOX') then
            for $item in $untypedItems return $item
            
        else if ($itemType eq 'jsonURI') then
            for $item in $untypedItems 
            let $uri := replace($item, '\\', '/')
            let $text := tt:unparsed-text($uri)
            let $json := json:parse($text)
            return
                json:parse($text)
                    
        else if ($itemType eq 'textURI') then
            for $item in $untypedItems 
            let $uri := replace($item, '\\', '/')
            let $encoding := ($item/@encoding, 'UTF-8')[1]
            let $normLinefeed := $item/@normLinefeed            
            let $text := tt:unparsed-text($uri, $encoding)
            return
                if (not($normLinefeed eq 'true')) then $text else
                    tt:normalizeLinefeed($text)
                
        else if ($itemType eq 'linesURI') then
            for $item in $untypedItems 
            let $uri := replace($item, '\\', '/')
            let $encoding := ($item/@encoding, 'UTF-8')[1]
            return
                tt:unparsed-text-lines($uri, $encoding)
        else if ($itemType eq 'wtxURI') then
            for $item in $untypedItems 
            let $uri := replace($item, '\\', '/')
            let $encoding := ($item/@encoding, 'UTF-8')[1]
            let $normLinefeed := $item/@normLinefeed            
            let $text := tt:unparsed-text($uri, $encoding)
            let $text :=
                if (not($normLinefeed eq 'true')) then $text else
                    tt:normalizeLinefeed($text)            
            let $name := replace($uri, '.*/', '')            
            return
                <resource uri="{$uri}" name="{$name}">{$text}</resource>            
        else if ($itemType eq 'csvURI') then
            for $item in $untypedItems             
            let $uri := replace($item, '\\', '/')
            let $encoding := $item/@encoding            
            let $sep := $item/@sep 
            let $delim := $item/@delim            
            let $header := $item/@header            
            let $names := $item/@names/tokenize(., '\s+')            
            let $fromRec := $item/@fromRec            
            let $toRec := $item/@toRec            
            return
                tt:parseCsv($uri, $encoding, $sep, $delim, $header, $names, $fromRec, $toRec)            
        else if ($itemType eq 'docDFD') then
            for $item in $untypedItems return m:resolveRcat($item)
        else if ($itemType eq 'docFOX') then
            for $item in $untypedItems return m:resolveRcat($item)            
        else if ($itemType eq 'docSEARCH') then
            for $item in $untypedItems        
            let $nodlURI := $item/@nodl
            let $pfilter := $item/*
            let $nodl := tt:doc($nodlURI)/*
            return
                tt:filteredCollection($nodl, $pfilter)
        else if ($itemType eq 'textDFD') then
            for $item in $untypedItems return m:resolveRcat($item)
        else if ($itemType eq 'textFOX') then
            for $item in $untypedItems return m:resolveRcat($item)
        else if ($itemType eq 'linesDFD') then
            for $item in $untypedItems return m:resolveRcat($item)
        else if ($itemType eq 'linesFOX') then
            for $item in $untypedItems return m:resolveRcat($item)
        else if ($itemType eq 'xtextDFD') then
            for $item in $untypedItems return m:resolveRcat($item)
        else if ($itemType eq 'csvDFD') then
            for $item in $untypedItems return m:resolveRcat($item)
(:            
            for $item in $untypedItems
            let $encoding := ($item/@encoding, 'ISO-8859-1')[1]           
            for $uri in $item//@href
            let $sep := $item/@csv.sep 
            let $delim := $item/@csv.delim            
            let $header := $item/@csv.header            
            let $names := $item/@csv.names/tokenize(., '\s+')            
            let $fromRec := $item/@csv.fromRec
            let $toRec := $item/@csv.toRec 
            return
                $item//@href/tt:parseCsv(resolve-uri(., base-uri(..)), 
                    $encoding, $sep, $delim, $header, $names, $fromRec, $toRec)
:)                    
        else if ($itemType eq 'csvFOX') then
            for $item in $untypedItems return m:resolveRcat($item)
        else if ($itemType eq 'jsonFOX') then
            for $item in $untypedItems return m:resolveRcat($item)
        else if ($itemType eq 'directory') then         
            for $item in $untypedItems return $item
        else if ($itemType eq 'file') then         
            for $item in $untypedItems return $item
        else if (tokenize($m:NON_STANDARD_TYPES, ' ') = $itemType) then
            for $item in $untypedItems return $item     
        else if (matches($itemType, 'element\(.*\)')) then
            for $item in $untypedItems return $item        
        else error(QName($tt:URI_ERROR, 'SYSTEM_ERROR_UNKNOWN_ITEM_TYPE'), concat('Unknown item type: ', $itemType))
};

(:~
 : Returns the element representing a request parameter.
 : 
 : @param request the request element
 : @param name the parameter name 
 : @return the element representing the parameter value
 :)
declare function m:_getParamElem($request as element()?, $name as xs:string)
      as element()? {
    $request/*[local-name(.) eq $name]
};

declare function m:_getUnparsedParamItems($param as element())
        as item()* {
    if ($param/valueItem) then $param/valueItem/string(.) 
    else if ($param/@sep) then tokenize($param, $param/@sep)
    else $param/string(.)
};

declare function m:_getUntypedParamItems($param as element())
        as item()* {
    let $nodeItems := $param/@nodeItems/xs:boolean(.) return

    if ($param/valueItem) then 
        if ($nodeItems) then $param/valueItem/* else $param/valueItem/string(.) 
    else if ($param/@sep) then tokenize($param, $param/@sep)
    else if ($nodeItems) then $param/* else $param/string(.)
};

