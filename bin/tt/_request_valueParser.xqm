(:~
 : _request_valueParser.xqm - a function for parsing a parameter value into a structured representation
 :
 : @version 20141104-1
 : 
 : ############################################################################
 :)

module namespace m="http://www.ttools.org/xquery-functions";

(:
import module namespace tt="http://www.ttools.org/xquery-functions" at
   "_constants.xqm",
   "_errorAssistent.xqm",   
   "_extensions.xqm",
   "_help.xqm",
   "_paramParsingTools.xqm",
   "_pfilter_parser.xqm",   
   "_rcat.xqm",   
   "_request_facets.xqm",   
   "_request_getters.xqm",
   "_reportAssistent.xqm",   
   "_stringTools.xqm";   
:)   
import module namespace tt="http://www.ttools.org/xquery-functions" at
   "_constants.xqm",
   "_errorAssistent.xqm",
   "_extensions.xqm",   
   "_request_facets.xqm",
   "_resourceAccess.xqm"
;

declare namespace z="http://www.ttools.org/structure";
declare namespace file="http://expath.org/ns/file";

(:
 : ============================================================================
 :
 :     p r i v a t e    f u n c t i o n s
 :
 : ============================================================================
 :)

(:~
 : Parses a parameter value into a structured representation. The
 : representation is an element whose name is the parameter name and
 : whose text content or item children represent the parsed value items.
 :
 : Error policy: in case of errors, a `z:errors`element is returned,
 : rather than an element named after the parameter and containing
 : the value items.
 :
 : @param context if set, the other input parameters represent a modifier 
 :    of a single parameter item, otherwise an operation parameter; 
 :    in the first case, $context describes the operation parameter 
 :    whose modifiers are evaluated: parameter name, item type and 
 :    item text
 : @param name the parameter name
 : @param paramText the text of the parameter value
 : @param valueItems the itemized text of the parameter value
 : @param itemType the item type
 : @param paramConfig the parameter configuration
 : @return a "param" element capturing the information content
 :    of the parameter value
 :)
declare function m:_parseParamValue($context as element(context)?,
                                    $name as xs:string, 
                                    $valueText as xs:string,
                                    $valueItems as xs:string+, 
                                    $itemType as xs:string, 
                                    $paramConfig as element())
        as element()* {
        
    let $edits := $paramConfig/@edit/(for $item in tokenize(normalize-space(.), '\s') return $item)
    let $postEdits := $paramConfig/@postEdit/(for $item in tokenize(normalize-space(.), '\s') return $item)        
    let $paramValue :=              
        for $item in $valueItems 
        let $itemText := m:_prepareParamValueItemText($item, $edits)
        let $itemValue := m:_parseParamValueItem($context, $item, $itemType, $name)
        let $errors :=
            if ($itemValue instance of element(z:errors)) then $itemValue
            else tt:_checkFacets($context, $itemText, $itemValue, $paramConfig)
        return
            ($itemValue, $errors)
    let $paramErrors := $paramValue[. instance of element(z:error) or . instance of element(z:errors)]
    return
        if ($paramErrors) then tt:wrapErrors($paramErrors) else
        
    let $isValueNodes as xs:boolean := some $v in $paramValue satisfies $v instance of node()
    let $isValueSingleton as xs:boolean := count($paramValue) le 1
    
    let $concatenate as xs:boolean :=
            not($isValueNodes) and 
            not($isValueSingleton) and 
            not(exists($valueItems[matches(string(.), '\s')]))
    let $paramValueNodes as node()* :=
        if (empty($paramValue)) then ()    
        else if ($isValueNodes) then
            if ($isValueSingleton) then $paramValue 
            else $paramValue/<valueItem>{.}</valueItem>
        else if ($isValueSingleton) then text {$paramValue}           
        else if ($concatenate) then 
            text {string-join(for $i in $paramValue return string($i), ' ')}
        else                    
            for $item in $paramValue return <valueItem>{$item}</valueItem>

    let $useItemType := tt:adaptItemTypeOfNonStandardItemType($itemType)
    return
        element {$name} {
            attribute itemType {$useItemType},
            if ($itemType eq $useItemType) then () else attribute origItemType {$itemType},
            attribute valueText {$valueText},
            if (not($isValueNodes)) then () else attribute nodeItems {'true'},
            if ($concatenate and not($isValueSingleton)) then attribute sep {'\s'} else (),
            $paramValueNodes
        }
};

(:~
 : Prepares the text of a single parameter item for parsing.
 :
 : @param item the parameter item text
 : @return the parameter item as a typed item
 :)
declare function m:_prepareParamValueItemText($item as xs:string, $edits as xs:string*)
        as xs:string {
    let $itemChopped := replace(replace($item, '^\s+|\s+$', ''), '\\s', ' ')
    let $itemText :=
        if (empty($edits)) then $itemChopped else m:_editItem($itemChopped, $edits)
    return
        $itemText
};

(:~
 : Parses the text of a single parameter item and returns a typed item.
 : Examples of result item types:
 : - xs:string
 : - xs:integer
 : - xs:date
 : - element(nameFilter)
 : - element(rcat)
 :
 : Error policy: in case of errors, an `z:errors` element is returned,
 : rather than a data item.
 :
 : @param context if set, the other input parameters represent a modifier 
 :    of a single parameter item, otherwise an operation parameter; 
 :    in the first case, $context describes the operation parameter 
 :    whose modifier is evaluated: parameter name, item type and 
 :    item text
 : @param itemText the parameter item text
 : @param itemType the item type
 : @param name the parameter name
 : @return the parameter item as a typed item
 :)
declare function m:_parseParamValueItem($context as element(context)?,
                                        $itemText as xs:string, 
                                        $itemType as xs:string, 
                                        $name as xs:string)
        as item()* {
    let $typedItem := m:_parseParamValueItem_builtin($context, $itemText, $itemType, $name)
    return if (exists($typedItem)) then $typedItem else
    
    let $typedItem :=
        (: *** nameFilter, nameFilterMap, pathFilter 
               ------------------------------------- :)
        if ($itemType eq 'nameFilter') then            
            let $nameFilter := tt:parseNameFilter($itemText) return
            if ($nameFilter and not($nameFilter/self::z:errors)) then $nameFilter else
                m:createStandardTypeError($context, $name, $itemType, $itemText, ': nameFilter syntax error')
        else if (matches($itemType, '^nameFilterMap(\(.*\))?$')) then            
            let $nameFilterMap := tt:parseNameFilterMap($itemText, $itemType) return
            if ($nameFilterMap and not($nameFilterMap/self::z:errors)) then $nameFilterMap else
                m:createStandardTypeError($context, $name, $itemType, $itemText, ': nameFilterMap syntax error')
        else if ($itemType eq 'pathFilter') then            
            let $pathFilter := tt:parsePathFilter($itemText) return
            if ($pathFilter and not($pathFilter/self::z:errors)) then $pathFilter else
                m:createStandardTypeError($context, $name, $itemType, $itemText, ': pathFilter syntax error')
                
        (: *** docURI, docFLX, docCAT, docSEARCH 
               --------------------------------- :)
        else if ($itemType eq 'docURI') then            
            if (tt:doc-available($itemText)) then $itemText else     
                m:createStandardTypeError($context, $name, $itemType, $itemText, ': no XML document at this location')
        else if ($itemType eq 'docFLX') then            
            if (tt:doc-available($itemText)) then $itemText else     
                m:createStandardTypeError($context, $name, $itemType, $itemText, ': no XML document at this location')
        else if ($itemType eq 'docCAT') then
            let $vamod := m:_getValueAndModifiers($itemText, $name, 'docCAT')
            return            
                if (tt:doc-available($vamod)) then 
                    let $root := tt:doc($vamod)/*
                    return
                        if (local-name($root) = ('rcat', 'docs') and namespace-uri($root) eq '') then 
                            <rcatRef>{$vamod/@*, string($vamod)}</rcatRef>                         
                        else
                            m:createStandardTypeError($context, $name, $itemType, $itemText, 
                                ': document at this location not an rcat document')
            else              
                m:createStandardTypeError($context, $name, $itemType, $itemText, ': no rcat document at this location')
        else if ($itemType eq 'docSEARCH') then  
            let $fields :=
                (: NODL URI and pfilter may be separater by ? or by % :)
                if (contains($itemText, '%')) then tt:getTextFields($itemText)                
                else if (contains($itemText, '?')) then (
                    replace($itemText, '\s*\?.*', ''),
                    replace($itemText, '.*?\?\s*', '')                )
                else $itemText
            let $nodlURI := $fields[1]
            let $pfilter := $fields[2]
            let $pfilterElem := tt:parsePfilter($pfilter)
            return
                if ($pfilterElem/self::tt:error) then $pfilterElem
                else if (not(tt:doc-available($nodlURI))) then 
                    m:createStandardTypeError($context, $name, $itemType, $nodlURI, ': no NODL document at this location')
                else
                    <filteredCollection nodl="{$nodlURI}">{
                        $pfilterElem
                    }</filteredCollection>

        else if ($itemType eq 'textURI') then
            let $vamod := m:_getValueAndModifiers($itemText, $name, 'textURI')        
            return if ($vamod/self::z:errors) then $vamod else            
                
            let $uri := replace($vamod, '\\', '/')
            let $encoding := ($vamod/@encoding, 'UTF-8')[1]            
            return        
                if (tt:unparsed-text-available($uri, $encoding)) then $vamod else
                    m:createStandardTypeError($context, $name, $itemType, $vamod, 
                        ': no text resource at this location')
        else if ($itemType eq 'wtextURI') then
            let $vamod := m:_getValueAndModifiers($itemText, $name, 'xtextURI')        
            return if ($vamod/self::z:errors) then $vamod else            
                
            let $uri := replace($vamod, '\\', '/')
            let $encoding := ($vamod/@encoding, 'UTF-8')[1]            
            return        
                if (tt:unparsed-text-available($uri, $encoding)) then $vamod else
                    m:createStandardTypeError($context, $name, $itemType, $vamod, 
                        ': no text resource at this location')
        else if ($itemType eq 'jsonURI') then  
            let $ok :=        
                if (tt:unparsed-text-available($itemText)) then
                    let $json := try {json:parse(tt:unparsed-text($itemText))} catch * {()}
                    return exists($json)
                else false()
            return
                if ($ok) then $itemText else 
                    m:createStandardTypeError($context, $name, $itemType, trace($itemText, 'ITEM_TEXT: '), ': no JSON document at this location')
        else if ($itemType eq 'csvURI') then
            let $vamod := m:_getValueAndModifiers($itemText, $name, 'csvURI')
            return if ($vamod/self::z:errors) then $vamod else            
                
            let $uri := replace($vamod, '\\', '/')
            let $encoding := ($vamod/@encoding, 'UTF-8')[1]            
            return        
                if (tt:unparsed-text-available($uri, $encoding)) then $vamod else
                    m:createStandardTypeError($context, $name, $itemType, $vamod, 
                        ': no text resource at this location')
        else if ($itemType eq 'linesURI') then   
            let $vamod := m:_getValueAndModifiers($itemText, $name, 'linesURI')        
            return if ($vamod/self::z:errors) then $vamod else            
                
            let $uri := replace($vamod, '\\', '/')
            let $encoding := ($vamod/@encoding, 'UTF-8')[1]            
            return
                if (tt:unparsed-text-available($uri, $encoding)) then $vamod else
                    m:createStandardTypeError($context, $name, $itemType, $vamod, 
                        ': no text resource at this location')
        else if ($itemType eq 'textDFD') then
            let $vamod := m:_getValueAndModifiers($itemText, $name, 'textDFD')
            return if ($vamod/self::z:errors) then $vamod else            
          
            let $rcat := tt:rcat($vamod, 'text', 'text', $vamod/@encoding, $vamod) return            
            if ($rcat and not($rcat/self::z:errors)) then $rcat else
                m:createStandardTypeError($context, $name, $itemType, $vamod, 
                    concat(': no valid directory filter descriptor; msg=', string-join($rcat//@msg, '; ')))
        else if ($itemType eq 'textFOX') then
            let $vamod := m:_getValueAndModifiers($itemText, $name, 'textFOX')
            return if ($vamod/self::z:errors) then $vamod else            
          
            let $rcat := tt:rcatFromFoxpath($vamod, 'text', 'text', $vamod/@encoding, $vamod) return            
            if ($rcat and not($rcat/self::z:errors)) then $rcat else
                m:createStandardTypeError($context, $name, $itemType, $vamod, 
                    concat(': no valid directory filter descriptor; msg=', string-join($rcat//@msg, '; ')))
        else if ($itemType eq 'linesDFD') then
            let $vamod := m:_getValueAndModifiers($itemText, $name, 'linesDFD')
            return if ($vamod/self::z:errors) then $vamod else            
           
            let $rcat := tt:rcat($vamod, 'text', 'lines', $vamod/@encoding, $vamod) return            
            if ($rcat and not($rcat/self::z:errors)) then $rcat else
                m:createStandardTypeError($context, $name, $itemType, $vamod, 
                    concat(': no valid directory filter descriptor; msg=', string-join($rcat//@msg, '; ')))
        else if ($itemType eq 'linesFOX') then
            let $vamod := m:_getValueAndModifiers($itemText, $name, 'linesFOX')        
            return if ($vamod/self::z:errors) then $vamod else            
           
            let $rcat := tt:rcatFromFoxpath($vamod, 'text', 'lines', $vamod/@encoding, $vamod) return           
            if ($rcat and not($rcat/self::z:errors)) then $rcat else
                m:createStandardTypeError($context, $name, $itemType, $vamod, 
                    concat(': no valid directory filter descriptor; msg=', string-join($rcat//@msg, '; ')))
        else if ($itemType eq 'xtextDFD') then
            let $vamod := m:_getValueAndModifiers($itemText, $name, 'xtextDFD')
            return if ($vamod/self::z:errors) then $vamod else            
           
            let $rcat := tt:rcat($vamod, 'text', 'xtext', $vamod/@encoding, $vamod) return            
            if ($rcat and not($rcat/self::z:errors)) then $rcat else               
                m:createStandardTypeError($context, $name, $itemType, $vamod, 
                    concat(': no valid directory filter descriptor; msg=', string-join($rcat//@msg, '; ')))
        else if ($itemType eq 'csvDFD') then     
            let $vamod := m:_getValueAndModifiers($itemText, $name, 'csvDFD') 
            return if ($vamod/self::z:errors) then $vamod else
                
            let $rcat := tt:rcat($vamod, 'csv', 'xcsv', $vamod/@encoding, $vamod) return           
                if ($rcat and not($rcat/self::z:errors)) then $rcat else               
                    m:createStandardTypeError($context, $name, $itemType, $vamod, 
                        concat(': no valid directory filter descriptor; msg=', string-join($rcat/z:error/@msg, '; ')))
        else if ($itemType eq 'csvFOX') then     
            let $vamod := m:_getValueAndModifiers($itemText, $name, 'csvFOX')
            return if ($vamod/self::z:errors) then $vamod else
                
            let $rcat := tt:rcatFromFoxpath($vamod, 'csv', 'xcsv', $vamod/@encoding, $vamod) return            
                if ($rcat and not($rcat/self::z:errors)) then $rcat else               
                    m:createStandardTypeError($context, $name, $itemType, $vamod, 
                        concat(': no valid directory filter descriptor; msg=', string-join($rcat/z:error/@msg, '; ')))
                        
        else if ($itemType eq 'jsonFOX') then
            let $rcat := tt:rcatFromFoxpath($itemText, 'json', 'jsonx', (), ()) return            
                if ($rcat and not($rcat/self::z:errors)) then $rcat else               
                    m:createStandardTypeError($context, $name, $itemType, $itemText, 
                        concat(': no valid directory filter descriptor; msg=', string-join($rcat/z:error/@msg, '; ')))

        else if ($itemType eq 'catDFD') then
            let $rcat := tt:rcat($itemText) return            
            if ($rcat and not($rcat/self::z:errors)) then $rcat else
                m:createStandardTypeError($context, $name, $itemType, $itemText, 
                    concat(': no valid directory filter descriptor; msg=', string-join($rcat/z:error/@msg, '; ')))

        else if ($itemType eq 'catFOX') then
            let $rcat := tt:rcatFromFoxpath($itemText) return           
            if ($rcat and not($rcat/self::z:errors)) then $rcat else
                m:createStandardTypeError($context, $name, $itemType, $itemText, 
                    concat(': no valid directory filter descriptor; msg=', string-join($rcat/z:error/@msg, '; ')))
                    
        else if ($itemType eq 'docDFD') then
            let $rcat := tt:rcat($itemText) return            
            if ($rcat and not($rcat/self::z:errors)) then $rcat else
                m:createStandardTypeError($context, $name, $itemType, $itemText, 
                    concat(': no valid directory filter descriptor; msg=', string-join($rcat/z:error/@msg, '; ')))

        else if ($itemType eq 'docFOX') then
            let $rcat := tt:rcatFromFoxpath($itemText) return            
            if ($rcat and not($rcat/self::z:errors)) then $rcat else
                m:createStandardTypeError($context, $name, $itemType, $itemText, 
                    concat(': no valid foxpath expr; msg=', string-join($rcat/z:error/@msg, '; ')))

        (: *** directory, file 
               --------------- :)           
        else if ($itemType eq 'directory') then  
            let $itext := replace($itemText, '\\', '/')
            (: let $value := resolve-uri($itext, static-base-uri()) :)
            let $value := tt:resolve-uri($itext)
            let $value := file:resolve-path($value)
            let $value := replace(replace($value, '\\', '/'), '/$', '')            
            return $value
        else if ($itemType eq 'file') then  
            let $itext := replace($itemText, '\\', '/')
            (: let $value := resolve-uri($itext, static-base-uri()) :)
            let $value := tt:resolve-uri($itext)            
            let $value := file:resolve-path($value)
            let $value := replace(replace($value, '\\', '/'), '/$', '')            
            return $value
            

        else tt:parseNonStandardItemType($name, $itemType, $itemText)
    return
        if ($typedItem instance of element(z:error)) then tt:wrapErrors($typedItem)
        else
            $typedItem
};

(:~
 : Parses the text of a parameter value item with a builtin type
 : into a typed value.
 :
 : @param context if set, the other input parameters represent a modifier 
 :    of a single parameter item, otherwise an operation parameter; 
 :    in the first case, $context describes the operation parameter 
 :    whose modifier is evaluated: parameter name, item type and 
 :    item text
 : @param itemText the item value text
 : @param itemType the item type
 : @param name the parameter name
 : @return the typed value of the parameter item
 :)
declare function m:_parseParamValueItem_builtin($context as element(context)?,
                                                $itemText as xs:string, 
                                                $itemType as xs:string, 
                                                $name as xs:string)
        as item()? {
    let $typedItem := 
        if ($itemType eq 'xs:string') then $itemText
        else if ($itemType eq 'xs:normalizedString') then
            if ($itemText castable as xs:normalizedString) then xs:normalizedString($itemText) else
                m:createStandardTypeError($context, $name, $itemType, $itemText)            
        else if ($itemType eq 'xs:token') then
            if ($itemText castable as xs:token) then xs:token($itemText) else
                m:createStandardTypeError($context, $name, $itemType, $itemText)            
        else if ($itemType eq 'xs:language') then
            if ($itemText castable as xs:language) then xs:language($itemText) else
                m:createStandardTypeError($context, $name, $itemType, $itemText)            
        else if ($itemType eq 'xs:NMTOKEN') then
            if ($itemText castable as xs:NMTOKEN) then xs:NMTOKEN($itemText) else
                m:createStandardTypeError($context, $name, $itemType, $itemText)            
        else if ($itemType eq 'xs:Name') then
            if ($itemText castable as xs:Name) then xs:Name($itemText) else
                m:createStandardTypeError($context, $name, $itemType, $itemText)            
        else if ($itemType eq 'xs:NCName') then
            if ($itemText castable as xs:NCName) then xs:NCName($itemText) else
                m:createStandardTypeError($context, $name, $itemType, $itemText)            
        else if ($itemType eq 'xs:ID') then
            if ($itemText castable as xs:ID) then xs:ID($itemText) else
                m:createStandardTypeError($context, $name, $itemType, $itemText)            
        else if ($itemType eq 'xs:IDREF') then
            if ($itemText castable as xs:IDREF) then xs:IDREF($itemText) else
                m:createStandardTypeError($context, $name, $itemType, $itemText)            
        else if ($itemType eq 'xs:dateTime') then
            if ($itemText castable as xs:dateTime) then xs:dateTime($itemText) else
                m:createStandardTypeError($context, $name, $itemType, $itemText)
        else if ($itemType eq 'xs:date') then
            if ($itemText castable as xs:date) then xs:date($itemText) else
                m:createStandardTypeError($context, $name, $itemType, $itemText)
        else if ($itemType eq 'xs:time') then
            if ($itemText castable as xs:time) then xs:time($itemText) else
                m:createStandardTypeError($context, $name, $itemType, $itemText)
        else if ($itemType eq 'xs:duration') then
            if ($itemText castable as xs:duration) then xs:duration($itemText) else
                m:createStandardTypeError($context, $name, $itemType, $itemText)
        else if ($itemType eq 'xs:yearMonthDuration') then
            if ($itemText castable as xs:yearMonthDuration) then xs:yearMonthDuration($itemText) else
                m:createStandardTypeError($context, $name, $itemType, $itemText)
        else if ($itemType eq 'xs:dayTimeDuration') then
            if ($itemText castable as xs:dayTimeDuration) then xs:dayTimeDuration($itemText) else
                m:createStandardTypeError($context, $name, $itemType, $itemText)
        else if ($itemType eq 'xs:float') then
            if ($itemText castable as xs:float) then xs:float($itemText) else               
                m:createStandardTypeError($context, $name, $itemType, $itemText)
        else if ($itemType eq 'xs:double') then
            if ($itemText castable as xs:double) then xs:double($itemText) else
                m:createStandardTypeError($context, $name, $itemType, $itemText)            
        else if ($itemType eq 'xs:decimal') then
            if ($itemText castable as xs:decimal) then xs:decimal($itemText) else
                m:createStandardTypeError($context, $name, $itemType, $itemText)            
        else if ($itemType eq 'xs:integer') then
            if ($itemText castable as xs:integer) then xs:integer($itemText) else
                m:createStandardTypeError($context, $name, $itemType, $itemText)            
        else if ($itemType eq 'xs:nonPositiveInteger') then
            if ($itemText castable as xs:nonPositiveInteger) then xs:nonPositiveInteger($itemText) else
                m:createStandardTypeError($context, $name, $itemType, $itemText)            
        else if ($itemType eq 'xs:negativeInteger') then
            if ($itemText castable as xs:negativeInteger) then xs:negativeInteger($itemText) else
                m:createStandardTypeError($context, $name, $itemType, $itemText)            
        else if ($itemType eq 'xs:long') then
            if ($itemText castable as xs:long) then xs:long($itemText) else
                m:createStandardTypeError($context, $name, $itemType, $itemText)            
        else if ($itemType eq 'xs:int') then
            if ($itemText castable as xs:int) then xs:int($itemText) else
                m:createStandardTypeError($context, $name, $itemType, $itemText)            
        else if ($itemType eq 'xs:short') then
            if ($itemText castable as xs:short) then xs:short($itemText) else
                m:createStandardTypeError($context, $name, $itemType, $itemText)            
        else if ($itemType eq 'xs:byte') then
            if ($itemText castable as xs:byte) then xs:byte($itemText) else
                m:createStandardTypeError($context, $name, $itemType, $itemText)            
        else if ($itemType eq 'xs:nonNegativeInteger') then
            if ($itemText castable as xs:nonNegativeInteger) then xs:nonNegativeInteger($itemText) else
                m:createStandardTypeError($context, $name, $itemType, $itemText)
        else if ($itemType eq 'xs:unsignedLong') then
            if ($itemText castable as xs:unsignedLong) then xs:unsignedLong($itemText) else
                m:createStandardTypeError($context, $name, $itemType, $itemText)
        else if ($itemType eq 'xs:unsignedInt') then
            if ($itemText castable as xs:unsignedInt) then xs:unsignedInt($itemText) else
                m:createStandardTypeError($context, $name, $itemType, $itemText)
        else if ($itemType eq 'xs:unsignedShort') then
            if ($itemText castable as xs:unsignedShort) then xs:unsignedShort($itemText) else
                m:createStandardTypeError($context, $name, $itemType, $itemText)
        else if ($itemType eq 'xs:unsignedByte') then
            if ($itemText castable as xs:unsignedByte) then xs:unsignedByte($itemText) else
                m:createStandardTypeError($context, $name, $itemType, $itemText)                
        else if ($itemType eq 'xs:positiveInteger') then
             if ($itemText castable as xs:positiveInteger) then xs:positiveInteger($itemText) else
                m:createStandardTypeError($context, $name, $itemType, $itemText)             
        else if ($itemType eq 'xs:gYearMonth') then
            if ($itemText castable as xs:gYearMonth) then xs:gYearMonth($itemText) else
                m:createStandardTypeError($context, $name, $itemType, $itemText)            
        else if ($itemType eq 'xs:gYear') then
            if ($itemText castable as xs:gYear) then xs:gYear($itemText) else
                m:createStandardTypeError($context, $name, $itemType, $itemText)            
        else if ($itemType eq 'xs:gMonthDay') then
            if ($itemText castable as xs:gMonthDay) then xs:gMonthDay($itemText) else
                m:createStandardTypeError($context, $name, $itemType, $itemText)            
        else if ($itemType eq 'xs:gDay') then
            if ($itemText castable as xs:gDay) then xs:gDay($itemText) else
                m:createStandardTypeError($context, $name, $itemType, $itemText)            
        else if ($itemType eq 'xs:gMonth') then
            if ($itemText castable as xs:gMonth) then xs:gMonth($itemText) else
                m:createStandardTypeError($context, $name, $itemType, $itemText)
        else if ($itemType eq 'xs:boolean') then
            if ($itemText castable as xs:boolean) then xs:boolean($itemText) else
                m:createStandardTypeError($context, $name, $itemType, $itemText)           
        else if ($itemType eq 'xs:base64Binary') then
            if ($itemText castable as xs:base64Binary) then xs:base64Binary($itemText) else
                m:createStandardTypeError($context, $name, $itemType, $itemText)           
        else if ($itemType eq 'xs:hexBinary') then
            if ($itemText castable as xs:hexBinary) then xs:hexBinary($itemText) else
                m:createStandardTypeError($context, $name, $itemType, $itemText)           
        else if ($itemType eq 'xs:anyURI') then
            if ($itemText castable as xs:anyURI) then xs:anyURI($itemText) else
                m:createStandardTypeError($context, $name, $itemType, $itemText)
        else if ($itemType eq 'xs:QName') then
            if ($itemText castable as xs:QName) then xs:QName($itemText) else
                m:createStandardTypeError($context, $name, $itemType, $itemText)
        else ()                    
    return
        $typedItem
};
