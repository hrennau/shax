(:~
 : _request_facets.xqm - a function for checking parameter values against facets
 :
 : @version 20160626-1
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
   "_pfilter_parser.xqm",   
   "_rcat.xqm",   
   "_request_getters.xqm",
   "_reportAssistent.xqm",
   "_resourceAccess.xqm",
   "_stringTools.xqm";   
:)   
import module namespace tt="http://www.ttools.org/xquery-functions" at
   "_constants.xqm",
   "_errorAssistent.xqm"   
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
 : Validates a parameter value item against parameter facets.
 :
 : Error policy: in case of errors, a `z:errors` element is returned,
 : rather than a data item. 
 :
 : @param context if set, the other input parameters represent modifiers 
 :    of a single parameter item, otherwise an operation parameter; 
 :    in the first case, $context describes the operation parameter 
 :    whose modifier is evaluated: parameter name, item type and 
 :    item text
 : @param itemText the text of the item  
 : @param itemValue the typed value of the item
 : @param paramConfig the parameter configuration
 : @return the empty sequence if the parameter or modifier is
 :    not found in error, and one or more error elements otherwise 
 :)
declare function m:_checkFacets($context as element(context)?,
                                $itemText as xs:string, 
                                $itemValue as item(), 
                                $paramConfig as element()?)
        as element()* {
    if (not($paramConfig)) then ()
    else if ($itemValue instance of element(z:error) 
            or $itemValue instance of element(z:errors))  
        then () else
    
    let $itemType := $paramConfig/@itemType/tt:adaptItemTypeOfNonStandardItemType(.)
    let $name := $paramConfig/@name
        
    let $errors := (            
        (: *** check @minDocCount, @maxDocCount, @docCount :)
        if (not($paramConfig/(@fct_minDocCount, @fct_maxDocCount, @fct_docCount))) then () else
        let $minCount := $paramConfig/@fct_minDocCount/xs:integer(.)
        let $maxCount := $paramConfig/@fct_maxDocCount/xs:integer(.)
        let $count := $paramConfig/@fct_docCount/xs:integer(.)
        let $actCount := count($itemValue/*/@href[doc-available(.)])
        let $actFileCount := count($itemValue/*)
        return
            if ($actCount lt $minCount) then
                let $msg :=
                    if ($actFileCount eq 0) then
                        '; parameter value does not select any files.'
                    else if ($actCount eq 0) then
                        '; parameter value does not select any well-formed XML documents.'
                    else
                        concat('; parameter value selects ', $actCount, ' documents, should select ',
                            'at least ', $minCount, ' documents.')
                return
                    m:createFacetError($context, $name, 'minDocCount', $paramConfig/@fct_minDocCount, 
                        $itemText, $msg)
                    
            else if ($actCount gt $maxCount) then
                m:createFacetError($context, $name, 'maxDocCount', $paramConfig/@fct_maxDocCount, $itemText,
                    concat(
                    '; parameter value selects ', $actCount, ' documents, should select ',
                            'at most ', $maxCount, ' documents.'))
            else if ($actCount ne $count) then
                let $msg :=
                    if ($actFileCount eq 0) then
                        '; parameter value does not select any files.'
                    else if ($actCount eq 0) then
                        '; parameter value does not select any well-formed XML documents.'
                    else
                        concat('; parameter value selects ', $actCount, ' documents, should select ',
                            $count, ' documents.')
                return
                    m:createFacetError($context, $name, 'countDocs', $paramConfig/@fct_docCount, 
                        $itemText, $msg)
            else ()
        ,
        (: *** check @values :)
        if (not($paramConfig/@fct_values)) then () else
        let $expectedItems := tokenize($paramConfig/@fct_values, ',\s*')
        return
            if ($itemText = $expectedItems) then () else  
                m:createFacetError($context, $name, 'values', $paramConfig/@fct_values, $itemText, 
                    '; the item value must be equal to one of these comma-separated values')
        ,
        (: *** check @pattern :)
        if (not($paramConfig/@fct_pattern)) then () else
        let $patternSpec := $paramConfig/@fct_pattern
        let $pattern := concat('^', replace($patternSpec, '#.*', ''), '$')
        let $options := string(replace($patternSpec, '.*#', '')[not(. eq $patternSpec)])
        return
            if (matches($itemText, $pattern, $options)) then () else
                m:createFacetError($context, $name, 'pattern', $paramConfig/@fct_pattern, $itemText, 
                    '; the item value must match the pattern')                
        ,
        (: *** check @length :)
        if (not($paramConfig/@fct_length)) then () else
        let $length := $paramConfig/@fct_length/xs:integer(.)
        return
            if (string-length($itemText) eq $length) then () else
                m:createFacetError($context, $name, 'length', $paramConfig/@fct_length, $itemText, 
                    '; the item value must have the specified length')                
        ,
        (: *** check @minLength :)
        if (not($paramConfig/@fct_minLength)) then () else
        let $minLength := $paramConfig/@fct_minLength/xs:integer(.)
        return
            if (string-length($itemText) ge $minLength) then () else                   
                m:createFacetError($context, $name, 'minLength', $paramConfig/@fct_minLength, $itemText, 
                    '; the item value must have a length greater than or equal the specified minimum length')               
        ,
        (: *** check @maxLength :)
        if (not($paramConfig/@fct_maxLength)) then () else
        let $maxLength := $paramConfig/@fct_maxLength/xs:integer(.)
        return
            if (string-length($itemText) le $maxLength) then () else                  
                m:createFacetError($context, $name, 'maxLength', $paramConfig/@fct_maxLength, $itemText, 
                    '; the item value must have a length less than or equal the specified maximum length')               
        ,
        (: *** check @min :)
        if (not($paramConfig/@fct_min)) then () else
        let $min := 
            if ($itemType eq 'xs:date') then $paramConfig/@fct_min/xs:date(.)
            else if ($itemType eq 'xs:time') then $paramConfig/@fct_min/xs:time(.)                
            else if ($itemType eq 'xs:dateTime') then $paramConfig/@fct_min/xs:dateTime(.)
            else $paramConfig/@fct_min/number(.)               
        return
            if ($itemValue ge $min) then () else                   
                m:createFacetError($context, $name, 'min', $paramConfig/@fct_min, $itemText, 
                    '; the item value must have a value greater than or equal the specified minimum value')
        ,
        (: *** check @minEx :)
        if (not($paramConfig/@fct_minEx)) then () else
        let $minEx := 
            if ($itemType eq 'xs:date') then $paramConfig/@fct_minEx/xs:date(.)
            else if ($itemType eq 'xs:time') then $paramConfig/@fct_minEx/xs:time(.)                
            else if ($itemType eq 'xs:dateTime') then $paramConfig/@fct_minEx/xs:dateTime(.)
            else $paramConfig/@fct_minEx/number(.)               
        return
            if ($itemValue gt $minEx) then () else                   
                m:createFacetError($context, $name, 'minEx', $paramConfig/@fct_minEx, $itemText, 
                    '; the item value must have a value greater than the specified exclusive minimum value')
        ,
        (: *** check @max :)
        if (not($paramConfig/@fct_max)) then () else
        let $max :=
            if ($itemType eq 'xs:date') then $paramConfig/@fct_max/xs:date(.)
            else if ($itemType eq 'xs:time') then $paramConfig/@fct_max/xs:time(.)                
            else if ($itemType eq 'xs:dateTime') then $paramConfig/@fct_max/xs:dateTime(.)
            else $paramConfig/@fct_max/number(.)               
        return
            if ($itemValue le $max) then () else                   
                m:createFacetError($context, $name, 'max', $paramConfig/@fct_max, $itemText, 
                    '; the item value must have a value less than or equal the specified maximum value')
        ,
            
        (: *** check @maxEx :)
        if (not($paramConfig/@fct_maxEx)) then () else
        let $maxEx :=
            if ($itemType eq 'xs:date') then $paramConfig/@fct_maxEx/xs:date(.)
            else if ($itemType eq 'xs:time') then $paramConfig/@fct_maxEx/xs:time(.)                
            else if ($itemType eq 'xs:dateTime') then $paramConfig/@fct_maxEx/xs:dateTime(.)
            else $paramConfig/@fct_maxEx/number(.)               
        return
            if ($itemValue le $maxEx) then () else                   
                m:createFacetError($context, $name, 'maxEx', $paramConfig/@fct_maxEx, $itemText, 
                    '; the item value must have a value less than the specified exclusive maximum value')
        ,
        (: *** check @fileExists :)
        if (not($paramConfig/@fct_fileExists)) then () 
        else if ($paramConfig/@fct_fileExists eq 'true' and not(file:exists($itemValue))) then
            m:createFacetError($context, $name, 'fileExists', $paramConfig/@fct_fileExists, $itemText, 
                '; file not found')
        else if ($paramConfig/@fileExists eq 'false' and file:exists($itemValue)) then              
            m:createFacetError($context, $name, 'fileExists', $paramConfig/@fct_fileExists, $itemText, 
                '; the file must not yet exist')
        else ()
        ,
        (: *** check @dirExists :)
        if (not($paramConfig/@fct_dirExists)) then () 
        else if ($paramConfig/@fct_dirExists eq 'true' and not(file:exists($itemValue))) then              
            m:createFacetError($context, $name, 'dirExists', $paramConfig/@fct_dirExists, $itemText, 
                '; directory not found')
        else if ($paramConfig/@fct_dirExists eq 'false' and file:exists($itemValue)) then              
            m:createFacetError($context, $name, 'dirExists', $paramConfig/@fct_dirExists, $itemText, 
                '; the directory must not yet exist')
        else ()
        ,
        (: *** check @rootElem :)
        if (not($paramConfig/@fct_rootElem)) then ()
        else
            let $facet := $paramConfig/@fct_rootElem/normalize-space(.)
            let $facetName := replace($facet, '^Q\{.*\}', '')               
            let $facetNamespace := replace($facet, '^Q\{(.*)\}.*', '$1')[. ne $facet]                
            return 
                if ($itemType eq 'docURI') then
                    let $root := tt:doc($itemValue)/*
                    let $lname := local-name($root)
                    let $ns := namespace-uri($root)
                    return
                        if ($lname eq $facetName and
                            not($ns ne $facetNamespace)) then ()
                        else
                            let $msg :=
                                if ($lname ne $facetName) then
                                    concat("the document root should be a '", $facetName,
                                    "' element, but is a",
                                    if (matches($lname, '^[aeioux]')) then 'n' else (),
                                    " '", $lname, "' element")
                                else
                                    concat("the document root element should be in namespace: ", $facetNamespace,
                                    ", but is ", 
                                    if (string($ns)) then concat("in namespace: ", $ns)
                                    else "in no namespace")
                        return                                    
                            m:createFacetError($context, $name, $itemText, $msg)
                else ()
        ,
(:                    
        (: *** check @rootName/@rootNamespace :)
        if (not($paramConfig/@fct_rootName) and 
            not($paramConfig/@fct_rootNamespace)) then ()
        else
            let $facetName := $paramConfig/@fct_rootName/normalize-space(.)                
            let $facetNamespace := $paramConfig/@fct_rootNamespace/normalize-space(.)                
            return 
                if ($itemType eq 'docURI') then
                    let $root := tt:doc($itemValue)/*
                    let $lname := local-name($root)
                    let $ns := namespace-uri($root)
                    return
                        if ($lname eq $facetName and
                            not($ns ne $facetNamespace)) then ()
                        else
                            let $msg :=
                                if ($lname ne $facetName) then
                                    concat("the document root should be a '", $facetName,
                                    "' element, but is a",
                                    if (matches($lname, '^[aeioux]')) then 'n' else (),
                                    " '", $lname, "' element")
                                else
                                    concat("the document root element should be in namespace: ", $facetNamespace,
                                    ", but is ", 
                                    if (string($ns)) then concat("in namespace: ", $ns)
                                    else "in no namespace")
                        return                                    
                            m:createFacetError($context, $name, $itemText, $msg)
                else (),
:)          
        m:checkNonStandardFacets($itemText, $itemValue, $paramConfig)
    )
    return
        if (empty($errors)) then () else
            <z:errors>{$errors}</z:errors>           
};
