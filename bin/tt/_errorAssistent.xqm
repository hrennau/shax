(: errorAssistent.xqm - provides utilities for creating and processing errors
 :
 : @version 20140918-1 first version 
 : ===================================================================================
 :)

module namespace m="http://www.ttools.org/xquery-functions";

import module namespace i="http://www.ttools.org/xquery-functions" at
    "_constants.xqm",
    "_reportAssistent.xqm",
    "_stringTools.xqm";

declare namespace z="http://www.ttools.org/structure";

(:
 : ============================================================================
 :
 :     p u b l i c    f u n c t i o n s
 :
 : ============================================================================
 :)

(:~
 : Creates an error element.
 :
 : @param errorCode the error code
 : @param msg the error message
 : @param details an element whose attributes are copied into the
 :    error element, specifying details, where the name and value
 :    of an attribute specifies the kind and value of a detail
 :)
declare function m:createError($errorCode as xs:string,
                               $msg as xs:string,
                               $details as element()?)
        as element(z:error) {
    <z:error code="{$errorCode}">{
        $details/@*,
        attribute msg {$msg}
    }</z:error> 
};        

(:~
 : Creates an error element.
 :
 : @param errorCode the error code
 : @param msg the error message
 : @param details an element whose attributes are copied into the
 :    error element, specifying details, where the name and value
 :    of an attribute specifies the kind and value of a detail
 :)
declare function m:createError($errorCode as xs:string,
                               $msg as xs:string)
        as element(z:error) {
    m:createError($errorCode, $msg, ())
};        

(:~
 : Creates an error element reporting a parameter type error. This
 : function is meant for use within the parser function of a user-defined
 : type.
 :
 : @param itemType the item type
 : @param itemText the item text
 : @param a message which will be appended to a standard prefix 
 :   revealing the type name and the parameter text value
 : @return an error element
 :)
declare function m:createTypeError($itemType as xs:string,
                                   $itemText as xs:string,
                                   $msg as xs:string)
        as element(z:error) {
    let $message := concat(
        "Value '", $itemText, "' not a valid instance of type ", $itemType, "; ", 
        lower-case(substring($msg, 1, 1)), substring($msg, 2))
    return        
        <z:error code="PARAMETER_TYPE_ERROR" itemType="{$itemType}" itemText="{$itemText}" msg="{$message}"/> 
};

(:~
 : Creates an error element reporting a parameter error not related to the param value.
 :
 : @param context if not set, $paramName identifies a parameter, otherwise $paramName
 :    identifies a modifier belonging to the parameter identified by $context/@paramName 
 : @param paramName a parameter name or modifier name
 : @param errorCode the error code
 : @param msg the error msg 
 : @return an error element
 :)
declare function m:createParamError($context as element(context)?,
                                    $paramName as xs:string,
                                    $errorCode as xs:string,
                                    $msg as xs:string)
        as element(z:error) {
    let $itemIdent :=
        if (not($context)) then concat("Parameter '", $paramName, "'")
        else concat("Parameter '", $context/@paramName, "', modifier '", $paramName, "'")        
    let $useMsg := concat($itemIdent, ': ', $msg)
    return        
        <z:error code="{$errorCode}" msg="{$useMsg}"/> 
};

(:~
 : Creates an error element reporting a parameter facet error. This
 : function is meant for use within the check function of a user-defined
 : facet.
 :
 : @param context if set, identifies the parameter of which a modifier is found in error 
 : @param paramName name of the parameter or modifier found in error
 : @param facetType the type of the facet which has been violated
 : @param facetValue the value of the facet which has been violated 
 : @param itemValue the item value which violates the facet
 : @param msgSuffix a message which will be appended to a standard prefix 
 :   revealing the parameter name and type and the facet type and value
 : @return an error element
 :)
declare function m:createFacetError($context as element(context)?,
                                    $paramName as xs:string,
                                    $facetType as xs:string,
                                    $facetValue as xs:string,
                                    $itemValue as xs:string,
                                    $msgSuffix as xs:string?)
        as element(z:error) {
    let $messagePrefix :=
        if (not($context)) then
            concat("Parameter '", $paramName, "': item value ('", $itemValue, "') ")
        else            
            concat("Parameter '", $context/@paramName, "', item value '", $context/@itemText, 
            "', modifier '", $paramName, "': value ('", $itemValue, "') ")        
        
    let $message := concat($messagePrefix, "not facet-valid; facet=", $facetType,        
                            ", facet value=", $facetValue, $msgSuffix)
    return        
        <z:error code="PARAMETER_FACET_ERROR" paramName="{$paramName}" itemValue="{$itemValue}" facetType="{$facetType}" 
                 facetValue="{$facetValue}" msg="{$message}"/> 
};

(:~
 : Creates an error element reporting a parameter facet error. This
 : function is meant for use within the check function of a user-defined
 : facet.
 :
 : Function variant which does not receive facet name and value as input
 : parameters, expecting the message received as input parameter to
 : contain an identification of the facet.
 :
 : Usage note: meant for cases where the standardized construction of
 : the message results in a text not sufficiently elegant and natural.
 :
 : @param context if set, identifies the parameter of which a modifier is found in error 
 : @param paramName name of the parameter or modifier found in error
 : @param itemValue the item value which violates the facet
 : @param msgSuffix a message which will be appended to a standard prefix 
 :   revealing the parameter name and type
 : @return an error element
 :)
declare function m:createFacetError($context as element(context)?,
                                    $paramName as xs:string,
                                    $itemValue as xs:string,
                                    $msgSuffix as xs:string?)
        as element(z:error) {
    let $messagePrefix :=
        if (not($context)) then
            concat("Parameter '", $paramName, "': item value ('", $itemValue, "') ")
        else            
            concat("Parameter '", $context/@paramName, "', item value '", $context/@itemText, 
            "', modifier '", $paramName, "': value ('", $itemValue, "') ")        
        
    let $message := concat($messagePrefix, "not facet-valid; ", $msgSuffix)
    return        
        <z:error code="PARAMETER_FACET_ERROR" paramName="{$paramName}" itemValue="{$itemValue}" msg="{$message}"/> 
};

(:~
 : Creates an error element reporting a parameter name error. This
 : function is meant for use within function '_normalizeParamNames'.
 :
 : @param context if set, identifies the parameter of which a modifier is found in error
 : @param errorCode the error code to be reported
 : @param paramName name of the parameter or modifier found in error
 : @param validNames the valid parameter or modifier names 
 : @param matchingNames the matching parameter or modifier names
 : @return an error element
 :)
declare function m:createParamNameError($context as element(context)?,
                                        $errorCode as xs:string,
                                        $paramName as xs:string,
                                        $validNames as xs:string+,
                                        $matchingNames as xs:string*)
        as element(z:error) {
    if ($errorCode eq 'UNKNOWN_PARAMETER_NAME') then
        let $useErrorCode :=
            if ($context) then 'UNKNOWN_MODIFIER_NAME' else $errorCode
        return        
            if (not($context)) then    
                <z:error code="{$useErrorCode}" name="{$paramName}"
                    msg="{concat("Unknown parameter name ('", $paramName, "'); ",
                    "valid names: ", string-join($validNames, ', '))}"/>
            else                
                <z:error code="{$useErrorCode}" name="{$paramName}"
                    msg="{concat("Parameter '", $context/@paramName, "': unknown modifier name ('", $paramName,
                          "'); valid modifier names: ", string-join($validNames, ', '))}"/>
    
    else if ($errorCode eq 'AMBIGUOUS_PARAMETER_NAME') then
        let $useErrorCode :=
            if ($context) then 'AMBIGUOUS_MODIFIER_NAME' else $errorCode
        return
            if (not($context)) then    
                <z:error code="{$useErrorCode}" name="{$paramName}"
                    msg="{concat("Ambiguous parameter name ('", $paramName, "'); ",
                    "matching names: ", string-join($matchingNames, ', '))}"/>
            else                
                <z:error code="{$useErrorCode}" name="{$paramName}"
                    msg="{concat("Parameter '", $context/@paramName, "': ambiguous modifier name ('", $paramName,
                          "'); matching modifier names: ", string-join($validNames, ', '))}"/>
    else
        error(QName($m:URI_ERROR, 'SYSTEM_ERROR'), concat('Unexpected error code: ', $errorCode))
};

declare function m:createStandardTypeError($context as element(context)?,
                                           $paramName as xs:string,
                                           $itemType as xs:string,
                                           $itemValue as xs:string)
        as element(z:error) {
    let $typeName := replace($itemType, '^xs:', '')
    let $messagePrefix :=
        if (not($context)) then
            concat("Parameter '", $paramName, "': item value ('", $itemValue, "') ")
        else            
            concat("Parameter '", $context/@paramName, "', item value '", $context/@itemText, 
            "', modifier '", $paramName, "': value ('", $itemValue, "') ")        
    let $message := concat($messagePrefix, "not a valid ", $typeName, " value")
    return        
        <z:error code="PARAMETER_TYPE_ERROR" paramName="{$paramName}" itemType="{$itemType}" itemValue="{$itemValue}" msg="{$message}"/> 
};

declare function m:createStandardTypeError($context as element(context)?,
                                           $paramName as xs:string,
                                           $itemType as xs:string,
                                           $itemValue as xs:string,
                                           $msgSuffix as xs:string?)
        as element(z:error) {
    let $typeName := replace($itemType, '^xs:', '')
    let $messagePrefix :=
        if (not($context)) then
            concat("Parameter '", $paramName, "': item value ('", $itemValue, "') ")
        else            
            concat("Parameter '", $context/@paramName, "' item value '", $context/@itemText, 
            "', modifier '", $paramName, "': value ('", $itemValue, "') ")        
    let $message := concat(
        $messagePrefix, "not a valid ", $typeName, " value", $msgSuffix)
    return        
        <z:error code="PARAMETER_TYPE_ERROR" paramName="{$paramName}" itemType="{$itemType}" itemValue="{$itemValue}" msg="{$message}"/> 
};

(:~
 : Extracts from a sequence `error` elements and wraps them in an
 : `errors` element. Extracted `error` elements may be top-level
 : or descendants of the input elements. In particular, they may or
 : may not be contained by `z:errors` elements. Returns the empty 
 : sequence if the input sequence does not contain `error` 
 : elements.
 :
 : @param errors the error elements
 : @return the container element containing the error elements
 :) 
declare function m:extractErrors($elems as node()*)
        as element(z:errors)? {
    let $errors := $elems/
        (descendant-or-self::z:error, descendant-or-self::z:errors/*)
    return
        if (not($errors)) then () else
            <z:errors>{$errors}</z:errors>        
};

(:~
 : Wraps a sequence of error elements in a container element.
 : The input sequence $errors is expected to consist of
 : `z:error` and/or `z:errors` elements.
 :
 : @param errors a sequence of `z:error` and/or `z:errors` elements
 : @return the container element containing the error elements
 :) 
declare function m:wrapErrors($errors as element()*)
        as element(z:errors)? {
    let $errorElems := ($errors/self::z:error, $errors/self::z:errors/z:error)
    return
        if (empty($errorElems)) then () else <z:errors>{$errorElems}</z:errors>        
};

(:
 : ============================================================================
 :
 :     p r i v a t e    f u n c t i o n s
 :
 : ============================================================================
 :)

(:~
 : Writes an error report. Note that the 'error' elements may be in the
 : general topicTools namespace, as well as in a tool specific namespace.
 : Therefore, the element test does not specify an element name.
 :)
declare function m:_getErrorReport($errors as element()*, $format as xs:string?)
        as element(z:errorReport) {
    m:_getErrorReport($errors, 'Invalid call', 'code', $format)
};        

(:~
 : Writes an error report. Note that the 'error' elements may be in the
 : general topicTools namespace, as well as in a tool specific namespace.
 : Therefore, the element test does not specify an element name.
 :)
declare function m:_getErrorReport($errors as element()*, $label as xs:string, $leftColAtt as xs:string, $format as xs:string?)
        as element(z:errorReport) {
    let $errors := for $err in $errors return if ($err/self::z:errors) then $err/* else $err        
    let $msgs := $errors/concat(m:padRight(@*[local-name(.) eq $leftColAtt], 40), m:_foldText(@msg, 100, 40, 43))
    return
    <z:errorReport>{
        string-join((
            '', 
            $label,
            string-join(for $i in 1 to string-length($label) return '=', ''),
            '',
            $msgs, 
            '',
            '-------------------------------------',
            ''
        ), '&#xA;')  
    }</z:errorReport>
};        

