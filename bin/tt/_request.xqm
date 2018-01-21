(:~
 : _request.xqm - a function for loading a request from the commnand line string
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
   "_pfilter_parser.xqm",   
   "_rcat.xqm",   
   "_request_facets.xqm",   
   "_request_getters.xqm",
   "_request_valueParser.xqm",   
   "_reportAssistent.xqm",   
   "_stringTools.xqm";   
:)   
import module namespace tt="http://www.ttools.org/xquery-functions" at
   "_constants.xqm",
   "_errorAssistent.xqm",
   "_request_valueParser.xqm"
   ;   

declare namespace z="http://www.ttools.org/structure";
declare namespace file="http://expath.org/ns/file";

(:
 : ============================================================================
 :
 :     p u b l i c    f u n c t i o n s
 :
 : ============================================================================
 :)
 
(:~
 : Parses a request string into a data structure. Operation name
 : and parameter values can be retrieved using functions of
 : module `_request_getters.xqm`.
 : 
 : @param request the request string
 : @param toolScheme a model of the tool interface
 : @return an element representing the request
 :)
declare function m:loadRequest($request as xs:string, $toolScheme as element()?)
        as element() {
    let $msg1 := m:_parseRequest($request)
    let $msg2 := m:_normalizeOperationName($msg1, $toolScheme)
    let $errors := $msg2/z:errors    
    return if ($errors) then $errors else    
    
    let $operationName := $msg2/local-name(.)
    let $rawConfig := $toolScheme//operation[@name eq $operationName]        
    let $config := m:_augmentOperationConfig($rawConfig)
    let $errors := $config/z:errors    
    return if ($errors) then $errors else
    
    let $msg3 := m:_normalizeParamNames((), $msg2, $config)
    let $errors := $msg3/z:errors    
    return if ($errors) then $errors else    

    let $msg4 := m:_addDefaultValues($msg3, $config)
    let $msg5 := m:_itemizeParamValues($msg4, $config)
    let $msg6 := m:_checkCardinalities((), $msg5, $config)    
    let $msg7 := m:_parseParamValues((), $msg6, $config)
    let $groupErrors := m:_checkParamGroups($msg7, $config)
    let $errorsAll := tt:wrapErrors(($msg7/z:errors, $groupErrors))
    return
        if ($errorsAll) then $errorsAll else $msg7 
};

(:
 : ============================================================================
 :
 :     p r i v a t e    f u n c t i o n s
 :
 : ============================================================================
 :)

(:~
 : Parses the text of a parameter item and returns a `vamod` element whose text 
 : content is the value string of the parameter item and whose attributes 
 : represent the modifiers of the parameter item.
 :
 : If the parsing of modifiers detects errors (e.g. use of an unknown 
 : modifier name or invalid modifier value), an `errors` element 
 : is returned whose children describe the error(s).
 :
 : @param itemText the text of the parameter item
 : @param paramName the parameter name
 : @param paramItemType the item type of the parameter
 : @return a `vamod` element representing value string and modifiers,
 :    or a `errors` element describing errors
 :)
declare function m:_getValueAndModifiers($itemText as xs:string, 
                                         $paramName as xs:string,
                                         $paramItemType as xs:string)
        as element() {
    let $parts := tt:splitString($itemText, '<', true(), ';')
    let $value := $parts[1]
    let $mods := subsequence($parts, 2)
    let $modifiers :=
            let $context := 
                <context paramName="{$paramName}" 
                         itemText="{$itemText}" 
                         itemType="{$paramItemType}"/>
            return
                m:_loadModifiers($context, $mods)
    return
        if ($modifiers/self::z:errors) then $modifiers else
            <vamod>{$modifiers/@*, $value}</vamod>        
};

(:~
 : Transforms a modifier string into a control element. Each modifier field
 : is represented by an attribute on this element, where field name and
 : value correspond to attribute name and value.
 : 
 : @param context describes the operation parameter whose modifiers are 
 :    evaluated: parameter name, item type and item text
 : @param modifiers a modifiers string
 : @return an element representing the request
 :)
declare function m:_loadModifiers($context as element(context),
                                  $modifiers as xs:string*)
        as element()? {
    let $mod1 := m:_parseModifiers($modifiers)
    let $config :=
        let $raw := tt:_getParamModifierScheme($context/@itemType)         
        let $translated := tt:_normalizeModifierScheme($raw)
        return
            m:_augmentOperationConfig($translated)
    let $errors := $config/z:errors    
    return if ($errors) then $errors else
    
    let $mod2 := m:_normalizeParamNames($context, $mod1, $config)
    let $errors := $mod2/z:errors    
    return if ($errors) then $errors else    

    let $mod3 := m:_addDefaultValues($mod2, $config)
    let $mod4 := m:_itemizeParamValues($mod3, $config)
    let $mod5 := m:_checkCardinalities($context, $mod4, $config)    
    let $mod6 := m:_parseParamValues($context, $mod5, $config)    
    let $errors := tt:wrapErrors($mod6/z:errors)
    return
        if ($errors) then $errors else m:getControl($mod6, (), ()) 
};

(:~
 : Parses a request string into a data structure. The root element
 : is a `request` element and each `param` child represents the 
 : name and original text content of a parameter.
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
    let $items := m:splitString($params, ',', true(), ())
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

(:~
 : Parses a modifier string into a data structure. The root element
 : is a `modifiers` element and each `param` child represents the
 : name and original text content of a modifier.
 : 
 : @param mod the modifier string
 : @return an element representing the modifiers
 :)
declare function m:_parseModifiers($modItems as xs:string*)
      as element() {
    let $items :=
        for $item in $modItems 
        let $item := replace($item, '^\s+|\s+$', '')        
        return
            if (not(contains($item, '='))) then
                if (starts-with($item, '~')) then 
                    <param name="{substring($item, 2)}" value="false"/>
                else 
                    <param name="{$item}" value="true"/>
            else
                let $name := replace($item, '^(.*?)\s*=.*', '$1', 's')
                let $value := replace($item, '^.*?=\s*', '', 's')
                return
                    <param name="{$name}" value="{$value}"/>
    let $modText := 
        if (empty($modItems)) then () else
            concat('<', string-join($modItems, '<'))
    return
        <modifiers text="{$modText}">{$items}</modifiers>
};

(:~
 : Replaces the operation name as it was invoked by the normalized operation name which
 : it identifies.
 : 
 : May produce the following errors:
 :    UNKNOWN_OPERATION_NAME
 :    AMBIGUOUS_OPERATION_NAME
 :
 : @param params an element representing the name/value pairs obtained by parsing the 
 :    request string
 : @param toolScheme a model of the tool interface 
 : @return an element representing the parsed and name-normalized request string
 :)
declare function m:_normalizeOperationName($params as element(), $toolScheme as element()?)
        as element() {
    if (not($toolScheme)) then $params else
    
    (: operation name :)
    let $rawName := $params/@operation
    let $candidateNames := for $n in $toolScheme//operation/@name order by lower-case($n) return $n
    let $matchingNames := tt:getMatchingNames($rawName, $candidateNames)
    let $opName := 
        if (count($matchingNames) eq 1) then $matchingNames 
        else if (count($matchingNames) eq 0) then
            tt:createError('UNKNOWN_OPERATION_NAME', 
                concat("Unknown operation name ('", $rawName, 
                    "'); valid names: ", string-join($candidateNames, ', ')), ())            
        else
            tt:createError('AMBIGUOUS_OPERATION_NAME', 
                concat("Ambiguous operation name ('", $rawName, 
                    "'); matching names: ", string-join($matchingNames, ', ')), ())            
    return
        if ($opName instance of element(z:error)) then 
            <__UNKNOWN__>{<z:errors>{$opName}</z:errors>}</__UNKNOWN__> 
        else    
            element {$opName} {$params/(@*, node())}
};

(:~
 : Normalizes parameter names.
 : 
 : Error policy: in case of errors, error diagnostics are not delivered as top level 
 : elements, but as a `z:errors` child of the response root element. The results of
 : successful evaluations are thus preserved and delivered together with any 
 : error diagnostics, making them accessible to subsequent checks. The rationale
 : is to enable as comprehensive error reports as possible.
 : 
 : May produce the following errors:
 :    UNKNOWN_PARAMEER_NAME
 :    AMBIGUOUS_PARAMETER_NAME
 :
 : @param params an element representing the name/value pairs obtained by parsing the 
 :    request string
 : @param params an element with child elements representing the name/value pairs 
 :    obtained by parsing the request string
 : @param params a structured representation of the input parameters as supplied by the request string
 : @return an element representing the parsed and name-normalized request string
 :)
declare function m:_normalizeParamNames($context as element(context)?,
                                        $params as element(), 
                                        $config as element()?)
        as element() {
    if (not($config)) then $params else    
               
    let $normParams :=
        for $param in $params/param
        let $rawName := $param/@name
        let $candidateNames := for $n in $config/param/@name order by lower-case($n) return $n
        let $matchingNames := tt:getMatchingNames($rawName, $candidateNames)
        return
            if (count($matchingNames) eq 1) then 
                <param name="{$matchingNames}" value="{$param/@value}"/>
            else if (count($matchingNames) eq 0) then
                tt:createParamNameError($context, 'UNKNOWN_PARAMETER_NAME', $rawName, $candidateNames, $matchingNames)
            else
                tt:createParamNameError($context, 'AMBIGUOUS_PARAMETER_NAME', $rawName, $candidateNames, $matchingNames)  
    let $errors := $normParams/self::z:error
    let $normalizedParams := $normParams except $errors    
    let $errorsAll := if (not($errors)) then () else <z:errors>{$errors}</z:errors>            
    return
        element {node-name($params)} {$params/@storeq, $normParams, $errorsAll}
};

(:~
 : Adds parameters set to default values to the parameters set explicitly.
 : 
 : @param params an element with child elements representing the name/value pairs 
 :    obtained by parsing the request string
 : @return an element whose child elements represent all parameters, including those set 
 :    explicitly and those set to default values
 :)
declare function m:_addDefaultValues($params as element(), 
                                     $config as element())
        as element() {
    let $paramNames := $params/param/@name
    let $defaultParams := $config/param[@default][not(@name = $paramNames)]
    return 
        if (not($defaultParams)) then $params else
            element {node-name($params)} {
                $params/@*,
                $params/*,
                for $dparam in $defaultParams return
                    <param name="{$dparam/@name}" value="{$dparam/@default}"/>
            }
};

(:~
 : Itemizes parameter values, replacing the concatenated parameter text by a
 : representation which allows access to the text of each inidividual
 : parameter value item.
 :
 : Note that the item text is made accessible, but is not yet parsed according 
 : to the item type.
 : 
 : @param params an element with child elements representing the name/value pairs 
 :    obtained by parsing the request string
 : @return intermediate representation of the request in which the supplied 
 :    parameter values are itemized
 :)
declare function m:_itemizeParamValues($params as element(), 
                                       $config as element())
        as element() {
    let $itemizedParams :=
        for $param in $params/param
        let $paramConfig := $config/param[@name eq $param/@name]
        let $maxOccurs as xs:integer := $paramConfig/@maxOccurs/xs:integer(.)
        let $multiple as xs:boolean := $maxOccurs lt 0 or $maxOccurs gt 1
        let $valueText := $param/@value/string(.)
        let $sepChar := $paramConfig/@sepChar/string()
        let $items :=
            if (not($multiple)) then $valueText
            else if ($sepChar eq '\s') then tokenize(normalize-space($valueText), '\s')
            else m:splitString($param/@value, $sepChar, true(), ())
        let $sepAtt := 
            if (count($items) lt 2) then ()
            else attribute sep {$sepChar}
        let $wsNormalizedValueText :=
            if (count($items) lt 2) then $valueText
            else if ($sepChar eq '\s') then string-join($items, ' ')
            else m:stringJoin($items, ';', ' ')
        return
            element {node-name($param)} {
                $param/@name,
                attribute paramText {$wsNormalizedValueText},
                $paramConfig/@itemType,
                $paramConfig/@cardinality[string(.)],
                $sepAtt,
                if (count($items) eq 1) then $valueText
                else if ($sepChar eq '\s') then string-join($items, ' ')
                else for $item in $items return <valueItem>{$item}</valueItem>
            }           
    return
        element {node-name($params)} {        
            $params/@*,
            $itemizedParams,
            $params/(* except param)
        }
};

(:~
 : Checks parameter cardinalities and in case of errors adds an `errors` element
 : containing for each error an `error` element. The `errors` element is inseted into
 : the copy of the $param element as an additional child element.
 : 
 : @param params an element with child elements representing the name/value pairs 
 :    obtained by parsing the request string
 : @config the operation config
 : @return a copy of $params, if no errors are detected, or a copy with an `errors`
 :    child element containing `error` elements, in case of errors
 :)
declare function m:_checkCardinalities($context as element(context)?,
                                       $params as element(), 
                                       $config as element())
        as element()* {
        
    (: check for missing parameters :)
    let $missingParamErrors :=
        for $p in $config/param[@minOccurs/xs:integer(.) gt 0]
                               [not(@name = $params/param/@name)]                                    
        return
            let $code := if ($context) then 'MISSING_MODIFIER' else 'MISSING_PARAMETER'
            let $msg := concat(
                if ($context) then concat("Parameter '", $context/@paramName, "': missing modifier '", $p/@name, "'")
                else concat("Parameter missing: '", $p/@name, "'"), 
                "; cardinality=", $p/@minOccurs, "-", $p/@maxOccurs/replace(., '-1', 'INF'))
            return
                tt:createError($code, $msg, ())
                    
    (: check for cardinality errors, parse and validate the parameter values :)                    
    let $cardinalityErrors :=   
        for $param in $params/param
        let $name := $param/@name/string(.)
        let $valueItems := m:_getUnparsedParamItems($param)        
        let $paramConfig := $config/param[@name eq $name]

        let $minOccurs := $paramConfig/@minOccurs/xs:integer(.)[. ge 0]
        let $maxOccurs := $paramConfig/@maxOccurs/xs:integer(.)[. ge 0]        
        let $type := $paramConfig/@type
        let $itemType := $paramConfig/@itemType
        return 
            if (not(count($valueItems) lt $minOccurs) and not(count($valueItems) gt $maxOccurs)) then () else
            
            let $code := if ($context) then 'INVALID_MODIFIER_CARDINALITY' else 'INVALID_PARAMETER_CARDINALITY'
            return
                tt:createParamError($context, $name, $code, 
                    concat("invalid cardinality (", count($valueItems), ")", 
                           "; valid range: ", $minOccurs, "-", $maxOccurs))
  
    let $errors := tt:wrapErrors(($params/z:errors, $missingParamErrors, $cardinalityErrors))
    return
        element {node-name($params)} {
            $params/@*,        
            $params/(* except z:errors),
            $errors
        }
};

(:~
 : Parses and validates the parameter values and writes the final XML representation 
 : of the request.
 :
 : Error policy: in case of errors, error diagnostics are not delivered as top level 
 : elements, but as a `z:errors` child of the response root element. The results of
 : successful evaluations are thus preserved and delivered together with any 
 : error diagnostics, making them accessible to subsequent checks. The rationale
 : is to enable as comprehensive error reports as possible.
 : 
 : @param context if set, $params represents the modifiers of a single parameter item,
 :    otherwise the parameters of an operation call; in the first case, $context
 :    describes the operation parameter whose modifiers are evaluated:  parameter name, 
 :    item type and item text
 : @param params an element representing a set of operation parameters or the parameter
 :    modifiers of a single operation parameter
 : @param config configuration of the parameters or modifiers
 : @return an augmented representation of the parameters or modifiers represented by $params,
 :    with type information added
 :)
declare function m:_parseParamValues($context as element(context)?,
                                     $params as element(), 
                                     $config as element())
        as element()* {
        
    (: check for cardinality errors, parse and validate the parameter values :)                    
    let $parsedParamsRaw :=   
        for $param in $params/param
        let $name := $param/@name/string(.)
        let $paramText := $param/@paramText
        let $valueItems := m:_getUnparsedParamItems($param)        
        let $paramConfig := $config/param[@name eq $name]

        let $type := $paramConfig/@type
        let $itemType := $paramConfig/@itemType
        return 
            tt:_parseParamValue($context, $name, $paramText, $valueItems, 
                $itemType, $paramConfig)
         
    let $typeErrors := $parsedParamsRaw/(self::z:error, self::z:errors)
    let $parsedParams := $parsedParamsRaw except $typeErrors
    
    let $errors := tt:wrapErrors(($params/z:errors, $typeErrors))
    return
        element {node-name($params)} {
            $params/@storeq,        
            $parsedParams,
            $errors
        }
};


(:~
 : Edits a parameter value item. Currently supported editing:
 : lc - to lowercase
 : uc - to uppercase
 :
 : @param item parameter value item
 : @param edits controls the editing
 : @return the edited value item
 :)
declare function m:_editItem($item as xs:string?, $edits as xs:string*)
        as xs:string? {
    if (not($item)) then () else
    
    let $s := $item
    let $s := if ($edits = 'lc') then lower-case($s) else $s
    let $s := if ($edits = 'uc') then upper-case($s) else $s
    return
        $s    
};

(:~
 : Checks any constraints referring to parameter groups. Supported
 : constraints: minimum and maximum number of group members
 : 
 : Returns a `z:errors` element in case of errors, the empty
 : sequence otherwise.
 :
 : @param params an element representing the name/value pairs obtained by parsing the 
 :    request string
 : @param config the operation config
 : @param toolScheme the tool scheme
 : @return intermediate representation of the request in which external parameter 
 :    values are itemized
 :)
declare function m:_checkParamGroups($request as element(), 
                                     $config as element())
        as element(z:errors)? {
    let $errors :=
    
    for $g in $config/pgroup
    let $gname := $g/@name
    let $memberNames := $g/@members/tokenize(., '\s+')
    let $actMemberNames := $request/*[local-name() = $memberNames]
    let $actOccurs := count($actMemberNames)
    let $minOccurs := $g/@minOccurs/xs:integer(.)    
    let $maxOccurs := $g/@maxOccurs/xs:integer(.)    
    return
        (: exactly one param must be set :)
        if ($minOccurs eq $maxOccurs) then
            if ($actOccurs eq $minOccurs) then () else
                tt:createError('PARAMETER_GROUP_CARDINALITY_ERROR', 
                    concat('Exactly ', $minOccurs, ' of these parameters must be set: ',
                        string-join($memberNames, ', '), '.'), ())           
        else (       
            if (not($g/@minOccurs)) then () else
                if ($actOccurs ge $minOccurs) then () else
                    tt:createError('PARAMETER_GROUP_MIN_OCCURS_ERROR',
                        concat('At least ', $minOccurs, ' of these parameters must be set: ',
                            string-join($memberNames, ', '), '.'), ()),
            if (not($g/@maxOccurs)) then () else
                if ($actOccurs le $maxOccurs) then () else
                    tt:createError('PARAMETER_GROUP_MAX_OCCURS_ERROR',
                        concat('At most ', $maxOccurs, ' of these parameters must be set: ', 
                            string-join($memberNames, ', '), '.'), ())
        )        
        
    return
        tt:wrapErrors($errors)
};

(:~
 : Augments an operation config:
 : - each parameter group element is augmented by a @members attribute containing the sorted
 :   list of member parameter names
 : - each parameter element is augmented by four additional attributes:
 :    @itemType - the result of removing the cardinality postfix from the type specification
 :    @cardinality - the cardinality postfix of the type specification
 :    @minOccurs - minimum number of occurrences
 :    @maxOccurs - maximum number of occurrences
 :
 : Error policy: in case of errors, error diagnostics (error elements) are not delivered 
 : as top level elements, but as a z:errors child of the response root element. This way, the
 : results of successful parameter evaluations are preserved and delivered together
 : with any error diagnostics, making them accessible to subsequent checks. The rationale
 : is to enable as comprehensive error reports as possible.
 :
 : Note. This function is used to augment both, the configuration of an operation,
 : and the configuration of a set of modifiers.
 : 
 : @param operationName the operation name
 : @param serviceModel a definition of all service operations 
  : @return an element describing the operation
 :)
declare function m:_augmentOperationConfig($config as element())
        as element() {
    let $pgroupElems := $config/pgroup
    let $paramElems := $config/(* except pgroup)
    
    (: $pgroups - elements representing the parameter groups :)
    let $pgroups :=
        for $g in $pgroupElems
        let $gname := $g/@name
        let $members := $paramElems[@pgroup eq $gname]
        let $memberNames :=
            string-join(
                for $member in $members order by $member/@name/lower-case(.) return $member/@name
            , ' ')
        return
            element {node-name($g)} {$g/@*, attribute members {$memberNames}}
    
    (: $params - elements representing the parameters :)
    let $paramsAndErrors :=
        for $p in $paramElems
        let $typeSpec := m:_parseTypeSpec($p/@type)
        let $sepChar := 
            if ($typeSpec/@maxOccurs eq '1') then ()
            else m:_getItemSep($p/@sep, $typeSpec/@itemType)
        return
            if ($typeSpec/self::z:error) then $typeSpec else
            element {node-name($p)} {
                $p/@*,
                if (not($sepChar)) then () else attribute sepChar {$sepChar},
                $typeSpec/(@typeSpec, @cardinality, @itemType, @minOccurs, @maxOccurs)
            }
    let $errors := $paramsAndErrors/self::z:error
    let $params := $paramsAndErrors except $errors    
    let $allErrors := if (not($errors)) then () else <z:errors>{$errors}</z:errors>
    return
        element {node-name($config)} {$config/@*, $params, $pgroups, $allErrors}
};    

(:~
 : Parses a type specification and returns the result as an element
 : with attributes containing various parts of the parsing result:
 :
 : @typeSpec - the original type spec string
 : @cardinality - the cardinality constraint string (e.g. '?' or '{0,7}' 
 : @itemType - the item type name
 : @minOccurs - the minimum number of occurrences
 : @maxOccurs - the maximum number of occurrences
 :
 : In case of an error, a single `z:error` element is returned, rather
 : than a `type` element.
 :
 : Possible errors:
 : INVALID_CARDINALITY_CONSTRAINT - if the constraint is syntactically incorrect
 :
 : @param typeSpec the type specification (e.g. 'xs:string+)
 : @return an element with attributes delivering parsing results, or
 :    an <z:error> element in case of a syntax error
 :)
declare function m:_parseTypeSpec($typeSpec as xs:string)
        as element() {
    let $tspec := replace($typeSpec, '\s', '')
    let $itemType := replace($tspec, '^(\i\c*)(\((.*)\))?(.*)$', '$1')
    let $cardinality := replace($tspec, '^(\i\c*)(\((.*)\))?(.*)$', '$4') 

    let $minMax :=
        if (not($cardinality)) then (1, 1)
        else if ($cardinality eq '?') then (0, 1)
        else if ($cardinality eq '*') then (0, -1)            
        else if ($cardinality eq '+') then (1, -1)
        else
            let $range := replace($cardinality, '^\{(\d+,\d+|\d+,|,\d+|\d+)\}$', '$1', 's')
            return
                if ($range eq $cardinality) then (: syntax error :)
                    <z:error code="PARAMETER_CARDINALITY_ERROR" typeSpec="{$tspec}"
                         msg="{concat('Parameter cardinality error: ', $cardinality, '; typeSpec=', $tspec)}"/>
                else
                    if (not(contains($range, ','))) then (xs:integer($range), xs:integer($range))
                    else
                        let $parts := tokenize($range, '\s*,\s*')
                        return
                            if (not($parts[1])) then (-1, xs:integer($parts[2]))
                            else if (not($parts[2])) then (xs:integer($parts[1]), '-1')
                            else (xs:integer($parts[1]), xs:integer($parts[2]))
    return
        if ($minMax instance of element(z:error)) then $minMax else
        
        <type typeSpec="{$tspec}" 
              cardinality="{$cardinality}" 
              itemType="{$itemType}" 
              minOccurs="{$minMax[1]}" 
              maxOccurs="{$minMax[2]}"/>
};

(:~
 : Transforms a modifier scheme into the format used by an operation schema.
 :
 : @param scheme the modifier scheme
 : @return a normalized version of the scheme in which 'modifier' elements are renamed to
 :    'param' elements
 :)
declare function m:_normalizeModifierScheme($scheme as element())
        as element()? {
    element {node-name($scheme)} {        
        for $c in $scheme/*
        return
            if ($c/self::modifier) then <param>{$c/(@*, node())}</param>
            else $c
    }            
};

(:~
 : Returns the separator character to be used as item separator when
 : splitting a parameter value into items. If the separator is a
 : whitespace character, it is represented by the string '\s'.
 :
 : @param sep the item separator as explicitly configured
 : @param itemType the item type
 : @return the separator character or regex character class
 :)  
declare function m:_getItemSep($sep as xs:string?, $itemType as xs:string?)
        as xs:string {
    if (not($sep)) then '\s'
    else if ($sep eq 'WS') then '\s'
    else if ($sep eq 'SC') then ';'
    else ';'
};        

(:~
 : Retrieves the parameter modifier scheme for a given parameter type.
 :
 : @param typeName the type name
 : @return the modifier scheme for this type, or the empty sequence if
 :    there is no scheme
 :)
declare function m:_getParamModifierScheme($typeName as xs:string)
        as element()? {
    $tt:PARAM_MODIFIER_SCHEMES/modifiers[tokenize(@paramTypes, '\s+') = $typeName][last()]        
};

