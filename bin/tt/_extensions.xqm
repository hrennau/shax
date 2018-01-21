(:~ 
 : _extensions.xqm - generated functions invoking application specific extensions.
 :
 : @version 20140402-1 first version 
 : ===================================================================================
 :)

module namespace m="http://www.ttools.org/xquery-functions";


declare namespace z="http://www.ttools.org/structure";

declare variable $m:NON_STANDARD_TYPES := '';

(:~
 : Parses a request string into a data type item. The function delegates the
 : parsing to the appropriate function identified by pseudo annotations.
 : 
 : @param paramName the parameter name
 : @param itemType the item type
 : @param itemText a string providing a single parameter item
 : @return the parsed item, or an z:errors element
 :)
declare function m:parseNonStandardItemType($paramName as xs:string, $itemType as xs:string, $itemText as xs:string)       
        as item()+ {       

    <z:error type="UNKNOWN_ITEMTYPE" paramName="{$paramName}" itemType="{$itemType}" 
        itemValue="{$itemText}"                       
        msg="{concat('Parameter ''', $paramName, ''' has unknown item type: ', $itemType)}"/>
};

(:~
 : Non-standard types resulting in atomic item types require a mapping of the
 : non-standard item type name to the atomic item type name in order to enable
 : correct delivery from the param element. The atomic item type name is
 : retrieved from the @itemType attribute on the type annotations' 
 : <type> element.
 :
 : @param itemType the item type name as communicated to the user
 : @return the item type of delivered value items
 :)
declare function m:adaptItemTypeOfNonStandardItemType($itemType as xs:string)
        as xs:string {
    $itemType        
};

declare function m:checkNonStandardFacets($itemText as xs:string, $typedItem as item()+, $paramConfig as element()?)
        as element()* {
    let $itemType := $paramConfig/@itemType
    let $name := $paramConfig/@name
    let $errors := (
    
        ()
    )
    return
        $errors           
};
