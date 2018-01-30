(:
 : -------------------------------------------------------------------------
 :
 : componentManager.xqm - Document me!
 :
 : -------------------------------------------------------------------------
 :)


module namespace f="http://www.ttools.org/shax/ns/xquery-functions";

import module namespace tt="http://www.ttools.org/xquery-functions" at 
    "tt/_constants.xqm",
    "tt/_namespaceTools.xqm";    

import module namespace app="http://www.ttools.org/shax/ns/xquery-functions" at
    "constants.xqm";

(:
    "componentFinder.xqm",
    "componentLocator.xqm",    
    "componentNavigator.xqm",
    "constants.xqm",
    "targetNamespaceTools.xqm",
    "utilities.xqm";    
:)
declare namespace z="http://www.ttools.org/shax/ns/xquery-functions/structure";
declare namespace zz="http://www.ttools.org/structure";

(:
 : ============================================================================
 :
 :     o p e r a t i o n s
 :
 : ============================================================================
 :)
 
 (:
 : ============================================================================
 :
 :     p u b l i c    f u n c t i o n s
 :
 : ============================================================================
 :)

(: ###################################################################################################################
   #                                                                                                                 #
   #   section:    g e t    n a m e    o r    u r i                                                                  #
   #                                                                                                                 #   
   ###################################################################################################################
:)   

(:~
 : Returns the name of a schema component. 
 :
 : Note. These are the rules for determining the component name.
 : If a 'ref' attribute is present, the name is obtained by resolving 
 : the attribute value as QName. Otherwise a 'name' attribute 
 : must be present, and the component name is determined as follows:
 : (a) the local name is equal to the value of the 'name' attribute
 : (b) the namespace URI is the target namespace if one of the
 :     following conditions is met:
 :     (b1) the component is neither xs:element nor xs:attribute
 :     (b2) the component is top-level (child element of xs:schema)
 :     (b3) the component is xs:element and 'elementForm' is 'qualified'
 :     (b4) the component is xs:attribute and 'attributeForm' is 'qualified'
 : (c) otherwise (none of the conditions in (b) applies) - the 
 :     namespace URI is null.
 :
 : @param comp the schema component
 : @return the component name as a QName
 :)
declare function f:getComponentName($comp as element())                        
        as xs:QName {   
    if ($comp/@ref) then resolve-QName($comp/@ref, $comp)
    else if ($comp/self::xs:element) then f:getElemComponentName($comp)
    else if ($comp/self::xs:attribute) then f:getAttComponentName($comp)
    else if ($comp/@name) then QName($comp/ancestor::xs:schema/@targetNamespace, $comp/@name)
    else if ($comp/self::xs:complexType or $comp/self::xs:simpleType) then f:getTypeComponentName($comp)   
    else
        tt:createError('INVALID_XSD_CONTENT',  
            concat("Schema comp without @name and @ref; ",
                   "type=", local-name($comp), "; ancs=", string-join($comp/ancestor-or-self::*/name(), ', ')),
            ())
};

(:~
 : Returns the normalized name of a schema component. See `getComponentName` 
 : for the rules how a component name is determined. See `normalizeQName` 
 : for details about QName normalization.
 :
 : @param comp the schema component
 : @param nsmap normalized bindings of namespace URIs to prefixes
 : @return the component name as a QName
 :)
declare function f:getNormalizedComponentName($comp as element(), 
                                              $nsmap as element())
        as xs:QName {
    f:getComponentName($comp) ! tt:normalizeQName(., $nsmap)        
};

(:~
 : Returns the name of an element declaration. It is the qualified name 
 : of element instances governed by the element declaration.
 :
 : Note: if the element declaration is not global, it depends on 
 : $comp/@elementForm or $schema/@elementFormDefault if the name 
 : is in the target namespace.
 :
 : @param comp the element declaration
 : @return the component name
 :)
declare function f:getElemComponentName($comp as element(xs:element))
        as xs:QName {
    if ($comp/@ref) then resolve-QName($comp/@ref, $comp)
    else
        let $uri := 
            if ($comp/parent::xs:schema or 'qualified' eq
                ($comp/@elementForm, $comp/ancestor::xs:schema/@elementFormDefault)[1])
            then $comp/ancestor::xs:schema/@targetNamespace
            else ()
        return QName($uri, $comp/@name)
};

(:~
 : Returns the component name of an attribute declaration. It is the 
 : qualified name of attribute instances governed by a the attribute 
 : declaration.
 :
 : Note: if the attribute declaration is not global, it depends on 
 : $comp/@attributeForm or $schema/@attributeFormDefault if the name 
 : is in the target namespace.
 :
 : @param comp the attribute declaration
 : @param schemas the schemas
 : @return the element name in instance documents
 :)
declare function f:getAttComponentName($comp as element(xs:attribute))
        as xs:QName {
    if ($comp/@ref) then resolve-QName($comp/@ref, $comp)
    else
        let $uri := 
            if ($comp/parent::xs:schema or 'qualified' eq
                ($comp/@attributeForm, $comp/ancestor::xs:schema/@attributeFormDefault)[1])
            then $comp/ancestor::xs:schema/@targetNamespace
         else ()
        return QName($uri, $comp/@name)
};

(:~
 : Returns the component name of a type definition. If the type is local, 
 : the returned QName has no namespace and a local name '_LOCAL_'.
 :
 : @param comp the element declaration
 : @return the component name
 :)
declare function f:getTypeComponentName($comp as element()+)
        as xs:QName {
    if (not($comp/@name)) then QName($app:URI_STRUCTURE, 'z:_LOCAL_')
    else QName($comp/ancestor::xs:schema/@targetNamespace, $comp/@name)
};

(:~
 : Returns the qualified name of the type of an element declaration or
 : an attribute declaration. If the declaration has a local type, the
 : name z:_LOCAL_ is returned; if the declaration has no type, the
 : empty sequence is returned
 :
 : @param comp a schema component, should be either an element declaration
 :     or an attribute declaration
 : @return the type name
 :) 
declare function f:getComponentTypeName($comp as element())
        as xs:QName {        
    let $name := $comp/@type/resolve-QName(., ..)
    return
        if ($name) then $name
        else if ($comp/(xs:simpleType, xs:complexType)) then 
            QName($app:URI_STRUCTURE, 'z:_LOCAL_')
        else ()
};        

(:~
 : Returns the normalized qualified name of the type of an element 
 : declaration or an attribute declaration. If the declaration has 
 : a local type, the name z:_LOCAL_ is returned; if the declaration 
 : has no type, the empty sequence is returned
 :
 : @param comp a schema component, should be either an element declaration
 :     or an attribute declaration
 : @param nsmap normalized bindings of namespace URIs to prefixes 
 : @return the type name
 :) 
declare function f:getNormalizedComponentTypeName(
                            $comp as element(), 
                            $nsmap as element())
        as xs:QName? {        
    let $name := $comp/@type/resolve-QName(., ..) ! app:normalizeQName(., $nsmap)
    return
        if (exists($name)) then $name
        else if ($comp/(xs:simpleType, xs:complexType)) then 
            QName($app:URI_STRUCTURE, 'z:_LOCAL_')
        else ()
};       

(:~
 : Returns the document URI of a schema component.
 :
 : @param comp the schema component
 : @return the document URI
 :)
declare function f:getDocumentUri($comp as element())
        as xs:string {
    $comp/root()/(document-uri(.), descendant-or-self::*[1]/base-uri(.))[1]        
};

(:~
 : Returns the final step of the document URI of a schema component.
 :
 : @param comp the schema component
 : @return the document URI
 :)
declare function f:getDocumentName($comp as element())
        as xs:string {
    replace(f:getDocumentUri($comp), '.*[\\/]', '')        
};

(:~
 : Returns for an XSD component a succinct occurrence descriptor 
 : as used in @z:occ (?, *, +, {i-j}).
 :
 : @params comp an XSD component (element declaration, group 
 :     reference or model group)
 : @return a succinct occurrence descriptor
 :)
declare function f:cardinalityDescForXsdComp($comp as element())
        as xs:string {
    let $minOccurs := ($comp/@minOccurs/xs:integer(.), 1)[1]        
    let $maxOccurs := ($comp/@maxOccurs/
        (if (. eq 'unbounded') then -1 else xs:integer(.)), 1)[1]    
    return
        f:cardinalityDescFromCardinalityRange($minOccurs, $maxOccurs)      
};        

(: ###################################################################################################################
   #                                                                                                                 #
   #   section:    e d i t    c o m p o n e n t                                                                      #
   #                                                                                                                 #   
   ###################################################################################################################
:)   

(:~
 : Returns an edited version of a schema component.
 :
 : @param skipAnno if true, annotations are removed
 : @param addUri if true, the document URI is added as an attribute 'uri'
 : @return the edited component
 :) 
declare function f:editComponent($comp as element(), $request as element())
        as element() {
    let $skipAnno := tt:getParams($request, 'skipAnno')
    return f:editComponent($comp, $skipAnno, (), ())        
};

(:~
 : Returns an edited version of a schema component.
 :
 : @param skipAnno if true, annotations are removed
 : @param addUri if true, the document URI is added as an attribute 'uri'
 : @return the edited component
 :) 
declare function f:editComponent($comp as element(), $skipAnno as xs:boolean?, $addUri as xs:boolean?, $addFname as xs:boolean?)
        as element() {
    let $raw := f:_editComponentRC($comp, $skipAnno)
    return
        if (not($addUri) and not($addFname)) then $raw else
            element {node-name($raw)} {
                if (not($addFname)) then () else attribute z:xsd {app:getDocumentName($comp)},
                if (not($addUri)) then () else attribute z:uri {f:getDocumentUri($comp)},                    
            
                $raw/@*,
                $raw/node()
            }
};        

 (:
 : ============================================================================
 :
 :     p r i v a t e    f u n c t i o n s
 :
 : ============================================================================
 :)

(:~
 : Recursive helper function of 'editComponent'.
 :)
declare function f:_editComponentRC($n as node(), $skipAnno as xs:boolean?)
        as node()? {
    typeswitch($n)
    case document-node() return 
        document {for $c in $n/node() return f:_editComponentRC($c, $skipAnno)}
    case element(xs:annotation) return
        if ($skipAnno) then () else f:_editComponentRC_copy($n, $skipAnno)    
    case element() return f:_editComponentRC_copy($n, $skipAnno)
    case comment() return
        if ($skipAnno) then () else $n    
    default return $n        
};        

(:~
 : Helper function of '_editComponentRC'.
 :)
declare function f:_editComponentRC_copy($n as element(), $skipAnno as xs:boolean?)
        as node() {
    element {node-name($n)} {    
        for $a in $n/@* return f:_editComponentRC($a, $skipAnno),
        for $c in $n/node() return f:_editComponentRC($c, $skipAnno)
    }       
};        






