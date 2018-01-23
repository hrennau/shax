(:
 : -------------------------------------------------------------------------
 :
 : namespaceTools.xqm - functions handling namespaces
 :
 : -------------------------------------------------------------------------
 :)

module namespace f="http://www.ttools.org/shax/ns/xquery-functions";
import module namespace tt="http://www.ttools.org/xquery-functions" at 
    "tt/_request.xqm",
    "tt/_reportAssistent.xqm",
    "tt/_errorAssistent.xqm",
    "tt/_log.xqm",
    "tt/_nameFilter.xqm",
    "tt/_pcollection.xqm";    

import module namespace i="http://www.ttools.org/shax/ns/xquery-functions" at
    "constants.xqm";

declare namespace shax="http://shax.org/ns/model";
declare namespace z="http://www.ttools.org/shax/ns/structure";

(:~
 : Creates namespace nodes capturing the in-scope-namespaces of
 : a given element.
 :
 : @param elem the element
 : @return namespace nodes
 :)
declare function f:copyNamespaces($elem as element())
        as node()* {
    for $prefix in in-scope-prefixes($elem) return
        namespace {$prefix} {namespace-uri-for-prefix($prefix, $elem)}
};

(:~
 : Returns all namespaces which must be imported by a given schema.
 :
 : @param xsd a schema element to be analyzed
 : @return the namespaces which must be imported
 :)
declare function f:schemaRequiredNamespaces($xsd as element(xs:schema))
        as xs:string* {
    distinct-values((                    
        $xsd//(@base, @type, @ref, @itemType)/resolve-QName(., ..),
        for $mtypes in $xsd//@memberTypes return
            tokenize(normalize-space($mtypes), ' ') ! resolve-QName(., $mtypes/..)
         ) ! namespace-uri-from-QName(.))
    [not(. = ($xsd/@targetNamespace, $i:URI_XSD))]
};

(:~
 : Returns a copy of one or more SHAX models in which namespace bindings are
 : normalized.
 :
 : @param elems shax:model and/or shax:models elements
 : @param nsmap namespace map specifying normalized namespace bindings
 : @return copies of the SHAX models with normalized namespace bindings 
 :)
declare function f:normalizeShaxModelNamespaceBindings($elems as element()+, $nsmap as element(z:nsMap))
        as element()+ {
    for $elem in $elems return 
        f:normalizeShaxModelNamespaceBindingsRC($elem, $nsmap)
};

declare function f:normalizeShaxModelNamespaceBindingsRC($n as node(), $nsmap as element(z:nsMap))
        as node() {
    typeswitch($n)
    case document-node() return
        document {for $c in $n/node() return f:normalizeShaxModelNamespaceBindingsRC($c, $nsmap) }
        
    case element() return
        let $qname :=
            let $nsu := namespace-uri($n)
            return
                if (not($nsu)) then local-name($n)
                else 
                    let $prefix := $nsmap/z:ns[@uri eq $nsu]/@prefix
                    let $name := string-join(($prefix, local-name($n)), ':')
                    return QName($nsu, $name)
        return
            element {$qname} {
                (: root element receives all namespace bindings :)
                if ($n/parent::*) then () else
                    for $ns in $nsmap/z:ns return namespace {$ns/@prefix} {$ns/@uri},
                for $a in $n/@* return f:normalizeShaxModelNamespaceBindingsRC($a, $nsmap),
                for $c in $n/node() return f:normalizeShaxModelNamespaceBindingsRC($c, $nsmap)                
            }
                
    case attribute(name) | attribute(base) | attribute(type) | attribute(itemType) | attribute(itemNode)
    return
        let $qname := 
            let $value := resolve-QName($n, $n/..)
            let $nsu := namespace-uri-from-QName($value)
            let $lname := local-name-from-QName($value)
            let $prefix := $nsmap/z:ns[@uri eq $nsu]/@prefix
            let $name := string-join(($prefix, $lname), ':')
            return QName($nsu, $name)
        return
            attribute {node-name($n)} {$qname}
        
    case attribute(memberTypes) return
        let $qnames :=
            for $value in tokenize(normalize-space($n), ' ') ! resolve-QName(., $n/..)
            let $nsu := namespace-uri-from-QName($value)
            let $lname := local-name-from-QName($value)
            let $prefix := $nsmap/z:ns[@uri eq $nsu]/@prefix
            let $name := string-join(($prefix, $lname), ':')
            return QName($nsu, $name)
        return  attribute {node-name($n)} {$qnames} 
        
    default return $n        
};        

(:~
 : Creates a namespace map representing normalized prefix bindings
 : for all namespaces used by a sequence of SHAX models.
 : <p/>
 : The map contains additional entries:
 : prefix shax - shax model namespace
 : prefix z - shax structure namespace
 :
 : @schemas the schemas to be evaluated
 : @return a map containing prefix/uri pairs
 :)
declare function f:getShaxModelNamespaceMap($elems as element()+)
      as element(z:nsMap) {
   let $namespaceUris := f:getShaxModelNamespaces($elems)
   let $nsElems := f:getNormalizedNamespaceBindings($namespaceUris) 
   return
      <z:nsMap>{
         $nsElems,
         <z:ns prefix="xml" uri="http://www.w3.org/XML/1998/namespace"/>,
         <z:ns prefix="xs" uri="http://www.w3.org/2001/XMLSchema"/>,
         <z:ns prefix="z" uri="http://www.shax.org/ns/structure"/>,
         <z:ns prefix="shax" uri="http://www.shax.org/ns/model"/>
      }</z:nsMap>
};

(:~
 : Returns the namespace URIs used in one or more shax:model or shax:models
 : elements.
 :
 : @param elems a sequence of shax:model or shax:models elements
 : @return a sorted list of all namespace URIs used in these elements
 :)
declare function f:getShaxModelNamespaces($elems as element()+)
        as xs:string* {
    let $nss_properties := $elems//shax:objectType//(* except shax:*)/namespace-uri(.) => distinct-values()
    let $nss_atts := $elems//(@name, @base, @type, @itemType, @itemNode)/resolve-QName(., ..) ! namespace-uri-from-QName(.)
    let $nss_memberTypes :=
        for $mtypes in $elems//@memberTypes
        let $types := tokenize(normalize-space($mtypes), ' ')
        return $types ! resolve-QName(., $mtypes/..) ! namespace-uri-from-QName(.)
    return
        distinct-values(($nss_properties, $nss_atts, $nss_memberTypes))[not(. eq $f:URI_XSD)] => sort()
};        

(:~
 : Returns for a sequence of namespace URIs normalized
 : namespace bindings. Each binding is represented by
 : a <z:ns> element with attributes @prefix and @uri
 : providing the prefix and the namespace URI,
 : repectively. 
 :
 : Normalized prefixes are the lower case letters 
 : corresponding to the position of the namespace URI 
 : within the list of namespace URIs. If the position 
 : is gt 25, the letters are reused and a suffix is 
 : appended which indicates the number of the current 
 : letter cycle (2, 3, ...). The prefixses therefore 
 : are: 'a', 'b', 'c', ..., 'x', 'y', 'a2', 'b2', .....
 :
 : @namespaceUris the namespace URIs
 : @return a sequence of <z:ns> elements describing the 
 :    normalized prefix bindings
 :)
declare function f:getNormalizedNamespaceBindings($namespaceUris as xs:string*) 
      as element(z:ns)* {
   for $nsu at $pos in $namespaceUris
   let $seriesNr := ($pos - 1) idiv 25
   let $postfix := if (not($seriesNr)) then () else $seriesNr + 1
   let $p := 1 + ($pos - 1) mod 25
   let $char := substring('abcdefghijklmnopqrstuvwxy', $p, 1)
   let $prefix := concat($char, $postfix)
   where not($nsu eq 'http://www.w3.org/XML/1998/namespace')
   return
       <z:ns prefix="{$prefix}" uri="{$nsu}"/>
};
