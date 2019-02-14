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
declare namespace zz="http://www.ttools.org/structure";

(:~
 : Returns copies of the namespace nodes of an element.
 :
 : @param elem the element whose namespace nodes shall be copied
 : @return copies of the namespace nodes of $elem
 :)
declare function f:namespaceNodes($elem as element()) 
        as namespace-node()+ {
    for $prefix in in-scope-prefixes($elem) return
        namespace {$prefix} {namespace-uri-for-prefix($prefix, $elem)}
};

(:~
 : Normalizes a QName according to a supplied binding of namespace prefixes.
 :
 : @param qname the QName to be normalized
 : @param nsmap a map representing the binding of namespace prefixes
 : @return the normalized QName
 :)
declare function f:normalizeQName(
                        $qname as xs:QName, 
                        $nsmap as element(zz:nsMap)?) 
        as xs:QName {
        
   if (empty($nsmap)) then $qname
   else
      let $uri := namespace-uri-from-QName($qname)[string()]
                    (: 20150327 - if no namespace, the URI must be empty sequence :)
      return
         if (empty($uri)) then $qname
         else
            let $prefix := $nsmap/zz:ns[@uri eq $uri]/@prefix
            return
               if (empty($prefix)) then $qname else
                  let $lexName := string-join(($prefix, local-name-from-QName($qname)), ':')
                  return QName($uri, $lexName)
};

(:~
 : Normalizes a QName according to a supplied binding of namespace prefixes.
 :
 : @param qname the QName to be normalized
 : @param nsmap a map representing the binding of namespace prefixes
 : @return the normalized QName
 :)
declare function f:normalizeQNameNONS(
                        $qname as xs:QName, 
                        $nsmap as element(zz:nsMap)?) 
        as xs:QName {
        
   if (empty($nsmap)) then $qname
   else
      let $uri := namespace-uri-from-QName($qname)[string()]
      return
         if (empty($uri)) then 
            QName($i:URI_NONS_DEFAULT, concat('nons:', local-name-from-QName($qname)))
         else
            let $prefix := $nsmap/zz:ns[@uri eq $uri]/@prefix return
                if (empty($prefix)) then $qname else
                    let $lexName := string-join(($prefix, local-name-from-QName($qname)), ':')
                    return QName($uri, $lexName)
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
declare function f:normalizeShaxModelNamespaceBindings($elems as element()+, $nsmap as element(zz:nsMap))
        as element()+ {
    for $elem in $elems return 
        f:normalizeShaxModelNamespaceBindingsRC($elem, $nsmap)
};

declare function f:normalizeShaxModelNamespaceBindingsRC($n as node(), $nsmap as element(zz:nsMap))
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
                    let $prefix := $nsmap/zz:ns[@uri eq $nsu]/@prefix
                    let $name := string-join(($prefix, local-name($n)), ':')
                    return QName($nsu, $name)
        return
            element {$qname} {
                (: root element receives all namespace bindings :)
                if ($n/parent::*) then () else
                    for $ns in $nsmap/zz:ns return namespace {$ns/@prefix} {$ns/@uri},
                for $a in $n/@* return f:normalizeShaxModelNamespaceBindingsRC($a, $nsmap),
                for $c in $n/node() return f:normalizeShaxModelNamespaceBindingsRC($c, $nsmap)                
            }
                
    case attribute(name) | attribute(base) | attribute(type) | attribute(itemType) | attribute(itemNode)
    return
        let $qname := 
            let $value := resolve-QName($n, $n/..)
            let $nsu := namespace-uri-from-QName($value)
            let $lname := local-name-from-QName($value)
            let $prefix := $nsmap/zz:ns[@uri eq $nsu]/@prefix
            let $name := string-join(($prefix, $lname), ':')
            return QName($nsu, $name)
        return
            attribute {node-name($n)} {$qname}
        
    case attribute(memberTypes) return
        let $qnames :=
            for $value in tokenize(normalize-space($n), ' ') ! resolve-QName(., $n/..)
            let $nsu := namespace-uri-from-QName($value)
            let $lname := local-name-from-QName($value)
            let $prefix := $nsmap/zz:ns[@uri eq $nsu]/@prefix
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
      as element(zz:nsMap) {
   let $namespaceUris := f:getShaxModelNamespaces($elems)
   let $nsElems := f:getNormalizedNamespaceBindings($namespaceUris) 
   return
      <zz:nsMap>{
         $nsElems,
         <zz:ns prefix="xml" uri="http://www.w3.org/XML/1998/namespace"/>,
         <zz:ns prefix="xs" uri="http://www.w3.org/2001/XMLSchema"/>,
         <zz:ns prefix="zz" uri="http://www.ttools.org/structure"/>,
         <zz:ns prefix="shax" uri="http://www.shax.org/ns/model"/>
      }</zz:nsMap>
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
 : a <zz:ns> element with attributes @prefix and @uri
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
 : @return a sequence of <zz:ns> elements describing the 
 :    normalized prefix bindings
 :)
declare function f:getNormalizedNamespaceBindings($namespaceUris as xs:string*) 
      as element(zz:ns)* {
   for $nsu at $pos in $namespaceUris
   let $seriesNr := ($pos - 1) idiv 25
   let $postfix := if (not($seriesNr)) then () else $seriesNr + 1
   let $p := 1 + ($pos - 1) mod 25
   let $char := substring('abcdefghijklmnopqrstuvwxy', $p, 1)
   let $prefix := concat($char, $postfix)
   where not($nsu eq 'http://www.w3.org/XML/1998/namespace')
   return
       <zz:ns prefix="{$prefix}" uri="{$nsu}"/>
};

(:~
 : Finds a prefix for a given namespace in the context of a given document.
 : The prefix returned has not yet been used in the document.
 :
 : @param n a node from the document in whose context the prefix is sought
 : @param uri namespace URI for which a prefix is sought
 : @param prefixProposal a prefix proposed by the caller, to be used if not in conflict
 : @recursiveAttempt is set, indicates the recursion level above the initial call
 :
 : @version 20100105 
 :)
declare function f:findPrefix($n as node(),
                              $uri as xs:string,
		                      $prefixProposal as xs:string?,
	                          $recursiveAttempt as xs:integer?) 
                 as xs:string {
   let $currentPrefixes := distinct-values($n/root()/descendant-or-self::*/in-scope-prefixes(.))
   return
      if ($prefixProposal and not($prefixProposal = $currentPrefixes))
      then $prefixProposal
      else
         let $nextRecursiveAttempt := ($recursiveAttempt + 1, 1)[1]
         return
            f:findPrefix($n, $uri,
                          concat("ns", $nextRecursiveAttempt),
                          $nextRecursiveAttempt)
};

(:~
 : Changes the target namespace of a schema to a new value. If the
 : new value is the empty string, the target namespace is removed.
 : Note that 'import' elements are transformed into 'include', if the
 : imported namespace matches the new namespace.
 :
 : @param n a node of the schema to be transformed
 : @param $uri the new target namespace
 : @param $prefix the prefix to be used for the new target namespace
 : @return the transformed schema node
 :
 : @version 20121202-2
 :)
declare function f:changeTns($n as node(), 
                             $uri as xs:string, 
                             $prefix as xs:string?) 
as node() 
{
   typeswitch ($n)
   case document-node() return f:changeTns($n/*, $uri, $prefix)

   case $e as element(xs:schema) return      
      let $usePrefix := f:findPrefix($e, $uri, $prefix, ())
      let $useNsmap := <zz:nsMap><zz:ns prefix="{$usePrefix}" uri="{$uri}"/></zz:nsMap>
      
      let $changed :=
         <xs:schema>{
	        if ($uri) then attribute targetNamespace {$uri} else (),
            attribute xml:base {base-uri($n)},
            attribute z:isChameleon {true()},
	        for $a in $n/(@* except (@targetNamespace, @xml:base), node()) 
            return 
               f:changeTns($a, $uri, $usePrefix)
	     }</xs:schema>
      let $copyNs := f:copyNSB($n, $changed)
      let $addTns := f:addNSBs($copyNs, $useNsmap)
      return
         $addTns
         
   (: do not change xs:include :)
   case element(xs:include) return $n

   (: transform xs:import into xs:include, if ncessary :)
   case element(xs:import) return
      if (not($n/@namespace eq $uri)) then $n 
      else
         <xs:include>{
	   $n/((@* except @namespace), node())
         }</xs:include>

   case $e as element() return
      element {node-name($e)} {
         for $ac in $e/(@*, node()) return f:changeTns($ac, $uri, $prefix)
      }

   case $a as attribute() return
   (: 
    : if the attribute has no namespace sensitive value, it is returned as is;
    : else any qname referencing the changed tns is adapted by replacing the
    : prefix by the prefix to be used for the new namespace. 
    :)
      if (namespace-uri-from-QName(node-name($a))) then $a else

      let $prevTns := string($a/ancestor::xs:schema/@targetNamespace) 
      let $newPrefixPart := if ($prefix) then concat($prefix, ":") else ""
      return
         attribute {node-name($a)} {
            if ($a/local-name() = ("type", "ref", "base", "itemType")) 
            then
	       if (namespace-uri-from-QName(resolve-QName($a, $a/..)) eq $prevTns)
               then string-join(($newPrefixPart, replace($a, ".*:", "")), "")
               else $a
            else if ($a/local-name() eq "memberTypes") then
                string-join(
	           for $name in tokenize($a, "\s+")
                   return
                      if (namespace-uri-from-QName(resolve-QName($name, $a/..)) eq $prevTns)
                      then string-join(($newPrefixPart, replace($name, ".*:", "")), "")
                      else $name
                , ' ')
            else $a
      }
   default return $n
};

(:~
 : Copy namespace bindings from the source node (element or document) to the
 : target node (element or document.e. 
 :
 : @param source the source node
 : @param target the target node
 :)
declare function f:copyNSB($source as node()*, 
                           $target as node())
   as node()? 
{
   let $bindings := f:namespaceBindings($source, false())/*
   return
       element {node-name($target)} {
          for $b in $bindings return
              namespace {$b/@p} {$b/@uri},
          $target/@*,
          $target/node()
       }
       
};

(:~
 : Adds an in-scope namespace to an element, if it does not already have it.
 : In the latter case, the element is returned unchanged. Special case: 
 : if the namespace URI of the in-scope namespace is empty, the element 
 : is returned with the default namespace removed, in order to allow 
 : referencing namespace-less elements from within the element. 
 :
 : Note: this function requires that copy namespaces modes contains 
 : 'inherited'.
 :
 : @param e the element to be modified
 : @param uri the namespace URI
 : @param prefix the prefix to be used
 :
 : @version: 20121202-1
 :)
declare function f:addNSB($e as element()?,
                          $uri as xs:string,
                          $prefix as xs:string) 
                 as element()? {
   if ($uri eq "") then 
      <_tmp xmlns="">{$e}</_tmp>/*   (: remove default namespace :)
   else if ($uri eq namespace-uri-for-prefix($prefix, $e)) then      
      $e                             (: return unchanged :) 
   else      
      let $ename := QName($uri, string-join(($prefix[.], "_"), ":"))                        
      let $copy := element {$ename} {$e}/*
      return
         document {$copy}/*
};

(:~
 : Adds namespace bindings to an element. The namespace bindings
 : are supplied as a namespace bindings map.
 :
 : @param elem the element
 : @param nsmap a map associating namespace prefixes with URIs
 : @return a copy of the element with namespace bindings added
 :
 : @version: 20121202-1
:)
declare function f:addNSBs($elem as element()?, 
                           $nsmap as element(zz:nsMap)) 
      as element()? {
    element {node-name($elem)} {
        for $ns in $nsmap/* return       
            namespace {$ns/@prefix} {$ns/@uri},
        $elem/@*,
        $elem/node()
    }   
};

(:~
 : Reports the namespace bindings found in a set of XML fragments 
 : (one or more elements or documents). The fragment roots are received 
 : as function parameter $roots.
 :
 : If $deep is false, only the bindings in the root element(s) are
 : considered, else all bindings found within the fragments.
 :
 : The report describes the bindings by stating a) all bindings found
 : at the root nodes, b) all bindings found at descendants provided the parent
 : node does not contain the same bindings. In other words, the report
 : shows the root bindings and all changes of binding within the fragment.
 :
 : (Note however that removals of bindings (possible in XML 1.1) are not 
 : reported.)
 :
 : Each bindings is described by a 'nsBinding' element whose @p, @uri and
 : @pa attributes contain the prefix, the namespace URI and the path
 : relative to the respective root node.
 :
 : Possible uses: a namespaceProfile can be used in order to transfer the
 : bindings of one fragment to another.
 :
 : @param $roots the fragment roots
 : @param $deep  if false, only the bindings occurring in the fragment roots
 :               are considered; otherwise, all bindings occurring withint the
 :               fragments are considered
 :
 : @version 0.1-20091212
 :)
declare function f:namespaceBindings($roots as node()*,
                                     $deep as xs:boolean) 
                 as element() {

   (: $roots = root elements :)
   let $roots := $roots/(self::document-node()/*, .)[1] return

   <nsBindings deep="{$deep}" baseUri="{$roots/base-uri(.)}">{
      if ($deep) then 
         let $allBindings := 
            <allBindings>{     
               for $root at $index in $roots,
                  $d in $root/descendant-or-self::*,
                  $p in in-scope-prefixes($d)[. ne "xml"] 
               let $uri := namespace-uri-for-prefix($p, $d)
               let $parentElem := $d/../self::*
               let $path := 
                  string-join(
                     $d/ancestor-or-self::*[not($d << $root)]/name(), "/")
               where not($parentElem) or
                     not($p = $parentElem/in-scope-prefixes(.)) or
                     not($uri eq $parentElem/namespace-uri-for-prefix($p, .))
               return
                  <nsBinding>{
                     attribute i {$index},
                     attribute p {$p},
                     attribute uri {$uri},
                     attribute pa {$path}
                  }</nsBinding>
            }</allBindings>
         return
            $allBindings/*[
               let $i := @i
               let $p := @p
               let $uri := @uri
               let $pa := @pa
               return
                  empty(preceding-sibling::nsBinding[@i eq $i and @p eq $p and @uri eq $uri and @pa eq $pa])
            ]   
      else
         for $root at $index in $roots,
             $p in in-scope-prefixes($root)[. ne "xml"] 
         let $uri := namespace-uri-for-prefix($p, $root)
         return 
            <nsBinding i="{$index}" p="{$p}" uri="{$uri}" />

   }</nsBindings>
};









