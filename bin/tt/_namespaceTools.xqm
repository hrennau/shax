xquery version "1.0";

(:
 : namespaceTools.xqm - utility functions for namespace-related operations
 :
 : Version 20140124 first version 
 :)

(:~
 : This module provides functions to perform various namespace-related operations.
 :
 : Uses modules: -
 :  
 : Public functions:
 : <ul>
 :    <li>addNSB - adds a namespace binding to an element</li>
 :    <li>addNSBs - adds namespace bindings to an element.</li>
 :    <li>changeTns - changes the target namespace of a schema document</li>
 :    <li>copyDeepNSB - copies namespace bindings from a source fragment a target node</li>
 :    <li>copyNSB - copies namespace bindings from a source node a target node</li>
 :    <li>createNsMap - creates a namespace bindings map capturing the 
             namespace bindings of a supplied element.</li>
 :    <li>findPrefix - finds for a given namespace a prefix not yet used in a document</li>
 :    <li>namespaceBindings - reports the namespace bindings found in a set of XML fragments</li>
 :    <li>normalizeQName - normalizes a QName according to a supplied mapping 
 :           of namespace URIs to prefixes.</li>
 : </ul>
 :
 : Private functions: -
 :
 : Global variables: -
 :
 : @author Hans-Juergen Rennau
 :
 : @version 20140124-1
 :)   
(: ######################################################################### :)

module namespace m="http://www.ttools.org/xquery-functions";
import module namespace pt="http://www.ttools.org/xquery-functions" at "_constants.xqm";

declare copy-namespaces preserve, inherit;

declare namespace z="http://www.xsdplus.org/ns/structure";
declare namespace s="http://www.xsdplus.org/ns/structure";
declare namespace xs="http://www.w3.org/2001/XMLSchema";
declare namespace xe="http://www.xsdplus.org/ns/errors";

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
declare function m:addNSB($e as element()?,
                          $uri as xs:string,
                          $prefix as xs:string) 
                 as element()? {
    if ($uri eq "") then 
        <_tmp xmlns="">{$e}</_tmp>/*                (: remove default namespace :)
    else if ($uri eq namespace-uri-for-prefix($prefix, $e)) then 
        $e                                          (: return unchanged :) 
    else                                            (: add namespace binding :)
        element {node-name($e)} {
            namespace {$prefix} {$uri},
            $e/@*, $e/node()}
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
declare function m:addNSBs($elem as element()?, 
                           $nsmap as element(z:nsMap)) 
      as element()? {
   let $copy :=
      <z:mp>{
         $nsmap/*[@prefix/string()]/attribute {QName(@uri, concat(@prefix, ':', '_'))} {}, $elem
      }</z:mp>/*
   return
      document {$copy}/*
};

(:~
 : Adds the "used" namespace bindings to an element. These are defined as
 : follows: for each prefix found in the element and its descendant,
 : the first-in-document-order namespace binding is used; if the
 : element has a default namespace, this is also added to the
 : namespace bindings.
 :
 : @param elem the element
 : @return a copy of the element with namespace bindings added
 :
 : @version: 20121215-1
:)
declare function m:addUsedNSBs($elem as element()?) 
      as element()? {
   let $prefixes :=  distinct-values($elem/descendant-or-self::*/in-scope-prefixes(.))
   let $nsmap :=
      <z:nsMap>{
         for $prefix in $prefixes[not (. eq 'xml')]
         let $uri := 
            $elem/descendant-or-self::*[in-scope-prefixes(.) = $prefix][1]/
            namespace-uri-for-prefix($prefix, .)
         order by lower-case($prefix)
         return
            <z:ns prefix="{$prefix}" uri="{$uri}"/>
         ,
         let $defaultNs := namespace-uri-for-prefix('', $elem)
         return
            if (string($defaultNs)) then <z:ns prefix="" uri="{$defaultNs}"/>
            else ()
      }</z:nsMap>
   return m:addNSBs($elem, $nsmap)
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
declare function m:changeTns($n as node(), 
                             $uri as xs:string, 
                             $prefix as xs:string?) 
as node() 
{
   typeswitch ($n)
   case document-node() return m:changeTns($n/*, $uri, $prefix)

   case $e as element(xs:schema) return      
      let $usePrefix := m:findPrefix($e, $uri, $prefix, ())
      return 
         m:addNSB(
            m:copyNSB($n, 
               <xs:schema>{
	          if ($uri) then attribute targetNamespace {$uri} else (),
                  attribute xml:base {base-uri($n)},
                  attribute s:isChameleon {true()},
	          for $a in $n/(@* except (@targetNamespace, @xml:base), node()) 
                     return m:changeTns($a, $uri, $usePrefix)
	       }</xs:schema>) 
	       , $uri, $usePrefix)

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
         for $ac in $e/(@*, node()) return m:changeTns($ac, $uri, $prefix)
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
 : target node (element or document. If parameter $deepCopy is true, all
 : bindings found within the fragment rooted in $source are copied, else 
 : only the bindings found in $source. 
 :
 : Note that in case of a deep copy, a prefix may be bound within $source to more
 : than one namespace URI. In such a case, the first binding encountered (in
 : document order) is used.
 :
 : @param source the source node
 : @param target the target node
 : @param $deepCopy unless true, only top-level bindings are considered
 : @param excludedPrefixes prefixes that must not be used
 :)
declare function m:copyDeepNSB($source as node()*, 
                               $target as node(),
                               $excludedPrefixes as xs:string*)
   as node()? {
   let $bindings := m:namespaceBindings($source, true())/*
         [let $p := @p return empty(preceding-sibling::nsBinding[@p eq $p])]
         [not(@p = $excludedPrefixes)]
   let $wrapperNs := $bindings[@p eq ""][1]/@uri
   return
      element {QName($wrapperNs, "_")} {
         for $b at $pos in $bindings[@p ne ""] 
         return 
            attribute
               {QName($b/@uri, 
                   string-join(($b/@p[string()], concat("_", $pos)), ":"))}
               {},
         $target
      }/*
};

(:~
 : Copy namespace bindings from the source node (element or document) to the
 : target node (element or document.e. 
 :
 : @param source the source node
 : @param target the target node
 :)
declare function m:copyNSB($source as node()*, 
                           $target as node())
   as node()? 
{
   let $bindings := m:namespaceBindings($source, false())/*
   let $wrapperNs := $bindings[@p eq ""][1]/@uri
   return
      element {QName($wrapperNs, "_")} {
         for $b at $pos in $bindings[@p ne ""] 
         return 
            attribute
               {QName($b/@uri, 
                   string-join(($b/@p[string()], concat("_", $pos)), ":"))}
               {},
         $target
      }/*
};

(:~
 : Creates a namespace bindings map capturing the namespace bindings
 : of a supplied element.
 :
 : @param elem the element
 : @return a map of namespace bindings
 :)
declare function m:createNsMap($elem as element())
      as element(z:nsMap) {
   <z:nsMap>{
      for $prefix in in-scope-prefixes($elem) return
         <z:ns prefix="{$prefix}" uri="{namespace-uri-for-prefix($prefix, $elem)}"/>
   }</z:nsMap>
};

(:~
 : Edits a data path by discarding all prefixes if $noprefix
 : is true, and returns the original path string otherwise.
 :
 : @path the path string
 : @noprefix indicates whether prefixes must be removed
 : @return the string to be used as rendering
 :)
declare function m:editDataPath($path as xs:string, $noprefix as xs:boolean?)
      as xs:string {
   if (not($noprefix)) then $path
   else replace($path, '(^|/)[^/]+?:', '$1')
};

(:~
 : Edits a normalized QName by discarding the prefix
 : if $noprefix is true and the namespace name is not
 : the XSD namespace name.
 :
 : @param name a normalized QName
 : @param noprefix if true, the prefix shall be discarded,
 :    provided the namespace name is not the XSD namespace name
 : @param nsmap a table associating namespaces with normalized
 :    prefixes
 :) 
declare function m:editNormalizedQName($name as xs:string?, 
                                       $noprefix as xs:boolean?,
                                       $nsmap as element(z:nsMap))
      as xs:string? {
   if (empty($name)) then ()
   else if (not($noprefix)) then $name
   else
      let $qname := m:resolveNormalizedQName($name, $nsmap)
      let $uri := namespace-uri-from-QName($qname)
      return
         if ($uri eq $pt:URI_XSD) then $name
         else replace($name, '.*:', '')
};

(:~
 : Finds a prefix for a given namespace in the context of a given document.
 : The prefix returned has not yet been used in the document.
 :
 : @param n a node from the document in whose context the prefix is sought
 : @param uri namespace URI for which a prefix is sought
 : @prefixProposal a proposed prefix
 : @recursiveAttempt is set, indicates the recursion level above the initial call
 :
 : @version 20100105 
 :)
declare function m:findPrefix($n as node(),
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
            m:findPrefix($n, $uri,
                          concat("ns", $nextRecursiveAttempt),
                          $nextRecursiveAttempt)
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
declare function m:namespaceBindings($roots as node()*,
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

(:~
 : Normalizes a QName according to a supplied binding of namespace prefixes.
 :
 : @param qname the QName to be normalized
 : @param nsmap a map representing the binding of namespace prefixes
 : @return the normalized QName
 :)
declare function m:normalizeQName(
                        $qname as xs:QName, 
                        $nsmap as element(z:nsMap)?) 
        as xs:QName {
        
   if (empty($nsmap)) then $qname
   else
      let $uri := namespace-uri-from-QName($qname)
      return
         if (empty($uri)) then $qname
         else
            let $prefix := $nsmap/z:ns[@uri eq $uri]/@prefix
            return
               if (empty($prefix)) then $qname else
                  let $lexName := string-join(($prefix, local-name-from-QName($qname)), ':')
                  return QName($uri, $lexName)
};

(:~
 : Resolves a normalized QName, using the namespace map which had been used
 : for the normalization. The resulting QName has a lexical name without prefix. 
 :
 : @param name the name string to be resolved
 : @nsmap  a map associating namespace URIs with prefixes
 : @return the QName 
 :)
declare function m:resolveNormalizedQName($name as xs:string, $nsmap as element(z:nsMap))
      as xs:QName? {
   if (empty($name)) then () else
   
   let $prefix := substring-before($name, ':')
   let $lname := if (string-length($prefix)) then substring-after($name, ':') 
                 else $name
   return
      QName($nsmap/*[@prefix eq $prefix]/@uri, $lname)                      
};

(:~
 : Resolves a normalized QName, using the namespace map which had been used
 : for the normalization and keeping the original prefix.
 :
 : @param name the name string to be resolved
 : @nsmap  a map associating namespace URIs with prefixes
 : @return the QName
 :)
declare function m:resolveNormalizedQNamePrefixed($name as xs:string, 
                                                  $nsmap as element(z:nsMap))
      as xs:QName? {
   if (empty($name)) then () else
   
   let $prefix := substring-before($name, ':')
   return
      QName($nsmap/*[@prefix eq $prefix]/@uri, $name)                      
};

(:
   D E P R E C A T E D    F U N C T I O N S
:)

(:~
 : DEPRECATED - use 'addNSBs' instead.
 :
 : Adds namespace bindings to an element. The namespace bindings
 : are supplied as a namespace bindings map.
 :
 : @param elem the element
 : @param nsmap a map associating namespace prefixes with URIs
 : @return a copy of the element with namespace bindings added
:)
declare function m:addNSB($elem as element()?, $nsmap as element(z:nsMap)) as element()? {
   <z:mp>{
      $nsmap/*[@prefix/string()]/attribute {QName(@uri, concat(@prefix, ':', '_'))} {}, $elem
   }</z:mp>/*
};

(:~
 : DEPRECATED - 'addNSB' instead.
 :
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
 : @param nsUri the namespace URI
 : @param prefix the prefix to be used
 :
 : @version: 20091212
 :)
declare function m:addInscopeNamespace($e as element()?,
                                       $nsUri as xs:string,
                                       $prefix as xs:string) 
                 as element()? {
   if ($nsUri eq "") then 
      <_tmp xmlns="">{$e}</_tmp>/*   (: remove default namespace :)
   else if ($nsUri eq namespace-uri-for-prefix($prefix, $e)) then      
      $e                             (: return unchanged :) 
   else                              
      element {QName($nsUri, string-join(($prefix[.], "_"), ":"))} 
	          {$e}/*                 (: add binding via transient wrapper :)                                        
};





(: Stylus Studio meta-information - (c) 2004-2009. Progress Software Corporation. All rights reserved.

<metaInformation>
   <scenarios>
      <scenario default="yes" name="Scenario1" userelativepaths="yes" externalpreview="no" useresolver="yes" url="" outputurl="" processortype="datadirect" tcpport="7864367" profilemode="0" profiledepth="" profilelength="" urlprofilexml="" commandline=""
                additionalpath="" additionalclasspath="" postprocessortype="none" postprocesscommandline="" postprocessadditionalpath="" postprocessgeneratedext="" host="" port="0" user="" password="" validateoutput="no" validator="internal"
                customvalidator="">
         <advancedProperties name="DocumentURIResolver" value=""/>
         <advancedProperties name="CollectionURIResolver" value=""/>
         <advancedProperties name="ModuleURIResolver" value=""/>
      </scenario>
   </scenarios>
   <MapperMetaTag>
      <MapperInfo srcSchemaPathIsRelative="yes" srcSchemaInterpretAsXML="no" destSchemaPath="" destSchemaRoot="" destSchemaPathIsRelative="yes" destSchemaInterpretAsXML="no"/>
      <MapperBlockPosition></MapperBlockPosition>
      <TemplateContext></TemplateContext>
      <MapperFilter side="source"></MapperFilter>
   </MapperMetaTag>
</metaInformation>
:)