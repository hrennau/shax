(:
 : -------------------------------------------------------------------------
 :
 : targetNamespaceTools.xqm - Document me!
 :
 : -------------------------------------------------------------------------
 :)

(: ============================================================================== :)
module namespace f="http://www.ttools.org/shax/ns/xquery-functions";
declare namespace z="http://www.ttools.org/shax/ns/structure";
declare namespace zz="http://www.ttools.org/structure";

import module namespace tt="http://www.ttools.org/xquery-functions" at 
    "tt/_constants.xqm";

import module namespace i="http://www.ttools.org/shax/ns/xquery-functions" at 
    "constants.xqm";
 
(:~
 : Returns the target namespaces found in a list of supplied
 : schema elements. The namespaces are sorted by their
 : string value set to lower case.
 :
 : @param schemas the schemas to be evaluated
 : @return a sorted list of target namespaces
 :)
declare function f:getTargetNamespaces($schemas as element(xs:schema)*) 
      as xs:string* {
   for $tns in distinct-values($schemas/@targetNamespace)
   order by lower-case($tns)
   return $tns
};

(:~
 : Creates a map associating all target namespaces with
 : normalized prefixes.
 : <p/>
 : The map contains an additional entry, associating
 : the prefix 'z' with the namespace of shax data 
 : structures.
 :
 : @schemas the schemas to be evaluated
 : @return a map containing prefix/uri pairs
 :)
declare function f:getTnsPrefixMap($schemas as element(xs:schema)*)
      as element(zz:nsMap) {

   let $tnss := 
      for $t in distinct-values($schemas/@targetNamespace)
      order by lower-case($t) 
      return $t
   return
      <zz:nsMap>{
         let $prefixTnsPairs := f:_getPrefixTnsPairs($tnss)
         for $pair in $prefixTnsPairs
         let $prefix := substring-before($pair, ':')
         let $tns := substring-after($pair, ':')
         where not($tns eq $tt:URI_XSD)         
         return
            <zz:ns>{
               attribute prefix {$prefix},
               attribute uri {$tns}
            }</zz:ns>,
         <zz:ns prefix="xml" uri="http://www.w3.org/XML/1998/namespace"/>,
         <zz:ns prefix="xs" uri="http://www.w3.org/2001/XMLSchema"/>,
         <zz:ns prefix="zz" uri="http://www.ttools.org/structure"/>
      }</zz:nsMap>
};

(:~
 : Creates a table with target namespace related data, 
 : providing for each TNS encountered in the input schemas:
 : (a) the URI
 : (b) a normalized prefix
 : (c) all schema document URIs (only if $docuris = true)
 :
 : @schemas the schemas to be evaluated
 : @return the target namespace map
 :)
declare function f:getTnsTable($schemas as element(xs:schema)*,
                               $docuris as xs:boolean?)
      as element(z:tnsTable) {

   let $tnss := f:getTargetNamespaces($schemas)
   let $urisNoTnsSchemas := f:_getDocUrisForTns($schemas, ())
   return
      <z:tnsTable>{
         if (empty($urisNoTnsSchemas)) then () else
            <z:tns>{
               attribute prefix {},
               attribute uri {},
               if (not($docuris)) then () else
                  for $uri in $urisNoTnsSchemas return <z:xsd uri="{$uri}"/>
            }</z:tns>,

         let $prefixTnsPairs := f:_getPrefixTnsPairs($tnss)
         for $pair in $prefixTnsPairs
         let $prefix := substring-before($pair, ':')
         let $tns := substring-after($pair, ':')
         let $uris :=
            if (not($docuris)) then () else f:_getDocUrisForTns($schemas, $tns)
         return
            <z:tns>{
               attribute prefix {$prefix},
               attribute uri {$tns},
               if (not($docuris)) then () else
                  for $uri in $uris return <xsd uri="{$uri}"/>
            }</z:tns>
      }</z:tnsTable>
};

(:~
 : Returns for a sequence of namespace URIs the
 : normalized prefixes. For each namespace a 
 : colon-separated concatenation of prefix and 
 : namespace URI is returned. Normalized prefixes
 : are the lower case letters corresponding to the
 : position of the namespace URI within the list
 : of namespace URIs. If the position is gt 25, 
 : the letters are reused and a suffix
 : is appended which indicates the number of
 : the current letter cycle (2, 3, ...). 
 : The prefixses therefore are:
 : 'a', 'b', 'c', ..., 'x', 'y', 'a2', 'b2', .....
 :
 : @tnss the target namespaces
 : @return the prefix/tns pairs
 :)
declare function f:_getPrefixTnsPairs($tnss as xs:string*) 
      as xs:string* {
   for $tns at $pos in $tnss
   let $seriesNr := ($pos - 1) idiv 25
   let $postfix := if (not($seriesNr)) then () else $seriesNr + 1
   let $p := 1 + ($pos - 1) mod 25
   let $char := substring('abcdefghijklmnopqrstuvwxy', $p, 1)
   let $prefix := concat($char, $postfix)
   where not($tns eq 'http://www.w3.org/XML/1998/namespace')
   return concat($prefix, ':', $tns)
};

(:~
 : Returns the document URIs of schemas with a given
 : target namespace.
 :
 : @schemas the schemas to be evaluated
 : @tns the target namespace
 : @return the document URIs
 :)
declare function f:_getDocUrisForTns($schemas as element(xs:schema)*, 
                                     $tns as xs:string?)
      as xs:string* {
   let $tns := string($tns)
   for $schema in $schemas[not(@targetNamespace ne $tns)]
   let $uri := ($schema/root()/document-uri(.), base-uri($schema))[1]
   order by lower-case($uri)
   return $uri
};
