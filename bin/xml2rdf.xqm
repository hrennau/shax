(:
 : -------------------------------------------------------------------------
 :
 : xml2rdf.xqm - functions for transforming XML into RDF.
 :
 : -------------------------------------------------------------------------
 :)
 
(:~@operations
   <operations>
      <operation name="xml2rdf" type="element()" func="xml2rdfOp">     
         <param name="dox" type="docFOX+" fct_minDocCount="1" sep="SC"/>    
         <param name="format" type="xs:string?" default="xml"/>
      </operation>
    </operations>  
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
    "constants.xqm",
    "shaclWriter.xqm",
    "shaxLoader.xqm",
    "schemaLoader.xqm",
    "targetNamespaceTools.xqm",
    "typeGlobalizer.xqm",
    "util.xqm",
    "xsdComponentManager.xqm";

    (: "typeGlobalizer.xqm", :) 
    
declare namespace z="http://www.ttools.org/shax/ns/structure";
declare namespace zz="http://www.ttools.org/structure";
declare namespace shax="http://shax.org/ns/model";
declare namespace shaxerr="http://shax.org/ns/error";
declare namespace nons="http://shax.org/ns/nonamespace";
declare namespace xsd="http://www.w3.org/2001/XMLSchema";

(:
 : ============================================================================
 :
 :     o p e r a t i o n s
 :
 : ============================================================================
 :)

(:~
 : Implements operation 'xml2rdf', which transforms XML
 : documents into RDF triples.
 :
 : @param request the operation request
 : @return RDF triples
 :) 
declare function f:xml2rdfOp($request as element())
        as item() {
    let $docs := tt:getParam($request, 'dox')/*
    let $format := tt:getParam($request, 'format')
    let $rdf := f:xml2rdf($docs, $format)
    return
        <z:rdf-triples countDocs="{count($docs)}">{
            $rdf
        }</z:rdf-triples>
};   

(:
 : ============================================================================
 :
 :     p u b l i c    f u n c t i o n s
 :
 : ============================================================================
 :)

(:~
 : Implements operation 'xml2rdf', which transforms XML
 : documents into RDF triples.
 :
 : @param request the operation request
 : @return RDF triples
 :) 
declare function f:xml2rdf($docs as element()*, $format as xs:string)
        as item()* {
    let $withIRIs :=
        for $doc at $pos in $docs 
        return
            copy $doc_ := $doc
            modify 
                for $e at $nnr in $doc_/descendant-or-self::*[@*, *] 
                return 
                    insert node attribute shax:nodeNr {concat($pos, '_', $nnr)} into $e
            return $doc_
    return $withIRIs
};   
