(:
 : -------------------------------------------------------------------------
 :
 : rdfeLoader.xqm - a module for loading rdfe documents, recursively
 :     resolving imports
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
    "constants.xqm",
    "rdfeLoaderOld.xqm",
    "shaclWriter.xqm",
    "util.xqm";
    
declare namespace z="http://www.ttools.org/shax/ns/structure";
declare namespace shax="http://shax.org/ns/model";
declare namespace xsd="http://www.w3.org/2001/XMLSchema";
declare namespace re="http://www.rdfe.org/ns/model";

(:~
 : Loads one or several rdfe documents by recursively expanding any imports. 
 : The result is a set of documents including the original input documents
 : plus all directly and indirectly imported documents.
 :
 : @param docs the rdfe documents to be loaded
 : @return a set of rdfe documents
 :)
declare function f:loadRdfe($docs as element(re:semanticMap)+)
        as element(re:semanticMap)+ {
    let $old := false() return if ($old) then f:expandRdfes_old($docs) else
    
    let $docsExpanded := f:loadRdfeRC($docs, ())[. instance of node()]
    let $errors := tt:extractErrors($docsExpanded)
    return if ($errors) then tt:wrapErrors($errors) else
            
    $docsExpanded   
};

(:~
 : Recursive helper function of `f:expandRdfes`.
 :)
declare function f:loadRdfeRC($docs as element(re:semanticMap)+, 
                              $foundSoFar as xs:string*)
        as item()* {
   let $DUMMY := trace(string-join($foundSoFar, ', '), 'FOUND_SO_FAR:           ')
   let $doc := head($docs)
   let $uri := trace($doc/base-uri(.) ! f:normalizeUri(.) , 'INSPECT SEMAP, DOC URI: ')
   let $remainingDocs := tail($docs)
   return
     if (not($uri = $foundSoFar)) then (
        let $uri := trace($doc/base-uri(.) ! f:normalizeUri(.) , 'LOAD SEMAP, DOC URI:    ') return
        $doc, 
        $uri,     
        let $newFoundSoFar := ($foundSoFar, $uri)
        let $importedDocs := $doc/re:import/@href/doc(resolve-uri(., base-uri(..)))/*
                              [not(base-uri(.) ! f:normalizeUri(.) = $newFoundSoFar)]
        let $importedDocsExpanded := 
            if (not($importedDocs)) then () else f:loadRdfeRC($importedDocs, $newFoundSoFar)
        let $furtherDocs := $importedDocsExpanded[. instance of node()]
        let $furtherDocUris := $importedDocsExpanded[. instance of xs:anyAtomicType]
        return (
            $furtherDocs,
            $furtherDocUris,
            if (empty($remainingDocs)) then ()
            else f:loadRdfeRC($remainingDocs, ($newFoundSoFar, $furtherDocUris))
        )                    
      ) 
      else if (empty($remainingDocs)) then ()
      else f:loadRdfeRC($remainingDocs, $foundSoFar)
     
};