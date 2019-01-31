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
declare function f:expandRdfes_old($docs as element(re:semanticMap)+)
        as element(re:semanticMap)+ {
    let $uris := $docs/base-uri(.) ! f:normalizeUri(.)

    let $allDocs := f:expandRdfes_oldRC($docs, $uris, ())[. instance of node()]
    let $errors := tt:extractErrors($allDocs)
    return
        if ($errors) then tt:wrapErrors($errors) else
            
    $allDocs
(:    
   (: DOUBTFUL if this elimination is necessary :)            
   (: eleminate elements with duplicate base URI :)
   let $allDocs :=
      for $m at $pos in $allDocs
      where empty($allDocs[position() < $pos][base-uri(.) eq base-uri($m)])
      return $m
    return
        $allDocs
:)        
};

(:~
 : Recursive helper function of `f:expandRdfes`.
 :)
declare function f:expandRdfes_oldRC($docs as element(re:semanticMap)+, 
                                     $foundSoFar as xs:string*,
                                     $remainingImports as element(re:import)*)
        as item()* {
   let $doc := head($docs)
   (: let $DUMMY := trace($doc/base-uri(.), 'DEAL_WITH_MODEL: ') :)
   let $remainingDocs := tail($docs)
   return
   
  (: not within recursion over one level of import elements;
   : this means: $doc is either the very root of the import tree, or
   : the recursion has just stepped down from a parent doc to 
   : an imported doc, and $doc is that parent doc
   :)
  if (empty($remainingImports)) then (
     $doc,
     let $uri := trace( $doc/base-uri(.) ! f:normalizeUri(.) , 'DOC-FOR-URI: ')
     let $newFoundSoFar := ($foundSoFar, $uri)
     let $imports := $doc/re:import  
     return
        (: process the imports :)           
        if ($imports) then f:expandRdfes_oldRC($docs, $newFoundSoFar, $imports)
        (: continue with next document, if any :)
        else if ($remainingDocs) then f:expandRdfes_oldRC($remainingDocs, $newFoundSoFar, ())
        (: nothing left :)
        else ()
   )

   (: within recursion over one level of import elements :)
   else
      let $actImport := head($remainingImports)
      let $tailRemainingImports := tail($remainingImports)       
      let $uri := trace( resolve-uri($actImport/@href, base-uri($actImport)) ! f:normalizeUri(.) , 'IMPORT URI: ')
      let $actImportContribution :=
         if ($uri = $foundSoFar) then trace((), concat('URI_ALREADY_LOADED: ', $uri))
         else if (not(doc-available($uri))) then trace(() , concat('FAILURE_TO_READ_FILE: ', $uri, ' ')) 
         else 
            let $importedDoc := doc($uri)/*
            return (
               $uri,  (: write into stream, so that it can be extracted on 
                         calling levels and transferred into $foundSoFar :)
               if (tt:extractErrors($importedDoc)) then ($importedDoc, $uri, $foundSoFar)
               else f:expandRdfes_oldRC($importedDoc, ($foundSoFar, $uri), ())
            )
                
      let $remainingImportsContribution :=
         if (empty($tailRemainingImports)) then () 
         else
            let $newFoundSoFar := distinct-values(($foundSoFar, 
                $actImportContribution[. instance of xs:anyAtomicType]))
            return
                f:expandImportsRC($doc, $newFoundSoFar, $tailRemainingImports)
      let $remainingDocsContribution :=
         if (empty($remainingDocs)) then () 
         else
            let $newFoundSoFar := distinct-values(($foundSoFar, 
                ($actImportContribution, $remainingImportsContribution)[. instance of xs:anyAtomicType]))
         return
            f:expandRdfes_oldRC($remainingDocs, $newFoundSoFar, ())
      return
         ($actImportContribution, $remainingImportsContribution, $remainingDocsContribution)
};
