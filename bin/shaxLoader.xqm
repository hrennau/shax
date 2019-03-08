(:
 : -------------------------------------------------------------------------
 :
 : shaxLoader.xqm - a module for loading SHAX models, recursively
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

(:~
 : Loads one or several SHAX models by recursively expanding any imports. 
 : The result is a single document with a <shax:models> root element, which
 : has <shax:model> child elements.
 :
 : @param docs the SHAX documents to be loaded
 : @return a document with a shax:models root with shax:model child elements
 :)
declare function f:loadShax($models as element(shax:model)+)
        as element(shax:models) {
    (: let $old := false() return if ($old) then f:expandImports_old($models) else :)
    
    let $expandedModels := f:loadShaxRC(($models), ())[. instance of node()]
    let $errors := tt:extractErrors($expandedModels)
    return if ($errors) then tt:wrapErrors($errors) 
    else <shax:models count="{count($expandedModels)}">{$expandedModels}</shax:models>
};

(:~
 : Recursive helper function of `f:loadShax`.
 :)
declare function f:loadShaxRC($docs as element()+, 
                              $foundSoFar as xs:string*)
        as item()* {
   (: let $DUMMY := trace(string-join($foundSoFar, ', '), 'FOUND_SO_FAR:           ') :)
   let $doc := head($docs)
   let $uri := $doc/base-uri(.) ! f:normalizeUri(.)
   let $remainingDocs := tail($docs)
   return
     if (not($uri = $foundSoFar)) then (
        let $_INFO := trace($uri, 'LOAD SHAX: ') return
        $doc, 
        $uri,     
        let $newFoundSoFar := ($foundSoFar, $uri)
        let $importedDocs := $doc/shax:import/@modelLocation/doc(resolve-uri(., base-uri(..)))/*
                              [not(base-uri(.) ! f:normalizeUri(.) = $newFoundSoFar)]
        let $importedDocsExpanded := 
            if (not($importedDocs)) then () else f:loadShaxRC($importedDocs, $newFoundSoFar)
        let $furtherDocs := $importedDocsExpanded[. instance of node()]
        let $furtherDocUris := $importedDocsExpanded[. instance of xs:anyAtomicType]
        return (
            $furtherDocs,
            $furtherDocUris,
            if (empty($remainingDocs)) then ()
            else f:loadShaxRC($remainingDocs, ($newFoundSoFar, $furtherDocUris))
        )                    
      ) 
      else if (empty($remainingDocs)) then ()
      else f:loadShaxRC($remainingDocs, $foundSoFar)     
};
