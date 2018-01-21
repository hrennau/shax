(:
 : -------------------------------------------------------------------------
 :
 : shaxLoader.xqm - a module for loading SHAX models, recursive
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

declare function f:expandImports($models as element(shax:model)+)
        as element(shax:models) {
    let $uris := $models/base-uri(.)
    let $uriNorms := for $uri in $uris return f:normalizeUri($uri)

    let $allModels := f:expandImportsRC($models, $uriNorms, ())[. instance of node()]
    let $errors := tt:extractErrors($allModels)
    return
        if ($errors) then
            tt:wrapErrors($errors) else
            
   (: eleminate elements with duplicate base URI; make sure that
    : elimination must be suppressed if the target namespace differs,
    : as a chameleon schema may be copied more than once in order
    : to acquire more than one namespace.
    :)

   let $allModels :=
      for $m at $pos in $allModels
      where empty($allModels[position() < $pos][base-uri(.) eq base-uri($m)])
      return $m

   (: eleminate duplicate schema elements with different base URI's.
    : A duplicate is recognized by containing a component already
    : contained by a preceding schema element.
    :)

(:
   let $elems := 
      for $elem at $pos in $elems 
      let $tns := string($elem/@targetNamespace)
      let $elementNames := $elem/xs:element/@name
      let $attributeNames := $elem/xs:attribute/@name
      let $attributeGroupNames := $elem/xs:attributeGroup/@name
      let $modelGroupNames := $elem/xs:modelGroup/@name
      return
         $elem [empty($elems[position() < $pos]
                            [string(@targetNamespace) eq $tns]
                            [xs:element/@name = $elementNames])]
               [empty($elems[position() < $pos]
                            [string(@targetNamespace) eq $tns]
                            [xs:attribute/@name = $attributeNames])]
               [empty($elems[position() < $pos]
                            [string(@targetNamespace) eq $tns]
                            [xs:attributeGroupNames/@name = $attributeGroupNames])]
               [empty($elems[position() < $pos]
                            [string(@targetNamespace) eq $tns]
                            [xs:modelGroupNames/@name = $modelGroupNames])]
:)

(:
   return
      ($errors, $allModels)
:)



(:
    let $expanded := ( 
        $models,
        ()
    )
:)    
    return
        <shax:models count="{count($allModels)}">{$allModels}</shax:models>
};

declare function f:expandImportsRC($models as element(shax:model)+, 
                                   $foundSoFar as xs:string*,
                                   $remainingImports as element(shax:import)*)
        as item()* {
   let $model := $models[1]
   (: let $DUMMY := trace($model/base-uri(.), 'DEAL_WITH_MODEL: ') :)
   let $remainingModels := tail($models)
   return
   
  (: not within recursion over one level of shax:import elements;
   : this means: $model is either the very root of the whole model, or
   : the recursion has just stepped down from a parent model element to 
   : an imported model element, and $model is that parent model element
   :)
  if (empty($remainingImports)) then (
     $model,
     let $imports := $model/shax:import  
     return
        if (empty($imports)) then 
           if (empty($remainingModels)) then ()
           else f:expandImportsRC($remainingModels, $foundSoFar, ())
        else
           f:expandImportsRC($models, $foundSoFar, $imports)
   )

   (: within recursion over one level of <shax:model> elements :)
   else
      let $actImport := $remainingImports[1]
      let $nextRemainingImports := tail($remainingImports)       
      let $uri := resolve-uri($actImport/@modelLocation, base-uri($actImport))
      let $uriNorm := f:normalizeUri($uri)
      let $actImportContribution :=
         if ($uriNorm = $foundSoFar) then ()
         else if (not(doc-available($uriNorm))) then trace(() , concat('FAILURE_TO_READ_FILE: ', $uriNorm, ' ')) 
         else 
            let $importedModel := doc($uriNorm)//shax:model
            return (
               $importedModel,
               $uriNorm,  (: write into stream, so that it can be extracted on 
                             calling levels and transferred into $foundSoFar :)
               if (tt:extractErrors($importedModel)) then 
                  ($importedModel, $uriNorm, $foundSoFar)
               else
                  f:expandImportsRC($importedModel, ($foundSoFar, $uriNorm), $nextRemainingImports)
            )
                
      let $remainingImportsContribution :=
         if (empty($nextRemainingImports)) then () 
         else
            let $nextFoundSoFar := 
                distinct-values(
                    ($foundSoFar, $actImportContribution[. instance of xs:anyAtomicType]))
            return
                f:expandImportsRC($model, $nextFoundSoFar, $nextRemainingImports)
      let $remainingModelsContribution :=
         if (empty($remainingModels)) then () 
         else
            let $nextFoundSoFar :=
                distinct-values(
                    ($foundSoFar, 
                        ($actImportContribution, $remainingImportsContribution)[. instance of xs:anyAtomicType]))
         return
            f:expandImportsRC($remainingModels, $nextFoundSoFar, ())
      return
         ($actImportContribution, $remainingImportsContribution, $remainingModelsContribution)
};

(:~
 : Normalizes a URI. Returns it without changes, unless the URI is
 : a file URI. In this case, reduces multiple slashes after 'file:' to
 : a single slash, puts drive letter lower-case and adds drive letter 'c' in
 : case the uri does not contain a drive letter.
 :
 : @param uri the uri to be normalized
 : @return the normalized uri
 :)
declare function f:normalizeUri($uri as xs:string) {
    if (not(matches($uri, '^\s*file:'))) then $uri else
    
    let $u := replace($uri, 'file:/+', 'file:/')
    let $driveLetter := replace($u, '^file:/(.):.+', '$1')[not(. eq $u)]
    return
        if ($driveLetter) then
            replace($u, '(file:/).(:.+)', concat('$1', lower-case($driveLetter), '$2'))
        else
            replace($u, '^(file:/)(.+)', '$1c:/$2') 
};


