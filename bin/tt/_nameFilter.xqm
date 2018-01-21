(:~
nameFilter.xqm - utility functions for name filtering

writeNameFilter - compiles a whitespace separated list of name patterns into a
                  filter element
writeNameFilterPath - ...   
filterNames - filters a list of names, using a supplied name filter
matchesNameFilterSome - ...
matchesNameFilter - ...

Version 20130822-1 #r => pattern is a regex
Version 20130113-1
:)

(: ============================================================================== :)

module namespace m="http://www.ttools.org/xquery-functions";

(: 
=================================================================

   p u b l i c    f u n c t i o s
   
=================================================================
:)

(:~
 : Reports whether a name matches a name filter. The name filter
 : was previously obtained by passing a whitespace separated list
 : of name patterns to function 'writeNameFilter'.
 :
 : @params name the name to be checked
 : @param filter the filter against which to check
 : @return true if the name matches the filter, or the filter is empty, false otherwise
 :)
declare function m:matchesNameFilter(
                        $name as xs:string?, 
                        $filter as element(nameFilter)?)
      as xs:boolean? {
    if (empty($name)) then () else      
    if (empty($filter)) then true() else

      (empty($filter/filterPos/filter) or 
            (some $f in $filter/filterPos/filter satisfies matches($name, string($f/@pattern), string($f/@options)))) and
      (every $f in $filter/filterNeg/filter satisfies not(matches($name, string($f/@pattern), string($f/@options)))) 
};

(:~
 : Reports whether a name matches at least one of a series of name filters.
 :
 : @params name the name to be checked
 : @param filters the filters against which to check 
 : @return true if the name matches at least one filter or there is not filter, false otherwise
 :)
declare function m:matchesNameFilters(
                        $name as xs:string?, 
                        $filters as element(nameFilter)*)
      as xs:boolean? {
    if (empty($name)) then () else      
    if (empty($filters)) then true() else
    some $filter in $filters satisfies m:matchesNameFilter($name, $filter) 
};

(:~
 : Checks if a sequence of names contains at least one name
 : matching a name filter. More precisely, returns true if 
 : there is at least one name which is not removed by the filter.
 :
 : Note. This implies that the function returns false
 : if the sequence of names is empty, and the function
 : returns true if there are names but there is no
 : name filter.
 :
 : @params names the names to be checked
 : @return true if there is at least one name not removed
 :    by the filter
 :)
declare function m:matchesNameFilterSome(
                        $names as xs:string*, 
                        $filter as element(nameFilter)?)
      as xs:boolean {
   if (empty($names)) then false()
   else if (empty($filter)) then true() else
      some $name in $names satisfies
         $name
            [empty($filter/filterPos/filter) or (some $f in $filter/filterPos/filter 
                   satisfies matches(., string($f/@pattern), string($f/@options)))]
            [every $f in $filter/filterNeg/filter 
                   satisfies not(matches(., string($f/@pattern), string($f/@options)))] 
};

(:~
 : Filters a sequence of names by a name filter. The name filter
 : was previously obtained by passing a whitespace separated list
 : of name patterns to function 'writeNameFilter'.
 :
 : @params names the names to be filtered
 : @return the filtered names
 :)
declare function m:filterNames(
                        $names as xs:string*, 
                        $filter as element(nameFilter)?)
        as xs:string* {
   if (empty($filter)) then $names else
   $names
      [empty($filter/filterPos/filter) or 
        (some $f in $filter/filterPos/filter satisfies 
            matches(., string($f/@pattern), string($f/@options)))]
      [every $f in $filter/filterNeg/filter satisfies 
        not(matches(., string($f/@pattern), string($f/@options)))] 
};

(:~
 : Retrieves the value associated with a name according to
 : a name filter map. The name filter map can optionally specify 
 : a value type. If a value type is specified, the value returned
 : by this function is typed correspondingly.
 :
 : @DO_IT: Currently only these types are supported:
 :    xs:boolean xs:int xs:integer xs:long xs:string 
 :
 : @param name the name with which the value is associated
 : @param nameFilterMap a name filter map which associates values
 :    with name patterns
 : @param defaultValue an optional default value
 : @return the value associated with the name
 :)
declare function m:nameFilterMapValue($name as xs:string, 
                                      $nameFilterMap as element(nameFilterMap)?, 
                                      $defaultValue as xs:anyAtomicType?)
        as xs:anyAtomicType? {
    if (not($nameFilterMap)) then $defaultValue else
    
    let $value := $nameFilterMap/entry[nameFilter][m:matchesNameFilter($name, nameFilter)][1]/@value         
    let $value :=
        if (exists($value)) then $value else
            let $value := $nameFilterMap/entry[not(*)][1]/@value
            return
                if (exists($value)) then $value else $defaultValue
    return
        if (not($value)) then () else
            let $valueType := $nameFilterMap/@valueType/string()
            return
                if (not($valueType)) then $value else
                    if ($valueType eq 'xs:boolean') then xs:boolean($value)
                    else if ($valueType eq 'xs:int') then xs:int($value)                    
                    else if ($valueType eq 'xs:integer') then xs:integer($value)                    
                    else if ($valueType eq 'xs:long') then xs:long($value)                    
                    else if ($valueType eq 'xs:string') then xs:string($value)                    
                    else $value
};

(:~
 : Checks whether a path matches a path filter.
 :
 : @param path the path to be checked
 : @param pathFilter the path filter against which to check
 : @return the boolean check result, or the empty sequence
 :    in case of an error
 :)
declare function m:matchesPathFilter(
                    $path as xs:string?,
                    $filterPath as element(pathFilter)?)
        as xs:boolean? {

    let $path := replace(replace($path, '^[A-Z]:', ''), '\\', '/')
    let $path := replace($path, '^/', '')
    let $steps := tokenize($path, '/')
    let $lastStep := $steps[last()]
    return
        if ($filterPath/nameFilter) then m:matchesNameFilter($lastStep, $filterPath/nameFilter) else
    
    let $pos1 := $filterPath/pathFilterPos/nameFilter
    let $pos2 := $filterPath/pathFilterPos/nameFilterPath
    let $neg1 := $filterPath/pathFilterNeg/nameFilter
    let $neg2 := $filterPath/pathFilterNeg/nameFilterPath
    
    let $posResult :=
        if (not(($pos1, $pos2))) then true() else
        
        let $posResult1 :=
            if (not($pos1)) then false() else
                some $f in $pos1 satisfies m:matchesNameFilter($lastStep, $f) 
        return
            if ($posResult1) then true() 
            else if (not($pos2)) then false() else
                some $f in $pos2 satisfies m:_matchesNameFilterPath($path, $f)
            
    return
        if (not($posResult)) then false() 
        else if (not(($neg1, $neg2))) then true()
        else
            not(some $f in $neg1 satisfies m:matchesNameFilter($lastStep, $f))
            and
            not(some $f in $neg2 satisfies m:_matchesNameFilterPath($path, $f))    
};

(: 
=================================================================

   p r i v a t e    f u n c t i o s
   
=================================================================
:)

(:~
 : Checks whether a path matches a name filter path.
 :
 : @param path the path to be checked
 : @param filterPath the filter path against which to check
 : @return the boolean check result, or the empty sequence
 :    if $path is the empty sequence
 :)
 declare function m:_matchesNameFilterPath(
                    $path as xs:string?,
                    $filterPath as element(nameFilterPath)?)
        as xs:boolean? {
    if (not($path)) then ()
    else if (not($filterPath)) then true() else
    
    (: remove windows drive letter :)
    let $path := replace(replace($path, '^[A-Z]:', ''), '\\', '/')
    
    let $usePath := replace($path, '^/', '')
    let $steps := tokenize($usePath, '/')
    let $filterSteps := $filterPath/*
    return
        m:_matchesNameFilterPathRC($steps, $filterSteps)
};        

(:~
 : Recursive helper function of '_matchesNameFilterPath'.
 :
 : @param steps the path steps to be checked
 : @param filterSteps the filter path steps against which to check
 : @return the boolean check result
 :)
declare function m:_matchesNameFilterPathRC(
                    $steps as xs:string*,
                    $filterSteps as element()*)
        as xs:boolean? {
    if (empty($steps)) then false() else
    
    let $step1 := $steps[1]
    let $filterStep1 := $filterSteps[1]
    let $sep := $filterStep1/@sep
    return
        if ($sep eq '/') then
            if (not(m:matchesNameFilter($step1, $filterStep1))) then false()
            else
                (: 1 path step: matched if only 1 filter step (which matches) :)
                if (count($steps) eq 1) then
                    if (count($filterSteps) eq 1) then true() else false()
                    
                (: >1 path steps :)                    
                else
                    (: only 1 filter step => false :)
                    if (count($filterSteps) eq 1) then false() else
                        m:_matchesNameFilterPathRC(subsequence($steps, 2), subsequence($filterSteps, 2))

        else
            let $matchCandidates :=
                if (count($filterSteps) eq 1) then $steps[last()] 
                    else $steps
            let $nextMatchingStepNr := m:_getNextMatchingStep($matchCandidates, $filterStep1)
            return
                if (empty($nextMatchingStepNr)) then false()
                (: match is the last step => true/false if the filter step was the last one :) 
                else if ($nextMatchingStepNr eq count($matchCandidates)) then 1 eq count($filterSteps)
                (: match is a non-last step :)
                else
                    let $try :=
                        (: no remaining filter steps => false :)                         
                        if (1 eq count($filterSteps)) then false()
                        (: remaining filter steps => continue :)                            
                        else
                            m:_matchesNameFilterPathRC(subsequence($matchCandidates, $nextMatchingStepNr + 1), 
                                subsequence($filterSteps, 2))

                    return                          
                        if ($try) then true() else
                            let $retrySteps :=  subsequence($matchCandidates, 1 + $nextMatchingStepNr)
                            let $retryNameFilters := subsequence($filterSteps, 1)
                            return
                                m:_matchesNameFilterPathRC($retrySteps, $retryNameFilters)
};

declare function m:_getNextMatchingStep($steps as xs:string*, $filterStep as element())
        as xs:integer? {
    m:_getNextMatchingStepRC($steps, $filterStep, 0)        
};

declare function m:_getNextMatchingStepRC($steps as xs:string*, $filterStep as element(), $sofar as xs:integer)
        as xs:integer? {
    if (empty($steps)) then () else
    let $step1 := $steps[1]
    let $sofar := $sofar + 1
    return
        if (m:matchesNameFilter($steps[1], $filterStep)) then $sofar else
            m:_getNextMatchingStepRC(subsequence($steps, 2), $filterStep, $sofar)
};
