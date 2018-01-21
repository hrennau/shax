(:
 : _help - topic tool help
 :
 : @version 2014-02-08T22:22:35+01:00 
 :)
module namespace m="http://www.ttools.org/xquery-functions";
declare namespace z="http://www.ttools.org/structure";

import module namespace tt="http://www.ttools.org/xquery-functions" at
    "_nameFilter.xqm",
    "_request.xqm",
    "_stringTools.xqm";

(:
 : ============================================================================
 :
 :     p u b l i c    f u n c t i o n s
 :
 : ============================================================================
 :)
 
(:
 : ============================================================================
 :
 :     p r i v a t e    f u n c t i o n s
 :
 : ============================================================================
 :)

declare function m:_help($request as element(), 
                         $toolScheme as element())
        as item() {
    let $mode as xs:string := tt:getParam($request, 'mode')
    return
        if ($mode eq 'overview') then m:_helpOverview($request, $toolScheme)
        else if ($mode eq 'scheme') then m:_helpScheme($request, $toolScheme)        
        else m:_helpOverview($request, $toolScheme)
};

(:~
 : Returns a view of the tool scheme (complete or filtered, according
 : to request parameters.
 :) 
declare function m:_helpScheme($request as element(),
                              $toolScheme as element())
        as element() {
    let $nf as element(nameFilter)? := tt:getParam($request, 'ops')
    let $ops := $toolScheme/operations/operation
                    [not($nf) or tt:matchesNameFilter(@name, $nf)]
    return
        element {node-name($toolScheme)} {
            $toolScheme/@*,
            $ops
        }
};        

(:~
 : Returns an overview of the topic tool operations.
 :) 
declare function m:_helpOverview($request as element(),
                                $toolScheme as element())
        as element() {
    let $default as xs:boolean := tt:getParam($request, 'default')
    let $type as xs:boolean := tt:getParam($request, 'type')    
    let $nf as element(nameFilter)? := tt:getParam($request, 'ops')
    let $fill := ' '        
    let $ops := 
        for $op in $toolScheme//operation[not($nf) or m:matchesNameFilter(@name, $nf)]
        order by $op/@name/lower-case(.)
        return $op
    let $opsCount := count($ops)    
    let $opColWidth := 2 + max((10, $ops/@name/string-length(.)))   
    let $opColBlanks := string-join(for $i in 1 to (1 + $opColWidth) return ' ', '')   
        
    let $oplines :=
        for $op at $pos in $ops
        let $name := $op/@name        
        let $paramsInfo := 
            for $p in $op/param
            let $itemTypeCardMinMax := m:_getParamItemTypeCardMinMax($p/@type)
            let $facets :=
                let $items :=            
                    $p/@*[starts-with(local-name(.), 'fct_')]/concat(replace(local-name(.), '^fct_', ''), '=', .)
                return
                    if (empty($items)) then () else
                        attribute facets {concat('facets: ', string-join($items, '; '))}      
            let $sep :=
                if ($p/@sep) then $p/@sep
                else if ($itemTypeCardMinMax[4] lt 0 or $itemTypeCardMinMax[4] gt 1) then attribute sep {'WS'}
                else ()
            order by lower-case($p/@name)
            return
                <param name="{$p/@name}" type="{$p/@type}" 
                    itemType="{$itemTypeCardMinMax[1]}"                
                    card="{$itemTypeCardMinMax[2]}"
                    minOccs="{$itemTypeCardMinMax[3]}"
                    maxOccs="{$itemTypeCardMinMax[4]}">{
                    $sep,
                    $facets, 
                    $p/@default
                }</param>        
        
        let $footnotes :=
            if (not($op/pgroup)) then () else string-join(        
            for $g in $op/pgroup
            let $gname := $g/@name
            let $memberNames :=
                string-join(
                    for $p in $op/param[@pgroup eq $gname] 
                    let $name := $p/@name
                    order by lower-case($name) 
                    return $name
                    , ', ')
            return 
                if ($g/@minOccurs/xs:integer(.) eq $g/@maxOccurs/xs:integer(.)) then
                    concat('Exactly ', $g/@minOccurs, ' of these parameters must be set: ', $memberNames)
                else (                
                    if (not($g/@minOccurs)) then () else
                        concat('At least ', $g/@minOccurs, ' of these parameters must be set: ', $memberNames),
                    if (not($g/@maxOccurs)) then () else
                        concat('At most ', $g/@maxOccurs, ' of these parameters must be set: ', $memberNames)
                )        
            , '&#xA;')
        
        let $params :=
            if (not($type)) then 
                string-join($paramsInfo/concat(@name, @card, @default[$default]/concat('=', .)), ', ')
            else        
                let $paramDescriptors :=
                    for $p in $paramsInfo
                    let $sep := $p/@sep/concat(' (sep=', ., ')')
                    return 
                        concat(                   
                            tt:padRight($p/concat(@name, @default[$default]/concat('=', .)), 20, '.'), ' : ', 
                            $p/@type, $sep, $p/@facets/concat('; ', .))
                                            
                let $lineSep := concat('&#xA;', $opColBlanks)                             
                return                            
                    concat(
                        string-join((
                            $paramDescriptors,
                            if (empty($footnotes)) then () else concat('  ', $footnotes))
                        , $lineSep),
                        if ($pos eq $opsCount) then () else '&#xA;'
                    )
                            
(:                
                concat(
                    string-join(($paramsInfo/concat(                   
                        m:rpad(concat(@name, @default[$default]/concat('=', .)), 20, '.'), ' : ', @type, @facets/concat('; ', .)),
                        if (empty($footnotes)) then () else concat('  ', $footnotes)),
                        concat('&#xA;', $opColBlanks)),
                    if ($pos eq $opsCount) then () else '&#xA;'
                )
:)                
        return
            concat(tt:padRight($name, $opColWidth, $fill), ' ', $params)
    let $sepLine := 
        string-join(
            for $i in 1 to max(for $o in $oplines, $l in tokenize($o, '&#xA;') return string-length($l)) return '='
        , '')                
    let $headlines := (
        '',
        concat('TOOL: ', $toolScheme/@name),
        '',
        concat(tt:padRight('OPERATIONS', $opColWidth, $fill), ' PARAMS'),
        $sepLine
    )            
    let $footlines := ($sepLine, '', '')
       
    let $text := string-join(('',$headlines, $oplines, $footlines), '&#xA;')            
    return
        <z:help mode="overview">{        
            $text
        }</z:help>
};        

(:~
 : Returns for a given type descriptor four items, providing the
 : item type, the cardinality descriptor, the minOccurs value and
 : the maxOccurs value. Note that 'unbounded' is represented by
 : -1.
 :)
declare function m:_getParamItemTypeCardMinMax($type as xs:string?)
        as item()+ {
    if (not($type)) then ("", "", "", "") else
    
    let $itemType := replace($type, '^([\i\c()]+).*', '$1')
    let $card := substring-after($type, $itemType)
    let $minMax :=
        if ($card eq '?') then (0, 1)
        else if (not($card)) then (1, 1)
        else if ($card eq '*') then (0, -1)
        else if ($card eq '+') then (1, -1)
        else
            let $cardItems := tokenize(replace($card, '[{}]', ''), '\s*,\s*')[position() le 2]
            return
                for $cardItem in $cardItems return xs:integer($cardItem)
    return
        ($itemType, $card, $minMax)
};
