(: pfilterParser.xqm - parses a pfilter into a structured representation (pfilter element)
 :
 : @version 20141205-1 first version 
 : ===================================================================================
 :)

module namespace m="http://www.ttools.org/xquery-functions";

import module namespace tt="http://www.ttools.org/xquery-functions" at
    "_constants.xqm",
    "_errorAssistent.xqm",
    "_pcollection_utils.xqm";

declare namespace z="http://www.ttools.org/structure";
declare namespace is="http://www.infospace.org/pcollection";

(:
 : ============================================================================
 :
 :     p u b l i c    f u n c t i o n s
 :
 : ============================================================================
 :)

(:~
 : Determines whether a node descriptor contained by an XML RCAT 
 : matches a pfilter.
 :
 : @param node a node descriptor
 : @param pfilter a pfilter
 : @return true if the node descriptor matches the pfilter or no pfilter has
 :    been specified, false otherwise
 :)
declare function m:matchesPfilter($pnode as element(is:pnode), $pfilter as element(is:pfilter)?)
        as xs:boolean {
    if (not($pfilter)) then true() else
    
    m:_matchesPfilterRC($pnode, $pfilter/*)
};

(:~
 : Transforms a pfilter into a where clause expressing it.
 :
 : @param node a node descriptor
 : @param pfilter a pfilter
 : @return the text of the where clause
 :)
declare function m:pfilterWhereClause($pfilter as element(is:pfilter)?)
        as xs:string? {
    if (not($pfilter)) then () else
    
    m:_pfilterWhereClauseRC($pfilter/*)
};

(:
 : ============================================================================
 :
 :     p r i v a t e    f u n c t i o n s
 :
 : ============================================================================
 :)

(:~
 : Determines whether a node descriptor contained by an XML RCAT 
 : matches a pfilter node.
 :
 : @param node a node descriptor
 : @param pfn a pfilter node
 : @return true if the node descriptor matches the pfilter node, false otherwise
 :)
declare function m:_matchesPfilterRC($pnode as element(is:pnode), $pfn as element())
        as xs:boolean {
    typeswitch ($pfn)
    case element(is:and) return
        every $c in $pfn/* satisfies m:_matchesPfilterRC($pnode, $c)
    case element(is:or) return
        some $c in $pfn/* satisfies m:_matchesPfilterRC($pnode, $c)
    case element(is:not) return
        not(m:_matchesPfilterRC($pnode, $pfn/*))
    case $p as element(is:p) return
        let $pname := $p/@name
        let $op := $p/@op
        let $pvalue := tt:_pnodeProperty($pnode, $pname) 
        (:(
             $pnode/@*[local-name(.) eq $pname], 
             $pnode/*[not(*)][local-name(.) eq $pname]/string(),
             $pnode/*[*][local-name(.) eq $pname]/is:item/string()
        ) :)      
        let $tvalue := if ($p/is:value/is:item) then $p/is:value/is:item/string() else $p/is:value/string()
        let $tvalue :=
            if ($op eq '~') then
                for $v in $tvalue return concat('^', replace($v, '\*', '.*'), '$')
            else $tvalue
        return
            if ($op eq '~') then
                some $pv in $pvalue satisfies (
                    some $tv in $tvalue satisfies matches($pv, $tv, 'i')
                )
            else if ($op eq '=') then $pvalue = $tvalue
            else if ($op eq '<') then $pvalue < $tvalue
            else if ($op eq '<=') then $pvalue <= $tvalue            
            else if ($op eq '>') then $pvalue > $tvalue
            else if ($op eq '>=') then $pvalue >= $tvalue
            else
                error(QName($tt:URI_ERROR, 'INVALID_PFILTER'), 
                    concat('Unexpected operator: ', $op))
    default return
        error(QName($tt:URI_ERROR, 'INVALID_PFILTER'), concat('Unexpected element, local name: ', local-name($pfn)))
};

(:~
 : Transforms a pfilter into a where clause expressing it.
 :
 : @param pfn a pfilter node
 : @return the fragment of the where clause corresponding to the given pfilter node
 :)
declare function m:_pfilterWhereClauseRC($pfn as element())
        as xs:string {
    typeswitch ($pfn)
    case element(is:and) return
        string-join(for $child in $pfn/* return m:_pfilterWhereClauseRC($child), ' and ')
    case element(is:or) return
        let $chain := 
            string-join(for $child in $pfn/* return m:_pfilterWhereClauseRC($child), ' or ')
        return
            if ($pfn/parent::is:pfilter) then $chain else concat('(', $chain, ')')
    case element(is:not) return
        concat('not(', m:_pfilterWhereClauseRC($pfn/*), ')')
    case $p as element(is:p) return
        let $pname := $p/@name
        let $op := $p/@op             
        let $tvalue := if ($p/is:value/is:item) then $p/is:value/is:item/string() else $p/is:value/string()
        let $useOp := 
            if ($op eq '~') then 'like'
            else if ($op eq '=' and count($tvalue) gt 1) then 'in'
            else $op        
        let $useTvalue :=
            let $edited :=
                if (not($op eq '~')) then $tvalue else 
                    for $v in $tvalue return replace($v, '\*', '%')
            let $edited := 
                for $item in $edited return 
                    concat("'", replace($item, '[,)]', '\\$0'), "'")                
            return
                if (count($edited) le 1) then $edited else
                    concat('(', string-join($edited, ', '), ')')
        return
            concat('`', $pname, '` ', $useOp, ' ', $useTvalue) 
    default return
        error(QName($tt:URI_ERROR, 'INVALID_PFILTER'), concat('Unexpected element, local name: ', local-name($pfn)))
};

