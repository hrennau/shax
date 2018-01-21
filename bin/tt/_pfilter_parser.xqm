(:
 :***************************************************************************
 :
 : pfilter_parser.xqm - parses a pfilter into a structured representation (pfilter element)
 :
 : @version 20141205-1 first version 
 :***************************************************************************
 :)

module namespace m="http://www.ttools.org/xquery-functions";

import module namespace i="http://www.ttools.org/xquery-functions" at
    "_constants.xqm",
    "_errorAssistent.xqm";

declare namespace z="http://www.ttools.org/structure";

(:
 : ============================================================================
 :
 :     p u b l i c    f u n c t i o n s
 :
 : ============================================================================
 :)

(:~
 : Parses a pfilter.
 :
 : @param text the text to be parsed
 : @return a structured representation of the pfilter
 :)
declare function m:parsePfilter($text as xs:string?)
        as element()? {
    if (not($text)) then () else
    
    let $pfilterEtc := m:_parseOrExpr($text)
    let $pfilter := $pfilterEtc[. instance of node()]
    let $textAfter := $pfilterEtc[not(. instance of node())][string()]
    return
        if ($textAfter) then
            i:createError('INVALID_PFILTER', 
                concat('Unexpected trailing text: ', $textAfter), ())
        else
            element {QName($i:URI_PCOLLECTION, 'pfilter')} {$pfilter}        
};

(:
 : ============================================================================
 :
 :     p r i v a t e    f u n c t i o n s
 :
 : ============================================================================
 :)

(:~
 : Parses an OrExpr.
 :
 : @param text the text to be parsed
 : @return a structured representation of the Or expression,
 :    followed by the text not yet parsed.
 :)
declare function m:_parseOrExpr($text as xs:string)
        as item()+ {
    let $orEtc := m:_parseOrExprRC($text)
    let $orNodes := $orEtc[. instance of node()]
    let $textAfter := $orEtc[not(. instance of node())]
    return (
        if (count($orNodes) lt 2) then $orNodes else
            element {QName($i:URI_PCOLLECTION, 'or')} {$orNodes},
        $textAfter
    )        
};

(:~
 : Recursive helper function of '_parseOrExpr'.
 :
 : @param text the text to be parsed
 : @return a structured representation the remaining items of
 :    the and expression, followed by the text not yet parsed
 :)
declare function m:_parseOrExprRC($text as xs:string)
        as item()+ {
    let $andEtc := m:_parseAndExpr($text)
    let $and := $andEtc[. instance of node()]
    let $textAfter := replace($andEtc[not(. instance of node())], '^\s+', '')
    return (
        $and,        
        if (not(starts-with($textAfter, '||'))) then $textAfter else
            m:_parseOrExprRC(substring($textAfter, 3))
    )        
};

(:~
 : Parses an AndExpr.
 :
 : @param text the text to be parsed
 : @return a structured representation of the And expression,
 :    followed by the text not yet parsed.
 :)
declare function m:_parseAndExpr($text as xs:string)
        as item()+ {
    let $andEtc := m:_parseAndExprRC($text)
    let $andNodes := $andEtc[. instance of node()]
    let $textAfter := $andEtc[not(. instance of node())]    
    return (
        if (count($andNodes) lt 2) then $andNodes else
            element {QName($i:URI_PCOLLECTION, 'and')} {$andNodes},
        $textAfter
    )        
};

(:~
 : Recursive helper function of '_parseAndExpr'.
 :
 : @param text the text to be parsed
 : @return a structured representation the remaining items of
 :    the and expression, followed by the text not yet parsed
 :)
declare function m:_parseAndExprRC($text as xs:string)
        as item()+ {
    let $particleEtc := m:_parseParticle($text)
    let $particle := $particleEtc[. instance of node()]
    let $textAfter := replace($particleEtc[not(. instance of node())], '^\s+', '')
    return (
        $particle,        
        if (not(starts-with($textAfter, '&amp;&amp;'))) then $textAfter 
            else m:_parseAndExprRC(substring($textAfter, 3))
    )        
};
 
(:~
 : Parses a particle, which is either a parenthesized expression, or
 : a not expression, or a ptest.
 :
 : @param text the text to be parsed
 : @return a structured representation of the particule, followed
 :    by the text not yet parsed
 :)
declare function m:_parseParticle($text as xs:string)
        as item()+ {
    let $useText := replace($text, '^\s+', '')
    return
        if (starts-with($useText, '(')) then m:_parseParenthesizedExpr($useText)
        else if (matches($useText, '^not\s*\(')) then m:_parseNotExpr($useText)
        else m:_parsePtest($useText)
};

(:~
 : Parses a parenthesized expression.
 :
 : @param text the text to be parsed
 : @return a structured representation of the parenthesized expression,
 :    followed by the text not yet parsed
 :)
declare function m:_parseParenthesizedExpr($text as xs:string)
        as item()+ {
    let $useText := replace($text, '^\s*\(', '')  
    let $orEtc := m:_parseOrExpr($useText)
    let $or := $orEtc[. instance of node()]
    let $textAfter := replace($orEtc[not(. instance of node())], '^\s+', '')
    return
        if (not(starts-with($textAfter, ')'))) then
            i:createError('PFILTER_SYNTAX_ERROR', 'Unbalanced parentheses', ())
        else (
            $or,
            substring($textAfter, 2)            
        )
};

(:~
 : Parses a not expression.
 :
 : @param text the text to be parsed
 : @return a structured representation of the not expression,
 :    followed by the text not yet parsed
 :)
declare function m:_parseNotExpr($text as xs:string)
        as item()+ { 
    let $useText := replace($text, '^\s*not\s*\(', '')
    let $orEtc := m:_parseOrExpr($useText)
    let $or := $orEtc[. instance of node()]
    let $textAfter := replace($orEtc[not(. instance of node())], '^\s+', '')
    return
        if (not(starts-with($textAfter, ')'))) then
            i:createError('PFILTER_SYNTAX_ERROR', 'Unbalanced parentheses', ())
        else (
            element {QName($i:URI_PCOLLECTION, 'not')} {$or},
            substring($textAfter, 2)
        )            
};

(:~
 : Parses a p-test consisting of a property name, an operator and a
 : test value which may be a single item or a list of one or more items.
 : A list of items is comma-separated and delimited by parentheses.
 :
 : Precondition: the received text starts with the first character of
 : the property name (optionally preceded by whitespace).
 :
 : @param text the text to be parsed
 : @return structured representation of the p-test, followed by the 
 : remaining text not yet parsed
 :)
declare function m:_parsePtest($text as xs:string)
        as item()+ {
    let $pname := replace($text, '^(\s*\i\c*).*', '$1')
    let $op := replace(substring($text, 1 + string-length($pname)), '^(\s*[=~>&lt;]+).*', '$1')
    let $valueEtc := 
        let $text := substring($text, 1 + string-length($pname) + string-length($op))
        return
            m:_parseValue($text)
    return (
        element {QName($i:URI_PCOLLECTION, 'p')} {
            attribute name {replace($pname, '^\s+', '')},
            attribute op {replace($op, '^\s+', '')},
            $valueEtc[. instance of node()]
        },
        $valueEtc[not(. instance of node())]
    )
};        

(:~
 : Parses a test value which may by a single item or a list of one or more 
 : items. A list of items is comma-separated and delimited by parentheses.
 :
 : @param text the text to be parsed
 : @return a structured representation of the value, followed by 
 :    the remaining text not yet parsed
 :)
declare function m:_parseValue($text as xs:string)
        as item()+ {
    let $useText := replace($text, '^\s+', '')
    return
        if (starts-with($useText, '(')) then m:_parseValueItemList($useText)
        else m:_parseSimpleValue($useText)
};

(:~
 : Parses a test value consisting of (possibly) multiple items. The value
 : is a comma-separated list of value items, delimited by parentheses.
 :
 : Precondition: the received text starts with (
 :
 : @param text the text to be parsed
 : @return structured representation of the value list (items element 
 :    with item child elements), followed by the remaining text not yet 
 :    parsed
 :)
declare function m:_parseValueItemList($text as xs:string)
        as item()+ {
    let $itemsEtc := m:_parseValueItemListRC(substring($text, 2))
    return (
        element {QName($i:URI_PCOLLECTION, 'value')} 
            {$itemsEtc[. instance of node()]},
        $itemsEtc[not(. instance of node())]
    )        
};        

(:~
 : Recursive helper function of '_parseValueItemList'.
 :
 : @param text the text to be parsed
 : @return an 'item' element containing the next value item,
 :    and the remainder of the input string not yet parsed
 :)
declare function m:_parseValueItemListRC($text as xs:string)
        as item()+ {
    let $item := replace($text, concat(
        '^(',
        '( [^,)\\] | \\[,)\\] )+',     (: all chars except ,)\, or escaped ,)\ :)
        ').*'), '$1', 'x')
    let $textAfter := substring($text, 1 + string-length($item))    
    let $useItem := replace(replace($item, '^\s+|\s+$', ''), '\\([,)\\])', '$1')    
    return (
        element {QName($i:URI_PCOLLECTION, 'item')} {$useItem},
        if (starts-with($textAfter, ')')) then substring($textAfter, 2)
        else if (starts-with($textAfter, ',')) then
            m:_parseValueItemListRC(substring($textAfter, 2))
        else i:createError('INVALID_PFILTER_STRING', 'Invalid value list', ())
    )            
};        

(:~
 : Parses a test value consisting of a single item. Such a value 
 : is distinguished from test values which may containn multiple items
 : by not starting with an opening parenthesis.
 :
 : Precondition: the input string starts with the test value to be
 : parsed.
 :
 : @param text the text to be parsed
 : @return a 'value' element containing the value and a string containing
 :    the remainder of the input text not yet parsed
 :)
declare function m:_parseSimpleValue($text as xs:string)
        as item()+ {
    let $item := replace($text, concat(
        '^(',
        '( [^&amp;|)\\] | \\[&amp;|)\\] )+',     (: all chars except &|Â§\, or escaped &|Â§\ :)
        ').*'), '$1', 'x')
    let $textAfter := substring($text, 1 + string-length($item))    
    let $useItem := replace(replace($item, '^\s+|\s+$', ''), '\\([&amp;|)\\])', '$1')    
    return (
        element {QName($i:URI_PCOLLECTION, 'value')} {$useItem},
        $textAfter
    )            
};        
