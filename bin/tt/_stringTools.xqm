(:~ 
 : _stringTools.xqm - various tools for string processing
 :
 : @version 20140423-1 first version 
 : ===================================================================================
 :)

module namespace m="http://www.ttools.org/xquery-functions";

import module namespace i="http://www.ttools.org/xquery-functions" at
   "_constants.xqm";   

declare namespace z="http://www.ttools.org/structure";

(:
 : ============================================================================
 :
 :     p u b l i c    f u n c t i o n s
 :
 : ============================================================================
 :)
 
 (:~
  : Returns a string consisting of $number repetitions of character $char.
  :
  : @param char the character to be repeated
  : @param number the number of repetitions
  : @return string consisting of repeated characters
  :)
 declare function m:repeatChar($char as xs:string, $number as xs:integer)
        as xs:string {
    string-join(for $i in 1 to $number return $char, '')        
};

(:~
 : Splits a string into items separated by the separator $sep, which is expected 
 : to be a single character. Occurrences $sep which are preceded by a backslash 
 : are not treated as separator. Literal backslash characters and any characters 
 : contained by $unescapeChars are unescaped, that is, within the returned items 
 : their preceding backslash is removed.
 :
 : Usage note. The typical reason for using $unescapeChars is when the same 
 : separator character is re-used on different levels of splitting. Example: request
 : parameter items may be separated by ; characters, and the same character may be
 : used within an item for separating the items of an item modifier. So the following
 : parameter value:
 :    par=foo<a=x y\;z; bar
 :
 : consists of two items, one of which has a modifier (a) with two values ("x y" and
 : "z"). Note that the ; between "x y" and "z" must be escaped, as otherwise the
 : overall splitting at ; would result in three items, rather than two. The items 
 : obtained by splitting at ; are 
 :    foo<a=x y\;z
 :    bar
 :
 : When later the first item is split at < characters, the items obtained by calling
 : this function without setting $unescapeChars will be:
 :    foo
 :    a=x y\;z
 :
 : As further parsing requires the removal of the backslashes, as otherwise the
 : items "x y" and "z" cannot be separated, it is convenient to combine the splitting 
 : at < with unescaping of ;. So the call
 :    tt:splitString('foo<a=x y\;z', '<', ';')
 :
 : will return these items:
 :    foo
 :    a=x y;z
 :
 : @param s the string to be split
 : @param sep the separator at which to split
 : @param trim if true, the items are returned without leading and trailing blanks
 : @param unescapeChars all occurrences of characters contained by this string
 :    are unescaped
 : @return the items
 :) 
declare function m:splitString($s as xs:string?, 
                               $sep as xs:string,
                               $trim as xs:boolean?,
                               $escapedChars as xs:string?)
        as xs:string* {
    for $item in m:_splitStringRC($s, $sep, $trim)   
    let $item := replace($item, concat('\\([\\', $sep, $escapedChars, '])'), '$1')          
    return 
        $item
};

(:~
 : Splits a string into items separated by the separator $sep. The items
 : are returned without preceding or trailing blanks.
 :
 : @param s the string to be split
 : @param sep the separator character
 : @return the items obtained by splitting the string at occurrences of character $sep
 :)
declare function m:splitString($s as xs:string?, $sep as xs:string)
        as xs:string* {
    m:splitString($s, $sep, true(), ())        
};

(:~
 : Concatenates string items using a separator $sep which is expected to be a
 : single character. Any occurrences of $sep within the items is escaped by
 : inserting a backslash character before it. Likewise, any occuurences of the
 : backslash character within the items is escaped by doubling it. If
 : $sepSuffix is used, it will be appended to each separator when concatenating
 : the items.
 :
 : Usage note. %sepSuffix can for example be used to append a blank to the
 : separator.
 :
 : @param items the items to be concatenated
 : @param sep the separator character
 : @param 
 : @return the concatenated string
 :)
declare function m:stringJoin($items as xs:string*, $sep as xs:string, $sepSuffix as xs:string?)
        as xs:string* {
    let $useSep := concat(substring($sep, 1, 1), $sepSuffix)
    return
        string-join(
            for $item in $items return 
                replace(replace($item, $sep, concat('\\', $sep)), '\\', '\\\\') 
            , $useSep)        
};

(:~
 : Splits a text at occurrences of the % character and returns the resulting
 : items with leading and trailing blanks removed. Occurrences of % escaped 
 : by a preceding backslash are treated as literal characters and returned 
 : without the preceding backslash. Likewise, two successive backslashes 
 : are treated as a single escaped backslash character and returned as a 
 : single backslash.
 :
 : Usage note. This function is used for splitting the text of a parameter item
 : into value fields. It may in particular be used when implementing the
 : parsing of custom data types whose syntax uses text fields.
 :
 : @param item the text of an item value
 : @return the fields of the string
 :)
declare function m:getTextFields($item as xs:string) 
        as xs:string* {
    m:splitString($item, '%', true(), ())
};

(:~
 : Merges text fields into a structured string. Field separator
 : is a `%` character. 
 :
 : Note. The text field strings are modified by escaping
 : the following characters by a leading `\`: `\` and `%`.
 :
 : @param fields the strings to be turned into text fields
 : @return a structured string composed of text fields
 :) 
declare function m:composeTextFields($fields as xs:string*)
        as xs:string {
    string-join(
        for $f in $fields return replace($f, '([\\%])', '\\$1')
    , '%')        
};

(:~
 : Parses a text into a sequence of name/value pairs.
 :
 : @param text the text to be parsed
 : @param entryName the element name used for elements representing a map entry
 : @param keyName the attribute name used for attributes representing a map key
 : @param valueName the attribute name used for attributes representing a map entry value
 : @return a sequence of elements representing the map entries
 :)
declare function m:getTextMap($text as xs:string?,
                              $entryName as xs:string?,
                              $keyName as xs:string?,
                              $valueName as xs:string?)
        as element()* {
    if (not($text)) then () else
    
    let $keyValueSep := '='
    let $entryName := ($entryName, 'entry')[1]    
    let $keyName := ($keyName, 'key')[1]        
    let $valueName := ($valueName, 'value')[1]
    let $fields := m:getTextFields($text)
    for $field in $fields
    let $key := m:trim(substring-before($field, $keyValueSep))     
    let $value := m:trim(substring-after($field, $keyValueSep))
    return    
        element {$entryName} {
            attribute {$keyName} {$key},
            attribute {$valueName} {$value}
        }            
};        

(:~
 : Transforms a sequence of text items into a sequence of name/operator/value triples 
 : represented by elements and their attributes.
 :
 :
 : Example: 
 :    $text:        'a=1 % b~=foo*'
 :    $ops:         '= ~='
 :    $entryElem:   'col'
 :    $nameAtt:     'name'
 :    $opAtt:       'op'
 :    $valueAtt:    'value'
 :
 : =>
 :    <col name="a" op="=" value="1"/>
 :    <col name="b" op="~?" value="foo*"/>
 :
 : Note the following default values:
 :    entryElem: 'entry'
 :    nameAtt:   'name'
 :    opAtt:     'op'
 :    valueAtt:  'value'
 :
 : @param text the text to be parsed
 : @param entryElem the element name used for elements representing a triple
 : @param nameAtt the attribute name used for attributes representing a name
 : @param opAtt the attribute name used for attributes representing an operator 
 : @param valueAtt the attribute name used for attributes representing a value
 : @return a sequence of elements representing the name/operator/value entries
 :)
declare function m:getNameOpValueTriples($items as xs:string*,
                                         $ops as xs:string,
                                         $entryElem as xs:string?,
                                         $nameAtt as xs:string?,
                                         $opAtt as xs:string?,
                                         $valueAtt as xs:string?)
        as element()* {
    if (empty($items)) then () else
    
    let $uuc := m:getNotUsedChar(string-join($items, ''))
    let $ops :=
        let $unsorted :=tokenize(normalize-space($ops), ' ')
        for $op in $unsorted order by string-length($op) descending return $op
    let $opsExpr := concat('\s*(', string-join($ops, '|'), ')\s*')
    let $fieldExprIn := concat('(.*?)', $opsExpr, '(.*)')
    let $fieldExprOut := concat('$1', $uuc, '$2', $uuc, '$3')
    
    let $entryElem := ($entryElem, 'entry')[1]    
    let $nameAtt := ($nameAtt, 'name')[1]        
    let $opAtt := ($opAtt, 'op')[1]    
    let $valueAtt := ($valueAtt, 'value')[1]
    
    for $item in $items
    let $parts := tokenize(replace($item, $fieldExprIn, $fieldExprOut), $uuc)
    return    
        element {$entryElem} {
            attribute {$nameAtt} {$parts[1]},
            attribute {$opAtt} {$parts[2]},            
            attribute {$valueAtt} {$parts[3]}
        }            
};

(:~
 : Parses a text into a sequence of name/operator/value triples.
 :
 :
 : Example: 
 :    $text:        'a=1 % b~=foo*'
 :    $ops:         '= ~='
 :    $entryElem:   'col'
 :    $nameAtt:     'name'
 :    $opAtt:       'op'
 :    $valueAtt:    'value'
 :
 : =>
 :    <col name="a" op="=" value="1"/>
 :    <col name="b" op="~?" value="foo*"/>
 :
 : Note the following default values:
 :    entryElem: 'entry'
 :    nameAtt:   'name'
 :    opAtt:     'op'
 :    valueAtt:  'value'
 :
 : @param text the text to be parsed
 : @param entryElem the element name used for elements representing a triple
 : @param nameAtt the attribute name used for attributes representing a name
 : @param opAtt the attribute name used for attributes representing an operator 
 : @param valueAtt the attribute name used for attributes representing a value
 : @return a sequence of elements representing the name/operator/value entries
 :)
declare function m:getTextNameOpValueTriples($text as xs:string?,
                                             $ops as xs:string,
                                             $entryElem as xs:string?,
                                             $nameAtt as xs:string?,
                                             $opAtt as xs:string?,
                                             $valueAtt as xs:string?)
        as element()* {
    if (not($text)) then () else
    
    let $uuc := m:getNotUsedChar($text)
    let $ops := trace(
        let $unsorted :=tokenize(normalize-space($ops), ' ')
        for $op in $unsorted order by string-length($op) descending return $op , 'OPS: ')
    let $opsExpr := concat('(', string-join($ops, '|'), ')')
    let $fieldExprIn := concat('(.*?)', $opsExpr, '(.*)')
    let $fieldExprOut := concat('$1', $uuc, '$2', $uuc, '$3')
    
    let $entryElem := ($entryElem, 'entry')[1]    
    let $nameAtt := ($nameAtt, 'name')[1]        
    let $opAtt := ($opAtt, 'op')[1]    
    let $valueAtt := ($valueAtt, 'value')[1]
    
    let $fields := m:getTextFields($text)
    for $field in $fields
    let $parts := tokenize(replace($field, $fieldExprIn, $fieldExprOut), $uuc)
    return    
        element {$entryElem} {
            attribute {$nameAtt} {$parts[1]},
            attribute {$opAtt} {$parts[2]},            
            attribute {$valueAtt} {$parts[3]}
        }            
};        

(:~
 : Edits a sequence of strings removing prefixes as specified. If
 : a $prefixTerminator is specified, any prefix not ending with this
 : string is edited before use, appending the terminator.
 :
 : @param strings the strings to be edited
 : @param prefixes the prefixes to be removed
 : @param prefixTerminator if specified, any prefix not ending with this
 :    string is edited before use by appending this string
 : @return the edited names
 :)
declare function m:removeStringPrefixes($strings as xs:string*, 
                                        $prefixes as xs:string*,
                                        $prefixTerminator as xs:string?)
        as xs:string* {
        
    (: prefix editing: add trailing terminator, if not already present :)
    let $prefixes := 
        if (empty($prefixTerminator)) then $prefixes else
        for $p in $prefixes 
        return
            if (ends-with($p, $prefixTerminator)) then $p 
            else concat($p, $prefixTerminator)
            
    for $s in $strings
    let $matchingPrefixes := $prefixes[starts-with($s, .)]
    return
        if (empty($matchingPrefixes)) then $s
        else
            let $matchingPrefix :=
                if (count($matchingPrefixes) gt 1) then
                    let $maxLen := max(for $p in $matchingPrefixes return string-length($p))
                    return $matchingPrefixes[string-length(.) eq $maxLen][1]
                else $matchingPrefixes
        return
            substring-after($s, $matchingPrefix)
};

(:~
 : Returns for a given name the normalized name(s) which the given name matches. 
 : The matches are determined as follows: (a) if there are candidate names which
 : are equal to the given name, ignoring case - the first or these candidate names;
 : (b) otherwise - all candidate names which start with the given name, ignoring case.
 : 
 : @param rawName the name as specified in the original request data
 : @return the matching name(s)
 :)
declare function m:getMatchingNames($rawName as xs:string, $candidateNames as xs:string*)
        as xs:string* {
    if (empty($candidateNames)) then () else
    
    let $exactMatch := $candidateNames[matches($rawName, concat('^', ., '$'), 'i')][1]
    return
        if (exists($exactMatch)) then $exactMatch
        else
            let $rawPattern := concat('^', $rawName)
            let $matches := $candidateNames[matches(., $rawPattern, 'i')]
            return
                $matches
};

(:~
 : Removes from a string all leading and trailing whitespace.
 :
 : @param s the string to be trimmed
 : @return the trimmed string
 :)
declare function m:trim($s as xs:string?)
        as xs:string? {
    if (empty($s)) then () else replace($s, '^\s+|\s+$', '')         
};

(:~
 : Escapes every character found in $replaceChars by a preceding backslash.
 : Also replaces every backslash by a coupld of backslashes.
 :
 : @param s the string to be escaped
 : @param replaceChars a string containing the characters to be replaced
 : @return the escaped string
 :)
declare function m:escapeString($s as xs:string, $replaceChars as xs:string)
        as xs:string {
    replace($s, concat('([\\', $replaceChars, '])'), '\\$1')   
};        

(:~
 : Unescapes a string, replacing every sequence of a backslash and the
 : character following it by the latter.
 :)
declare function m:unescapeString($s as xs:string)
        as xs:string {
    replace($s, '\\(.)', '$1')        
    (: replace(replace($s, '\\\\', '\\'), '\\([^\\])', '$1') :)        
};

(:~
 : Escapes the replacement string of a replace function call.
 : 
 : @param s the replacement string to be escaped
 : @return the escaped replacement string
 :)
 
declare function m:escapeReplacementString($s as xs:string)
        as xs:string {
    replace(replace($s, '\\', '\\\\'), '\$', '\\\$')    
};        

declare function m:normalizeLinefeed($text as xs:string?)
        as xs:string? {
    replace($text, '&#xD;&#xA;', '&#xA;')        
};

(:~
 : Pads a string by adding trailing fill characters.
 :
 : @param name s the string
 : @param width the desired string length
 : @param fill the fill character
 : @return the padded string
 :)
declare function m:padRight($s as xs:string?, $width as xs:integer, $fill as xs:string)
        as xs:string? {
    let $len := string-length($s)
    return
        if ($len ge $width) then $s else
            concat($s, string-join(for $i in 1 to $width - $len return $fill, ''))
};

(:~
 : Pads a string by adding trailing blanks.
 :
 : @param name s the string
 : @param width the desired string length
 : @return the padded string
 :)
declare function m:padRight($s as xs:string?, $width as xs:integer)
        as xs:string {
    m:padRight($s, $width, ' ')        
};

(:~
 : Pads a string by inserting preceding fill characters.
 :
 : @param name s the string
 : @param width the desired string length
 : @param fill the fill character
 : @return the padded string
 :)
declare function m:padLeft($s as xs:string?, $width as xs:integer, $fill as xs:string)
        as xs:string? {
    let $len := string-length($s)
    return
        if ($len ge $width) then $s else
            concat(string-join(for $i in 1 to $width - $len return $fill, ''), $s)
};

(:~
 : Pads a string by inserting preceding blanks.
 :
 : @param name s the string
 : @param width the desired string length
 : @return the padded string
 :)
declare function m:padLeft($s as xs:string?, $width as xs:integer)
        as xs:string {
    m:padLeft($s, $width, ' ')        
};

(:~
 : Returns a character not used within string $s. The result is the first character
 : not used within $s with a codepoint >= 30000. If a different starting point for
 : searching an unused character is desired - for example because $s contains Chinese
 : text - use the other variant of this function which lets you specify at which 
 : codepoint to start the search for an unused character.
 :
 : @param s the string for which to return an unused character
 : @param tryCodepoint a codepoint to try
 : @return a character not used within $s
 :)
declare function m:getNotUsedChar($s as xs:string)
        as xs:string {
    m:getNotUsedChar($s, 30000)        
};

(:~
 : Returns a character not used within string $s. The result is the first character
 : not used within $s with a codepoint >= $tryCodepoint. If $tryCodepoint is not
 : specified, the value 30000 is assumed.
 :
 : @param s the string for which to return an unused character
 : @param tryCodepoint a codepoint to try
 : @return a character not used within $s
 :)
declare function m:getNotUsedChar($s as xs:string, $tryCodepoint as xs:integer?)
        as xs:string {
    let $tryCodepoint := if (exists($tryCodepoint)) then $tryCodepoint else 30000
    let $char := codepoints-to-string($tryCodepoint)
    return
        if (not(matches($s, $char))) then $char else m:getNotUsedChar($s, $tryCodepoint + 1)
};        

(:
 : ============================================================================
 :
 :     p r i v a t e    f u n c t i o n s
 :
 : ============================================================================
 :)

(:~
 : Recursive helper function of _splitString. Note that the returned items
 : contain the original text - doubled backslashes are not yet replaced by
 : single backslashes, and separators preceded by backslash or not yet replaced
 : by the separator itself.
 :
 : @param s the string to be split
 : @param sep the separator character
 : @param trim if true, the items are returned without leading and trailing blanks 
 : @return the items
 :) 
declare function m:_splitStringRC($s as xs:string?, $sep as xs:string, $trim as xs:boolean?)
        as xs:string* {
    if (empty($s)) then () else
    
    (: the next item preceding a separator, or the whole string, if there is no separator;
       effective separators must not be preceded by an even number of backslashes
             (\\\\)*
       An even number of backslashes is either occurring at the beginning of the string, or
       it is preceded by a substring ending with a non-backslash character: 
             (.*?[^\\])?
    :)
    let $item := replace($s, concat('^ ( (.*?[^\\])? (\\\\)* )?', $sep, '.*'), '$1', 'sx')    
    return 
        if ($item eq $s) then 
            if ($trim) then replace($s, '^\s+|\s+$', '') else $s
        else    
            let $next := substring($s, string-length($item) + 2)
            return (
                if ($trim) then replace($item, '^\s+|\s+$', '') else $item,
                if (not(string($next))) then () else m:_splitStringRC($next, $sep, $trim)
            )            
};        
