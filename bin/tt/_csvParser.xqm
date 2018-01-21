(: csvParser.xqm - parses csv and turns it into xml
 :
 : @version 20140110-1 
 : ===================================================================================
 :)

module namespace f="http://www.ttools.org/xquery-functions";
import module namespace tt="http://www.ttools.org/xquery-functions" at
    "_constants.xqm",
    "_request.xqm",    
    "_resourceAccess.xqm"    
    ;    

(:
 : ============================================================================
 :
 :     p u b l i c    f u n c t i o n s
 :
 : ============================================================================
 :)

(:~
 : Parses a text file as CSV document and transforms it into
 : an XML representation.
 :)
declare function f:parseCsv($uri as xs:string,
                            $encoding as xs:string?,
                            $sep as xs:string?,
                            $delim as xs:string?,
                            $header as xs:boolean?,
                            $names as xs:string*,
                            $fromRec as xs:integer?,
                            $toRec as xs:integer?)
        as element() {
    let $sep := ($sep, ',')[1]
    let $encoding := ($encoding, 'ISO-8859-1')[1]    
    
    let $defaultNames := ('table', 'row', 'cell')        
    let $useNames := 
        if (count($names) ge 3) then $names
        else 
            ($names, subsequence($defaultNames, 1 + count($names)))
    let $namesString := string-join($useNames, $sep)
        
    let $control :=
        <control uri="{$uri}"
                 encoding="{$encoding}"
                 sep="{$sep}"
                 delim="{($delim, '"')[1]}"
                 header="{$header}"                 
                 names="{$namesString}"                 
                 fromRec="{($fromRec, '1')[1]}"                 
                 toRec="{($toRec, '0')[1]}"/>
    let $lines := tt:unparsed-text-lines($uri, $encoding)[string(.)]   
    return
        element {$useNames[1]} {
            f:_getCsvRecordsRC(1, 1, $lines, $control, ())        
        }
};

(:
 : ============================================================================
 :
 :     p r i v a t e    f u n c t i o n s
 :
 : ============================================================================
 :)

(:~
 : csvParser.xqm - parses csv and turns it into xml
 :
 : The csv items are separated by $sep and may be delimited
 : by $delimit. If delimited, the item may contain character
 : $sep, and any occurrences of $delim are doubled.
 :
 : Note. The query does not support the use of end of line characters
 : within data items.
 : 
 : @param uri uri of the csv file
 : @param sep separator used (default: ;)
 : @param delim delimiter used if an item contains the separator or the delimiter itself
 :              (default: &quot;)
 : @param names the names of wrapper elem, row elem, column elems
 :
 : @version 2014-01-09-a
 :)

declare function f:_getCsvRecordsRC($recordNr as xs:integer,
                                    $lineNr as xs:integer,
                                    $lines as xs:string*,
                                    $control as element(control),
                                    $accum as element()*) 
        as element()* {
    let $toRec as xs:integer := $control/@toRec/xs:integer(.) return        
    if ($toRec gt 0 and $recordNr gt $toRec) then $accum 
    else if ($lineNr gt count($lines)) then $accum else

    let $sep := $control/@sep
    let $delim := $control/@delim  
    let $names := tokenize($control/@names, concat($sep, '\s*'))
    let $fromRec as xs:integer := $control/@fromRec/xs:integer(.)    
    let $recNrAtt as xs:string? := $control/@recNrAtt/string()    
    let $rowElem := ($names[2], 'row')[1]
    let $colElem := 'col'

    let $recordText := $lines[$lineNr]
    let $itemsAndUpdatedLineNr := f:_getCsvItems($recordText, $lines, $lineNr, $sep, $delim)
    let $items := $itemsAndUpdatedLineNr[position() lt last()]
    let $updatedLineNr := $itemsAndUpdatedLineNr[last()]
    let $control :=
        if ($recordNr ne 1 or not($control/@header eq 'true')) then $control
        else trace(
            let $useNames := for $item in $items return replace($item, '\s', '_')
            return
            <control>{
                attribute names {string-join((subsequence($names, 1, 2), $useNames), $sep)},
                $control/(@* except @names)
            }</control> , 'NEW_CONTROL: ')
    let $record :=
        if ($fromRec gt 0 and $recordNr lt $fromRec) then ()
        else if ($recordNr eq 1 and $control/@header eq 'true') then () 
        else        
            element {$rowElem} {
                if (not($recNrAtt)) then () else attribute {$recNrAtt} {$recordNr},
                for $cell at $nr in $items return
                    element {($names[2 + $nr], $colElem)[1]} {$cell}                    
            }
    return
        f:_getCsvRecordsRC($recordNr + 1, $updatedLineNr + 1, $lines, $control, ($accum, $record))            
};

declare function f:_getCsvItems($recordText as xs:string,
                                $lines as xs:string+,
                                $lineNr as xs:integer,
                                $sep as xs:string,
                                $delim as xs:string)
        as item()* {
    let $delimited := substring($recordText, 1, 1) eq $delim
    let $rawItem := 
        if (not($delimited)) then replace($recordText, concat($sep, '.*'), '') 
        else
            let $notDelim := concat('[^', $delim, ']')
            let $value :=
                replace($recordText, concat('^(', $delim, '(|.*?', $notDelim, ')', 
                   '(', $delim, $delim, ')*', $delim, ')($|', $notDelim, '.*)'), '$1')
            return
                if ($value ne $recordText) then $value 
                else
                    (: either no match, or recordText = single item -> check! :)
                    if (matches($recordText, concat('^', $delim, '(|.*?[^', $delim, '])(', $delim, $delim, ')*', $delim, '$')))                   
                        then $value
                    else
                        let $updatedRecordTextAndLineNr :=
                            f:_expandRecordText($recordText, $lines, $lineNr, $delim)
                        let $updatedRecordText := $updatedRecordTextAndLineNr[1]
                        let $updatedLineNr := $updatedRecordTextAndLineNr[2]
                        let $value :=
                            replace($updatedRecordText, concat('(', $delim, '(.*?[^', $delim, ']|)', 
                                '(', $delim, $delim, ')*', $delim, ')[^', $delim, '].*'), '$1', 's')
                        return
                            ($value, $updatedRecordText, $updatedLineNr)
  
    let $recordText := ($rawItem[2], $recordText)[1]
    let $lineNr := ($rawItem[3], $lineNr)[1]
    let $rawItem := $rawItem[1]    
    let $rawItemLength := string-length($rawItem)
    let $remainder := substring($recordText, 2 + $rawItemLength)
    let $item :=
        if (not($delimited)) then $rawItem else 
            replace(substring($rawItem, 2, string-length($rawItem) - 2), concat($delim, $delim), $delim)
    return (
        $item,

        if (not($remainder)) then $lineNr  (: end of record text -> return updated lineNr :)
        else if ($delimited and not(substring($recordText, 1 + $rawItemLength, 1) = $sep)) then
            error(QName((), 'INVALID_CSV'), concat('Invalid data encountered; closing delimiter ''', $delim, 
               ''' must be followed by separater ''', $sep, '''; found: ', $recordText))
        else (: next item within line :)
            f:_getCsvItems($remainder, $lines, $lineNr, $sep, $delim)
    )
};

(:~
 : Expands the record text by as many lines as necessary in order to find the end of the current
 : item which is expected to be delimited, starting within the current value of $recordText
 : and ending in the next line or a later line. The line in which the current item ends is
 : the first line after the current line, which contains a substring consisting of an uneven 
 : number of delimiter characters which is neither preceded nor followed by the delimiter 
 : character.
 :
 : @recordText the incomplete record text within which a delimited item begins, but is not
 :    completed
 : @lines the lines of the csv text
 : @lineNr the number of the last line which has been incorporated into $recordText
 : @delim the delimiter character 
 : @returns two items, the first being the new $recordText and the second the number of the line
 :    following the last line used to expand the current $record Text (in other words: the
 :    new value of $lineNr)
 :)
declare function f:_expandRecordText($recordText as xs:string, $lines as xs:string+, $lineNr as xs:integer,
                                     $delim as xs:string)
        as item()+ {
    let $nextLineNr := $lineNr + 1    
    let $nextLine := $lines[$nextLineNr]
    let $recordText := concat($recordText, '&#xA;', $nextLine)
    let $notDelim := concat('[^', $delim, ']')    
    let $matches := matches($recordText, 
        concat('(^|', $notDelim, ')(', $delim, $delim, ')*', $delim, '($|', $notDelim, ')'))
    return
        if ($matches) then ($recordText, $nextLineNr) else
            f:_expandRecordText($recordText, $lines, $nextLineNr, $delim)
};        
