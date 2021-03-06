module namespace f="http://www.ttools.org/xquery-functions";
import module namespace i="http://www.ttools.org/xquery-functions" at 
    "_foxpath-processorDependent.xqm",
    "_foxpath-uri-operations.xqm",
    "_foxpath-util.xqm";

(:~
 : Foxpath function `bslash#1'. Edits a text, replacing forward slashes by 
 : back slashes.
 :
 : @param arg text to be edited
 : @return edited text
 :)
declare function f:foxfunc_bslash($arg as xs:string?)
        as xs:string? {
    replace($arg, '/', '\\')        
};      

(:~
 : Foxpath function `file-content#1'. Edits a text, replacing forward slashes by 
 : back slashes.
 :
 : @param arg text to be edited
 : @return edited text
 :)
declare function f:foxfunc_file-content($uri as xs:string?, 
                                        $encoding as xs:string?,
                                        $options as map(*)?)
        as xs:string? {
    let $redirectedRetrieval := f:fox-unparsed-text_github($uri, $encoding, $options)
    return
        if ($redirectedRetrieval) then $redirectedRetrieval
        else i:fox-unparsed-text($uri, $encoding, $options)
};      

(:~
 : Foxpath function `repeat#2'. Creates a string which is the concatenation of
 : a given number of instances of a given string.
 :
 : @param string the string to be repeated
 : @param count the number of repeats
 : @return the result of repeating the string
 :)
declare function f:foxfunc_repeat($string as xs:string?, $count as xs:integer?)
        as xs:string {
    string-join(for $i in 1 to $count return $string, '')
};      

(:~
 : Writes a collection of files into a folder.
 :
 : @param files the file URIs
 : @param dir the folder into which to write
 : @return 0 if no errors were observed, 1 otherwise
 :)
declare function f:foxfunc_write-files($files as item()*, 
                                       $dir as xs:string?,
                                       $encoding as xs:string?)
        as xs:integer {
    let $tocItems :=        
        for $file at $pos in $files
        let $file := 
            if ($file instance of attribute()) then string($file) else $file
        let $path :=
            if ($file instance of node()) then 
                let $raw := $file/root()/document-uri(.)
                return if ($raw) then $raw else concat('__file__', $pos)
            else $file        
        let $fname := replace($path, '^.+/', '')
        group by $fname
        return
            if (count($file) eq 1) then 
                <file name="{$fname}" path="{$path}"/>
            else
                <files originalName="{$fname}" count="{count($file)}">{
                    let $prePostfix := replace($fname, '(.+)(\.[^.]*$)', '$1~~~$2')
                    let $pre := substring-before($prePostfix, '~~~')
                    let $post := substring-after($prePostfix, '~~~')
                    for $f at $pos in $file
                    let $hereName := if ($pos eq 1) then $fname else concat($pre, '___', $pos, '___', $post)
                    return
                        <file originalName="{$fname}" name="{$hereName}" path="{$f}"/>
                }</files> 
    let $toc := <toc countFnames="{count($tocItems)}" countFiles="{count($files)}">{$tocItems}</toc>
    let $tocFname := concat($dir, '/', '___toc.write-files.xml')
    let $_ := file:write($tocFname, $toc)
    
    let $errors :=
        for $file at $pos in $files
        let $file := 
            if ($file instance of attribute()) then string($file) else $file
        let $path :=
            if ($file instance of node()) then 
                let $raw := $file/root()/document-uri(.)
                return if ($raw) then $raw else concat('__file__', $pos)
            else $file   
        let $fname := $toc//file[@path eq $path]/@name/string()
        let $fname_ := string-join(($dir, $fname), '/')        
        let $fileContent := 
            if ($file instance of node()) then serialize($file)
            else f:fox-unparsed-text($file, $encoding, ())        
        return
            try {
                trace(file:write-text($fname_, $fileContent) , concat('Write file: ', $fname_, ' '))
            } catch * {trace(1, concat('ERR:CODE: ', $err:code, ', ERR:DESCRIPTION: ', $err:description, ' - '))}
    return
        ($errors[1], 0)[1]
};

(:~
 : Writes a collection of json documents as json docs into a folder.
 :
 : @param files the file URIs
 : @param dir the folder into which to write
 : @return 0 if no errors were observed, 1 otherwise
 :)
declare function f:foxfunc_write-json-docs($files as xs:string*, 
                                           $dir as xs:string?,
                                           $encoding as xs:string?)
        as xs:integer {
    let $tocItems :=        
        for $file at $pos in $files
        let $file := 
            if ($file instance of attribute()) then string($file) else $file
        let $path :=
            if ($file instance of node()) then 
                let $raw := $file/root()/document-uri(.)
                return if ($raw) then $raw else concat('__file__', $pos)
            else $file        
        let $fnameOrig := replace($path, '^.+/', '')
        let $fname := 
            if ($file instance of node()) then $fnameOrig 
            else concat($fnameOrig, '.xml')
        group by $fnameOrig
        return
            if (count($file) eq 1) then 
                <file name="{$fname}" originalName="{$fnameOrig}" path="{$path}"/>
            else
                <files originalName="{$fnameOrig}" count="{count($file)}">{
                    let $prePostfix := replace($fnameOrig, '(.+)(\.[^.]*$)', '$1~~~$2')
                    let $pre := substring-before($prePostfix, '~~~')
                    let $post := substring-after($prePostfix, '~~~')
                    for $f at $pos in $file
                    let $name := 
                        let $raw :=
                            if ($pos eq 1) then $fnameOrig else 
                                concat($pre, '___', $pos, '___', $post)
                        return
                             if ($f instance of node()) then $raw else concat($raw, '.xml')
                    return
                        <file name="{$name}" originalName="{$fnameOrig[1]}" path="{$f}"/>
                }</files> 
    let $toc := <toc countFnames="{count($tocItems)}" countFiles="{count($files)}">{$tocItems}</toc>
    let $tocFname := concat($dir, '/', '___toc.write-json-docs.xml')
    let $_ := file:write($tocFname, $toc)
    
    let $errors :=
        for $file at $pos in $files
        let $file := 
            if ($file instance of attribute()) then string($file) else $file
        let $path :=
            if ($file instance of node()) then 
                let $raw := $file/root()/document-uri(.)
                return if ($raw) then $raw else concat('__file__', $pos)
            else $file   
        let $fname := $toc//file[@path eq $path]/@name/string()
        let $fname_ := string-join(($dir, $fname), '/')        
        let $fileContent := 
            if ($file instance of node()) then serialize($file)
            else 
                try {
                    let $fileContent := f:fox-unparsed-text($file, $encoding, ())
                    return
                        json:parse($fileContent) ! serialize(.)
                } catch * {trace((), 
                    concat('ERR:CODE: ', $err:code, ', ERR:DESCRIPTION: ', $err:description, ' - '))}
        where $fileContent                    
        return
            try {
                trace(file:write-text($fname_, $fileContent) , concat('Write file: ', $fname_, ' '))
            } catch * {trace(1, concat('ERR:CODE: ', $err:code, ', ERR:DESCRIPTION: ', $err:description, ' - '))}
    return
        ($errors[1], 0)[1]
(:        
    let $errors :=
        for $file in $files
        let $path := $file
        let $fname := replace($path, '^.+/', '')
        let $fname_ := trace(concat(string-join(($dir, $fname), '/'), '.xml') , 'PATH#: ')
        let $fileContent := f:fox-unparsed-text($file, $encoding, ())
        let $fileContentXml := json:parse($fileContent) ! serialize(.)
        return
            try {
                file:write-text($fname_, $fileContentXml)
            } catch * {1}
    return
        ($errors[1], 0)[1]
:)        
};


(:~
 : Foxpath function `xwrap#3`. Collects the items of $items into an XML document
 : Before copying into the result document, every item from $items is processed as follows:
 : (A) if an item is a node: 
 :   (1) if flag 'b' is not set, the item is not modified
 :   (2) otherwise a copy enhanced by an @xml:base attribute is created
 : (B) if an item is atomic: 
 :   (1) if flag 'd' is set, the item is interpreted as URI and it is attempted to be
 :       parsed into a document, with an @xml:base attribute added to the root element,
 :       if flag 'b' is set, and without @xml:base otherwise; if parsing fails, a 
 :       <PARSE-ERROR> element is created with the item value as ocntent
 :   (2) if flag 'w' is set, the item is interpreted as URI and the text found at
 :       this URI is retrieved and wrapped in a <_text_> element with an @xml:base attribute
 :   (3) if flag 't' is set, the item is interpreted as URI and the text found at 
 :       this URI is retrieved
 :   (4) if none of the flags 'd', 'w', 't' is set: the item is not modified 
 :
 : @param items the items from which to create the content of the result document
 : @param name the name of the root element of the result document
 : @return the result document
 :)
declare function f:foxfunc_xwrap($items as item()*, $name as xs:QName, $flags as xs:string?, $options as map(*)?) 
        as element()? {
    let $val :=
        for $item in $items 
        return

        (: item a node => copy item :)
        if ($item instance of node()) then
            if (contains($flags, 'b') and ($item instance of element() or $item instance of document-node())) then
                let $baseUri := base-uri($item)
                let $elem := $item/descendant-or-self::*[1]
                return
                    let $xmlBase := if ($elem/@xml:base) then () else attribute xml:base {$baseUri}
                    return
                        element {node-name($elem)} {
                            $elem/@*, $xmlBase, $elem/node()
                        }
            else
                $item
                
        (: item a URI, flag 'd' => parse document at that URI :)                
        else if (contains($flags, 'd')) then
            let $doc := try {i:fox-doc($item, $options)/*} catch * {()}
            return
                if ($doc) then 
                    if (contains($flags, 'b')) then
                        let $xmlBase := if ($doc/@xml:base) then () else attribute xml:base {$item}
                        return
                            if (not($xmlBase)) then $doc else
                                element {node-name($doc)} {
                                    $doc/@*,
                                    $xmlBase,
                                    $doc/node()
                                }
                    else $doc
                else                                    
                    <PARSE-ERROR>{$item}</PARSE-ERROR>
                    
        (: item a URI, flag 'w' => read text at that URI, write it intoa wrapper element :)                    
        else if (contains($flags, 'w')) then
            let $text := try {i:fox-unparsed-text($item, (), $options)} catch * {()}
            return
                if ($text) then <_text_ xml:base="{$item}">{$text}</_text_>
                else <READ-ERROR>{$item}</READ-ERROR>
                
        (: item a URI, flag 't' => read text at that URI, copy it into result :)                
        else if (contains($flags, 't')) then
            let $text := try {i:fox-unparsed-text($item, (), $options)} catch * {()}
            return
                if ($text) then $text
                else <READ-ERROR>{$item}</READ-ERROR>
        else
            $item
    return
        element {$name} {attribute countItems {count($val)}, $val}
};