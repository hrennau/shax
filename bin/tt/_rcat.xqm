(:
 :***************************************************************************
 :
 : rcat.xqm - functions for creating and resolving resource catalogs
 :
 :***************************************************************************
 :)
 
(:
@TODO
Input parameters specifies one or more directories, positive and/or
negative name patterns, a switch determining whether subdirectories
are considered too, the format (xml or text). A further
parameter controls whether the name of the (root) directory
is prepended before the file name. If the parameter "expression"
is used, only XML files are considered for which the expression
evaluates to true (effective boolean value).

@param a whitespace seperated list of directories
@param patterns whitespace separated list of name patterns to be included and/or excluded
@param deep if true, the files of subdirectories are considered, too
@param relative if true, the file names are relative to the root directory from
       where they were found, otherwise the root directory is prepended
@param expression if specified, only files are considered which are XML
   and for which the expression evaluates to true (effective boolean value)
@param prefix if set, each this prefix is prepended before each path
@param withdirs if true, the file list contains also directories, otherwise only fils
@param format if text, file names are rendered in plain text, one name per line,
       otherwise as XML document
@return a file list in XML format

@version 20121120-A
==================================================================================
:)

module namespace m="http://www.ttools.org/xquery-functions";

import module namespace tt="http://www.ttools.org/xquery-functions" at
    "_csvParser.xqm",
    "_foxpath.xqm",    
    "_nameFilter_parser.xqm",
    "_nameFilter.xqm",
    "_resourceAccess.xqm",        
    "_stringTools.xqm";

declare namespace z="http://www.ttools.org/structure";
declare namespace file="http://expath.org/ns/file";

(:
 : ============================================================================
 :
 :     p u b l i c    f u n c t i o n s
 :
 : ============================================================================
 :)
(:
 :    r e s o l v e   d f d
 :    ---------------------
 :)
 
(:~
 : Resolves a directory filter descriptor, delivering the filtered documents.
 :
 : In case of an error, a `z:errors` element is returned.
 : 
 : @param dfd a directory filter descriptor
 : @return the filtered documents
 :)
declare function m:dfd($dfd as xs:string?)
        as document-node()* {
    if (not($dfd)) then () else
        m:resolveRcat(m:rcat($dfd))
};

(:
 :    r e s o l v e   r c a t
 :    -----------------------
 :)

(:~
 : Resolves a resource catalog.
 :
 : @param rcat the resource catalog
 : @return the sequence of documents or other
 :    representations of the URIs contained by the
 :    catalog
 :)
declare function m:resolveRcat($rcat as node()?)
        as item()* {
    m:resolveRcat($rcat, ())
};

(:~
 : Resolves a resource catalog.
 :
 : @param rcat the resource catalog
 : @param query conditions imposed on external properties of the resources to be resolved 
 : @return the sequence of documents or other
 :    representations of the URIs contained by the
 :    catalog
 :)
declare function m:resolveRcat($rcat as node()?, $pquery as xs:string?)
        as item()* {
    let $targetFormat := $rcat/(@targetFormat, @format, 'xml')[string()][1]
    return
        if (not($targetFormat ne 'xml')) then
            m:_resolveRcat_xml($rcat, $pquery)
        else if ($targetFormat eq 'text') then
            m:_resolveRcat_txt($rcat)
        else if ($targetFormat eq 'xtext') then
            m:_resolveRcat_xtext($rcat)
        else if ($targetFormat eq 'lines') then
            m:_resolveRcat_lines($rcat)
        else if ($targetFormat eq 'xcsv') then
            m:_resolveRcat_xcsv($rcat)
        else
            tt:createError('INVALID_ARG', concat('Invalid rcat passed to ',
                '''resolveRcat'' - it specifies an unexpected target ',
                'format: ', $targetFormat , '; the target format must be ',
                'one of these: xml, text, xtext, lines, xcsv'), ()) 
};

(:
 :    c o n s t r u c t    r c a t
 :    ----------------------------
 :)

(:~
 : Constructs an rcat referencing the XML documents selected by a 
 : foxpath expression.
 :
 : @param foxpath a foxpath expression 
 : @return the rcat referencing the documents selected by the foxpath expression
 :)
declare function m:rcatFromFoxpath($foxpath as xs:string)
        as element(rcat) {
    let $selFiles := tt:resolveFoxpath($foxpath, map:entry('IS_CONTEXT_URI', true()), ())        
    return
        if ($selFiles instance of element(errors)) then
            tt:wrapErrors(
                tt:createError('INVALID_FOXPATH_EXPR', concat('Expression text: ', $foxpath))
            )
        else 
            let $baseURI := file:current-dir() ! replace(., '\\', '/')
            return
                <rcat foxpath="{$foxpath}" 
                      format="xml" 
                      targetFormat="xml"
                      countFiles="{count($selFiles)}" 
                      t="{current-dateTime()}"
                      xml:base="{$baseURI}">{
                    $selFiles ! <resource href="{.}"/> 
                }</rcat>            
};

(:~
 : Constructs an rcat referencing the XML documents selected by a 
 : foxpath expression.
 :
 : @param foxpath a foxpath expression 
 : @param format the format of the resources referenced by the rcat
 : @param targetFormat the format of the items to be constructed 
 :    from the resources referenced by the rcat
 : @param encoding the encoding of text resources reference by the rcat
 :    (irrelevant in case of XML documents) 
 : @param properties an element with attributes to be added to the rcat root element
 : @return the rcat referencing the documents selected by $dfd
 :)
 declare function m:rcatFromFoxpath($foxpath as xs:string,
                                    $format as xs:string?,
                                    $targetFormat as xs:string?,
                                    $encoding as xs:string?,
                                    $properties as element()?)
        as element() {
    let $format := ($format[string()], 'xml')[1]
    let $targetFormat := ($targetFormat[string()], $format)[1]
    let $attEncoding :=
        if (not($encoding) or $format eq 'xml') then () else attribute encoding {$encoding}
        
    let $furtherAtts :=
        if ($format eq 'csv') then (
            attribute csv.sep {($properties/@sep, ',')[1]},
            attribute csv.delim {($properties/@delim, '"')[1]},
            attribute csv.header {($properties/@header, 'false')[1]},
            attribute csv.names {($properties/@names, 'table row cell')[1]},
            attribute csv.fromRec {($properties/@fromRec, '1')[1]},
            attribute csv.toRec {($properties/@toRec, '0')[1]}
        ) else ()

    let $selFiles := tt:resolveFoxpath($foxpath, map:entry('IS_CONTEXT_URI', true()), ())        
    return
        if ($selFiles instance of element(errors)) then
            tt:wrapErrors(
                tt:createError('INVALID_FOXPATH_EXPR', concat('Expression text: ', $foxpath))
            )
        else 
            let $baseURI := file:current-dir() ! replace(., '\\', '/')
            return
                <rcat foxpath="{$foxpath}" 
                      format="{$format}" 
                      targetFormat="{$targetFormat}"
                      countFiles="{count($selFiles)}">{
                    $attEncoding,
                    attribute t {current-dateTime()},
                    attribute xml:base {$baseURI},
                    $furtherAtts,
                    $selFiles ! <resource href="{.}"/> 
                }</rcat>         
};


(:~
 : Constructs an rcat referencing XML documents.
 :
 : @param dfd directory filter descriptor 
 : @return the rcat referencing the documents selected by $dfd
 :)
 declare function m:rcat($dfd as xs:string?)
        as element() {
    m:rcat($dfd, 'xml', 'xml', (), ())        
};        

(:~
 : Constructs an rcat referencing XML documents.
 :
 : @param dfd directory filter descriptor
 : @param properties an element with attributes to be added to the rcat root element
 : @return the rcat referencing the documents selected by $dfd
 :)
 declare function m:rcat($dfd as xs:string?,
                         $properties as element()?)
        as element() {
    m:rcat($dfd, 'xml', 'xml', (), $properties)        
};        

(:~
 : Constructs an rcat referencing XML documents.
 :
 : @param dfd directory filter descriptor
 : @param format the format of the resources referenced by the rcat
 : @param targetFormat the format of the items to be constructed 
 :    from the resources referenced by the rcat
 : @param encoding the encoding of text resources reference by the rcat
 :    (irrelevant in case of XML documents) 
 : @param properties an element with attributes to be added to the rcat root element
 : @return the rcat referencing the documents selected by $dfd
 :)
 declare function m:rcat($dfd as xs:string?,
                         $format as xs:string?,
                         $targetFormat as xs:string?,
                         $encoding as xs:string?,
                         $properties as element()?)
        as element() {
        
    let $format := ($format[string()], 'xml')[1]
    let $targetFormat := ($targetFormat[string()], $format)[1]
    return
        if ($format eq 'xml') then m:_rcat_xml($dfd)
       
        else if ($format eq 'text' or $format eq 'csv') then        
            if ($targetFormat eq 'text') then
                m:_rcat_text($dfd, $encoding, $targetFormat, $properties)
            else if ($targetFormat eq 'xcsv') then
                m:_rcat_csv($dfd, $encoding, $properties)
            else
                m:_rcat_text($dfd, $encoding, $targetFormat, $properties)               
        else 
            tt:createError('UNEXPECTED_RCAT_FORMAT', concat('Unexpected rcat format: ', $format, $targetFormat, ' ; ',
                          'must be one of these: xml, text'), ())                
};

(:
 : ============================================================================
 :
 :     p r i v a t e    f u n c t i o n s
 :
 : ============================================================================
 :)

(:
 :    r e s o l v e    r c a t
 :    ------------------------
 :)

(:~
 : Resolves an rcat to the XML documents referenced.
 :
 : @param rcat the rcat document (document node or root element)
 : @param pquery a pquery (resource properties query) selecting referenced resources 
 : @return the document nodes of all referenced and possibly selected resources
 :)
declare function m:_resolveRcat_xml($rcat as node()?, $pquery as xs:string?)
        as node()* { 
    let $rcat := $rcat/descendant-or-self::*[1]   
    return if ($rcat/self::z:errors) then $rcat else
    
    let $pq :=
        if (not($pquery)) then () else m:_parsePquery($pquery)
    for $rdesc in $rcat//*[@href][doc-available(@href)]
    where not($pq) or m:_matchesPquery($rdesc, $pq)
    return
        let $uri := $rdesc/@href/resolve-uri(., base-uri(..)) 
        return tt:doc($uri)        
};

(:~
 : Resolves an rcat to the text contents of the resources referenced.
 :
 : @param rcat the rcat document (document node
 :    or root element)
 : @return the text contents of all referenced text files
 :    documents
 :)
declare function m:_resolveRcat_txt($rcat as node()?)
        as xs:string* { 
    let $rcat := $rcat/descendant-or-self::*[1]   
    return if ($rcat/self::z:errors) then $rcat else
    
    let $encoding := ($rcat/@encoding, 'UTF-8')[1]
    let $normLinefeed := $rcat/@normLinefeed
    
    for $href in $rcat//@href 
    let $text := tt:unparsed-text(resolve-uri($href, base-uri($href/..)), $encoding)
    let $text := 
        if (not($normLinefeed eq 'true')) then $text else tt:normalizeLinefeed($text)
    return 
        $text       
};

(:~
 : Resolves an rcat to the text lines of the contents of the resources
 : referenced.
 :
 : @param rcat the rcat document (document node
 :    or root element)
 : @return the lines of the text contents of all referenced 
 :    text files documents
 :)
declare function m:_resolveRcat_lines($rcat as node()?)
        as xs:string* { 
    let $rcat := $rcat/descendant-or-self::*[1]   
    return if ($rcat/self::z:errors) then $rcat else
    
    let $encoding := ($rcat/@encoding, 'UTF-8')[1]    
    for $href in $rcat//@href 
    return 
        tt:unparsed-text-lines(resolve-uri($href, base-uri($href/..)), $encoding)       
};

(:~
 : Resolves an rcat to the text contents of all text files referenced,
 : wrapping each text content in a "resource" element node with a @uri 
 : attribute and a @name attribute stating the resrouce URI and 
 : resource name, respectively.
 :
 : @param rcat the rcat document (document node
 :    or root element)
 : @return the wrapped text contents of all referenced text files
 :)
declare function m:_resolveRcat_xtext($rcat as node()?)
        as element(resource)* { 
    let $rcat := $rcat/descendant-or-self::*[1]   
    return if ($rcat/self::z:errors) then $rcat else
    
    let $encoding := ($rcat/@encoding, 'UTF-8')[1]
    let $normLinefeed := $rcat/@normLinefeed/string()    
    for $uri in $rcat//@href
    let $name := replace($uri, '.*/', '')
    let $text := tt:unparsed-text($uri, $encoding)
    let $text := 
        if (not($normLinefeed eq 'true')) then $text else
            replace($text, '&#xD;&#xA;', '&#xA;')
    return
        <resource uri="{$uri}" name="{$name}" yogi="">{$text}</resource>
};

(:~
 : Resolves an rcat to xml-csv documents.
 :
 : @param rcat the rcat document (document node
 :    or root element)
 : @return the xml-csv documents
 :)
declare function m:_resolveRcat_xcsv($rcat as node()?)
        as element()* { 
    let $rcat := $rcat/descendant-or-self::*[1]   
    return if ($rcat/self::z:errors) then $rcat else
    
    let $encoding := ($rcat/@encoding, 'ISO-8859-1')[1]
    let $sep := $rcat/@csv.sep 
    let $delim := $rcat/@csv.delim            
    let $header := $rcat/@csv.header            
    let $names := $rcat/@csv.names/tokenize(., '\s+')            
    let $fromRec := $rcat/@csv.fromRec
    let $toRec := $rcat/@csv.toRec 
    return    
        for $uri in $rcat//@href
        return
            m:parseCsv(resolve-uri($uri, base-uri($uri/..)), 
                       $encoding, $sep, $delim, $header, $names, $fromRec, $toRec)
};

(:
 :    c o n s t r u c t    r c a t
 :    ----------------------------
 :)

(:~
 : Parses a directory filter descriptor, delivering a document catalog.
 : The document catalog contains for each found document a 'doc' element
 : with a @href attribute.
 :
 : In case of an error, a `z:errors` element is returned.
 :
 : @param docDFD a directory filter descriptor
 : @param load if true, the filtered documents are returned, otherwise
 :    a catalog containing the filtered document URIs
 : @return the filtered documents, or a catalog of filtered document URIs
 :)
declare function m:_rcat_xml($dfd as xs:string?)
        as element()? {
    if (not($dfd)) then () else
    
    let $format := 'xml'
    let $targetFormat := 'xml'   
    let $encoding := ()
    return
        m:_rcatFromDfd($dfd, $format, $targetFormat, $encoding, ())
};

(:~
 : Parses a directory filter descriptor, delivering a document catalog.
 : The document catalog contains for each resource a 'doc' element with
 : a @href attribute. The documents are assumed to be text documents.
 :
 : @param dfd a directory filter descriptor; structured string with the
 :    following fields: directories, file name filter, sub directory filter,
 :    encoding.
 : @param encoding the encoding to be used when loading the resources
 : @param targetFormat if used, specifies a target format (which defaults to
 :    'text'); possible values: text, xtext, lines
 : @param modifiers the attributes on this element are transferred to the 
 :    catalog element
 : @return the document catalog
 :)
declare function m:_rcat_text($dfd as xs:string?, 
                              $encoding as xs:string?,
                              $targetFormat as xs:string?, 
                              $properties as element()?)
        as element()? {
    if (not($dfd)) then () else

    let $format := 'text'
    let $targetFormat := ($targetFormat[string()], $format)[1]
    let $encoding := ($encoding, 'UTF-8')[1]
    
    let $fields := m:getTextFields(normalize-space($dfd))    
    let $rcatProperties := 
        let $atts := $properties/(@* except @encoding)
        return
            if (empty($atts)) then () else <properties>{$atts}</properties>
            
    return
        m:_rcatFromDfd($dfd, $format, $targetFormat, $encoding, $rcatProperties)
};

(:~
 : Parses a directory filter descriptor, delivering a document catalog.
 : The document catalog contains for each resource a 'doc' element with
 : a @href attribute. The documents are assumed to be CSV records.
 :
 : @param dfd a directory filter descriptor; structured string with the
 :    following fields: directories, file name filter, sub directory filter,
 :    encoding, CSV separator (default: ,), CSV delimiter (default: "),
 :    start row number (default: 1), end row number (default: -1, meaning
 :    all records).
 : @param encoding the encoding to be used when loading the resources
 : @param modifiers the attributes on this element are transferred to the 
 :    catalog element
 : @return the document catalog
 :)
declare function m:_rcat_csv($dfd as xs:string?, 
                             $encoding as xs:string?,
                             $properties as element()?)
        as element()? {
    if (not($dfd)) then () else

    let $format := 'csv'
    let $targetFormat := 'xcsv'
    
    let $fields := m:getTextFields(normalize-space($dfd))
    let $encoding := ($encoding, 'UTF-8')[1]

    let $rcatProperties :=
        <properties>{
            attribute csv.sep {($properties/@sep, ',')[1]},
            attribute csv.delim {($properties/@delim, '"')[1]},
            attribute csv.header {($properties/@header, 'false')[1]},
            attribute csv.names {($properties/@names, 'table row cell')[1]},
            attribute csv.fromRec {($properties/@fromRec, '1')[1]},
            attribute csv.toRec {($properties/@toRec, '0')[1]},
            ()
        }</properties>           
   
    return
        m:_rcatFromDfd($dfd, $format, $targetFormat, $encoding, $rcatProperties)
};

(:~
 : Transforms a directory filter descriptor into an rcat.
 :
 : @param dfd a directory filter descriptor
 : @param format the document format
 : @param targetFormat the desired format of documents retrieved
 :    from the rcat
 : @param encoding the character encoding (not necessary if files are XML)
 : @properties properties modifying the retrieval of documents from
 :    the URIs contained by the rcat
 : @return an rcat
 :) 
declare function m:_rcatFromDfd(
                         $dfd as xs:string,
                         $format as xs:string,
                         $targetFormat as xs:string,                        
                         $encoding as xs:string?,
                         $properties as element()?)
        as element()? {   
    let $fields := m:getTextFields(normalize-space($dfd))
    return
        (: special case - only one field => interpreted as path/fileNames (e.g. /a/b/c/*.xml *.xsd) :)
        if (count($fields) eq 1) then
            let $dirFiles := replace($fields, '\\', '/', 's')

            let $dirs := 
                if (not(contains($dirFiles, '/'))) then ''
                else replace($dirFiles, '(^.*)/.*', '$1', 's')
            
            let $files := replace($dirFiles, '^.*/', '')
            let $subDirs := ()
            let $query := ()
            return
                m:_rcat($format, $targetFormat, $dirs, $files, $subDirs, $query, $encoding, $properties)
                
        (: standard case - several fields :)                
        else
            let $dirs := replace($fields[1], '\\', '/', 's')
            let $files := $fields[2]
            let $subDirs := $fields[3]
            let $query := $fields[4]
            return
                m:_rcat($format, $targetFormat, $dirs, $files, $subDirs, $query, $encoding, $properties)        
};

(:~
 : Writes a resource catalog. In case of errors, a
 : `z:errors` element is returned.
 :
 : The catalog element has the following attributes:
 : - dirs : the directories
 : - files : the file name pattern(s)
 : - subDirs : sub directory patterns (in case of recursive search)
 : - countFiles : the number of URIs
 : - xml:base - the base URI
 : - format - the resource format ('xml' or 'text')
 : - encoding - the encoding of the resources (relevant only if format different from 'xml')
 : - t : the creation time
 :
 : @param onlyXml if true, non-XML resources are ignored
 : @param dirs the directory name(s)
 : @param files the file name pattern(s)
 : @param subDirs a filter on resources found during recursive search - excludes the
 :    contents of sub directories not matching these path filters
 : @param query deprecated
 : @param encoding the encoding to be used in order to retrieve the resources (irrelevant
 :    in case of XML documents, as these are self-describing)   
 : @param properties optional element with attributes to be copied into the rcat root element 
 :    (for example specifying format details)
 :)
declare function m:_rcat($format as xs:string,
                         $targetFormat as xs:string,
                         $dirs as xs:string,
                         $files as xs:string?,
                         $subDirs as xs:string?,                               
                         $query as xs:string?, 
                         $encoding as xs:string?,
                         $properties as element()?)
        as element()? {      
    let $attEncoding :=
        if (not($encoding) or $format eq 'xml') then () else attribute encoding {$encoding}
    let $fileFilter as element()? := 
        if (not($files)) then () else m:parseNameFilter($files)
    let $dirFilter as element()? := 
        if (not($subDirs)) then () else m:parsePathFilter($subDirs)    
    return
        let $errors := ($fileFilter, $dirFilter)/self::z:errors
        return
            if ($errors) then
                if (count($errors) gt 1) then 
                    <z:errors>{($fileFilter, $dirFilter)/z:error}</z:errors>
                else $errors 
        else

    (: let $base := file:parent(tt:static-base-uri()) :)
(:    
    let $base := tt:static-base-uri()
    let $base := file:path-to-uri($base)
:)
    let $base := m:_rcat_baseUri()
    let $xmlbase := if ($base) then attribute xml:base {$base} else ()
        
    let $foundFiles :=
        for $dirSpec in 
            if ($dirs eq '') then $dirs else tokenize(normalize-space($dirs), '\s+')
        let $deep := starts-with($dirSpec, '|')
        let $dir := replace($dirSpec, '^\|', '')
        let $dir := resolve-uri($dir, $base)
        return
            m:_getFiles($dir, $deep, $fileFilter, $dirFilter, $format, $query)
    return if ($foundFiles/self::z:errors) then $foundFiles else
    
    let $dcat :=
        <rcat dirs="{$dirs}" files="{$files}" subDirs="{$subDirs}" 
              format="{$format}" targetFormat="{$targetFormat}"
              countFiles="{count($foundFiles)}" t="{current-dateTime()}">{
            $xmlbase,
            $attEncoding,
            $properties/@*,
            for $f in $foundFiles 
            order by lower-case($f/@href)  
            return $f
        }</rcat>

    return
        $dcat
};

(:~
 : Returns the base URI used by function `m:_rcat`.
 :
 : @return the base URI
 :)
declare function m:_rcat_baseUri()
        as xs:string {
    let $base := file:current-dir()
    let $base := file:path-to-uri($base) ! replace(., '\\', '/')
    return $base    
};

(:~
 : Recursive helper function of '_rcat'.
 :
 :)
declare function m:_getFiles($dir as xs:string, 
                             $deep as xs:boolean,                           
                             $fileFilter as element(nameFilter)?,
                             $dirFilter as element(pathFilter)?,                            
                             $format as xs:string,
                             $query as xs:string?)
        as element()* {
    let $dir := replace($dir, '([^/\\])$', '$1/')
    return if (not(file:exists($dir))) then 
        <z:errors>
            <z:error type="DIRECTORY_NOT_FOUND" dir="{$dir}" msg="{concat('Directory not found: ', $dir)}"/>        
        </z:errors> else
        
    let $dirContent := file:list($dir) ! replace(., '\s', '%20') ! replace(., '\\', '/') ! concat($dir, .)
    let $dirFiles := $dirContent[not(file:is-dir(.))]
    let $subDirs := if (not($deep)) then () else $dirContent[file:is-dir(.)] ! replace(., '\\$', '/')
                
    let $ownFiles :=
        let $dirCheck:=
            if (not($dirFilter)) then true() else
                let $dirPath :=
                    replace(replace(replace($dir, 'file:/+', ''), '[a-zA-Z]:', ''), '(/|\\)$', '')
                return
                    tt:matchesPathFilter($dirPath, $dirFilter)
        return if (not($dirCheck)) then () else
        
        for $file in $dirFiles
        let $fileName := file:name($file)
        return
            if ($fileFilter and not(tt:matchesNameFilter($fileName, $fileFilter))) then () else
            
        let $fileNorm := replace($file, '\\', '/')
        let $formatCondition := 
            if ($format eq 'xml') then tt:doc-available($fileNorm)
            else true()
(:        
        let $queryCondition as xs:boolean? :=
            if (not($query)) then 
                if ($onlyXml) then tt:doc-available($fileNorm) else true()
            else if (not(tt:doc-available($fileNorm))) then false()
            else
                let $bindings := map{ '' := tt:doc($fileNorm)[1]} 
                return
                    boolean(xquery:eval($query, $bindings))
        where $queryCondition
:)        
        where $formatCondition
        return
            <resource href="{$fileNorm}"/>
    return (
        $ownFiles,
        if (not($deep)) then () else
        for $subDir in $subDirs
        let $subDirName := file:name($subDir)
        (: where not($dirFilter) or tt:matchesPathFilter($subDirName, $dirFilter) :)
        return 
            m:_getFiles($subDir, $deep, $fileFilter, $dirFilter, $format, $query)     
    )            
};

(:
 :
 :    p a r s e    p r o p e r t i e s    q u e r y
 :    ---------------------------------------------
 :)

(:~
 : Parses a pquery (properties query) to a structured representation.
 :
 : @param text the text of the pquery
 : @return a 'pquery' element containing a structured representation of
 :    the pquery
 :)
declare function m:_parsePquery($text as xs:string?)
        as element(pquery)? { 
    if (not($text)) then () else
    
    (: 
        @TODO - for a proof of concept, the query is constrained to consist
                of a single query item; this must be extended (and, or, ...)
    :)           
    let $pqItem := m:_parsePqueryItem($text)
    return
        <pquery>{
            $pqItem
        }</pquery>
};

declare function m:_parsePqueryItem($itemText as xs:string?)
        as element()? {
    if (not($itemText)) then () else
    
    let $sep := codepoints-to-string(40000)
    let $nameOpValue := 
        let $concat := replace($itemText, '^(.+?)(~+|=+)(.*)', concat('$1', $sep, '$2', $sep, '$3'))
        return tokenize($concat, $sep)
    
    return
        <prop name="{$nameOpValue[1]}" value="{$nameOpValue[3]}" op="{$nameOpValue[2]}"/>
};

(:~
 : Checks whether a resource descriptor matches a pquery.
 :
 : @param rdesc a resource descriptor
 : @param pquery a pquery (properties query)
 : @return true if the resource descriptor matches the pquery
 :) 
declare function m:_matchesPquery($rdesc as element(), $pquery as element(pquery)?)
        as xs:boolean {
    if (not($pquery)) then true() else

    (:
        @TODO - for a proof of concept, the query is constrained to consist
                of a single query item; this must be extended (and, or, ...)
    :)
    let $pqItem := $pquery/*
    return
        m:_matchesPqueryItem($rdesc, $pqItem)
};

(:~
 : Checks whether a resource descriptor matches a pquery item.
 :
 : @param rdesc a resource descriptor
 : @param pqueryItem a pquery (properties query)
 : @return true if the resource descriptor matches the pquery
 :) 
declare function m:_matchesPqueryItem($rdesc as element(), $pqueryItem as element(prop)?)
        as xs:boolean {
    if (not($pqueryItem)) then true() else
    
    let $name := $pqueryItem/@name
    let $op := $pqueryItem/@op
    let $value := $pqueryItem/@value
    let $property := $rdesc/ancestor-or-self::*/@*[local-name(.) eq $name][last()]    

    return
        (: 
            @TODO - the handling of a missing property must be refined; for the time being,
                    a missing property is treated as excluding a query match
        :)
        if (empty($property)) then false()

        (: operant '=' :)
        else if ($op eq '=') then
            $property eq $value
            
        (: operant '~' :)            
        else if ($op eq '~') then
            let $filter := tt:parseNameFilter($value)
            return
                tt:matchesNameFilter($property, $filter)
        else                
            tt:createError('INVALID_ARG', concat('Pquery contains unexpected operator: ', $op), ())               
};
