(: resourceAccess.xqm - functions for accessing resources
 :
 : @version 20141205-1 first version 
 : ===================================================================================
 :)

module namespace m="http://www.ttools.org/xquery-functions";

declare base-uri "..";

declare variable $m:BASE_URI := file:current-dir() ! replace(., '\\', '/');

(:
 : ============================================================================
 :
 :     p u b l i c    f u n c t i o n s
 :
 : ============================================================================
 :)

declare function m:resolve-uri($uri as xs:string?) 
        as xs:anyURI? {
    let $uri := resolve-uri($uri, $m:BASE_URI) return
    resolve-uri($uri)
};

declare function m:static-base-uri() 
        as xs:anyURI? {
    (: add a trailing /, if missing, to patch BaseX bug :)
    let $value := static-base-uri()
    return xs:anyURI(replace($value, '[^/]$', '$0/')) 
};

declare function m:doc($uri as xs:string?)
        as document-node()? {
    let $uri := resolve-uri($uri, $m:BASE_URI) return
    doc($uri)
};

declare function m:doc-available($uri as xs:string?)
        as xs:boolean {
    let $uri := resolve-uri($uri, $m:BASE_URI) return
    try {        
        doc-available($uri)
    } catch * {
        let $encoded := encode-for-uri($uri)
        return
            doc-available($encoded)
    }
};

declare function m:unparsed-text($href as xs:string?)
        as xs:string? {
    let $href := resolve-uri($href, $m:BASE_URI) return
    unparsed-text($href)
};

declare function m:unparsed-text($href as xs:string?, $encoding as xs:string)
        as xs:string? {
    let $href := resolve-uri($href, $m:BASE_URI) return
    unparsed-text($href, $encoding)
};

declare function m:unparsed-text-lines($href as xs:string?)
        as xs:string* {
    let $href := resolve-uri($href, $m:BASE_URI) return
    unparsed-text-lines($href)
};

declare function m:unparsed-text-lines($href as xs:string?, $encoding as xs:string)
        as xs:string* {
    let $href := resolve-uri($href, $m:BASE_URI) return
    unparsed-text-lines($href, $encoding)
};

declare function m:unparsed-text-available($href as xs:string?)
        as xs:boolean {
    let $href := resolve-uri($href, $m:BASE_URI) return
    unparsed-text-available($href)
};

declare function m:unparsed-text-available($href as xs:string?, $encoding as xs:string)
        as xs:boolean {
    let $href := resolve-uri($href, $m:BASE_URI) return
    unparsed-text-available($href, $encoding)
};

declare function m:uri-collection($uri as xs:string?)
        as xs:anyURI* {
    let $uri := resolve-uri($uri, $m:BASE_URI) return
    uri-collection($uri)
};
