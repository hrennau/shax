(:
 : -------------------------------------------------------------------------
 :
 : triplesUtil.xqm - tools for constructing and manipulating triples
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
    "namespaceTools.xqm";

declare namespace shax="http://shax.org/ns/model";
declare namespace z="http://www.ttools.org/shax/ns/structure";

(:~
 : Constructs an xtriple, which is an XML representation of a triple.
 :
 : @param subjectIri the subject IRI
 : @param predicateIri the predicate IRI
 : @param value object value, an IRI or RDF literal
 : @param type an optional datatype or '#iri'
 : @param lang an optional language tag
 : @param reverse if true, parameters $subjectIri and $value are swapped, so that
 :    parameter $value is treated as subject IRI and parameter $subjectIri is 
 :    treated as object value 
 : @return a triple in XML representation
 :)
declare function f:xtriple($subjectIri as xs:string, 
                           $predicateIri as xs:string,
                           $value as item(),
                           $type as xs:string?,
                           $lang as xs:string?,
                           $reverse as xs:boolean?) 
        as element(shax:triple) {
    if ($reverse) then f:xtriple($value, $predicateIri, $subjectIri, $type, $lang)
    else f:xtriple($subjectIri, $predicateIri, $value, $type, $lang)
};        

(:~
 : Constructs an xtriple, which is an XML representation of a triple.
 :
 : Structure:
 :   shax:triple
 :   . @s
 :   . @p
 :   . @v | @o
 :
 : @param subjectIri the subject IRI
 : @param predicateIri the predicate IRI
 : @param value object value, an IRI or RDF literal
 : @param type a datatype or '#iri'
 : @return a triple in XML representation
 :)
declare function f:xtriple($subjectIri as xs:string, 
                            $predicateIri as xs:string,
                            $value as item(),                            
                            $type as xs:string?,
                            $lang as xs:string?) 
        as element(shax:triple) {
    (: @TO_DO - if (not($type) and not($lang)): analyze $value and set @o, @v, @lang accorfdingly. :)   
    
    if ($type eq '#iri') then 
        <shax:triple s="{f:rdfIriRep($subjectIri)}" p="{f:rdfIriRep($predicateIri)}" o="{f:rdfIriRep($value)}"/>        
    else     
        <shax:triple s="{f:rdfIriRep($subjectIri)}" p="{f:rdfIriRep($predicateIri)}" v="{$value}">{
            $type ! attribute type {.},
            $lang ! attribute lang {.}
        }</shax:triple>
};        

(:~
 : Constructs an xtriple.
 :)
declare function f:xtriple($subjectIri as xs:string, 
                           $predicateIri as xs:string,
                           $value as item()) 
        as element(shax:triple) {
    let $type :=
        if ($value instance of xs:integer) then 'xs:integer'
        else if ($value instance of xs:decimal) then 'xs:decimal'
        else if ($value instance of xs:double) then 'xs:double'
        else if ($value instance of xs:float) then 'xs:float'
        else if ($value instance of xs:boolean) then 'xs:boolean'
        else
            if (starts-with($value, '<')) then '#iri'
            else 'xs:string'
    return
        <shax:triple s="{f:rdfIriRep($subjectIri)}" p="{f:rdfIriRep($predicateIri)}">{
            if ($type eq '#iri') then attribute o {f:rdfIriRep($value)}
            else attribute v {$value},
            attribute type {$type}
         }</shax:triple>
};        

(:~
 : Constructs xtriples representing the properties of a blank node.
 :)
declare function f:xblanknode($xtriples as element(shax:triple)*)
        as element(shax:triple)? {
    ()        
};

(:~
 : Transforms a list of xtriples to a single "xlisttriple".
 :)
declare function f:xtriples2List($xtriples as element(shax:triple)*)
        as element(shax:triple)? {
    if (empty($xtriples)) then () else
    
    let $head := head($xtriples)
    return
        <shax:triple s="{$head/@s}" p="{$head/@p}" list="true">{
            $xtriples/<shax:item>{@v, @o, @type, @lang}</shax:item>
        }</shax:triple>
};
(:~
 : Edits a set of xtriples:
 : - replaces intermediate blank node identifiers by _:b1, 2, 3, ...
 : - replaces full URIs by prefixed names, when possible
 : - and adds a table of namespace prefixes. 
 :
 : Namespace bindings already present in the shax:triples document are preserved.
 :
 : Implementation note: full URIs are recognized by the leading "<".
 :)
declare function f:editXtriples($xtriples as element(shax:triples), $options as element(options)?)
        as element(shax:triples) {
    let $nons := ($options/@nons, $f:URI_NONS_DEFAULT)[1]        
    let $prefixNons := $options/@prefixNons
    
    let $urisRaw := $xtriples/shax:triple/(@s, @p, @o, shax:item/@o)[starts-with(., '<')] 
    (: IDEA: preliminary node ids in format: <>310b7c06-3a25-4720-bcf9-93f17bf0bb50
       For these, an own section of $nsmap will be constructed containing _b1, _n2, ...
     :)
    let $uris := distinct-values($urisRaw)
    let $_INFO := trace(count($urisRaw), '#URIraws: ')
    let $_INFO := trace(count($uris), '#URIs: ')
    
    let $uris_bnode := $uris[starts-with(., '<>')]
    let $uris_nobn := $uris[not(starts-with(., '<>'))]
    let $uriNamespaces := 
        for $uriRep at $pos in $uris_nobn
        let $uri := replace($uriRep, '^<|>$', '')
        let $nsUri := replace($uri, '(.+[#/])[^#/]*$', '$1')
        where ($nsUri ne $uri or matches($nsUri, '[#/]$')) and not(ends-with($nsUri, 'http://'))
        group by $nsUri
        where count($uri) gt 1
        return $nsUri
    let $uriNamespaces := $uriNamespaces => distinct-values() => sort()
    let $incomingBindings := $xtriples/shax:nsmap/*
    let $nsmap := 
        <shax:nsmap>{
            let $bindings := (
                $incomingBindings,
                for $ns at $pos in $uriNamespaces
(:                
                let $len := string-length($ns)
                where $uris_nobn[starts-with(., $ns) and string-length(.) gt $len]
:)                
                return
                    if ($incomingBindings/@uri = $ns) then ()
                    else
                        (: special case: URI prefix = nonamespace URI => 
                               use nonamespace prefix, if there is one :)           
                        let $prefix :=
                            if (matches($ns, $nons || '[#/]?$') and $prefixNons) then $prefixNons 
                            else f:getPrefix($pos)
                        return
                        <shax:ns uri="{$ns}" prefix="{$prefix}"/>
            )  
            let $bindings := (  
                $bindings, 
                if ($bindings/@prefix = 'rdf') then () else 
                    <shax:ns uri="http://www.w3.org/1999/02/22-rdf-syntax-ns#" prefix="rdf"/>
            )
            for $binding in $bindings 
            order by $binding/@prefix 
            return $binding                                            
        }</shax:nsmap>
   
    (: $fnEdit - function mapping full URI to prefixed name, if possible :)
    let $fnEdit := function($urep, $nsmap, $bnodeMap) { 
        if (not(starts-with($urep, '<'))) then $urep        
        else if (starts-with($urep, '<>')) then $bnodeMap($urep)
        else
            let $uri := replace($urep, '^<|>$', '')
            let $matches := $nsmap/shax:ns[matches($uri, concat('^', @uri, '[^#/]*$'))][1]
            return 
                if (not($matches)) then $urep
                else ($matches/@prefix || ':' || substring-after($uri, $matches/@uri)) 
                        ! replace(., "[~.\-!$&amp;'()*+,;=/?#@%_]", "\\$0")
                        ! replace (., '\s', '') (: *TO_DO* Whitespace should be replaced appropriately :)
        }

    let $bnodeMap := map:merge((
        for $ident at $pos in $uris_bnode return 
            map:entry($ident, '_:b' || $pos)))
    let $uriMap := map:merge($uris ! map:entry(., $fnEdit(., $nsmap, $bnodeMap)))
    let $_INFO := trace(count(map:keys($uriMap)), 'uri_map constructed, #keys: ')
    
    let $dedupTriples :=
        for $xt in $xtriples/shax:triple
        group by $ident := $xt/concat(@s, '~', @p, '~', @o, '~', @v, 
                           '~', string-join(shax:item/@v, ', '), '~', string-join(shax:item/@o, ', '))
        return $xt[1]
    (:        
    let $DUMMY := file:write('DEBUG_DEDUP_TRIPLES_DEDUP.xml', <shax:xtriples>{$dedupTriples}</shax:xtriples>)
    let $DUMMY := file:write('DEBUG_DEDUP_TRIPLES.xml', $xtriples)
     :)
    let $_INFO := trace(count($xtriples/shax:triple), '#xtriples: ')
    let $_INFO := trace(count($dedupTriples), '#xtriple dedup: ')
    let $_INFO := trace(count($xtriples/shax:triple/@s => distinct-values()), '#xtriple subjects: ')

    let $variant := 2
    let $inTriples := if ($variant eq 1) then $xtriples/shax:triple else $dedupTriples
    let $editedTriples :=
        for $xt at $pos in $inTriples return        
            element {node-name($xt)} {
                $xt/@s/(if (not(starts-with(., '<'))) then . else attribute s {$fnEdit(., $nsmap, $bnodeMap)}),
                $xt/@p/(if (not(starts-with(., '<'))) then . else attribute p {$fnEdit(., $nsmap, $bnodeMap)}),
                $xt/@o/(if (not(starts-with(., '<'))) then . else attribute o {$fnEdit(., $nsmap, $bnodeMap)}),
                $xt/(@v, @type, @lang, @list),                
                $xt/shax:item/element {node-name(.)} {
                    if (not(starts-with(@p, '<'))) then @p else attribute p {@p/$fnEdit(., $nsmap, $bnodeMap)},
                    @o/(if (not(starts-with(., '<'))) then . else attribute o {$fnEdit(., $nsmap, $bnodeMap)}),
                    @v, @type, @lang
                }
            }
    let $editedTriples :=
        for $t in $editedTriples
        group by $ident := $t/concat(@s, '~', @p, '~', @o, '~', @v)
        (: order by $ident :)
        return $t[1]    
    return
        element {node-name($xtriples)} {
            $nsmap,
            $editedTriples
        }
};

(:~
 : Transforms an xtriples document into a Turtle representation of its
 : triples.
 :)
declare function f:xtriples2Triples($xtriples as element(shax:triples))
        as xs:string {
    let $_INFO := trace(count($xtriples), 'Map xtriples -> triples, #')        
    let $prefixes := (
        $xtriples/shax:nsmap/shax:ns/concat('@prefix ', @prefix, ': <', @uri, '> .'),
        if ($xtriples/shax:nsmap/shax:ns/@prefix = 'rdf') then () else
            '@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .'
    )
    let $triples :=        
        for $xtriple at $pos in $xtriples/shax:triple
        let $_INFO := if ($pos mod 10000) then () else trace($pos, 'xtriple->triple, #')
        group by $subject := $xtriple/@s
        let $indent := string-join(for $i in 1 to string-length($subject) return ' ', '')
        order by $subject
        return (
            for $t at $pos in $xtriple
            let $s :=
                if ($pos eq 1) then $subject
                else $indent
            let $delimit := if ($pos eq count($xtriple)) then '.' else ';'    
            (: order by if (starts-with($t/@p, 'rdf:')) then 'aaa' else replace($t/@p, '.+:', ''), replace($t/@pm, ':.*', '') :)   
            return
                if ($t/@list eq 'true') then
                    let $ov := 
                        for $item in $t/shax:item return
                            if ($item/@o) then $item/@o
                            else $item/f:rdfLiteralRep(@v, @type, @lang)
                    return
                        concat($s, ' ', $t/@p, ' ', '( ', string-join($ov, ' '), ' )', $delimit)
                else
                    let $v := $t/(if (@o) then () else f:rdfLiteralRep(@v, @type, @lang))
                    return
                        concat($s, ' ', $t/@p, ' ', $t/(@o, $v), ' ', $delimit) 
        ) 
    return
        string-join(($prefixes, ' ', $triples), '&#xA;')
};

(:~
 : Returns a random identifier to be used as bnode
 : identifier.
 : @return an identifier of the form '<>uuid' :)
declare function f:newBnodeIdentifier() as xs:string {
    '<>' || random:uuid()
};

(:~
 : Returns a prefix identified as the n-th prefix to
 : be assigned. The prefixes returned are, dependent
 : on the number: a, b, ..., x, y, a2, b2, ..., x2, y2,
 : a3, b3, ...
 :
 : @param nr number of prefix
 : @return a prefix
 :)
declare function f:getPrefix($nr as xs:integer)
        as xs:string {
    let $chars := 'abcdefghijklmnopqrstuvwxy'        
    let $turn := ($nr - 1) idiv 25
    let $postfix := string($turn)[$turn ne 0]
    let $char := (1 + (($nr - 1) mod 25)) ! substring($chars, ., 1)
    return
        $char || $postfix
};

(:~
 : Maps a string representing an IRI to a valid representation
 : of that IRI.
 :)
declare function f:rdfIriRep($iri as xs:string)
        as xs:string {
    if (starts-with($iri, '<')) then $iri
    else if (matches($iri, '^\c*:') and not(matches($iri, '^\c*://'))) then $iri
    else concat('<', $iri, '>')        
};        

(:~
 : Constructs the lexical representation of an RDF literal.
 :
 : @param value the literal value as a plain string
 : @param type the data type
 : @return lexical representation of the literal value
 :)
declare function f:rdfLiteralRep($value as xs:string?, $type as xs:string?, $lang as xs:string?)
        as xs:string? {
    if (not($value)) then '""'  else
    
    let $raw := replace($value, '\\', '\\\\') !
                replace(., '"', '\\"') ! 
                replace(., '&#xA;', '\\n')
    return
        if ($lang) then concat('"', $raw, '"@', $lang)
        else    
            let $typeLname := $type ! replace(., '.+:', '')
            return
                if ($typeLname = ('boolean', 'integer', 'decimal')) then $raw
                else if ($typeLname ne 'string') then concat('"', $raw, '^^', $type, '"')
                else concat('"', $raw, '"')
};

