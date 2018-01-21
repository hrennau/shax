(:
 : -------------------------------------------------------------------------
 :
 : docs.xqm - Document me!
 :
 : -------------------------------------------------------------------------
 :)

(:
 :***************************************************************************
 :
 :     i n t e r f a c e
 :
 :***************************************************************************
 :)
 
(:~@operations
   <operations>
      <operation name="_dcat" func="getRcat" type="element()">
         <param name="docs" type="catDFD*" sep="SC" pgroup="input"/>           
         <param name="dox" type="catFOX*" fct_minDocCount="1" sep="SC" pgroup="input"/>
         <pgroup name="input" minOccurs="1"/>         
      </operation>
      <operation name="_docs" func="getDocs" type="element()+">
         <pgroup name="input" minOccurs="1"/>
         <param name="doc" type="docURI*" sep="WS" pgroup="input"/>        
         <param name="docs" type="docDFD*" sep="SC" pgroup="input"/>
         <param name="dox" type="docFOX*" fct_minDocCount="1" sep="SC" pgroup="input"/>         
         <param name="dcat" type="docCAT*" sep="WS" pgroup="input"/>         
         <param name="fdocs" type="docSEARCH*" sep="SC" pgroup="input"/>         
      </operation>
      <operation name="_doctypes" func="getDoctypes" type="node()">
         <pgroup name="input" minOccurs="1"/>
         <param name="doc" type="docURI*" sep="WS" pgroup="input"/> 
         <param name="docs" type="docDFD*" sep="SC" pgroup="input"/>
         <param name="dox" type="docFOX*" fct_minDocCount="1" sep="SC" pgroup="input"/>         
         <param name="dcat" type="docCAT*" sep="WS" pgroup="input"/>     
         <param name="fdocs" type="docSEARCH*" sep="SC" pgroup="input"/>         
         <param name="attNames" type="xs:boolean" default="false"/>        
         <param name="elemNames" type="xs:boolean" default="false"/>     
         <param name="sortBy" type="xs:string?" fct_values="name,namespace" default="name"/>         
      </operation>      
    </operations>   
:)  

module namespace f="http://www.ttools.org/xquery-functions";
import module namespace tt="http://www.ttools.org/xquery-functions" at 
    "_request.xqm",
    "_reportAssistent.xqm",
    "_nameFilter.xqm";
    
declare namespace z="http://www.ttools.org/structure";

(:
 : ============================================================================
 :
 :     o p e r a t i o n s
 :
 : ============================================================================
 :)

(:~
 : Document me!
 :
 : @param request the operation request
 : @return a report describing ...
 :) 
declare function f:getRcat($request as element())
        as element() {     
    let $rcats := tt:getParams($request, 'docs dox')  
    return 
        if (count($rcats) eq 1) then $rcats else
        
            (: merge rcats :)
            let $hrefs := distinct-values($rcats//@href)
            let $count := count($hrefs)
            let $dirs := string-join(for $rcat in $rcats return $rcat/@dirs, ' ; ')
            let $files := string-join(for $rcat in $rcats return $rcat/@files, ' ; ')            
            let $subDirs := string-join(for $rcat in $rcats return $rcat/@subDirs, ' ; ')
            return            
                element {$rcats[1]/node-name(.)} {
                    attribute count {$count},
                    attribute dirs {$dirs},
                    attribute files {$files},                    
                    attribute subDirs {$subDirs},
                    for $href in $hrefs
                    order by lower-case($href)
                    return <doc href="{$href}"/>                    
                }
};

(:~
 : Document me!
 :
 : @param request the operation request
 : @return a report describing ...
 :) 
declare function f:getDocs($request as element())
        as element() {     
    let $docs := tt:getParams($request, ('doc', 'docs', 'dox', 'dcat', 'fdocs'))
    let $count := count($docs)
    return
        <z:documents count="{$count}">{
            for $doc in $docs return
                element {node-name($doc/*)} {
                    attribute z:documentURI {document-uri($doc)},
                    $doc/*/(@*, node())
                }                            
        }</z:documents>
};

(:~
 : Document me!
 :
 : @param request the operation request
 : @return a report describing ...
 :) 
declare function f:getDoctypes($request as element())
        as element() {
    let $docs := tt:getParams($request, ('doc', 'docs', 'dox', 'dcat', 'fdocs'))   
    let $sortBy := tt:getParams($request, 'sortBy')    
    let $withAttNames as xs:boolean := tt:getParams($request, 'attNames')
    let $withElemNames as xs:boolean := tt:getParams($request, 'elemNames')
    let $doctypes :=
        for $doc in $docs
        let $root := $doc/*
        let $lname := local-name($root)
        let $ns := namespace-uri($root)
        let $doctype := concat($lname, '@', $ns)
        group by $doctype
        let $docRefs := 
            let $uris := $doc/document-uri(.) 
            return 
                for $uri in $uris order by lower-case($uri) return <doc href="{$uri}"/>
        let $attNames :=
            if (not($withAttNames)) then () else
                let $names := 
                    for $name in distinct-values($doc//@*/local-name()) order by lower-case($name) return $name
                return
                    <z:attNames count="{count($names)}">{$names}</z:attNames>
        let $elemNames :=
            if (not($withElemNames)) then () else
                let $names := 
                    for $name in distinct-values($doc//*/local-name()) order by lower-case($name) return $name
                return
                    <z:elemNames count="{count($names)}">{$names}</z:elemNames>            
        return
            <z:doctype name="{$doctype}" count="{count($doc)}">{
                $attNames,
                $elemNames,
                $docRefs
            }</z:doctype>
    let $sortedDoctypes :=
        if ($sortBy eq 'namespace') then
            for $d in $doctypes
            let $dt := $d/lower-case(@name)
            let $name := substring-before($dt, '@')
            let $namespace := substring-after($dt, '@')
            order by $namespace, $name
            return $d
        else
            for $d in $doctypes
            let $dt := $d/lower-case(@name)            
            let $name := substring-before($dt, '@')
            let $namespace := substring-after($dt, '@')
            order by $name, $namespace
            return $d
                      
    return
        <z:doctypes countDocs="{count($docs)}" countDoctypes="{count($doctypes)}">{
            $sortedDoctypes
        }</z:doctypes>
}; 

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

