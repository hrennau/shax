(:
 : shax - 
 :
 : @version 2019-02-17T22:29:19.759+01:00 
 :)

import module namespace tt="http://www.ttools.org/xquery-functions" at
    "tt/_docs.xqm",
    "tt/_help.xqm",
    "tt/_pcollection.xqm",
    "tt/_request.xqm";

import module namespace a1="http://www.ttools.org/shax/ns/xquery-functions" at
    "schemaLoader.xqm",
    "shax.xqm",
    "typeGlobalizer.xqm",
    "xml2rdf.xqm",
    "xsd.xqm",
    "xsd2shax.xqm";

declare namespace m="http://www.ttools.org/shax/ns/xquery-functions";
declare namespace z="http://www.ttools.org/shax/ns/structure";
declare namespace zz="http://www.ttools.org/structure";

declare variable $request as xs:string external;

(: tool scheme 
   ===========
:)
declare variable $toolScheme :=
<topicTool name="shax">
  <operations>
    <operation name="_dcat" func="getRcat" type="element()" mod="tt/_docs.xqm" namespace="http://www.ttools.org/xquery-functions">
      <param name="docs" type="catDFD*" sep="SC" pgroup="input"/>
      <param name="dox" type="catFOX*" fct_minDocCount="1" sep="SC" pgroup="input"/>
      <pgroup name="input" minOccurs="1"/>
    </operation>
    <operation name="_docs" func="getDocs" type="element()+" mod="tt/_docs.xqm" namespace="http://www.ttools.org/xquery-functions">
      <pgroup name="input" minOccurs="1"/>
      <param name="doc" type="docURI*" sep="WS" pgroup="input"/>
      <param name="docs" type="docDFD*" sep="SC" pgroup="input"/>
      <param name="dox" type="docFOX*" fct_minDocCount="1" sep="SC" pgroup="input"/>
      <param name="dcat" type="docCAT*" sep="WS" pgroup="input"/>
      <param name="fdocs" type="docSEARCH*" sep="SC" pgroup="input"/>
    </operation>
    <operation name="_doctypes" func="getDoctypes" type="node()" mod="tt/_docs.xqm" namespace="http://www.ttools.org/xquery-functions">
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
    <operation name="_search" type="node()" func="search" mod="tt/_pcollection.xqm" namespace="http://www.ttools.org/xquery-functions">
      <param name="nodl" type="docURI" fct_rootElem="Q{{http://www.infospace.org/pcollection}}nodl"/>
      <param name="query" type="xs:string?"/>
    </operation>
    <operation name="_searchCount" type="item()" func="searchCount" mod="tt/_pcollection.xqm" namespace="http://www.ttools.org/xquery-functions">
      <param name="nodl" type="docURI" fct_rootElem="Q{{http://www.infospace.org/pcollection}}nodl"/>
      <param name="query" type="xs:string?"/>
    </operation>
    <operation name="_createNcat" type="node()" func="createNcat" mod="tt/_pcollection.xqm" namespace="http://www.ttools.org/xquery-functions">
      <param name="nodl" type="docURI" fct_rootElem="Q{{http://www.infospace.org/pcollection}}nodl"/>
    </operation>
    <operation name="_feedNcat" type="node()" func="feedNcat" mod="tt/_pcollection.xqm" namespace="http://www.ttools.org/xquery-functions">
      <param name="nodl" type="docURI" fct_rootElem="Q{{http://www.infospace.org/pcollection}}nodl"/>
      <param name="doc" type="docURI*" sep="WS"/>
      <param name="docs" type="catDFD*" sep="SC"/>
      <param name="dox" type="catFOX*" sep="SC"/>
      <param name="path" type="xs:string?"/>
    </operation>
    <operation name="_copyNcat" type="node()" func="copyNcat" mod="tt/_pcollection.xqm" namespace="http://www.ttools.org/xquery-functions">
      <param name="nodl" type="docURI?" fct_rootElem="Q{{http://www.infospace.org/pcollection}}nodl"/>
      <param name="query" type="xs:string?"/>
      <param name="toNodl" type="docURI" fct_rootElem="Q{{http://www.infospace.org/pcollection}}nodl"/>
    </operation>
    <operation name="_deleteNcat" type="node()" func="deleteNcat" mod="tt/_pcollection.xqm" namespace="http://www.ttools.org/xquery-functions">
      <param name="nodl" type="docURI" fct_rootElem="Q{{http://www.infospace.org/pcollection}}nodl"/>
    </operation>
    <operation name="_nodlSample" type="node()" func="nodlSample" mod="tt/_pcollection.xqm" namespace="http://www.ttools.org/xquery-functions">
      <param name="model" type="xs:string?" fct_values="xml, sql, mongo" default="xml"/>
    </operation>
    <operation name="load" type="node()" func="loadOp" mod="schemaLoader.xqm" namespace="http://www.ttools.org/shax/ns/xquery-functions">
      <param name="xsd" type="docFOX*" sep="WS" pgroup="input"/>
      <param name="xsdCat" type="docCAT*" sep="WS" pgroup="input"/>
      <param name="retainChameleons" type="xs:boolean?" default="false"/>
      <pgroup name="input" minOccurs="1"/>
    </operation>
    <operation name="shacl" type="item()" func="shaclOp" mod="shax.xqm" namespace="http://www.ttools.org/shax/ns/xquery-functions">
      <param name="shax" type="docFOX" sep="SC" pgroup="input"/>
      <pgroup name="input" minOccurs="1"/>
      <param name="format" type="xs:string?" default="ttl" fct_values="xml, ttl"/>
      <param name="deep" type="xs:boolean?" default="false"/>
    </operation>
    <operation name="globalizeTypes" type="element()?" func="globalizeTypesOp" mod="typeGlobalizer.xqm" namespace="http://www.ttools.org/shax/ns/xquery-functions">
      <param name="xsd" type="docFOX*" sep="SC" pgroup="in" fct_minDocCount="1"/>
      <param name="xsds" type="docCAT*" sep="SC" pgroup="in"/>
      <param name="odir" type="directory?" fct_dirExists="true"/>
      <pgroup name="in" minOccurs="1"/>
    </operation>
    <operation name="localTypesReport" type="element()?" func="localTypesReportOp" mod="typeGlobalizer.xqm" namespace="http://www.ttools.org/shax/ns/xquery-functions">
      <param name="xsd" type="docFOX*" sep="SC" pgroup="in" fct_minDocCount="1"/>
      <param name="xsds" type="docCAT*" sep="SC" pgroup="in"/>
      <param name="skipAnno" type="xs:boolean?" default="true"/>
      <pgroup name="in" minOccurs="1"/>
    </operation>
    <operation name="rdfe" type="item()" func="rdfeOp" mod="xml2rdf.xqm" namespace="http://www.ttools.org/shax/ns/xquery-functions">
      <param name="dox" type="docFOX+" fct_minDocCount="1" sep="SC"/>
      <param name="jox" type="jsonFOX*" fct_minDocCount="0" sep="SC"/>
      <param name="semap" type="docFOX?" fct_minDocCount="0" pgroup="semapSepro"/>
      <param name="sepro" type="docFOX?" pgroup="semapSepro"/>
      <param name="r1" type="xs:integer?"/>
      <param name="r2" type="xs:integer?"/>
      <param name="blankNodes" type="xs:boolean?" default="false"/>
      <param name="format" type="xs:string?" fct_values="xmlaug, xtriples0, xtriples, triples" default="triples"/>
      <param name="nons" type="xs:string?"/>
      <param name="prefixNons" type="xs:string?"/>
      <pgroup name="semapSepro" minOccurs="1"/>
    </operation>
    <operation name="xsd" type="item()*" func="xsdOp" mod="xsd.xqm" namespace="http://www.ttools.org/shax/ns/xquery-functions">
      <param name="shax" type="docFOX+" sep="SC" fct_minDocCount="1" pgroup="input"/>
      <param name="odir" type="directory?" fct_dirExists="true"/>
      <param name="ofile" type="xs:string?" default="#stdout"/>
      <param name="osuffixes" type="xs:string*"/>
      <pgroup name="input" minOccurs="1"/>
    </operation>
    <operation name="xsd2shax" type="element()" func="xsd2shaxOp" mod="xsd2shax.xqm" namespace="http://www.ttools.org/shax/ns/xquery-functions">
      <param name="xsd" type="docFOX+" sep="SC"/>
      <param name="ignoreAnno" type="xs:boolean?" default="false"/>
    </operation>
    <operation name="_help" func="_help" mod="tt/_help.xqm">
      <param name="default" type="xs:boolean" default="false"/>
      <param name="type" type="xs:boolean" default="false"/>
      <param name="mode" type="xs:string" default="overview" fct_values="overview, scheme"/>
      <param name="ops" type="nameFilter?"/>
    </operation>
  </operations>
  <types/>
  <facets/>
</topicTool>;

declare variable $req as element() := tt:loadRequest($request, $toolScheme);


(:~
 : Executes pseudo operation '_storeq'. The request is stored in
 : simplified form, in which every parameter is represented by a 
 : parameter element whose name captures the parameter value
 : and whose text content captures the (unitemized) parameter 
 : value.
 :
 : @param request the request element
 : @return the operation result
 :)
declare function m:execOperation__storeq($request as element())
        as node() {
    element {node-name($request)} {
        attribute crTime {current-dateTime()},
        
        for $c in $request/* return
        let $value := replace($c/@paramText, '^\s+|\s+$', '', 's')
        return
            element {node-name($c)} {$value}
    }       
};

    
(:~
 : Executes operation '_dcat'.
 :
 : @param request the request element
 : @return the operation result
 :)
declare function m:execOperation__dcat($request as element())
        as element() {
    tt:getRcat($request)        
};
     
(:~
 : Executes operation '_docs'.
 :
 : @param request the request element
 : @return the operation result
 :)
declare function m:execOperation__docs($request as element())
        as element()+ {
    tt:getDocs($request)        
};
     
(:~
 : Executes operation '_doctypes'.
 :
 : @param request the request element
 : @return the operation result
 :)
declare function m:execOperation__doctypes($request as element())
        as node() {
    tt:getDoctypes($request)        
};
     
(:~
 : Executes operation '_search'.
 :
 : @param request the request element
 : @return the operation result
 :)
declare function m:execOperation__search($request as element())
        as node() {
    tt:search($request)        
};
     
(:~
 : Executes operation '_searchCount'.
 :
 : @param request the request element
 : @return the operation result
 :)
declare function m:execOperation__searchCount($request as element())
        as item() {
    tt:searchCount($request)        
};
     
(:~
 : Executes operation '_createNcat'.
 :
 : @param request the request element
 : @return the operation result
 :)
declare function m:execOperation__createNcat($request as element())
        as node() {
    tt:createNcat($request)        
};
     
(:~
 : Executes operation '_feedNcat'.
 :
 : @param request the request element
 : @return the operation result
 :)
declare function m:execOperation__feedNcat($request as element())
        as node() {
    tt:feedNcat($request)        
};
     
(:~
 : Executes operation '_copyNcat'.
 :
 : @param request the request element
 : @return the operation result
 :)
declare function m:execOperation__copyNcat($request as element())
        as node() {
    tt:copyNcat($request)        
};
     
(:~
 : Executes operation '_deleteNcat'.
 :
 : @param request the request element
 : @return the operation result
 :)
declare function m:execOperation__deleteNcat($request as element())
        as node() {
    tt:deleteNcat($request)        
};
     
(:~
 : Executes operation '_nodlSample'.
 :
 : @param request the request element
 : @return the operation result
 :)
declare function m:execOperation__nodlSample($request as element())
        as node() {
    tt:nodlSample($request)        
};
     
(:~
 : Executes operation 'load'.
 :
 : @param request the request element
 : @return the operation result
 :)
declare function m:execOperation_load($request as element())
        as node() {
    a1:loadOp($request)        
};
     
(:~
 : Executes operation 'shacl'.
 :
 : @param request the request element
 : @return the operation result
 :)
declare function m:execOperation_shacl($request as element())
        as item() {
    a1:shaclOp($request)        
};
     
(:~
 : Executes operation 'globalizeTypes'.
 :
 : @param request the request element
 : @return the operation result
 :)
declare function m:execOperation_globalizeTypes($request as element())
        as element()? {
    a1:globalizeTypesOp($request)        
};
     
(:~
 : Executes operation 'localTypesReport'.
 :
 : @param request the request element
 : @return the operation result
 :)
declare function m:execOperation_localTypesReport($request as element())
        as element()? {
    a1:localTypesReportOp($request)        
};
     
(:~
 : Executes operation 'rdfe'.
 :
 : @param request the request element
 : @return the operation result
 :)
declare function m:execOperation_rdfe($request as element())
        as item() {
    a1:rdfeOp($request)        
};
     
(:~
 : Executes operation 'xsd'.
 :
 : @param request the request element
 : @return the operation result
 :)
declare function m:execOperation_xsd($request as element())
        as item()* {
    a1:xsdOp($request)        
};
     
(:~
 : Executes operation 'xsd2shax'.
 :
 : @param request the request element
 : @return the operation result
 :)
declare function m:execOperation_xsd2shax($request as element())
        as element() {
    a1:xsd2shaxOp($request)        
};
     
(:~
 : Executes operation '_help'.
 :
 : @param request the request element
 : @return the operation result
 :)
declare function m:execOperation__help($request as element())
        as node() {
    tt:_help($request, $toolScheme)        
};

(:~
 : Executes an operation.
 :
 : @param req the operation request
 : @return the result of the operation
 :)
declare function m:execOperation($req as element())
      as item()* {
    if ($req/self::zz:errors) then tt:_getErrorReport($req, 'Invalid call', 'code', ()) else
    if ($req/@storeq eq 'true') then m:execOperation__storeq($req) else
    
    let $opName := tt:getOperationName($req) 
    let $result :=    
        if ($opName eq '_help') then m:execOperation__help($req)
        else if ($opName eq '_dcat') then m:execOperation__dcat($req)
        else if ($opName eq '_docs') then m:execOperation__docs($req)
        else if ($opName eq '_doctypes') then m:execOperation__doctypes($req)
        else if ($opName eq '_search') then m:execOperation__search($req)
        else if ($opName eq '_searchCount') then m:execOperation__searchCount($req)
        else if ($opName eq '_createNcat') then m:execOperation__createNcat($req)
        else if ($opName eq '_feedNcat') then m:execOperation__feedNcat($req)
        else if ($opName eq '_copyNcat') then m:execOperation__copyNcat($req)
        else if ($opName eq '_deleteNcat') then m:execOperation__deleteNcat($req)
        else if ($opName eq '_nodlSample') then m:execOperation__nodlSample($req)
        else if ($opName eq 'load') then m:execOperation_load($req)
        else if ($opName eq 'shacl') then m:execOperation_shacl($req)
        else if ($opName eq 'globalizeTypes') then m:execOperation_globalizeTypes($req)
        else if ($opName eq 'localTypesReport') then m:execOperation_localTypesReport($req)
        else if ($opName eq 'rdfe') then m:execOperation_rdfe($req)
        else if ($opName eq 'xsd') then m:execOperation_xsd($req)
        else if ($opName eq 'xsd2shax') then m:execOperation_xsd2shax($req)
        else if ($opName eq '_help') then m:execOperation__help($req)
        else
        tt:createError('UNKNOWN_OPERATION', concat('No such operation: ', $opName), 
            <error op='{$opName}'/>)    
     let $errors := if ($result instance of node()+) then tt:extractErrors($result) else ()     
     return
         if ($errors) then tt:_getErrorReport($errors, 'System error', 'code', ())     
         else $result
};

m:execOperation($req)
    