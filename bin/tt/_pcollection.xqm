xquery version "3.0";
(:
 :***************************************************************************
 :
 : pcollection.xqm - functions for managing and using p-faced collections
 :
 :***************************************************************************
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
      <operation name="_search" type="node()" func="search">     
         <param name="nodl" type="docURI" fct_rootElem="Q{http://www.infospace.org/pcollection}nodl"/> 
         <param name="query" type="xs:string?"/>         
      </operation>   
      <operation name="_searchCount" type="item()" func="searchCount">     
         <param name="nodl" type="docURI" fct_rootElem="Q{http://www.infospace.org/pcollection}nodl"/> 
         <param name="query" type="xs:string?"/>         
      </operation>   
      <operation name="_createNcat" type="node()" func="createNcat">     
         <param name="nodl" type="docURI" fct_rootElem="Q{http://www.infospace.org/pcollection}nodl"/> 
      </operation>
      <operation name="_feedNcat" type="node()" func="feedNcat">     
         <param name="nodl" type="docURI" fct_rootElem="Q{http://www.infospace.org/pcollection}nodl"/>
         <param name="doc" type="docURI*" sep="WS"/>         
         <param name="docs" type="catDFD*" sep="SC"/>
         <param name="dox" type="catFOX*" sep="SC"/>         
         <param name="path" type="xs:string?"/>
      </operation>
      <operation name="_copyNcat" type="node()" func="copyNcat">
         <param name="nodl" type="docURI?" fct_rootElem="Q{http://www.infospace.org/pcollection}nodl"/>
         <param name="query" type="xs:string?"/>         
         <param name="toNodl" type="docURI" fct_rootElem="Q{http://www.infospace.org/pcollection}nodl"/>         
      </operation>
      <operation name="_deleteNcat" type="node()" func="deleteNcat">     
         <param name="nodl" type="docURI" fct_rootElem="Q{http://www.infospace.org/pcollection}nodl"/> 
      </operation>
      <operation name="_nodlSample" type="node()" func="nodlSample">     
         <param name="model" type="xs:string?" fct_values="xml, sql, mongo" default="xml"/>       
      </operation>
    </operations>      
:)  

module namespace f="http://www.ttools.org/xquery-functions";
import module namespace tt="http://www.ttools.org/xquery-functions" at 
    "_errorAssistent.xqm",
    "_log.xqm",
    "_nameFilter.xqm",
    "_pcollection_sql.xqm",
    "_pcollection_utils.xqm",
    "_pcollection_xml.xqm",
    "_pfilter.xqm",    
    "_pfilter_parser.xqm",
    "_request.xqm",
    "_reportAssistent.xqm";

declare namespace z="http://www.ttools.org/structure";
declare namespace pc="http://www.infospace.org/pcollection";

(:
 : ============================================================================
 :
 :     o p e r a t i o n s
 :
 : ============================================================================
 :)
 
 (:~
  : Filters a p-collection and returns the matching nodes.
  :
  : @param request the operation request
  : @return the matching nodes, wrapped in a container element
  :)
declare function f:search($request as element())
        as element() {
    let $nodl := tt:getParam($request, 'nodl')/*
    let $query := tt:getParam($request, 'query')
    let $docs := f:filteredCollection($nodl, $query)    
    let $docReport :=    
        <filteredCollection count="{count($docs)}">{
            $docs
        }</filteredCollection>        
    return
        $docReport
};

 (:~
  : Filters a p-collection and returns the number of matching nodes.
  :
  : @param request the operation request
  : @return the matching nodes, wrapped in a container element
  :)
declare function f:searchCount($request as element())
        as element() {
    let $nodl := tt:getParam($request, 'nodl')/*
    let $query := tt:getParam($request, 'query')
    let $count := f:filteredCollectionCount($nodl, $query)    
    let $docReport :=    
        <filteredCollection count="{$count}"/>
    return
        $docReport
};

(:~
 : Creates an Ncat.
 :
 : @param request the operation request
 : @return a report describing the operation result
 :) 
declare function f:createNcat($request as element())
        as element() {
    let $nodl := tt:getParams($request, 'nodl')/*
    let $enodl := f:_extendedNodl($nodl)
    let $xml := $enodl//pc:ncatModel/pc:xmlNcat
    return  
        if ($xml) then f:_createXmlNcat($enodl, $request) else
    let $sql := $enodl//pc:ncatModel/pc:sqlNcat
    return
        if ($sql) then f:_createSqlNcat($enodl, $request) else
            tt:createError('UNEXPECTED_NCAT_MODEL', 
                concat('Child elems of ncat: ', 
                    string-join($enodl//pc:ncatModel/*/name(), ', ')), ())
};

(:~
 : Loads documents into a p-collection.
 :
 : @param request the operation request
 : @return a report describing ...
 :) 
declare function f:feedNcat($request as element())
        as element() {
    let $nodl := tt:getParams($request, 'nodl')/*
    let $enodl := tt:log(f:_extendedNodl($nodl), 2, 'ENODL: ')    
    return
        if ($nodl/pc:ncatModel/pc:xmlNcat) then f:_feedXmlNcat($enodl, $request) else  
        if ($nodl/pc:ncatModel/pc:sqlNcat) then f:_feedSqlNcat($enodl, $request) else
            tt:createError('UNEXPECTED_NCAT_TYPE', concat(
                'Unexpected ncat type: ', $nodl/@ncatType), ())
};

(:~
 : Copys pnodes from one ncat into another.
 :
 : @param request the operation request
 : @return a report describing ...
 :) 
declare function f:copyNcat($request as element())
        as element() {
    let $toNodl := tt:getParams($request, 'toNodl')/*
    let $toEnodl := f:_extendedNodl($toNodl)   
        
    let $nodl := tt:getParams($request, 'nodl')/*
    let $query := tt:getParams($request, 'query')
    let $queryParsed := tt:parsePfilter($query)    
    let $enodl := f:_extendedNodl($nodl)

    let $ncatType := $enodl/pc:ncatModel/
        (if (pc:sqlNcat) then 'sql' else if (pc:xmlNcat) then 'xml' else 'xml')
    let $toNcatType := $toEnodl/pc:ncatModel/
        (if (pc:sqlNcat) then 'sql' else if (pc:xmlNcat) then 'xml' else 'xml')

    return
        let $pnodes := 
            if ($ncatType eq 'sql') then f:_getPnodes_sql($enodl, $queryParsed) else
            f:_getPnodes_xml($enodl, $queryParsed)
        let $result :=
            if ($toNcatType eq 'sql') then f:_insertPnodes_sql($toEnodl, $pnodes) else
            f:_insertPnodes_xml($toEnodl, $pnodes)
        return
            $result
};

(:~
 : Delete an Ncat.
 :
 : @param request the operation request
 : @return a report describing ...
 :) 
declare function f:deleteNcat($request as element())
        as element() {
    let $nodl := tt:getParams($request, 'nodl')/*
    let $enodl := f:_extendedNodl($nodl)    
    return
        if ($nodl/pc:ncatModel/pc:xmlNcat) then f:_deleteXmlNcat($enodl, $request) else  
        if ($nodl/pc:ncatModel/pc:sqlNcat) then f:_deleteSqlNcat($enodl, $request) else
            tt:createError('UNEXPECTED_NCAT_TYPE', concat(
                'Unexpected ncat type: ', $nodl/@ncatType), ())
};

(:~
 : Document me!
 :
 : @param request the operation request
 : @return a report describing ...
 :) 
declare function f:nodlSample($request as element())
        as element() {
    let $model := tt:getParams($request, 'model')
    return
    
        <nodl xmlns="http://www.infospace.org/pcollection">
            <collection name="COLLECTION_NAME" uri="" formats="xml" doc="A collection of FOOs."/>
            <pmodel>
                <property name="foo" type="xs:string*" maxLength="100" expr="//foo"/>       
                <property name="bar" type="xs:string" maxLength="100" expr="//bar"/>        
                <property name="foobar" type="xs:string*" maxLength="100" expr="//foobar"/>
                <anyProperty/>        
            </pmodel>
            <nodeConstructor kind="SELECT_ONE: uri|text"/>    
            <ncatModel>{
                if ($model eq 'xml') then
                    <xmlNcat documentURI="DOC_URI" asElement="*foo* *bar*"/> else
                if ($model eq 'sql') then
                    <sqlNcat rdbms="MySQL" host="localhost" db="DB" user="USER" password="PASSWORD"/> else        
                ()
            }</ncatModel>        
        </nodl>   
};

(:
 : ============================================================================
 :
 :     p u b l i c    f u n c t i o n s
 :
 : ============================================================================
 :)

(:~
 : Returns a filtered pcollection. If no query is specified, the complete collection
 : is returned, otherwise only those collection members whose external properties
 : match the query.
 :
 : @param nodl the NODL document describing the collection
 : @param query a pfilter against which the external properties of the collection 
 :    members are matched
 : @return all collection members whose external properties match the specified
 :    pfilter, or all collection members if no pfilter has been specified
 :) 
declare function f:filteredCollection($nodl as element(pc:nodl), $query as item()?)
        as node()* {
    let $enodl := f:_extendedNodl($nodl) 
    let $dummy := file:write('/projects/infospace/pcol/enodl.xml', $enodl)
    let $pfilter := 
        if ($query instance of element(pc:pfilter)) then $query else
            tt:parsePfilter($query)
    let $errors := tt:wrapErrors($pfilter)
    return if ($errors) then $errors else
        
    let $ncatModel := $enodl//pc:ncatModel/*
    return
        if ($ncatModel/self::pc:xmlNcat) then f:_filteredCollection_xml($enodl, $pfilter)
        else if ($ncatModel/self::pc:sqlNcat) then f:_filteredCollection_sql($enodl, $pfilter)
        else
            let $modelElemName := local-name($ncatModel)
            let $problem := if ($modelElemName = ('xmlNcat', 'sqlNcat', 'mongoNcat')) then 'unsupported'
                            else 'unknown'
            let $msg :=
                if (not($ncatModel)) then "NODL does not specify an NCAT model"
                else concat("NODL uses an ", $problem, " NCAT type ",
                    "(model element name: '", $modelElemName, "')")
            return
                tt:createError('INVALID_NODL', concat($msg, '; NODL: ', $enodl/@uri), ())
};

(:~
 : Returns the size of a filtered pcollection. If no query is specified, the 
 : collection size is returned, otherwise the number of collection members
 : matching the query.
 :
 : @param nodl the NODL document describing the collection
 : @param query a pfilter against which the external properties of the collection 
 :    members are matched
 : @return all collection members whose external properties match the specified
 :    pfilter, or all collection members if no pfilter has been specified
 :) 
declare function f:filteredCollectionCount($nodl as element(pc:nodl), $query as item()?)
        as item() {
    let $enodl := f:_extendedNodl($nodl)
    let $pfilter := 
        if ($query instance of element(pc:pfilter)) then $query else
            tt:parsePfilter($query)
    let $errors := tt:wrapErrors($pfilter)
    return if ($errors) then $errors else
        
    let $ncatModel := $enodl//pc:ncatModel/*
    return
        if ($ncatModel/self::pc:xmlNcat) then error()
        else if ($ncatModel/self::pc:sqlNcat) then f:_filteredCollectionCount_sql($enodl, $pfilter)
        else
            let $modelElemName := local-name($ncatModel)
            let $problem := if ($modelElemName = ('xmlNcat', 'sqlNcat', 'mongoNcat')) then 'unsupported'
                            else 'unknown'
            let $msg :=
                if (not($ncatModel)) then "NODL does not specify an NCAT model"
                else concat("NODL uses an ", $problem, " NCAT type ",
                    "(model element name: '", $modelElemName, "')")
            return
                tt:createError('INVALID_NODL', concat($msg, '; NODL: ', $enodl/@uri), ())
};

(:
 : ============================================================================
 :
 :     p r i v a t e    f u n c t i o n s
 :
 : ============================================================================
 :)
