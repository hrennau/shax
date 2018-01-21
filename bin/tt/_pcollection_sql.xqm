(:
 :***************************************************************************
 :
 : pcollection_sql.xqm - functions for managing and searching sql-based p-faced collections
 :
 :***************************************************************************
 :)
 
module namespace f="http://www.ttools.org/xquery-functions";
import module namespace tt="http://www.ttools.org/xquery-functions" at 
    "_errorAssistent.xqm",    
    "_nameFilter.xqm",
    "_pcollection_utils.xqm",    
    "_pfilter.xqm",    
    "_pfilter_parser.xqm",
    "_request.xqm",
    "_reportAssistent.xqm",
    "_resourceAccess.xqm",
    "_sqlExecutor.xqm";

declare namespace z="http://www.ttools.org/structure";
declare namespace pc="http://www.infospace.org/pcollection";

(:
 : ============================================================================
 :
 :     p r i v a t e    f u n c t i o n s
 :
 : ============================================================================
 :)

(:~
 : Returns a filtered pcollection. If no query is specified, the complete collection
 : is returned, otherwise only those collection members whose external properties
 : match the query.
 :
 : @param enodl the extended NODL document describing the collection
 : @param query a pfilter against which the external properties of the collection 
 :    members are matched
 : @return all collection members whose external properties match the specified
 :    pfilter, or all collection members if no pfilter has been specified
 :) 
declare function f:_filteredCollection_sql($enodl as element(pc:enodl), $pfilter as element(pc:pfilter)?)
        as node()* {
        
    let $resourceCol := 
        $enodl//pc:sqlNcat/pc:tables/pc:table[@main eq 'true']/
            @resourceCol/tokenize(., '\s+')[1]

    let $select := f:_getSelect($enodl, $pfilter, ())
    let $conn := f:_connectForExtendedNodl($enodl)
    let $retOpenDb := f:_openDbForExtendedNodl($conn, $enodl)    
    let $sqlSelect := tt:log( tt:writeSql($select), 0, 'SELECT:&#xA;')
    let $retSelect := tt:execute($conn, $sqlSelect)
    let $docs :=
        if ($resourceCol eq '_node_text') then
            for $col in $retSelect//sql:column[@name eq '_node_text'] return parse-xml($col)
        else 
            for $uri in distinct-values($retSelect//sql:column[@name eq '_node_uri']) 
            return if (not(tt:doc-available($uri))) then () else tt:doc($uri)        
    return
        $docs   
};

(:~
 : Returns the size of a filtered pcollection. If no query is specified, the 
 : collection size is returned, otherwise the number of collection members
 : matching the query.
 :
 : @param enodl the extended NODL document
 : @param pfilter a query filtering the pcollection
 : @return the size of the filtered or unfiltered collection
 :) 
declare function f:_filteredCollectionCount_sql($enodl as element(pc:enodl), $pfilter as element(pc:pfilter)?)
        as xs:integer {
    let $resourceCol := 
        $enodl//pc:sqlNcat/pc:tables/pc:table[@main eq 'true']/
            @resourceCol/tokenize(., '\s+')[1]
    let $selectCol := concat('count(', $resourceCol, ')')
    let $select := f:_getSelect($enodl, $pfilter, true())
    let $conn := f:_connectForExtendedNodl($enodl)
    let $retOpenDb := f:_openDbForExtendedNodl($conn, $enodl)    
    let $sqlSelect := tt:log( tt:writeSql($select), 0, 'SELECT:&#xA;')
    let $retSelect := tt:execute($conn, $sqlSelect)
    return
        $retSelect[1]//sql:column[@name eq $selectCol]/xs:integer(.)
};

(:~
 : Returns an XML representation of the select query implied by a pfilter.
 :
 : Note. The tables clause contains the main table and all other tables required
 : by the query.
 :
 : @param enodl extended NODL
 : @param pfilter a pfilter
 : @return the select statement, represented in XML
 :)
declare function f:_getSelect($enodl as element(pc:enodl), 
                              $pfilter as element(pc:pfilter)?,
                              $count as xs:boolean?)
        as element() {
    let $tables := $enodl//pc:sqlNcat/pc:tables/pc:table        
    let $mainTable := $tables[@main eq 'true'] 
    let $otherTables := $tables except $mainTable
    let $resourceCol := $mainTable/@resourceCol/tokenize(., '\s+')[1]
    let $selectCol := 
        if (not($count)) then $resourceCol else concat('count(', $resourceCol, ')')
        
    (: $reqTables - the tables containing one of the columns referenced by the query :)
    let $reqTableNames := 
        let $pnames := $pfilter//pc:p/@name/lower-case(.)
        return $tables[pc:col/@name/lower-case(.) = $pnames]/@name        
    let $mainTableName := $mainTable/@name
    let $otherTableNames := $reqTableNames[not(. eq $mainTableName)]
        
    (: the table clause lists all required tables -
       the main table and the required other tables :)
    let $tablesClause :=
        <tables>{
            <table name="{$mainTableName}">{
                if (empty($otherTableNames)) then () else attribute alias {'t1'}
            }</table>,
            for $t at $pos in $otherTableNames
            let $alias := concat('t', $pos + 1)
            let $onName := 't1.nkey'
            let $onOp := '='
            let $onValue := concat('t', $pos + 1, '.nkey')
            return
                <table name="{$t}" alias="{$alias}"  join="left">{
                    <on>{
                        <col name="{$onName}" op="{$onOp}" value="{$onValue}"/>
                    }</on>
                }</table>
        }</tables>
    
    let $whereClause :=
        if (not($pfilter)) then () else 
            <where text="{tt:pfilterWhereClause($pfilter)}"/>
    return
        <select cols="{$selectCol}">{
            $tablesClause,
            $whereClause
        }</select>
};

(:~
 : Creates the database and tables required by the ncat
 : of a pcollection, as specified by a nodl document.
 :
 : @param request the operation request
 : @return a report describing ...
 :) 
declare function f:_createSqlNcat($enodl as element(pc:enodl), $request as element())
        as element() {
    let $sql := $enodl//pc:sqlNcat
    let $db := $sql/pc:connection/@db
    let $tables := $sql/pc:tables/pc:table
    let $mainTable := $tables[@main eq 'true']
    let $otherTables := $tables except $mainTable

    let $createTable_main :=      
        <createTable name="{$mainTable/@name}" db="{$db}" ifNotExists="true">{
            $mainTable/*/tt:rmElemNamespaces(.)
        }</createTable>
        
    let $createTable_other :=
        for $t in $otherTables
        return
            <createTable name="{$t/@name}" db="{$db}" ifNotExists="true">{
                $t/*/f:rmElemNamespaces(.)
            }</createTable>
            
    let $createDb :=    
        <createDb name="{$db}" ifNotExists="true">{       
            if (not($sql/pc:charset/@name)) then () else attribute charset {$sql/pc:charset/@name},
            if (not($sql/pc:charset/@collation)) then () else attribute collation {$sql/pc:charset/@collation}           
        }</createDb>
    return
        try {
            let $conn := f:_connectForExtendedNodl($enodl)  
            let $retCreateDb := f:_createDbForExtendedNodl($conn, $enodl)   
            let $retCreateTableMain := tt:sqlCreateTable($conn, $db, $createTable_main)
            let $retCreateTableOther :=
                for $t in $createTable_other return tt:sqlCreateTable($conn, $db, $t)
            let $countTables := 1 + count($createTable_other)            
            return
                <z:createSqlNcat countTables="{$countTables}">{
                    ()
                    (:
                    $enodl,                
                    f:sqlInfoColumns($conn, $db, (), (), ())
                    :)
                }</z:createSqlNcat>
        } catch * {
            tt:createError('SQL_ERROR', $err:description, ())        
        }
};        

(:~
 : Deletes the tables required by a pcollection. Note that the database is
 : not deleted, even if it does not contain any tables except for those now
 : deleted.
 :
 : @param enodl an extended NODL
 : @param request the operation request
 : @return a report describing the Ncat deletion
 :) 
declare function f:_deleteSqlNcat($enodl as element(pc:enodl), $request as element())
        as element() {
    let $sql := $enodl//pc:sqlNcat
    let $db := $sql/pc:connection/@db
    let $tableMain := $sql/pc:tables/pc:table[@main eq 'true']/@name
    let $tablesOther := $sql/pc:tables/pc:table[not(@main eq 'true')]/@name/string()    
    return
        try {
            let $conn := f:_connectForExtendedNodl($enodl)
            let $countDel := 
                sum(for $t in $tablesOther return tt:sqlDropTable($conn, $db, $t))
            let $countDel := $countDel + tt:sqlDropTable($conn, $db, $tableMain)
            return
                <z:deleteNcat nodl="{$enodl/@nodlURI}" countDeletedTables="{$countDel}"/>
        } catch * {
            tt:createError('SQL_ERROR', $err:description, ())        
        }
};        

(:~
 : Feeds an ncat with pnodes created for a set of XML documents.
 :
 : @param request the operation request
 : @return a report describing ...
 :) 
declare function f:_feedSqlNcat($enodl as element(pc:enodl), $request as element())
        as element() {
    let $dcat := tt:getParams($request, 'docs dox')  
    let $doc := tt:getParams($request, 'doc')
    let $path := tt:getParams($request, 'path')
    
    (: create and add pnodes :)
   
    let $conn := tt:_connectForExtendedNodl($enodl)   
    let $retSqlUse := tt:_openDbForExtendedNodl($conn, $enodl)    

    let $nodeConstructor := $enodl/pc:nodeConstructor
    let $pmodel := $enodl/pc:pmodel    
    let $processingPlan := f:_sqlProcessingPlan($enodl)
    
    let $results := (   
        for $d in $doc
        let $uri := if ($path) then () else document-uri($d)
        let $nodes := if (not($path)) then $d else tt:evaluate($path, $d) 
        for $node in $nodes
        let $node := if ($node/self::document-node()) then $node else document {$node}
        let $pnode := tt:_pnode($node, $uri, $pmodel, $nodeConstructor)
        return
            f:_insertPnode_sql($conn, (), $uri, $pnode, $node, $processingPlan)
        ,       
        for $href in $dcat//@href
        let $uri := $href/resolve-uri(., base-uri(..))
        return
            f:_createAndInsertPnode_sql($uri, $path, $conn, 
                $nodeConstructor, $pmodel, $processingPlan)
    )
    return
        <feedNcat name="{$enodl//pc:collection/@name}"/>

(:
    let $dcat := tt:getParams($request, 'docs')
    let $processingPlan := f:_sqlProcessingPlan($enodl)   
    (: let $dummy := file:write('/projects/infospace/pcol/processingPlan.xml', $processingPlan) :)
    
    (: add nodes from dcat :)
    let $conn := tt:_connectForExtendedNodl($enodl)
    let $retSqlUse := tt:_openDbForExtendedNodl($conn, $enodl)    
    let $results := 
        let $pmodel := $enodl/pc:pmodel
        let $nodeConstructor := $enodl/pc:nodeConstructor        
        for $href in $dcat//@href
        let $uri := $href/resolve-uri(., base-uri(..))
        let $node := tt:doc($uri)
        let $pnode := tt:_pnode($node, $uri, $pmodel, $nodeConstructor)
        return
            f:_insertPnode_sql($conn, (), $uri, $pnode, $node, $processingPlan)
    return
        <feedNcat name="{$enodl//pc:collection/@name}" countProcessed="{count($dcat//@href)}">{$results}</feedNcat>
:)
};

(:~
 : Creates a plan how to process a resource in order to fill the ncat with
 : all resource-related information.
 :
 : Example:
 : <sqlProcessingPlan>
 :   <connection host="localhost" db="pcol" user="root" password="admin"/>
 :   <mainTable db="pcol" table="priscilla_ncat" resourceCol="_node_uri" nodeConstructorKind="uri">
 :     <setProperty name="tns" expr="/xs:schema/@targetNamespace" 
 :                  sqlType="VARCHAR(100)" optional="true"/>
 :   </mainTable>
 :   <otherTables>
 :     <setProperty name="elem" expr="/*/xs:element/@name" table="priscilla_ncat_elem" 
 :                  sqlType="VARCHAR(100)" minOccurs="0" maxOccurs="-1"/>
 :     <setProperty name="stype" expr="/*/xs:simpleType/@name" table="priscilla_ncat_stype" 
 :                  sqlType="VARCHAR(100)" minOccurs="0" maxOccurs="-1"/>
 :     <setProperty name="ctype" expr="/*/xs:complexType/@name" table="priscilla_ncat_ctype" 
 :                  sqlType="VARCHAR(100)" minOccurs="0" maxOccurs="-1"/>
 :   </otherTables>
 ; </sqlProcessingPlan> 
 :
 : @param enodl an extended nodl
 : @return a 'sqlPlan' element describing the required processing
 :)
declare function f:_sqlProcessingPlan($enodl as element(pc:enodl))
        as element(sqlProcessingPlan) {
    let $sql := $enodl/pc:ncatModel/pc:sqlNcat
    let $tables := $sql/pc:tables
    let $connection := $sql/pc:connection    
    let $pmodel := $enodl/pc:pmodel
    let $nodeConstructorKind := $enodl/pc:nodeConstructor/@kind/tokenize(., '\s*\+\s*')
    let $resourceCol := 
        let $values := (
            '_node_text'[$nodeConstructorKind = 'text'],
            '_node_uri'[$nodeConstructorKind = 'uri']
        )
        return
            if (exists($values)) then $values else '_node_uri'
    
    let $db := $connection/@db
    let $mainTable := $tables/pc:table[@main eq 'true']/@name      
        
    let $psetters :=
        for $p in $pmodel/pc:property
        let $name := $p/@name/string()
        let $expr := $p/@expr

        let $isMultiple := $p/@maxOccurs ne '1'
        let $table :=
            if (not($isMultiple)) then $tables/pc:table[@main eq 'true']
            else $tables/pc:table[pc:col/@name = $name]
        let $col := $table/pc:col[@name eq $name]
        
        let $tableAtt := 
            if (not($isMultiple)) then () else
                attribute table {$table/@name}
        let $sqlTypeAtt := attribute sqlType {$col/@type}                
        let $occAtts :=
            if ($isMultiple) then (
                $p/@minOccurs,
                $p/@maxOccurs
            ) else
                attribute optional {$p/@minOccurs eq '0'}
        return
            <setProperty name="{$name}" expr="{$expr}">{
                $tableAtt,
                $sqlTypeAtt,
                $occAtts
            }</setProperty>
    return
        <sqlProcessingPlan>{
            $connection/f:rmElemNamespaces(.),
            <mainTable db="{$db}" table="{$mainTable}" resourceCol="{$resourceCol}" nodeConstructorKind="{$nodeConstructorKind}">{
                $psetters[not(@table)]
            }</mainTable>,
            <otherTables>{
                $psetters[@table]
            }</otherTables>
        }</sqlProcessingPlan>            
};

(:~
 : Imports pnodes into a SQL based ncat.
 :
 : @param request the operation request
 : @return a report describing ...
 :) 
declare function f:_insertPnodes_sql($enodl as element(pc:enodl), 
                                     $pnodes as element()*)
        as element() {
    let $processingPlan := tt:log(f:_sqlProcessingPlan($enodl) , 2, 'P_PLAN: ')   
    let $conn := tt:_connectForExtendedNodl($enodl)
    let $retSqlUse := tt:_openDbForExtendedNodl($conn, $enodl)    
    let $results :=
        for $pnode in $pnodes
        return
            f:_insertPnode_sql($conn, (), (), $pnode, (), $processingPlan) 
        
    return
        <copyNcat targetName="{$enodl//pc:collection/@name}" 
                  countCopied="{count($pnodes)}">{$results}</copyNcat>
};

(:~
 : Creates a pnode and imports it into a SQL based ncat.
 :
 : @param uri the node URI
 : @param path a path expression leading from the document root to the node(s) to be imported
 : @param conn a connection handle
 : @param nodeConstructor the node constructor element of an enodl
 : @param pmodel the pmodel element of an enodl
 : @param pp a SQL processing plan
 : @return the empty sequence
 :)
declare function f:_createAndInsertPnode_sql($uri as xs:string,
                                             $path as xs:string?,
                                             $conn as xs:string,
                                             $nodeConstructor as element(pc:nodeConstructor),
                                             $pmodel as element(pc:pmodel),
                                             $pp as element(sqlProcessingPlan))
        as empty-sequence() {
    let $node := tt:doc($uri)
    let $node := 
        if (not($path)) then $node else tt:evaluate($path, $node)
    let $pnode := tt:_pnode($node, $uri, $pmodel, $nodeConstructor)
    return
        f:_insertPnode_sql($conn, (), $uri, $pnode, $node, $pp)
};

(:~
 : Imports a pnode into a SQL based ncat.
 :
 : @param conn a connection handle
 : @param nkey if specified, the node key to be used when inserting node-related rows into the database
 : @param nodeUri the node URI of the node to be processed
 : @param pnode the pnode describing the node
 : @param node the node itself 
 : @param sql a SQL processing plan
 : @return items received as result when inserting rows into the database
 :)
declare function f:_insertPnode_sql($conn as xs:integer,
                                    $nkey as xs:string?,
                                    $nodeUri as xs:string?,
                                    $pnode as element(pc:pnode),
                                    $node as node()?,
                                    $pp as element(sqlProcessingPlan))
        as item()* {
    let $docRoot := $node/descendant-or-self::*[1]    
    let $useNodeUri := ($nodeUri, $pnode/@_node_uri/string())[1]
    let $db := $pp/connection/@db    
    let $mainTable := $pp/mainTable   
    let $mainTableName := $mainTable/@table
    let $otherTables := $pp/otherTables
    let $resourceCol := $mainTable/@resourceCol/tokenize(., '\s+')
    let $nodeConstructorKind := $mainTable/@nodeConstructorKind/tokenize(., '\s+')
    
    (: a query selecting the main table record describing a given node URI :)
    let $sqlSelectNkey :=
        if (not($useNodeUri) or not($resourceCol = '_node_uri')) then () else
            f:writeSql(
                <select db="{$db}" cols="nkey">{
                    <tables>
                        <table name="{$mainTableName}"/>
                    </tables>,
                    <where>
                        <col name="_node_uri" value="{$useNodeUri}"/>
                    </where>                
                }</select>
            )
            
    return (
    
    (: part 1: delete existing entry to be overwritten, if there is one :)
    let $retDeleteRow :=
        let $nkey := 
            if ($nkey) then $nkey
            else if ($sqlSelectNkey) then 
                let $retSelectNkey := f:execute($conn, $sqlSelectNkey)
                return
                    $retSelectNkey//sql:column[@name eq 'nkey']/string()
            else ()
        return
            if (not($nkey)) then () else            
                f:execute($conn,
                    f:writeSql(
                        <delete db="{$db}" table="{$mainTableName}">
                            <where>
                                <col name="nkey">{$nkey}</col>
                            </where>
                        </delete>
                    )
                )
    return
        $retDeleteRow,
    
    (: part 2: perform inserts :)    
    let $mainTableInserts :=
        element {node-name($mainTable)} {
            $mainTable/@*,
            for $p in $mainTable/setProperty
            let $value := tt:_pnodeProperty($pnode, $p/@name)            
            let $sqlValue :=
                let $sqlType := $p/@sqlType                
                return f:_value2sqlValue($value, $sqlType)
            where exists($value)
            return
                <col name="{$p/@name}">{$sqlValue}</col>
        }                
    let $otherTableInserts :=
        element {node-name($otherTables)} {
            $otherTables/@*,
            for $p in $otherTables/setProperty
            let $rawValue := tt:_pnodeProperty($pnode, $p/@name)            
            let $sqlValue :=
                let $sqlType := $p/@sqlType            
                let $maxOccurs := $p/@maxOccurs/xs:integer(.)                
                let $value :=
                    if ($maxOccurs lt 0) then $rawValue
                    else $rawValue[position() le $maxOccurs]
                for $item in $value 
                return 
                    f:_value2sqlValue($item, $sqlType)                
            where exists($rawValue)                
            return
                <col>{
                    $p/@table,                
                    $p/@name,
                    if (count($sqlValue) eq 1) then $sqlValue
                    else for $item in $sqlValue return <item>{$item}</item>
                }</col>
        }
        
    (: if neither an nkey nor a URI is known (or the URI is known but not stored and 
       therefore cannot be used to retrieve the ncat), the nkey must be set to a 
       known value, in order to enable the insertions into other tables :)
    let $targetNkey :=
        if ($nkey) then $nkey
        else if (not($useNodeUri) or not($resourceCol = '_node_uri')) then
            let $sqlSelectMaxNkey := concat('SELECT MAX(nkey) as MAXNKEY FROM ', $mainTableName)
            let $retSelectMaxNkey := f:execute($conn, $sqlSelectMaxNkey)
            let $max := $retSelectMaxNkey//sql:column[@name eq 'MAXNKEY']/xs:integer(.)
            return
                if (not($max)) then 1 
                else $max + 1
        else ()     
        
    let $insertMain :=
        (: main table :)
        <insert db="{$db}" table="{$mainTableName}">{
            if (not($targetNkey)) then () else
                <col name="nkey">{$targetNkey}</col>,
            if (not($resourceCol = '_node_text')) then () else
                let $nodeText :=
                    let $theNode := ($node, tt:_pnodeNode($pnode))[1]
                    return f:_getDocTextForSql($theNode)
                return
                    <col name="_node_text">{$nodeText}</col>,
            if (not($resourceCol = '_node_uri')) then () else
                <col name="_node_uri">{$useNodeUri}</col>,
            $mainTableInserts/*                
        }</insert>      
        
    (: insert main table :)         
    let $sqlInsertMain := f:writeSql($insertMain)  
    let $retInsertMain := f:execute($conn, $sqlInsertMain)    
    
    (: determine nkey to be used for other tables :)    
    let $nkey :=
        if ($targetNkey) then $targetNkey 
        else
            let $sqlLastInsertId := 'SELECT LAST_INSERT_ID()'
            return f:execute($conn, $sqlLastInsertId)/sql:column[1]/string()
(:        
        else if ($sqlSelectNkey) then
            let $retSelectNkey := f:execute($conn, $sqlSelectNkey)
            return
                $retSelectNkey//sql:column[@name eq 'nkey']/string()
        else
            tt:createError('SYSTEM_ERROR', 
                'NKEY should be known at this point of processing', ())
:)            
    (: insert other tables :)            
    let $insertOther :=         
        for $col in $otherTableInserts/col  
        let $values := 
            if ($col/item) then $col/item/string() else $col/string()
        for $value in $values
        return
            <insert db="{$db}" table="{$col/@table}">{
                <col name="nkey">{$nkey}</col>,
                <col>{$col/@name, $value}</col>
            }</insert>

    let $sqlInsertOther := 
        for $cmd in $insertOther return f:writeSql($cmd)
    let $retInsertOther :=
        for $sql in $sqlInsertOther return f:execute($conn, $sql)    

    return (
        $retInsertMain,
        $retInsertOther
    )
    )
};

(:~
 : Retrieves pnodes from a SQL based ncat.
 :
 : @param enodl the extended NODL document describing the ncat
 : @param query only pnodes matching this pfilter are exported
 : @return a sequence of pnodes
 :) 
declare function f:_getPnodes_sql($enodl as element(pc:enodl), $pfilter as element(pc:pfilter)?)
        as node()* {
    let $processingPlan := f:_sqlProcessingPlan($enodl)        
    let $resourceCol := $enodl//pc:sqlNcat/pc:tables/pc:table[@main eq 'true']/@resourceCol/tokenize(., '\s+')
    let $useResourceCol := $resourceCol[1]

    let $select :=
        let $tablesModel := $enodl//pc:sqlNcat/pc:tables
        let $reqTables := 
            let $pnames := $pfilter//pc:p/@name/lower-case(.)
            return
                $tablesModel/pc:table[pc:col/@name/lower-case(.) = $pnames]/@name        
        let $mainTable := $tablesModel/pc:table[@main eq 'true']/@name
        let $otherTables := $reqTables[not(. eq $mainTable)]
        let $cols :=
            string-join((if (empty($otherTables)) then () else 't1', 'nkey'), '.')
        let $tablesClause :=
            <tables>{
                <table name="{$mainTable}">{
                    if (empty($otherTables)) then () else attribute alias {'t1'}
                }</table>,
                for $t at $pos in $otherTables
                let $alias := concat('t', $pos + 1)
                let $onName := 't1.nkey'
                let $onOp := '='
                let $onValue := concat('t', $pos + 1, '.nkey')
                return
                    <table name="{$t}" alias="{$alias}"  join="left">{
                        <on>{
                            <col name="{$onName}" op="{$onOp}" value="{$onValue}"/>
                        }</on>
                    }</table>
            }</tables>
        
        let $whereClause :=
            if (not($pfilter)) then () else
                <where text="{tt:pfilterWhereClause($pfilter)}"/>
        return
            <select cols="{$cols}" distinct="true">{
                $tablesClause,
                $whereClause
            }</select>
            
    let $conn := f:_connectForExtendedNodl($enodl)
    let $retOpenDb := f:_openDbForExtendedNodl($conn, $enodl)    
    let $sqlSelect := tt:log( tt:writeSql($select) , 2, 'SELECT:&#xA;')
    let $retSelect := tt:execute($conn, $sqlSelect)
    let $pnodes :=
        for $nkey in $retSelect//sql:column[@name eq 'nkey']/string()
        return
            f:_getPnode_sql($conn, $nkey, $processingPlan)
    return
        $pnodes
};

(:~
 : Retrieves a pnode from a SQL based ncat.
 :
 : @param conn a connection handle
 : @param nkey the primary key of the node in the main table
 : @return a pnode capturing the node identity and the node properties
 :)
declare function f:_getPnode_sql($conn as xs:integer,
                                 $nkey as xs:string,
                                 $sql as element(sqlProcessingPlan))
        as element(pc:pnode)? {
    let $mainTable := $sql/mainTable   
    let $mainTableName := $mainTable/@table
    let $otherTables := $sql/otherTables
    let $resourceCol := $mainTable/@resourceCol/tokenize(., '\s+')
    let $nodeConstructorKind := $mainTable/@nodeConstructorKind/tokenize(., '\s+')

    let $mainProperties :=
        let $mainSql := concat("SELECT * FROM `", $mainTableName, "` where `NKEY` = '", $nkey, "'")
        let $mainRet := f:execute($conn, $mainSql)
        let $resouceDescriptiors := $mainRet/*[@name = $resourceCol]
        let $docText := 
            let $text := $mainRet/*[@name eq '_node_text']
            return
                if (not($text)) then () else
                    let $ename := QName($tt:URI_PCOLLECTION, 'node')
                    return
                        element {$ename} {parse-xml($text)}
        for $col in $mainRet/*
        let $colName := $col/@name
        where not($colName = ('_node_text'))
        return (
            attribute {$colName} {$col},
            $docText
        )
    let $otherProperties := 
        for $p in $sql/otherTables/setProperty
        let $tname := $p/@table
        let $pname := $p/@name
        
        let $propSql := concat("SELECT `", $pname, "` FROM `", $tname, "` where `NKEY` = '", $nkey, "'")
        let $propRet := f:execute($conn, $propSql)
        let $pvalue := $propRet/*[@name eq $pname]
        where exists($pvalue)
        return
            if (count($pvalue) eq 1) then 
                attribute {$pname} {$pvalue}
            else
                let $ename := QName($tt:URI_PCOLLECTION, $pname)
                return
                    element {$ename} {
                        for $item in $pvalue 
                        let $enameItem := QName($tt:URI_PCOLLECTION, 'item')
                        return 
                            element {$enameItem} {$item/string(.)}
                    }

    return
        <pnode xmlns="http://www.infospace.org/pcollection">{
            ($mainProperties, $otherProperties)[. instance of attribute()],
            ($mainProperties, $otherProperties)[not(. instance of attribute())]            
        }</pnode>
};

(:~
 : Creates a model of the database tables required by a SQL-based ncat
 : which is defined by a given NODL document.
 :
 : @param nodl a NODL document
 : @param pmodel an element expressing the pmodel
 : @return an element expressing the model of required SQL tables
 :)
declare function f:_sqlTablesModel($nodl as element(pc:nodl), $pmodel as element(pc:pmodel))
        as element(pc:tables) {
    let $maxIndexLength := 200        
    let $collectionName := $nodl/pc:collection/@name        
    let $resourceCol :=
        let $reps := 
            $nodl/pc:nodeConstructor/@kind/tokenize(normalize-space(.), '\s*\+\s*')
        let $cnames :=  (
            '_node_text'[$reps = 'text'],
            '_node_uri'[$reps = 'uri']            
        )       
        return
            if (exists($cnames)) then $cnames else '_node_uri'
            
    let $propSingle := $pmodel/pc:property[@maxOccurs eq '1']
    let $propMultiple := $pmodel/pc:property[@maxOccurs ne '1']
    let $tnameMain := concat($collectionName, '_ncat')
    return            
        <tables xmlns="http://www.infospace.org/pcollection">{
            <table name="{$tnameMain}" main="true" resourceCol="{$resourceCol}">{
                <col name="nkey" type="INT UNSIGNED" pkey="true" auto="true"/>,
                if (not($resourceCol = '_node_text')) then () else
                    <col name="_node_text" type="LONGTEXT"/>,
                if (not($resourceCol = '_node_uri')) then () else
                    <col name="_node_uri" type="VARCHAR(400)"/>,
                for $prop in $propSingle
                let $pname := $prop/@name
                let $sqlType := f:_propertySqlType($prop)
                order by lower-case($prop/@name)
                return
                    <col name="{$pname}" type="{$sqlType}" index="true">{
                        if ($prop/@maxLength/xs:integer(.) le $maxIndexLength) then () 
                        else if ($sqlType eq 'BIGINT') then () 
                        else
                            attribute indexLength {200}
                    }</col>,
                if (not($resourceCol = '_node_uri')) then () else                    
                    <unique cols="_node_uri(200)"/>                    
            }</table>,
            
            for $prop in $propMultiple
            let $pname := $prop/@name
            let $sqlType := f:_propertySqlType($prop)            
            let $tname := concat($tnameMain, '_', $pname)
            return
                <table name="{$tname}">{
                    <col name="pkey" type="INT UNSIGNED" pkey="true" auto="true"/>,
                    <col name="nkey" type="INT UNSIGNED"/>,
                    <col name="{$pname}" type="{$sqlType}" index="true">{
                        if ($prop/@maxLength/xs:integer(.) le $maxIndexLength) then ()
                        else if ($sqlType eq 'BIGINT') then ()                        
                        else
                            attribute indexLength {200}
                    }</col>,
                    <fkey name="fk_nkey" cols="nkey" 
                        parent="{$tnameMain}" parentCols="nkey"
                        onDelete="cascade" onUpdate="cascade"/>                    
                }</table>,
            if (not($pmodel/pc:anyProperty)) then () else
                <table name="{concat($tnameMain, '_dyn')}" cols="pkey pname pvalue ncat ">{
                    <col name="pkey" type="INT UNSIGNED" pkey="true" auto="true"/>,
                    <col name="nkey" type="INT UNSIGNED"/>,
                    <col name="pname" type="VARCHAR(50)" index="true"/>,
                    <col name="pvalue" type="VARCHAR(500)"/>,
                    <fkey name="fk_nkey" cols="nkey" 
                        parent="{$tnameMain}" parentCols="nkey"
                        onDelete="cascade" onUpdate="cascade"/>               
                }</table>
        }</tables>       
};        
(:~
 : Maps an property element to an SQL data type.
 :
 : @param rpType the property data type
 : @return the SQL data type
 :)
declare function f:_propertySqlType($p as element())
        as xs:string {
    let $type := $p/@type
    let $maxLength := ($p/@maxLength/xs:integer(.), 400)[1]
    let $itemType := replace($type, '^\s*(\i\c*).*', '$1')
    return
        if ($itemType eq 'xs:dateTime') then 'DATETIME'
        else if ($itemType eq 'xs:boolean') then 'CHAR(5)'        
        else if ($itemType eq 'xs:integer') then 'BIGINT'
        else concat('VARCHAR(', $maxLength, ')')
};        

(:~
 : Opens a connection to the data base specified by a nodl.
 :
 : @param enodel an extended nodl
 : @return the connection handle
 :)
declare function f:_connectForExtendedNodl($enodl as element())
        as xs:integer? {
    let $connData := $enodl//pc:sqlNcat/pc:connection
    let $conn :=
        tt:connect($connData/@host, $connData/@user, $connData/@password)        
    return
        $conn
};        

(:~
 : Opens a connection to the data base specified by a nodl.
 :
 : @param enodel an extended nodl
 : @return the connection handle
 :)
declare function f:_openDbForExtendedNodl($conn as xs:integer, $enodl as element())
        as xs:integer? {
    let $connData := $enodl//pc:sqlNcat/pc:connection        
    let $db := $connData/@db
    let $sqlUse := concat('USE ', $db)
    return 
        f:execute($conn, $sqlUse)
};        

(:~
 : Creates the data base specified by a nodl.
 :
 : @param enodel an extended nodl
 : @return the connection handle
 :)
declare function f:_createDbForExtendedNodl($conn as xs:integer, $enodl as element())
        as xs:integer? {
    let $sql := $enodl//pc:sqlNcat        
    let $connData := $sql/pc:connection       
    let $createDb :=    
        <createDb name="{$connData/@db}" ifNotExists="true">{       
            if (not($sql/pc:charset/@name)) then () else attribute charset {$sql/pc:charset/@name},
            if (not($sql/pc:charset/@collation)) then () else attribute collation {$sql/pc:charset/@collation}           
        }</createDb>
    return
        tt:sqlCreateDb($conn, $createDb)          
};        

(:~
 : Maps a property value item to the value which is actually stored
 : in the SQL data base.
 :
 : @param value the value item
 : @sqlType the SQL type
 : @return the string to be stored
 :)
declare function f:_value2sqlValue($value as item()?, $sqlType as xs:string)
        as xs:string {
    if (upper-case($sqlType) eq 'BIT(1)') then
        if ($value eq 'true') then '1' else '0'
    else string($value)
};

(:~
 : Transforms a document into document text to be stored
 : in a relational database.
 :
 : @param doc the document
 : @return the document text ready for storage
 :)
declare function f:_getDocTextForSql($doc as node())
        as xs:string {
    let $serParams :=
        <output:serialization-parameters xmlns:output="http://www.w3.org/2010/xslt-xquery-serialization">
            <output:indent value="no"/>
        </output:serialization-parameters>
    let $prettyDoc := tt:prettyPrint($doc)
    let $docText := replace(serialize($prettyDoc, $serParams), '&#xD;&#xA;', '&#xA;')
    return
        $docText
};            

