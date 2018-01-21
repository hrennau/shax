(:
 :***************************************************************************
 :
 : sqlExecutor.xqm - functions for executing SQL commands
 :
 :***************************************************************************
 :)
 
module namespace f="http://www.ttools.org/xquery-functions";
import module namespace tt="http://www.ttools.org/xquery-functions" at 
    "_request.xqm",
    "_reportAssistent.xqm",
    "_errorAssistent.xqm",    
    "_nameFilter.xqm",
    "_sqlWriter.xqm";
    
declare namespace z="http://www.ttools.org/structure";

(:
 : ============================================================================
 :
 :     p u b l i c    f u n c t i o n s
 :
 : ============================================================================
 :)

declare function f:execute($conn as xs:integer, $sql as xs:string)
        as item()* {       
    sql:execute($conn, $sql)        
};

(:~
 : Creates a connection to server $server, with user $user and password $pw.
 :
 : @param server the server name (e.g. 'localhost')
 : @param user the user name
 : @param pw the password
 : @return the connection handle
 :)
declare function f:connect($server as xs:string, $user as xs:string, $pw as xs:string?)
        as xs:integer {
    let $uri := concat('jdbc:mysql://', $server)
    return
        sql:connect($uri, $user, $pw)
       
};

(:~
 : Creates a database.
 :
 : @param conn the connection handle
 : @return nothing
 :)
declare function f:sqlCreateDb($conn as xs:integer, $createDb as element(createDb))
        as empty-sequence() {
    let $sql := tt:writeSql($createDb)
    return
        f:execute($conn, $sql)       
};

(:~
 : Deletes a database.
 :
 : @param conn the connection handle
 : @return nothing
 :)
declare function f:sqlDropDb($conn as xs:integer, $dropDb as element(dropDb))
        as empty-sequence() {
    let $sql := tt:writeSql($dropDb)
    return
        f:execute($conn, $sql)       
};

(:~
 : Creates a database.
 :
 : @param conn the connection handle
 : @return a report of the accessible databases
 :)
declare function f:createDb($conn as xs:integer, $db as xs:string)
        as empty-sequence() {
    let $cmd := concat('CREATE DATABASE IF NOT EXISTS ', $db, ' CHARACTER SET utf8;')
    return
        f:execute($conn, $cmd)       
};

(:~
 : Returns a 'dbs' element reporting the accessible databases.
 :
 : @param conn the connection handle
 : @return a report of the accessible databases
 :)
declare function f:sqlShowDatabases($conn as xs:integer)
        as element() {
    let $cmd := 'SHOW DATABASES;'        
    let $dbsRaw := f:execute($conn, $cmd)
    let $dbs :=
        for $db in $dbsRaw/sql:column[@name eq 'Database']/string() 
        return <db>{$db}</db>
    return        
        <dbs count="{count($dbs)}">{$dbs}</dbs>
};

(:~
 : Returns a 'tables' element reporting the tables of a given database.
 :
 : @param conn the connection handle
 : @param db the database name
 : @return a report of the accessible databases
 :)
declare function f:sqlInfoTables($conn as xs:integer, $db as xs:string)
        as element() {
    let $cmdUseDb := 'USE INFORMATION_SCHEMA'
    let $cmdSelect := concat('SELECT TABLE_NAME FROM TABLES WHERE TABLE_SCHEMA=''', $db, '''')
    let $retUse := f:execute($conn, $cmdUseDb)
    let $tablesRaw := f:execute($conn, $cmdSelect)
    let $tables :=
        for $table in $tablesRaw/sql:column[@name eq 'TABLE_NAME']/string() 
        order by lower-case($table)
        return <table>{$table}</table>    
    return
        <tables db="{$db}" count="{count($tables)}">{
            $tables
        }</tables>   
};

(:~
 : Returns a 'tables' element reporting the tables of a given database.
 :
 : @param conn the connection handle
 : @param db the database name
 : @return a report of the accessible databases
 :)
declare function f:sqlInfoColumns($conn as xs:integer, 
                                  $db as xs:string, 
                                  $tableFilters as element(nameFilter)*,
                                  $columnFilters as element(nameFilter)*,
                                  $typeFilters as element(nameFilter)*)
        as element() {
    let $cmdUseDb := concat('USE ', $db)
    let $tables := f:sqlInfoTables($conn, $db)
    let $infoAtts := (
        if (empty($tableFilters)) then () else attribute tableFilter {string-join($tableFilters/@text, '; ')},
        if (empty($columnFilters)) then () else attribute columnFilter {string-join($columnFilters/@text, '; ')},
        if (empty($typeFilters)) then () else attribute dtypeFilter {string-join($typeFilters/@text, '; ')},        
        ()
    )        
    let $tableCols :=
        let $colNames := ('COLUMN_NAME', 'COLUMN_TYPE', 'COLUMN_DEFAULT')
        let $colNamesDesc := string-join(for $n in $colNames return concat('`', $n, '`'), ', ')
        for $table in $tables/*
        let $cmdSelect := concat('SELECT ', $colNamesDesc, ' FROM `COLUMNS` WHERE TABLE_SCHEMA=''', $db, ''' and TABLE_NAME=''', $table, '''')
        let $tableColumnsRaw := f:execute($conn, $cmdSelect)
        where not($tableFilters) or (some $tf in $tableFilters satisfies tt:matchesNameFilter($table, $tf))
        return
            <table name="{$table}" countColumns="{count($tableColumnsRaw)}">{
                for $record in $tableColumnsRaw
                let $name := $record/sql:column[lower-case(@name) eq 'column_name']/string()
                let $type := $record/sql:column[lower-case(@name) eq 'column_type']/string()                
                let $default := $record/sql:column[lower-case(@name) eq 'column_default']/string()                
                order by lower-case($name)
                where (not($columnFilters) or (some $cf in $columnFilters satisfies tt:matchesNameFilter($name, $cf)))
                      and
                      (not($typeFilters) or (some $df in $typeFilters satisfies tt:matchesNameFilter($type, $df)))                      
                return
                    <col name="{$name}">{
                        if (not($type)) then () else attribute type {$type},
                        if (not($default)) then () else attribute default {$default}
                    }</col>
            }</table>
    let $countCols := sum($tableCols/@coundColumns/xs:integer)            
    return
        <columns db="{$db}" countColumns="{$countCols}">{
            $infoAtts,
            $tableCols
        }</columns>
};

(:~
 : Creates a data base table.
 :
 : @param conn the connection handle
 : @param db the database name
 : @param tableDesc an XML descriptor of the 'createTable' command
 : @return a report of the accessible databases
 :)
declare function f:sqlCreateTable($conn as xs:integer,
                                  $db as xs:string?, 
                                  $tableDesc as element(createTable))
        as element() {       
    let $sqlUseDb := concat('USE ', $db)
    let $sqlCreateTable := tt:writeSql($tableDesc)
    let $retCmeUseDb := f:execute($conn, $sqlUseDb)
    let $retCreateTable := f:execute($conn, $sqlCreateTable)
    return
        <tableCreated>{
            <tableDesc>{$tableDesc}</tableDesc>,
            <sql>{$sqlCreateTable}</sql>           
        }</tableCreated>
};

(:~
 : Executes a 'DROP TABLE' command supplied as a command descriptor.
 :
 : @param conn the connection handle
 : @param db the database name
 : @param table the table name
 : @return 1, if the table was dropped, 0, if it does not exist
 :)
declare function f:sqlDropTable($conn as xs:integer,
                                $db as xs:string?, 
                                $table as xs:string)
        as xs:integer {
    if (not(f:sqlTableExists($conn, $db, $table))) then 0 else
    
    let $retUseDb := 
        if (not($db)) then () else 
            f:execute($conn, concat('USE `', $db, '`'))
   
    let $sqlDropTable := concat('DROP TABLE IF EXISTS `', $table, '`')
    let $retDropTable := f:execute($conn, $sqlDropTable)
    return
        1
};

(:~
 : Returns true if the specified table exists, false otherwise.
 :
 : @param conn connection handle
 : @param db specifies the database
 : @param table the table name
 : @return true if the table exists, false otherwise
 :)
declare function f:sqlTableExists($conn as xs:integer,
                                  $db as xs:string?, 
                                  $table as xs:string)
        as xs:boolean {
    let $fromClause := if (not($db)) then () else concat("FROM `", $db, "` ")         
    let $sqlCheckExists := concat("SHOW TABLES ", $fromClause, "LIKE '", $table, "'")
    let $retCheckExists := f:execute($conn, $sqlCheckExists)
    return
        exists($retCheckExists)
};

(:~
 : Executes an 'INSERT' command supplied as a command descriptor.
 :
 : @param conn the connection handle
 : @param db the database name
 : @param tableDesc an XML descriptor of the 'createTable' command
 : @return a report of the accessible databases
 :)
declare function f:execInsert($conn as xs:integer,
                              $db as xs:string?, 
                              $insert as element(insert))
        as element() {
    let $retUseDb := 
        let $db := ($db, $insert/@db)[1]
        return
            if (not($db)) then () else
                let $sqlUse := concat('USE ', $db)
                return f:execute($conn, $sqlUse)        
            
    
    let $sqlInsert := tt:writeSql($insert)
    let $retInsert := f:execute($conn, $sqlInsert)
    return
        <insertExecuted>{
            <insertDesc>{$insert}</insertDesc>,
            <sql>{$sqlInsert}</sql>           
        }</insertExecuted>
};

(:~
 : Executes a 'SELECT' command supplied as a command descriptor.
 :
 : @param conn the connection handle
 : @param db the database name
 : @param tableDesc an XML descriptor of the 'createTable' command
 : @return a report of the accessible databases
 :)
declare function f:execSelect($conn as xs:integer,
                              $db as xs:string?, 
                              $select as element(select))
        as element() {
    let $retUseDb := 
        let $db := ($db, $select/@db)[1]
        return
            if (not($db)) then () else
                let $sqlUse := concat('USE ', $db)
                return f:execute($conn, $sqlUse)        
            
    
    let $sqlSelect := tt:writeSql($select)
    let $retSelect := f:execute($conn, $sqlSelect)
    return
        <select cmd="{$sqlSelect}">{
            for $row in $retSelect
            return
                <row>{
                    for $col in $row/*
                    let $name := $col/@name
                    return
                        if ($name castable as xs:NCName) then element {$name} {$col/string()}
                        else <_col name="{$name}">{string($col)}</_col>
                }</row>
        }</select>
};
