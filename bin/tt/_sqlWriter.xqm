(:
 : -------------------------------------------------------------------------
 :
 : sqlWriter.xqm - Document me!
 :
 : -------------------------------------------------------------------------
 :)
 
module namespace f="http://www.ttools.org/xquery-functions";
import module namespace tt="http://www.ttools.org/xquery-functions" at 
    "_request.xqm",
    "_reportAssistent.xqm",
    "_errorAssistent.xqm",
    "_stringTools.xqm",    
    "_nameFilter.xqm";
    
declare namespace z="http://www.ttools.org/structure";

declare variable $f:indent := '   ';

(:
 : ============================================================================
 :
 :     p u b l i c    f u n c t i o n s
 :
 : ============================================================================
 :)

(:~
 : Transforms command descriptors into SQL text.
 :
 : @param cmds command descriptors
 : @return the SQL text
 :)
declare function f:writeSql($cmds as element()*)
        as item()* {
    if (empty($cmds)) then () else
    
    let $sqlList :=
        for $cmd in $cmds
        let $cmd := f:_normalizeCommand($cmd)
        return
            typeswitch($cmd)
            case element(createDb) return f:_writeSql_createDb($cmd)            
            case element(dropDb) return f:_writeSql_dropDb($cmd)            
            case element(createTable) return f:_writeSql_createTable($cmd)
            case element(dropTable) return f:_writeSql_dropTable($cmd)            
            case element(insert) return f:_writeSql_insert($cmd)            
            case element(delete) return f:_writeSql_delete($cmd)            
            case element(select) return f:_writeSql_select($cmd)            
            default return
                tt:createError('UNKNOWN_SQL_DESCRIPTOR', concat('Unknown descriptor type: ', name($cmd)), ())
    let $errors := tt:wrapErrors($sqlList[. instance of node()])                
    return
        if ($errors) then $errors else
            string-join($sqlList, '&#xA;')
       
};

(:
 : ============================================================================
 :
 :     p r i v a t e    f u n c t i o n s
 :
 : ============================================================================
 :)

(:~
 : Transforms a 'CREATE DATABASE' command descriptor into SQL text.
 :
 : @param cmd the command descriptor
 : @return the SQL text
 :)
declare function f:_writeSql_createDb($cmd as element(createDb))
        as item() {
    let $ifNotExists := $cmd/@ifNotExists        
    let $db := $cmd/@name
    let $ifNotExists := $cmd/@ifNotExists[. eq 'true']
    let $charset := $cmd/@charset
    let $collation := $cmd/@collation
    return
        string-join((
            'CREATE DATABASE', 
            'IF NOT EXISTS'[$ifNotExists],
            $db,
            if (not($charset)) then () else concat('CHARACTER SET ', $charset),
            if (not($collation)) then () else concat('COLLATE ', $collation)
        ), ' ')            
};        

(:~
 : Transforms a 'DROP DATABASE' command descriptor into SQL text.
 :
 : @param cmd the command descriptor
 : @return the SQL text
 :)
declare function f:_writeSql_dropDb($cmd as element(dropDb))
        as item() {       
    let $db := $cmd/@name
    return
        concat('DROP DATABASE ', $db)
};        

(:~
 : Transforms a 'createTable' command descriptor into SQL text.
 :
 : @param cmd the command descriptor
 : @return the SQL text
 :)
declare function f:_writeSql_createTable($cmd as element(createTable))
        as item() {
    let $tabName := concat('`', $cmd/@name, '`')   
    let $cols := $cmd/col
    let $fkeys := $cmd/fkey
    let $uniques := $cmd/unique
    let $lastCol := $cols[last()]    
    let $maxNameLen := max($cols/@name/string-length())
    let $ifNotExists := 'IF NOT EXISTS '[$cmd/@ifNotExists[. eq 'true']]
    
    let $defs :=
        let $pkeyClause := $cols[@pkey eq 'true'][1]/concat('PRIMARY KEY (', @name, ')')       
        return
        
        string-join((
            (: column definitions :)
            for $col in $cols
            let $colName := concat('`', $col/@name, '`')
            let $type := $col/@type
            let $unique := $col[@unique eq 'true']/'UNIQUE'
            let $auto := $col[@auto eq 'true']/'AUTO_INCREMENT'
            let $modifiers := string-join(($unique, $auto), ' ')
            let $typeAndModifiers := string-join(($type, $modifiers[string()]), ' ')
            return
                concat(tt:padRight($colName, $maxNameLen), ' ', $typeAndModifiers)
            ,
            (: implicit pkey clause :)
            let $marked := $cols[@pkey]
            return
                if (count($marked) ne 1) then () else 
                    concat('PRIMARY KEY (', $marked/@name, ')')            
            ,
            (: single column indexes :)
            for $col in $cols[@index eq 'true']
            let $lengthPostfix := $col/@indexLength/concat('(', ., ')')
            let $cname := concat('`', $col/@name, '`')
            return
                concat('INDEX (', $cname, $lengthPostfix, ')')
            ,
            (: explicit UNIQUE indexes :)
            for $unique in $uniques
            let $name := $unique/@name
            let $cols := $unique/@cols
            return
                string-join(('UNIQUE', $name, concat('(', $cols, ')')), ' '),
                
            (: foreign keys :)            
            for $fkey in $fkeys
            let $iname := $fkey/@name[string()]
            let $cols := string-join(tokenize($fkey/@cols, '\s+'), ',')
            let $pcols := string-join(tokenize($fkey/@parentCols, '\s+'), ',')            
            let $parent := $fkey/@parent  
            let $onDelete := $fkey/@onDelete/
                (if (. eq 'cascade') then 'CASCADE' else if (. eq 'setNull') then 'SET NULL' else ())
            let $onUpdate := $fkey/@onUpdate/
                (if (. eq 'cascade') then 'CASCADE' else if (. eq 'setNull') then 'SET NULL' else ())            
            return (
                string-join((
                    string-join((
                        'FOREIGN KEY', $iname, concat('(', $cols, ')'), 
                        'REFERENCES ', $parent, concat('(', $pcols, ')')
                    ), ' '),
                    if (not($onDelete)) then () else
                        concat($f:indent, 'ON DELETE ', $onDelete),
                    if (not($onUpdate)) then () else
                        concat($f:indent, 'ON UPDATE ', $onUpdate)
                ), concat('&#xA;', $f:indent))                        
            )                
        ), concat(',&#xA;', $f:indent))        
    return tt:log(
        string-join((
            concat('CREATE TABLE ', $ifNotExists, $tabName),
            '(',
            concat($f:indent, $defs),
            ')'
        ), '&#xA;') , 1, 'CREATE_TABLE:&#xA;')            
};        

(:~
 : Transforms a 'DROP TABLE' command descriptor into SQL text.
 :
 : @param cmd the command descriptor
 : @return the SQL text
 :)
declare function f:_writeSql_dropTable($cmd as element(dropTable))
        as item() {
    let $tabName := $cmd/@table   
    return
        concat('DROP TABLE `', $tabName, '`')
};        
(:~
 : Transforms an 'INSERT' command descriptor into SQL text.
 :
 : @param cmd the command descriptor
 : @return the SQL text
 :)
declare function f:_writeSql_insert($cmd as element(insert))
        as item() {
    let $tabName := $cmd/@table   
    let $cols := $cmd/col
    let $lastCol := $cols[last()]    
    return
        string-join((
            concat('INSERT INTO ', $tabName, ' SET'),
            for $col in $cols
            let $colName := concat('`', $col/@name, '`')
            let $rawValue := ($col/@value, $col/string())[1] 
            let $value := concat("'", tt:escapeString($rawValue, "'"), "'")
            let $sep := if ($col is $lastCol) then () else ',' 
            return
                concat($colName, '=', $value, $sep)
        ), ' ')            
};        

(:~
 : Transforms a 'DELETE' command descriptor into SQL text.
 :
 : @param cmd the command descriptor
 : @return the SQL text
 :)
declare function f:_writeSql_delete($cmd as element(delete))
        as item() {
    let $tabName := $cmd/@table   
    let $whereCols := $cmd/where/*   
    let $where :=
        if (empty($whereCols)) then () else
        concat(' WHERE ',
            string-join(
                for $col in $whereCols
                let $colName := concat('`', $col/@name, '`')
                let $colValueRaw := ($col/@value, string($col))[1]
                let $colValue := concat("'", tt:unescapeString($colValueRaw), "'")
                let $op := ($col/@op, '=')[1]
                return
                    concat($colName, ' ', $op, ' ', $colValue)
            , ' AND '))                            
    return
        concat('DELETE FROM ', $tabName, $where)            
};        

(:~
 : Transforms a 'SELECT' command descriptor into SQL text.
 :
 : @param cmd the command descriptor
 : @return the SQL text
 :)
declare function f:_writeSql_select($cmd as element(select))
        as item() {  
    let $countFlag := $cmd/@count/xs:boolean(.)
    let $distinctFlag := 
        if (not($cmd/@distinct eq 'true')) then () else 'DISTINCT '        
    let $tableClause :=
        let $furtherTables :=
            for $t in $cmd/tables/table[position() gt 1]
            let $name := $t/@name
            let $alias := $t/@alias
            let $join :=
                if (not($t/@join)) then () else 
                    concat($t/@join/upper-case(.), ' JOIN ')
            let $on := 
                if (not($join)) then () else
                    string-join($t/on/col/concat(@name, ' ', @op, ' ', @value), ' AND ')
            return
                concat($join, $name, ' AS ', $alias, if (not($join)) then () else concat(' ON ', $on))
        let $table1 := 
            let $t1 := $cmd/tables/table[1]        
            let $name := $t1/@name
            let $alias := 
                let $explicit := $t1/@alias
                return
                    if ($explicit) then $explicit
                    else if (empty($furtherTables)) then () 
                    else 't1'
            return
                concat($name, if (not($alias)) then () else concat(' AS ', $alias))
        return
            string-join(($table1, $furtherTables), concat('&#xA;', $f:indent))
                
    let $showCols := (string-join(tokenize($cmd/@cols, '\s+'), ', ')[string()], '*')[1]
  
    let $whereClause :=
        if ($cmd/where/@text) then concat('WHERE ', $cmd/where/@text) else
        
        let $whereCols := $cmd/where/* 
        return
            if (empty($whereCols)) then () else
            
        concat('WHERE ',
            string-join(
                for $col in $whereCols
                let $colName := concat('`', $col/@name, '`')
                let $colValue := concat("'", tt:unescapeString($col/@value), "'")
                let $op := ($col/@op, '=')[1]
                
                let $op := 
                    if ($op eq '~') then 'LIKE' 
                    else $op
                let $colValue := replace($colValue, '\*', '%')                    
                return
                    concat($colName, ' ', $op, ' ', $colValue)
            , ' AND '))                            
    return
        concat('SELECT ', $distinctFlag, $showCols, ' FROM ', $tableClause, '&#xA;', $f:indent, $whereClause)            
};        

declare function f:_normalizeCommand($cmd as element())
        as element() {
    f:_normalizeCommandRC($cmd)        
};        

declare function f:_normalizeCommandRC($n as node())
        as node()? {
    typeswitch($n)
    case document-node() return
        document {for $c in $n/node() return f:_normalizeCommandRC($c)}
    case element() return
        element {node-name($n)} {
            for $a in $n/@* return f:_normalizeCommandRC($a),
            for $c in $n/node() return f:_normalizeCommandRC($c)            
        }
    case attribute() return
        attribute {node-name($n)} {normalize-space($n)}
    default return $n        
};        