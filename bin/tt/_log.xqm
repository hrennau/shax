(: log.xqm - provides a function for conditional logging of items
 :
 : @version 20141220 
 : ===================================================================================
 :)

module namespace m="http://www.ttools.org/xquery-functions";

declare variable $m:LOG_LEVEL as xs:integer := 0;

(:
 : ============================================================================
 :
 :     p u b l i c    f u n c t i o n s
 :
 : ============================================================================
 :)

(:~
 : Returns the items received, logging the value if the specified log level
 : is greater or equal the global constant $m:LOG_LEVEL.
 :
 : @param items the items to be logged
 : @param logLevel the log level of the log message
 : @param msg trace message, used if items are traced
 : @return the items received
 :)
declare function m:log($items as item()*, $logLevel as xs:integer, $msg as xs:string)
        as item()* {
    if ($logLevel le $m:LOG_LEVEL) then trace($items, $msg) else $items        
};        