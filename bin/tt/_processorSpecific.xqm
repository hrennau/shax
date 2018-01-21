xquery version "3.0";
(:
 :***************************************************************************
 :
 : processorSpecific.xqm - processor specific extension functions
 :
 : Edition for processor: BaseX
 :
 :***************************************************************************
 :)
 
module namespace f="http://www.ttools.org/xquery-functions";
import module namespace tt="http://www.ttools.org/xquery-functions" at 
    "_constants.xqm";

declare namespace z="http://www.ttools.org/structure";

(:
 : ============================================================================
 :
 :     p r i v a t e    f u n c t i o n s
 :
 : ============================================================================
 :)

(:~
 : Executes an XQuery expression.
 :
 : @param query an XQuery expression
 : @param ctxt an item to be used as context item
 : @return the result of evaluating the expression
 :) 
declare function f:evaluate($query as xs:string, $context as item()?)
        as item()* {
        
    let $context := map {'' : $context }
    return
        xquery:eval($query, $context)
};
