xquery version "3.0";
(:
 :***************************************************************************
 :
 : _pcollection.utils.xqm - utility functions supporting the processing of pcollections
 :
 :***************************************************************************
 :)

module namespace f="http://www.ttools.org/xquery-functions";
import module namespace tt="http://www.ttools.org/xquery-functions" at 
    "_errorAssistent.xqm",
    "_log.xqm",
    "_nameFilter.xqm",
    "_resourceAccess.xqm";    

declare namespace z="http://www.ttools.org/structure";
declare namespace pc="http://www.infospace.org/pcollection";

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

(: 
   *** create extended nodl ***************************************************
:)

(:~
 : Transforms a NODL document into an extended NODL document. An
 : extended NODL is a comprehensive collection model, containing
 : explicit information implied by the NODL contents.
 :
 : An extended NODL contains the following additional information:
 : * enodl/pmodel/property/
 :     (@itemType, @itemTypeCategory, @minOccurs, @maxOccrs)
 : * enodl/ncatModel/sqlModel/tables/table/
 :     (@name, @main, @resourceCol)
 : * enodl/ncatModel/sqlModel/tables/table/col/
 *     (@name, @type, @pkey, @auto, @index) 
 : * enodl/ncatModel/sqlModel/tables/table/unique/@cols 
 : * enodl/ncatModel/sqlModel/tables/table/fkey/
 :     (@name, @cols, @parent, @parentCols, @onDelete, @onUpdate)
 : 
 : Example snippet:
 : <sqlNcat>
 :   <rdbms name="MySQL"/>
 :     <connection host="localhost" db="pcol" user="root" password="admin"/>
 :     <tables>
 :       <table name="priscilla_ncat" main="true" resourceCol="_node_uri">
 :         <col name="nkey" type="INT UNSIGNED" pkey="true" auto="true"/>
 :         <col name="_node_uri" type="VARCHAR(400)"/>
 :         <col name="tns" type="VARCHAR(100)" index="true"/>
 :         <unique cols="_node_uri(200)"/>
 :       </table>
 :       <table name="priscilla_ncat_elem">
 :         <col name="pkey" type="INT UNSIGNED" pkey="true" auto="true"/>
 :         <col name="nkey" type="INT UNSIGNED"/>
 :         <col name="elem" type="VARCHAR(100)" index="true"/>
 :         <fkey name="fk_nkey" cols="nkey" parent="priscilla_ncat" parentCols="nkey" 
 :               onDelete="cascade" onUpdate="cascade"/>
 :       </table>
 :    </tables>
 :  </sqlNcat>
 :
 : @param nodl a NODL 
 : @return the extended NODL model
 :)
declare function f:_extendedNodl($nodl as element(pc:nodl))
        as element(pc:enodl) {
    let $asElementNames :=
        let $asElement := $nodl//pc:xmlNcat/@asElement
        return
            if (not($asElement)) then () else
                for $item in tokenize(normalize-space($asElement), ' ') return
                    concat('^', replace($item, '\*', '.*'), '$')
        
    let $pmodel := 
        let $pm := $nodl//pc:pmodel
        return
            element {node-name($pm)} {                
                $pm/@*,
                $pm/pc:property/f:_extendedPropertyElem(., $asElementNames),
                $pm/pc:anyProperty
            }                
    return (
        f:_extendedNodlRC($nodl, $pmodel)
        , file:write('/projects/infospace/mongodb/enodl.xml', f:_extendedNodlRC($nodl, $pmodel)) )        
};

(:~
 : Recursive helper function of function 'extendedNodl'.
 :
 : @param n the node to be processed
 : @param pmodel element defining the pmodel
 : @return a fragment of the extended NODL
 :)
declare function f:_extendedNodlRC($n as node(), $pmodel as element(pc:pmodel))
        as node()* {
    typeswitch ($n)
    case document-node() return
        document {for $c in $n/node() return f:_extendedNodlRC($c, $pmodel)}

    case element(pc:nodl) return
        <enodl xmlns="http://www.infospace.org/pcollection">{
            attribute nodlURI {$n/root()/document-uri(.)},
            for $a in $n/@* return f:_extendedNodlRC($a, $pmodel),
            for $c in $n/node() return f:_extendedNodlRC($c, $pmodel)            
        }</enodl>
    case element(pc:sqlNcat) return
        let $nodl := $n/ancestor::pc:nodl
        (: let $collName := $n/ancestor::pc:nodl/pc:collection/@name :)
        return
            <sqlNcat xmlns="http://www.infospace.org/pcollection">{
                (: for $a in $n/@* return f:_extendedNodlRC($a, $pmodel), :)        
                <rdbms name="{$n/@rdbms}"/>,
                <connection>{
                    $n/@host,
                    $n/@db,
                    $n/@user,
                    $n/@password
                }</connection>,
                
                for $c in $n/node() return f:_extendedNodlRC($c, $pmodel),
                f:_sqlTablesModel($nodl, $pmodel)
            }</sqlNcat>
    case element(pc:pmodel) return $pmodel

    case element() return
        element {node-name($n)} {
            for $a in $n/@* return f:_extendedNodlRC($a, $pmodel),
            for $c in $n/node() return f:_extendedNodlRC($c, $pmodel)            
        }
        
    case attribute(documentURI) return
        attribute {node-name($n)} {$n/resolve-uri(., base-uri(..))}
        
    default return $n        
};

(:~
 : Enhances the property definition as found in a NODL, adding further 
 : attributes representing the result of analysis. These attributes are:
 : * itemType - the type of a value item
 : * itemTypeCategory - one of string, number, boolean, other
 : * minOccurs - minimum number of occurrences
 : * maxOccurs - maximum number of occurrences
 : * numeric - true if item type is numeric, false otherwise
 :
 : @param p an 'property' element
 : @param asElementNames name patterns of thos properties which are 
 :    always represented by elements
 : @return an element with attrbutes capturing type features.
 :)
declare function f:_extendedPropertyElem($p as element(), $asElementNames as xs:string*)
        as element(pc:property) {
    let $type := $p/@type/replace(., '\s+', '')
    let $card := replace($type, '^\i\c+', '')    
    let $asElement := 
        let $name := $p/@name
        return 
            some $n in $asElementNames satisfies matches($name, $n)
    let $minMax :=
        if (not($card)) then (1, 1)
        else if ($card = '?') then (0, 1)        
        else if ($card = '*') then (0, -1)
        else if ($card = '+') then (1, -1)        
        else
            let $limits := tokenize(replace($card, '[{}]', ''), ',')
            let $min := $limits[1] ! xs:integer(.)
            let $max := 
                if (empty($limits[2])) then $min
                else if ($limits[2] eq '*') then -1
                else xs:integer($limits[2])
            return ($min, $max)
    let $itemType := replace($type, '^(\i\c+).*', '$1')
    let $itemTypeCategory := f:_itemTypeCategory($itemType)
    return
        element {node-name($p)}{
            $p/@*,
            attribute itemType {$itemType},
            attribute itemTypeCategory {$itemTypeCategory},
            attribute minOccurs {$minMax[1]},
            attribute maxOccurs {$minMax[2]},
            if (not($asElement)) then () else attribute asElement {'true'}
        }
};

(:~
 : Returns the type category of a given item type. The
 : category is one of: number, string, boolean, other.
 : 
 : Usage note. The item type category of an external property
 : enables a decision which data type to use in the 
 : representation of a pnode - for example whether to use
 : a string type of a numeric type.
 :
 : @param t the item type (e.g. 'xs:integer')
 : @return the item type category
 :) 
declare function f:_itemTypeCategory($t as xs:string)
        as xs:string {
    if ($t = ('xs:decimal', 
              'xs:float', 
              'xs:double', 
              'xs:integer',
              'xs:nonPositiveInteger', 
              'xs:negativeInteger',
              'xs:long',
              'xs:int',
              'xs:short',
              'xs:byte',
              'xs:nonNegativeInteger',
              'xs:unsignedLong',
              'xs:unsignedInt',
              'xs:unsignedShort',
              'xs:unsignedByte',
              'xs:positiveInteger')) then 'number'
    else if ($t =('xs:string',
                  'xs:normalizedString',
                  'xs:token',
                  'xs:language',
                  'xs:NMTOKEN',
                  'xs:name',
                  'xs:NCName',
                  'xs:ID')) then 'string'
    else if ($t = 'xs:boolean') then 'boolean'
    else 'other'
};

(: 
   *** create pnode model *****************************************************
:)

(: 
   *** create pnode ***********************************************************
:)

(:~
 : Transforms a node into a pnode.
 :
 : Note. The names and values of the external properties are
 : controled by the pmodel received as a parameter, and the
 : representation of the node is controlled by the
 : node constructor model received as another parameter.
 :
 : @param node the node to be transformed
 : @param nodeURI the node URI to be stored in the pnode
 : @param pnodeModel a model describing the pnode to be created
 : @param a pnode
 :)
declare function f:_pnode($node as node(),
                          $nodeURI as xs:string?,
                          $pmodel as element(pc:pmodel),
                          $nodeConstructor as element(pc:nodeConstructor))
        as element(pc:pnode) {
    let $docRoot := $node/descendant-or-self::*[1]
    let $psetters := $pmodel/pc:property    
    let $nodeConstructorKind := tokenize($nodeConstructor/@kind, '\+')
    
    return
        <pnode xmlns="http://www.infospace.org/pcollection">{
            if (not($nodeConstructorKind = 'uri')) then () else
                let $uri := ($nodeURI, $node/root()/document-uri(.))[1]
                return
                    attribute _node_uri {$uri}
            ,  
            let $properties :=
                for $p in $psetters
                let $pname := $p/@name
                let $value := tt:evaluate($p/@expr, $docRoot)
                let $useValue := $value ! xs:string(.)  (: refinements pending :)
                where exists($useValue)
                return
                    if (count($useValue) eq 1) then
                        if ($p/@asElement eq 'true') then
                            element {$pname} {$useValue}
                        else
                            attribute {QName((), $pname)} {$useValue}
                    else
                        element {$pname} {
                            for $item in $useValue return <item>{$item}</item>
                        }
            return (
                $properties[self::attribute()],
                $properties[self::element()]
            )
            ,
            if (not($nodeConstructorKind = 'text')) then () else
                <_node>{$node}</_node>                   
        }</pnode>                
};

(: 
   *** read pnode data ********************************************************
:)

(:~
 : Retrieves from a pnode a property value.
 :
 : @param pnode a pnode
 : @param pname a property name
 : @return the property value
 :)
declare function f:_pnodeProperty($pnode as element(pc:pnode), $pname as xs:string)
        as xs:string* {
    $pnode/@*[local-name(.) eq $pname]/string(.), 
    $pnode/*[not(*)][local-name(.) eq $pname]/string(),
    $pnode/*[*][local-name(.) eq $pname]/pc:item/string()
};

(:~
 : Retrieves from a pnode the node which it describes.
 :
 : @param node a node descriptor
 : @param pfn a pfilter node
 : @return true if the node descriptor matches the pfilter node, false otherwise
 :)
declare function f:_pnodeNode($pnode as element(pc:pnode))
        as node()? {
    if ($pnode/pc:node) then $pnode/pc:node/*
    else if ($pnode/@node_uri) then 
        let $uri := $pnode/resolve-uri(@node_uri, base-uri(.))
        return
            if (not(doc-available($uri))) then () else tt:doc($uri)
    else ()    
};

