(: constants.xqm - provides constants used by xquery topic tool applications
 :
 : @version 20140124-1 first version 
 : ===================================================================================
 :)

module namespace m="http://www.ttools.org/xquery-functions";

declare variable $m:URI_ERROR := "http://www.ttools.org/errors";
declare variable $m:URI_XSD := "http://www.w3.org/2001/XMLSchema";
declare variable $m:URI_PCOLLECTION := "http://www.infospace.org/pcollection";

(:
declare variable $m:MODIFIERS_CSV := ('encoding', 'delim', 'sep', 'header', 'names', 'fromRec', 'toRec');
:)

declare variable $m:NODE_SER_PARAMS :=
    <output:serialization-parameters xmlns:output="http://www.w3.org/2010/xslt-xquery-serialization">
        <output:omit-xml-declaration value="no"/>
        <output:method value="xml"/>
        <output:indent value="no"/>  
    </output:serialization-parameters>;

declare variable $m:PARAM_MODIFIER_SCHEMES :=
    <paramModifierSchemes>
        <modifiers paramTypes="docCAT">
            <modifier name='pquery' type="xs:string" default=''/>
        </modifiers>
        <modifiers paramTypes="csvURI">
            <modifier name='encoding' type="xs:string" default='ISO-8859-1'/>
            <modifier name='delim' type="xs:string" fct_length="1" default='"'/>
            <modifier name='sep' type="xs:string" fct_length="1" default=','/>
            <modifier name='header' type='xs:boolean' default='false'/>
            <modifier name='names' type='xs:NCName+' sep="WS" default='table row col'/>
            <modifier name='fromRec' type='xs:integer' default='1'/>
            <modifier name='toRec' type='xs:integer' default='0'/>
        </modifiers>
        <modifiers paramTypes="csvDFD">
            <modifier name='encoding' type="xs:string" default='ISO-8859-1'/>
            <modifier name='delim' type="xs:string" fct_length="1" default='"'/>
            <modifier name='sep' type="xs:string" fct_length="1" default=','/>
            <modifier name='header' type='xs:boolean' default='false'/>
            <modifier name='names' type='xs:NCName+' sep="WS" default='table row col'/>
            <modifier name='fromRec' type='xs:integer' default='1'/>
            <modifier name='toRec' type='xs:integer' default='0'/>
        </modifiers>
        <modifiers paramTypes="csvFOX">
            <modifier name='encoding' type="xs:string" default='ISO-8859-1'/>
            <modifier name='delim' type="xs:string" fct_length="1" default='"'/>
            <modifier name='sep' type="xs:string" fct_length="1" default=','/>
            <modifier name='header' type='xs:boolean' default='false'/>
            <modifier name='names' type='xs:NCName+' sep="WS" default='table row col'/>
            <modifier name='fromRec' type='xs:integer' default='1'/>
            <modifier name='toRec' type='xs:integer' default='0'/>
        </modifiers>
        <modifiers paramTypes="textURI">
            <modifier name='encoding' type="xs:string" default='ISO-8859-1'/>
            <modifier name='normLinefeed' type="xs:boolean" default='true'/>            
        </modifiers>
        <modifiers paramTypes="textDFD">
            <modifier name='encoding' type="xs:string" default='ISO-8859-1'/>
            <modifier name='normLinefeed' type="xs:boolean" default='true'/>            
        </modifiers>
        <modifiers paramTypes="textFOX">
            <modifier name='encoding' type="xs:string" default='ISO-8859-1'/>
            <modifier name='normLinefeed' type="xs:boolean" default='true'/>            
        </modifiers>
        <modifiers paramTypes="xtextURI">
            <modifier name='encoding' type="xs:string" default='ISO-8859-1'/>
            <modifier name='normLinefeed' type="xs:boolean" default='true'/>            
        </modifiers>
        <modifiers paramTypes="xtextDFD">
            <modifier name='encoding' type="xs:string" default='ISO-8859-1'/>
            <modifier name='normLinefeed' type="xs:boolean" default='true'/>            
        </modifiers>
        <modifiers paramTypes="linesURI">
            <modifier name='encoding' type="xs:string" default='ISO-8859-1'/>
            <modifier name='normLinefeed' type="xs:boolean" default='true'/>            
        </modifiers>
        <modifiers paramTypes="linesDFD">
            <modifier name='encoding' type="xs:string" default='ISO-8859-1'/>
            <modifier name='normLinefeed' type="xs:boolean" default='true'/>            
        </modifiers>
        <modifiers paramTypes="linesFOX">
            <modifier name='encoding' type="xs:string" default='ISO-8859-1'/>
            <modifier name='normLinefeed' type="xs:boolean" default='true'/>            
        </modifiers>
    </paramModifierSchemes>;
        

