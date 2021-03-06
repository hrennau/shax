(:
 : -------------------------------------------------------------------------
 :
 : constants.xqm - a container for application-wide constants
 :
 : -------------------------------------------------------------------------
 :)

module namespace f="http://www.ttools.org/shax/ns/xquery-functions";
import module namespace tt="http://www.ttools.org/xquery-functions" at 
    "tt/_request.xqm",
    "tt/_reportAssistent.xqm",
    "tt/_errorAssistent.xqm",
    "tt/_log.xqm",
    "tt/_nameFilter.xqm",
    "tt/_pcollection.xqm";    

declare variable $f:URI_SHAX := "http://shax.org/ns/model";
declare variable $f:URI_SHAX_WORLD := "http://shax.org/ns/world#";
declare variable $f:URI_SHAX_NONS := "http://shax.org/ns/no-namespace#";
declare variable $f:URI_STRUCTURE := "http://www.ttools.org/shax/ns/structure";
declare variable $f:URI_NONS_DEFAULT := "http://shax.org/ns/nonamespace";
declare variable $f:URI_ERROR := "http://www.ttools.org/shax/ns/errir";
declare variable $f:URI_XSD := "http://www.w3.org/2001/XMLSchema";
declare variable $f:URI_RDF := "http://www.w3.org/1999/02/22-rdf-syntax-ns#";
declare variable $f:URI_XSD_RDFE := "../xsd/rdfe.xsd";
declare variable $f:QNAME_XSDTYPE_STRING as xs:QName := QName($f:URI_XSD, 'xs:string');
declare variable $f:QNAME_XSDTYPE_INTEGER as xs:QName := QName($f:URI_XSD, 'xs:integer');
declare variable $f:QNAME_XSDTYPE_BOOLEAN as xs:QName := QName($f:URI_XSD, 'xs:boolean');
declare variable $f:DIR_DEBUG := ".";
declare variable $f:DEBUG_LEVEL as xs:integer := 1;

