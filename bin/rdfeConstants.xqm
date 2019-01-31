(:
 : -------------------------------------------------------------------------
 :
 : rdfeConstants.xqm - RDFe related constants
 :
 : -------------------------------------------------------------------------
 :)

module namespace f="http://www.rdfe.org/ns/xquery-functions";
import module namespace tt="http://www.ttools.org/xquery-functions" at 
    "tt/_nameFilter.xqm";    

declare variable $f:URI_RDFE := 'http://www.rdfe.org/ns/model#';
declare variable $f:URI_RDFEE := 'http://www.rdfe.org/ns/errors#';
declare variable $f:CLARC_PREFIX_RDFE := '{' || $f:URI_RDFE || '}';
declare variable $f:BUILTIN_NAMESPACE_BINDINGS :=
    map{
        'rdf': 'http://www.w3.org/1999/02/22-rdf-syntax-ns#',
        'rdfs': 'http://www.w3.org/2000/01/rdf-schema#',
        'rdfe': $f:URI_RDFE,
        'rdfee': $f:URI_RDFEE,        
        'xs': 'http://www.w3.org/2001/XMLSchema#'
    };
