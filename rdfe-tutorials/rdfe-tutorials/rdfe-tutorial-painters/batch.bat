rem *** ADAPT HOME_SHAX
set HOME_SHAX=/tt/shax/bin

echo Command 1
call basex -b "request=rdfe?dox=data/painters.xml,semap=rdfe/painters.rdfe.01.xml" -o ttl/triples01-only-painters.ttl %HOME_SHAX%/shax.xq

echo Command 2
call basex -b "request=rdfe?dox=data/painters.xml,semap=rdfe/painters.rdfe.02.xml" -o ttl/triples02-painters-and-paintings-without-connection.ttl %HOME_SHAX%/shax.xq

echo Command 3
call basex -b "request=rdfe?dox=data/painters.xml,semap=rdfe/painters.rdfe.03.xml" -o ttl/triples03.ttl %HOME_SHAX%/shax.xq

echo Command 4
call basex -b "request=rdfe?dox=data/painters-zh.xml, semap=rdfe/painters-zh.rdfe.03.xml" -o ttl/triples03-zh.ttl %HOME_SHAX%/shax.xq

echo Command 5
call basex -b "request=rdfe?dox=data/paintings.xml,semap=rdfe/paintings.rdfe.03.xml" -o ttl/triples03-paintings.ttl %HOME_SHAX%/shax.xq

echo Command 6
call basex -b "request=rdfe?dox=data/painters.xml,semap=rdfe/painters.rdfe.04.xml" -o ttl/triples04.ttl %HOME_SHAX%/shax.xq

echo Command 7
call basex -b "request=rdfe?dox=data/painters.xml,semap=rdfe/painters.rdfe.05.xml" -o ttl/triples05-without-wikidata-ids.ttl %HOME_SHAX%/shax.xq

echo Command 8
call basex -b "request=rdfe?dox=data/painters-with-wikidata-ids.xml, semap=rdfe/painters.rdfe.05.xml" -o ttl/triples05-with-wikidata-ids.ttl %HOME_SHAX%/shax.xq
