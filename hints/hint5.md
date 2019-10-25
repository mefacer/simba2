## Interpretem el *heatmap*

El *heatmap* és la representació conjunta de dos *dendrogrames*: un dendrograma vertical que mostra agrupacions de files (gens o variables) i un dendrograma horitzontal que mostra agrupacions de columnes (mostres o casos). Un dendrograma és un arbre que representa una jerarquia de particions (des de la màxima on cada element és un clúster fins a la mínima on tots els elements estan agrupats en un clúster únic). 

<font color='blue'> Per decidir quina partició escollir, un possible criteri és tallar l'arbre per on les *branques són més llargues*</font>, perquè correspon a maximitzar les distàncies entre els clústers resultants.

- Les caselles del heatmap tenen un color indicatiu del nivell d'expressió, segons la llegenda. Les caselles de color gris fosc són valors d'expressió no obtinguts (NA).
- Al dendrograma vertical hi ha els clústers dels gens. Després d'escollir la partició, els gens agrupats en un mateix clúster tendeixen a tenir pautes d'expressió similars (més correlacionades, degut a l'ús de la distància de Pearson) en el conjunt de les mostres.
- La variable categòrica *funció del gen* es visualitza segons els colors que veiem sota del dendrograma vertical i és  útil per explorar si els gens agrupats en un mateix clúster (expressions correlacionades) tendeixen a tenir la mateixa funcionlitat o, per contra, l'agrupació per clústers no es correspon a l'agrupació per funcionalitats. 
- Al dendrograma horitzontal hi ha els clústers de les mostres. Després d'escollir la partició, les mostres agrupades en un mateix clúster tendeixen a tenir pautes d'expressió similars (és a dir, menys distants segons la distància Euclidiana) en el conjunt dels gens.
- La variable *tractament* es visualitza segons els colors que veiem sota del dendrograma horitzontal i és útil per  explorar si les mostres d'un mateix clúster (expressions similars) tendeixen a correspondre a un mateix tractment o, per contra, l'agrupació per clústers no s'associa bé a cap agrupació per tractaments.
 
