## Heatmap

Un *heatmap* (o mapa de calor) és una representació gràfica de dades on els valors individuals continguts en una matriu es representen com a colors.

En mapes de calor, les dades es mostren en una quadrícula on cada fila representa un gen i cada columna representa una mostra o cas. El color i la intensitat de les caselles s'utilitzen per representar canvis en l'expressió gènica.

El mapa de calor també es pot combinar amb mètodes de *clustering* que agrupen els gens i les mostres junts en funció de la similitud del seu patró d'expressió gènica. Això pot ser útil per identificar els gens que normalment s'expressen molt semblant i detectar patrons sota condicions o covariables establertes. El mètode implementat per la funció *heatmap*, utilitza l'anàlisi de clústers jeràrquics.

La implementació *hclust* (hierarchical cluster analysis) de R utilitza el *Mètode de Ward* que calcula i actualitza a cada pas la dissimilaritat entre clústers, aquest mètode és d'aglomeració.

**Mètode de Ward**

Ward va proposar que la pèrdua d'informació que es produeix en integrar els diferents individus en clústers pot mesurar-se a través de la suma total dels quadrats de les desviacions entre cada punt (individu) i la mitjana del clúster en el qual s'integra. Perquè el procés de clusterització resulti òptim, en el sentit que els grups formats no distorsionin les dades originals, proposava la següent estratègia:

A cada pas de l'anàlisi, es considera la possibilitat de la unió de cada parell de grups i s'obta per la fusió d'aquells dos grups que menys incrementin la suma dels quadrats de les desviacions en unir-se.

Definim:

- $x_{ij}^k$ com el valor de la $j$-èssima variable sobre l'$i$-èssim individu del $k$-èssim clúster, que té $n_k$ individus.
- $m^k$ com el centroide del clúster $k$, amb components $m_{j}^k$.
- $E_k$ com la suma de quadrats dels errors del clúster $k$, és a dir, la distancia euclídea al quadrat entre cada individu del cluster $k$ al seu centroide:

$$E_k = \sum_{i=1}^{n_k}\sum_{j=1}^{p}(x_{ij}^k - m_{j}^k)^2$$

- $E$ com la suma de quadrats dels errors per a tots els clústers, és a dir, si suposem $h$ clústers:

$$E = \sum_{k=1}^{h} E_k$$

El procés comença amb $m$ clústers, cada clúster només té un sol individu, per tant, cada individu coincideix amb el centre del clúster i en aquest primer pas $E_k=0$, per a cada clúster i això fa que $E=0$. L'objectiu del mètode de Ward és trobar en cada etapa aquells dos clústers els quals la seva unió proporcioni el menor increment en la suma total d'errors $E$. Suposem que els clústers $C_p$ i $C_q$ s'uneixen resultant un nou clúster $C_t$, llavors definim l'increment de $E$ com,

$$\bigtriangleup E_{pq} = E_t - E_p - E_q$$

el procés es repeteix fins a l'obtenció del dendrograma i l'agrupació dels individus en els diferents clústers.
