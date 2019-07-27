## Heatmap

Un mapa de calor (*heatmap*) és una representació gràfica de dades on els valors individuals continguts en una matriu es representen com a colors.
En mapes de calor aplicats a expressions de gens, les dades es mostren en una quadrícula on cada fila representa un gen i cada columna representa una mostra o cas. Els colors de les cel·les representen nivells d'expressió diferents. El mapa de calor se sol combinar amb mètodes de *clustering* doble que agrupen d'una banda els gens i d'altra les mostres, en funció de la similitud del seu patró d'expressió gènica. Els clústers de gens poden ser útils per identificar gens que s'expressen de manera similar i detectar patrons sota condicions establertes (covariables), com ara la funció gènica. Igualment, els clústers de mostres els usarem per veure si aquestes s'agrupen de manera natural segons els tractaments. Són clusters jeràrquics, que implementen procediments d'aglomeració per passos:  partint d'una *matriu de distàncies* inicials, en cada pas es determinen uns nous grups reagrupant-ne dos dels anteriors segons un *criteri d'enllaç*: simple, complet, de Ward, mitjà, etc.   

La implementació amb el paquet  ``ComplexHeatmap''  de R permet utilitzar matrius de distàncies i funcions d'enllaç diferents per files i per columnes. Hem escollit la *distància Euclidiana* i *el mètode d'enllaç de Ward* per a les mostres, mentre que per als gens hem triat la *distància de Pearson* ($d=\frac{1-r}2$, on $r$ és el coeficient de correlació de Pearson) i *el mètode d'enllaç complet*. <font color='blue'> Creiem que el coeficient de Pearson és més addient per comparar expressions en base a *openarrays* perquè d'aquesta manera comparem les correlacions entre les expressions dels dos gens, sense comparar els nivells d'expressió concrets.</font>


**Mètode d'enllaç complet**

A cada pas intermedi de l'anàlisi, es considera la possibilitat de la unió de cada parell dels  grups $C_p$ i $C_q$  existents. Per això es ``recalcula la distància'' entre els dos grups com la distància entre els  *veïns més llunyans* dels dos grups. És a dir, si 
$C_p$ i $C_q$ són dos clústers del pas anterior, es recalcula

$$d(C_p, C_q)\colon = \max_{i,j} d( x_{i\cdot}^p , x_{j\cdot}^{q} ); \qquad x_{i\cdot}^p \in C_p ,\, x_{j\cdot}^{q} \in C_q$$
on $x_{i\cdot}^p$ és  el vector d'observacions en totes les variables de l'individu $i$, que pertany al grup $C_p$ en el pas anterior.
En el pas actual,  s'obta per la fusió d'aquells dos grups que estiguin a menor ``distància recalculada''. El mètode d'enllaç complet es pot associar a diverses distàncies o dissimilaritats. 

**Mètode de Ward**

Ward va proposar un mètode associat a la distància Euclidiana. La idea és que  la pèrdua d'informació que es produeix en integrar els diferents individus en clústers es pot mesurar a través de la suma total dels quadrats de les desviacions entre cada punt (individu) i la mitjana del clúster en el qual s'integra. Perquè el procés de clusterització resulti òptim, en el sentit que els grups formats distorsionin el menys possible les dades originals, proposà la estratègia següent: *A cada pas de l'anàlisi, es considera la possibilitat de la unió de totes les parelles de grups i s'obta per la fusió d'aquells dos grups que menys incrementin la suma dels quadrats de les desviacions en unir-se.*

Per fer-ho més formal, definim:

- $x_{ik}^p$ com el valor de la $k$-èssima variable sobre l'$i$-èssim individu del $p$-èssim clúster, que té $n_p$ individus.
- $m^p$ com el centroide del clúster $C_p$, amb components $m_{k}^p$ en les variables $X_k$.
- $E_p$ com la suma de quadrats dels errors del clúster $C_p$, és a dir, la distancia Euclidiana al quadrat entre cada individu del cluster $p$ al seu centroide:
$$E_p = \sum_{i=1}^{n_p}\sum_{k=1}^{K}(x_{ik}^p - m_{k}^p)^2$$

- $E$ com la suma de quadrats dels errors per a tots els clústers, és a dir, si suposem $h$ clústers: $E = \sum_{p=1}^{P} E_p$

El procés comença amb $n$ clústers, tants com individus i cada clúster només té un sol individu el qual, per tant, coincideix amb el centre del clúster. En aquest primer pas, $E_p=0$ per a cada clúster i tenim $E=0$. Suposem que els clústers $C_p$ i $C_q$ s'uneixen resultant un nou clúster $C_t$, aleshores definim l'increment de $E$ com,
$$\bigtriangleup E_{pq} = E_t - E_p - E_q$$
En aquesta etapa s'uneixen aquells dos clústers els quals la seva unió proporcioni el menor increment en la suma total de quadrats dels errors $E$, equivalentment el menor increment $\bigtriangleup E_{pq}$. 
El procés es repeteix fins a l'agrupació de tots els individus en un únic clúster i es dibuixa el dendrograma. 
