## Anàlisi de components principals (ACP): gràfiques

L'ACP és un mètode per analitzar l'estructura d'una matriu de covariàncies o de correlacions donada i, a partir de les correlacions existents, eliminar l'eventual redundància per reduir la dimensió del conjunt de dades. 

**Objectiu de l'ACP**

Donades $p$ variables $X_1,X_2, \ldots,X_p$ que suposarem centrades
(si es treballa amb la matriu de correlacions, es consideren les variables normalitzades amb variància igual a 1), volem
construir $p$ noves variables $Y_1,Y_2, \ldots,Y_p$, que anomenarem
components (les components principals seran un cert subconjunt
d'aquestes) amb les següents condicions:

 1. Les noves variables (components) han de ser combinació lineal de les variables originals.
 2. Les components han de ser incorrelacionades.
 3. El conjunt de totes les components ha d'explicar la mateixa variància total que les variables originals, i les components estaran ordenades de major a menor variància.

El teorema que demostra l'existència i unicitat de les components, i
en dóna el procediment de càlcul és el següent:

**Teorema**
*Les components  venen donades per la transformació lineal
$$Y =V^tX \qquad\qquad  (1)$$
on la matriu $V$ és la matriu ortogonal de vectors propis columna
donada per la descomposició espectral (Jordan) de $\Sigma$ - la
matriu de covariàncies de $X$ -, amb els vectors propis ordenats
segons el corresponent valor propi $\lambda_j$, de més gran a més petit.*

Com que la matriu de covariàncies és simètrica i semi-definida no-negativa, els valors propis són reals no-negatius, amb la descomposició:
$$\Sigma=V\Lambda V^t \qquad\qquad  (2)$$
amb $\Lambda$ la matriu diagonal de valors propis. Equivalentment,

$$\Lambda=V^t \Sigma V=\Sigma_Y. \qquad(3)$$
La igualtat (3) prova que les
components definides per (1) són incorrelacionades perquè tenen matriu de covariàncies diagonal i estan ordenades de major a menor variancia. En particular,
tenim que
$$Var (Y_j ) = \lambda_j,\quad\mbox{amb}\quad \lambda_1\geq\lambda_2\geq \ldots \geq \lambda_p.$$
De les equacions (3) i (2), igualant les traces, es dedueix fàcilment que:
 $$VT(Y):= tr(\Sigma_Y)=\lambda_1+ \cdots + \lambda_p=tr(\Sigma)=\sigma_1^2  +\cdots + \sigma_p^2=: VT(X)\ $$
on $VT$ indica la variància total. Aquesta igualtat s'anomena *principi de conservació de la variància*. Pel cas més habitual en què les variables inicials estan no només centrades sinó tipificades , és a dir $\sigma_i^2=1$, $\,\forall i$, es té que $\lambda_1+\lambda_2+\cdots + \lambda_p=p.$

De tot l'anterior, és lògic que s'escullin les $k$ primeres
components per representar tota la informació, si representen un percentatge "prou important" ($70\%$,
$80\%$, etc.,) de la variància total:
$$
\frac{\lambda_1+\ldots+\lambda_k}{\lambda_1+\ldots+\lambda_p} \geq
0.7  \mbox{ (o bé 0.8, etc.)}
$$
Les $k$ primeres components (on $k$ es determina pel criteri anterior o altres criteris) s'anomenen **components principals**.


**Gràfica de les variables**

Suposem variables tipificades. En aquest cas, la matriu $W=V\Lambda^{1/2}$ s'anomena matriu de pesos (*loadings*). Els pesos  són les correlacions entre variables i components i, alhora, les coordenades per expressar les variables com a combinació lineal (o mitjana
ponderada) de les components.  Si suposem que $k=2$, la variable original $X_i$ es representa amb les coordenades $(w_{i1},w_{i2})$ en forma de "fletxa". La primera igualtat del teorema és equivalent a
 $$
 \Sigma=W^tW
 $$
 Pels termes diagonals, resulta:
 $$
 \sigma_i^2=1=w_{i1}^2+w_{i2}^2+\cdots+w_{ip}^2
 $$
Per tant, els coeficients $w_{ij}^2$, o cosinus quadrats, sumen 1. 
Si la variable està ben representada en dimensió $k=2$, es té que
$$
\mbox{long.vector}^2=w_{i1}^2+w_{i2}^2\approx 1
$$
és a dir, la fletxa d'aquesta variable s'apropa a la circumferència de radi 1. Per contra, una fletxa curta indica que la variable no està ben representada en aquest subespai bidimensional.

**Gràfica dels individus**

Els individus es representen per les seves puntuacions en les noves variables, anomenades *scores*. Variables i individus s'interpreten conjuntament.  
