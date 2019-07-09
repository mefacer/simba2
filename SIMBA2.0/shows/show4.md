## Anàlisi de components principals (ACP): gràfiques

L'anàlisi de components principals és una de les diferents maneres per analitzar l'estructura d'una matriu de correlacions donada. En aquest apartat definirem la teoria de l'anàlisi de components principals i la seva representació gràfica.

**L'objectiu de l'ACP**

Donades $p$ variables $X_1,X_2, \ldots,X_p$ que suposarem centrades
(sovint també normalitzades amb variància igual a 1), volem
construir $p$ noves variables $Z_1,Z_2, \ldots,Z_p$, que anomenarem
compoents (les components principals seran cert subconjunt
d'aquestes) amb les següents condicions:

 1. Les noves variables han de ser combinació lineal de les
 variables originals.
 2. Les noves variables han de ser incorrelacionades.
 3. Les noves variables han de contenir la mateixa informaicó (variància
 total) que les originals, i estar ordenades de major a menor
 variància.

El teorema que demostra l'existència i unicitat de les components, i
en dóna el procediment de càlcul és el següent:

**Teorema 1**
Les components  venen donades per la transformació lineal
$$Y =V^tX$$
on la matriu $V$ és la matriu ortogonal de vectors propis columna
donada per la descomposició espectral (Jordan) de $\Sigma$ - la
matriu de covariàncies de $X$ -, amb els vectors propis ordenats
segons el valor propi $\lambda_j$, de més gran a més petit. A més,
com que la descomposició espectral compleix

$$\Sigma=V\Lambda V^t$$

amb $\Lambda$ la matriu diagonal de valors propis. La descomposició espectral és equivalent a

$$\Lambda=V^t\Sigma$$
on
$$V=\Sigma_Z$$

De la igualtat anterior del teorema teorema es dedueix que les
components són incorrelacionades perquè tenen matriu de covariàncies
diagonal i estan ordenades de major a menor variancia. En particular
tenim que
$$Var (Z_j ) = \lambda_j,\quad\mbox{amb}\quad \lambda_1\geq\lambda_2\geq \ldots \geq \lambda_p.$$
De l'equació  anterior, igualant les traces, tenim que:

 $$VT(Z):= \lambda_1+\lambda_2+\cdots + \lambda_p=\sigma_1^2 +\sigma_2^2+\cdots + \sigma_p^2=: VT(X)\ $$
on $VT$ indica la variància total. Aquesta igualtat s'anomena *principi de conservació de la variància*.

De tot l'anterior, és lògic que s'escullin les $k$ primeres
components, si representen un percentatge "prou important" ($70\%$,
$80\%$, etc.,) de la variància total:
$$
\frac{\lambda_1+\ldots+\lambda_k}{\lambda_1+\ldots+\lambda_p} \geq
0.7  \mbox{ (o bé 0.8, etc.)}
$$
Les $k$ primeres components, escollides segons aquest o un altre
criteri, s'anomenen *components principals*.

Pel cas més habitual en que les variables inicials estan
estandarditzades (centrades i escalades), és a dir $\sigma_i^2=1$,
$\,\forall i$, es té que
$$
\lambda_1+\lambda_2+\cdots + \lambda_p=1.
$$

**Gràfica de les variables**

 Suposem variables centrades i escalades. La matriu $W=V\Lambda^{1/2}$
s'anomena matriu de pesos *loadings*  i dóna les coordenades
per expressar  les variables com a combinació lineal (o mitjana
ponderada) de les components (veure la gràfica de les variables, a
l'apartat següent). Si suposem que $k=2$, la variable original $X_i$
 es representa amb les coordenades $(w_{i1},w_{i2})$, en forma de "fletxa". La primera igualtat del teorema és equivalent a
 $$
 \Sigma=W^tW
 $$
 Pels termes diagonals, resulta:
 $$
 \sigma_i^2=1=w_{i1}^2+w_{i2}^2+\cdots+w_{ip}^2
 $$
Per tant, els coeficients $w_{ij}^2$, o cosinus quadrats, sumen 1 i,
si la variable està ben representada en dimensió $k=2$, es té que
$$
\mbox{long.vector}^2=w_{i1}^2+w_{i2}^2\approx 1
$$
i la fletxa d'aquesta variable s'apropa a la circumferència de radi 1.
