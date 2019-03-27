## Anàlisi de la variància (ANOVA)
L'anàlisi de la variància (ANOVA) és el mètode clàssic per comparar mitjanes entre grups, dos grups o més.

**Observació**. Hi ha una sèrie de supòsits que s'han de fer abans que s'apliqui l'ANOVA, la desviació en aquests supòsits portaran a resultats que poden ser enganyosos o inexactes. Aquests supòsits inclouen la independència, normalitat i variància constant dels errors. En algunes situacions, hi ha transformacions que poden ser utilitzades per evitar les violacions d'aquests supòsits, com ara la transformació logarítmica de les dades.

Suposem que tenim $N$ observacions repartides en $k$ grups i definim $n=\frac{N}{k}$. Llavors $x_{ij}$ seria l'individu $j$ corresponent al grup $i$. En aquest cas assumim que l'estudi és balancejat, és a dir, el nombre d'individus per grup és el mateix. Denotem $\bar{x.}$ com la mitjana de la mostra global, i $\bar{x_{i}}$ com la mitjana del grup $i$.

Les observacions es poden tornar a escriure com:

$$x_{ij} = \bar{x.} + (\bar{x_{i}} - \bar{x.}) + (x_{ij} - \bar{x_{i}})$$

Això ens porta al següent model:

$$x_{ij} = \mu + \alpha_{i} + \epsilon_{ij}$$

on $\mu$ i $\alpha_{i}$ són la mitjana global i la mitjana del grup $i$ respectivament. S'assumeix que el terme d'error $\epsilon_{ij}$ és iid i segueix una distribució normal

$$\epsilon_{ij} \sim \mathcal{N}(\mu,\,\sigma^{2})\$$,

La hipòtesi nul·la en un model ANOVA és que les mitjanes dels grups són iguals, és a dir:

$$\alpha_1 = \alpha_2 = ... = \alpha_k$$

Si això és cert, el terme d'error per a la diferència de grups queda definit com:

$$\bar{x_{i}} - \mu \sim \mathcal{N}(0,\,\frac{\sigma^{2}}{n}=\bar{\sigma}^2)\,$$

Es pot mesurar la quantitat total de variabilitat entre observacions sumant els quadrats de les diferències entre cadascun $\bar{x}.$ i $x_{ij}$:


$$SST(\text{Suma de quadrats totals}) = \sum_{i=1}^{k} \sum_{j=1}^{n_{i}} (x_{ij} - \bar{x}.)^2$$

La variabilitat total es pot desglossar en 2 termes:

- La variabilitat entre grups:

$$SSG = \sum_{i=1}^{k} n_{i}(\bar{x}_{i} - \bar{x}.)^2$$

amb $k-1$ graus de llibertat.
- La variabilitat intra grups:

$$SSE = \sum_{i=1}^{k} \sum_{j=1}^{n_{i}} (x_{ij} - \bar{x_{i}})^2$$

amb $N-k$ graus de llibertat.

Per tant, podem escriure la suma de quadrats totals com:

$$SST = SSG + SSE$$

Si la variabilitat entre grups és gran en relació amb la variabilitat intra grups, llavors les dades suggereixen que les mitjanes de les poblacions són significativament diferents. Si no existeixen diferencies, entre els grups, esperaríem que les mitjanes quadràtiques

$$MSG = \frac{SSG}{(k-1)}$$

$$MSE = \frac{SSE}{(N-k)}$$

siguin similars. El test estadístic ANOVA es defineix com la ràtio entre les dues mitjanes quadràtiques:

$$F = \frac{MSG}{MSE}$$

L'estadístic $F$ segueix una distribució F de Snedecor amb $k-1$ i $N-k$ graus de llibertat. Si la hipòtesi nul·la és certa, $F$ seria proper a 1. D'altra banda, si la mitjana quadràtica entre grups $MSG$ és gran, suposaria un valor gran de l'estadístic F. Bàsicament, l'ANOVA examina les dues fons de la variància total i mira quina part contribueix més. Per aquest motiu, s'anomena anàlisi de la variància encara que la intenció sigui comparar les mitjanes dels grups.
