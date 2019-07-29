## Anàlisi de la variància (ANOVA)
L'anàlisi de la variància (ANOVA) és el mètode clàssic per comparar mitjanes entre grups, dos grups o més, i determinar la significació (o la no-significació) estadística  de les diferències entre mitjanes poblacionals (valors esperats).

**Observació**. L'ANOVA comporta un seguit de supòsits sobre el model probabilístic: incorrelació, Gaussianitat, centrament i variància constant dels errors.  Desviacions importants en aquestes assumpcions poden portar a resultats enganyosos o inexactes.  En algunes situacions, hi ha transformacions que poden ser utilitzades per evitar les violacions d'aquests supòsits, com ara la transformació *logarítmica*  que se sol aplicar, entre altres, a les dades d'expressió gènica.

Suposem que tenim $N$ observacions repartides per igual en $k$ grups i definim $n=\frac{N}{k}$. Llavors $x_{ij}$ seria l'observació de l'individu $j$ corresponent al grup $i$. En aquest cas diem que l'estudi és *balancejat*, és a dir, el nombre d'individus per grup (rèpliques) és el mateix. Els resultats s'estenen al cas no-balancejat. Denotem $\bar{x}$ com la mitjana de la mostra global, i $\bar{x_{i}}$ com la mitjana del grup $i$. Les observacions es poden escriure com:

$$x_{ij} = \bar{x} + (\bar{x_{i}} - \bar{x}) + (x_{ij} - \bar{x_{i}})$$

Això ens porta al model lineal següent:

$$X_{ij} = \mu + \alpha_{i} + \epsilon_{ij}$$

on $\mu$ és el valor esperat global i $\alpha_{i}$ és la part diferencial del grup $i$, respectivament. Els supòsits es tradueixen en què $\epsilon_{ij}$ són iid amb llei normal:

$$\epsilon_{ij} \sim \mathcal{N}(0,\,\sigma^{2})$$

La hipòtesi nul·la de l'ANOVA és que les mitjanes teòriques dels grups són iguals, és a dir, que les parts diferencials són totes zero:

$$\alpha_1 = \alpha_2 = ... = \alpha_k=0$$
L'estadístic de contrast de l'ANOVA es base en descomposar la variabilitat. La variabilitat total de les dades es pot mesurar sumant els quadrats de les diferències entre $x_{ij}$ i $\bar{x}$:


$$SST\,(\text{Suma de quadrats totals}) = \sum_{i=1}^{k} \sum_{j=1}^{n_{i}} (x_{ij} - \bar{x})^2$$

La variabilitat total es pot desglossar en 2 sumands 
$$SST = SSG + SSE$$
que expressen:

- La variabilitat entre grups:

$$SSG \,(\text{Suma de quadrats entre grups}) = \sum_{i=1}^{k} n_{i}(\bar{x}_{i} - \bar{x})^2$$

amb $k-1$ graus de llibertat i llei khi-quadrat, quan la hipòtesi nul·la és certa.

- La variabilitat dins de o intra grups, també anomenada variabilitat residual:

$$SSE \,(\text{Suma de quadrats residual}) = \sum_{i=1}^{k} \sum_{j=1}^{n_{i}} (x_{ij} - \bar{x_{i}})^2$$

amb $N-k$ graus de llibertat i llei khi-quadrat.

Si la variabilitat entre grups és gran en relació amb la variabilitat intra grups, aleshores les dades suggereixen que les mitjanes de les poblacions són significativament diferents. En efecte, si no hi ha diferències entre grups, la suma de quadrats entre grups $SSG$ i per tant la mitjana quadràtica entre grups $MSE$, seran properes a zero, mentre que si hi ha  diferències entre els grups, esperaríem que $MSG$ sigui gran comparada amb la mitjana quadràtica dins dels grups, també anomenada mitjana quadràtica residual o dels errors $MSE$:

$$MSG = \frac{SSG}{(k-1)} \qquad MSE = \frac{SSE}{(N-k)}$$


El estadístic del test ANOVA es defineix com la ràtio entre les dues mitjanes quadràtiques:

$$F = \frac{MSG}{MSE}$$

Si la hipòtesi nul·la és certa, l'estadístic $F$ segueix una distribució de Fisher-Snedecor amb $k-1$ i $N-k$ graus de llibertat i el valor de  $F$ s'espera que sigui proper a 0. D'altra banda, si la mitjana quadràtica entre grups $MSG$ és gran, això implica un valor gran de l'estadístic $F$ que es trobarà lluny del zero a la cua dreta de la distribució, és a dir un $p$-valor petit, amb el conseqüent rebuig de la hipòtesi nul·la i l'acceptació de diferències significatives entre les mitjanes dels grups. 

*Remarca:* Com que l'ANOVA examina les dues fons de la variància total i mira quina part contribueix més, s'anomena anàlisi de la variància encara que la finalitat sigui comparar les mitjanes dels grups.
