## Interpretació de la taula amb l'estadístic, p-valor-ANOVA i p-valor-FDR

L'estadístic i el p-valor-ANOVA corresponen al test de diferència de mitjanes per a cada gen individualment. El valor de l'estadístic marca la cua dreta de la distribució, l'area de la cua és el p-valor-ANOVA. 

Per contra, el p-valor-FDR es refereix al conjunt de tests efectuats per a tots els gens. És un p-valor conjunt o experimental. 

**Exemples ficticis per a la interpretació de l'expressió diferencial d'un gen**

Fixem un llindar de $\alpha=$**0.05** per al p-valor-ANOVA, que la probabilitat d'error nominal màxima permesa per a cada test ANOVA (cada gen).

- Gen: GGGGG1
- p-valor-ANOVA = 0.1275

Direm que el gen GGGGG1 no presenta diferències significatives entre les expressions mitjanes perquè el p-valor-ANOVA està per damunt del llindar (0.1275 $>$ 0.05). No cal mirar el p-valor-FDR. 

- Gen: HHHHH2
- p-valor-ANOVA = 0.0423
- p-valor-FDR = 0.0572

Hi ha prou evidència per acceptar que el gen HHHHH2 presenta diferències estadísticament significatives entre les mitjanes dels tractaments, ja que el p-valor-ANOVA és inferior o igual al llindar establert (0.0423 $\leq$ 0.05). Ara bé, com que el p-valor-FDR se situa per damunt de 0.05 (0.0572), això ens indica que, si acceptem que aquest gen té diferències significatives, la probabilitat d'error del conjunt de decisions (gens marcats com a significatius) pot superar el 5\% nominal perquè el llindar de control és del 6\%.  

- Gen: KKKKK3
- p-valor-ANOVA = 0.0102
- p-valor-FDR = 0.0374

Hi ha prou evidència per acceptar que el gen KKKKK3 presenta diferències estadísticament significatives entre les mitjanes dels tractaments, ja que el P-valor (ANOVA) és inferior o igual al llindar establert (0.0102 $\leq$ 0.05). A més, com que el FDR se situa per sota de 0.05 (0.0374), això ens indica que, si acceptem que aquest gen té diferències significaties, la probabilitat d'error del conjunt de decisions (gens marcats com a significatius) segueix controlada pel 5\% nominal.  

**Recomanació:** <font color='blue'> En l'ambit de recerca on ens trobem, on el nombre de mostres no és excessiu, sinó que es treballa amb una quantitat moderada de gens (50 o menys), prendrem les decisions en base al p-valor-ANOVA, però controlant i informant del llindar del p-valor-FDR per tal que no superi ``cotes inacceptables'' (suggeriment: el doble del valor de $\alpha$).</font>