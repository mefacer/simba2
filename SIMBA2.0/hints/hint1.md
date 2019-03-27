## Interpretació de l'ANOVA

Fixem un llindar de **0.1** per al FDR (correcció del p-valor segons el false discovery rate).

**Exemple fictici per a la interpretació d'un gen que s'expressa de manera significativament diferencial**:

- Gen: GGGGG1
- FDR-p-value = 0.0423

Hi ha evidencies estadísticament significatives per rebutjar la hipòtesi nul·la d'igualtat de mitjanes entre tractaments, ja que el FDR-p-value és inferior o igual al llindar establert (0.0423 $\leq$ 0.1).

**Exemple fictici per a la interpretació d'un gen que NO s'expressa de manera significativament diferencial**:

- Gen: HHHHH2
- FDR-p-value = 0.23

Les diferència no són estadísticament significatives, i no es pot rebutjar la hipòtesi nul·la d'igualtat de mitjanes entre tractaments, ja que el FDR-p-value no és inferior al llindar establert (0.23 > 0.1).
