### Interpretació de les comparacions 2 a 2 (Tukey-Kramer)

Considerem les comparacions de parelles de tractaments com a proves post-hoc, és a dir, després que per a certs gens s'hagi acceptat  l'existència de diferències significatives entre les mitjanes dels tractaments, amb l'ANOVA. Per això utilitzem el mateix valor $\alpha$ fixat per a l'ANOVA. Per als gens on no hi hagi diferències significatives segons l'ANOVA, ja no es procedeix al test de Tukey-Kramer.

**Tukey-Table 1**

Aquest Taula conté el p-valor de Tukey-Kramer, per a cada parella de tractaments en columna i per a cadascun dels gens significatius, en fila. Els gens no-significatius segons l'ANOVA no surten a la taula. 

*Exemple d'una comparació significativa entre una parella de tractaments:*

- Gen: GGGGG1
- Comparació: 2-1
- Tukey-Kramer p-value: 0.0251 
- Nivell significació (llindar $\alpha$ (ANOVA)) : $0.05$


Hi ha evidencies estadísticament significatives per rebutjar la hipòtesis nul·la, és a dir, les mitjanes del tractament 2 i del tractament 1 són significativament diferents.

*Exemple d'una comparació NO significativa entre una parella de tractaments:*

- Gen: HHHHH2
- Comparació: 3-2
- Tukey-Kramer p-value: 0.1504
- Nivell significació (llindar $\alpha$ (ANOVA)) : $0.05$

Les mitjanes del tractament 3 i del tractament 2 no són signifcativament diferents.

**Tukey-Table 2**

En aquesta taula, si bé coherent amb l'anterior, hi tenim informació complementària: 

-Els valors a les columnes 1,2 ...,$I$ són les mitjanes pels diversos tractaments i les files tots els gens, tant si són significatius com si no ho són. 

-Fixat un gen, si cap de les mitjanes no té lletra és perquè no hi ha diferències significatives a l'ANOVA, la qual cosa es comprova també amb la columna p-value.

-Fixat un gen, les mitjanes que tenen la mateixa lletra (exponent) No són significativament diferents. 

-Fixat un gen, les mitjanes que tenen lletres diferents (exponent) són significativament diferents. 

-La columna "p-value" és, com hem dit, el mateix que l'ANOVA.

-La columna "harmonic_sample_size" és $r^*$: mitjana harmònica del nombre de rèpliques.  Com que es consideren les rèpliques vàlides, pot variar segons el gen.

-La columna "mse" és l'estimació de la variància del model obtinguda amb l'ANOVA. 

-La columna "hsd" dona el llindar per a les diferències honestament significatives (HSD=q·SEM). 

(Podeu revisar la teoria del test de Tukey al desplegable corresponent.)



