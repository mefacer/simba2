## Interpretació de les gràfiques de l'ACP

Les gràfiques de variables i de casos d'individus s'interpreten conjuntament. La posició de les variables en la gràfica permet visualitzar l'estructura de correlació de les dades i suggereix possibles interpretacions de les components principals. Són gràfiques exploratòries que no informen de la significació estadística.

Els % en les etiquetes dels eixos indiquen la quantitat de variabilitat explicada per aquesta dimensió o component. 

#### Gràfica de variables 

Per defecte es treballa amb la matriu de correlacions (equivalent a considerar dades tipificades). En aquest cas totes les variables queden representades per fletxes de longitud entre 0 i 1. 

**Longitud i qualitat de representació**. La longitud de la fletxa indica la mida de la seva projecció en el subespai de les components (preferentment, de les dues primeres) i, per tant, la seva  qualitat de representació : *Una variable està ben representada  en el subespai de les dues components, si la fletxa té longitud propera a 1, és a dir, s'aproxima a la circunferència de radi 1*. <font color='blue'> Només es poden interpretar les variables ben representades! Si convé, es poden fer representacions en altres dimensions. </font>

**Angles i correlació**. El cosinus de l'angle format pels dos vectors és proporcional a la seva correlació, per tant: *Els angles que formen dues fletxes (o variables) entre sí, ens mostren la correlació existent entre les corresponents variables*.  *Els sentits dels vectors indiquen el signe de la correlació*. L'angle que forma una fletxa (variable) i un eix (component) indica la correlació entre la variable i la component. Per tant: 

-Si dues variables estan ben representades, com més petit (proper a zero) sigui l'angle entre les seves fletxes, més forta i positiva és la seva correlació. 

-Si dues variables estan ben representades, com més proper a 180$^o$ ($\pi$) sigui l'angle entre les seves fletxes, més forta i negativa és la seva correlació. 

-Si dues variables estan ben representades i l'angle entre les dues fletxes és proper al recte, la correlació és pràcticament zero.

-Si hi ha un grup de variables ben representades i en posicions properes en el gràfic, això indica que totes elles estan molt relacionades entre sí i que ens subministren bàsicament la mateixa informació, diem que conformen *un factor latent* o que són *colineals*. 

-Si veiem diversos grups de variables correlacionades, vol dir que hi ha *diversos factors latents* a la matriu de dades. 

-Si una o més  variables estan ben representades i tenen un angle  petit amb un dels eixos de coordenades, això indica que una de les components  està fortament correlacionada amb aquesta(es) variable(s) i, per tant, es pot interpretar (etiquetar) la component en base al significat d'aquesta(es) variable(s).

#### Gràfica d'individus

Els individus o casos es representen mitjançant un diagrama de dispersió dels *scores* o puntuacions en l'espai de les components. Els casos amb puntuacions més elevades en una component es poden interpretar en base a les variables més correlacionades amb aquesta component. Els colors de la gràfica dels scores se solen utilitzar per visualitzar una variable qualitativa o covariant.

