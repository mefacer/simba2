## Interpretació dels gràfics ACP

Atenció: En aquest procediment, *els casos són gens, i les variables són els nivells mitjans, per tractament, de l'expressió gènica d'aquests gens*. Els \% en les etiquetes dels eixos indiquen la quantitat de variabilitat explicada per aquesta dimensió o component. 

Els gràfics de variabes i de casos s'interpreten conjuntament. La posició de les variables en el gràfic mostra l'estructura de les dades i suggereix possibles interpretacions de les components principals. 

Són gràfiques exploratòries, no porten significació estadística associada.

### Gràfic de variables 

Per a dades escalades o tipificades (cas habitual), les variables queden representades per fletxes de longitud entre 0 i 1. 

**Longitud i qualitat de representació**. La longitud de la fletxa indica la mida de la seva projecció en el subespai de les components (preferentment, de les dues primeres) i, per tant, la seva  qualitat de representació : *Una variable està  ben representada  en el subespai de les dues compoenents, si la fletxa té longitud propera a 1, és a dir, s'aproxima a la circunferència de radi 1*. **Només es poden interpretar les variables ben representades !**

**Angles i correlació**. El cosinus de l'angle format pels dos vectors és proporcional a la seva correlació, per tant: *Els angles que formen dues fletxes (o variables) entre sí, ens mostren la correlació existent entre les corresponents variables*.  *Els sentits dels vectors indiquen el signe de la correlació*. L'angle que forma una fletxa (variable) i un eix (component) indica la correlació entre la variable i la component.
 
Per tant, 

-Si dues variables estan ben representades, com més petit (proper a zero) sigui l'angle entre les seves fletxes, més forta i positiva és la seva correlació. 

-Si dues variables estan ben representades, com més  proper a 180$^o$ ($\pi$) sigui l'angle entre les seves fletxes, més forta i negativa és la seva correlació. 

-Si dues variables estan ben representades i l'angle entre les dues fletxes és proper al recte, la correlació és pràcticament zero.

-Si hi ha un grup de variables ben representades i en posicions properes en el gràfic, això indica que totes elles estan molt relacionades entre sí i que ens subministren bàsicament la mateixa informació, diem que conformen *un factor latent*. 

-Si apareixen varis grups de variables corrleacionades, vol dir que hi ha *diversos factors latents* a la matriu de dades. 

-Si una o mès  variables estan ben representades i tenen un angle molt petit amb un dels eixos de coordenades, això indica que una de les components  està fortament correlacionada amb aquesta(es) variable(s) i, per tant, es pot interpretar (etiquetar) la component en base al significat d'aquesta(es) variable(s).


### Gràfic de casos individuals

Els gens (en general, els casos) es representen mitjançant un diagrama de dispersió dels scores o puntuacions en l'espai de les components. Els casos amb puntuacions més elevades en una component es poden interpretar en base amb les variables més correlacionades amb aquesta component. Els colors de la gràfica de dispersió se solen utilitzar per visualitzar una variable qualitativa.


## Exemple anàlisi gràfica
 <img src="exhint3.png" width="500">

Gràfica de variables (recordem que són mitjanes per tractament):

 - La primera component explica un 87\% de la variabilitat total de les mitjanes en els diversos gens. La segona component contribueix en un 9\%. Globalment, en dimensió 2 la pèrdua d'informació dels nivells mitjans és només d'un 4\%.
 - Totes les variables (les tres mitjanes) estan ben representades amb les dues primeres components.
 - Es pot observar una correlació molt alta entre el tractament 3 i el tractament 1, en mitjana.
 - La correlació del tractament 2 amb els altres tractaments és més feble.
 - La **primera component** (eix horitzontal) està correlacionada  valors alts d'expressió mitjana en tots tres tractaments.
 
 **Que les mitjanes estiguin correlacionades o no, en cap cas indica l'existència o no existència de diferències significatives !**

 
Gràfica de casos (recordem que són gens): 

 - La gràfica de la dreta ens mostra els gens (punts) i la seva funcionalitat gènica (llegenda de colors). **No s'hi veu cap patró clar d'agrupament per colors**.
 - Hi a dos gens molt separats a la part positiva de l'eix horitzontal: Aquests gens tenen uns nivells d'expressió mitjana alta en tots els tractaments.
 - Els gens situalts a la part negativa de l'eix horitzontal tendeixen a presentar nivells d'expressió més baixos en mitjana en tots els tractaments. 
