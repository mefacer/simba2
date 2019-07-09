## Métode de Tukey (*honestly-significant-difference*)

Recordem que quan el nombre de possibles comparacions és elevat, per a un nivell de significació $\alpha$ donat, pot conduir a una inflació de l'error de tipus I, com també hem vist quan parlàvem de multiplicitat de contrastos.

Per identificar quins tractaments són significativament diferents entre ells i corregir el problema de la inflació de l'error de tipus I, hem utilitzat el mètode de Tukey i les seves hipòtesis són:


$$H_{0}: \mu_i = \mu_j \text{ per a cada parella de mitjanes }  i \neq j$$

$$H_{1}: \mu_i \neq \mu_j \text{ almenys una parella de mitjanes }  i \neq j$$

L'estadístic de contrast que utilitza el test de Tukey queda definit com:

$$Q_{I,n-I} = q_{I,n-I}(\alpha)\sqrt{\frac{MSE}{n}}$$

on $q_{I,n-I}$ és la quantila teòrica,

$$q_{I,n-I}(\alpha) = \frac{\bar{X}_j - \bar{X}_k}{\sqrt{\frac{\hat{S}^2}{n}}}$$

$\hat{X}_j$ i $\hat{X}_k$ són la mitjana del grup $j$ i $k$, respectivament, i $\hat{X}_j > \hat{X}_k$. $\hat{S}^2$ és l'estimació de la variància del error o residual; i $n$ és la grandària mostral per a tots els grups; on $I$ i $n-I$ són els graus de llibertat de la distribució del rang estudentitzat ($I$ correspon al nombre de nivells que té el factor, $n$ correspon a la grandària mostral).En el cas de tenir grandàries mostrals diferents entre els nivells del factor, hem d'utilitzar un altre $n$ (mitjana harmònica):

$$n_h = \frac{t}{\sum_{i=1}^{t}\frac{1}{n_i}}$$

La diferència entre mitjanes serà significativa amb un nivell de significació $\alpha$ si

$$|\bar{X}_j - \bar{X}_k | > HSD $$

on $HSD=Q_{I,n-I}$.

L'interval de confiança per a la diferència de mitjanes el definim com:

$$IC(\mu_j - \mu_k)_{(1-\alpha)}= (\bar{X}_j - \bar{X}_k) \pm q_{I,n-I,1-\alpha}\sqrt{\frac{MSE}{n}}$$

Si l'interval de la diferència inclou el 0, no rebutjem la hipòtesi nul·la del test i per tant, no hi ha diferències entre $\mu_j$ i $\mu_k$.
